# NEV reading on synthetic files (see helper-synthetic-nev.R)

# writes the fixture to `<root>/<name>/sample.nev`, one folder per file
fixture_file <- function(fixture, root, name = fixture$version) {
  dir <- file.path(root, name)
  dir.create(dir, recursive = TRUE)
  write_nev_fixture(fixture, file.path(dir, "sample.nev"))
}

test_that("read_nev_header() reads the headers without touching data packets", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (version in c("2.3", "3.0")) {
    path <- fixture_file(synthetic_nev_fixture(version), root)
    # a broken data section (not a whole number of packets) must not matter
    cat("trailing", file = path, append = TRUE)

    expect_silent(header <- read_nev_header(path))
    expect_identical(list.files(dirname(path)), "sample.nev")
    expect_equal(header$header_basic$bytes_in_data_packet, 104)
    expect_equal(header$header_extended$NEUEVLBL$label, c("LA1", "LA2", "RB1"))
    expect_equal(header$header_extended$NEUEVLBL$electrode_id, c(1, 2, 3))
    expect_equal(header$header_extended$NEUEVWAV$digitization_factor,
                 c(250, 1000, 125))
  }
})

test_that("digital inputs and comments survive packets straddling read chunks", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (version in c("2.3", "3.0")) {
    path <- fixture_file(synthetic_nev_fixture(version), root)
    # 3.5 packets per chunk: 7 packets are read as 3 + 3 + 1
    nev <- read_nev(path, prefix = file.path(dirname(path), "data"),
                    exclude_events = "spike", chunk_bytes = 3.5 * 104)

    digital <- get_event(nev, "digital_inputs")
    expect_equal(digital$digital_input, c(5, 65535))
    expect_equal(digital$time_in_seconds, c(100, 3000000100) / 30000)

    comment <- get_event(nev, "comment")
    expect_equal(comment$comment, "trial start")
    expect_equal(comment$data, 42)

    expect_false("spike" %in% nev$event_types)
    expect_null(get_event(nev, "spike"))
  }
})

test_that("spike waveforms keep every sample, one column per spike", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (version in c("2.3", "3.0")) {
    fixture <- synthetic_nev_fixture(version, additional_flags = 0L)
    path <- fixture_file(fixture, root)
    nev <- read_nev(path, prefix = file.path(dirname(path), "data"),
                    exclude_events = NULL, chunk_bytes = 2 * 104)
    spikes <- get_event(nev, "spike")
    samples <- lapply(fixture$packets[c(2, 3, 5, 7)], `[[`, "samples")

    # 1-byte samples of electrode 2 fill all waveform bytes: 96 (2.x), 92 (3.0)
    n_samples <- if (version == "3.0") 92 else 96
    n_pad <- n_samples / 2
    waveform <- spikes$waveform[]
    expect_equal(dim(waveform), c(n_samples, 4))
    expect_equal(waveform[, 1], c(samples[[1]] * 250 / 1000, rep(0, n_pad)))
    expect_equal(waveform[, 2], samples[[2]] * 1000 / 1000)
    expect_equal(waveform[, 3], c(samples[[3]] * 125 / 1000, rep(0, n_pad)))
    expect_equal(waveform[, 4], c(samples[[4]] * 250 / 1000, rep(0, n_pad)))

    # file order, not grouped by electrode or sample size
    expect_equal(spikes$channelID[], c(1, 2, 3, 1))
    expect_equal(spikes$classification[], c(1, 0, 2, 0))
    last_timestamp <- if (version == "3.0") 2^32 + 7 else 4e9
    expect_equal(spikes$timeInSeconds[],
                 c(200, 300, 3e9, last_timestamp) / 30000)
    expect_true("spike" %in% nev$event_types)
  }
})

test_that("the 16-bit waveform flag overrides per-electrode sample sizes", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- synthetic_nev_fixture("2.3", additional_flags = 1L)
  path <- fixture_file(fixture, root)
  nsp <- import_nsp(path, prefix = file.path(dirname(path), "data"),
                    exclude_events = NULL, exclude_nsx = 1:9, verbose = FALSE)

  waveform <- get_event(nsp$nev, "spike")$waveform[]
  expect_equal(dim(waveform), c(48, 4))
  expect_equal(waveform[, 2], fixture$packets[[3]]$samples * 1000 / 1000)
})

test_that("a recording without spikes imports when spikes are not excluded", {
  fixture <- synthetic_nev_fixture("2.3")
  fixture$packets <- Filter(function(p) p$type != "spike", fixture$packets)
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- fixture_file(fixture, root)
  nsp <- import_nsp(path, prefix = file.path(dirname(path), "data"),
                    exclude_events = NULL, exclude_nsx = 1:9, verbose = FALSE)

  expect_false("spike" %in% nsp$nev$event_types)
  expect_null(get_event(nsp$nev, "spike"))
  expect_equal(get_event(nsp$nev, "digital_inputs")$digital_input, c(5, 65535))
})
