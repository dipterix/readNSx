# NSx reading on synthetic files (see helper-synthetic-nsx.R)

# writes a synthetic file to `<root>/<name>/sample.ns3`, one folder per file
nsx_fixture_file <- function(root, name, values, blocks, ...) {
  dir <- file.path(root, name)
  dir.create(dir, recursive = TRUE)
  write_synthetic_nsx(file.path(dir, "sample.ns3"), values, blocks, ...)
}

import_fixture <- function(path) {
  read_nsx(path, prefix = file.path(dirname(path), "out"))
}

# 2 kS/s data points; one sampling interval is 500,000 ns
interval_ns <- 1e9 / 2000
ptp_start_ns <- 1e12
classic_start <- 315300548

test_that("both data block layouts import the same data", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values()
  paths <- c(
    classic = nsx_fixture_file(
      root, "classic", values,
      list(list(timestamp = classic_start, points = 1:20))),
    ptp = nsx_fixture_file(
      root, "ptp", values,
      nsx_ptp_blocks(ptp_start_ns + (0:19) * interval_ns),
      time_resolution = 1e9)
  )
  for (path in paths) {
    nsx <- import_fixture(path)
    expect_equal(nsx$nparts, 1)
    for (ch in 1:3) {
      expect_equal(nsx_imported_channel(nsx, ch), values[ch, ] * nsx_uv_per_count)
    }
  }
})

test_that("data points are written in batches, not once per data block", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  old <- options(readNSx.chunk_size = 7L)
  on.exit(options(old), add = TRUE)

  calls <- 0
  write_h5_slice_original <- write_h5_slice
  local_mocked_bindings(write_h5_slice = function(...) {
    calls <<- calls + 1
    write_h5_slice_original(...)
  })

  values <- nsx_fixture_values()
  paths <- c(
    nsx_fixture_file(root, "classic", values,
                     list(list(timestamp = classic_start, points = 1:20))),
    nsx_fixture_file(root, "ptp", values,
                     nsx_ptp_blocks(ptp_start_ns + (0:19) * interval_ns),
                     time_resolution = 1e9)
  )
  for (path in paths) {
    calls <- 0
    nsx <- import_fixture(path)
    # 3 channels x ceiling(20 data points / 7 per batch)
    expect_equal(calls, 9)
    expect_equal(nsx_imported_channel(nsx, 2), values[2, ] * nsx_uv_per_count)
  }
})

test_that("clock jumps that cancel out do not split a recording", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values()
  # the pattern seen in a real recording: +60, -90 and +30 sampling intervals
  # right after data points 4, 6 and 10
  jumps <- c(rep(0, 4), rep(60, 2), rep(-30, 4), rep(0, 10)) * interval_ns
  path <- nsx_fixture_file(
    root, "ptp", values,
    nsx_ptp_blocks(ptp_start_ns + (0:19) * interval_ns + jumps),
    time_resolution = 1e9)

  nsx <- import_fixture(path)
  expect_equal(nsx$nparts, 1)
  meta <- nsx_partition_meta(nsx)
  expect_equal(meta$n_points, 20)
  expect_equal(meta$relative_time, ptp_start_ns / 1e9)
  expect_equal(meta$duration, 20 / 2000)
  expect_equal(nsx_imported_channel(nsx, 3), values[3, ] * nsx_uv_per_count)
})

test_that("a pause splits a one-point-per-block recording", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values()
  # recording paused for 1 second after data point 10
  timestamps <- ptp_start_ns + (0:19) * interval_ns + c(rep(0, 10), rep(1e9, 10))
  path <- nsx_fixture_file(root, "ptp", values, nsx_ptp_blocks(timestamps),
                           time_resolution = 1e9)

  check <- function() {
    nsx <- import_fixture(path)
    expect_equal(nsx$nparts, 2)
    meta <- nsx_partition_meta(nsx)
    expect_equal(meta$n_points, c(10, 10))
    expect_equal(meta$relative_time, timestamps[c(1, 11)] / 1e9)
    expect_equal(nsx_imported_channel(nsx, 1), values[1, ] * nsx_uv_per_count)
  }
  check()

  # frames of 5 data blocks: the pause falls in the third frame
  old <- options(readNSx.packets_per_frame = 5)
  on.exit(options(old), add = TRUE)
  unlink(file.path(dirname(path), "out*"), recursive = TRUE)
  check()
})

test_that("a one-point-per-block file whose header resolution is wrong stops with an error", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values()
  # nanosecond time-stamps, but the header claims a 30 kHz clock (or an
  # implausible value that falls back to 30 kHz): every step looks like a pause
  for (resolution in c(30000, 1000)) {
    path <- nsx_fixture_file(
      root, sprintf("res%d", resolution), values,
      nsx_ptp_blocks(ptp_start_ns + (0:19) * interval_ns),
      time_resolution = resolution)
    expect_error(import_fixture(path), "time_resolution_timestamp", fixed = TRUE)
  }
})

test_that("each classic data block is a partition, timed by the header resolution", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values()
  # 30 kHz clock ticks; a 1-second pause after data point 10 (15 ticks each)
  starts <- c(classic_start, classic_start + 10 * 15 + 30000)
  blocks <- list(list(timestamp = starts[[1]], points = 1:10),
                 list(timestamp = starts[[2]], points = 11:20))
  for (version in c("2.3", "3.0")) {
    path <- nsx_fixture_file(root, version, values, blocks, version = version)
    nsx <- import_fixture(path)
    expect_equal(nsx$nparts, 2)
    meta <- nsx_partition_meta(nsx)
    # stored in the channel files with full precision
    expect_identical(meta$relative_time, starts / 30000)
    expect_equal(meta$duration, c(10, 10) / 2000)
    expect_equal(nsx_imported_channel(nsx, 2), values[2, ] * nsx_uv_per_count)
  }
})

test_that("contiguous classic data blocks are still separate partitions", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values()
  # the second block starts exactly one sampling interval after the first ends
  clocks <- list(
    list(name = "30k", resolution = 30000, start = classic_start, interval = 15),
    list(name = "1e9", resolution = 1e9, start = ptp_start_ns, interval = interval_ns)
  )
  for (clock in clocks) {
    starts <- clock$start + c(0, 10 * clock$interval)
    blocks <- list(list(timestamp = starts[[1]], points = 1:10),
                   list(timestamp = starts[[2]], points = 11:20))
    path <- nsx_fixture_file(root, clock$name, values, blocks,
                             time_resolution = clock$resolution)
    nsx <- import_fixture(path)
    expect_equal(nsx$nparts, 2)
    expect_equal(nsx_partition_meta(nsx)$relative_time, starts / clock$resolution)
    expect_equal(nsx_imported_channel(nsx, 1), values[1, ] * nsx_uv_per_count)
  }
})

test_that("an implausible timestamp resolution falls back to 30 kHz", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values()
  path <- nsx_fixture_file(
    root, "classic", values,
    list(list(timestamp = classic_start, points = 1:20)),
    time_resolution = 2000)

  nsx <- import_fixture(path)
  expect_equal(nsx$header_basic$time_resolution_timestamp, 30000)
  expect_equal(nsx_partition_meta(nsx)$relative_time, classic_start / 30000)
})

test_that("a block declaring more data points than the file holds is cut short", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values(n_points = 10)
  # "Number of Data Points" is unsigned 32-bit; 3e9 is above 2^31 - 1
  path <- nsx_fixture_file(
    root, "classic", values,
    list(list(timestamp = classic_start, points = 1:10, declared = 3e9)))

  expect_warning(nsx <- import_fixture(path), "fewer data points than its header")
  expect_equal(nsx$nparts, 1)
  expect_equal(nsx_partition_meta(nsx)$n_points, 10)
  expect_equal(nsx_imported_channel(nsx, 1), values[1, ] * nsx_uv_per_count)
})

test_that("files that end early or hold no data points are reported", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values()

  # a corrupt header byte after the first data block
  path <- nsx_fixture_file(
    root, "corrupt", values,
    list(list(timestamp = classic_start, points = 1:10),
         list(timestamp = classic_start + 1e6, points = 11:20)))
  bytes <- readBin(path, "raw", file.size(path))
  bytes[314 + 66 * 3 + 13 + 10 * 3 * 2 + 1] <- as.raw(2)
  writeBin(bytes, path)
  expect_warning(nsx <- import_fixture(path), "does not start with 0x01")
  expect_equal(nsx$nparts, 1)
  expect_equal(nsx_imported_channel(nsx, 1), values[1, 1:10] * nsx_uv_per_count)

  # only an empty data block
  path <- nsx_fixture_file(
    root, "empty", values,
    list(list(timestamp = classic_start, points = integer(0))))
  expect_warning(nsx <- import_fixture(path), "contains no data points")
  expect_equal(nsx$nparts, 0)
})

test_that("importing again repairs the empty file a killed import left behind", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values()
  path <- nsx_fixture_file(root, "classic", values,
                           list(list(timestamp = classic_start, points = 1:20)))
  import_fixture(path)

  # killed between creating a channel file and its first write to disk
  channel_file <- list.files(root, pattern = "^ch2\\.h5$", recursive = TRUE,
                             full.names = TRUE)
  expect_length(channel_file, 1)
  file.create(channel_file)

  nsx <- import_fixture(path)
  expect_equal(nsx_imported_channel(nsx, 2), values[2, ] * nsx_uv_per_count)
})

test_that("invalid reader options fail before anything is written", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- nsx_fixture_file(
    root, "classic", nsx_fixture_values(),
    list(list(timestamp = classic_start, points = 1:20)))
  bad <- list(
    readNSx.chunk_size = 0,
    readNSx.chunk_size = "1000",
    readNSx.packets_per_frame = 1,
    readNSx.packets_per_frame = 2.5,
    readNSx.max_tick_multiple = 1,
    readNSx.max_tick_multiple = NA_real_
  )
  for (ii in seq_along(bad)) {
    old <- options(bad[ii])
    expect_error(import_fixture(path), names(bad)[[ii]], fixed = TRUE)
    options(old)
  }
  expect_identical(list.files(dirname(path)), "sample.ns3")
})

test_that("results of the C++ readers survive garbage collection", {
  skip_on_cran()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  values <- nsx_fixture_values(n_channels = 2, n_points = 5)
  timestamps <- ptp_start_ns + (0:4) * interval_ns
  path <- nsx_fixture_file(root, "ptp", values, nsx_ptp_blocks(timestamps),
                           time_resolution = 1e9)
  skip_bytes <- 314 + 66 * 2
  n_bytes <- file.size(path) - skip_bytes

  # every allocation triggers a garbage collection; switched off again even
  # when `expr` fails, so the rest of the test run is not slowed down
  torture <- function(expr) {
    gctorture(TRUE)
    on.exit(gctorture(FALSE))
    expr
  }

  partitions <- torture(scanNSxPartitions(path, skip_bytes, n_bytes, 2L, 8L,
                                          1e9, 2000, 100000, 2))
  expect_equal(partitions$n_points, 5)
  expect_equal(partitions$stride, 13 + 2 * 2)
  expect_equal(partitions$byte_offset, skip_bytes + 13)
  expect_equal(partitions$start_time, ptp_start_ns / 1e9)
  expect_equal(partitions$bytes_unread, 0)
  expect_identical(partitions$stop_reason, "")

  # independent of the scan result, so a scan failure is not blamed on it
  points <- torture(readNSxPoints(path, skip_bytes + 13, 13 + 2 * 2, 2L,
                                  c(0.25, 0.25), c(0, 0), 0, 5))
  expect_equal(points, values * 0.25)

  # the legacy reader returns two vectors; with one channel they have the same
  # length, so a freed vector is reused for the other and the failure shows
  values1 <- nsx_fixture_values(n_channels = 1, n_points = 5)
  path1 <- nsx_fixture_file(root, "ptp1", values1, nsx_ptp_blocks(timestamps),
                            time_resolution = 1e9)
  skip_bytes1 <- 314 + 66
  legacy <- torture(readNSxDataPacket30(path1, file.size(path1) - skip_bytes1,
                                        2000, 1L, skip_bytes1, 1, 0))
  expect_equal(legacy$data, as.vector(values1))
  expect_equal(legacy$timestamps, timestamps / 1e9)
})
