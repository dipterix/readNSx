# Tests for the HDF5 storage layer. The point of most of these is that the
# format on disk is fixed: files written by older versions of readNSx, or by
# other HDF5 tools, must keep reading back exactly as they did before.
#
# The reverse direction - that files readNSx writes are readable by other HDF5
# tools - cannot be asserted here without depending on one of them, so it is not
# covered automatically. Adding `hdf5r` to Suggests and reading a freshly
# written file with it is the way to check that if it is ever in doubt.

h5_fixtures <- function() {
  list(
    dbl = as.double(1:50) / 7,
    int = 1:50,
    chr = "{\"a\":1,\"b\":\"x\"}",
    chrv = c("alpha", "beta", "gamma"),
    lgl = c(TRUE, FALSE, NA),
    mat = matrix(as.double(1:12), 3, 4),
    arr = array(as.double(1:24), c(2, 3, 4)),
    empty = numeric(0),
    # Types readNSx does not write itself, but that save_h5()/load_h5() must
    # handle for rave-ieeg/ravecore.
    cplx = c(1 + 4i, 3 - 1i),
    cplx_na = c(1 + 4i, NA_complex_, complex(real = NaN, imaginary = Inf)),
    cplx_mat = matrix(complex(real = 1:6, imaginary = 6:1), 2, 3),
    chr_na = c("a", NA_character_, "c"),
    chr_utf8 = c("plain", "caf\u00e9 \u00b5V", "\u4e2d\u6587"),
    int_na = c(1L, NA_integer_, 3L),
    dbl_na = c(1, NA_real_, NaN, Inf, -Inf)
  )
}


test_that("every supported type survives a round trip unchanged", {
  fixtures <- h5_fixtures()
  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)

  for (nm in names(fixtures)) {
    save_h5(fixtures[[nm]], tmp_file, nm, replace = TRUE, quiet = TRUE)
  }

  expect_setequal(h5_names(tmp_file), names(fixtures))

  for (nm in names(fixtures)) {
    expect_identical(load_h5(tmp_file, nm, ram = TRUE), fixtures[[nm]], info = nm)
    # The stored shape must survive too, not just the values.
    expect_equal(h5_native_dims(tmp_file, nm), r_dims_of(fixtures[[nm]]), info = nm)
  }
})


test_that("pre-allocation accepts slices written out of order", {
  set.seed(1)
  expected <- rnorm(500)

  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)

  allocate_h5(tmp_file, "data", dims = 500, chunk = 128, level = 4,
              ctype = "numeric", new_file = TRUE, quiet = TRUE)
  # A hyperslab write must not assume the slices arrive front to back.
  for (i in sample(seq_len(10))) {
    write_h5_slice(expected[((i - 1) * 50 + 1):(i * 50)], tmp_file, "data",
                   start = (i - 1) * 50 + 1, quiet = TRUE)
  }

  expect_equal(load_h5(tmp_file, "data", ram = TRUE), expected)
})


test_that("partial reads return exactly what a full read would", {
  vec <- as.double(1:1000)
  mat <- matrix(as.double(1:200), 20, 10)

  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)
  save_h5(vec, tmp_file, "vec", quiet = TRUE)
  save_h5(mat, tmp_file, "mat", quiet = TRUE)

  v <- load_h5(tmp_file, "vec")
  m <- load_h5(tmp_file, "mat")

  # `subset()` allocates its result with `array()`, so an indexed read of a
  # rank-1 dataset keeps a length-1 `dim`. Long-standing behaviour, asserted
  # here so that it cannot drift.
  expect_equal(v[101:200], array(vec[101:200], dim = 100L))

  # Contiguous requests take the hyperslab path ...
  expect_equal(m[3:8, 2:5], mat[3:8, 2:5, drop = FALSE])
  expect_equal(m[, 2:5], mat[, 2:5, drop = FALSE])

  # ... and non-contiguous ones fall back to reading and subsetting, which must
  # give the same answer.
  expect_equal(v[c(5, 1, 900)], array(vec[c(5, 1, 900)], dim = 3L))
  expect_equal(m[c(1, 5, 3), c(9, 2)], mat[c(1, 5, 3), c(9, 2), drop = FALSE])
  expect_equal(m[c(TRUE, rep(FALSE, 19)), 2:5], mat[1, 2:5, drop = FALSE])

  # Out-of-range indices are padded with NA rather than raising.
  expect_equal(v[999:1002], array(c(vec[999:1000], NA, NA), dim = 4L))

  expect_equal(v[], vec)
  expect_equal(m[], mat)
  expect_null(dim(v))
  expect_equal(dim(m), c(20L, 10L))
  expect_equal(length(v), 1000L)
})


test_that("a file written by hdf5r before the rewrite still reads correctly", {
  # `golden-hdf5r.h5` was produced by the hdf5r-based version of readNSx. It is
  # the guarantee that data already exported by users stays readable.
  golden <- test_path("golden-hdf5r.h5")
  skip_if_not(file.exists(golden), "golden fixture missing")

  expected <- list(
    dbl = as.double(1:50) / 7,
    int = 1:50,
    chr = "{\"a\":1,\"b\":\"x\"}",
    lgl = c(TRUE, FALSE, NA),
    mat = matrix(as.double(1:12), 3, 4),
    arr = array(as.double(1:24), c(2, 3, 4)),
    `grp/nested` = as.double(101:110)
  )

  expect_setequal(h5_names(golden), names(expected))
  for (nm in names(expected)) {
    expect_identical(load_h5(golden, nm, ram = TRUE), expected[[nm]], info = nm)
  }

  # load_h5_all must rebuild the nested group structure
  all_data <- load_h5_all(golden, ram = TRUE)
  expect_true(is.environment(all_data$grp))
  expect_identical(all_data$grp$nested, expected[["grp/nested"]])
})


test_that("complex is stored as the numpy-compatible {r, i} compound", {
  z <- c(1 + 4i, NA_complex_, 3 - 1i)
  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)

  save_h5(z, tmp_file, "z", quiet = TRUE)
  expect_identical(load_h5(tmp_file, "z", ram = TRUE), z)
  # Rank 1, not 2: the two compound members are one element, not a dimension.
  expect_equal(h5_native_dims(tmp_file, "z"), 3)

  # Pre-allocation and slice writes work for complex too.
  allocate_h5(tmp_file, "za", dims = 6, ctype = "complex", new_file = FALSE,
              quiet = TRUE)
  write_h5_slice(z, tmp_file, "za", start = 4, quiet = TRUE)
  expect_identical(load_h5(tmp_file, "za", ram = TRUE)[4:6], z)
})


test_that("strings keep their encoding and their NAs", {
  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)

  s <- c("plain", "caf\u00e9 \u00b5V", "\u4e2d\u6587", NA_character_)
  save_h5(s, tmp_file, "s", quiet = TRUE)
  back <- load_h5(tmp_file, "s", ram = TRUE)

  expect_identical(back, s)
  expect_true(is.na(back[[4]]))
  # R marks only the genuinely non-ASCII elements, so comparisons are unaffected.
  expect_identical(Encoding(back), Encoding(s))
})


test_that("h5_valid does not destroy the file it is asked about", {
  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)
  save_h5(as.double(1:10), tmp_file, "data", quiet = TRUE)

  expect_true(h5_valid(tmp_file, "r"))
  expect_true(h5_valid(tmp_file, "w"))
  expect_identical(load_h5(tmp_file, "data", ram = TRUE), as.double(1:10))

  expect_false(h5_valid(tempfile(fileext = ".h5"), "r"))
  expect_false(h5FileValid(tempfile(fileext = ".h5")))
  expect_identical(h5_names(tempfile(fileext = ".h5")), character())
})


test_that("datasets larger than 2^31 elements are addressable", {
  # Only the touched chunks are ever written, so this costs almost no disk.
  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)

  n <- 3e9
  start <- 2.9e9
  expect_gt(n, 2^31)

  allocate_h5(tmp_file, "data", dims = n, chunk = 131072, level = 4,
              ctype = "numeric", new_file = TRUE, quiet = TRUE)
  expect_equal(h5_native_dims(tmp_file, "data"), n)
  expect_equal(length(load_h5(tmp_file, "data")), n)

  write_h5_slice(as.double(1:100), tmp_file, "data", start = start, quiet = TRUE)
  expect_identical(
    h5_native_read_slab(tmp_file, "data", start = start, count = 100),
    as.double(1:100)
  )
})
