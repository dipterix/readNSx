# DIPSAUS DEBUG START
# library(testthat)
# library(readNSx)

test_that("int32 conversion", {
  x <- as.integer(c(2^31-1, -2^31+1, 0, -1, 1, NA_integer_))
  bits <- sapply(x, intToBits)
  y <- apply(bits, 2, function(s) {
    rawToInt32(packBits(s))
  })
  expect_identical(x, y)
})


test_that("uint8 conversion", {
  x <- seq(0, 255)
  y <- rawToUInt8(as.raw(x))
  expect_identical(x, y)
})


test_that("int64 conversion", {
  skip_if(system.file(package = "bit64") == "")
  x <- c(bit64::lim.integer64(), 0, -1, 1, bit64::NA_integer64_)
  y <- do.call(
    bit64::c.integer64,
    lapply(strsplit(bit64::as.bitstring(x), ""), function(s){
      bits <- rev(as.raw(s))
      rawToInt64(packBits(bits))
    })
  )

  expect_identical(x, y)
})

