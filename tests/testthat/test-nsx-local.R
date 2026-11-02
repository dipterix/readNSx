# Test NSx/NEV file reading functionality
# Local tests only - skipped on CRAN and when test files are missing

test_that("read_nev works with local NEV file", {
  skip_on_cran()

  # Local test file path
 
 nev_path <- "~/rave_data/raw_dir/PAV058/001/raw/PAV054_Datafile_001.nev"
  nev_path <- normalizePath(nev_path, mustWork = FALSE)
  skip_if_not(file.exists(nev_path), "Local NEV test file not found")

  # Read NEV file
  result <- read_nev(path = nev_path)

  # Check basic structure
  expect_type(result, "list")

  # NEV should have header and event data
  expect_true("header_basic" %in% names(result) || "basic_header" %in% names(result) || length(result) > 0)
})

test_that("read_nsx works with local NSx file", {
  skip_on_cran()

  # Try to find NSx files in the same directory as NEV
  nev_path <- "~/rave_data/raw_dir/PAV058/001/raw/PAV054_Datafile_001.nev"
  nev_path <- normalizePath(nev_path, mustWork = FALSE)
  skip_if_not(file.exists(nev_path), "Local NEV test file not found")

  # Look for corresponding NSx files (ns1-ns9)
  base_path <- sub("\\.nev$", "", nev_path, ignore.case = TRUE)
  nsx_extensions <- sprintf(".ns%d", 1:9)
  nsx_paths <- paste0(base_path, nsx_extensions)
  existing_nsx <- nsx_paths[file.exists(nsx_paths)]

  skip_if(length(existing_nsx) == 0, "No NSx files found alongside NEV file")

  # Test with first available NSx file
  nsx_path <- existing_nsx[1]

  # Read NSx file
  result <- read_nsx(path = nsx_path)

  # Check basic structure
  expect_type(result, "list")

  # NSx should return data structure
  expect_true(length(result) > 0)
})

test_that("scanNSxPartitions returns correctly named list",
{
  skip_on_cran()

  # Local test file path
  nev_path <- "~/rave_data/raw_dir/PAV058/001/raw/PAV054_Datafile_001.nev"
  nev_path <- normalizePath(nev_path, mustWork = FALSE)
  skip_if_not(file.exists(nev_path), "Local NEV test file not found")

  # Look for corresponding NSx files
  base_path <- sub("\\.nev$", "", nev_path, ignore.case = TRUE)
  nsx_extensions <- sprintf(".ns%d", 1:9)
  nsx_paths <- paste0(base_path, nsx_extensions)
  existing_nsx <- nsx_paths[file.exists(nsx_paths)]

  skip_if(length(existing_nsx) == 0, "No NSx files found alongside NEV file")

  nsx_path <- existing_nsx[1]

  # Get file info to call low-level scan function
  ftype <- get_file_type(path = nsx_path)
  spec <- get_specification(ftype$version, "nsx")

  # Read basic header to get channel count and skip bytes
  conn <- file(nsx_path, "rb")
  on.exit(close(conn))

  header_basic <- read_sequential(conn, spec$specification$section1$dictionary)
  n_channels <- header_basic$channel_count
  skip_bytes <- header_basic$bytes_in_headers
  file_size <- file.size(nsx_path)
  n_bytes <- file_size - skip_bytes

  close(conn)
  on.exit()

  # Call the low-level scan function; file spec 3.0 has 64-bit timestamps
  # ftype$version may be character vector like c("3", "0"), so paste it
  version_str <- paste(ftype$version, collapse = ".")
  partitions <- scanNSxPartitions(
    nsx_path, skip_bytes, n_bytes, n_channels,
    if (grepl("^3", version_str)) 8L else 4L,
    header_basic$time_resolution_timestamp, 30000 / header_basic$period,
    100000, 2
  )

  # Check the named list structure from makeNamedList 4-element
  expect_type(partitions, "list")
  expect_named(partitions, c("byte_offset", "stride", "n_points", "start_time",
                           "bytes_unread", "stop_reason"))
  expect_type(partitions$byte_offset, "double")
  expect_type(partitions$stride, "double")
  expect_type(partitions$n_points, "double")
  expect_type(partitions$start_time, "double")
  expect_true(length(partitions$n_points) >= 1)
})
