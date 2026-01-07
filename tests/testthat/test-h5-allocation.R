# Tests for allocate_h5 and write_h5_slice functions

test_that("Three write strategies produce identical results", {
  skip_if_not(system.file(package = "hdf5r") != "", message = "hdf5r not installed")

 
  # Setup: create test data with random slice order
  set.seed(42)
  total_length <- 1000L
  n_slices <- 10L
  slice_size <- total_length / n_slices

  # Generate random data for each slice
  slices <- lapply(seq_len(n_slices), function(i) {
    rnorm(slice_size)
  })

 
  # Random order for writing slices
  write_order <- sample(seq_len(n_slices))

  # Expected final result (assembled in correct order)
  expected_data <- unlist(slices)

  # --- Case 1: In-memory array, write slices at random indices ---
  case1_array <- rep(NA_real_, total_length)
  for (i in write_order) {
    start_idx <- (i - 1L) * slice_size + 1L
    end_idx <- i * slice_size
    case1_array[start_idx:end_idx] <- slices[[i]]
  }

  # --- Case 2: In-memory array, write slices, save_h5 as whole ---
  case2_array <- rep(NA_real_, total_length)
  for (i in write_order) {
    start_idx <- (i - 1L) * slice_size + 1L
    end_idx <- i * slice_size
    case2_array[start_idx:end_idx] <- slices[[i]]
  }

  tmp_file_case2 <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file_case2, force = TRUE), add = TRUE)
  save_h5(x = case2_array, file = tmp_file_case2, name = "data", new_file = TRUE)
  case2_loaded <- load_h5(tmp_file_case2, "data", ram = TRUE)

  # --- Case 3: HDF5 preallocate + write_h5_slice ---
  tmp_file_case3 <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file_case3, force = TRUE), add = TRUE)

  allocate_h5(
    file = tmp_file_case3,
    name = "data",
    dims = total_length,
    ctype = "numeric",
    new_file = TRUE
  )

  for (i in write_order) {
    start_idx <- (i - 1L) * slice_size + 1L
    write_h5_slice(
      x = slices[[i]],
      file = tmp_file_case3,
      name = "data",
      start = start_idx
    )
  }
  case3_loaded <- load_h5(tmp_file_case3, "data", ram = TRUE)

  # --- Verify all three cases produce identical results ---
  expect_equal(case1_array, expected_data)
  expect_equal(as.vector(case2_loaded), expected_data)
  expect_equal(as.vector(case3_loaded), expected_data)

  # Cross-check: all three are equal to each other
 
  expect_equal(case1_array, as.vector(case2_loaded))
  expect_equal(case1_array, as.vector(case3_loaded))
  expect_equal(as.vector(case2_loaded), as.vector(case3_loaded))
})


test_that("Three write strategies produce identical results (2D case)", {
  skip_if_not(system.file(package = "hdf5r") != "", message = "hdf5r not installed")

  # Setup: 2D array with column-wise slices in random order
  set.seed(123)
  nrow <- 50L
  ncol <- 20L
  n_col_slices <- 4L
  cols_per_slice <- ncol / n_col_slices

  # Generate random data for each column slice
  slices <- lapply(seq_len(n_col_slices), function(i) {
    matrix(rnorm(nrow * cols_per_slice), nrow = nrow, ncol = cols_per_slice)
  })

  # Random order for writing
  write_order <- sample(seq_len(n_col_slices))

  # Expected final result
  expected_data <- do.call(cbind, slices)

  # --- Case 1: In-memory matrix, write slices at random indices ---
  case1_matrix <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  for (i in write_order) {
    start_col <- (i - 1L) * cols_per_slice + 1L
    end_col <- i * cols_per_slice
    case1_matrix[, start_col:end_col] <- slices[[i]]
  }

  # --- Case 2: In-memory matrix, write slices, save_h5 as whole ---
  case2_matrix <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  for (i in write_order) {
    start_col <- (i - 1L) * cols_per_slice + 1L
    end_col <- i * cols_per_slice
    case2_matrix[, start_col:end_col] <- slices[[i]]
  }

  tmp_file_case2 <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file_case2, force = TRUE), add = TRUE)
  save_h5(x = case2_matrix, file = tmp_file_case2, name = "data", new_file = TRUE)
  case2_loaded <- load_h5(tmp_file_case2, "data", ram = TRUE)

  # --- Case 3: HDF5 preallocate + write_h5_slice ---
  tmp_file_case3 <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file_case3, force = TRUE), add = TRUE)

  allocate_h5(
    file = tmp_file_case3,
    name = "data",
    dims = c(nrow, ncol),
    ctype = "numeric",
    new_file = TRUE
  )

  for (i in write_order) {
    start_col <- (i - 1L) * cols_per_slice + 1L
    write_h5_slice(
      x = slices[[i]],
      file = tmp_file_case3,
      name = "data",
      start = c(1L, start_col)
    )
  }
  case3_loaded <- load_h5(tmp_file_case3, "data", ram = TRUE)

  # --- Verify all three cases produce identical results ---
  expect_equal(case1_matrix, expected_data)
  expect_equal(case2_loaded, expected_data)
  expect_equal(case3_loaded, expected_data)

  # Cross-check
  expect_equal(case1_matrix, case2_loaded)
  expect_equal(case1_matrix, case3_loaded)
})


test_that("allocate_h5 and write_h5_slice work with hdf5r", {
  skip_if_not(system.file(package = "hdf5r") != "", message = "hdf5r not installed")


  # Create temp file

  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)

  # Test 1D allocation and writing
  dims <- 100L
  allocate_h5(
    file = tmp_file,
    name = "test_1d",
    dims = dims,
    ctype = "numeric",
    replace = TRUE,
    new_file = TRUE
  )

  # Verify allocation
  expect_true(file.exists(tmp_file))
  loaded <- load_h5(tmp_file, "test_1d", ram = TRUE)
  expect_equal(length(loaded), 100L)
  # HDF5 allocates with zeros (or uninitialized), not NA
  expect_true(all(loaded == 0 | is.na(loaded)))

  # Write first chunk
  chunk1 <- 1:50
  write_h5_slice(x = chunk1, file = tmp_file, name = "test_1d", start = 1L)

  # Write second chunk
  chunk2 <- 51:100
  write_h5_slice(x = chunk2, file = tmp_file, name = "test_1d", start = 51L)

  # Verify data

  loaded <- load_h5(tmp_file, "test_1d", ram = TRUE)
  expect_equal(loaded, 1:100)

  # Test 2D allocation
  allocate_h5(
    file = tmp_file,
    name = "test_2d",
    dims = c(10L, 20L),
    ctype = "numeric",
    replace = TRUE
  )

  # Verify 2D allocation
  loaded <- load_h5(tmp_file, "test_2d", ram = TRUE)
  expect_equal(dim(loaded), c(10L, 20L))
  # HDF5 allocates with zeros (or uninitialized), not NA
  expect_true(all(loaded == 0 | is.na(loaded)))

  # Write 2D slice
  slice_data <- matrix(1:50, nrow = 10, ncol = 5)
  write_h5_slice(x = slice_data, file = tmp_file, name = "test_2d", start = c(1L, 1L))

  # Write another 2D slice
  slice_data2 <- matrix(51:100, nrow = 10, ncol = 5)
  write_h5_slice(x = slice_data2, file = tmp_file, name = "test_2d", start = c(1L, 6L))

  # Verify 2D data
  loaded <- load_h5(tmp_file, "test_2d", ram = TRUE)
  expect_equal(loaded[, 1:5], matrix(1:50, nrow = 10, ncol = 5))
  expect_equal(loaded[, 6:10], matrix(51:100, nrow = 10, ncol = 5))

  # Test integer ctype
  allocate_h5(
    file = tmp_file,
    name = "test_int",
    dims = 50L,
    ctype = "integer",
    replace = TRUE
  )
  write_h5_slice(x = 1L:25L, file = tmp_file, name = "test_int", start = 1L)
  loaded <- load_h5(tmp_file, "test_int", ram = TRUE)
  expect_equal(loaded[1:25], 1L:25L)
})


test_that("allocate_h5 and write_h5_slice handle errors correctly", {
  skip_if_not(system.file(package = "hdf5r") != "", message = "hdf5r not installed")

  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)

  # Error: write to non-existent dataset

  expect_error(
    write_h5_slice(x = 1:10, file = tmp_file, name = "nonexistent", start = 1L),
    regexp = "allocate_h5|does not exist"
  )

  # Allocate first

  allocate_h5(file = tmp_file, name = "test", dims = 100L, new_file = TRUE)

  # Error: write out of bounds
  expect_error(
    write_h5_slice(x = 1:50, file = tmp_file, name = "test", start = 60L),
    regexp = "out of bounds"
  )

  # Error: negative start

  expect_error(
    write_h5_slice(x = 1:10, file = tmp_file, name = "test", start = 0L),
    regexp = "out of bounds"
  )

  # Error: dimension mismatch
  expect_error(
    write_h5_slice(x = matrix(1:10, 2, 5), file = tmp_file, name = "test", start = 1L),
    regexp = "dimensions"
  )
})


test_that("allocate_fakeh5 and write_fakeh5_slice work without hdf5r", {
  # Test the RDS-based fallback implementation directly

  tmp_file <- tempfile()
  on.exit(unlink(sprintf("%s.ralt", tmp_file), recursive = TRUE, force = TRUE), add = TRUE)

  # Test 1D allocation
  allocate_fakeh5(
    file = tmp_file,
    name = "test_1d",
    dims = 100L,
    ctype = "numeric",
    new_file = TRUE
  )

  # Verify allocation
  expect_true(dir.exists(sprintf("%s.ralt", tmp_file)))
  loaded <- load_fakeh5(tmp_file, "test_1d", ram = TRUE)
  expect_equal(length(loaded), 100L)
  expect_true(all(is.na(loaded)))

  # Write chunks
  write_fakeh5_slice(x = 1:50, file = tmp_file, name = "test_1d", start = 1L)
  write_fakeh5_slice(x = 51:100, file = tmp_file, name = "test_1d", start = 51L)

  # Verify
  loaded <- load_fakeh5(tmp_file, "test_1d", ram = TRUE)
  expect_equal(as.vector(loaded), 1:100)

  # Test 2D allocation
  allocate_fakeh5(
    file = tmp_file,
    name = "test_2d",
    dims = c(10L, 20L),
    ctype = "integer"
  )

  # Write 2D slices
  slice1 <- matrix(1L:50L, nrow = 10, ncol = 5)
  write_fakeh5_slice(x = slice1, file = tmp_file, name = "test_2d", start = c(1L, 1L))

  slice2 <- matrix(51L:100L, nrow = 10, ncol = 5)
  write_fakeh5_slice(x = slice2, file = tmp_file, name = "test_2d", start = c(1L, 6L))

  # Verify
  loaded <- load_fakeh5(tmp_file, "test_2d", ram = TRUE)
  expect_equal(loaded[, 1:5], slice1)
  expect_equal(loaded[, 6:10], slice2)
})


test_that("write_fakeh5_slice handles errors correctly", {
  tmp_file <- tempfile()
  on.exit(unlink(sprintf("%s.ralt", tmp_file), recursive = TRUE, force = TRUE), add = TRUE)

  # Error: write to non-existent dataset
  expect_error(
    write_fakeh5_slice(x = 1:10, file = tmp_file, name = "nonexistent", start = 1L),
    regexp = "allocate_h5|does not exist"
  )

  # Allocate first
  allocate_fakeh5(file = tmp_file, name = "test", dims = 100L, new_file = TRUE)

  # Error: write out of bounds
  expect_error(
    write_fakeh5_slice(x = 1:50, file = tmp_file, name = "test", start = 60L),
    regexp = "out of bounds"
  )

  # Error: dimension mismatch
  expect_error(
    write_fakeh5_slice(x = matrix(1:10, 2, 5), file = tmp_file, name = "test", start = 1L),
    regexp = "dimensions"
  )
})
