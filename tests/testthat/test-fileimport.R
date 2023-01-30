# DIPSAUS DEBUG START
# library(testthat)
# library(readNSx)

test_that("Testing importing NSP", {

  cache_root <- tools::R_user_dir('readNSx', which = "cache")
  sample_dir <- file.path(cache_root, "sampledata")
  sample_file <- file.path(sample_dir, "sample.nev")

  # For those who reach here, I don't have any sample data that
  # I can share on the internet, mostly due to license and privacy
  # concerns. If you would like to provide sample data (especially none
  # human subject data), please contact me by filing an issue
  # ticket on Github
  skip_if_not(file.exists(sample_file), message = "Sample data not found.")

  dest_dir <- file.path(cache_root, 'test')
  dest_prefix <- file.path(dest_dir, 'data')

  if(!dir.exists(dest_dir)) {
    nsp <- import_nsp(sample_file, prefix = dest_prefix)
  } else {
    nsp <- get_nsp(dest_prefix)
  }

  # make sure the versions are consistent
  nev <- nsp$nev
  expect_equal(nev$header_basic$file_spec, c(2, 3))
  expect_equal(nev$header_basic$time_resolution_samples, 30000)
  expect_equal(nev$header_basic$file_type, 'NEURALEV')
  expect_true(is.data.frame(nev$header_extended$NEUEVLBL))
  expect_true('electrode_id' %in% names(nev$header_extended$NEUEVLBL))

  ns3 <- nsp$ns3
  expect_equal(ns3$header_basic$file_type, 'NEURALCD')
  expect_equal(
    nev$header_basic$time_resolution_samples,
    ns3$header_basic$time_resolution_timestamp
  )
  # NS3 2000 Hz
  expect_equal(
    ns3$header_basic$time_resolution_timestamp,
    ns3$header_basic$period * 2000
  )
  expect_true(is.data.frame(ns3$header_extended$CC))
  expect_true('electrode_id' %in% names(ns3$header_extended$CC))

  label_table <- as.data.frame(nev$header_extended$NEUEVLBL)
  channel_table <- as.data.frame(ns3$header_extended$CC)

  expect_equal(
    label_table$label[
      label_table$electrode_id %in% channel_table$electrode_id
    ],
    channel_table$electrode_label
  )

  # check file names
  fnames <- readNSx:::channel_filename(channel_table$electrode_id, channel_table$electrode_label)
  for(ii in seq_len(ns3$nparts)) {
    expect_true(all(
      file.exists(file.path(sprintf("%s_ieeg%s%d", ns3$prefix, ns3$partition_prefix, ii), fnames))
    ))
  }


})
