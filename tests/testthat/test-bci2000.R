# Test BCI2000 file reading functionality
# Runs on both local and CRAN (uses bundled sample file)

test_that("read_bci2000 works with sample file", {
  # Get bundled sample file

  file <- system.file("samples", "bci2000_sample.dat", package = "readNSx")
  skip_if_not(file.exists(file), "BCI2000 sample file not found")


  # Read the file


  result <- read_bci2000(file)

  # Check basic structure
  expect_type(result, "list")
  # Result should have header_basic, parameters, states, signals, summary
  expect_true("header_basic" %in% names(result))
  expect_true("parameters" %in% names(result))
  expect_true("signals" %in% names(result))

  # Check header_basic
  expect_type(result$header_basic, "list")

  # Check parameters structure
  expect_type(result$parameters, "list")
  expect_true(length(result$parameters) > 0)

  # Check that parameters have correct named list structure
  # Each parameter section should contain named parameter entries
  first_section <- result$parameters[[1]]
  expect_type(first_section, "list")

  if (length(first_section) > 0) {
    first_param <- first_section[[1]]
    expect_type(first_param, "list")
    # Parameters should have these fields from makeNamedList 8-element
    # (plus additional fields added by R wrapper)
    expected_core_fields <- c("section", "name", "comment", "data_type",
                              "value", "default", "lower_bound", "higher_bound")
    for (field in expected_core_fields) {
      expect_true(field %in% names(first_param),
                  info = sprintf("Field '%s' should be in parameter", field))
    }
  }

  # Check signals matrix
  expect_true(is.matrix(result$signals))
  expect_true(nrow(result$signals) > 0)
  expect_true(ncol(result$signals) > 0)

  # Check states - may be matrix or list depending on version
  expect_true("states" %in% names(result))
})

test_that("read_bci2000_header works with sample file", {
  file <- system.file("samples", "bci2000_sample.dat", package = "readNSx")
  skip_if_not(file.exists(file), "BCI2000 sample file not found")

  header <- read_bci2000_header(file)

  expect_type(header, "list")
  expect_true("basic_header" %in% names(header) || length(header) > 0)
})

test_that("parsed BCI2000 parameters survive garbage collection", {
  skip_on_cran()
  # one statement per return path of the parameter parser: number, empty list,
  # list, and string
  statements <- c(
    "Filtering int AlignChannels= 0 0 0 1 // align channels in time (0=no, 1=yes)",
    "Filtering intlist Empty= 0 // no values",
    "Filtering floatlist SourceChGain= 2 0.01617 0.01591 0.003 -500 500 // gain for each channel",
    "System string ApplicationIP= 172.16.11.215 127.0.0.1 127.0.0.1 127.0.0.1 // listening IP"
  )
  expected <- lapply(statements, parseBCIParamDef)

  # every allocation triggers a garbage collection; switched off again even
  # when the parser fails
  torture <- function(expr) {
    gctorture(TRUE)
    on.exit(gctorture(FALSE))
    expr
  }
  tortured <- lapply(statements, function(s) torture(parseBCIParamDef(s)))
  expect_identical(tortured, expected)
})
