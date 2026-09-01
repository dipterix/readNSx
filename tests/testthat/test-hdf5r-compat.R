# Round-trip tests against the implementation readNSx (and rave-ieeg/ravecore)
# used before the HDF5 layer was rewritten.
#
# `hdf5r` is deliberately NOT in Suggests: it is exactly the dependency this
# rewrite removed, and it frequently fails to compile. It is reached only via
# `asNamespace("hdf5r")` - never `hdf5r::`, which R CMD check would treat as an
# undeclared import - and the whole file skips when it is absent.
#
# Everything between the two markers below is a copy of the previous
# implementation (`git show HEAD~:R/hdf5.R`, itself the descendant of raveio's
# `LazyH5`), with the `.ralt` fallback branches removed and every `hdf5r::`
# rewritten as `ns$`. It is the reference the new writer is measured against.

skip_if_not(nzchar(system.file(package = "hdf5r")), "hdf5r not installed")

ns <- asNamespace("hdf5r")

# ---------------------------------------------------------------------------
# BEGIN historical hdf5r implementation
# ---------------------------------------------------------------------------

OldLazyH5 <- R6::R6Class(
  classname = "OldLazyH5",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    file = NULL,
    name = NULL,
    read_only = TRUE,
    data_ptr = NULL,
    file_ptr = NULL,
    last_dim = NULL,
    finalize = function() {
      self$close(all = TRUE)
    }
  ),
  public = list(

    quiet = FALSE,

    print = function() {
      if (!is.null(private$data_ptr)) {
        if (private$data_ptr$is_valid) {
          base::print(private$data_ptr)
        } else {
          base::cat("Pointer closed. Information since last open:\nDim: ",
                    paste(private$last_dim, collapse = "x"), " \tRank: ",
                    length(private$last_dim), "\n")
        }
      }
      invisible(self)
    },
    initialize = function(file_path, data_name, read_only = FALSE, quiet = FALSE) {

      # First get absolute path, otherwise hdf5r may report file not found error
      if (read_only) {
        private$file <- normalizePath(file_path)

        if (!ns$is_hdf5(private$file)) {
          stop("File is not a valid HDF5 file")
        }
      } else {
        file_path <- normalizePath(file_path, mustWork = FALSE)
        private$file <- file_path
      }
      self$quiet <- isTRUE(quiet)
      private$name <- data_name
      private$read_only <- read_only
    },

    save = function(x, chunk = "auto", level = 7, replace = TRUE,
                    new_file = FALSE, force = TRUE, ctype = NULL, size = NULL,
                    ...) {
      # ctype and size is deprecated but kept in case of compatibility issues
      # ptr$create_dataset =
      # function (name, robj = NULL, dtype = NULL, space = NULL, dims = NULL,
      #           chunk_dims = "auto", gzip_level = 4, link_create_pl = h5const$H5P_DEFAULT,
      #           dataset_create_pl = h5const$H5P_DEFAULT, dataset_access_pl = h5const$H5P_DEFAULT)
      if (private$read_only) {
        if (!force) {
          stop('File is read-only. Use "force=TRUE"')
        } else {
          # Close current pointer
          self$close(all = TRUE)
          private$read_only <- FALSE

          on.exit({
            self$close(all = TRUE)
            private$read_only <- TRUE
          }, add = TRUE, after = FALSE)
        }
      }

      if (new_file && file.exists(private$file)) {
        self$close(all = TRUE)
        file.remove(private$file)
      }

      self$open(new_dataset = replace, robj = x, chunk = chunk, gzip_level = level, ...)

      self$close(all = TRUE)

    },


    open = function(new_dataset = FALSE, robj, ...) {

      # check data pointer
      # if valid, no need to do anything, otherwise, enter if clause
      if (new_dataset || is.null(private$data_ptr) || !private$data_ptr$is_valid) {

        # Check if file is valid,
        if (is.null(private$file_ptr) || !private$file_ptr$is_valid) {
          # if no, create new link
          mode <- ifelse(private$read_only, "r", "a")
          tryCatch({
            private$file_ptr <- ns$H5File$new(private$file, mode)
          }, error = function(e) {
            # Open for writting, we should close all connections first
            # then the file can be opened, otherwise, Access type: H5F_ACC_RDONLY
            # will lock the file for writting
            f <- ns$H5File$new(private$file, "r")
            if (!self$quiet) {
              message("Closing all other connections to [{private$file}] - {f$get_obj_count() - 1}")
            }

            try({ f$close_all() }, silent = TRUE)
            private$file_ptr <- ns$H5File$new(private$file, mode)
          })
        }

        has_data <- private$file_ptr$path_valid(private$name)

        if (!private$read_only && (new_dataset || ! has_data)) {
          # need to create new dataset
          g <- strsplit(private$name, split = "/")[[1]]
          g <- g[trimws(g) != ""]

          ptr <- private$file_ptr
          nm <- ""

          for (i in g[-length(g)]) {
            nm <- sprintf("%s/%s", nm, i)
            if (!ptr$path_valid(path = nm)) {
              ptr <- ptr$create_group(i)
            } else {
              ptr <- ptr[[i]]
            }
          }

          # create dataset
          nm <- g[length(g)]
          if (ptr$path_valid(path = nm)) {
            # dataset exists, unlink first
            ptr$link_delete(nm)
          }
          # new create
          if (missing(robj)) {
            robj <- NA
          }
          ptr$create_dataset(nm, robj = robj, ...)
          if (ptr$is_valid && inherits(ptr, "H5Group")) {
            ptr$close()
          }
        } else if (!has_data) {
          stop(sprintf(
            "File [%s] has no [%s] in it.",
            private$file, private$name
          ))
        }

        private$data_ptr <- private$file_ptr[[private$name]]

      }

      private$last_dim <- private$data_ptr$dims

    },


    close = function(all = TRUE) {
      try({
        # check if data link is valid
        if (!is.null(private$data_ptr) && private$data_ptr$is_valid) {
          private$data_ptr$close()
        }

        # if file link is valid, get_obj_ids() should return a vector of 1
        if (all && !is.null(private$file_ptr) && private$file_ptr$is_valid) {
          private$file_ptr$close_all()
        }
      }, silent = TRUE)
    },
    subset = function(
    ...,
    drop = FALSE, stream = FALSE,
    envir = parent.frame()
    ) {
      self$open()
      dims <- self$get_dims()

      # step 1: eval indices
      args <- eval(substitute(alist(...)))
      if (length(args) == 0 || (length(args) == 1 && args[[1]] == "")) {
        return(private$data_ptr$read())
      }
      args <- lapply(args, function(x) {
        if (x == "") {
          return(x)
        } else {
          return(eval(x, envir = envir))
        }
      })

      # step 2: get allocation size
      alloc_dim <- sapply(seq_along(dims), function(ii) {
        if (is.logical(args[[ii]])) {
          return(sum(args[[ii]]))
        } else if (is.numeric(args[[ii]])) {
          return(length(args[[ii]]))
        } else {
          # must be blank "", otherwise raise error
          return(dims[ii])
        }
      })

      # step 3: get legit indices
      legit_args <- lapply(seq_along(dims), function(ii) {
        if (is.logical(args[[ii]])) {
          return(args[[ii]])
        } else if (is.numeric(args[[ii]])) {
          return(
            args[[ii]][args[[ii]] <= dims[ii] & args[[ii]] > 0]
          )
        } else {
          return(args[[ii]])
        }
      })

      # step 4: get mapping
      mapping <- lapply(seq_along(dims), function(ii) {
        if (is.logical(args[[ii]])) {
          return(
            rep(TRUE, sum(args[[ii]]))
          )
        } else if (is.numeric(args[[ii]])) {
          return(args[[ii]] <= dims[ii] & args[[ii]] > 0)
        } else {
          return(args[[ii]])
        }
      })

      # alloc space
      re <- array(NA, dim = alloc_dim)

      if (stream) {
        re <- do.call(`[<-`, c(list(re), mapping, list(
          value = private$data_ptr$read(
            args = legit_args,
            drop = FALSE,
            envir = environment()
          )
        )))
      } else {
        re <- do.call(`[<-`, c(list(re), mapping, list(
          value = do.call("[", c(list(private$data_ptr$read()), legit_args, list(drop = FALSE)))
        )))
      }

      self$close(all = !private$read_only)


      if (drop) {
        return(drop(re))
      } else {
        return(re)
      }
    },

    get_dims = function(stay_open = TRUE) {
      self$open()
      re <- private$data_ptr$dims
      if (!stay_open) {
        self$close(all = !private$read_only)
      }
      re
    },

    allocate = function(dims, chunk = "auto", level = 4, ctype = "numeric",
                        replace = TRUE, new_file = FALSE) {
      # Pre-allocate an HDF5 dataset with specified dimensions
      # without loading data into memory
      if (private$read_only) {
        stop("File is read-only. Cannot allocate dataset.")
      }

      if (new_file && file.exists(private$file)) {
        self$close(all = TRUE)
        file.remove(private$file)
      }

      # Open or create file
      if (is.null(private$file_ptr) || !private$file_ptr$is_valid) {
        private$file_ptr <- ns$H5File$new(private$file, "a")
      }

      # Create parent groups if needed
      g <- strsplit(private$name, split = "/")[[1]]
      g <- g[trimws(g) != ""]

      ptr <- private$file_ptr
      nm <- ""

      for (i in g[-length(g)]) {
        nm <- sprintf("%s/%s", nm, i)
        if (!ptr$path_valid(path = nm)) {
          ptr <- ptr$create_group(i)
        } else {
          ptr <- ptr[[i]]
        }
      }

      # Dataset name
      nm <- g[length(g)]

      # Remove existing dataset if replace = TRUE
      if (ptr$path_valid(path = nm)) {
        if (replace) {
          ptr$link_delete(nm)
        } else {
          stop("Dataset already exists. Use replace = TRUE to overwrite.")
        }
      }

      # Determine dtype based on ctype
      if (ctype == "integer") {
        dtype <- ns$h5types$H5T_NATIVE_INT
      } else {
        dtype <- ns$h5types$H5T_NATIVE_DOUBLE
      }

      # Create dataspace with specified dimensions
      space <- ns$H5S$new(dims = dims, maxdims = dims)

      # Create dataset with pre-allocated space
      ptr$create_dataset(
        name = nm,
        dtype = dtype,
        space = space,
        chunk_dims = chunk,
        gzip_level = level
      )

      if (ptr$is_valid && inherits(ptr, "H5Group")) {
        ptr$close()
      }

      self$close(all = TRUE)
      invisible(self)
    },

    write_slice = function(x, start) {
      # Write data to a specific location in the dataset using hyperslab selection
      # start: 1-based index vector (i, j, k, ...) for the starting position
      # x: data to write

      if (private$read_only) {
        stop("File is read-only. Cannot write to dataset.")
      }

      # Ensure start is a vector
      if (length(start) == 1 && !is.null(dim(x))) {
        stop("start must have the same number of dimensions as the data")
      }
      start <- as.integer(start)

      # Get data dimensions
      if (is.null(dim(x))) {
        x_dims <- length(x)
      } else {
        x_dims <- dim(x)
      }

      if (length(start) != length(x_dims)) {
        stop("start must have the same number of dimensions as the data")
      }

      # Open file and dataset
      if (is.null(private$file_ptr) || !private$file_ptr$is_valid) {
        if (!file.exists(private$file)) {
          stop("File does not exist. Call old_allocate_h5() first.")
        }
        private$file_ptr <- ns$H5File$new(private$file, "a")
      }

      if (!private$file_ptr$path_valid(private$name)) {
        self$close(all = TRUE)
        stop("Dataset does not exist. Call old_allocate_h5() first.")
      }

      private$data_ptr <- private$file_ptr[[private$name]]
      dataset_dims <- private$data_ptr$dims

      # Validate bounds
      end_idx <- start + x_dims - 1L
      if (any(end_idx > dataset_dims) || any(start < 1L)) {
        self$close(all = TRUE)
        stop(sprintf(
          "Write out of bounds: start=%s, count=%s, dataset dims=%s",
          paste(start, collapse = ","),
          paste(x_dims, collapse = ","),
          paste(dataset_dims, collapse = ",")
        ))
      }

      on.exit({
        self$close(all = TRUE)
      }, add = TRUE)

      # Use native HDF5 hyperslab selection for memory-efficient writing
      # This avoids creating large index vectors for big slices
      # hdf5r's select_hyperslab uses 1-based indexing (matching R convention)
      h5_start <- as.numeric(start)
      h5_count <- as.numeric(x_dims)

      # Get file dataspace and select hyperslab
      file_space <- private$data_ptr$get_space()
      file_space$select_hyperslab(start = h5_start, count = h5_count)

      # Create memory dataspace matching the data
      mem_space <- ns$H5S$new(dims = x_dims)

      # Write using low-level API with hyperslab selection
      private$data_ptr$write_low_level(
        robj = x,
        file_space = file_space,
        mem_space = mem_space
      )

      # Clean up spaces
      file_space$close()
      mem_space$close()

      invisible(self)
    }
  )
)

old_load_h5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE) {
  re <- tryCatch({
    re <- OldLazyH5$new(file_path = file, data_name = name, read_only = read_only, quiet = quiet)
    re$open()
    re
  }, error = function(e) {

    if (!read_only) {
      stop("Another process is locking the file. Cannot open file with write permission; use ", sQuote("old_save_h5"), " instead...\n  file: ", file, "\n  name: ", name)
    }
    if (!quiet) {
      message("Open failed. Attempt to open with a temporary copy...")
    }

    # Fails when other process holds a connection to it!
    # If read_only, then copy the file to local directory
    tmpf <- tempfile(fileext = "conflict.h5")
    file.copy(file, tmpf)
    OldLazyH5$new(file_path = tmpf, data_name = name, read_only = read_only)
  })

  if (ram) {
    f <- re
    re <- re[]
    f$close()
  }

  re
}

old_save_h5 <- function(
    x, file, name, chunk = "auto", level = 4, replace = TRUE,
    new_file = FALSE, ctype = NULL, quiet = FALSE, ...) {

  {
    f <- tryCatch({
      f <- OldLazyH5$new(file, name, read_only = FALSE, quiet = quiet)
      f$open()
      f$close()
      f
    }, error = function(e) {
      if ( !quiet ) {
        message("Saving failed. Attempt to unlink the file and retry...")
      }
      if (file.exists(file)) {
        # File is locked,
        tmpf <- tempfile(fileext = "conflict.w.h5")
        file.copy(file, tmpf)
        unlink(file, recursive = FALSE, force = TRUE)
        file.copy(tmpf, file)
        unlink(tmpf)
      }
      # Otherwise it's some weird error, or dirname not exists, expose the error
      OldLazyH5$new(file, name, read_only = FALSE)
    })
    on.exit({
      f$close(all = TRUE)
    }, add = TRUE)
    f$save(x, chunk = chunk, level = level, replace = replace, new_file = new_file, ctype = ctype, force = TRUE, ...)

  }

  return(invisible(normalizePath(file, mustWork = FALSE)))
}

old_allocate_h5 <- function(file, name, dims, chunk = "auto", level = 4,
                        replace = TRUE, new_file = FALSE, ctype = "numeric",
                        quiet = FALSE) {
  # Pre-allocate an HDF5 dataset with specified dimensions
  # dims: integer vector of dimensions
  # ctype: "numeric" (double) or "integer"
  dims <- as.integer(dims)
  if (any(dims <= 0)) {
    stop("dims must be positive integers")
  }

  {
    f <- tryCatch(
      {
        OldLazyH5$new(file, name, read_only = FALSE, quiet = quiet)
      },
      error = function(e) {
        if (!quiet) {
          message("Allocation failed. Attempting to unlink and retry...")
        }
        if (file.exists(file)) {
          tmpf <- tempfile(fileext = "conflict.w.h5")
          file.copy(file, tmpf)
          unlink(file, recursive = FALSE, force = TRUE)
          file.copy(tmpf, file)
          unlink(tmpf)
        }
        OldLazyH5$new(file, name, read_only = FALSE, quiet = quiet)
      }
    )
    on.exit(
      {
        f$close(all = TRUE)
      },
      add = TRUE
    )
    f$allocate(
      dims = dims, chunk = chunk, level = level,
      ctype = ctype, replace = replace, new_file = new_file
    )
  }

  return(invisible(normalizePath(file, mustWork = FALSE)))
}

old_write_h5_slice <- function(x, file, name, start, quiet = FALSE) {
  # Write data to a specific location in an existing HDF5 dataset
  # x: data to write
  # start: 1-based starting index (scalar for 1D, vector for nD)
  {
    f <- tryCatch(
      {
        OldLazyH5$new(file, name, read_only = FALSE, quiet = quiet)
      },
      error = function(e) {
        stop("Cannot open file for writing: ", e$message)
      }
    )
    on.exit(
      {
        f$close(all = TRUE)
      },
      add = TRUE
    )
    f$write_slice(x = x, start = start)
  }

  return(invisible(normalizePath(file, mustWork = FALSE)))
}

`[.OldLazyH5` <- function(obj, ...) {
  on.exit({ obj$close() }, add = TRUE)
  obj$subset(..., envir = parent.frame())
}

dim.OldLazyH5 <- function(x) {
  dim_info <- x$get_dims(stay_open = FALSE)
  if (length(dim_info) == 1) { dim_info <- NULL }
  dim_info
}

length.OldLazyH5 <- function(x) prod(x$get_dims())

# ---------------------------------------------------------------------------
# END historical hdf5r implementation
# ---------------------------------------------------------------------------


# Types both implementations can write. `hdf5r` cannot write complex at all
# (`create_dataset()` fails inside `H5Tget_super()`), so complex is covered
# separately below, new-writes-only.
shared_fixtures <- function() {
  list(
    dbl       = as.double(1:50) / 7,
    dbl_na    = c(1, NA_real_, NaN, Inf, -Inf),
    int       = 1:50,
    int_na    = c(1L, NA_integer_, 3L),
    lgl       = c(TRUE, FALSE),
    lgl_na    = c(TRUE, NA, FALSE),
    chr       = "{\"a\":1,\"b\":\"x\"}",
    chrv      = c("alpha", "beta", "gamma"),
    mat       = matrix(as.double(1:12), 3, 4),
    arr       = array(as.double(1:24), c(2, 3, 4)),
    empty     = numeric(0)
  )
}


test_that("data written by the old implementation reads back unchanged", {
  fixtures <- shared_fixtures()
  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)

  for (nm in names(fixtures)) {
    old_save_h5(fixtures[[nm]], tmp_file, nm, replace = TRUE, quiet = TRUE)
  }

  for (nm in names(fixtures)) {
    expect_identical(load_h5(tmp_file, nm, ram = TRUE), fixtures[[nm]], info = nm)
  }
  expect_setequal(h5_names(tmp_file), names(fixtures))
})


test_that("data written by readNSx reads back unchanged through the old implementation", {
  fixtures <- shared_fixtures()
  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)

  for (nm in names(fixtures)) {
    save_h5(fixtures[[nm]], tmp_file, nm, replace = TRUE, quiet = TRUE)
  }

  for (nm in names(fixtures)) {
    expect_identical(old_load_h5(tmp_file, nm, ram = TRUE), fixtures[[nm]], info = nm)
  }
})


test_that("both implementations produce the same HDF5 layout", {
  fixtures <- shared_fixtures()
  f_old <- tempfile(fileext = ".h5")
  f_new <- tempfile(fileext = ".h5")
  on.exit(unlink(c(f_old, f_new), force = TRUE), add = TRUE)

  # A scalar chunk is rejected by hdf5r for rank > 1, so size it per fixture.
  chunk_for <- function(x) {
    dm <- dim(x)
    if (is.null(dm)) { 8 } else { rep(2L, length(dm)) }
  }

  for (nm in names(fixtures)) {
    ch <- chunk_for(fixtures[[nm]])
    old_save_h5(fixtures[[nm]], f_old, nm, chunk = ch, level = 4, quiet = TRUE)
    save_h5(fixtures[[nm]], f_new, nm, chunk = ch, level = 4, quiet = TRUE)
  }

  describe <- function(path, nm) {
    h <- ns$H5File$new(path, "r")
    on.exit(try(h$close_all(), silent = TRUE), add = TRUE)
    d <- h[[nm]]
    list(dims = d$dims,
         type = gsub("[[:space:]]+", " ", d$get_type()$to_text()),
         chunk = d$get_create_plist()$get_chunk(length(d$dims)),
         maxdims = d$get_space()$get_simple_extent_dims()$maxdims)
  }

  for (nm in names(fixtures)) {
    a <- describe(f_old, nm)
    b <- describe(f_new, nm)
    expect_identical(b$dims, a$dims, info = paste(nm, "dims"))
    expect_identical(b$chunk, a$chunk, info = paste(nm, "chunk"))
    expect_identical(b$maxdims, a$maxdims, info = paste(nm, "maxdims"))
    if (storage.mode(fixtures[[nm]]) == "character") {
      # Deliberate divergence: readNSx labels strings CSET_UTF8 so that
      # non-ASCII survives. hdf5r labels everything ASCII and then cannot read
      # its own non-ASCII output back.
      expect_match(b$type, "STRSIZE H5T_VARIABLE", info = nm)
      expect_match(b$type, "CSET H5T_CSET_UTF8", info = nm)
      expect_match(a$type, "CSET H5T_CSET_ASCII", info = nm)
    } else {
      expect_identical(b$type, a$type, info = paste(nm, "type"))
    }
  }
})


test_that("pre-allocated datasets and slice writes match the old implementation", {
  set.seed(2)
  expected <- rnorm(400)
  f_old <- tempfile(fileext = ".h5")
  f_new <- tempfile(fileext = ".h5")
  on.exit(unlink(c(f_old, f_new), force = TRUE), add = TRUE)

  for (spec in list(list(f = f_old, alloc = old_allocate_h5, write = old_write_h5_slice),
                    list(f = f_new, alloc = allocate_h5,     write = write_h5_slice))) {
    spec$alloc(spec$f, "data", dims = 400, chunk = 64, level = 4,
               ctype = "numeric", new_file = TRUE, quiet = TRUE)
    for (i in sample(seq_len(8))) {
      spec$write(expected[((i - 1) * 50 + 1):(i * 50)], spec$f, "data",
                 start = (i - 1) * 50 + 1, quiet = TRUE)
    }
  }

  # Each implementation must read what the other wrote.
  expect_equal(load_h5(f_old, "data", ram = TRUE), expected)
  expect_equal(old_load_h5(f_new, "data", ram = TRUE), expected)
})


test_that("degenerate dimensions are stored identically; only the old reader drops them", {
  # Both writers put 1x2x3x1x4x1 on disk. The old *reader* collapses the
  # length-1 dimensions to 2x3x4 whichever implementation wrote the file, so
  # this is a limitation of the old reader rather than a format difference.
  x <- array(as.double(1:24), c(1, 2, 3, 1, 4, 1))
  f_old <- tempfile(fileext = ".h5")
  f_new <- tempfile(fileext = ".h5")
  on.exit(unlink(c(f_old, f_new), force = TRUE), add = TRUE)
  old_save_h5(x, f_old, "a", quiet = TRUE)
  save_h5(x, f_new, "a", quiet = TRUE)

  file_dims <- function(path) {
    h <- ns$H5File$new(path, "r")
    on.exit(try(h$close_all(), silent = TRUE), add = TRUE)
    h[["a"]]$dims
  }
  expect_identical(file_dims(f_new), file_dims(f_old))
  expect_equal(file_dims(f_new), c(1, 2, 3, 1, 4, 1))

  # The new reader keeps the shape, from either file ...
  expect_identical(load_h5(f_old, "a", ram = TRUE), x)
  expect_identical(load_h5(f_new, "a", ram = TRUE), x)
  # ... and the old reader drops it, from either file.
  expect_equal(dim(old_load_h5(f_old, "a", ram = TRUE)), c(2L, 3L, 4L))
  expect_equal(dim(old_load_h5(f_new, "a", ram = TRUE)), c(2L, 3L, 4L))
})


test_that("complex is stored so that hdf5r and h5py both decode it", {
  # The old implementation cannot write complex at all, so this is one-way:
  # readNSx writes, hdf5r reads. `{r, i}` is numpy's complex128 layout.
  z <- c(1 + 4i, NA_complex_, 3 - 1i, complex(real = NaN, imaginary = Inf))
  zm <- matrix(complex(real = 1:6, imaginary = 6:1), 2, 3)

  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)
  save_h5(z, tmp_file, "z", quiet = TRUE)
  save_h5(zm, tmp_file, "zm", quiet = TRUE)

  expect_identical(load_h5(tmp_file, "z", ram = TRUE), z)
  expect_identical(load_h5(tmp_file, "zm", ram = TRUE), zm)

  h <- ns$H5File$new(tmp_file, "r")
  on.exit(try(h$close_all(), silent = TRUE), add = TRUE)
  expect_match(gsub("[[:space:]]+", " ", h[["z"]]$get_type()$to_text()),
               'H5T_COMPOUND \\{ H5T_IEEE_F64LE "r" : 0; H5T_IEEE_F64LE "i" : 8; \\}')
  # hdf5r decodes an {r, i} compound as a two-column data.frame; recombining it
  # has to give the original values back.
  parts <- h[["z"]]$read()
  expect_identical(complex(real = parts$r, imaginary = parts$i), z)
})


test_that("hdf5r's own {Real, Imaginary} complex layout also reads as complex", {
  # hdf5r cannot write into such a dataset, but it can create one, and files
  # from other tools use those member names.
  tmp_file <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp_file, force = TRUE), add = TRUE)
  h <- ns$H5File$new(tmp_file, "a")
  h$create_dataset("z", dtype = ns$H5T_COMPLEX$new(), space = ns$H5S$new(dims = 3))
  h$close_all()

  # Member names differ from ours, and HDF5 matches compound members by name,
  # so this only works because the reader adopts the file's own names.
  expect_identical(load_h5(tmp_file, "z", ram = TRUE), complex(3))
})


test_that("non-ASCII strings survive, which they do not through hdf5r", {
  s <- c("plain", "caf\u00e9 \u00b5V", "\u4e2d\u6587")
  Encoding(s) <- "UTF-8"

  f_new <- tempfile(fileext = ".h5")
  f_old <- tempfile(fileext = ".h5")
  on.exit(unlink(c(f_new, f_old), force = TRUE), add = TRUE)
  save_h5(s, f_new, "s", quiet = TRUE)
  old_save_h5(s, f_old, "s", quiet = TRUE)

  expect_identical(load_h5(f_new, "s", ram = TRUE), s)

  h <- ns$H5File$new(f_new, "r")
  on.exit(try(h$close_all(), silent = TRUE), add = TRUE)
  expect_identical(h[["s"]]$read(), s)

  # The old writer mislabels UTF-8 bytes as ASCII and cannot read them back;
  # asserted so that the divergence is a recorded decision, not an accident.
  expect_error(old_load_h5(f_old, "s", ram = TRUE))
})


test_that("NA_character_ survives, which it does not through hdf5r", {
  s <- c("a", NA_character_, "c")
  f_new <- tempfile(fileext = ".h5")
  f_old <- tempfile(fileext = ".h5")
  on.exit(unlink(c(f_new, f_old), force = TRUE), add = TRUE)
  save_h5(s, f_new, "s", quiet = TRUE)
  old_save_h5(s, f_old, "s", quiet = TRUE)

  # readNSx stores a real HDF5 NULL, so the NA comes back as an NA.
  expect_identical(load_h5(f_new, "s", ram = TRUE), s)

  # The old writer turns it into the literal text "NA" ...
  expect_identical(old_load_h5(f_old, "s", ram = TRUE), c("a", "NA", "c"))
  # ... and reads readNSx's NULL back as an empty string.
  expect_identical(old_load_h5(f_new, "s", ram = TRUE), c("a", "", "c"))
})
