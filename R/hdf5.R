
# HDF5 storage helpers.
#
# All I/O goes through the C++ layer in `src/h5native.cpp`, which is linked
# against `hdf5lib` and therefore always available - no system HDF5 and no
# other R package is involved.
#
# The on-disk layout is the one `hdf5r` established: the HDF5 dataspace
# dimensions are the reverse of the R dimensions, and R's column-major buffer is
# stored verbatim. Files written by earlier versions of readNSx read back
# unchanged, and files written now stay readable by `hdf5r`, `h5py`, and the
# HDF5 command-line tools.
#
# Dimensions, offsets and counts are in R order and 1-based throughout, and are
# carried as doubles so that datasets longer than 2^31 elements work.


# `cpp11` will not coerce, so everything crossing into C++ has to be a double.
as_h5_index <- function(x) {
  x <- as.numeric(x)
  if (anyNA(x)) {
    stop("readNSx: HDF5 dimensions and offsets must not be missing")
  }
  x
}

# "auto" - or anything that is not a positive number - lets the C++ layer pick
# the chunk size, which it is told by passing NA.
as_h5_chunk <- function(chunk) {
  if (!length(chunk) || !is.numeric(chunk)) { return(NA_real_) }
  chunk <- as.numeric(chunk)
  if (anyNA(chunk) || any(chunk < 1)) { return(NA_real_) }
  chunk
}

# R dimensions of `x`, treating a dimensionless vector as rank 1.
r_dims_of <- function(x) {
  dm <- dim(x)
  if (is.null(dm)) { return(length(x)) }
  as.numeric(dm)
}

storage_from_ctype <- function(ctype) {
  if (!length(ctype) || !is.character(ctype)) { return(NULL) }
  switch(
    ctype[[1]],
    "numeric" = "double",
    "float" = "double",
    "string" = "character",
    "cplx" = "complex",
    ctype[[1]]
  )
}

# `ctype` names the type the caller wants stored. Coercing here keeps the stored
# type independent of how the caller happened to build `x`.
coerce_ctype <- function(x, ctype) {
  target <- storage_from_ctype(ctype)
  if (is.null(target) || identical(storage.mode(x), target)) { return(x) }
  if (identical(target, "character")) { return(as.character(x)) }
  storage.mode(x) <- target
  x
}


# Lazy handle to one dataset inside an HDF5 file. No file handle is held between
# calls, so nothing here locks a file for other readers.
LazyH5Internal <- R6::R6Class(
  classname = "LazyH5Internal",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    file = NULL,
    name = NULL,
    read_only = TRUE,
    last_dim = NULL,
    finalize = function() {
      self$close(all = TRUE)
    }
  ),
  public = list(

    quiet = FALSE,

    print = function() {
      base::cat(
        sep = "",
        "<HDF5 dataset>\n",
        sprintf("  File   : %s\n", private$file),
        sprintf("  Dataset: %s\n", private$name),
        sprintf("  Dim    : %s\n", paste(private$last_dim, collapse = "x"))
      )
      invisible(self)
    },
    initialize = function(file_path, data_name, read_only = FALSE, quiet = FALSE) {

      # First get absolute path, otherwise HDF5 may report file not found
      if (read_only) {
        private$file <- normalizePath(file_path)

        if (!h5_native_is_h5(private$file)) {
          stop("File is not a valid HDF5 file")
        }
      } else {
        private$file <- normalizePath(file_path, mustWork = FALSE)
      }
      self$quiet <- isTRUE(quiet)
      private$name <- data_name
      private$read_only <- read_only
    },

    save = function(x, chunk = "auto", level = 7, replace = TRUE,
                    new_file = FALSE, force = TRUE, ctype = NULL, size = NULL,
                    ...) {
      # `size` is deprecated but kept in case of compatibility issues
      if (private$read_only && !force) {
        stop('File is read-only. Use "force=TRUE"')
      }

      if (new_file && file.exists(private$file)) {
        file.remove(private$file)
      }

      x <- coerce_ctype(x, ctype)
      h5_native_write(private$file, private$name, x, as_h5_chunk(chunk),
                      as.integer(level), replace)

      private$last_dim <- r_dims_of(x)
      invisible(self)
    },


    # Kept for backwards compatibility: validates that the dataset can be
    # reached and caches its dimensions. No connection is left open.
    open = function(new_dataset = FALSE, robj, ...) {

      has_data <- h5_native_exists(private$file, private$name)

      if (private$read_only) {
        if (!has_data) {
          stop(sprintf(
            "File [%s] has no [%s] in it.",
            private$file, private$name
          ))
        }
      } else if (!h5_writable(private$file)) {
        stop(sprintf("Cannot open file [%s] for writing.", private$file))
      }

      if (has_data) {
        private$last_dim <- h5_native_dims(private$file, private$name)
      }

      invisible(self)
    },


    close = function(all = TRUE) {
      # Every operation opens and closes the file; nothing is held to release.
      invisible()
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
        return(h5_native_read(private$file, private$name))
      }
      args <- lapply(args, function(x) {
        if (x == "") {
          return(x)
        } else {
          return(eval(x, envir = envir))
        }
      })

      # step 2: get allocation size
      alloc_dim <- vapply(seq_along(dims), function(ii) {
        if (is.logical(args[[ii]])) {
          return(as.integer(sum(args[[ii]])))
        } else if (is.numeric(args[[ii]])) {
          return(as.integer(length(args[[ii]])))
        } else {
          # must be blank "", otherwise raise error
          return(as.integer(dims[ii]))
        }
      }, integer(1L))

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

      # A request for one contiguous block per dimension - the common
      # `channel$data[a:b]` case - is served straight from disk instead of
      # reading the whole dataset first.
      block <- h5_contiguous_block(legit_args, dims)
      if (is.null(block)) {
        selected <- do.call("[", c(
          list(h5_native_read(private$file, private$name)),
          legit_args, list(drop = FALSE)
        ))
      } else {
        selected <- h5_native_read_slab(
          private$file, private$name,
          start = as_h5_index(block$start), count = as_h5_index(block$count)
        )
      }

      re <- do.call(`[<-`, c(list(re), mapping, list(value = selected)))

      if (drop) {
        return(drop(re))
      } else {
        return(re)
      }
    },

    get_dims = function(stay_open = TRUE) {
      private$last_dim <- h5_native_dims(private$file, private$name)
      private$last_dim
    },

    allocate = function(dims, chunk = "auto", level = 4, ctype = "numeric",
                        replace = TRUE, new_file = FALSE) {
      # Pre-allocate an HDF5 dataset with specified dimensions
      # without loading data into memory
      if (private$read_only) {
        stop("File is read-only. Cannot allocate dataset.")
      }

      if (new_file && file.exists(private$file)) {
        file.remove(private$file)
      }

      h5_native_allocate(private$file, private$name, as_h5_index(dims), ctype,
                         as_h5_chunk(chunk), as.integer(level), replace)

      private$last_dim <- dims
      invisible(self)
    },

    write_slice = function(x, start) {
      # Write data to a specific location in the dataset using hyperslab
      # selection
      # start: 1-based index vector (i, j, k, ...) for the starting position
      # x: data to write

      if (private$read_only) {
        stop("File is read-only. Cannot write to dataset.")
      }

      # Ensure start is a vector
      if (length(start) == 1 && !is.null(dim(x))) {
        stop("start must have the same number of dimensions as the data")
      }
      start <- as_h5_index(start)

      # Get data dimensions
      x_dims <- r_dims_of(x)

      if (length(start) != length(x_dims)) {
        stop("start must have the same number of dimensions as the data")
      }

      if (!file.exists(private$file)) {
        stop("File does not exist. Call allocate_h5() first.")
      }
      if (!h5_native_exists(private$file, private$name)) {
        stop("Dataset does not exist. Call allocate_h5() first.")
      }

      dataset_dims <- h5_native_dims(private$file, private$name)

      # Validate bounds
      end_idx <- start + x_dims - 1
      if (any(end_idx > dataset_dims) || any(start < 1)) {
        stop(sprintf(
          "Write out of bounds: start=%s, count=%s, dataset dims=%s",
          paste(start, collapse = ","),
          paste(x_dims, collapse = ","),
          paste(dataset_dims, collapse = ",")
        ))
      }

      h5_native_write_slab(private$file, private$name, x, start)

      invisible(self)
    }
  )
)

# Are `legit_args` a single contiguous block? Returns the R-order start/count of
# that block, or NULL when the request has to go through a full read instead.
h5_contiguous_block <- function(legit_args, dims) {
  if (length(legit_args) != length(dims)) { return(NULL) }

  start <- numeric(length(dims))
  count <- numeric(length(dims))

  # A blank index is the empty symbol. Binding it to a local variable would make
  # that variable "missing", so each element is only ever touched in place.
  for (ii in seq_along(dims)) {
    if (is.numeric(legit_args[[ii]])) {
      n <- length(legit_args[[ii]])
      if (n == 0 || anyNA(legit_args[[ii]])) { return(NULL) }
      if (n > 1 && any(diff(legit_args[[ii]]) != 1)) { return(NULL) }
      start[[ii]] <- legit_args[[ii]][[1]]
      count[[ii]] <- n
    } else if (is.logical(legit_args[[ii]])) {
      # Logical indices recycle in ways a hyperslab cannot express; fall back to
      # reading the dataset and subsetting in R.
      return(NULL)
    } else {
      # blank index: the whole dimension
      start[[ii]] <- 1
      count[[ii]] <- dims[[ii]]
    }
  }

  list(start = start, count = count)
}

#' @export
`[.LazyH5Internal` <- function(obj, ...) {
  on.exit({obj$close()}, add = TRUE)
  obj$subset(..., envir = parent.frame())
}

#' @export
dim.LazyH5Internal <- function(x) {
  dim_info <- x$get_dims(stay_open = FALSE)
  if (length(dim_info) == 1) {
    dim_info <- NULL
  }
  dim_info
}

#' @export
length.LazyH5Internal <- function(x) {
  dim_info <- x$get_dims()
  prod(dim_info)
}

#' @export
as.array.LazyH5Internal <- function(x, ...) {
  as.array(x$subset(), ...)
}

# Can the file be opened for writing? Creates an empty HDF5 file when none
# exists yet.
h5_writable <- function(file) {
  if (!dir.exists(dirname(file))) { return(FALSE) }
  tryCatch(
    h5_native_writable(normalizePath(file, mustWork = FALSE)),
    error = function(e) FALSE
  )
}

load_h5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE) {

  re <- tryCatch({
    re <- LazyH5Internal$new(file_path = file, data_name = name, read_only = read_only, quiet = quiet)
    re$open()
    re
  }, error = function(e) {

    if (!read_only) {
      stop("Another process is locking the file. Cannot open file with write permission; use ", sQuote("save_h5"), " instead...\n  file: ", file, "\n  name: ", name)
    }
    if (!quiet) {
      message("Open failed. Attempt to open with a temporary copy...")
    }

    # Fails when other process holds a connection to it!
    # If read_only, then copy the file to local directory
    tmpf <- tempfile(fileext = "conflict.h5")
    file.copy(file, tmpf)
    tryCatch({
      LazyH5Internal$new(file_path = tmpf, data_name = name, read_only = read_only)
    }, error = function(e2) {
      stop(e)
    })
  })

  if (ram) {
    f <- re
    re <- re[]
    f$close()
  }

  re
}


save_h5 <- function(
    x, file, name, chunk = "auto", level = 4, replace = TRUE,
    new_file = FALSE, ctype = NULL, quiet = FALSE, ...) {

  f <- tryCatch({
    f <- LazyH5Internal$new(file, name, read_only = FALSE, quiet = quiet)
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
    LazyH5Internal$new(file, name, read_only = FALSE)
  })
  on.exit({
    f$close(all = TRUE)
  }, add = TRUE)
  f$save(x, chunk = chunk, level = level, replace = replace, new_file = new_file, ctype = ctype, force = TRUE, ...)

  return(invisible(normalizePath(file, mustWork = FALSE)))
}


allocate_h5 <- function(file, name, dims, chunk = "auto", level = 4,
                        replace = TRUE, new_file = FALSE, ctype = "numeric",
                        quiet = FALSE) {
  # Pre-allocate an HDF5 dataset with specified dimensions
  # dims: numeric vector of dimensions; kept as double so that a dataset may
  #       hold more than 2^31 elements
  # ctype: "numeric" (double) or "integer"
  dims <- as.numeric(dims)
  if (anyNA(dims) || any(dims <= 0)) {
    stop("dims must be positive integers")
  }

  f <- tryCatch(
    {
      LazyH5Internal$new(file, name, read_only = FALSE, quiet = quiet)
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
      LazyH5Internal$new(file, name, read_only = FALSE, quiet = quiet)
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

  return(invisible(normalizePath(file, mustWork = FALSE)))
}


write_h5_slice <- function(x, file, name, start, quiet = FALSE) {
  # Write data to a specific location in an existing HDF5 dataset
  # x: data to write
  # start: 1-based starting index (scalar for 1D, vector for nD)
  f <- tryCatch(
    {
      LazyH5Internal$new(file, name, read_only = FALSE, quiet = quiet)
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

  return(invisible(normalizePath(file, mustWork = FALSE)))
}


h5_valid <- function(file, mode = c("r", "w"), close_all = FALSE) {
  mode <- match.arg(mode)

  if (!h5FileValid(file)) { return(FALSE) }
  if (mode == "r") { return(TRUE) }

  h5_writable(file)
}

h5_names <- function(file) {
  if (!file.exists(file)) { return(character()) }
  if (!h5_native_is_h5(normalizePath(file, mustWork = FALSE))) {
    return(character())
  }
  unique(h5_native_list_datasets(normalizePath(file, mustWork = FALSE)))
}

h5FileValid <- function(filename) {
  if (!length(filename)) { return(FALSE) }
  filename <- filename[[1]]
  if (!file.exists(filename)) { return(FALSE) }
  if (isTRUE(file.info(filename)[["isdir"]])) { return(FALSE) }
  filename <- normalizePath(filename)
  return(tryCatch({
    h5_native_is_h5(filename)
  }, error = function(e) { FALSE }))
}


load_h5_all <- function(file, ram = FALSE) {
  file <- normalizePath(file, mustWork = TRUE)
  # Check if the file is HDF5 format
  if ( h5FileValid(file) ) {

    dset_names <- h5_names(file)
    re <- structure(
      new.env(parent = emptyenv()),
      class = c("readNSx_h5_datasets", "readNSx_printable", "environment")
    )
    lapply(dset_names, function(nm) {
      y <- load_h5(file, name = nm, ram = ram)
      nm_path <- strsplit(nm, "/", fixed = TRUE)[[1]]
      d <- re
      for (ii in seq_along(nm_path)) {
        nm <- nm_path[[ii]]
        if (ii != length(nm_path)) {
          if (!exists(nm, envir = d)) {
            d[[nm]] <- structure(
              new.env(parent = emptyenv()),
              class = c("readNSx_h5_datasets", "readNSx_printable", "environment")
            )
          }
          d <- d[[nm]]
        } else {
          d[[nm]] <- y
        }
      }
      NULL
    })

  } else {
    re <- NULL
  }
  re
}
