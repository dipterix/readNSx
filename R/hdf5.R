
LazyH5Internal <- R6::R6Class(
  classname = 'LazyH5Internal',
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    file = NULL,
    name = NULL,
    read_only = TRUE,
    data_ptr = NULL,
    file_ptr = NULL,
    last_dim = NULL,
    finalize = function(){
      self$close(all = TRUE)
    }
  ),
  public = list(

    quiet = FALSE,

    print = function(){
      if(!is.null(private$data_ptr)){
        if(private$data_ptr$is_valid){
          base::print(private$data_ptr)
        }else{
          base::cat('Pointer closed. Information since last open:\nDim: ',
                    paste(private$last_dim, collapse = 'x'), ' \tRank: ',
                    length(private$last_dim), "\n")
        }
      }
      invisible(self)
    },
    initialize = function(file_path, data_name, read_only = FALSE, quiet = FALSE){

      # First get absolute path, otherwise hdf5r may report file not found error
      if(read_only){
        private$file <- normalizePath(file_path)

        if(!hdf5r::is_hdf5(private$file)) {
          stop("File is not a valid HDF5 file")
        }
      }else{
        file_path <- normalizePath(file_path, mustWork = FALSE)
        private$file <- file_path
      }
      self$quiet <- isTRUE(quiet)
      private$name <- data_name
      private$read_only <- read_only
    },

    save = function(x, chunk = 'auto', level = 7, replace = TRUE,
                    new_file = FALSE, force = TRUE, ctype = NULL, size = NULL,
                    ...){
      # ctype and size is deprecated but kept in case of compatibility issues
      # ptr$create_dataset =
      # function (name, robj = NULL, dtype = NULL, space = NULL, dims = NULL,
      #           chunk_dims = "auto", gzip_level = 4, link_create_pl = h5const$H5P_DEFAULT,
      #           dataset_create_pl = h5const$H5P_DEFAULT, dataset_access_pl = h5const$H5P_DEFAULT)
      if(private$read_only){
        if(!force){
          stop('File is read-only. Use "force=TRUE"')
        }else{
          # Close current pointer
          self$close(all = TRUE)
          private$read_only <- FALSE

          on.exit({
            self$close(all = TRUE)
            private$read_only <- TRUE
          }, add = TRUE, after = FALSE)
        }
      }

      if(new_file && file.exists(private$file)){
        self$close(all = TRUE)
        file.remove(private$file)
      }

      self$open(new_dataset = replace, robj = x, chunk = chunk, gzip_level = level, ...)

      self$close(all = TRUE)

    },


    open = function(new_dataset = FALSE, robj, ...){

      # check data pointer
      # if valid, no need to do anything, otherwise, enter if clause
      if(new_dataset || is.null(private$data_ptr) || !private$data_ptr$is_valid){

        # Check if file is valid,
        if(is.null(private$file_ptr) || !private$file_ptr$is_valid){
          # if no, create new link
          mode <- ifelse(private$read_only, 'r', 'a')
          tryCatch({
            private$file_ptr <- hdf5r::H5File$new(private$file, mode)
          }, error = function(e){
            # Open for writting, we should close all connections first
            # then the file can be opened, otherwise, Access type: H5F_ACC_RDONLY
            # will lock the file for writting
            f <- hdf5r::H5File$new(private$file, 'r')
            if(!self$quiet){
              message('Closing all other connections to [{private$file}] - {f$get_obj_count() - 1}')
            }

            try({ f$close_all() }, silent = TRUE)
            private$file_ptr <- hdf5r::H5File$new(private$file, mode)
          })
        }

        has_data <- private$file_ptr$path_valid(private$name)

        if(!private$read_only && (new_dataset || ! has_data)){
          # need to create new dataset
          g <- strsplit(private$name, split = "/")[[1]]
          g <- g[trimws(g) != '']

          ptr <- private$file_ptr
          nm <- ''

          for(i in g[-length(g)]){
            nm <- sprintf('%s/%s', nm, i)
            if(!ptr$path_valid(path = nm)){
              ptr <- ptr$create_group(i)
            }else{
              ptr <- ptr[[i]]
            }
          }

          # create dataset
          nm <- g[length(g)]
          if(ptr$path_valid(path = nm)){
            # dataset exists, unlink first
            ptr$link_delete(nm)
          }
          # new create
          if(missing(robj)){
            robj <- NA
          }
          ptr$create_dataset(nm, robj = robj, ...)
          if(ptr$is_valid && inherits(ptr, 'H5Group')){
            ptr$close()
          }
        }else if(!has_data){
          stop(sprintf(
            'File [%s] has no [%s] in it.',
            private$file, private$name
          ))
        }

        private$data_ptr <- private$file_ptr[[private$name]]

      }

      private$last_dim <- private$data_ptr$dims

    },


    close = function(all = TRUE){
      try({
        # check if data link is valid
        if(!is.null(private$data_ptr) && private$data_ptr$is_valid){
          private$data_ptr$close()
        }

        # if file link is valid, get_obj_ids() should return a vector of 1
        if(all && !is.null(private$file_ptr) && private$file_ptr$is_valid){
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
      if(length(args) == 0 || (length(args) == 1 && args[[1]] == '')){
        return(private$data_ptr$read())
      }
      args <- lapply(args, function(x){
        if(x == ''){
          return(x)
        }else{
          return(eval(x, envir = envir))
        }
      })

      # step 2: get allocation size
      alloc_dim <- sapply(seq_along(dims), function(ii){
        if(is.logical(args[[ii]])){
          return(sum(args[[ii]]))
        }else if(is.numeric(args[[ii]])){
          return(length(args[[ii]]))
        }else{
          # must be blank '', otherwise raise error
          return(dims[ii])
        }
      })

      # step 3: get legit indices
      legit_args <- lapply(seq_along(dims), function(ii){
        if(is.logical(args[[ii]])){
          return(args[[ii]])
        }else if(is.numeric(args[[ii]])){
          return(
            args[[ii]][args[[ii]] <= dims[ii] & args[[ii]] > 0]
          )
        }else{
          return(args[[ii]])
        }
      })

      # step 4: get mapping
      mapping <- lapply(seq_along(dims), function(ii){
        if(is.logical(args[[ii]])){
          return(
            rep(TRUE, sum(args[[ii]]))
          )
        }else if(is.numeric(args[[ii]])){
          return(args[[ii]] <= dims[ii] & args[[ii]] > 0)
        }else{
          return(args[[ii]])
        }
      })

      # alloc space
      re <- array(NA, dim = alloc_dim)

      if(stream){
        re <- do.call(`[<-`, c(list(re), mapping, list(
          value = private$data_ptr$read(
            args = legit_args,
            drop = FALSE,
            envir = environment()
          )
        )))
      }else{
        re <- do.call(`[<-`, c(list(re), mapping, list(
          value = do.call('[', c(list(private$data_ptr$read()), legit_args, list(drop = FALSE)))
        )))
      }

      self$close(all = !private$read_only)


      if(drop){
        return(drop(re))
      }else{
        return(re)
      }
    },

    get_dims = function(stay_open = TRUE){
      self$open()
      re <- private$data_ptr$dims
      if(!stay_open){
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
        private$file_ptr <- hdf5r::H5File$new(private$file, "a")
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
        dtype <- hdf5r::h5types$H5T_NATIVE_INT
      } else {
        dtype <- hdf5r::h5types$H5T_NATIVE_DOUBLE
      }

      # Create dataspace with specified dimensions
      space <- hdf5r::H5S$new(dims = dims, maxdims = dims)

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
          stop("File does not exist. Call allocate_h5() first.")
        }
        private$file_ptr <- hdf5r::H5File$new(private$file, "a")
      }

      if (!private$file_ptr$path_valid(private$name)) {
        self$close(all = TRUE)
        stop("Dataset does not exist. Call allocate_h5() first.")
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
      mem_space <- hdf5r::H5S$new(dims = x_dims)

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

#' @export
`[.LazyH5Internal` <- function(obj, ...){
  on.exit({obj$close()}, add = TRUE)
  obj$subset(..., envir = parent.frame())
}

#' @export
dim.LazyH5Internal <- function(x){
  dim_info <- x$get_dims(stay_open = FALSE)
  if(length(dim_info) == 1){
    dim_info <- NULL
  }
  dim_info
}

#' @export
length.LazyH5Internal <- function(x){
  dim_info <- x$get_dims()
  prod(dim_info)
}

#' @export
as.array.LazyH5Internal <- function(x, ...){
  as.array(x$subset(), ...)
}

hdf5r_installed <- function() {
  system.file(package = "hdf5r") != ""
  # FALSE
}

load_h5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE) {
  if(endsWith(tolower(file), ".ralt")) {
    file <- gsub("\\.ralt", "", file, ignore.case = TRUE)
  }

  if(dir.exists(sprintf("%s.ralt", file))) {
    tryCatch({
      suppressWarnings({
        re <- load_fakeh5(file = file, name = name, read_only = read_only, quiet = quiet, ram = ram)
        return(re)
      })
    }, error = function(...) {})
  }

  re <- tryCatch({
    re <- LazyH5Internal$new(file_path = file, data_name = name, read_only = read_only, quiet = quiet)
    re$open()
    re
  }, error = function(e){

    if(!read_only){
      stop('Another process is locking the file. Cannot open file with write permission; use ', sQuote('save_h5'), ' instead...\n  file: ', file, '\n  name: ', name)
    }
    if(!quiet){
      message('Open failed. Attempt to open with a temporary copy...')
    }

    # Fails when other process holds a connection to it!
    # If read_only, then copy the file to local directory
    tmpf <- tempfile(fileext = 'conflict.h5')
    file.copy(file, tmpf)
    LazyH5Internal$new(file_path = tmpf, data_name = name, read_only = read_only)
  })

  if(ram){
    f <- re
    re <- re[]
    f$close()
  }

  re
}


save_h5 <- function(x, file, name, chunk = 'auto', level = 4,replace = TRUE,
                    new_file = FALSE, ctype = NULL, quiet = FALSE, ...){
  if(endsWith(tolower(file), ".ralt")) {
    file <- gsub("\\.ralt", "", file, ignore.case = TRUE)
  }

  if( hdf5r_installed() ) {
    f <- tryCatch({
      f <- LazyH5Internal$new(file, name, read_only = FALSE, quiet = quiet)
      f$open()
      f$close()
      f
    }, error = function(e){
      if( !quiet ){
        message('Saving failed. Attempt to unlink the file and retry...')
      }
      if(file.exists(file)){
        # File is locked,
        tmpf <- tempfile(fileext = 'conflict.w.h5')
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

  } else {
    save_fakeh5(
      x = x, file = file, name = name, chunk = chunk,
      level = level, replace = replace, new_file = new_file,
      ctype = ctype, quiet = TRUE, ...)
  }

  return(invisible(normalizePath(file, mustWork = FALSE)))
}


allocate_h5 <- function(file, name, dims, chunk = "auto", level = 4,
                        replace = TRUE, new_file = FALSE, ctype = "numeric",
                        quiet = FALSE) {
  # Pre-allocate an HDF5 dataset with specified dimensions
  # dims: integer vector of dimensions
  # ctype: "numeric" (double) or "integer"
  if (endsWith(tolower(file), ".ralt")) {
    file <- gsub("\\.ralt", "", file, ignore.case = TRUE)
  }

  dims <- as.integer(dims)
  if (any(dims <= 0)) {
    stop("dims must be positive integers")
  }

  if (hdf5r_installed()) {
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
  } else {
    allocate_fakeh5(
      file = file, name = name, dims = dims, chunk = chunk,
      level = level, replace = replace, new_file = new_file,
      ctype = ctype, quiet = quiet
    )
  }

  return(invisible(normalizePath(file, mustWork = FALSE)))
}


write_h5_slice <- function(x, file, name, start, quiet = FALSE) {
  # Write data to a specific location in an existing HDF5 dataset
  # x: data to write
  # start: 1-based starting index (scalar for 1D, vector for nD)
  if (endsWith(tolower(file), ".ralt")) {
    file <- gsub("\\.ralt", "", file, ignore.case = TRUE)
  }

  if (hdf5r_installed()) {
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
  } else {
    write_fakeh5_slice(
      x = x, file = file, name = name, start = start, quiet = quiet
    )
  }

  return(invisible(normalizePath(file, mustWork = FALSE)))
}


h5_valid <- function(file, mode = c('r', 'w'), close_all = FALSE){
  if(endsWith(tolower(file), ".ralt")) {
    file <- gsub("\\.ralt", "", file, ignore.case = TRUE)
  }

  mode <- match.arg(mode)

  re <- FALSE
  if( hdf5r_installed() ) {
    re <- tryCatch({
      file <- normalizePath(file, mustWork = TRUE)
      f <- hdf5r::H5File$new(filename = file, mode = mode)
      if(close_all){
        f$close_all()
      } else {
        f$close()
      }
      TRUE
    }, error = function(e){
      FALSE
    })
  } else {
    re <- dir.exists(sprintf("%s.ralt", file))
  }
  re

}

h5_names <- function(file){
  if(endsWith(tolower(file), ".ralt")) {
    file <- gsub("\\.ralt", "", file, ignore.case = TRUE)
  }

  # make sure the file is valid
  if(!h5_valid(file, 'r')){ return(character()) }
  file <- normalizePath(file, mustWork = FALSE)
  if( hdf5r_installed() && file.exists(file)) {
    f <- hdf5r::H5File$new(filename = file, mode = 'r')
    names <- hdf5r::list.datasets(f)
    f$close()
  }
  names <- c(names, fakehh5_names(file))

  unique(names)
}

h5FileValid <- function(filename){
  if(!length(filename)){ return(FALSE) }
  filename <- filename[[1]]
  if(!file.exists(filename)){ return(FALSE) }
  if(isTRUE(file.info(filename)[['isdir']])){ return(FALSE) }
  filename <- normalizePath(filename)
  return(tryCatch({
    hdf5r::is.h5file(filename)
  }, error = function(e){ FALSE }))
}


load_h5_all <- function(file, ram = FALSE){
  file <- normalizePath(file, mustWork = TRUE)
  # Check if the file is HDF5 format
  if( h5FileValid(file) ){

    dset_names <- h5_names(file)
    re <- structure(
      new.env(parent = emptyenv()),
      class = c("readNSx_h5_datasets", "readNSx_printable", "environment")
    )
    lapply(dset_names, function(nm){
      y <- load_h5(file, name = nm, ram = ram)
      nm_path <- strsplit(nm, "/")[[1]]
      d <- re
      for(ii in seq_along(nm_path)){
        nm <- nm_path[[ii]]
        if(ii != length(nm_path)){
          if(!exists(nm, envir = d)) {
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

  }else{
    re <- NULL
  }
  re
}
