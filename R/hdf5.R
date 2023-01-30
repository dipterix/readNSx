
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
    last_dim = NULL
  ),
  public = list(

    quiet = FALSE,

    finalize = function(){
      self$close(all = TRUE)
    },
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

load_h5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE){

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

  return(invisible(normalizePath(file)))
}


h5_valid <- function(file, mode = c('r', 'w'), close_all = FALSE){
  mode <- match.arg(mode)
  tryCatch({
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

}



h5_names <- function(file){
  # make sure the file is valid
  if(!h5_valid(file, 'r')){ return(FALSE) }
  file <- normalizePath(file, mustWork = TRUE)
  f <- hdf5r::H5File$new(filename = file, mode = 'r')
  names <- hdf5r::list.datasets(f)
  f$close()
  names
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
