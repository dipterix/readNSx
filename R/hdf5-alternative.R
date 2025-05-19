
LazyFakeH5Internal <- R6::R6Class(
  inherit = LazyH5Internal,
  classname = 'LazyFakeH5Internal',
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    # file = NULL,
    # name = NULL,
    # read_only = TRUE,
    # data_ptr = NULL,
    # file_ptr = NULL,
    # last_dim = NULL
    get_data_path = function() {
      sub_dir <- gsub("^[/]+", "", private$name)
      sub_dir <- gsub("[/]+", "@", sub_dir)
      file.path(private$file, sub_dir)
    },
    ensure_data_path = function(new_file = FALSE) {
      if(new_file) {
        if(file.exists(private$file)) {
          unlink(private$file, recursive = TRUE)
        }
      }

      if(!dir.exists(private$file)) {
        dir.create(private$file, showWarnings = FALSE, recursive = FALSE)
      }

      sub_dir <- gsub("^[/]+", "", private$name)
      sub_dir <- gsub("[/]+", "@", sub_dir)

      data_path <- file.path(private$file, sub_dir)
      if(!dir.exists(data_path)) {
        dir.create(data_path, showWarnings = FALSE, recursive = TRUE)
      }
      data_path
    }
  ),
  public = list(

    quiet = FALSE,

    print = function(){
      data_path <- private$get_data_path()
      meta_rds <- file.path(data_path, "meta.rds")
      meta <- readRDS(meta_rds)
      dm <- meta$.dim
      if(!length(dm)) {
        dm <- meta$.length
      }
      cat(
        sep = "\n",
        c(
          "Alternative storage in RDS format",
          sprintf("Dataset: %s", private$name),
          sprintf("Filename: %s", private$file),
          sprintf("Space: length=%s\tDims=%s", meta$.length, paste(dm, collapse = "x"))
        )
      )
      invisible(self)
    },
    initialize = function(file_path, data_name, read_only = FALSE, quiet = FALSE){
      if(!endsWith(tolower(file_path), ".ralt")) {
        file_path <- sprintf("%s.ralt", file_path)
      }
      file_path <- normalizePath(file_path, mustWork = FALSE, winslash = "/")

      if( grepl("[@\\.]", data_name) ) {
        stop("Data name must not contain `@` or `.`")
      }

      # First get absolute path, otherwise hdf5r may report file not found error
      if(read_only){
        private$file <- file_path
        if(!dir.exists(file_path)) {
          stop("Path `", file_path, "` must exist and needs to be a folder. It cannot be a file/symlink/empty.")
        }
      }else{
        file_path <- file_path
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
      if(private$read_only && !force){
        stop('File is read-only. Use "force=TRUE"')
      }

      data_path <- private$ensure_data_path(new_file = new_file)

      saveRDS(x, file = file.path(data_path, "data.rds"))
      saveRDS(list(
        .length = length(x), .dim = dim(x), .r_storage = storage.mode(x),
        chunk = chunk, level = level, ctype = ctype, size = size, ...
      ), file = file.path(data_path, "meta.rds"))

      invisible()

    },


    open = function(new_dataset = FALSE, robj, ...){},


    close = function(all = TRUE){},
    subset = function(
      ...,
      drop = FALSE, stream = FALSE,
      envir = parent.frame()
    ) {
      data_path <- private$get_data_path()
      data_rds <- file.path(data_path, "data.rds")
      x <- readRDS(data_rds)
      x[..., drop = drop]
    },

    get_dims = function(...){
      data_path <- private$get_data_path()
      meta_rds <- file.path(data_path, "meta.rds")
      meta <- readRDS(meta_rds)
      if(length(meta$.dim)) {
        return(meta$.dim)
      }
      return(meta$.length)
    }
  )
)

load_fakeh5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE){
  re <- LazyFakeH5Internal$new(file_path = file, data_name = name, read_only = read_only, quiet = quiet)
  if(ram){
    re <- re[]
  }
  return(re)
}


save_fakeh5 <- function(x, file, name, chunk = 'auto', level = 4,replace = TRUE,
                    new_file = FALSE, ctype = NULL, quiet = FALSE, ...){
  f <- LazyFakeH5Internal$new(file, name, read_only = FALSE, quiet = quiet)
  f$save(x, chunk = chunk, level = level, replace = replace, new_file = new_file, ctype = ctype, force = TRUE, ...)
  return(invisible(normalizePath(file, mustWork = FALSE)))
}


fakehh5_names <- function(file){
  if(!endsWith(tolower(file_path), ".ralt")) {
    file_path <- sprintf("%s.ralt", file_path)
  }
  file_path <- normalizePath(file_path, mustWork = FALSE, winslash = "/")
  if(!dir.exists(file_path)) { return(character()) }

  nms <- list.dirs(file_path, full.names = FALSE, recursive = FALSE)
  gsub("@", "/", nms)
}

load_fakeh5_all <- function(file, ram = FALSE){
  if(!endsWith(tolower(file_path), ".ralt")) {
    file_path <- sprintf("%s.ralt", file_path)
  }
  file_path <- normalizePath(file_path, mustWork = FALSE, winslash = "/")
  if(!dir.exists(file_path)) { return(NULL) }
  nms <- list.dirs(file_path, full.names = FALSE, recursive = FALSE)

  re <- structure(
    new.env(parent = emptyenv()),
    class = c("readNSx_h5_datasets", "readNSx_printable", "environment")
  )
  lapply(nms, function(nm){
    y <- load_fakeh5(file, name = gsub("@", "/", nms), ram = ram)
    nm_path <- strsplit(nm, "@")[[1]]
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
  re
}
