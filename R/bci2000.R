#' @export
print.BCIObjClass <- function(x, ...) {
  printBCIObject(x)
  invisible(x)
}

#' @export
format.BCIObjClass <- function(x, ...) {
  return( formatBCIObject(x) )
}

#' @export
print.BCIData <- function(x, ...) {
  if(is.character(x$summary)) {
    cat(x$summary, sep = "\n")
    return(invisible(x))
  }
  NextMethod(x)
}

#' @name read_bci2000
#' @title Read \code{'BCI2000'} recording data
#' @param file path to the recording data
#' @returns Parsed signal data
#' @examples
#'
#' # Package comes with sample data
#' file <- system.file("samples", "bci2000_sample.dat",
#'                     package = "readNSx")
#' result <- read_bci2000(file)
#'
#' print(result)
#'
#' # time-stamp in seconds
#' source_time <- result$states$data[, "SourceTime"] / 1000
#'
#' # Signal data 500 time-points x 64 channels
#' dim(result$signals)
#'
#' @export
read_bci2000_header <- function(file) {
  s <- readLines(file, n = 1)
  s <- trimws(str_match(s, "(^|\\ )([^=\\ ]+)=[\\ ]{0,1}([^=\\ ]+)")[[1]])
  s <- simplify2array(strsplit(s, split = "=[ ]{0,1}", fixed = FALSE))
  basic_headers <- structure(as.list(s[2, ]), names = tolower(s[1, ]))
  basic_header_names <- names(basic_headers)

  if(!"bci2000v" %in% basic_header_names) {
    basic_headers$bci2000v <- "1.0"
    basic_headers$dataformat <- "int16"
  }

  basic_headers$headerlen %!!<-% as.integer
  basic_headers$sourcech %!!<-% as.integer
  basic_headers$statevectorlen %!!<-% as.integer

  basic_headers$data_size <- switch(
    paste(basic_headers$dataformat, collapse = ""),
    "int16" = 2L,
    "int32" = 4L,
    "float32" = 4L,
    {
      stop("Unknown data element format ", sQuote(basic_headers$dataformat))
    }
  )



  if(length(basic_headers$headerlen)) {
    headers <- readChar(file, basic_headers$headerlen)
    headers <- strsplit(headers, "(\r\n|\n)")[[1]]
  } else {
    conn <- file(file, "r")
    on.exit({
      tryCatch({ close(conn) }, error = function(e){})
    }, add = TRUE)
    headers <- NULL
    line <- "?"
    while( nchar(trimws(line)) > 0 ) {
      line <- readLines(conn, n = 1)
      headers <- c(headers, line)
    }
    basic_headers$headerlen <- sum(nchar(headers) + nchar("\r\n"))
    close(conn)
  }

  # find start of state and parameter headers
  idx1 <- which(grepl("^\\[[ ]{0,}State Vector Definition[ ]{0,}\\]$", trimws(headers), ignore.case = TRUE))
  idx2 <- which(grepl("^\\[[ ]{0,}Parameter Definition[ ]{0,}\\]$", trimws(headers), ignore.case = TRUE))

  # Name Length Value ByteLocation BitLocation CRLF
  state_defs <- headers[ seq(idx1 + 1, idx2 - 1L) ]
  state_defs <- unlist(lapply(strsplit(state_defs, " "), function(x) {
    n <- paste(x[seq_len(length(x) - 4)], collapse = " ")
    v <- as.integer(x[ -seq_len(length(x) - 4) ])
    structure(list(v), names = n)
  }), recursive = FALSE, use.names = TRUE)

  # Section DataType Name= Value DefaultValue LowRange HighRange // Comment
  param_defs <- headers[ - seq_len(idx2) ]
  param_defs <- param_defs[grepl("^.*= .*$", param_defs)]

  param_defs <- lapply(param_defs, function(x) {
    # message(x)
    x_ <- x
    x <- strsplit(x, "= ", fixed = TRUE)[[1]]
    # x <- strsplit(param_defs, "= ", fixed = TRUE)[[1]]
    pre <- strsplit(trimws(x[[1]]), " ", fixed = TRUE)[[1]]
    val <- strsplit(trimws(x[[2]]), "//", fixed = TRUE)[[1]]
    data_type <- pre[[2]]

    if(length(val) > 1) {
      comments <- paste(val[-1], collapse = "//")
      val <- val[[1]]
    } else {
      comments <- ""
    }

    # parser1 <- function(val, f = NULL, nil = " ", names = NULL) {
    #   val <- bciStrDecode(x = trimws(val), nil = nil)
    #   val <- strsplit(val, " ", fixed = TRUE)[[1]]
    #   if(is.function( f )) {
    #     val <- f(val)
    #   }
    #   val
    # }
    # tryCatch({
    #   val <- switch(
    #     substr(data_type, 1, 3),
    #
    #     Value DefaultValue LowRange HighRange
    #     "int" = {
    #       parser1(val, as.integer, nil = " ", names = c("value", "default", "low_range", "high_range"))
    #     },
    #     {
    #       cat(data_type, ":", val, "\n")
    #       val
    #     }
    #   )
    # }, warning = function(e) {
    #   message(data_type, ": ", val)
    # })

    list(
      raw = x_,
      section = pre[[1]],
      data_type = data_type,
      name = pre[[3]],
      comments = comments,
      values = val
    )
  })

  parameters <- new.env(parent = emptyenv())
  lapply(param_defs, function(param) {
    section <- param$section
    section <- strsplit(section, ":", fixed = TRUE)[[1]]

    li <- parameters
    for(nm in section) {
      nm <- bciStrDecode(nm, nil = "")
      li[[nm]] %?<-% new.env(parent = emptyenv())
      li <- li[[nm]]
    }
    li[[ param$name ]] <- param
  })

  as_list <- function(x) {
    nms <- names(x)
    structure(lapply(nms, function(nm) {
      if(is.environment(x[[nm]])) {
        return(as_list(x[[nm]]))
      } else {
        return(x[[nm]])
      }
    }), names = nms)
  }

  parameters <- as_list(parameters)

  list(
    basic = basic_headers,
    state_definitions = state_defs,
    parameter_definitions = parameters
  )
}


#' @rdname read_bci2000
#' @export
read_bci2000 <- function(file) {
  header <- read_bci2000_header(file)
  data_parser <- createBCIObject("BCIDataParser", list(
    n_channels = header$basic$sourcech,
    state_bytes = header$basic$statevectorlen,
    state_definitions = header$state_definitions,
    data_format = header$basic$dataformat
  ))
  data_size <- file.size(file) - header$basic$headerlen
  conn <- file(file, "rb")
  conn_open <- TRUE

  # In case any error happens, always close the connection
  on.exit({
    if( conn_open ) {
      # trycatch might not be needed here but ..
      tryCatch({ close(conn) }, error = function(e) { })
    }
  }, add = TRUE)
  # read & skip the headers (it's fast so seek is not needed)
  readBin(conn, "raw", n = header$basic$headerlen)

  # read rest of the data to be parsed
  data <- readBin(conn, "raw", n = data_size, endian = "little")
  close(conn)
  conn_open <- FALSE

  parseBCIDataRaw(data_parser, data, TRUE)
  parsed <- maturalizeBCIObject(data_parser)

  parsed_summary <- format(data_parser)

  parsed_summary <- c(
    "<BCI2000 Recording Data>",
    "[Definitions]:",
    sprintf("  %s", strsplit(parsed_summary, "[\r]{0,1}\n")[[1]]),
    "[Enclosing Items]:",
    "  $header_basic          - BCI2000 basic header information",
    "  $parameter_definitions - Recording parameters",
    "  $states                - A list containing state definitions and state data",
    "  $signals               - Time x Channel signal matrix"
  )

  # release C++ memory
  rm(data_parser)

  structure(
    list(
      header_basic = header$basic,
      parameter_definitions = header$parameter_definitions,

      states = list(
        definitions = header$state_definitions,
        data = structure(t(parsed$states), dimnames = list(NULL, names(header$state_definitions)))
      ),

      signals = t(parsed$data),
      summary = parsed_summary
    ),
    class = c("BCIData.Recording", "BCIData")
  )
}

