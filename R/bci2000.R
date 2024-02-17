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
print.BCIData <- function(x, indent = "", ...) {
  if(is.character(x$summary)) {
    summary <- x$summary
    if(nzchar(indent)) {
      summary <- sprintf("%s%s", indent, strsplit(paste(x$summary, collapse = "\n"), "\n")[[1]])
    }
    cat(summary, sep = "\n")
    return(invisible(x))
  }
  NextMethod(x)
}

parse_bci_param_def <- function(s) {
  sel <- grepl("^[^ ]{1,} [^ ]{1,} [^ ]{1,}= ", s)
  n <- length(s)
  lapply(seq_len(n), function(ii) {
    if(!sel[[ii]]) { return(NULL) }
    params <- parseBCIParamDef(s[[ii]])
    section <- strsplit(params$section, ":", fixed = TRUE)[[1]]
    section <- unname(sapply(section, bciStrDecode, ""))
    params$full_section <- section
    params$section <- section
    params$summary <- c(
      sprintf("[BCIParamDef: %s]%s", params$name,
              paste(utils::capture.output(utils::str(params$value, indent.str = "  ")), collapse = "\n"))
    )
    class(params) <- c("BCIData.Parameter", "BCIData")
    return(params)
  })
}

#' @export
format.BCIData.Parameter.List <- function(x, indent = "", ...) {
  nms <- names(x)
  paste(
    c(
      sprintf("%s<BCIParameters>", indent),
      sprintf("%s  - %s", indent, nms)
    ),
    collapse = "\n"
  )
}

#' @export
print.BCIData.Parameter.List <- function(x, ...) {
  cat("%s\n", format(x, ...))
}

#' @name read_bci2000
#' @title Read \code{'BCI2000'} recording data
#' @param file path to the recording data
#' @returns Parsed signal data
#' @examples
#'
#' # Package comes with sample data
#' file <- system.file("samples", "bci2000_sample.dat", package = "readNSx")
#' result <- read_bci2000(file)
#'
#' print(result)
#'
#' # Notive: v1.0 and v1.1 are different, but all in `Source` section
#' # sample rate
#' result$parameters$Source$SamplingRate$value
#'
#' # Signal data 64 channels x 500 time-points
#' dim(result$signals)
#'
#' @export
read_bci2000_header <- function(file) {
  # DIPSAUS DEBUG START
  # file <- "~/Dropbox (PennNeurosurgery)/RAVE/Samples/ECoGAnalysisTraining/data/bci2000_datafile.dat"
  # file <- file <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/ECoGAnalysisTraining/BCI2000Tools/mex/testdata.dat"

  meta <- readLines(file, n = 1)
  s <- trimws(str_match(meta, "(^|\\ )([^=\\ ]+)=[\\ ]{0,1}([^=\\ ]+)")[[1]])
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
  param_defs <- parse_bci_param_def(headers[ - seq_len(idx2) ])
  param_defs <- param_defs[!vapply(param_defs, is.null, FALSE)]
  parameters <- new.env(parent = emptyenv())
  lapply(param_defs, function(param) {
    if(is.null(param)) { return() }
    section <- param$section

    li <- parameters
    for(nm in section) {
      nm <- bciStrDecode(nm, nil = "")
      li[[nm]] %?<-% new.env(parent = emptyenv())
      li <- li[[nm]]
    }
    li[[ param$name ]] <- param
    return()
  })

  as_list <- function(x) {
    nms <- names(x)
    structure(lapply(nms, function(nm) {
      if(is.environment(x[[nm]])) {
        return(as_list(x[[nm]]))
      } else {
        return(x[[nm]])
      }
    }), names = nms, class = "BCIData.Parameter.List")
  }

  parameters <- as_list(parameters)

  list(
    meta = meta,
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

  parsed_summary <- c(
    "<BCI2000 Recording Data>",
    header$meta,
    "[State Definitions]:",
    sprintf("  %s", strsplit(format(data_parser), "[\r]{0,1}\n")[[1]]),
    "[Parameter Definitions]",
    format(header$parameter_definitions, indent = "  "),
    "[Enclosing Items]:",
    "  $header_basic          - BCI2000 basic header information",
    "  $parameters            - Recording parameters",
    "  $states                - A list containing state definitions and state data",
    "  $signals               - Time x Channel signal matrix"
  )

  # release C++ memory
  rm(data_parser)

  structure(
    list(
      header_basic = header$basic,
      parameters = header$parameter_definitions,

      states = list(
        definitions = header$state_definitions,
        data = structure(parsed$states, dimnames = list(names(header$state_definitions), NULL))
      ),

      signals = parsed$data,
      summary = parsed_summary
    ),
    class = c("BCIData.Recording", "BCIData")
  )
}

