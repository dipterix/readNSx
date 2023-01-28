#' @export
as.character.readNSx_printable <- function(x, ...) {
  format(x, ...)
}

#' @export
print.readNSx_printable <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
}

#' @export
format.readNSx_filetype <- function(x, ...) {
  sprintf("Blackrock file type %s (%s) - specification version: %s",
          x$type, x$info, paste(x$version, collapse = "."))
}

#' @export
format.readNSx_specification <- function(x, ...) {
  spec <- x$specification
  section_names <- names(spec)
  s <- sprintf("<Blackrock [%s] specification version [%s]>", x$type, x$version)
  for(snm in section_names) {
    section <- spec[[ snm ]]
    s <- c(s, section$description[[ length(section$description) ]])
    item_names <- names(section$dictionary)
    for(inm in item_names) {
      item <- section$dictionary[[ inm ]]
      s <- c(
        s, sprintf(
          "  - %s [type: %s%s]", inm, item$type,
          ifelse(length(item$size) == 1,
                 sprintf(", size: %d", item$size),
                 ""
          )
        )
      )
    }
  }
  paste(s, collapse = "\n")
}

#' @export
format.readNSx_nev_basic_header <- function(x, ...) {
  ts <- x$time_origin
  paste(c(
    "Basic header information (NEV):",
    sprintf("  Internal type: %s", x$file_type),
    sprintf("  Application: %s", x$application_to_create_file),
    sprintf("  Highest sample rate: %.2f Hz", x$time_resolution_samples),
    sprintf("  Data packet size: %d B/packet", x$bytes_in_data_packet),
    sprintf("  Time origin: %04d-%02d-%02d %02d:%02d:%02d %03.0fms",
            ts[[1]], ts[[2]], ts[[4]], ts[[5]], ts[[6]], ts[[7]], ts[[8]])
  ), collapse = "\n")
}

#' @export
format.readNSx_nev_extended_header <- function(x, ...) {
  nms <- names(x)
  paste(c(
    "Extended header information (NEV):",
    sapply(nms, function(nm) {
      tbl <- x[[nm]]
      cnames <- names(tbl)
      if(length(cnames) > 3) {
        cnames <- c(cnames[1:3], "...")
      }
      cnames <- paste(cnames, collapse = ", ")
      sprintf("  - %s (%d x %d: %s)", nm, nrow(tbl), ncol(tbl), cnames)
    })
  ), collapse = "\n")
}

#' @export
format.readNSx_nev_data_packets <- function(x, ...) {
  sprintf("Data-packets (NEV):\n  Total size: %d", x$size())
}

#' @export
format.readNSx_nev <- function(x, ...) {
  s <- c(
    sprintf("<Blackrock NEV data, version: %s>", x$specification$version),
    format.readNSx_nev_basic_header( x$header_basic ),
    format.readNSx_nev_extended_header( x$header_extended ),
    "Data packet information (NEV):",
    sprintf("  Prefix: %s", x$prefix),
    sprintf("  Event types: %s", paste(x$event_types, collapse = ", "))
  )
  excluded <- attr(x, "packets_excluded")
  if(length(excluded)) {
    s <- c(s, sprintf("  Excluded event types: %s", deparse_svec(excluded)))
  }
  paste(s, collapse = "\n")
}

#' @export
format.readNSx_nsx_basic_header <- function(x, ...) {
  ts <- x$time_origin
  comments <- trimws(paste(x$comment, collapse = ""))
  s <- c(
    "Basic header information (NSx):",
    sprintf("  Internal type: %s", x$file_type),
    sprintf("  Channel count: %s", x$channel_count),
    sprintf("  Sample rate: %.0f Hz", 30000 / x$period),
    sprintf("  Time origin: %04d-%02d-%02d %02d:%02d:%02d %03.0fms",
            ts[[1]], ts[[2]], ts[[4]], ts[[5]], ts[[6]], ts[[7]], ts[[8]])
  )
  if( nzchar(comments) ) {
    s <- c(s, sprintf("  Comments: %s", comments))
  }

  paste(s, collapse = "\n")
}

#' @export
format.readNSx_nsx_extended_header <- function(x, ...) {
  nms <- names(x)
  paste(c(
    "Extended header information (NSx):",
    sapply(nms, function(nm) {
      tbl <- x[[nm]]
      cnames <- names(tbl)
      if("electrode_id" %in% cnames) {
        s <- sprintf("  - %s (%d x %d, channels: %s): ", nm, nrow(tbl), ncol(tbl), deparse_svec(tbl$electrode_id))
      } else {
        s <- sprintf("  - %s (%d x %d): ", nm, nrow(tbl), ncol(tbl))
      }
      if(length(cnames) > 3) {
        cnames <- c(cnames[1:3], "...")
      }
      cnames <- paste(cnames, collapse = ", ")
      sprintf("%s%s", s, cnames)
    })
  ), collapse = "\n")
}


#' @export
format.readNSx_nsx <- function(x, ...) {
  s <- c(
    format.readNSx_nsx_basic_header(x$header_basic),
    format.readNSx_nsx_extended_header(x$header_extended),
    "Cache status:",
    sprintf("  Prefix: %s", x$prefix),
    sprintf("  Number of partitions: %d", x$nparts)
  )

  paste(s, collapse = "\n")
}

format_h5_datasets <- function(x, sep = "$") {

  if(!inherits(x, "readNSx_h5_datasets")) { return("") }

  s <- unlist(lapply(names(x), function(nm) {
    d <- x[[nm]]
    sprintf("%s%s%s", sep, nm, format_h5_datasets(d, sep = sep))
  }))

  return(s)
}

#' @export
format.readNSx_h5_datasets <- function(x, ...) {
  return(paste(
    collapse = "\n",
    c(
      "<HDF5 data collection> (with the following names)",
      sprintf("  %s", format_h5_datasets(x, ...))
    )
  ))
}
