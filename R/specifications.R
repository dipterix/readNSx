# DIPSAUS DEBUG START
# path <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/YEK/EMU-057_task-LOCALIZER_run-01/EMU-057_subj-YEK_task-LOCALIZER_run-01_NSP-1.nev'
# type <- "nev"
# version <- "2.3"
# x <- blackrock_specification(version, type)
# conn <- file(path, "rb")

#' Get 'Blackrock' file type
#' @description Reads the first 10 bytes containing file type and version
#' information.
#' @param path path to the 'Blackrock' \code{'.nev'} or \code{'.nsx'} file, or
#' a binary connection.
#' @return A list containing file information, including file type, version
#' information, and normalized absolute path.
#' @export
get_file_type <- function( path ) {
  header_info <- readBin(path, what = "raw", size = 1L, endian = "little", n = 10)
  file_info <- parse_string(header_info[seq_len(8)])
  file_version <- parse_uint8(header_info[c(9, 10)])

  file_type <- switch(
    file_info,
    NEURALEV = { "nev" },
    BREVENTS = { "nev" },
    BRSMPGRP = { "nsx" },
    NEURALCD = { "nsx" },
    NEURALSG = { "nsx" },
    {
      stop("`readNSx::get_file_type`: Unsupported files format [", file_info, "].")
    }
  )


  structure(
    list(
      type = file_type,
      version = file_version,
      info = file_info,
      path = normalizePath(path)
    ),
    class = c("readNSx_filetype", "readNSx_printable")
  )
}

#' Get '.nev' or 'nsx' specification
#' @param version either character string or a vector of two integers; for
#' example, \code{"2.2"}, \code{"2.3"}, \code{c(3, 0)}. Currently only these
#' three versions are supported since I was unable to find any other versions.
#' Please file an issue ticket if others versions are desired.
#' @param type file type; choices are \code{'nev'} or \code{'nsx'}
#' @return The file specification as a list. The specification usually contains
#' three sections: basic header (fixed length), extended header
#' (dictionary-style), and data packets (data stream). The specification is
#' used to parse the data files.
#' @examples
#'
#' get_specification(version = c(2,3), type = "nev")
#'
#' get_specification(version = "3.0", type = "nsx")
#'
#' @section 'NEV' Data:
#' A 'NEV' object consists of three sections:
#'
#' Section 1 contains basic information such as the time-origin of all the
#' time-stamps, the time-stamp sampling frequency, data packets sizes.
#'
#' Section 2 is extended header containing the configurations of channels,
#' digital signals, etc. For any data packets in section 3, there should
#' be at least one table in this section describing the settings.
#'
#' section 3 is a collection of event packets such as digital signal inputs (
#' most likely to be used at version 2.2 or by 'Ripple'), spike waveform,
#' comments (sometimes storing epoch information), etc.
#'
#' Please be aware that while most common entries can be found across different
#' file versions, some entries are version-specific. If you are making your
#' script general, you need to be very careful handling these differences.
#' For more information, please search for the data specification manual
#' from the 'Blackrock-Microsystems' website.
#'
#' @section 'NSx' Data:
#' A 'NSx' file refers to the data files ending with \code{'ns1'} through
#' \code{'ns9'}. Common types are \code{'ns2'} (sampling at 1000 Hz),
#' \code{'ns3'} (sampling at 2000 Hz), and \code{'ns5'} (sampling at 30,000 Hz).
#'
#' A 'NSx' file also consists of three sections. Section 1 contains basic
#' information such as the time-origin of all the time-stamps, sampling
#' frequencies, and channel counts within the file. Please be careful that
#' item \code{time_resolution_timestamp} is not the sampling frequency for
#' signals. This item is the sampling frequency for time-stamp. To obtain the
#' signal sample rate, divided \code{time_resolution_timestamp} by
#' \code{period}. For example, \code{'ns3'} usually has time-stamp resolution
#' \code{30,000} and \code{period=15}, hence the signal sample rate is
#' \code{30000/15=2000Hz}.
#'
#' Section 2 usually contains one and only one channel table of which the
#' number of rows should coincide with number of channels from section 1. Other
#' information such as channel labels, physical connectors, pins, units, filter
#' settings, digital-to-analog conversion are also included. Since
#' \code{readNSx} always attempts to convert signals in 'volts' or 'milli-volts'
#' to 'micro-volts', the \code{'units'} column might be different to what's
#' actual recorded in the 'NSx' file headers.
#'
#' Section 3 contains partitions of continuous recording. When imported/loaded
#' from \code{readNSx}, the digital signals are always converted to analog
#' signals with 'micro-volts' unit. Please use \code{\link{get_channel}} to
#' get the channel data.
#'
#' @export
get_specification <- function(version, type = c("nev", "nsx")) {
  type <- match.arg(type)
  version <- as.character(version)
  if(!length(version) || any(is.na(version))) {
    stop("readNSx: blackrock_specification version must be valid.")
  }
  if(length(version) >= 2) {
    version <- sprintf("%s.%s", version[[1]], version[[2]])
  }

  # get specification
  spec_file <- system.file(
    package = "readNSx", "specifications",
    sprintf("blackrock-%s-%s.json", type, version))

  if( spec_file == "" ) {
    stop(sprintf("readNSx: cannot find specification configuration for [%s] with version [%s]. Supported versions are 2.2, 2.3, and 3.0. For older file versions (2.0 and 2.1), I cannot find their specification files anywhere online. Please file an issue ticket on my Github repository if you have the specification file and would like to add support.", type, version))
  }

  spec <- jsonlite::read_json(spec_file)
  spec$type <- type

  structure(spec, class = c(
    sprintf("readNSx_specification_%s", type),
    "readNSx_specification", "readNSx_printable"
  ))

}
