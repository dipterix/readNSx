#' @name import_nsp
#' @title Import signal data from 'Blackrock-Microsystems' data files
#' @description Please use \code{import_nsp} to import 'NEV' and 'NSx' files.
#'
#' @param path path to 'NEV' or 'NSx' files
#' @param prefix path prefix to save data files into
#' @param exclude_events exclude one or more 'NEV' data events, choices are
#' \code{'spike'}, \code{'video_sync'}, \code{'digital_inputs'},
#' \code{'tracking'}, \code{'button_trigger'}, \code{'comment'},
#' \code{'configuration'}; default is \code{'spike'} since the spike data takes
#' very long time to parse, and most users might still use their own offline
#' spike-sorting algorithms.
#' @param exclude_nsx excluded 'NSx' types, integer vectors from 1 to 9; for
#' example, \code{exclude_nsx=c(1,2)} will prevent \code{'ns1'} and \code{'ns2'}
#' from being imported
#' @param verbose logical or a progress object: when logical, \code{verbose}
#' indicates whether to print the progress as messages;
#' when \code{verbose} is a progress object, this object must have \code{inc}
#' method; examples are \code{Progress} in the \code{shiny} package,
#' \code{progress2} from \code{dipsaus}, or \code{shiny_progress} from
#' \code{shidashi}
#' @param partition_prefix additional prefix to the data partition; default is
#' \code{"/part"}
#'
#' @return A list of configurations, see \code{\link{get_specification}} for
#' what's contained.
#'
#'
#' @examples
#'
#' # Please get your own sample data first. This package does not
#' # provide sample data for privacy and license concerns :)
#'
#' if(interactive() && file.exists("sampledata.nev")) {
#'
#' library(readNSx)
#'
#' # ---- Import for the first time --------------------------------
#' import_nsp(
#'   path = "sampledata.nev",
#'   prefix = file.path(
#'     "~/BIDSRoot/MyDataSet/sub-YAB/ses-008/ieeg/",
#'     "sub-YAB_ses-008_task-congruency_acq-NSP1_run-01"
#'   ),
#'   exclude_events = "spike", partition_prefix = "/part"
#' )
#'
#' # ---- Load header information --------------------------------
#' prefix <- "sub-YAB_ses-008_task-congruency_acq-NSP1_run-01"
#' nev <- get_nev(prefix)
#' ns3 <- get_nsx(prefix, which = 3)
#'
#' # get nev from nsx, or nsx from nev
#' get_nev(ns3)
#' get_nsx(nev, which = 5)
#'
#' # ---- Load channel data
#' result <- get_channel(prefix, channel_id = 10)
#' channel_signal <- result$channel_detail$part1$data
#' channel_signal[]
#'
#' }
#'
#'
#' @export
import_nsp <- function(path, prefix = NULL, exclude_events = "spike",
                             exclude_nsx = NULL, verbose = TRUE,
                             partition_prefix = "/part") {

  # DIPSAUS DEBUG START
  # path <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/YDY/block058/EMU-058_subj-YDY_task-noisyAV_run-06_NSP-2.nev'
  # prefix <- "~/Dropbox (PENN Neurotrauma)/RAVE/Samples/bids/TestData/sub-YDY/ses-058/ieeg/sub-YDY_ses-058_task-noisyAV_acq-NSP2_run-06"

  # set up progress
  inc_progress <- function( details, topic = NULL ) {
    if( is.atomic(verbose) ) {
      topic <- paste(topic, collapse = "")
      if( verbose ) {
        if(nzchar(topic)) { topic <- sprintf("%s - ", topic) }
        message( topic, details )
      }
      return()
    } else {

      # progress bar: shiny, dipsaus::progress2, or shidashi::shiny_progress
      tryCatch({
        verbose$inc( details, topic )
      }, error = function(...){})

    }

  }

  # check path
  path_pattern <- gsub("\\.(nev|ns[1-9])[\\\\/]{0,}$", "", path, ignore.case = TRUE)

  if(length(prefix) != 1 || is.na(prefix) || !nzchar(trimws(prefix))) {
    prefix <- path_pattern
  }

  path_nev <- paste0(path_pattern, ".nev")
  path_nev <- path_nev[file.exists(path_nev)]
  exclude_nsx <- parse_svec(exclude_nsx)
  nsx <- seq_len(9)
  nsx <- nsx[!nsx %in% exclude_nsx]
  path_nsx <- paste0(path_pattern, sprintf(".ns%d", nsx))
  n_nsx <- sum(file.exists(path_nsx))

  msg <- sprintf("Found [%d] .nev file(s) and [%d] .nsx file(s)", length(path_nev), n_nsx)
  if(length(path_nev) + n_nsx == 0) {
    stop("No .nev nor .ns1-9 file found. Please check your file paths.")
  }
  inc_progress(msg, "Importing Blackrock")

  results <- structure(list(), class = "readNSx_collection")

  if(length(path_nev)) {
    inc_progress(sprintf("Excluding %s", paste(exclude_events, collapse = ", ")), "Importing NEV")
    results$nev <- read_nev(path = path_nev, prefix = prefix, exclude_events = exclude_events)
  } else {
    inc_progress("No NEV file found: skipping", "Importing NEV")
  }

  for(path in path_nsx) {
    nsx <- substring(path, nchar(path) - 2)
    if(file.exists(path)) {
      inc_progress(sprintf("Parsing %s", nsx), "Importing NSx")
      results[[nsx]] <- read_nsx(path = path, prefix = prefix, partition_prefix = partition_prefix)
    } else {
      inc_progress(sprintf("Skipping %s", nsx), "Importing NSx")
    }
  }
  return(results)
}

#' @title Load 'NEV' information from path prefix
#' @param x path \code{prefix} specified in \code{\link{import_nsp}}, or
#' \code{'nev/nsx'} object
#' @param ... reserved for future use
#' @return 'NEV' header information if \code{x} is valid, otherwise \code{NULL}.
#' See Section "'NEV' Data" in \code{\link{get_specification}}
#' @export
get_nev <- function(x, ...) {
  UseMethod("get_nev")
}

#' @export
get_nev.default <- function(x, ...) {
  if(length(x) != 1 || is.na(x) || !is.character(x)) { return(NULL) }
  # x is character - prefix
  x1 <- file.path(sprintf("%s_events", x), "nev-headers.rds")
  if(!file.exists(x1)) {
    x <- gsub("\\.(nev|ns[1-9])$", "", x = x, ignore.case = TRUE)
    x1 <- file.path(sprintf("%s_events", x), "nev-headers.rds")
    if(!file.exists(x1)) {
      warning("Cannot find header file: ", x1)
      return(NULL)
    }
  }
  nev <- readRDS(x1)
  if(!inherits(nev, 'readNSx_nev')) {
    warning("Header file found, but it's not a valid NEV header.")
    return(NULL)
  }
  nev$prefix <- normalizePath(x, mustWork = FALSE)
  return(nev)
}

#' @export
get_nev.readNSx_nev <- function(x, ...) {
  x
}

#' @export
get_nev.readNSx_nsx <- function(x, ...) {
  return(get_nev.default( x$prefix ))
}

#' @export
get_nev.readNSx_collection <- function(x, ...) {
  x$nev
}


#' @title Get event data packets from 'NEV'
#' @param x path \code{prefix} (see \code{\link{import_nsp}}), or
#' \code{'nev/nsx'} object
#' @param event_type event type to load, common event types are
#' \describe{
#' \item{\code{'digital_inputs'}}{packet identifier 0}
#' \item{\code{'spike'}}{packet identifier 1 to 10000 as of version 3.0}
#' \item{\code{'recording'}}{packet identifier 65529 as of version 3.0, available after version 3.0}
#' \item{\code{'configuration'}}{packet identifier 65530 as of version 3.0, available after version 3.0}
#' \item{\code{'log'}}{packet identifier 65531 as of version 3.0, available after version 3.0}
#' \item{\code{'button_trigger'}}{packet identifier 65532 as of version 3.0, available after version 3.0}
#' \item{\code{'tracking'}}{packet identifier 65533 as of version 3.0, available after version 3.0}
#' \item{\code{'video_sync'}}{packet identifier 65534 as of version 3.0, available after version 3.0}
#' \item{\code{'comment'}}{packet identifier 65535 as of version 3.0, available after version 2.3}
#' }
#' @param ... pass to other methods
#' @return A data frame of corresponding event type, or \code{NULL} if event
#' is not found or invalid
#' @export
get_event <- function(x, event_type, ...) {
  event_type <- tolower(event_type)
  if(!inherits(x, c("readNSx_collection", "readNSx_nev"))) {
    # Don't trust the header, always reload
    x <- get_nev(x)
    x <- x$prefix
  }
  nev <- get_nev(x)
  if(!inherits(nev, "readNSx_nev")) {
    stop("get_event: Cannot get NEV events as `x` contains invalid prefix. If you have ever moved/renamed your data, please reload header using updated prefix.")
  }
  event_path <- sprintf("%s_events", nev$prefix)
  if(event_type == "spike") {
    spike_path <- file.path(event_path, "waveforms.h5")
    if(!file.exists(spike_path)) { return(NULL) }
    return(load_h5_all(spike_path))
  }

  event_file <- file.path(event_path, sprintf("event-%s.rds", event_type))

  if(!file.exists(event_file)) { return(NULL) }
  return(readRDS(event_file))
}


#' @title Load 'NSx' information from path prefix
#' @param x path \code{prefix} specified in \code{\link{import_nsp}}, or
#' \code{'nev/nsx'} object
#' @param which which 'NSx' to load, for example, \code{which=3} loads
#' \code{'ns3'} headers
#' @param ... reserved for future use
#' @return 'NSx' header information if data is found, otherwise returns
#' \code{NULL}. See Section "'NSx' Data" in \code{\link{get_specification}}
#' @export
get_nsx <- function(x, which, ...) {
  UseMethod("get_nsx")
}

#' @export
get_nsx.default <- function(x, which, ...) {
  if(length(x) != 1 || is.na(x) || !is.character(x)) { return(NULL) }
  which <- tolower(as.character(which))
  if(nchar(which) < 3) {
    which <- sprintf("ns%s", which)
  }
  if(!which %in% sprintf("ns%d", seq_len(9))) { return(NULL) }
  # x is character - prefix
  x1 <- file.path(sprintf("%s_ieeg", x), sprintf("%s_summary.rds", which))
  if(!file.exists(x1)) {
    x <- gsub("\\.(nev|ns[1-9])$", "", x = x, ignore.case = TRUE)
    x1 <- file.path(sprintf("%s_events", x), sprintf("%s_summary.rds", which))
    if(!file.exists(x1)) {
      return(NULL)
    }
  }
  nsx <- readRDS(x1)
  if(!inherits(nsx, 'readNSx_nsx')) {
    warning("Header file found, but it's not a valid NSx header.")
    return(NULL)
  }
  nsx$prefix <- normalizePath(x, mustWork = FALSE)
  return(nsx)
}

#' @export
get_nsx.readNSx_nsx <- function(x, which, ...) {
  which <- tolower(as.character(which))
  if(nchar(which) < 3) {
    which <- sprintf("ns%s", which)
  }
  if(!identical(x$which, which)) { return(NULL) }
  return(x)
}

#' @export
get_nsx.readNSx_nev <- function(x, which, ...) {
  return(get_nsx.default( x$prefix, which ))
}

#' @export
get_nsx.readNSx_collection <- function(x, which, ...) {
  which <- tolower(as.character(which))
  if(nchar(which) < 3) {
    which <- sprintf("ns%s", which)
  }
  x[[which]]
}

#' Get channel data
#' @description Obtain channel information and data from given prefix and
#' channel ID.
#' @param x path \code{prefix} specified in \code{\link{import_nsp}}, or
#' \code{'nev/nsx'} object
#' @param channel_id integer channel number. Please be aware that channel
#' number, channel ID, electrode ID refer to the same concept in
#' 'Blackrock' 'NEV' specifications. Electrodes are not physical metals, they
#' refer to channels for historical reasons.
#' @return A list containing channel data and meta information, along with
#' the enclosing 'NSx' information; for invalid channel ID, this function
#' returns \code{NULL}
#' @export
get_channel <- function(x, channel_id) {

  channel_id <- as.integer(channel_id)
  if(length(channel_id) != 1 || is.na(channel_id)) {
    stop("readNSX::get_channel: invalid `channel_id`, must be an integer of length 1")
  }

  channel_info <- NULL
  nsx <- NULL

  # get nsx in reverse order
  for(which in rev(seq_len(9))) {
    nsx <- get_nsx(x, which = which)
    if(inherits(nsx, "readNSx_nsx")) {
      if(channel_id %in% nsx$header_extended$CC$electrode_id) {
        channel_info <- nsx$header_extended$CC[
          nsx$header_extended$CC$electrode_id == channel_id,
        ]
        # get sample rate
        channel_info$sample_rate_signal <- 30000 / nsx$header_basic$period
        channel_info$sample_rate_timestamp <- nsx$header_basic$time_resolution_timestamp
        channel_info$which_nsp <- which
        if(length(nsx$partition_prefix) != 1) {
          nsx$partition_prefix <- "/part"
        }
        break
      }
    }
  }

  if(is.null(channel_info)) { return(NULL) }

  # make sure using the first one
  channel_info <- channel_info[1,]

  channel_filename <- channel_filename(
    channel_id = channel_info$electrode_id,
    channel_label = channel_info$electrode_label)

  channel_info$filename <- channel_filename

  partition_path_prefix <- sprintf("%s_ieeg%s", nsx$prefix, nsx$partition_prefix)

  data <- structure(
    lapply(seq_len(nsx$nparts), function(ii) {
      fpath <- file.path(sprintf("%s%d", partition_path_prefix, ii), channel_filename)
      if(!file.exists(fpath)) {
        return(NULL)
      }
      list(
        meta = jsonlite::fromJSON(load_h5(fpath, "meta", ram = TRUE)),
        data = load_h5(fpath, "data", read_only = TRUE, ram = FALSE)
      )
    }),
    names = sprintf("part%d", seq_len(nsx$nparts))
  )

  structure(
    list(
      nsx = nsx,
      channel_info = channel_info,
      channel_detail = data
    )
  )


}


#' @title Get a collection list containing 'NEV' and 'NSx' headers
#' @param x path \code{prefix} specified in \code{\link{import_nsp}}, or
#' \code{'nev/nsx'} object
#' @return A list containing \code{'nev'} and imported \code{'nsx'} headers,
#' see \code{\link{import_nsp}} for details
#' @export
get_nsp <- function(x) {
  nev <- get_nev(x)
  if(!inherits(nev, "readNSx_nev")) {
    stop("readNSx::get_nsp: Cannot obtain the NEV header information")
  }

  re <- structure(
    list(nev = nev),
    class = "readNSx_collection"
  )

  lapply(seq_len(9), function(ii) {
    nsx <- get_nsx(nev, which = ii)
    if(inherits(nsx, "readNSx_nsx")) {
      re[[ sprintf("ns%d", ii) ]] <<- nsx
    }
  })

  re
}
