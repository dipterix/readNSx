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
#' @returns A list of configurations, see 'Details'.
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

  results <- list()

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
