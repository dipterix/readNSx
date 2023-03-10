% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blackrock.R
\name{import_nsp}
\alias{import_nsp}
\title{Import signal data from 'Blackrock-Microsystems' data files}
\usage{
import_nsp(
  path,
  prefix = NULL,
  exclude_events = "spike",
  exclude_nsx = NULL,
  verbose = TRUE,
  partition_prefix = "/part"
)
}
\arguments{
\item{path}{path to 'NEV' or 'NSx' files}

\item{prefix}{path prefix to save data files into}

\item{exclude_events}{exclude one or more 'NEV' data events, choices are
\code{'spike'}, \code{'video_sync'}, \code{'digital_inputs'},
\code{'tracking'}, \code{'button_trigger'}, \code{'comment'},
\code{'configuration'}; default is \code{'spike'} since the spike data takes
very long time to parse, and most users might still use their own offline
spike-sorting algorithms.}

\item{exclude_nsx}{excluded 'NSx' types, integer vectors from 1 to 9; for
example, \code{exclude_nsx=c(1,2)} will prevent \code{'ns1'} and \code{'ns2'}
from being imported}

\item{verbose}{logical or a progress object: when logical, \code{verbose}
indicates whether to print the progress as messages;
when \code{verbose} is a progress object, this object must have \code{inc}
method; examples are \code{Progress} in the \code{shiny} package,
\code{progress2} from \code{dipsaus}, or \code{shiny_progress} from
\code{shidashi}}

\item{partition_prefix}{additional prefix to the data partition; default is
\code{"/part"}}
}
\value{
A list of configurations, see \code{\link{get_specification}} for
what's contained.
}
\description{
Please use \code{import_nsp} to import 'NEV' and 'NSx' files.
}
\examples{

# Please get your own sample data first. This package does not
# provide sample data for privacy and license concerns :)

if(interactive() && file.exists("sampledata.nev")) {

library(readNSx)

# ---- Import for the first time --------------------------------
import_nsp(
  path = "sampledata.nev",
  prefix = file.path(
    "~/BIDSRoot/MyDataSet/sub-YAB/ses-008/ieeg/",
    "sub-YAB_ses-008_task-congruency_acq-NSP1_run-01"
  ),
  exclude_events = "spike", partition_prefix = "/part"
)

# ---- Load header information --------------------------------
prefix <- "sub-YAB_ses-008_task-congruency_acq-NSP1_run-01"
nev <- get_nev(prefix)
ns3 <- get_nsx(prefix, which = 3)

# get nev from nsx, or nsx from nev
get_nev(ns3)
get_nsx(nev, which = 5)

# ---- Load channel data
result <- get_channel(prefix, channel_id = 10)
channel_signal <- result$channel_detail$part1$data
channel_signal[]

}


}
