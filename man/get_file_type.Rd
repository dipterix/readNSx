% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specifications.R
\name{get_file_type}
\alias{get_file_type}
\title{Get 'Blackrock' file type}
\usage{
get_file_type(path)
}
\arguments{
\item{path}{path to the 'Blackrock' \code{'.nev'} or \code{'.nsx'} file, or
a binary connection.}
}
\value{
A list containing file information, including file type, version
information, and normalized absolute path.
}
\description{
Reads the first 10 bytes containing file type and version
information.
}
