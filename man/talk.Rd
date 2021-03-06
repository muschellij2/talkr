% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/talk.R
\name{talk}
\alias{talk}
\alias{talk_expr}
\title{Main function to "talk" to \code{dplyr}}
\usage{
talk(.data, cmd, error_find_function = TRUE, ...)

talk_expr(.data, cmd, error_find_function = TRUE, ...)
}
\arguments{
\item{.data}{A \code{data.frame} to perform the commands on}

\item{cmd}{the natural language command}

\item{error_find_function}{Should the command error if the function
cannot be found from the command?}

\item{...}{Additional arguments to pass to the associated
\code{talkr} function.}
}
\value{
A \code{data.frame}
}
\description{
Main function to "talk" to \code{dplyr}
}
\examples{
library(tibble)
library(dplyr)
df = mtcars \%>\%
  rownames_to_column(var = "car")
res = df \%>\%
  talk("Sort df by mpg")
}
