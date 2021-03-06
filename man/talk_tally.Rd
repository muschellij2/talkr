% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/talk_tally.R
\name{talk_tally}
\alias{talk_tally}
\alias{talk_add_tally}
\alias{talk_tallier}
\alias{talk_tally_expr}
\title{Talk to tally a Data set}
\usage{
talk_tally(.data, cmd, verbose = FALSE, ...)

talk_add_tally(.data, cmd, verbose = FALSE, ...)

talk_tallier(.data, cmd, verbose = FALSE, add = FALSE, ...)

talk_tally_expr(data_colnames, cmd, allowed_words = "", ...)
}
\arguments{
\item{.data}{The data set/\code{data.frame} to
perform the operation.}

\item{cmd}{Command to perform on the data set.}

\item{verbose}{print diagnostic output}

\item{...}{additional arguments to pass to \code{\link{talk_get_colnames}}}

\item{add}{argument not for general use, but passed down to
allow for switching between \code{count}/\code{tally}
and \code{add_count}/\code{add_tally}}

\item{data_colnames}{column names of the data}

\item{allowed_words}{words allowed to be in the command other than
the column names}
}
\value{
A \code{data.frame} or list of them if \code{length(cmds) > 1}.
}
\description{
Talk to tally a Data set
}
\examples{
library(tibble)
library(dplyr)
df = mtcars \%>\%
  rownames_to_column(var = "car")
  cmds = c(
    "tally",
    "add tally",
    "tally up mpg, hp, mpg")
 data_colnames = df
 .data = df
 results = lapply(cmds, talk_tally, .data = df)
df = df \%>\%
  rename(GEAR = gear)
data_colnames = df
cmd = "tally by gear "
 gear = df \%>\%
 talk("group by gear") \%>\%
 talk("tally")
testthat::expect_true(all(colnames(gear) == c("GEAR", "n")))
testthat::expect_true(nrow(gear) == length(unique(df$GEAR)))
 gear = df \%>\%
 talk("group by gear") \%>\%
 talk("add tally")
testthat::expect_false( "n" \%in\% colnames(df))
testthat::expect_true( "n" \%in\% colnames(gear))
}
