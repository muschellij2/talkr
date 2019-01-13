#' Talk to Count a Data set
#'
#' @param cmd Command to perform on the data set.
#' @param .data The data set/\code{data.frame} to
#' perform the operation.
#' @param verbose print diagnostic output
#' @param ... additional arguments to pass to \code{\link{talk_get_colnames}}
#'
#' @return A \code{data.frame} or list of them if \code{length(cmds) > 1}.
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' df = mtcars %>%
#'   rownames_to_column(var = "car")
#'   cmds = c(
#'     "count by  mpg",
#'     "count up mpg, hp")
#'  data_colnames = df
#'  .data = df
#'  results = lapply(cmds, talk_count, .data = df)
#' df = df %>%
#'   rename(GEAR = gear)
#'  gear = df %>%
#'  talk_count("arrange by gear ")
#' testthat::expect_true(all(colnames(gear) == c("GEAR", "n")))
talk_count = function(.data, cmd, verbose = FALSE, ...) {

  data_colnames = colnames(.data)
  out = talk_group_by_expr(data_colnames, cmd, ...)

  out = lapply(out, function(x) {
    count(x = .data, !!! x)
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}

#' @export
#' @rdname talk_count
#' @param data_colnames column names of the data
talk_count_expr = talk_group_by_expr


