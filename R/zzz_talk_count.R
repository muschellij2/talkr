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
#'     "count up mpg, hp",
#'     "count up mpg, hp, mpg")
#'  data_colnames = df
#'  .data = df
#'  results = lapply(cmds, talk_count, .data = df)
#' df = df %>%
#'   rename(GEAR = gear)
#' data_colnames = df
#' cmd = "count by gear "
#'  gear = df %>%
#'  talk_count("count by gear ")
#' testthat::expect_true(all(colnames(gear) == c("GEAR", "n")))
talk_count = function(.data, cmd, verbose = FALSE, ...) {
  out = talk_counter(.data, cmd, verbose = verbose,
                     add = FALSE, ...)
  out
}

#' @export
#' @rdname talk_count
talk_add_count = function(.data, cmd, verbose = FALSE, ...) {
  out = talk_counter(.data, cmd, verbose = verbose,
                     add = TRUE, ...)
  out
}

#' @export
#' @rdname talk_count
#' @param add argument not for general use, but passed down to
#' allow for switching between \code{count}/\code{tally}
#' and \code{add_count}/\code{add_tally}
talk_counter = function(.data, cmd, verbose = FALSE,
                        add = FALSE, ...) {

  data_colnames = colnames(.data)
  out = talk_count_expr(data_colnames, cmd, ...)

  func = dplyr::count
  if (add) {
    func = dplyr::add_count
  }

  out = lapply(out, function(x) {
    func(x = .data, !!! x)
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}

#' @export
#' @rdname talk_count
#' @param data_colnames column names of the data
#' @param allowed_words words allowed to be in the command other than
#' the column names
talk_count_expr = function(data_colnames, cmd,
                           allowed_words = "",
                           ...) {
  talk_group_by_expr(data_colnames, cmd,
                     allowed_words = allowed_words,
                     ...)
}

