#' Talk to tally a Data set
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
#'     "tally",
#'     "add tally",
#'     "tally up mpg, hp, mpg")
#'  data_colnames = df
#'  .data = df
#'  results = lapply(cmds, talk_tally, .data = df)
#' df = df %>%
#'   rename(GEAR = gear)
#' data_colnames = df
#' cmd = "tally by gear "
#'  gear = df %>%
#'  talk("group by gear") %>%
#'  talk("tally")
#' testthat::expect_true(all(colnames(gear) == c("GEAR", "n")))
#' testthat::expect_true(nrow(gear) == length(unique(df$GEAR)))
#'  gear = df %>%
#'  talk("group by gear") %>%
#'  talk("add tally")
#' testthat::expect_false( "n" %in% colnames(df))
#' testthat::expect_true( "n" %in% colnames(gear))
talk_tally = function(.data, cmd, verbose = FALSE, ...) {

  out = talk_tallier(.data, cmd, verbose = verbose,
                     add = FALSE, ...)
  out
}

#' @export
#' @rdname talk_tally
talk_add_tally = function(.data, cmd, verbose = FALSE, ...) {
  out = talk_tallier(.data, cmd, verbose = verbose,
                     add = TRUE, ...)
  out
}

#' @export
#' @rdname talk_tally
#' @param add argument not for general use, but passed down to
#' allow for switching between \code{count}/\code{tally}
#' and \code{add_count}/\code{add_tally}
talk_tallier = function(.data, cmd, verbose = FALSE,
                        add = FALSE, ...) {

  data_colnames = colnames(.data)
  out = talk_tally_expr(data_colnames, cmd, ...)

  func = dplyr::tally
  if (add) {
    func = dplyr::add_tally
  }

  out = lapply(out, function(x) {
    # func(x = .data, !!! x)
    func(x = .data)
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}

#' @export
#' @rdname talk_tally
#' @param data_colnames column names of the data
#' @param allowed_words words allowed to be in the command other than
#' the column names
talk_tally_expr = function(data_colnames, cmd,
                           allowed_words = "",
                           ...) {
  # talk_group_by_expr(data_colnames, cmd,
  #                    allowed_words = allowed_words,
  #                    ...)
  # in futre maybe allow wt and sort
  return("")
}

