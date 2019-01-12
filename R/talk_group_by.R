#' Talk to Group a Data set
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
#'     "group by  mpg",
#'     "Group by  column    mpg  ",
#'     "group by column 5",
#'     "arrange by gear",
#'     "group by columns 4 and 5",
#'     "group by columns 4 and 5, mpg decreasing",
#'     # duplciate
#'     "group by columns 2 and 5, mpg decreasing",
#'     "group by columns 4, 5, and 6",
#'     "group by mpg descending",
#'     "group by mpg ascending",
#'     "group by mpg ascending",
#'     "group by mpg low to high")
#'  data_colnames = df
#'  .data = df
#'  results = lapply(cmds, talk_group_by, .data = df)
#' df = df %>%
#'   rename(GEAR = gear)
#'  gear = df %>%
#'  talk_group_by("arrange by gear and column 3")
#' testthat::expect_true(all(group_vars(gear) == c("GEAR", "cyl")))
talk_group_by = function(.data, cmd, verbose = FALSE, ...) {

  data_colnames = colnames(.data)
  out = talk_group_by_expr(data_colnames, cmd)

  out = lapply(out, function(x) {
    group_by(.data = .data, !!! rlang::parse_exprs(x))
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}


#' @export
#' @rdname talk_group_by
#' @param data_colnames column names of the data
talk_group_by_expr  = function(data_colnames, cmd, ...) {
  res = talk_get_colnames(data_colnames, cmd, ...)
  out = lapply(res, function(x) {
    x$df_var
  })
  out = lapply(out, rlang::expr)
}


#' @export
#' @rdname talk_group_by
talk_process_group_by_cmd = function(cmd) {
  # make ascending
  cmd = talk_process_arrange_cmd(cmd)
  cmd = gsub("ascending", "", cmd)
  cmd = gsub("descending", "", cmd)

  return(cmd)
}
