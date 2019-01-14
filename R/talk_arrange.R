#' Talk to Arrange a Data set
#'
#' @param cmd Command to perform on the data set.
#' @param .data The data set/\code{data.frame} to
#' perform the operation.
#' @param verbose print diagnostic output
#' @param ... additional arguments to pass to \code{\link{talk_get_colnames}}
#'
#' @return A \code{data.frame} or list of them if \code{length(cmds) > 1}.
#' @export
#' @importFrom rlang is_string expr parse_exprs
#' @import dplyr
#' @importFrom tibble as_tibble tibble
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' df = mtcars %>%
#'   rownames_to_column(var = "car")
#'
#' cmds = c(
#'     "Sort by  mpg",
#'     "Sort by  column    mpg  ",
#'     "arrange by column 5",
#'     "arrange by gear",
#'     "arrange by columns 4 and 5",
#'     "arrange by columns 4 and 5, mpg decreasing",
#'     # duplciate
#'     "arrange by columns 2 and 5, mpg decreasing",
#'     "arrange by columns 4, 5, and 6",
#'     "sort by mpg descending",
#'     "sort by MPG ascending",
#'     "sort by mpg ascending",
#'     "sort by mpg low to high")
#'  results = lapply(cmds, talk_arrange, .data = df)
#' df = df %>%
#'   rename(GEAR = gear)
#'   cmd = "arrange by gear descending"
#'  data_colnames = df
#'  gear = df %>%
#'  talk_arrange("arrange by gear")
#' testthat::expect_true(!is.unsorted(gear$GEAR))
talk_arrange = function(.data, cmd, verbose = FALSE, ...) {

  data_colnames = colnames(.data)
  out = talk_arrange_expr(data_colnames, cmd, ...)
  # x = out[[5]]
  out = lapply(out, function(x) {
    arrange(.data = .data, !!! rlang::parse_exprs(x))
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}


#' @export
#' @rdname talk_arrange
#' @param data_colnames column names of the data
talk_arrange_expr  = function(data_colnames, cmd, ...) {

  var = variable = NULL
  ordering = var_num = df_var = NULL
  rm(list = c("var", "variable", "ordering", "var_num", "df_var"))

  ss = talk_get_colnames(data_colnames, cmd, ...)
  if (is.character(ss)) {
    ss = list(ss)
  }
  d = talk_check_colnames(data_colnames)
  data_colnames = d$data_colnames
  cn_df = d$colname_df
  cn = d$lower_colnames

  res = lapply(ss, function(x) {
    d = tibble(
      var = x,
      variable = !var %in% c("descending", "ascending"),
      var_num = cumsum(variable)
    )
    variables = d[ d$variable, ]
    orders = d[ !d$variable, ]
    orders = orders %>%
      rename(ordering = var) %>%
      select(ordering, var_num)
    variables = left_join(variables, orders, by = "var_num")
    variables = variables %>%
      select(-variable) %>%
      mutate(ordering = ifelse(is.na(ordering), "ascending",
                               ordering)
      )

    s = variables %>%
      group_by(var) %>%
      summarize(
        num_records = n(),
        num_ordering = length(unique(ordering))
      )
    # if say ascending descending
    if (max(s$num_ordering) > 1) {
      warning("Multiple orderings given, first chosen")
      print(s[ s$num_ordering > 1,])
    }
    variables = variables %>%
      group_by(var) %>%
      dplyr::slice(1)
    variables = left_join(
      variables,
      cn_df, by = "var")

    variables = variables %>%
      ungroup() %>%
      arrange(var_num) %>%
      mutate(var_out = ifelse(
        ordering == "descending",
        paste0("desc(", df_var, ")"),
        df_var
      ))

    return(variables)
  })

  out = lapply(res, function(x) {
    x$var_out
  })
  out = lapply(out, rlang::expr)
}


#' @export
#' @rdname talk_arrange
talk_process_arrange_cmd = function(cmd) {
  # make ascending
  cmd = gsub("lowest", "low", cmd)
  cmd = gsub("highest", "high", cmd)
  cmd = gsub("low to high", "ascending", cmd)
  cmd = gsub("high to low", "descending", cmd)

  cmd = gsub("increase", "ascending", cmd)
  cmd = gsub("decrease", "descending", cmd)

  cmd = gsub("increasing", "ascending", cmd)
  cmd = gsub("decreasing", "descending", cmd)

  #
  cmd = gsub("variable", "column", cmd)
  cmd = gsub("covariate", "column", cmd)

  # column 1 becomes column1
  cmd = gsub("columns", "column", cmd)
  cmd = gsub("column (\\d+)", "column\\1", cmd)
  cmd = gsub("column(\\D|$)", "", cmd)

  return(cmd)
}
