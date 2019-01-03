#' Talk to Arrange a Data set
#'
#' @param cmd Command to perform on the data set.
#' @param .data The data set/\code{data.frame} to
#' perform the operation.
#'
#' @return A \code{data.frame} or list of them if \code{length(cmds) > 1}.
#' @export
#' @importFrom rlang is_string expr parse_exprs
#' @import dplyr
#' @importFrom tibble as_tibble data_frame
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' df = mtcars %>%
#'   rownames_to_column(var = "car")
#'   cmds = c(
#'     "Sort df by  mpg",
#'     "Sort df by  column    mpg  ",
#'     "arrange by column 5",
#'     "arrange by gear",
#'     "arrange by columns 4 and 5",
#'     "arrange by columns 4 and 5, mpg decreasing",
#'     # duplciate
#'     "arrange by columns 2 and 5, mpg decreasing",
#'     "arrange by columns 4, 5, and 6",
#'     "sort by mpg descending",
#'     "sort by mpg ascending",
#'     "sort by mpg ascending",
#'     "sort by mpg low to high")
#'  results = lapply(cmds, talk_arrange, .data = df)
#' df = df %>%
#'   rename(GEAR = gear)
#'  gear = df %>%
#'  talk_arrange("arrange by gear")
#' testthat::expect_true(!is.unsorted(gear$GEAR))
talk_arrange = function(.data, cmd) {
  # stopifnot(rlang::is_string(cmd))
  cmd = process_cmd(cmd)
  cmd = talk_process_arrange_cmd(cmd)
  cmd = remove_df(cmd)
  reg_cn = colnames(.data)
  cn_df = data_frame(
    var = tolower(reg_cn),
    df_var = reg_cn
  )
  if (any(duplicated(cn_df$var))) {
    stop("Need to have distinct column names, even with case")
  }
  cn = tolower(reg_cn)
  names(cn) = reg_cn

  search_words = c("sort", "arrange", "order")
  search_str = paste0(search_words, collapse = "|")
  stopifnot(grepl(search_str, x = cmd))


  my_stopwords = c("the", "by", "it", "and")
  my_stopwords = c(my_stopwords, search_words)
  ss = strsplit(cmd, " ")
  ss = lapply(ss, function(x) {
    x[ !x %in% my_stopwords]
  })
  column_indices = function(cmd, cn) {
    cmd = trimws(cmd)
    ind = grepl("column", cmd)
    ind = ind | grepl("^\\d*$", cmd)
    if (any(ind)) {
      xx = cmd[ind]
      xx = gsub("column", "", xx)
      xx = trimws(xx)
      xx = as.numeric(xx)
      if (any(xx > length(cn))) {
        stop("column index > than number of column names")
      }
      xx = cn[xx]
      cmd[ind] = xx
    }
    return(cmd)
  }

  ss = lapply(ss, column_indices, cn = cn)
  allowed_words = c(cn, "descending", "ascending")

  check = sapply(ss, function(x) {
    all(x %in% allowed_words)
  })

  if (!all(check)) {
    warning("Some words not allowed! Removed")
    print(ss[!check])
  }

  # x = ss[[4]]

  res = lapply(ss, function(x) {
    x = x[x %in% allowed_words]
    d = data_frame(
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
      # mutate(var_out = ifelse(
      #   ordering == "descending",
      #   paste0("desc(", var, ")"),
      #   var
      # ))
    # variables = paste(variables, collapse = ", ")
    return(variables)
  })

  out = lapply(res, function(x) {
    x$var_out
  })
  out = lapply(out, rlang::expr)

  # x = out[[5]]
  out = lapply(out, function(x) {
    arrange(.data = .data, !!! rlang::parse_exprs(x))
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
}



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
