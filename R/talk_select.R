#' Talk to Filter a Data set
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
#' @importFrom friendlyeval treat_string_as_expr
#' @import dplyr
#' @importFrom tibble as_tibble data_frame
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' df = tibble::rownames_to_column(mtcars, var = "car")
#' df = df %>%
#'   rename(cylinders = cyl,
#' horsepower = hp,
#' American = am,
#' carburetor = carb)
#' .data = df
#' cmds = c("subset columns 5 and 6",
#'         "select column horsepower",
#'         "drop columns 1 and 3 american",
#'         "select MPG column four 5")
#' cmd = cmds
#'  data_colnames = colnames(df)
#'  exprs = sapply(cmds, talk_select_expr, data_colnames = df)
#'  exprs = sapply(exprs, function(x) x$condition)
#'  results = lapply(cmds, talk_select, .data = df)
#'  cyl = df %>%
#'  talk_select("select if cylinders American")
#' testthat::expect_true(all(colnames(cyl) == c("cylinders", "American")))
talk_select = function(.data, cmd,
                       verbose = FALSE, ...) {
  if (verbose) {
    message("call to talk_select")
  }
  # stopifnot(rlang::is_string(cmd))
  data_colnames = colnames(.data)
  names(data_colnames) = tolower(data_colnames)
  out = talk_select_expr(data_colnames, cmd, ...)

  if (verbose) {
    message(out)
  }

  # x = out[[5]]
  out = lapply(out, function(x) {
    x = x$condition
    ret = select(.data = .data, !!!friendlyeval::treat_strings_as_exprs(x))
    # need match
    ret
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}

#' @export
#' @rdname talk_select
#' @param data_colnames column names of the data
talk_select_expr = function(data_colnames, cmd, ...) {

  # res = talk_get_colnames(data_colnames, cmd, ...)

  df_var = var_num = is_not = NULL
  rm(list = c("is_not", "df_var", "var_num"))
  if (is.data.frame(data_colnames)) {
    data_colnames = colnames(data_colnames)
  }
  d = talk_check_colnames(data_colnames)
  data_colnames = d$data_colnames
  cn = d$lower_colnames
  cn_df = d$colname_df

  cmd = process_cmd(cmd, drop_punct = FALSE)
  cmd = fix_contractions(cmd)
  cmd = remove_punct_keep_ops(cmd)
  cmd = talk_process_select_cmd(cmd)
  cmd = remove_df(cmd)

  not_condition = clean_cmd = NULL
  rm(list = c("not_condition", "clean_cmd"))

  split_condition = is_condition_not(cmd, clean_cmd = "select")
  split_condition =  tibble::as_tibble(split_condition)
  split_condition = split_condition %>%
    rename(is_not = not_condition,
           command_clean = clean_cmd,
           condition = cmd)
  split_condition$is_not = as.logical(split_condition$is_not )


  search_words = c("filter", "subset", "select")
  search_str = paste0(search_words, collapse = "|")
  stopifnot(grepl(search_str, x = split_condition$command_clean))


  my_stopwords = c("the", "by", "it", "and",
                   "with", "values", "a",
                   "is", "of",
                   "in",
                   "up",
                   "there",
                   "are",
                   "filter",
                   "select",
                   "subset",
                   "so", "that",
                   "value",
                   "row",
                   "have",
                   "where",
                   "only",
                   "rows", "want")
  my_stopwords = c(my_stopwords, search_words)
  ss = strsplit(split_condition$condition, " ")
  ss = sapply(ss, function(x) {
    paste(x[ !x %in% my_stopwords], collapse = " ")
  })
  split_condition$condition = ss

  split_condition$n = 1:nrow(split_condition)
  split_condition = split(split_condition, split_condition$n)
  variables = lapply(split_condition, function(s) {
    res = talk_cmd_to_colnames(
      data_colnames = data_colnames,
      cmd = s$condition)
    variables = talk_get_colnames(
      data_colnames = data_colnames,
      cmd = s$condition,
      ...
    )
    v = tibble(df_var = variables)

    v$is_not = s$is_not
    v = v %>%
      ungroup() %>%
      mutate(condition = ifelse(
        is_not,
        paste0("-", df_var),
        df_var
      ))
    v$command_clean = s$command_clean
    v$full_command = paste0(s$command_clean,
                        "(", paste(v$condition, collapse = ", "),
                        ")")
    v
  } )


  return(variables)
}


#' @export
#' @rdname talk_select
talk_process_select_cmd = function(cmd) {
  # cmd = c("and then", "greater then", "then")
  # cmd = gsub("([^and] | |^)then", "\\1than", cmd)
  cmd = trimws(cmd)


  cmd = gsub("variable", "column", cmd)
  cmd = gsub("covariate", "column", cmd)

  cmd = gsub("^subset", "select", cmd)
  cmd = gsub("^(go and|go in and|go in|)grab", "select", cmd)
  cmd = gsub("^return", "select", cmd)
  # cmd = gsub("^select column(s|)", "select", cmd)
  # cmd = gsub("^keep column(s|)", "select", cmd)
  cmd = gsub("^extract", "select", cmd)
  cmd = gsub("^(give|give me|give us|return) (back |)column",
             "select column", cmd)

  cmd = gsub(" were ", " are ", cmd)
  cmd = sub("^do ", "", cmd)

  cmd = gsub("lower", "less", cmd)
  cmd = gsub("fewer", "less", cmd)

  cmd = gsub("not greater", "less", cmd)
  cmd = gsub("not less", "greater", cmd)

  cmd = gsub("not drop", "select", cmd)
  cmd = gsub("not keep", "drop", cmd)


  # column 1 becomes column1
  cmd = gsub("columns", "column", cmd)
  cmd = gsub("column (\\d+)", "column\\1", cmd)
  cmd = gsub("column(\\D|$)", "", cmd)


  return(cmd)
}
