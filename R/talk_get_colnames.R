#' Talk to Arrange a Data set
#'
#' @param cmd Command to perform on the data set.
#' @param data_colnames column names of the data
#' @param ... additional arguments to pass to \code{\link{talk_get_colnames}}
#'
#' @param stop_words Words to remove from the command
#' @param additional_stop_words additional stop words to
#' remove.  Helpful if you want to pass in these words
#' instead of with \code{stop_words}
#' @param allowed_words words allowed to be in the command other than
#' the column names
#' @param remove_duplicated remove duplicated columns in the
#' command
#'
#' @return A \code{data.frame} or list of them if \code{length(cmds) > 1}.
#' @export
#' @importFrom rlang is_string expr parse_exprs
#' @import dplyr
#' @importFrom tibble as_tibble tibble
#' @examples
#' library(tibble)
#' library(dplyr)
#' df = mtcars %>%
#'   rownames_to_column(var = "car")
#'   cmds = c(
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
#'     "sort by mpg ascending",
#'     "sort by mpg ascending",
#'     "sort by mpg low to high hat")
#'  data_colnames = df
#'  cmd = "arrange by columns 4 and 5, mpg decreasing"
#'  results = lapply(cmds, talk_get_colnames, data_colnames = df)
#'  results = lapply(cmds, talk_get_colnames, data_colnames = df,
#'  allowed_words = "")
#'  results = lapply(cmds, talk_get_colnames, data_colnames = df,
#'  allowed_words = NULL)
#'
talk_get_colnames = function(
  data_colnames, cmd,
  stop_words = talkr::talk_stop_words,
  additional_stop_words = NULL,
  remove_duplicated = TRUE,
  allowed_words = c("descending", "ascending"),
  ...
) {

  var = variable = NULL
  ordering = var_num = df_var = NULL
  rm(list = c("var", "variable", "ordering", "var_num", "df_var"))


  # stopifnot(rlang::is_string(cmd))
  cmd = process_cmd(cmd)
  cmd = talk_process_arrange_cmd(cmd)
  cmd = remove_df(cmd)
  if (!rlang::is_string(cmd)) {
    warning("Command should be string, results may be wrong")
  }
  if (is.data.frame(data_colnames)) {
    data_colnames = colnames(data_colnames)
  }
  ss = talk_cmd_to_colnames(data_colnames = data_colnames,
                            cmd = cmd, stop_words = stop_words,
                            additional_stop_words = additional_stop_words)
  d = talk_check_colnames(data_colnames)
  data_colnames = d$data_colnames
  cn_df = d$colname_df
  cn = d$lower_colnames

  allowed_words = c(cn, allowed_words)
  allowed_words = allowed_words[ !allowed_words %in% ""]
  check = sapply(ss, function(x) {
    all(x %in% allowed_words)
  })

  if (!all(check)) {
    warning("Some words not allowed! Removed")
    print(ss[!check])
  }

  ss = lapply(ss, function(x) {
    x = x[x %in% allowed_words]
  })

  check_duplicated = sapply(ss, function(x) {
    any(duplicated(x))
  })
  if (any(check_duplicated)) {
    warning("Duplicate columns detected!")
    if (remove_duplicated) {
      ss = lapply(ss, function(x) {
        x[ !duplicated(x)]
      })
    } else {
      # warn?
    }
  }

  ss = lapply(ss, function(x) {
    variables = tibble(var = x)
    variables = left_join(
      variables,
      cn_df, by = "var")
    variables = variables %>%
      mutate(df_var = ifelse(
        var %in% allowed_words & is.na(df_var),
        var, df_var)
      ) %>%
      filter(!is.na(df_var))
    variables$df_var
  })

  if (length(ss) == 1) {
   ss = ss[[1]]
  }
  attr(ss, "allowed_words") = allowed_words
  return(ss)
}

#' @export
#' @rdname talk_get_colnames
talk_cmd_to_colnames = function(
  data_colnames, cmd,
  stop_words = talkr::talk_stop_words,
  additional_stop_words = NULL
) {

  d = talk_check_colnames(data_colnames)
  data_colnames = d$data_colnames
  cn_df = d$colname_df
  cn = d$lower_colnames

  my_stopwords = c("the", "by", "it", "and", "then",
                   "there",
                   "row",
                   "up",
                   "if",
                   "rows", "want",
                   "are")
  my_stopwords = c(my_stopwords, stop_words, additional_stop_words)
  my_stopwords = unique(my_stopwords)
  if (any(my_stopwords %in% cn)) {
    warning(
      paste0(
        "stop words given may be column names ",
        "of the data, results may fail"
      )
    )
  }

  ss = strsplit(cmd, " ")
  ss = lapply(ss, function(x) {
    x[ !x %in% my_stopwords]
  })
  column_indices = function(cmd, cn) {
    cmd = trimws(cmd)
    cmd = replace_ordinals(cmd, max_n = length(cn))
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

  return(ss)
}

#' @export
#' @rdname talk_get_colnames
talk_check_colnames = function(data_colnames) {

  if (is.data.frame(data_colnames)) {
    data_colnames = colnames(data_colnames)
  }
  cn_df = tibble(
    var = tolower(data_colnames),
    df_var = data_colnames
  )
  if (any(duplicated(cn_df$var))) {
    stop("Need to have distinct column names, even with case")
  }
  cn = tolower(data_colnames)
  names(cn) = data_colnames
  L = list(data_colnames = data_colnames,
           lower_colnames = cn,
           colname_df = cn_df)
  L
}
