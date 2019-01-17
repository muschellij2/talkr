
# "subset rows by hp > 5",
# "drop values that do not have an age < 4",
# "keep rows",
# "drop rows",

# the idea is this - keep filter
# if condition is NOT then "not filter"
# put condition in (), then ! if NOT filter

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
#' cmds = c("subset rows, by horsepower > 100",
#'         "keep rows where horsepower less than 100",
#'         "drop rows where American equals 1",
#'         "drop rows where American is equal to 1",
#'         "drop rows where American is equal 1",
#'         "drop values in column 6 not equal to 4",
#'         "filter out MPG greater than 30",
#'         "filter the rows where American is not less than or equal 0",
#'         "return the data with values of cylinders not less than 4",
#'         "filter where cylinders are not missing",
#'         "filter if cylinders isn't missing",
#'         "filter so gear equal 6")
#'  data_colnames = colnames(df)
#'  exprs = lapply(cmds, talk_filter_expr, data_colnames = df)
#'  exprs = sapply(exprs, function(x) x$condition)
#'  results = lapply(cmds, talk_filter, .data = df)
#'  cyl = df %>%
#'  talk_filter("filter if cylinders is equal to 6")
#' testthat::expect_true(all(cyl$cylinders == 6))
#'  results = lapply(cmds, talk_filter, .data = df)
#' cmd = "filter where cylinders are not missing and gear greater than 6"
#' res = talk_filter_expr(cmd, data_colnames = df)
#' testthat::expect_true(res$condition == "!is.na(cylinders) & gear > 6")
#'
#' cmd = "filter where cylinders equal to 4 or gear equals 6"
#' res = talk_filter_expr(cmd, data_colnames = df)
#' testthat::expect_true(res$condition == "cylinders == 4 | gear == 6")
#' talk_colnames_class(cyl)
talk_filter = function(.data, cmd,
                       verbose = FALSE,
                       ...) {
  if (verbose) {
    message("call to talk_filter")
  }
  stopifnot(rlang::is_string(cmd))
  data_colnames = colnames(.data)
  out = talk_filter_expr(data_colnames, cmd, ...)

  out = out$condition
  if (verbose) {
    message(out)
  }

  # x = out[[5]]
  colnames(.data) = tolower(data_colnames)
  out = lapply(out, function(x) {
    ret = filter(.data = .data, !!!friendlyeval::treat_strings_as_exprs(x))
    colnames(ret) = data_colnames
    ret
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}

#' @export
#' @rdname talk_filter
#' @param data_colnames column names of the data
#' @importFrom utils head
talk_filter_expr = function(data_colnames, cmd, ...) {

  cmd_id = is_not = NULL
  rm(list = c("is_not", "cmd_id"))
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

  cmd = process_cmd(cmd, drop_punct = FALSE)
  cmd = fix_contractions(cmd)
  cmd = remove_punct_keep_ops(cmd)
  cmd = talk_process_filter_cmd(cmd)
  cmd = remove_df(cmd)

  bad = !grepl("condition", cmd)
  if (any(bad)) {
    cmd[bad] = paste0("filter condition ", cmd[bad])
  }



  split_condition = strsplit(cmd, split = "condition")
  split_condition = lapply(split_condition, trimws)
  split_condition = lapply(split_condition, function(x) {
    x = x[ !x %in% ""]
    xx = paste(x, collapse = " ")
    res = is_condition_not(x[1], clean_cmd = "filter")
    x = x[-1]
    df = tibble(
      command = xx,
      is_not = res$not_condition,
      command_clean = res$clean_cmd,
      attempt_clean = res$cmd,
      condition = x
    )
  })
  split_condition = bind_rows(split_condition, .id = "cmd_id")
  # split_condition = t(sapply(split_condition, function(x) {
  #   x = x[ !x %in% ""]
  #   x = c(x[1], paste(x[2:length(x)], collapse = " "))
  #   res = is_condition_not(x[1], clean_cmd = "filter")
  #   c(x, res$not_condition, res$clean_cmd, res$cmd)
  # }))
  # colnames(split_condition) = c("command", "condition",
  #                               "is_not",
  #                               "command_clean",
  #                               "attempt_clean")
  split_condition = tibble::as_tibble(split_condition)
  split_condition$is_not = as.logical(split_condition$is_not )

  names(cn) = data_colnames

  search_words = c("filter", "subset", "select")
  search_str = paste0(search_words, collapse = "|")

  stopifnot(grepl(search_str, x = split_condition$command_clean))


  my_stopwords = c("the", "by", "it", "and",
                   "with", "values", "a", "an",
                   "is", "of",
                   "in",
                   "up",
                   "there",
                   "are",
                   "out",
                   "filter",
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

  operators = c("%in%",
                ">=", ">",
                "<=", "<",
                "==", "!=")

  column_indices = function(cmd, cn) {
    cmd = trimws(cmd)
    ind = grepl("column", cmd)
    cn_name = paste0("column", 1:length(cn))
    # ind = ind | grepl("^\\d*$", cmd)
    if (any(ind)) {
      xx = cmd[ind]
      for (icn in seq_along(cn)) {
        xx = gsub(cn_name[icn], cn[icn], xx)
      }
      xx = trimws(xx)
      cmd[ind] = xx
    }
    return(cmd)
  }

  ss = sapply(split_condition$condition, column_indices, cn = cn)
  split_condition$condition  = ss

  test_string = c("na", "finite", "nan")
  test_string = paste(
    # order matters here
    # c(paste0("!is.", test_string),
    c(
      paste0("is.", test_string)
    ),
    collapse = "|")
  test_string = paste0("(", test_string, ")")

  condition = split_condition$condition
  split_condition = split_condition %>%
    mutate(
      condition = gsub(paste0("(.*)(!| )", test_string), "\\2\\3(\\1)",
                       condition),
      # OCD with spaces
      condition = sub(" )$", ")", condition),
      condition = trimws(condition))
  # before and and or - this did not exist
  split_condition = split_condition %>%
    group_by(cmd_id) %>%
    mutate(condition = paste(condition, collapse = " ")) %>%
    head(1) %>%
    ungroup

  split_condition = split_condition %>%
    mutate( condition = paste0(ifelse(is_not, "!(", ""),
                         condition,
                         ifelse(is_not, ")", ""))
    )

  return(split_condition)
}


#' @export
#' @rdname talk_filter
talk_process_filter_cmd = function(cmd) {
  # cmd = c("and then", "greater then", "then")
  # cmd = gsub("([^and] | |^)then", "\\1than", cmd)
  cmd = trimws(cmd)

  # make ascending
  cmd = gsub("higher", "greater", cmd)
  cmd = gsub("larger", "greater", cmd)

  cmd = gsub("^subset", "filter", cmd)
  cmd = gsub("^(go and|go in and|go in|)grab", "filter", cmd)
  cmd = gsub("^return", "filter", cmd)
  cmd = gsub("^select row(s|)", "filter", cmd)
  cmd = gsub("^keep row(s|)", "filter", cmd)
  cmd = gsub("^extract", "filter", cmd)
  cmd = gsub("^(give|give me|give us|return) (back |)row[[:alpha:]]",
             "filter", cmd)

  cmd = gsub(" were ", " are ", cmd)
  cmd = sub("^do ", "", cmd)

  cmd = gsub("lower", "less", cmd)
  cmd = gsub("fewer", "less", cmd)

  cmd = gsub("not greater", "less", cmd)
  cmd = gsub("not less", "greater", cmd)

  cmd = gsub("not drop", "filter", cmd)
  cmd = gsub("not keep", "drop", cmd)

  cmd = gsub("equal to ", "equal ", cmd)

  cmd = gsub("greater than (or |)equal to", ">=", cmd)
  cmd = gsub("less than (or |)equal to", "<=", cmd)

  cmd = gsub("greater than (or |)equal", ">=", cmd)
  cmd = gsub("less than (or |)equal", "<=", cmd)

  cmd = gsub("over the", "greater than", cmd)
  cmd = gsub("under the", "less than", cmd)

  cmd = gsub("greater than", ">", cmd)
  cmd = gsub("less than", "<", cmd)

  # equal to never goign to happen due to gsub above
  cmd = gsub("is equal to", "==", cmd)
  cmd = gsub("is equal ", "== ", cmd)
  cmd = gsub("not equal to", "!=", cmd)

  test_string = "(missing|finite|infinite|infinity|nan|not a number)"
  cmd = gsub(paste0("is ", test_string), "is.\\1", cmd)
  cmd = gsub(paste0("are ", test_string), "is.\\1", cmd)

  cmd = gsub(paste0("is not ", test_string), "!is.\\1", cmd)
  cmd = gsub(paste0("are not ", test_string), "!is.\\1", cmd)

  cmd = gsub("not a number", "nan", cmd)
  cmd = gsub("is[.]infinity", "is.infinite", cmd)
  cmd = gsub("is[.]missing", "is.na", cmd)

  cmd = gsub("!is[.](na|finite|infinite|nan)", " !is.\\1", cmd)
  cmd = gsub("is[.](na|finite|infinite|nan)", " is.\\1", cmd)

  cmd = trim_multi_space(cmd)
  cmd = gsub("! is[.]", " !is.", cmd)
  cmd = trim_multi_space(cmd)

  # cmd = gsub("are not missing", "!is.na", cmd)

  cmd = gsub("equals", "==", cmd)
  cmd = gsub("not equal(s|)", "!=", cmd)

  cmd = gsub("greater than", ">", cmd)
  cmd = gsub("less than", "<", cmd)

  cmd = gsub("equal", "==", cmd)

  cmd = gsub("record(s|)", "row", cmd)
  cmd = gsub("cell(s|)", "row", cmd)
  cmd = gsub("element(s|)", "row", cmd)
  cmd = gsub("case(s|)", "row", cmd)
  cmd = gsub(" rows ", " row ", cmd)

  cmd = gsub(" by ", " condition ", cmd)
  cmd = gsub(" where ", " condition ", cmd)
  cmd = gsub(" which ", " condition ", cmd)
  cmd = gsub(" with ", " condition ", cmd)
  cmd = gsub(" so ", " condition ", cmd)
  cmd = gsub(" if ", " condition ", cmd)

  cmd = gsub(" value(s|) ", " condition ", cmd)
  cmd = gsub(" row(s|) ", " row ", cmd)


  bad = !grepl("condition", cmd)
  if (any(bad)) {
    cmd[bad] = gsub("(^| )(out|drop|keep|remove) ", " \\1 condition ", cmd[bad])
    cmd[bad] = trimws(cmd[bad])
  }

  #
  cmd = gsub("variable", "column", cmd)
  cmd = gsub("covariate", "column", cmd)

  # column 1 becomes column1
  cmd = gsub("columns", "column", cmd)
  cmd = gsub("column (\\d+)", "column\\1", cmd)
  cmd = gsub("column(\\D|$)", "", cmd)

  cmd = gsub(" and ", " condition & ", cmd)
  cmd = gsub(" or ", " condition | ", cmd)

  return(cmd)
}


#' @export
#' @rdname talk_filter
talk_colnames_class = function(.data) {

  cn_df = talk_check_colnames(.data)
  cn_df = cn_df$colname_df
  cn_df$classes = sapply(.data, class)
  cn_df
}

