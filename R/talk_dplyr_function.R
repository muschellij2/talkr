#' Determine the `dplyr` function from the command
#'
#' @param cmd A command to pass, that will determine which
#' \code{dplyr} function to use
#'
#' @return A string
#' @export
#'
#' @examples
#' cmds = c("Sort df by  mpg", "arrange the data by mpg", "group_by mpg")
#' res = sapply(cmds, talk_dplyr_function)
#' testthat::expect_true(talk_dplyr_function("group_by MPG") == "group_by")
talk_dplyr_function = function(cmd) {
  stopifnot(rlang::is_string(cmd))
  cmd = process_cmd(cmd)
  search_words = c("sort", "arrange", "order")
  search_str = paste0(search_words, collapse = "|")
  dplyr_func = ""
  if (grepl(search_str, x = cmd)) {
    dplyr_func = "arrange"
  }
  select_words = c("select", "grab", "keep", "subset")
  select_words = c(select_words, "order column(s|)\\s*(the|)\\s*by")

  first_word = strsplit(cmd, " ")[[1]]
  first_word = first_word[1]
  if (first_word %in% c("select", "arrange", "filter",
                        "mutate", "group_by", "summarize",
                        "count", "tally",
                        "add_count", "add_tally",
                        "rename", "ggplot", "replace",
                        "regress")) {
    dplyr_func = first_word
  }
  # if (grepl(" all ", cmd)) {
  #   dplyr_func = paste0(dplyr_func, "_all")
  # }
  # if (grepl(" if ", cmd)) {
  #   dplyr_func = paste0(dplyr_func, "_all")
  # }
  return(dplyr_func)
}
