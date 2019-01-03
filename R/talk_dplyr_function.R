#' Determine the `dplyr` function from the command
#'
#' @param cmd A command to pass, that will determine which
#' \code{dplyr} function to use
#'
#' @return A string
#' @export
#'
#' @examples
#' cmds = c("Sort df by  mpg", "arrange the data by mpg")
#' sapply(cmds, talk_dplyr_function)
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

  # if (grepl(" all ", cmd)) {
  #   dplyr_func = paste0(dplyr_func, "_all")
  # }
  # if (grepl(" if ", cmd)) {
  #   dplyr_func = paste0(dplyr_func, "_all")
  # }
  return(dplyr_func)
}
