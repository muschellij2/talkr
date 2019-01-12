#' Replace Ordinal Numeric in a Command
#'
#' @param cmd Command string to replace ordinal values
#' @param max_n Maxium number to go over.  For example
#' if \code{max_n = 100} then \code{"one hundred one"} will not be
#' replaced
#' @return A character string same length as \code{cmd}
#' @export
#'
#' @examples
#' cmd = c("sort the twelfth column", "filter the third row ")
#' replace_ordinals(cmd)
#' @importFrom english english ordinal
replace_ordinals = function(cmd, max_n = 100) {
  # nc = 20
  # res = seq(nc)
  # eng = english(res)
  # ord = ordinal(res)
  for (inc in seq(max_n)) {
    val = english::english(inc)
    # twelve to 12
    cmd = gsub(val, inc, cmd)

    # twelfth column to column12
    val = english::ordinal(inc)
    find_str = paste0(val, " (column|row)( |s|$)")
    rep_str = paste0("\\1", inc)
    cmd = gsub(find_str, rep_str, cmd)
  }
  cmd
}
