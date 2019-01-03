#' Main function to "talk" to \code{dplyr}
#'
#' @param .data A \code{data.frame} to perform the commands on
#' @param cmd the natural language command
#' @param ... Additional arguments to pass to the associated
#' \code{talkr} function.
#'
#' @return A \code{data.frame}
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' df = mtcars %>%
#'   rownames_to_column(var = "car")
#' res = df %>%
#'   talk("Sort df by mpg")
talk = function(.data, cmd, ...) {
  stopifnot(rlang::is_string(cmd))
  cmd = process_cmd(cmd)
  func = talk_dplyr_function(cmd)
  talk_func = paste0("talk_", func)
  res = do.call(talk_func, args = list(.data = .data, cmd = cmd, ...))
  return(res)
}
