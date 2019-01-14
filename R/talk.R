#' Main function to "talk" to \code{dplyr}
#'
#' @param .data A \code{data.frame} to perform the commands on
#' @param cmd the natural language command
#' @param ... Additional arguments to pass to the associated
#' \code{talkr} function.
#' @param error_find_function Should the command error if the function
#' cannot be found from the command?
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
talk = function(.data, cmd, error_find_function = TRUE, ...) {
  stopifnot(rlang::is_string(cmd))
  func = talk_dplyr_function(cmd)
  cmd = process_cmd(cmd)
  if (func == "") {
    if (error_find_function) {
      stop("dplyr Function not determined from command")
    } else {
      print("couldn't find function")
      return(.data)
    }
  }
  talk_func = paste0("talk_", func)
  res = do.call(talk_func, args = list(.data = .data, cmd = cmd, ...))
  return(res)
}

#' @export
#' @rdname talk
talk_expr = function(.data, cmd, error_find_function = TRUE, ...) {
  stopifnot(rlang::is_string(cmd))
  cmd = process_cmd(cmd)
  func = talk_dplyr_function(cmd)
  if (func == "") {
    if (error_find_function) {
      stop("dplyr Function not determined from command")
    } else {
      print("couldn't find function")
      return(.data)
    }
  }
  talk_func = paste0("talk_", func, "_expr")
  res = do.call(talk_func, args = list(data_colnames = .data, cmd = cmd, ...))
  if (length(res) == 1 &
      is.list(res)) {
    res = res[[1]]
  }
  if (func %in% c("filter", "select")) {
    res = res$condition
  }
  L = list(expression = res,
           func = func)
  return(L)
}

