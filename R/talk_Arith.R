#' Talk to Arith all types a Data set
#'
#' @param cmd Command to perform on the data set.
#' @param .data The data set/\code{data.frame} to
#' perform the operation.
#' @param verbose print diagnostic output
#' @param ... additional arguments to pass to \code{\link{talk_get_colnames}}
#'
#' @return A \code{data.frame} or list of them if \code{length(cmds) > 1}.
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' df = mtcars %>%
#'   rownames_to_column(var = "car")
#'   cmds = c(
#'     "Arith gear mpg as factors",
#'     "Arith car as factor",
#'     "Arith gear as character")
#'  data_colnames = df
#'  .data = df
#'  cmd = cmds[1]
#'  results = lapply(cmds, talk_Arith, .data = df)
#'  testthat::expect_warning(talk_Arith(.data, cmd),
#'  "allowed")
#' df = df %>%
#'   rename(GEAR = gear)
#'  gear = df %>%
#'  talk_Arith("Arith by gear and column 3 as factors")
#'  d_classes = sapply(gear[, c("cyl", "GEAR")], class)
#' testthat::expect_true(all(d_classes == "factor"))
talk_Arith = function(.data, cmd, verbose = FALSE, ...) {

  data_colnames = colnames(.data)
  out = talk_Arith_expr(data_colnames, cmd, ...)

  out = lapply(out, function(x) {
    func = attr(x, "Arithment_function")
    mutate_at(.tbl = .data, .vars = vars(!!! x), .funs = func)
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}


talk_Arith_classes = function() {
  x = c("factor", "logical", "numeric", "character", "integer")
  x = c(x, paste0(x, "s"))
  x
}


#' @export
#' @rdname talk_Arith
#' @param data_colnames column names of the data
#' @param allowed_words words allowed to be in the command other than
#' the column names
talk_Arith_expr  = function(
  data_colnames, cmd,
  allowed_words = c("factor", "logical", "numeric", "character",
                    "double"),
  ...) {
  cmd = talk_process_Arith_cmd(cmd)
  out = talk_get_colnames(
    data_colnames, cmd,
    allowed_words = allowed_words, ...)
  if (is.character(out)) {
    out = list(out)
  }
  out = lapply(out, function(xout) {
    types = c("factor", "numeric", "double", "logical", "character" )
    check = types %in% xout
    if (!any(check)) {
      stop("Do not know which type to convert to")
    }
    if (sum(check) > 1) {
      keep = types[check]
      keep = paste(keep, collapse = ", ")
      warnings(paste0("Multiple types found: ", keep, ", using ",
                      types[check][1]))
    }
    out_type = types[check][1]
    out_type = recode(out_type,  "double" = "numeric")
    out_type = paste0("as.", out_type)
    xout = xout[ !xout %in% types]
    xout = friendlyeval::treat_strings_as_exprs(xout)
    attr(xout, "Arith_operator") = out_type
    xout
  })
  return(out)
}


#' @export
#' @rdname talk_Arith
talk_process_Arith_cmd = function(cmd) {
  # make ascending
  cmd = talk_process_arrange_cmd(cmd)

  types = c("add", "plus",
            "subtract",  "minus",
            "multiply", "times",
            "divide",
            "power", "square", "cube", "raise")
  for (itype in types) {
    cmd = gsub(paste0("(^| )", itype, "s( |$)"),
               paste0(" ", itype, " "), cmd)
  }
  cmd = trim_multi_space(cmd)

  return(cmd)
}
