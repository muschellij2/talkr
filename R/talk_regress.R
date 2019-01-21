

#' Talk to Group a Data set
#'
#' @param cmd Command to perform on the data set.
#' @param .data The data set/\code{data.frame} to
#' perform the operation.
#' @param verbose print diagnostic output
#' @param ... additional arguments to pass to \code{\link{talk_get_colnames}}
#' or \code{\link{tidy}}
#' @param tidy should \code{\link{tidy}} be run?
#'
#' @return A \code{data.frame} or list of them if \code{length(cmds) > 1}.
#' @export
#' @importFrom stats glm as.formula
#' @importFrom broom tidy
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' df = mtcars %>%
#'   rownames_to_column(var = "car")
#'   cmds = c(
#'     "regress  mpg on cyl and hp",
#'     "regress  mpg on cyl and hp columns 4",
#'     "regress  mpg on cyl and hp and column 6",
#'     "regress am on  hp and column 6 on mpg binomial",
#'     "regress am on  hp and column 6 on mpg binomial logit link"
#'     )
#'  data_colnames = df
#'  .data = df
#'  results = lapply(cmds, talk_regress, .data = df)
#'  cmd = "regress mpg on cyl and hp and column 6 poisson"
#'  mod_1 = talk_regress(.data, cmd)
#'  cmd = "regress poisson cyl and hp and column 6 on mpg"
#'  mod_2 = talk_regress(.data, cmd)
#'  testthat::expect_equal(coef(mod_1), coef(mod_2))
#'  cmd = "regress poisson cyl and hp and column 6 on mpg"
#'  mod_2 = talk_regress(.data, cmd, exponentiate = TRUE)
#'
#'  cmd = "regress am on  hp and column 6 and mpg binomial"
#'  #'
#'  cmd =  "regress columns 2 and 5, mpg decreasing"
#'  testthat::expect_error(talk_regress(.data, cmd),
#'  "y variable")
talk_regress = function(.data, cmd,
                        tidy = TRUE,
                        verbose = FALSE, ...) {

  data_colnames = colnames(.data)
  out = talk_regress_expr(data_colnames, cmd, ...)

  make_family = function(family, link) {
    if (is.null(family)) {
      family = "gaussian"
    }
    args = list()
    args$link = link
    do.call(what = family, args = args)
  }

  x = out[[1]]

  out = lapply(out, function(x) {
    x = as.list(x)
    x = lapply(x, function(x) {
      if (is.null(x)) {
        return(x)
      }
      if (is.na(x)) {
        x = NULL
      }
      x
    })
    args = list(data = .data)
    args$formula = as.formula(x$formula)
    args$family = make_family(x$family, x$link)
    res = do.call(glm, args = args)
    # exp = x$family %in% c("binomial",
    #                       "poisson",
    #                       "quasibinomial",
    #                       "quasipoisson")
    if (tidy) {
      res = broom::tidy(res, conf.int = TRUE, ...)
    }
    # r = names(res$call)
    # # res$call = res$call[ r != ""]
    # # res$call$formula = call(res$call$formula )
    # res$call$formula = call(x$formula)
    # res$call[[ which(r == "")]] = as.name("glm")
    # res$call$data = as.name(".data")
    res
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}

#' @export
#' @rdname talk_regress
talk_regress_links = function() {
  links = c("logit", "probit", "cauchit", "cloglog", "identity",
            "log", "sqrt", "1/mu\\^2", "inverse")
  return(links)
}

#' @export
#' @rdname talk_regress
talk_regress_families = function() {
  families = c("binomial",
               "gaussian",
               "Gamma",
               "inverse.gaussian",
               "poisson",
               "quasi",
               "quasibinomial",
               "quasipoisson")
  return(families)
}

#' @export
#' @rdname talk_regress
#' @param data_colnames column names of the data
#' @param allowed_words words allowed to be in the command other than
#' the column names
talk_regress_expr  = function(
  data_colnames, cmd,
  allowed_words = c("condition",
                    talk_regress_links(),
                    talk_regress_families()),
  ...) {

  cmd_id = NULL
  rm(list = c("cmd_id"))

  cmd = fix_contractions(cmd)
  cmd = remove_punct_keep_ops(cmd)
  cmd = remove_df(cmd)
  cmd = talk_process_regress_cmd(cmd)

  bad = !grepl("condition", cmd)
  if (any(bad)) {
    cmd[bad] = paste0("regress condition ", cmd[bad])
  }

  ##############################
  # get links
  ##############################
  ss = strsplit(cmd, " ")
  links = sapply(ss, function(x) {
    x = x[ grepl("link=", x)]
    if (length(x) == 0) {
      x = NA
    } else {
      x = gsub("link=", "", x)
      x = gsub("'", "", x)
    }
    x
  })
  cmd = sapply(ss, function(x) {
    x = x[ !grepl("link=", x)]
    x = paste0(x, collapse = " ")
    x
  })


  res = talk_get_colnames(
    data_colnames,
    cmd, allowed_words = allowed_words, ...)
  if (is.list(res)) {
    res = sapply(res, function(x) {
      paste(x, collapse = " ")
    })
  } else {
    res = paste(res, collapse = " ")
  }

  ss = strsplit(res, split = " ")
  families = rep(NA_character_, length = length(res))
  find_fams = sapply(ss, function(x) {
    x = talk_regress_families() %in% x
    names(x) = talk_regress_families()
    x
  })
  cs = colSums(find_fams)
  if (any(cs > 1)) {
    print(res)
    stop("Multiple families found in the regression, failing")
  }
  have_fam = cs > 0
  if (any(have_fam)) {
    w = apply(find_fams[, have_fam, drop = FALSE], 2, which)
    families[have_fam] = sapply(w, function(r) {
      talk_regress_families()[r]
    })
  }
  res = sapply(ss, function(x) {
    x = x[ !x %in% talk_regress_families()]
    x = paste(x, collapse = " ")
    x
  })

  split_condition = tibble(
    original_cmd = cmd,
    command = res,
    link = links,
    family = families)
  split_condition$cmd_id = 1:nrow(split_condition)


  ss = strsplit(split_condition$command, split = "condition")
  ss = lapply(ss, trimws)
  x = ss[[1]]
  ss = lapply(ss, function(x) {
    x = x[ !x %in% ""]
    ss2 = strsplit(x, " ")
    nl = sapply(ss2, length)
    ss2 = ss2[order(nl)]
    x = sapply(ss2, paste, collapse = " + ")
    xx = paste(x, collapse = " ~ ")
    form = xx
    xx = paste0("regress ", xx)
    yvar = x[1]
    x = x[-1]
    if (length(x) >= 1) {
      condition = x
    } else {
      condition = NA
    }
    df = tibble(
      command_clean = "regress",
      attempt_clean = xx,
      yvar = yvar,
      condition = condition,
      formula = form
    )
  })
  split_condition_form = bind_rows(ss, .id = "cmd_id")
  split_condition_form = split_condition_form %>%
    mutate(cmd_id = as.numeric(cmd_id))
  split_condition = left_join(split_condition,
                              split_condition_form,
                              by = "cmd_id")

  split_condition = tibble::as_tibble(split_condition)

  good = grepl("~", split_condition$formula)
  if (!all(good)) {
    bad = split_condition[!good, ]
    print(bad[, c("original_cmd", "formula")])
    stop("Could not find y variable from command")
  }
  out = split(split_condition, split_condition$cmd_id)
  out
}


#' @export
#' @rdname talk_regress
talk_process_regress_cmd = function(cmd) {
  # make ascending
  cmd = talk_process_arrange_cmd(cmd)
  cmd = gsub("ascending", "", cmd)
  cmd = gsub("descending", "", cmd)

  cmd = trimws(cmd)

  cmd = gsub("inverse gaussian", "inverse.gaussian", cmd)
  cmd = gsub("quasi (binomial|poisson)", "quasi\\1", cmd)
  cmd = gsub(" gamma", " Gamma", cmd)
  cmd = gsub(" log([-]| ) linear", " log-linear", cmd)
  cmd = trim_multi_space(cmd)

  cmd = gsub(" log-linear ", " log link ", cmd)

  # cmd = gsub(" one ", " 1 ", cmd)
  cmd = gsub(" one ", " 1 ", cmd)
  cmd = gsub(" 1 over ", " 1/ ", cmd)
  cmd = gsub(" 1/\\s+mu ", " 1/mu ", cmd)
  cmd = gsub(" squared ", " ^2 ", cmd)
  cmd = gsub(" mu\\^2 ", " mu^2 ", cmd)
  cmd = gsub(" square root ", " sqrt ", cmd)

  links = talk_regress_links()
  links = paste(links, collapse = "|")
  links = paste0("(", links, ")")
  cmd = gsub(paste0(links, " link"), " link='\\1'", cmd)
  cmd = trim_multi_space(cmd)

  for (ilink in  talk_regress_links()) {
    cmd = gsub(paste0(" ", ilink, " "),
               paste0("'", ilink, "'"),
               cmd)
  }
  cmd = gsub(" link is ", " link=", cmd)

  # cmd = gsub(" log[-]linear", " poisson", cmd)

  cmd = gsub(" on ", " condition ", cmd)
  cmd = gsub(" by ", " condition ", cmd)
  cmd = gsub(" where ", " condition ", cmd)
  cmd = gsub(" which ", " condition ", cmd)
  cmd = gsub(" with ", " condition ", cmd)
  cmd = gsub(" so ", " condition ", cmd)
  cmd = gsub(" if ", " condition ", cmd)

  cmd = gsub("variable", "column", cmd)
  cmd = gsub("covariate", "column", cmd)

  # column 1 becomes column1
  cmd = gsub("columns", "column", cmd)
  cmd = gsub("column (\\d+)", "column\\1", cmd)
  cmd = gsub("column(\\D|$)", "", cmd)


  cmd = gsub("^regress ", "", cmd)

  return(cmd)
}

