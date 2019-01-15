
talk_geoms = function() {

  geoms = c("abline", "auto", "bar", "bin2d", "blank", "boxplot", "contour",
            "count", "crossbar", "density", "density_2d", "dotplot", "errorbar",
            "errorbarh", "freqpoly", "hex", "histogram", "hline",
            "jitter", "label",
            "linerange",
            "map", "path", "point", "polygon", "qq_line", "quantile", "raster",
            "ribbon", "rug", "segment", "smooth", "spoke", "violin", "vline")
  geoms = c(geoms, paste0("geom_", geoms))
  geoms = c(geoms, paste0("geom", geoms))
  # geoms = c(geoms, c("facets", "facet",
  #                    "facet_wrap", "facet_grid",
  #                    "facetwrap", "facetgrid"))
  geoms
}

talk_aes = function() {
  aes_accepted = c("shape", "alpha", "shape", "size", "fill",
                   "group", "stroke", "color", "colour", "x", "y",
                   "xintercept", "yintercept",
                   "intercept", "slope")
  aes_accepted = c(aes_accepted, paste0(aes_accepted, "="))
  aes_accepted = c(aes_accepted, c("facets", "facet",
                                   "facet_wrap", "facet_grid",
                                   "facetwrap", "facetgrid"))
  aes_accepted
}

talk_ggplot_words = function() {
  c(talk_geoms(), talk_aes())
}


#' Talk to ggplot a Data set
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
#' @import ggplot2
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' df = mtcars %>%
#'   rownames_to_column(var = "car")
#'
#'   cmds = c(
#'     "ggplot by  mpg",
#'     "ggplot by  column    mpg  ",
#'     "ggplot by column 5",
#'     "ggplot a histogram of mpg, coloured by gear",
#'     "ggplot by columns 4 and 5",
#'     # duplciate
#'     "ggplot by mpg descending coloured by American")
#'  data_colnames = df
#'  .data = df
#'  allowed_words = talkr:::talk_ggplot_words()
#'  cmd = c("ggplot a histogram of mpg, coloured by gear",
#'  "ggplot a histogram of mpg, facetted by gear")
#'  results = lapply(cmds, talk_ggplot_expr, data_colnames = df)
#'  results = lapply(cmd, talk_ggplot, .data = df)
#'  cmd =  "ggplot by columns 2 and 5, mpg decreasing"
#'  testthat::expect_warning(talk_ggplot(.data, cmd),
#'  "orderings")
#' df = df %>%
#'   rename(GEAR = gear)
#'  gear = df %>%
#'  talk_ggplot("arrange by gear and column 3")
#' testthat::expect_true(all(ggplot_vars(gear) == c("GEAR", "cyl")))
talk_ggplot = function(.data, cmd, verbose = FALSE, ...) {

  data_colnames = colnames(.data)
  out = talk_ggplot_expr(data_colnames, cmd, ...)
  small_list = out[[1]]
  out = lapply(out, function(small_list) {
    aes_run = do.call(ggplot2::aes_, args = small_list$variables)
    # hack
    L = lapply(small_list$plot_types, function(x) {
      eval(parse(text = x))
    })
    g = ggplot2::ggplot(data = .data, mapping = aes_run)
    g = Reduce("+", L, init = g)
    return(g)
  })

  if (length(cmd) == 1) {
    return(out[[1]])
  }
  out
}


#' @export
#' @rdname talk_ggplot
#' @param data_colnames column names of the data
talk_ggplot_expr  = function(data_colnames, cmd,
                             allowed_words = talk_ggplot_words(),
                             ...) {

  is_not = NULL
  rm(list = "is_not")
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
  cmd = remove_punct_keep_ops(cmd,
                              punct = setdiff(no_ops(),
                                              c("_", "~")))
  cmd = talk_process_ggplot_cmd(cmd)
  cmd = remove_df(cmd)

  var = variable = NULL
  ordering = var_num = df_var = NULL
  rm(list = c("var", "variable", "ordering", "var_num", "df_var"))

  ss = talk_get_colnames(data_colnames, cmd,
                         allowed_words = allowed_words,
                         ...)
  if (is.character(ss)) {
    ss = list(ss)
  }
  d = talk_check_colnames(data_colnames)
  data_colnames = d$data_colnames
  cn_df = d$colname_df
  cn = d$lower_colnames
  x = ss[[1]]

  res = lapply(ss, function(x) {
    d = tibble(
      var = x,
      aes = var %in% talk_aes(),
      plot_type = var %in% talk_geoms(),
      variable = !aes & !plot_type,
      var_num = cumsum(rev(variable))
    )
    variables = d[ d$variable & !d$aes, ]
    orders = d[ !d$variable & d$aes, ]

    orders = orders %>%
      rename(aesthetic = var) %>%
      mutate(aesthetic = gsub("=", "", aesthetic)) %>%
      select(aesthetic, var_num)
    if (nrow(orders) > 0) {
      variables = left_join(variables, orders, by = "var_num")
    } else {
      variables = variables %>%
        mutate(aesthetic = NA)
    }
    variables = variables %>%
      select(-variable) %>%
      mutate(
        aesthetic = ifelse(is.na(aesthetic) & var_num == 1, "x",
                           aesthetic),
        aesthetic = ifelse(is.na(aesthetic) & var_num == 2, "y",
                           aesthetic),
        aesthetic = ifelse(is.na(aesthetic), "",
                           aesthetic),
        var = ifelse(plot_type, paste0('"',
                                       var, '"'), var),
        aesthetic = ifelse(plot_type, "geom", aesthetic)
      )

    variables = variables %>%
      mutate(df_var = var)

    p_type = d[ d$plot_type, ]
    if (nrow(p_type) == 0) {
      if ("x" %in% variables$aesthetic &
          "y" %in% variables$aesthetic) {
        p_type = tibble::tibble(
          var = "point",
        )
      }
      if ("x" %in% variables$aesthetic &
          !"y" %in% variables$aesthetic) {
        p_type = tibble::tibble(
          var = "histogram"
        )
      }
    }
    p_type = p_type %>%
      mutate(
        var = trimws(var),
        df_var = gsub("^geom([^_])", "geom_\\1", var),
      )
    p_type = p_type %>%
      mutate(
        var = var,
        aes = FALSE,
        plot_type = TRUE,
        aesthetic = "geom",
        var_num = NA,
        var_out = paste0(df_var, "()")
      ) %>%
      select(-var_num)


    variables = variables %>%
      ungroup() %>%
      arrange(var_num) %>%
      mutate(
        aesthetic = sub("=~$", "~", trimws(aesthetic)),
        aesthetic = gsub("=$", "", trimws(aesthetic)),
        aesthetic = gsub("=", "", trimws(aesthetic)),
        aesthetic = recode(aesthetic,
                           "facets" = "facet_wrap",
                           "facetwrap" = "facet_wrap",
                           "facetgrid" = "facet_grid",
                           "facet" = "facet_wrap"),
        # var_out = paste0(aesthetic, "= ~", df_var),
        var_out = paste0("~ ", df_var)
        # var_out = gsub("facets=", "facet_wrap(~", var_out),
        # var_out = gsub("==", "=", var_out)
      )
    variables = full_join(
      variables, p_type,
      by = c("var", "aes", "plot_type", "aesthetic", "df_var", "var_out"))

    variables = variables %>%
      mutate(
        var_out = ifelse(grepl("facet", aesthetic),
                         paste0(aesthetic, "(", var_out, ")"),
                         var_out),
        plot_type = plot_type  | grepl("facet", aesthetic)
      )

    plot_types = variables %>%
      filter(plot_type)
    variables = variables %>%
      filter(!plot_type)
    L = list(variables = variables,
             plot_types = plot_types)
    return(L)
  })

  out = lapply(res, function(x) {
    variables = friendlyeval::treat_strings_as_exprs(x$variables$var_out)
    names(variables) = x$variables$aesthetic
    plot_types = x$plot_types$var_out
    L = list(variables = variables,
             plot_types = plot_types)
    L
  })
  out
}


#' @export
#' @rdname talk_ggplot
talk_process_ggplot_cmd = function(cmd) {
  # make ascending
  cmd = talk_process_arrange_cmd(cmd)

  cmd = gsub("x variable(s|)", "x=", cmd)
  cmd = gsub("x is ", "x=", cmd)
  cmd = gsub(" ex variable(s|)", "x=", cmd)
  cmd = gsub("y variable(s|)", "y=", cmd)
  cmd = gsub("y is ", "y=", cmd)
  cmd = gsub(" x intercept", " xintercept", cmd)
  cmd = gsub(" y intercept", " yintercept", cmd)


  cmd = gsub("assigned ", "assign ", cmd)
  cmd = gsub("assigns ", "assign ", cmd)
  cmd = gsub("assign ", "equal ", cmd)
  cmd = gsub("equal to ", "equal ", cmd)

  # equal to never goign to happen due to gsub above
  cmd = gsub("is equal to", "=", cmd)
  cmd = gsub("is equal ", "= ", cmd)

  cmd = gsub("equals ", "equal ", cmd)
  cmd = gsub("equal", "==", cmd)


  cmd = gsub("ascending", "", cmd)
  cmd = gsub("descending", "", cmd)
  cmd = gsub("colo(|u)red", "color", cmd)
  cmd = gsub("grouped", "group=", cmd)
  cmd = gsub("grouping", "group=", cmd)
  cmd = gsub("colour", "color", cmd)
  cmd = gsub("filled", "fill=", cmd)
  cmd = gsub("facets ", "facet ", cmd)
  cmd = gsub("facetted ", "facet ", cmd)
  cmd = gsub("facet wrap", "facet_wrap", cmd)
  cmd = gsub("facet grid", "facet_grid", cmd)
  cmd = gsub("facet ", "facet_wrap ", cmd)

  cmd = trim_multi_space(cmd)

  aes_accepted = c("shape", "alpha", "shape", "size", "fill",
                   "group", "stroke", "color", "colour")
  for (iaes in aes_accepted) {
    rep_string = paste0("(", iaes, ")")
    cmd = gsub(rep_string, "\\1=", cmd)
  }

  cmd = gsub("==", "=", cmd)
  cmd = gsub("=\\s+=", "=", cmd)

  cmd = gsub(" a b line", "abline", cmd)
  cmd = gsub(" h line", "hline", cmd)
  cmd = gsub(" v line", "vline", cmd)

  cmd = gsub("line plot", "geomline", cmd)
  cmd = gsub("scatter plot", "geompoint", cmd)
  cmd = gsub("scatterplot", "geompoint", cmd)
  cmd = gsub(" line ", " geomline ", cmd)
  cmd = gsub(" point(s|) ", " geompoint ", cmd)
  cmd = gsub(" histogram", " geomhistogram", cmd)
  cmd = gsub(" boxplot", " geomboxplot", cmd)

  cmd = gsub("error bar", " geomerrorbar", cmd)
  cmd = gsub("errorbar h ", "errorbarh ", cmd)
  cmd = gsub(" errorbar", " geomerrorbar", cmd)
  cmd = gsub(" (line|point) range", " geom\\1range", cmd)

  return(cmd)
}




