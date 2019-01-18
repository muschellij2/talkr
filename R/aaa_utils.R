fix_contractions = function(cmd) {
  cmd = gsub("doesn't", "do not", cmd)
  cmd = gsub("don't", "do not", cmd)
  # be verbs
  cmd = gsub(" isn't", "are not", cmd)
  cmd = gsub(" aren't", "are not", cmd)
  cmd = gsub(" weren't", "are not", cmd)
  cmd = gsub(" cannot", "are not", cmd)
  cmd
}

trim_multi_space = function(x) {
  x = trimws(x)
  x = gsub("\\s+", " ", x)
}

process_cmd = function(
  cmd,
  lower.case = TRUE,
  drop_punct = TRUE) {
  cmd = trimws(cmd)
  # cmd = paste(cmd, collapse = " ")
  if (lower.case) {
    cmd = tolower(cmd)
  }
  # take out
  cmd = trim_multi_space(cmd)

  cmd = gsub("^group by ", "group_by ", cmd)
  cmd = gsub("^facet (wrap|grid) ", "facet_\\1 ", cmd)
  cmd = gsub("^add (count|tally)", "add_\\1", cmd)

  cmd = gsub("density_2d", "density2d", cmd)
  cmd = gsub("qq_line", "qqline", cmd)

  cmd = gsub("group_by", "groupby", cmd)
  cmd = gsub("add_count", "addcount", cmd)
  cmd = gsub( "add_tally", "addtally", cmd)
  cmd = gsub("facet_(wrap|grid)", "facet\\1", cmd)
  # remove punctutation
  string = paste0("[", ifelse(drop_punct, "[:punct:]", ""),
                  "[:blank:]]+")
  cmd = gsub(string, " ", cmd)
  cmd = gsub("groupby", "group_by", cmd)
  cmd = gsub( "addcount", "add_count", cmd)
  cmd = gsub( "addtally", "add_tally", cmd)
  cmd = gsub("density2d", "density_2d", cmd)
  cmd = gsub("qqline", "qq_line", cmd)
  cmd = gsub("facet(wrap|grid)", "facet_\\1", cmd)

  cmd = trim_multi_space(cmd)
  return(cmd)
}

punct = function() {
  c("!", '"', "#", "$", "%", "&", "'", "(", ")", "*",
    "+", ",", "-", ".", "/", ":", ";", "<", "=", ">",
    "?", "@", "[", "\\", "]", "^", "_", "`", "{", "|", "}", "~")
}
no_ops = function() {
  setdiff(punct(), c("!", "=", ">", "<", "|", "&"))
}

remove_punct_keep_ops = function(cmd, punct = no_ops()) {
  for (i in punct) {
    cmd = gsub(i, " ", cmd, fixed = TRUE)
  }
  cmd = trim_multi_space(cmd)
}


remove_df = function(cmd) {
  cmd = gsub("data frame", "", cmd)
  cmd = gsub("data[.]frame", "", cmd)
  cmd = gsub("data set", "", cmd)
  cmd = gsub("the data", "", cmd)
  cmd = gsub("the sheet", "", cmd)
  cmd = trim_multi_space(cmd)
  cmd
}



is_condition_not = function(cmd, clean_cmd = "filter") {
  all_terms = c("filter out",
                "subset out",
                "select out",
                "remove row",
                "remove column",
                "drop (|out |off )row",
                "drop (|out |off )column",
                "remove",
                "drop",
                "get rid of",
                "not select",
                "not filter",
                "not keep",
                "do not keep")
  terms = paste(all_terms, collapse = "|")
  terms = paste0("^", terms)
  not_condition = grepl(terms, cmd)
  for (iterm in all_terms) {
    cmd = gsub(iterm, paste0(clean_cmd, " "), cmd)
  }
  cmd = trim_multi_space(cmd)
  L = list(
    not_condition = not_condition,
    clean_cmd = clean_cmd,
    cmd = cmd)
  return(L)
}

