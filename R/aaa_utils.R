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

  # remove puncutation
  string = paste0("[", ifelse(drop_punct, "[:punct:]", ""),
                  "[:blank:]]+")
  cmd = gsub(string, " ", cmd)
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

remove_punct_keep_ops = function(cmd) {
  for (i in no_ops()) {
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


