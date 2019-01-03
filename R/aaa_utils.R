trim_multi_space = function(x) {
  x = trimws(x)
  x = gsub("\\s+", " ", x)
}

process_cmd = function(cmd, lower.case = TRUE) {
  cmd = trimws(cmd)
  # cmd = paste(cmd, collapse = " ")
  if (lower.case) {
    cmd = tolower(cmd)
  }
  # take out
  cmd = trim_multi_space(cmd)

  # remove puncutation
  cmd = gsub("[[:punct:][:blank:]]+", " ", cmd)
  cmd = trim_multi_space(cmd)
  return(cmd)
}


remove_df = function(cmd) {
  cmd = gsub("data frame", "", cmd)
  cmd = gsub("data[.]frame", "", cmd)
  cmd = gsub("data set", "", cmd)
  cmd = gsub("the data", "", cmd)
  cmd = gsub("the sheet", "", cmd)
  cmd = trim_multi_space(cmd)
}
