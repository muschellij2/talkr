talk_stop_words =  c("sort", "arrange", "order",
                      "group", "filter",
                      "qplot",
                      "mutate", "subset", "select",
                      "count", "transmute",
                      "summarize", "summarise",
                      "missing", "infinite", "finite",
                      "covariate", "variable",
                      "nan", "not a number")

usethis::use_data(talk_stop_words, compress = "xz",
                  overwrite = TRUE)
