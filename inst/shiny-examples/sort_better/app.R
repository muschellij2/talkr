library(shiny)
library(DT)
library(talkr)
library(dplyr)

df = tibble::rownames_to_column(mtcars, var = "car")
df = df %>%
  rename(cylinders = cyl,
         horsepower = hp,
         American = am,
         carburetor = carb)
xdf = df
L = list(xdf)

shinyApp(
  ui = fluidPage(
    singleton(tags$head(
      tags$script(src = "//cdnjs.cloudflare.com/ajax/libs/annyang/2.6.0/annyang.min.js"),
      includeScript("init.js")
    )),
    div(
      h1("Say a command that starts with 'sort' or 'arrange' or 'order'"),
      h3(
        textOutput('cmd')
      ),
      dataTableOutput('df'),
      h3(
        textOutput('cmd_clean')
      ),
      h3(
        textOutput('group_vars')
      ),
      helpText(
        'You are recommended to use Google Chrome to play with this app.',
        'To change the title, say something that starts with "sort" or arrange, e.g.',
        '"arrange by hp", or "sort mpg descending".'
      ),
      helpText(HTML(
        'The source code of this app is <a href="https://github.com/muschellij2/talkr">on Github</a>.',
        'You may also see <a href="http://vimeo.com/yihui/shiny-voice">my demo</a> of playing with this app.'
      ))
    )
  ),
  server = function(input, output) {
    get_cmd = reactive({
      cmd = input$command
      print(input$title)
      cmd = unique(cmd)
      print(cmd)
      if (is.null(cmd)) {
        cmd = ""
      }
      if (cmd == "say run something") {
        cmd = ""
      }
      print(cmd)
      cmd
    })

    resulting_df = reactive({
      cmd = get_cmd()
      if (!cmd %in% "") {
        cmd = sub("^group by", "group_by", cmd)
        if (grepl("^reset", cmd)) {
          df <<- xdf
          L <<- list(df)
        } else if (grepl("undo", cmd)) {
          L <<- L[-1]
          if (length(L) == 0) {
            L <<- list(df)
          }
        } else {
          df <<- df %>%
            talk(cmd, error_find_function = FALSE)
          L <<- c(list(df), L)
        }
      }
      print(class(L[[1]]))
      L[[1]]
    })

    output$df = renderDataTable({
      print(input$command)
      resulting_df()
    })
    output$group_vars = renderText({
      df = resulting_df()
      gv = dplyr::group_vars(df)
      # print(gv)
      # print(head(df))
      if (length(gv) == 0) {
        gv = ""
      }
      gv = paste(gv, collapse = ", ")
      paste0("Group vars:", gv)
    })
    output$cmd = renderText({
      print(get_cmd())
      paste0("Your command is: ", get_cmd())
    })
    output$cmd_clean = renderText({
      cmd = get_cmd()
      if (grepl("^reset", cmd)) {
        res = "reset"
      } else {
        res <- df %>%
          talk_expr(cmd, error_find_function = FALSE)
        res = paste0(res$func, "(", res$expression, ")")
      }
      paste0("Your clean command is: ", res)
    })
  }
)
