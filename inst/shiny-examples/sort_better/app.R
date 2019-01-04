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

shinyApp(
  ui = fluidPage(
    singleton(tags$head(
      tags$script(src = "//cdnjs.cloudflare.com/ajax/libs/annyang/2.6.0/annyang.min.js"),
      includeScript("init.js")
    )),
    div(
      h1("Say the word 'run' then a command that starts with 'sort' or 'arrange'"),
      h3(
        textOutput('cmd')
      ),
      dataTableOutput('df'),
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
      cmd
    })

    resulting_df = reactive({
      cmd = get_cmd()
      df <<- df %>%
        talk(cmd, error_find_function = FALSE)
      df
    })

    output$df = renderDataTable({
      print(input$command)
      resulting_df()
    })
    output$cmd = renderText({
      paste0("Your command is: ", get_cmd())
    })
  }
)
