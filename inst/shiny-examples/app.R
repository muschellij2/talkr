library(shiny)
library(DT)
library(talkr)

df = tibble::rownames_to_column(mtcars, var = "car")

shinyApp(
  ui = fluidPage(
    singleton(tags$head(
      tags$script(src = "//cdnjs.cloudflare.com/ajax/libs/annyang/2.6.0/annyang.min.js"),
      includeScript('init.js')
    )),
    div(
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
    resulting_df = reactive({
      print(input$yes)
      cmd = c(input$arrange,
              input$filter,
              input$sort)
      cmd = unique(cmd)
      print(cmd)
      if (is.null(cmd)) {
        cmd = ""
      }
      if (cmd == "say command something") {
        cmd = ""
      }
      res = df %>%
        talk(cmd, error_find_function = FALSE)
      res
    })
    output$df = renderDataTable({
      resulting_df()
    })
  }
)
