library(shiny)
library(DT)
library(talkr)
library(dplyr)

run_df = tibble::rownames_to_column(mtcars, var = "car")
run_df = run_df %>%
  rename(cylinders = cyl,
         horsepower = hp,
         transmission = am,
         Displacement = disp,
         axle_ratio = drat,
         qmile = qsec,
         Engine = vs,
         weight = wt,
         carburetor = carb)
xdf = run_df
L = list(xdf)
names(L)[1] = "start"
cmd_history = ""
cmds = c("sort", "arrange", "order", "filter", "filter out",
         "drop rows", "subset rows", "select columns",
         "reset", "undo", "group_by", "table", "count")
cmds = paste0("'", cmds, "'")
cmds = paste(cmds, collapse = ", ")

shinyApp(
  ui = fluidPage(
    singleton(tags$head(
      tags$script(src = "//cdnjs.cloudflare.com/ajax/libs/annyang/2.6.0/annyang.min.js"),
      includeScript("init.js")
    )),
    div(
      h1(paste0(
        "Say a command that starts with ", cmds)
      ),
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
      h3(
        plotOutput('plot')
      ),
      h3(
        "Command history:",
        tableOutput('cmd_history')
      ),
      h3(
        "Classes of your columns:",
        tableOutput('class_table')
      ),
      helpText(
        'You are recommended to use Google Chrome to play with this app.',
        'To change the title, say something that starts with "sort" or arrange, e.g.',
        '"arrange by hp", or "sort mpg descending".'
      ),
      h3(
        textOutput('phrase')
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
      cmd = unique(cmd)
      print(cmd)
      if (is.null(cmd)) {
        cmd = ""
      }
      cmd = trimws(cmd)
      if (cmd == "say run something") {
        cmd = ""
      }
      cmd = gsub(" for ", " four ", cmd)
      cmd = gsub(" for$", " four", cmd)
      cmd = gsub(" underscore ", "_", cmd)
      cmd = gsub("fassett", "facet", cmd)
      print(cmd)
      # if (cmd != "") {
      #   cmd_history <<- c(cmd, cmd_history)
      # }
      # if (grepl("^undo", cmd)) {
      #   cmd = "undo"
      #   cmd_history <<- cmd_history[-1]
      #   if (length(cmd_history) == 0) {
      #     cmd_history <<- ""
      #   }
      # }
      # if (grepl("^reset", cmd)) {
      #   cmd_history <<- ""
      # }
      cmd
    })


    get_list = reactive({
      cmd = get_cmd()
      if (!cmd %in% "") {
        cmd = sub("^group by", "group_by", cmd)
        if (grepl("^reset", cmd)) {
          run_df <<- xdf
          L <<- list(start = run_df)
        } else if (grepl("^undo", cmd)) {
          L <<- L[-1]
          if (length(L) == 0) {
            L <<- list(start = xdf)
          }
          print(paste("length L is ", length(L)))
          # print(head(L[[1]]))
        } else if (grepl("^(gg|)plot", cmd)) {

        } else {
          run_df <<- L[[1]]
          print(cmd)
          res <- run_df %>%
            talk_expr(cmd, error_find_function = FALSE)
          res$expression = paste(res$expression, collapse = ", ")
          run_expr = paste0(res$func, "(", res$expression, ")")
          res = try( {run_df <<- run_df %>%
            talk(cmd, error_find_function = FALSE)
          })
          if (!inherits(res, "try-error")) {
            L = c(list(run_df), L)
            names(L)[1] = run_expr
            L <<- L
          }
          print(paste("length L is ", length(L)))
          print(names(L))
        }
      }
      L
    })
    resulting_df = reactive({
      L = get_list()
      print("Class of L in resulting_df")
      print(class(L[[1]]))
      L[[1]]
    })

    output$cmd_history = renderTable({
      cmd = input$command
      print("in cmd history")
      print(length(L))
      print(names(L))
      L = get_list()
      tab = data.frame(
        command = rev(names(L)), stringsAsFactors = FALSE)
      print(tab)
      tab
    })

    output$class_table = renderTable({
      cmd = input$command

      cn_df = talk_colnames_class(L[[1]])
      cn_df
    })





    output$df = renderDataTable({
      print(input$command)
      resulting_df()
    })

    output$plot = renderPlot({
      cmd = get_cmd()
      res = NULL
      if (grepl("^(gg|)plot", cmd)) {
        res = try( {
          run_df %>%
            talk(cmd, error_find_function = FALSE)
        })
        print("res is")
        print(class(res))
        if (inherits(res, "try-error")) {
          res = NULL
        }
      }
      print(res)
    })
    output$group_vars = renderText({
      run_df = resulting_df()
      gv = dplyr::group_vars(run_df)
      # print(gv)
      # print(head(df))
      if (length(gv) == 0) {
        gv = ""
      }
      gv = paste(gv, collapse = ", ")
      paste0("Group vars:", gv)
    })
    output$cmd = renderText({
      cmd = get_cmd()
      print(cmd)
      if (grepl("^undo", cmd)) {
        cmd = "undo"
      }
      paste0("Your command is: ", cmd)
    })

    output$phrase = renderText({
      print(input$phrase)
      print(length(input$phrase))
      paste(input$phrase, collapse = "\n")
    })

    output$cmd_clean = renderText({
      cmd = get_cmd()
      if (grepl("^reset", cmd)) {
        res = "reset"
      } else if (grepl("^undo", cmd)) {
        res = "undo"
      } else {
        if (length(L) > 1) {
          ddf = L[[2]]
        } else {
          ddf = L[[1]]
        }
        res <- ddf %>%
          talk_expr(cmd, error_find_function = FALSE)
        res$expression = paste(res$expression, collapse = ", ")
        res = paste0(res$func, "(", res$expression, ")")
      }
      paste0("Your clean command is: ", res[1])
    })
  }
)
