#' Talk Shiny Examples
#'
#' @param example Name of example to run.
#'
#' @return The output from \code{\link{runApp}}
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples
#' if (interactive()) {
#' talk_shiny_example()
#' }
talk_shiny_example = function(example = c("sort_better", "simple_sort")) {
  example = example[1]
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "talkr"))
  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (!nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "talkr")
  shiny::runApp(appDir, display.mode = "normal")
}
