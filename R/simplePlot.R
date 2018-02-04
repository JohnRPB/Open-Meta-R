#' A test function
#'
#' This function was created to allow you to test the OpenCPU service
#' @param x x-variable
#' @param y y-variable
#' @keywords ocpu
#' @export
#' @examples
#' simplePlot()

simplePlot <- function(json = "~/Documents/SoftwareDevelopment/VikingCodeSchool/final_project/openMeta/src/dbstudiesJSON.json") {
  df <- data.frame(jsonlite::fromJSON(json))
  
  ## ggplot2::qplot(df$x, df$y, geom = "line")
  ggplot2::ggplot(data = df, ggplot2::aes(x = sampleSize, y = testStatVal)) + ggplot2::geom_point()
}

