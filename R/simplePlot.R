#' A test function
#'
#' This function was created to allow you to test the OpenCPU service
#' @param df json array
#' @keywords ocpu
#' @export
#' @examples
#' simplePlot()

simplePlot <- function(json) {
  df <- data.frame(jsonlite::fromJSON(json))
  df <- df[df$active == TRUE,]
  
  ## ggplot2::qplot(df$x, df$y, geom = "line")
  ggplot2::ggplot(data = df, ggplot2::aes(x = sampleSize, y = testStatVal)) + ggplot2::geom_point()
}

