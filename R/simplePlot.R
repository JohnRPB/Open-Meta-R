#' A test function
#'
#' This function was created to allow you to test the OpenCPU service
#' @param x x-variable
#' @param y y-variable
#' @keywords ocpu
#' @export
#' @examples
#' simplePlot()

simplePlot <- function(x, y) {
  df <- data.frame(x = x, y = y)
  ## ggplot2::qplot(df$x, df$y, geom = "line")
  ggplot2::ggplot(data = df, ggplot2::aes(x = x, y =y)) + ggplot2::geom_point()
}

