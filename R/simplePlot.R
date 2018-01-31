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
  ggplot(data = df, aes(x = x, y =y)) + geom_line()
}

