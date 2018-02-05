#' A test function
#'
#' This function was created to allow you to test the OpenCPU service
#' @param df json array
#' @keywords ocpu
#' @export
#' @examples
#' simplePlot()
#' NOTE: for ggplot() to work, "import(ggplot2)" must be in the NAMESPACE file

simplePlot <- function(json = "~/Documents/SoftwareDevelopment/VikingCodeSchool/final_project/openMeta/src/dbstudiesJSON.json") {
  
  #'------------------------------------------------------------------------
  #' DATA FRAME transformations
  #' -----------------------------------------------------------------------
  
  df <- data.frame(jsonlite::fromJSON(json))
  
  #' Ensures that only studies selected for analysis are available
  df <- df[df$active == TRUE,]
  
  #'------------------------------------------------------------------------
  #' DISPLAY PROPERTIES
  #' -----------------------------------------------------------------------
  
  
  
  #'------------------------------------------------------------------------
  #' PACKAGE FUNCTIONS
  #' -----------------------------------------------------------------------
  #' If your module has any dependencies, it is recommended you require the
  #' functions from those dependencies that you need, below, like so:
  #' functionNeeded <- packageUsed::functionNeeded
  
  ggplot <- ggplot2::ggplot
  aes <- ggplot2::aes
  geom_point <- ggplot2::geom_point
  
  ggplotly <- plotly::ggplotly
  saveWidget <- htmlwidgets::saveWidget
  
  #'------------------------------------------------------------------------
  #' CUSTOM MODULE CODE
  #'------------------------------------------------------------------------
  #' Put your primary code below
  
  plot <- ggplot(data = df, aes(x = sampleSize, y = testStatVal)) + geom_point()
  p <- ggplotly(plot)
  saveWidget(p, "mymap.html", selfcontained = FALSE) 
  p
}

