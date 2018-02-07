#' A funnel plot
#'
#' This function allows you to generate funnel plots with OpenMeta.
#' @param df json array
#' @keywords ocpu
#' @export
#' @examples
#' forest()
#' NOTE: for ggplot() to work, "import(ggplot2)" must be in the NAMESPACE file

forest <- function(json = "~/Documents/SoftwareDevelopment/VikingCodeSchool/final_project/openMeta/src/newStudiesJSON.json") {
  
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
  
  #' ggplot2
  ggplot <- ggplot2::ggplot
  aes <- ggplot2::aes
  geom_point <- ggplot2::geom_point
  xlab <- ggplot2::xlab
  ylab <- ggplot2::ylab
  geom_line <- ggplot2::geom_line
  geom_segment <- ggplot2::geom_segment
  scale_x_reverse <- ggplot2::scale_x_reverse
  scale_y_continuous <- ggplot2::scale_y_continuous
  coord_flip <- ggplot2::coord_flip
  theme_gray <- ggplot2::theme_gray
  geom_errorbarh <- ggplot2::geom_errorbarh
  geom_vline <- ggplot2::geom_vline
  facet_grid <- ggplot2::facet_grid
  
  #' metafor
  rma <- metafor::rma
  
  #' plotly
  ggplotly <- plotly::ggplotly
  saveWidget <- htmlwidgets::saveWidget
  
  #'------------------------------------------------------------------------
  #' CUSTOM MODULE CODE
  #'------------------------------------------------------------------------
  #' Put your primary code below
  
  #' Prepartory forest plot code
  
  #Create 'cite' vector by merging author and year columns
  
  df$cite <- NA
  years <- sapply(strsplit(df$pubDate, "-"), function(x) {x[[1]]})
  df$cite <- paste(df$name, years) 
  
  #Reorder bibliographic info based on value of d (yi), so effect sizes can be plotted in descending order
  df$cite <- reorder(df$cite, df$effectSizeVal, FUN=mean)
  
  #Look at your overall meta-analytic estimate (random-effects)
  random <- rma(effectSizeVal, stdErr^2, data=df)
  random
  
  #Turn off annoying option that prevents you from binding rows of text
  options(stringsAsFactors = FALSE)
  
  #You need to create a matrix for the new dfa row you wish to insert, give it the same column names as df
  #and then bind it to df. You will need to do this for each row you wish to insert.
  #In this case, 4 groups + 1 overall estimate = 5 entries...
  
  overall.row <- matrix(colnames(df), nrow=1)
  overall.row.df = as.data.frame (overall.row)
  names(overall.row.df) = names(df)
  df=rbind(df, overall.row.df)
  
  #Make sure everything that is numeric is numeric, and everything that is a factor is a factor
  df$effectSizeVal = as.numeric (df$effectSizeVal)
  df$stdErr= as.numeric (df$stdErr)
  df$cite= as.factor (df$cite)
  
  #Get standard errors from variances
  df$se = sqrt(df$stdErr)
  
  #Calculate 95% CI values
  df$lowerci = (-1.96*df$se)+df$effectSizeVal
  df$upperci = (1.96*df$se)+df$effectSizeVal
  
  #Make a plot called 'p', and map citation dfa to y-axis, effect sizes to x-axis
  #specify the min and max of the CIs, and give different shapes based on levels of tester
  p <- ggplot(df, aes(y=name, x=effectSizeVal, xmin=lowerci, xmax=upperci))+
    #Add dfa points and color them black
    geom_point(color = 'black')+
    #add the CI error bars
    geom_errorbarh(height=.1)+
    #Specify the limits of the x-axis and relabel it to something more meaningful
    scale_x_continuous(limits=c(-2,2), name='Standardized Mean Difference (d)')+
    #Give y-axis a meaningful label
    ylab('Reference')+
    #Add a vertical dashed line indicating an effect size of zero, for reference
    geom_vline(xintercept=0, color='black', linetype='dashed')+
    #Create sub-plots (i.e., facets) based on levels of setting
    #Apply my APA theme
    theme_gray()
  
    p <- ggplotly(p, hoverinfo="text", text = ~paste('Study: ', df$name))
    saveWidget(p, "mymap.html", selfcontained = FALSE) 
    p
}

