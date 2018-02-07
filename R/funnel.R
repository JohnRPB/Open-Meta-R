#' A funnel plot
#'
#' This function allows you to generate funnel plots with OpenMeta.
#' @param df json array
#' @keywords ocpu
#' @export
#' @examples
#' funnel()
#' NOTE: for ggplot() to work, "import(ggplot2)" must be in the NAMESPACE file

funnel <- function(json = "~/Documents/SoftwareDevelopment/VikingCodeSchool/final_project/openMeta/src/newStudiesJSON.json") {
  
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
  theme_bw <- ggplot2::theme_bw
  theme_gray <- ggplot2::theme_gray
  
  #' metafor
  rma <- metafor::rma
  
  #' plotly
  ggplotly <- plotly::ggplotly
  saveWidget <- htmlwidgets::saveWidget
  
  #'------------------------------------------------------------------------
  #' CUSTOM MODULE CODE
  #'------------------------------------------------------------------------
  #' Put your primary code below
  
  #Store the meta-analytic estimate and its standard error from whatever model you run (substitute your own values)
  mod = rma(yi = effectSizeVal, stdErr, data=df, method="ML")
  estimate = mod$b
  se = mod$se
  
  #Store a vector of values that spans the range from 0
  #to the max value of impression (standard error) in your dataset.
  #Make the increment (the final value) small enough (I choose 0.001)
  #to ensure your whole range of data is captured
  se.seq=seq(0, max(df$stdErr), 0.001)
  
  #Now, compute vectors of the lower-limit and upper limit values for
  #the 95% CI region, using the range of SE that you generated in the previous step, and the stored value of your meta-analytic estimate.
  ll95 = estimate-(1.96*se.seq)
  ul95 = estimate+(1.96*se.seq)
  
  #You can do this for a 99% CI region too
  ll99 = estimate-(3.29*se.seq)
  ul99 = estimate+(3.29*se.seq)
  
  #And finally, do the same thing except now calculating the confidence interval
  #for your meta-analytic estimate based on the stored value of its standard error
  meanll95 = estimate-(1.96*se)
  meanul95 = estimate+(1.96*se)
  
  #Now, smash all of those calculated values into one data frame (called 'dfCI').
  #You might get a warning about '...row names were found from a short variable...'
  #You can ignore it.
  dfCI = data.frame(ll95, ul95, ll99, ul99, se.seq, estimate, meanll95, meanul95)
  
  #Now we can actually make the funnel plot.
  #Using your original data-frame, map standard error to your x-axis (for now) and Zr to your y-axis
  fp <- ggplot(aes(x = stdErr, y = effectSizeVal), data = df) +
    #Add your data-points to the scatterplot
    geom_point() +
    #Give the x- and y- axes informative labels
    xlab('Standard Error') + ylab('Effect size')+
    #Now using the 'dfCI' data-frame we created, plot dotted lines corresponding
    #to the lower and upper limits of your 95% CI region,
    #And dashed lines corresponding to your 99% CI region
    geom_line(aes(x = se.seq, y = ll95), linetype = 'dotted', data = dfCI) +
    geom_line(aes(x = se.seq, y = ul95), linetype = 'dotted', data = dfCI) +
    geom_line(aes(x = se.seq, y = ll99), linetype = 'dashed', data = dfCI) +
    geom_line(aes(x = se.seq, y = ul99), linetype = 'dashed', data = dfCI) +
    #Now plot dotted lines corresponding to the 95% CI of your meta-analytic estimate
    geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI) +
    geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), linetype='dotted', data=dfCI) +
    #Reverse the x-axis ordering (se) so that the tip of the funnel will appear
    #at the top of the figure once we swap the x- and y-axes...
    scale_x_reverse()+
    #Specify the range and interval for the tick-marks of the y-axis (Zr);
    #Choose values that work for you based on your data
    scale_y_continuous(breaks=seq(-1.25,2,0.25))+
    #And now we flip the axes so that SE is on y- and Zr is on x-
    coord_flip()+
    #Finally, apply my APA-format theme (see code at end of post).
    #You could, alternatively, specify theme_bw() instead.
  theme_gray()
  
  p <- ggplotly(fp, dynamicTicks=TRUE, hoverinfo="text", text = ~paste('Study: ', df$name))
  saveWidget(p, "mymap.html", selfcontained = FALSE) 
  p
}

