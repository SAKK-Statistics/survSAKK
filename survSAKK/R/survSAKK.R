# -----------------------------------------------------------------------------#
# METADATA
# TITLE: Publication ready Kaplan-Meier plot
# 
# WRITTEN BY Vithersan Somasundaram and Katrin Gysel
# -----------------------------------------------------------------------------#

#' Publication Ready Kaplan-Meier Plot
#' 
#' Plot publication ready Kaplan-Meier plot using the results from `survival::survfit()`. 
#' 
#' @param fit An object of class `survfit`, usually returned by the `survfit` funciton. 
#' @param gird  A logical value for drawing Grid. (TRUE or FALSE)
#' @param col Can accept a single value for color, or a vector of color values to set color(s)
#' @param main Title
#' @param sub Subtitle
#' @param xlab Label given to x-axis
#' @param ylab Label given to y-axis
#' @param cex.lab A numeric value specifying the size of the xlab and ylab
#' @param cex.axis A numeric values specifying the size of the axis size.
#' @param bty The type of box to be drawn around the plot ("n","o","7","L","C","U")
#' @param lty A vector of string specifying line types for each curve (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”)
#' @param lwd A vector of numeric values for line widths
#' @param xlim Set xlim based on the range, seq(starting value,  end value, number of increment of the sequence)
#' @param ylim Set ylim based on the range, seq(starting value,  end value, number of increment of the sequence) 
#' @param show.legend Display legend
#' @param legend.position Position of the legend, c(x,y), "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#' @param legend.legend A vector of string given to legend.
#' @param legend.text.font An integer specifying the font style of the legend text; (1: normal, 2: bold, 3: italic, 4: bold and italic)
#' @param legend.cex expansion factor for legend text.
#' @param legend.title The title of the legend
#' @param legend.title.cex Expansion factor for legend title.
#'  
#' @export
#' 
#' @example path.R
#' 
#' @import survival


surv.plot <- function(
    fit, grid = FALSE, col = NULL,
    ## Layout options
    main = NULL, sub = NULL,
    xlab = "Time", ylab = "Estimated Survival Probability",
    cex.lab = 1, cex.axis = 0.75,
    bty = "l", lty = "solid", lwd = 1,
    xlim = seq(from = 0, to = ceiling(max(fit$time))+ceiling(min(fit$time))),
    ylim = seq(from = 0, to = 1, by = 0.25), 
    ## Legend options
    show.legend = TRUE, legend.position = "topright", legend.legend = NULL,
    legend.text.font = 1, legend.cex = 0.75,
    legend.title = NULL,legend.title.cex = 1,
    ...
){
  
# Preparation
  
  # Extract data from fit
  data <- as.data.frame(eval(fit$call$data)) 
  
  # Define color for KM-Plot if not manually specified
  if (is.null(col)){
    if(is.null(fit$strata)){
      col <- "black"
    } else {
      col <- as.factor(names(fit$strata))
    }
  } 
  
  # Extract Group names for legend if not manually specifed
  if(is.null(fit$strata)){
    group <- "Cohort"
    legend.legend <- group                                                          
  } else {
    group <- levels(as.factor(names(fit$strata)))
    legend.legend <- group
  }
  
  
# Visualize KM-Plot
  base::plot(
    ## Plot the survival curve
    fit,
    main = main,
    sub = sub,
    col = col,                               
    lty = lty,
    lwd = lwd,
    ## Add censoring information with ticks
    ## Modify Layout
    xaxs = "i", yaxs = "i",                  # Start axis exactly from zero origin
    xaxt = "n", yaxt = "n",                  # Remove the original axes
    bty = bty,                               # Remove borders
    ylim = range(ylim),                      # Set y-axis limits 
    xlim = range(xlim),                      # Set x-axis limits
    xlab = xlab,                             # Draw x label
    ylab = ylab,                             # Draw y label
    cex.lab = cex.lab                        # Label size
  )
  
  # Draw grid
  if (is.logical(grid)) {
    if (grid == TRUE) {
      grid(nx = length(xlim)-1, ny = length(ylim)-1)
    } 
  } else {
    stop("`gird` expecting TRUE or FALSE as an argument!")
  }
  
  # Customize the x coordinates
  graphics::axis(
    side = 1,                                # Specifies the side (1,2,3,4)
    las = 0,                                 # Rotate the labels
    mgp = c(3,0.50,0),                       # Adjust the label position (axis title, axis label, axis line)
    at = xlim,                               # Specify tick mark position
    labels = xlim,                           # Draw labels
    cex.axis = cex.axis                      # Axis size    
  )
  
  # Customize the y coordinates
  graphics::axis(side = 2,                   # Specifies the side (1,2,3,4)
                 las = 1,                              # Rotate the labels 
                 mgp = c(3,0.75,0),                    # Adjust the label position (axis title, axis label, axis line)
                 at = ylim,                            # Specify tick mark position
                 labels = ylim,                        # Draw labels
                 cex.axis = cex.axis                   # Axis size  
  )
  
  # Add legend to plot
  if (show.legend == TRUE){
    legend(x = legend.position[1],             # the x coordinates to positon the legend
           y = legend.position[2],             # the y coordinates to positoin the legend
           legend = legend.legend ,            # the text of the legend
           bty = "n",                          # boarder type for legend fixed as "none"
           col = col,                           
           lty = lty,
           text.font = legend.text.font,
           title = legend.title,
           cex = legend.cex,
           title.cex = legend.title.cex
    )
  }
  
  # Draw risk table
  # text(x = 0:xlim[2],                      # Starting point of the x values
  #      y = par("usr")[3] - 1,              # Starting point of the y values
  #      labels = fit$n,                     # Use the values from fit.
  #      xpd = NA,                           # Change the clipping region.
  #      cex = 1.0                           # Increase text size
  #      )
  
}


# - Testing

# Testing the function
library(survival)
lung <- survival::lung  
lung$time_yr <- lung$time/365.25
lung$time_mt <- lung$time_yr*12

lung$sex <- factor(lung$sex,
                   levels = c(1,2),
                   labels = c("Male","Female"))

survobject <- survival::survfit(Surv(time_mt, status) ~ sex, data = lung)

survobject <- survival::survfit(Surv(time_yr, status) ~ sex, data = lung)

survobject <- survival::survfit(Surv(time_mt, status) ~ 1, data = lung)

survobject <- survival::survfit(Surv(time_yr, status) ~ 1, data = lung)

surv.plot(fit = survobject)
