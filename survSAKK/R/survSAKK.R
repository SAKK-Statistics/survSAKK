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
#' @param fit a survfit object created with `survival::survfit()`
#' 
#' @export
#' 
#' @example path.R
#' 
#' @import survival


# surv.plot <- function(){
#   
# }


# adjustable parameters for the function

fit <- survobject                                                               # fit:      An object of class `survfit`, usually returned by the `survfit` funciton. 
col <- NULL                                                                     # col:      Can accept a single value for color, or a vector of color values to set color(s)
# Layout options
main <- NULL                                                                    # main:     Title
sub <- NULL                                                                     # sub:      Subtitle
xlab <- "Time"                                                                  # xlab:     Label given to x-axis
ylab <- "Estiamted Survival Probability"                                        # ylab:     Label given to y-axis
cex.lab <- 1                                                                    # cex.lab   A numeric value specifying the size of the xlab and ylab.
cex.axis <- 0.75                                                                # cex.axis  A numeric values specifying the size of the axis size. 
lty <- "solid"                                                                  # lty:      A vector of string specifying line types for each curve (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”).
lwd <- 1                                                                        # lwd:      A vector of numeric values for line widths.
xlim <- seq(from = 0, to = ceiling(max(fit$time))+ceiling(min(fit$time)))       # xlim:     Set xlim based on the range, seq(starting value,  end value, number of increment of the sequence)
ylim <- seq(from = 0, to = 1, by = 0.25)                                        # ylim:     Set ylim based on the range, seq(starting value,  end value, number of increment of the sequence)
# Legend options
show.legend <- TRUE                                                             # show.legend        Display legend 
legend.position <- "topright"                                                   # legend.position    Position of the legend, c(x,y), "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
legend.legend <- NULL                                                           # legend.legend      A vector of string given to legend.
legend.text.font <- 1                                                           # legend.text.font   An integer specifying the font style of the legend text; (1: normal, 2: bold, 3: italic, 4: bold and italic)
legend.cex <- 0.75                                                                 # legend.cex         expansion factor for legend text.
legend.title <- NULL                                                            # legend.title       The title of the legend
legend.title.cex <- 1                                                           # legend.title.cex   expansion factor for legend title. 

#- Function:

# Falls Script zu lang wird sollten wir die Preparation auf einem seperaten R file abspeichern

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


# KM-Plot
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
  bty = "n",                               # Remove borders
  ylim = range(ylim),                      # Set y-axis limits 
  xlim = range(xlim),                      # Set x-axis limits
  xlab = xlab,                             # Draw x label
  ylab = ylab,                             # Draw y label
  cex.lab = cex.lab                        # Label size
)

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
legend(x = legend.position[1],
       y = legend.position[2],
       legend = legend.legend ,
       bty = "n",
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
