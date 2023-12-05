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
fit <- survobject                                                               # Survfit object 
col <- NULL                                                                     # col:      can accept a single value for color, or a vector of color values to set color(s)
xlab <- "Time"                                                                  # xlab:     Label x-axis
ylab <- "Estiamted Survival Probability"                                        # ylab:     Label y-axis
cex.lab <- 1                                                                    # cex.lab   Font Size for xlab and ylab.
lty <- "solid"                                                                  # lty:      line-type 
lwd <- "1"                                                                      # lwd:      line-width
xlim <- seq(from = 0, to = ceiling(max(fit$time))+ceiling(min(fit$time)))       # xlim:     Set xlim based on the range, seq(starting value,  end value, number of increment of the sequence)
ylim <- seq(from = 0, to = 1, by = 0.25)                                        # ylim:     Set ylim based on the range, seq(starting value,  end value, number of increment of the sequence)




#- 

# Extract data from fit
data <- as.data.frame(eval(fit$call$data)) 

# Colouring KM-Plot
if (is.null(col)){
  if(is.null(fit$strata)){
    col <- "black"
    group <- ""
  } else {
    col <- as.factor(names(fit$strata))
    group <- names(fit$strata)
  }
} 


# KM-Plot
plot(
  ## Plot the survival curve
  fit,
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
axis(
  side = 1,                                # Specifies the side (1,2,3,4)
     las = 0,                              # Rotate the labels
     mgp = c(3,0.50,0),                    # Adjust the label position (axis title, axis label, axis line)
     at = xlim,                            # Specify tick mark position
     labels = xlim                         # Draw labels
)

# Customize the y coordinates
axis(side = 2,                             # Specifies the side (1,2,3,4)
     las = 1,                              # Rotate the labels 
     mgp = c(3,0.75,0),                    # Adjust the label position (axis title, axis label, axis line)
     at = ylim,                            # Specify tick mark position
     labels = ylim                         # Draw labels
)
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

surv.plot(S = fit)


