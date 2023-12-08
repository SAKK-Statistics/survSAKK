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
#'
#' @export
#'
#' @example path.R
#'
#' @import survival


# surv.plot <- function(
#
# ){
#
# }

# adjustable parameters for the function

fit <- survobject                                                               # fit:      An object of class `survfit`, usually returned by the `survfit` funciton.
conf.int <- 0.95                                                                # conf.int  specifies the coverage probability. (FALSE, TRUE using 95% confidence intervals.
                                                                                # Alternatively, this can be a numeric value giving the desired confidence level.
conf.band <- FALSE                                                              # conf.band: Mapping the specified coverage probability
conf.band.col <- NULL                                                           # conf.band.col: Can accept a single value for color, or a vector of color values to set color(s)
conf.band.alpha <- 0.25                                                         # conf.band.alpha: Modiy color transparency for the confidence band.
conf.type = "log-log"                                                           # conf.type Specifies the transformation. Default: "log-log". Options ("log", "log-log", "plain", "logit", "arcsin")
grid <- FALSE                                                                   # gird:     A logical value for drawing Grid. (TRUE or FALSE)
col <- NULL                                                                     # col:      Can accept a single value for color, or a vector of color values to set color(s)
# Layout options
main <- NULL                                                                    # main:     Title
sub <- NULL                                                                     # sub:      Subtitle
xlab <- "Time"                                                                  # xlab:     Label given to x-axis
ylab <- "Estiamted Survival Probability"                                        # ylab:     Label given to y-axis
cex.lab <- 1                                                                    # cex.lab   A numeric value specifying the size of the xlab and ylab.
cex.axis <- 0.75                                                                # cex.axis  A numeric values specifying the size of the axis size.
bty <- "l"                                                                      # bty       The type of box to be drawn around the plot ("n","o","7","L","C","U")
lty <- c("solid","dotted","dotted")                                             # lty:      A vector of string specifying line types for each curve (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”).
lwd <- 1                                                                        # lwd:      A vector of numeric values for line widths.
xlim <- seq(from = 0, to = ceiling(max(fit$time))+ceiling(min(fit$time)))       # xlim:     Set xlim based on the range, seq(starting value,  end value, number of increment of the sequence)
ylim <- seq(from = 0, to = 1, by = 0.25)                                        # ylim:     Set ylim based on the range, seq(starting value,  end value, number of increment of the sequence)
# Legend options
show.legend <- TRUE                                                             # show.legend        Display legend
legend.position <- "topright"                                                   # legend.position    Position of the legend, c(x,y), "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
legend.legend <- NULL                                                           # legend.legend      A vector of string given to legend.
legend.text.font <- 1                                                           # legend.text.font   An integer specifying the font style of the legend text; (1: normal, 2: bold, 3: italic, 4: bold and italic)
legend.cex <- 0.75                                                              # legend.cex         expansion factor for legend text.
legend.title <- NULL                                                            # legend.title       The title of the legend
legend.title.cex <- 1                                                           # legend.title.cex   expansion factor for legend title.                                                                     # Other graphical parameters of the plot() function arguments

#- Function:

# Preparation ####

# Extract data from fit
data <- as.data.frame(eval(fit$call$data))

# Extract no. of stratum
stratum <- max(1, length(fit$strata))

# Define colour for KM-Plot if not manually specified
if (is.null(col)){
  if(is.null(fit$strata)){
    col <- "black"
  } else {
    col <- as.factor(names(fit$strata))
  }
}


# Extract Group names for legend if not manually specified
if(is.null(fit$strata)){
  group <- "Cohort"
  legend.legend <- group
} else {
  group <- levels(as.factor(names(fit$strata)))
  legend.legend <- group
}


# Main Function ####

## Base Plot ####
base::plot(
  ## Plot the survival curve
  fit,
  conf.int = conf.int,
  conf.type = conf.type,
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


## Add confidence band ####
if(conf.band == TRUE){
  # Check if conf.band.col is defined otherwise print error.
  if(is.null(conf.band.col)){
    stop("Please specify `conf.band.col` to display the confidence band")
  } else {
    mapping <- 0
    # Loop for drawing polygons
    for (i in 1:stratum) {
      # More than 1 group
      if(stratum >1){
        mapping[length(mapping)+1] <- mapping[i]+fit$strata[i]
      }
      # Only 1 group
      if(stratum == 1){
        mapping[length(mapping)+1] <- length(fit$time)
      }

      # Extract x_coordinates from survfit object
      # Creates empty vector to store x coordiantes and
      # put the same 'x_values' in two subsequent element of the vector
      x_values <- fit$time[(mapping[i]+1):mapping[i+1]]
      x_coordinates <- rep(NA, length(x_values)*2)
      x_coordinates[seq(from = 1, to = length(x_values) * 2, by = 2)] <- x_values
      x_coordinates[seq(from = 2, to = length(x_values) * 2 - 1, by = 2)] <- x_values[2:length(x_values)]
      # Insert value in the last element of the vector
      x_coordinates[length(x_coordinates)] <- x_values[length(x_values)]
      x_coordinates <- c(x_coordinates, rev(x_coordinates))

      # Extract y_coordiantes from surfvit object
      # Creates empty vector to store y coordiantes and
      # put the same 'lower' in two subsequent element of the vector
      lower <- fit$lower[(mapping[i]+1):mapping[i+1]]
      y_coordinates_lwr <- rep(NA, length(lower)*2)
      y_coordinates_lwr[seq(1, length(lower)*2, 2)] <- lower
      y_coordinates_lwr[seq(2, length(lower)*2, 2)] <- lower

      # Creates empty vector to store y coordiantes and
      # put the same 'upper' in two subsequent element of the vector
      upper <- fit$upper[(mapping[i]+1):mapping[i+1]]
      y_coordinates_upr <- rep(NA, length(upper)*2)
      y_coordinates_upr[seq(1, length(upper)*2, 2)] <- upper
      y_coordinates_upr[seq(2, length(upper)*2, 2)] <- upper
      y_coordinates <- c(y_coordinates_lwr, rev(y_coordinates_upr))

      y_coordinates[is.na(y_coordinates)] <- min(lower,na.rm = T) # wieso ?

      # Draw CI band
      polygon(x = x_coordinates,
              y = y_coordinates,
              col = adjustcolor(conf.band.col[i], alpha.f =  conf.band.alpha), border = FALSE)
      # Draw CI band
    }
  }
}

## Draw grid ####
if (is.logical(grid)) {
  if (grid == TRUE) {
    grid(nx = length(xlim)-1, ny = length(ylim)-1)
  }
} else {
  stop("`gird` expecting TRUE or FALSE as an argument!")
}

## Add legend to plot  ####
if (is.logical(show.legend)){
  if(show.legend == TRUE){
    legend(x = legend.position[1],             # the x coordinates to positon the legend
           y = legend.position[2],             # the y coordinates to positoin the legend
           legend = legend.legend ,            # the text of the legend
           bty = "n",                          # boarder type for legend fixed as "none"
           col = col,
           lty = "solid",                      # line type for legend fixed as "solid"
           text.font = legend.text.font,
           title = legend.title,
           cex = legend.cex,
           title.cex = legend.title.cex
    )
  }
} else {
  stop("`show.legend` expecting TRUE or FAlSE as an argument!")
}

## Add Segments ####

#NOTE: Im moment CI statistik nur für 0.95 wird berechnet. Drawing aber für unterschiedliche CI möglich
# Als nächster Schritt anpassen. durch call(fit) und dann das mit dem korrekten CI berechnen.

# optional parameter
segment.fixed_timepoint <-  NULL                  # segment.fixed_timepoint: Draw segment(s) at a fixed time point (e.g. "6 month")
segment.fixed_quantile <- NULL                    # segment.fixed_quantile: Draw segment(s) at a fixed quantile (e.g. "median")
segment.col <- "black"                         # segment.col:
segment.lty <- "dashed"                           # segment.lty:
segment.lwd <- 1                                  # segment.lwd:


if (!is.null(segment.fixed_quantile ) & is.null(segment.fixed_timepoint)){
  # Code for segment at a specific quantile
  segment_y <- segment.fixed_quantile
  segment_x <- quantile(fit,probs = 1 - segment_y)

  # Vertical Line
  segments(x0 = segment_x$quantile,
           y0 = 0,
           x1 = segment_x$quantile,
           y1 = segment_y,
           col = segment.col,
           lty = segment.lty,
           lwd = segment.lwd)

  # Horizontal Line
  segments(x0 = 0,
           y0 = segment_y,
           x1 = segment_x$quantile,
           y1 = segment_y,
           col = segment.col,
           lty = segment.lty,
           lwd = segment.lwd )
  # Annotate the segment (Survival time at specific quantile)
  text(x = segment_x$quantile - 0.05,
       y = segment_y + 0.01,
       labels = paste0("Survival rate [", conf.int, "%]",
                       "\n",
                       round(segment_x$quantile,digits = 2),
                       " [",
                       round(segment_x$lower,digits = 2),
                       ",",
                       round(segment_x$upper,digits = 2),
                       "]"),
       pos = 4,
       col = segment.col,
       cex = 0.75) #legend.cex? or segment.cex?
} else if (is.null(segment.fixed_quantile ) & !is.null(segment.fixed_timepoint)){
  # Code for segment at a specific time point
  segment_x <- segment.fixed_timepoint
  segment_y <- summary(fit,time = segment_x)

  # Vertical Line
  segments(x0 = segment_x,
           y0 = 0,
           x1 = segment_x,
           y1 = segment_y$surv,
           col = segment.col,
           lty = segment.lty,
           lwd = segment.lwd)
  # Horizontal Line
  segments(x0 = 0,
           y0 = segment_y$surv,
           x1 = segment_x,
           y1 = segment_y$surv,
           col = segment.col,
           lty = segment.lty,
           lwd = segment.lwd )
  # Annotate the segment (Survival rate at specific timepoint)
  text(x = segment_x - 0.05,
       y = segment_y$surv + 0.01,
       labels = paste0("Survival rate [", segment_y$conf.int, "%]",
                       "\n",
                       round(segment_y$surv, digits = 2),
                       " [",
                       round(segment_y$lower, digits = 2),
                       ",",
                       round(segment_y$upper, digits = 2),
                       "]"),
       pos = 4,  # Position the text to the left of the point
       col = segment.col,
       cex = 0.75) #legend.cex? or segment.cex?
} else if (!is.null(segment.fixed_quantile) & !is.null(segment.fixed_timepoint)) {
  stop("`segment.fixed_timepoint` AND `segment.fixed_quantile ` not applicable! Choose one of the two options.")
}

## Draw risk table ####



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

survobject <- survival::survfit(Surv(time_yr, status) ~ 1, data = lung)

survobject <- survival::survfit(Surv(time_mt, status) ~ 1, data = lung)
