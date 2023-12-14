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
#' @param fit An object of class `survfit`, usually returned by the `survfit` function.
#' @param conf.int specifies the coverage probability. (FALSE, TRUE using 95% confidence intervals.  Alternatively, this can be a numeric value giving the desired confidence level.
#' @param conf.band Mapping the specified coverage probability
#' @param conf.band.col Can accept a single value for colour, or a vector of colour values to set colour(s)
#' @param conf.band.alpha Modify colour transparency for the confidence band
#' @param conf.type Specifies the transformation. Default: "log-log". Options ("log", "log-log", "plain", "logit", "arcsin")
#' @param grid gird A logical value for drawing Grid. (TRUE or FALSE)
#' @param col Can accept a single value for colour, or a vector of colour values to set colour(s)
#' @param main Title
#' @param sub Subtitle
#' @param xlab Label given to x-axis
#' @param ylab Label given to y-axis
#' @param cex.lab A numeric value specifying the size of the xlab and ylab
#' @param cex.axis A numeric value specifying the size of the axis size
#' @param bty The type of box to be drawn around the plot ("n","o","7","L","C","U")
#' @param lty A vector of string specifying line types for each curve (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”)
#' @param lwd A vector of numeric values for line widths.
#' @param xlim Set xlim based on the range, seq(starting value,  end value, number of increment of the sequence)
#' @param ylim Set ylim based on the range, seq(starting value,  end value, number of increment of the sequence)
#' @param show.legend Display legend
#' @param legend.position Position of the legend: c(x,y), "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
#' @param legend.legend A vector of string given to legend
#' @param legend.text.font An integer specifying the font style of the legend text; (1: normal, 2: bold, 3: italic, 4: bold and italic)
#' @param legend.cex Expansion factor for legend text
#' @param legend.title The title of the legend
#' @param legend.title.cex Expansion factor for legend title
#'
#'
#' @export
#'
#' @example ./R/example.R
#'
#' @import survival
#' @importFrom grDevices adjustcolor


surv.plot <- function(
    fit,
    # Confidence Interval options
    conf.int = fit$conf.int,
    conf.band = FALSE,
    conf.band.col = NULL,
    conf.band.alpha = 0.25,
    conf.type = "log-log",
    # Layout options
    grid = FALSE,
    col = NULL,
    main = NULL,
    sub = NULL,
    xlab = "Time",
    ylab = "Estimates Survival Probability",
    cex.lab = 1,
    cex.axis = 0.75,
    bty = "l",
    lty = c("solid","dotted","dotted"),
    lwd = 1,
    xlim = seq(from = 0, to = ceiling(max(fit$time))+ceiling(min(fit$time))),
    ylim = seq(from = 0, to = 1, by = 0.25),
    # Legend Options
    show.legend = TRUE,
    legend.position = "topright",
    legend.legend = NULL,
    legend.text.font = 1,
    legend.cex = 0.75,
    legend.title = NULL,
    legend.title.cex = 1
){

  # Extract Information from survfit object ####

  ## Extract data from fit ####
  data <- as.data.frame(eval(fit$call$data))

  ## Extract no. of stratum ####
  stratum <- max(1, length(fit$strata))

  ## Define colour for KM-Plot if not manually specified ####
  if (is.null(col)){
    if(is.null(fit$strata)){
      col <- "#666666"
    } else {
      for (i in 1:stratum){
        col[i] <- c("#666666","#a6761d","#66a61e","#377eb8","#e41a1c","#984ea3","#ff7f00","#f781bf","#ffff33")[i]
      }
    }
  }


  ## Extract Group names for legend if not manually specified ####
  if (is.null(legend.legend)){
    if(is.null(fit$strata)){
      group <- "Cohort"
      legend.legend <- group
    } else {
      group <- levels(as.factor(names(fit$strata)))
      legend.legend <- group
    }
  }

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

  ## Draw grid ####
  if (is.logical(grid)) {
    if (grid == TRUE) {
      grid(nx = length(xlim)-1, ny = length(ylim)-1)
    }
  } else {
    stop("`gird` expecting TRUE or FALSE as an argument!")
  }

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
        graphics::polygon(x = x_coordinates,
                y = y_coordinates,
                col = adjustcolor(conf.band.col[i], alpha.f =  conf.band.alpha), border = FALSE)
      }
    }
  }

  ## Add legend to plot  ####
  if (is.logical(show.legend)){
    if(show.legend == TRUE){
      graphics::legend(x = legend.position[1],             # the x coordinates to positon the legend
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
}
