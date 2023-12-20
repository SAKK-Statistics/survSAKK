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
#' @param fit An object of class `survfit`, usually returned by the `survfit` function
#' @param mark.censoring Curves are marked at each censoring time if TRUE otherwise FALSE.
#' @param conf.int Specifies the coverage probability. (FALSE, TRUE using 95% confidence intervals.  Alternatively, this can be a numeric value giving the desired confidence level.
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
#' @param segment.type A numeric value specifying the layout of the segment (1: Draws specified segment (full bandwidth), 2: Draws specified segment, 3: Drawing vertical and horizontal segment)
#' @param segment.timepoint A single value or a vector of fixed time points of segment(s)
#' @param segment.quantile A single value or a vector of fixed quantile of segment(s) at a fixed quantile (e.g. 0.5 corresponds to median)
#' @param segment.col Can accept a single value for colour, or a vector of colour values to set colour(s)
#' @param segment.annotation.col Can accept a single value for colour, or a vector of colour values to set colour(s)
#' @param segment.lty A vector of string specifying line types for each curve (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”)
#' @param segment.lwd A vector of numeric values for line widths
#' @param segment.cex A numeric values specifying the size of the segment annotation size
#' @param segment.font A numeric value specifying the font face (1 = plain, 2 = bold, 3 = italic, 4 = bold-italic)
#' @param segment.main Title of segment text
#' @param segment.annotation Position of the segment annotation: (c(x,y), "bottomleft", "left", "right", "none")
#' @param segment.annotation.space Spacing between the text in unit of x-coordinates
#'
#' @export
#'
#' @example ./R/example.R
#'
#' @import survival
#' @importFrom grDevices adjustcolor
#' @importFrom stats quantile
#' @importFrom graphics segments
#' @importFrom graphics text
#'


surv.plot <- function(
    fit,
    mark.censoring = TRUE,
    # Confidence Interval options
    conf.int = fit$conf.int,
    conf.band = TRUE,
    conf.band.col = col,
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
    # Legend options
    show.legend = TRUE,
    legend.position = "topright",
    legend.legend = NULL,
    legend.text.font = 1,
    legend.cex = 0.75,
    legend.title = NULL,
    legend.title.cex = 1,
    # Segment options
    segment.type = 3,
    segment.timepoint = NULL,
    segment.quantile = NULL,
    segment.main = NULL,
    segment.annotation = "right",
    segment.col = "#666666",
    segment.annotation.col = col,
    segment.lty = "dashed",
    segment.lwd = 1,
    segment.cex = 0.75,
    segment.annotation.space = 0.03,
    segment.font = 1
){

  # Function for rounding p-value ####
  ## two significant digit e.g. p = 0.43 or 0.057
  ## if 0.001 > p > 0.0001, then round to one significant digit
  ## else p < 0.0001

  round.pval <- function(x){
    if (x < 0.0001){
      pval <- "< 0.0001"
    } else if (x <= 0.001 && x >= 0.0001){
      pval <- format(signif(x, digits = 1), scientific = FALSE)
    } else {
      pval <- format(signif(x, digits = 2), scientific = FALSE)
    }
    return(pval)
  }

  # Extract Information from survfit object ####
  ## Extract data from fit ####
  data <- as.data.frame(eval(fit$call$data))

  ## Recalculate survival object ####
  # Note: Recalculation is done to be sure that the survival object is correct
  # for plotting with the desired CI and transformation.

    # recalculate the fit object based on defined `conf.type`
    fit$call$conf.type <- conf.type
    # recalculate the fit object based on defined `conf.int`
    fit$call$conf.int <- conf.int

    fit <- eval(fit$call)

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

# Base Plot ####
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
    mark.time = mark.censoring,
    pch = c("I"),
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

# Draw grid ####
  if (is.logical(grid)) {
    if (grid == TRUE) {
      grid(nx = length(xlim)-1, ny = length(ylim)-1)
    }
  } else {
    stop("`gird` expecting TRUE or FALSE as an argument!")
  }

# Add confidence band ####
  if(conf.band == TRUE){
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

        y_coordinates[is.na(y_coordinates)] <- min(lower,na.rm = T)

        # Draw CI band
        if(is.null(conf.band.col)){
        graphics::polygon(x = x_coordinates,
                y = y_coordinates,
                col = adjustcolor(col = col[i], alpha.f =  conf.band.alpha), border = FALSE)
        }
        else{
          graphics::polygon(x = x_coordinates,
                            y = y_coordinates,
                            col = adjustcolor(col = conf.band.col[i], alpha.f =  conf.band.alpha), border = FALSE)
        }
      }
    }

# Add legend to plot  ####
  if (is.logical(show.legend)){
    if(show.legend == TRUE){
      graphics::legend(x = legend.position[1],   # the x coordinates to position the legend
             y = legend.position[2],             # the y coordinates to position the legend
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

# Add Segment ####
  ## Define different options to display the segment text  ####
  if (length(segment.annotation) == 2) {
    # Checks if it's a numeric vector (x, y coordinates)
    text_xpos <- segment.annotation[1]
    text_ypos <- segment.annotation[2]
    # Position the text below of the specified (x,y)
    pos <- 1
  } else if (segment.annotation == "bottomleft") {
    text_ypos <- 0.03
    text_xpos <- min(xlim)
    # Position the text to the right of the specified (x,y)
    pos <- 4
  } else if (segment.annotation == "left"){
    text_ypos <- 0.53
    text_xpos <- min(xlim)
    pos <- 4
  } else if (segment.annotation == "right"){
    text_ypos <- 0.53
    text_xpos <- max(xlim)
    # Position the text to the left of the specified (x,y)
    pos <- 2
  }

  ## Determining the y coordinate for each text ####
  if (stratum == 1){
    text_ypos[i] <- text_ypos
  } else {
    for (i in stratum-1){
      text_ypos[i+1] <- text_ypos[i]+ segment.annotation.space
    }
  }


  ## Draw segments ####
  if (segment.type == 3){
    ### Type 3: Drawing vertical and horizontal segments ####
    if (!is.null(segment.quantile) & is.null(segment.timepoint)){
      # Code for segment at a specific quantile
      segment_y <- segment.quantile
      segment_x <- quantile(fit,probs = 1 - segment_y)

      # Draw vertical Line
      segments(x0 = segment_x$quantile,
               y0 = 0,
               x1 = segment_x$quantile,
               y1 = segment_y,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)

      # Draw horizontal Line
      segments(x0 = 0,
               y0 = segment_y,
               x1 = segment_x$quantile,
               y1 = segment_y,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd )

      # Annotate the segment (Survival time at specific quantile)
      if (segment.annotation != "none"){
        text(x = text_xpos,
             y = text_ypos,
             labels = paste0(round(segment_x$quantile,digits = 2),
                             " [",
                             round(segment_x$lower,digits = 2),
                             ",",
                             round(segment_x$upper,digits = 2),
                             "]"),
             pos = pos,
             col = segment.annotation.col,
             cex = segment.cex,
             font = segment.font)
      }
    } else if (is.null(segment.quantile ) & !is.null(segment.timepoint)){
      # Code for segment at a specific time point
      segment_x <- segment.timepoint
      segment_y <- summary(fit,time = segment_x)

      # Draw vertical Line
      segments(x0 = segment_x,
               y0 = 0,
               x1 = segment_x,
               y1 = segment_y$surv,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)

      # Draw horizontal Line
      segments(x0 = 0,
               y0 = segment_y$surv,
               x1 = segment_x,
               y1 = segment_y$surv,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)

      # Annotate the segment
      if (segment.annotation != "none"){
        text(x = text_xpos,
             y = text_ypos,
             labels = paste0(round(segment_y$surv, digits = 2),
                             " [",
                             round(segment_y$lower, digits = 2),
                             ",",
                             round(segment_y$upper, digits = 2),
                             "]"),
             pos = pos,
             col = segment.annotation.col,
             cex = segment.cex,
             font = segment.font)
      }
    } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
      stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options.")
    }
  } else if (segment.type == 2){
    ### Type 2: Draw specified segment ####
    if (!is.null(segment.quantile ) & is.null(segment.timepoint)){
      # Code for segment at a specific quantile
      segment_y <- segment.quantile
      segment_x <- quantile(fit,probs = 1 - segment_y)

      # Horizontal Line
      segments(x0 = 0,
               y0 = segment_y,
               x1 = segment_x$quantile,
               y1 = segment_y,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)

      # Annotate the segment
      if (segment.annotation != "none"){
        text(x = text_xpos,
             y = text_ypos,
             labels = paste0(round(segment_x$quantile,digits = 2),
                             " [",
                             round(segment_x$lower,digits = 2),
                             ",",
                             round(segment_x$upper,digits = 2),
                             "]"),
             pos = pos,
             col = segment.annotation.col,
             cex = segment.cex,
             font = segment.font)
      }
    } else if (is.null(segment.quantile ) & !is.null(segment.timepoint)){
      # Code for segment at a specific time point
      segment_x <- segment.timepoint
      segment_y <- summary(fit,time = segment_x)

      # Vertical Line
      segments(x0 = segment_x,
               y0 = 0,
               x1 = segment_x,
               y1 = segment_y$surv,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)

      # Annotate the segment
      if (segment.annotation != "none"){
        text(x = text_xpos,
             y = text_ypos,
             labels = paste0(round(segment_y$surv, digits = 2),
                             " [",
                             round(segment_y$lower, digits = 2),
                             ",",
                             round(segment_y$upper, digits = 2),
                             "]"),
             pos = pos,
             col = segment.col,
             cex = segment.cex,
             font = segment.font)
      }
    } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
      stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options.")
    }
  } else if (segment.type == 1){
    ### Type 1: Drawing specified segment (full bandwidth) ####
    if (!is.null(segment.quantile ) & is.null(segment.timepoint)){
      # Code for segment at a specific quantile
      segment_y <- segment.quantile
      segment_x <- quantile(fit,probs = 1 - segment_y)

      # Draw horizontal Line
      segments(x0 = 0,
               y0 = segment_y,
               x1 = max(xlim),
               y1 = segment_y,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd )

      # Annotate the segment
      if (segment.annotation != "none"){
        text(x = text_xpos,
             y = text_ypos,
             labels = paste0(round(segment_x$quantile,digits = 2),
                             " [",
                             round(segment_x$lower,digits = 2),
                             ",",
                             round(segment_x$upper,digits = 2),
                             "]"),
             pos = pos,
             col = segment.annotation.col,
             cex = segment.cex,
             font = segment.font)
      }
    } else if (is.null(segment.quantile ) & !is.null(segment.timepoint)){
      # Code for segment at a specific time point
      segment_x <- segment.timepoint
      segment_y <- summary(fit,time = segment_x)

      # Draw vertical Line
      segments(x0 = segment_x,
               y0 = 0,
               x1 = segment_x,
               y1 = max(ylim),
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)

      # Annotate the segment
      if (segment.annotation != "none"){
        text(x = text_xpos,
             y = text_ypos,
             labels = paste0(round(segment_y$surv, digits = 2),
                             " [",
                             round(segment_y$lower, digits = 2),
                             ",",
                             round(segment_y$upper, digits = 2),
                             "]"),
             pos = pos,
             col = segment.annotation.col,
             cex = segment.cex,
             font = segment.font)
      }
    } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
      stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options. ")
    }
  }

  ## Draw title for segment text ####
  if (segment.annotation != "none"){
    if (!is.null(segment.main)){
      text(text_xpos, max(text_ypos) + segment.annotation.space, label = segment.main, pos = pos,
           col = "black", cex = segment.cex)
    } else if (is.null(segment.main) & !is.null(segment.quantile)){
      if (segment.quantile == 0.5){
        text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0("Median [95%]"), pos = pos,
             col = "black", cex = segment.cex)
      } else {text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0(segment.quantile,"-Quantile [95%]"), pos = pos,
                   col = "black", cex = segment.cex)
      }
    } else if (is.null(segment.main) & !is.null(segment.timepoint)){
      text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0(segment.quantile,"Survival [95%]"), pos = pos,
           col = "black", cex = segment.cex)
    }
  }
} # final closer of the function
