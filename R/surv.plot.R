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
#' @export
#' @param fit An object of class `survfit` containing survival data.
#' @param mark.censoring Mark censoring events on the curves. Logical (default: TRUE)
#' @param conf.int Display confidence intervals. (FALSE, TRUE for 95% confidence intervals, or specify a numeric value for desired coverage.
#' @param conf.band Mapping the specified coverage probability
#' @param conf.band.col Colour(s) for confidence band. Can accept a single value for colour, or a vector of colour values.
#' @param conf.band.transparent Transparency for the confidence band.
#' @param conf.type Transformation type of the confidence interval.Options: "log", "log-log", "plain", "logit", "arcsin";(default: "log-log").
#' @param grid Draw a grid on the plot Logical (default: FALSE)
#' @param col Colour(s) for the survival curves. Can accept a single value for colour, or a vector of colour values to set colour(s).
#' @param main Title of the plot.
#' @param sub Subtitle of the plot.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param cex A numeric value specifying the global size of the text.
#' @param cex.lab A numeric value specifying the size of the xlab and ylab text.
#' @param cex.axis A numeric value specifying the size of the axis size.
#' @param bty The type of box to be drawn around the plot ("n","o","7","L","C","U")
#' @param lty A vector of string specifying line types for each curve (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”).
#' @param lwd A vector of numeric values for line widths.
#' @param xlim X-axis limits specified as a sequence; seq(starting value,  end value, number of increment of the sequence).
#' @param ylim Y-axis limits specified as a sequence; seq(starting value,  end value, number of increment of the sequence).
#' @param show.legend Display legend.
#' @param legend.position Position of the legend (c(x,y), "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center").
#' @param legend.name A vector of string specifying the name(s) of stratum.
#' @param legend.text.font An integer specifying the font style of the legend text; (1: normal, 2: bold, 3: italic, 4: bold and italic).
#' @param legend.cex Expansion factor for legend text.
#' @param legend.title The title of the legend.
#' @param legend.title.cex Expansion factor for legend title.
#' @param segment.type A numeric value specifying the layout of the segment (1: Draws specified segment (full bandwidth), 2: Draws specified segment, 3: Drawing vertical and horizontal segment).
#' @param segment.timepoint A single value or a vector of fixed time points of segment(s).
#' @param segment.quantile A single value or a vector of fixed quantile of segment(s) at a fixed quantile (e.g. 0.5 corresponds to median).
#' @param segment.col Can accept a single value for colour, or a vector of colour values to set colour(s).
#' @param segment.annotation.col Can accept a single value for colour, or a vector of colour values to set colour(s).
#' @param segment.lty A vector of string specifying line types for each curve (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”).
#' @param segment.lwd A vector of numeric values for line widths.
#' @param segment.cex A numeric values specifying the size of the segment annotation size.
#' @param segment.font A numeric value specifying the font face (1 = plain, 2 = bold, 3 = italic, 4 = bold-italic, ...).
#' @param segment.main Title of segment text.
#' @param segment.main.font A numeric value specifying the fon face (1 = plain, 2 = bold, 3 = italic, 4 = bold-italic, ...).
#' @param segment.annotation Position of the segment annotation: (c(x,y), "bottomleft", "left", "right", "none").
#' @param segment.annotation.space Spacing between the text in unit of x-coordinates.
#' @param stat  Statistics which is displayed in the plot ("logrank", "coxph", "coxmodel", "none").
#' @param stat.position Position where the stat should be displayed: (c(x,y), "bottomleft", "left", "right", "none").
#' @param stat.col Can accept a single value for colour.
#' @param stat.cex A numeric value specifying the size of the stat size.
#' @param stat.font The font face (1 = plain, 2 = bold, 3 = italic, 4 = bold-italic).
#' @param risktable A logical value for drawing risk table (Default: TRUE).
#' @param risktable.title Title of risk table.
#' @param risktable.title.font Title font of risk table (1 = normal, 2 = bold, 3 = italic, 4 = bold and italic).
#' @param risktable.title.col Colour for the risk table title. Can accept a single value for colour.
#' @param risktable.title.position Position of the title on the x-axis.
#' @param risktable.cex A numeric value specifying the size of the risk table size.
#' @param risktable.title.cex A numeric value specifying the size of the risk table title.
#' @param risktable.name.cex A numeric value specifying the size of the riksk legend name(s).
#' @param risktable.col Colour(s) for the risk table. Can accept a single value for colour, or a vector of colour values to set colour(s).
#' @param risktable.name.font legend name(s) font of risk table (1 = normal, 2 = bold, 3 = italic, 4 = bold and italic).
#' @param risktable.name.col Colour for the risk table name. Can accept a single value for colour.
#' @param risktable.name.position Position of the legend name(s) on the x-axis.
#'
#' @return Publication-Ready Kaplan-Meier Plot incorporating various statistics and layout customisation options to enhance the efficiency and adaptability of the Kaplan-Meier plot.
#'
#' @examples
#' # Load Libraray
#'  require(survival)
#'  require(survSAKK)
#'
#' # Load Data
#'  veteran$time_yr <- veteran$time/365.25
#'  veteran$time_mt <- veteran$time_yr*12
#'
#' # Create survival object
#'  veteran_fit_yr <- survfit(Surv(time_yr, status) ~ 1, data = veteran)
#'  veteran_trt_fit_mt <- survfit(Surv(time_mt, status) ~ trt, data = veteran)
#'
#' # Generate survival plots
#'  par(mfrow=c(1,2))
#'  survSAKK::surv.plot(fit = veteran_fit_yr,
#'           risktable = FALSE,
#'           xlim = seq(0,3, by = 0.5),
#'           segment.quantile = 0.5,
#'           segment.annotation.space = 0.02)
#'
#'  survSAKK::surv.plot(fit = veteran_trt_fit_mt,
#'           col = c("#5ab4ac","#d8b365"),
#'           legend.title = "Treatment Regimens",
#'           legend.name = c("LT60","OV60"),
#'           segment.timepoint = 1,
#'           segment.col = "darkred",
#'           stat = "coxph",
#'           stat.position = "bottomleft",
#'           risktable.col = c("#5ab4ac","#d8b365"))
#'  par(mfrow=c(1,1))
#'
#' @import survival
#' @import graphics
#' @import stats
#' @importFrom grDevices adjustcolor



surv.plot <- function(
    fit,
    mark.censoring = TRUE,
    # Confidence Interval options
    conf.int = fit$conf.int,
    conf.band = TRUE,
    conf.band.col = col,
    conf.band.transparent = 0.25,
    conf.type = "log-log",
    # Layout options
    grid = FALSE,
    col = NULL,
    main = NULL,
    sub = NULL,
    xlab = "Time",
    ylab = "Estimated survival probability",
    cex = NULL,
    cex.lab = 1,
    cex.axis = 1,
    bty = "l",
    lty = c("solid","dotted","dotted"),
    lwd = 1,
    xlim = seq(from = 0, to = ceiling(max(fit$time))+ceiling(min(fit$time))),
    ylim = seq(from = 0, to = 1, by = 0.25),
    # Legend options
    show.legend = TRUE,
    legend.position = "topright",
    legend.name = NULL,
    legend.text.font = 1,
    legend.cex = 1,
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
    segment.cex = 1,
    segment.annotation.space = 0.03,
    segment.font = 1,
    segment.main.font = 1,
    # Stats options
    stat = "none",
    stat.position = "right",
    stat.col = "black",
    stat.cex = 1,
    stat.font = 1,
    # risk table options
    risktable = TRUE,
    risktable.title = "Number at risk",
    risktable.title.font = 3,
    risktable.title.col = "black",
    risktable.title.position = par("usr")[1] - (par("usr")[2]- par("usr")[1])*0.1,
    risktable.cex = 1,
    risktable.title.cex = 1,
    risktable.name.cex = 1,
    risktable.col = "black",
    risktable.name.font = 2,
    risktable.name.col = "#666666",
    risktable.name.position = par("usr")[1] - (par("usr")[2]- par("usr")[1])*0.1
){

  # 1. PREPARTION ####

  ## Function for rounding p-value ####
  # two significant digit e.g. p = 0.43 or 0.057
  # if 0.001 > p > 0.0001, then round to one significant digit
  # else p < 0.0001

  round.pval <- function(x){
    if (x < 0.0001){
      pval <- "p < 0.0001"
    } else if (x <= 0.001 && x >= 0.0001){
      pval <- paste("p =", format(signif(x, digits = 1), scientific = FALSE))
    } else {
      pval <- paste("p = ", format(signif(x, digits = 2), scientific = FALSE))
    }
    return(pval)
  }


  ## Global font parameter ####
  if(!is.null(cex)){
    cex.lab <- cex
    cex.axis <- cex
    legend.cex <- cex
    legend.title.cex <- cex
    segment.cex <- cex
    stat.cex <- cex
    risktable.cex <- cex
    risktable.title.cex <- cex
    risktable.name.cex <- cex
  }

  ## Function to draw table (surv.stats) into plot ####
  # plottbl() function allows to plot reproducible different tables in the graphics
  plottbl <- function (x, y,
                       table, # A data frame, matrix or similar object that will be displayed
                       cex = stat.cex,
                       # Positioning for the table relative to ‘⁠x,y⁠’.
                       xjust = 0,
                       yjust = 1,
                       # The amount of padding around text in the cells as a proportion
                       # of the maximum width and height of the strings in each column
                       xpad = 0.1,
                       ypad = 0.5,
                       text.col = stat.col,
                       pos = pos,
                       font = stat.font)
  {
    tabdim <- dim(table)
    column.names <- colnames(table)
    cellwidth <- rep(0, tabdim[2])

    # Calculate cell widths for each column
    for(column in 1:tabdim[2]){
      cellwidth[column] <- max(strwidth(c(column.names[column], format(table[, column])),
                                        cex = cex)) * (1 + xpad)
    }

    nvcells <- tabdim[1] + 1
    cellheight <- max(strheight(c(column.names, as.vector(unlist(table))),
                                cex = cex)) * (1 + ypad)
    ytop <- y + yjust * nvcells * cellheight

    # Draw column names
    xleft <- x - xjust * (sum(cellwidth))
    for (column in 1:tabdim[2]) {
      text(xleft + cellwidth[column] * 0.5, ytop - 0.5 * cellheight, column.names[column],
           cex = cex, col = stat.col, font = stat.font)
      xleft <- xleft + cellwidth[column]
    }

    # Draw tables cells
    for (row in 1:tabdim[1]) {
      xleft <- x - xjust * (sum(cellwidth))
      for (column in 1:tabdim[2]) {
        text(xleft + 0.5 * cellwidth[column],
             ytop - (row + 0.5) * cellheight, table[row, column],
             cex = cex, col = stat.col, font = stat.font)
        xleft <- xleft + cellwidth[column]
      }
    }
  }

  ## Extract Information from survfit object ####

  ### Extract data from fit ####
  data <- as.data.frame(eval(fit$call$data))

  ### Recalculate survival object ####
  # Note: Recalculation is done to be sure that the survival object is correct,
  # and for plotting with the desired CI and transformation.

  # recalculate the fit object based on defined `conf.type`
  fit$call$conf.type <- conf.type
  # recalculate the fit object based on defined `conf.int`
  fit$call$conf.int <- conf.int

  fit <- eval(fit$call)

  ### Extract level of stratum ####
  stratum <- max(1, length(fit$strata))

  ## Define colour for KM-plot if not manually specified ####
  if (is.null(col)){
    if(is.null(fit$strata)){
      col <- "#666666"
    } else {
      for (i in 1:stratum){
        col[i] <- c("#666666","#a6761d","#66a61e","#377eb8","#e41a1c","#984ea3","#ff7f00","#f781bf","#ffff33")[i]
      }
    }
  }

  ## Extract Group(stratum) names for legend if not manually specified ####
  if (is.null(legend.name)){
    if(is.null(fit$strata)){
      group <- "Cohort"
      legend.name <- group
    } else {
      group <- names(fit$strata)
      legend.name <- group
    }
  }

  # 2. SURV.PLOT ####

  ## Main Plotting Function ####
  base::plot(
    # Plot the survival curve
    fit,
    conf.int = conf.int,
    conf.type = conf.type,
    main = main,
    sub = sub,
    col = col,
    lty = lty,
    lwd = lwd,
    # Add censoring information with ticks
    mark.time = mark.censoring,
    pch = c("I"),
    # Modify Layout
    xaxs = "i", yaxs = "i",               # Start axis exactly from zero origin
    xaxt = "n", yaxt = "n",               # Remove the original axes
    bty = bty,                            # Remove borders
    ylim = range(ylim),                   # Set y-axis limits
    xlim = range(xlim),                   # Set x-axis limits
    xlab = xlab,                          # Draw x label
    ylab = ylab,                          # Draw y label
    cex.lab = cex.lab                     # Label size
  )

  # Customize the x coordinates
  graphics::axis(
    side = 1,                             # Specifies the side
    las = 0,                              # Rotate the labels
    mgp = c(3,0.50,0),                    # Adjust the label position (axis title, axis label, axis line)
    at = xlim,                            # Specify tick mark position
    labels = xlim,                        # Draw labels
    cex.axis = cex.axis                   # Axis size
  )

  # Customize the y coordinates
  graphics::axis(
    side = 2,                             # Specifies the side
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
    mapping <- 0
    # Loop for drawing polygons
    for (i in 1:stratum) {
      if(stratum >1){
        mapping[length(mapping)+1] <- mapping[i]+fit$strata[i]
      }
      if(stratum == 1){
        mapping[length(mapping)+1] <- length(fit$time)
      }
      # Extract x_coordinates from survfit object
      x_values <- fit$time[(mapping[i]+1):mapping[i+1]]
      # Create empty vector to store x coordinates.
      x_coordinates <- rep(NA, length(x_values)*2)
      x_coordinates[seq(from = 1, to = length(x_values) * 2, by = 2)] <- x_values
      # Put the same 'x_values' in two subsequent element of the vector
      x_coordinates[seq(from = 2, to = length(x_values) * 2 - 1, by = 2)] <- x_values[2:length(x_values)]
      # Insert value in the last element of the vector
      x_coordinates[length(x_coordinates)] <- x_values[length(x_values)]
      x_coordinates <- c(x_coordinates, rev(x_coordinates))

      # Extract y_coordiantes_lwr from surfvit object(lower)
      lower <- fit$lower[(mapping[i]+1):mapping[i+1]]
      # Creates empty vector to store y coordiantes
      y_coordinates_lwr <- rep(NA, length(lower)*2)
      # Put the same 'lower' in two subsequent element of the vector
      y_coordinates_lwr[seq(1, length(lower)*2, 2)] <- lower
      y_coordinates_lwr[seq(2, length(lower)*2, 2)] <- lower

      # Extract y_coordinates_upr from survfit object(upper)
      upper <- fit$upper[(mapping[i]+1):mapping[i+1]]
      # Creates empty vector to store y coordinates
      y_coordinates_upr <- rep(NA, length(upper)*2)
      # Put the same 'upper' in two subsequent element of the vector
      y_coordinates_upr[seq(1, length(upper)*2, 2)] <- upper
      y_coordinates_upr[seq(2, length(upper)*2, 2)] <- upper
      # Combine both y_coordinates
      y_coordinates <- c(y_coordinates_lwr, rev(y_coordinates_upr))
      y_coordinates[is.na(y_coordinates)] <- min(lower,na.rm = T)

      # Draw CI band
      if(is.null(conf.band.col)){
        graphics::polygon(
          x = x_coordinates,
          y = y_coordinates,
          col = adjustcolor(col = col[i],
                            alpha.f =  conf.band.transparent),
          border = FALSE)
      }
      else{
        graphics::polygon(
          x = x_coordinates,
          y = y_coordinates,
          col = adjustcolor(col = conf.band.col[i],
                            alpha.f =  conf.band.transparent),
          border = FALSE)
      }
    }
  }

  ## Add legend to plot  ####
  if (is.logical(show.legend)){
    if(show.legend == TRUE){
      graphics::legend(
        x = legend.position[1],   # the x coordinates to position the legend
        y = legend.position[2],             # the y coordinates to position the legend
        legend = legend.name ,            # the text of the legend
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

  # 3.SURV.SEGMENT ####

  ## Define different options to display the segment text  ####
  if (length(segment.annotation) == 2) {
    # Checks if it's a numeric vector (x, y coordinates)
    text_xpos <- segment.annotation[1]
    text_ypos <- segment.annotation[2]
    # Position the text to the right of the specified (x,y)
    pos = 4
  } else if (segment.annotation == "bottomleft") {
    text_ypos <- 0.03
    text_xpos <- min(xlim)
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

  ## Main Segment Function ####
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
               lwd = segment.lwd)

      # Annotate the segment (Survival time at specific quantile)
      if (!("none" %in% segment.annotation)){
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
    } else if (is.null(segment.quantile) & !is.null(segment.timepoint)){
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
      if (!("none" %in% segment.annotation)){
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
      if (!("none" %in% segment.annotation)){
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
      if (!("none" %in% segment.annotation)){
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
      if (!("none" %in% segment.annotation)){
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
      if (!("none" %in% segment.annotation)){
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

  ### Draw title for segment text ####
  if (!("none" %in% segment.annotation)){
    if (!is.null(segment.main)){
      text(text_xpos, max(text_ypos) + segment.annotation.space, label = segment.main, pos = pos,
           col = "black", cex = segment.cex, font = segment.main.font)
    } else if (is.null(segment.main) & !is.null(segment.quantile)){
      if (segment.quantile == 0.5){
        text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0("Median [95%]"), pos = pos,
             col = "black", cex = segment.cex, font = segment.main.font)
      } else {text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0(segment.quantile,"-Quantile [95%]"), pos = pos,
                   col = "black", cex = segment.cex, font = segment.main.font)
      }
    } else if (is.null(segment.main) & !is.null(segment.timepoint)){
      text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0(segment.quantile,"Survival [95%]"), pos = pos,
           col = "black", cex = segment.cex, font = segment.main.font)
    }
  }

  # 4. SURV.STATS ####

  ## Define different options for stat position ####
  if (length(stat.position) == 2){
    # If it's a numeric vector (x, y coordinates)
    stat_xpos <- stat.position[1]
    stat_ypos <- stat.position[2]
    # Position the text to the right of the specified (x,y)
    pos <- 4 #vorher 1
  } else if (stat.position == "bottomleft"){
    stat_ypos <- 0.03
    stat_xpos <- min(xlim)
    pos <- 4
  } else if (stat.position == "left"){
    stat_ypos <- 0.53
    stat_xpos <- min(xlim)
    pos <- 4
  }else if (stat.position == "right"){
    stat_ypos <- 0.53
    stat_xpos <- max(xlim)
    # Position the text to the left of the specified (x,y)
    pos <- 2
  }

  ## Log rank test ####

  # To compare the survival curves of two or more groups
  logrank <- fit$call
  logrank$conf.type <- NULL
  logrank$conf.int <- NULL
  logrank[1] <- call("survdiff")

  # Check first if strata > 1
  if(is.null(fit$strata)){
    logrank <- NULL
  } else {
    logrank <- eval(logrank)
    # Recalculating p-Value
    logrankpval <- as.numeric(format.pval(1 - pchisq(logrank$chisq, df = length(logrank$n) - 1), esp = 0.001))
    logrankpval <- round.pval(logrankpval)
  }

  ## Cox proportional hazard regression ####

  # To describe the effect of variables on survival
  model <- fit$call
  model$conf.type <- NULL
  model$conf.int <- NULL
  model[1] <- call("coxph")
  model <- summary(eval(model))

  ## Display statistics in the plot ####

  if(stat == "logrank"){
    stats <- paste0("Logrank test: ", logrankpval)
  } else if(stat == "coxph"){
    stats <- paste0("HR ",
                    round(model$conf.int[,"exp(coef)"], digits = 2),
                    " (95% CI: ",
                    round(model$conf.int[,"lower .95"], digits = 2),
                    " to ",
                    round(model$conf.int[,"upper .95"], digits = 2),
                    ")")
  } else if(stat == "coxmodel"){
    if("right" %in% stat.position){
      # table is always written from the specified x,y pos from left to right
      # therefore stat.positon="right" position is outside of the border.
      # It has to be corrected for tables.

      # Extract infos and create data frame from model
      tbl <- data.frame(N = model$n,
                        Events = model$nevent,
                        HR = round(model$conf.int[,"exp(coef)"], digits = 2),
                        lwrCI = round(model$conf.int[,"lower .95"], digits = 2),
                        uprCI = round(model$conf.int[,"upper .95"], digits = 2),
                        Logrank = logrankpval)
      # Annotation
      # plottbl() function was written to allow to plot different tables reproducible
      plottbl(x = stat_xpos - max(xlim)/2,
              y = stat_ypos,
              tbl,
              cex = stat.cex)
    } else {
      # Extract infos and create data frame from model
      tbl <- data.frame(N = model$n,
                        Events = model$nevent,
                        HR = round(model$conf.int[,"exp(coef)"], digits = 2),
                        lwrCI = round(model$conf.int[,"lower .95"], digits = 2),
                        uprCI = round(model$conf.int[,"upper .95"], digits = 2),
                        Logrank = logrankpval)
      # Annotation
      # plottbl() function was written to allow to plot different tables reproducible
      plottbl(x = stat_xpos,
              y = stat_ypos,
              tbl,
              cex = stat.cex)
    }
  }

  if (stat != "none" && stat != "coxmodel"){
    # Annotate the stats in the plot when stat = "coxph, loglik etc.
    text(x = stat_xpos,
         y = stat_ypos,
         labels = stats,
         pos = pos,
         col = stat.col,
         cex = stat.cex,
         font = stat.font)
  }

  # 5. SURV.RISKTABLE ####
  if(is.logical(risktable)){
    if (risktable == TRUE){
      obsStrata <- if(is.null(fit$strata)){
        obsStrata <- 1
      } else {
        obsStrata <- fit$strata
      }

      grp <- rep(1:stratum, times=obsStrata)

      # Initialize a matrix 'n.risk.matrix' with zeros
      n.risk.matrix <- matrix(0,nrow = length(xlim), ncol = stratum)

      # Loop over each stratum and each time point defined by 'xlim'
      for (stratum_i in 1:stratum) {
        for (x in 1:length(xlim)) {
          # Find the indices where the survival time for the current group is greater than the current 'xlim'
          index <- which(fit$time[grp == stratum_i] > xlim[x])
          # If there are no such indices, set the corresponding element in 'n.risk.matrix' to 0
          if (length(index) == 0)
            n.risk.matrix[x,stratum_i] <- 0
          else
            # Otherwise, set the element to the minimum number at risk for the specified group and time point
            n.risk.matrix[x,stratum_i] <- fit$n.risk[grp == stratum_i][min(index)]
        }
      }

      # Set up the plot with margin (ora) and outer margins (oma)
      par(mar = c(stratum + 5, stratum + 5, 4, 2)+0.1,  # c(bottom, left, top, right)
          mgp = c(2,3,0)                        # c(axis title, axis label, axis ticks)
          )

      # Add risktable.title text to the outer margin
      mtext(risktable.title, side = 1, outer = FALSE,
            line = 4, adj = NA, at = risktable.title.position,
            font = risktable.title.font,
            cex = risktable.title.cex,
            col = risktable.title.col)

      # Add legend text to the outer margin for each stratum
      for (i in 1:stratum){
        mtext(text = legend.name[i], side = 1, outer = FALSE,
              line = i+4, adj = NA, at = risktable.name.position,
              font = risktable.name.font,
              cex = risktable.name.cex,
              col = risktable.name.col)
      }

      # Add vector of risk counts text to the margin
      mtext(text = as.vector(n.risk.matrix), side = 1, outer = FALSE,
            line = rep((1:stratum) + 4, each = length(xlim)),
            at = rep(xlim, stratum),
            cex = risktable.cex,
            col = c(rep(risktable.col, each = length(xlim)))
      )
    }
  } else {
    stop("`risktable` expecting TRUE or FALSE as an argument!")
  }

} # final closer of the function
