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
#' @param fit An object of class `survfit` containing survival data.
#' @param mark.censoring A logical parameter indicating whether to mark censoring
#'    events on the survival curves. Default: \code{TRUE}.
#' @param conf.int Controlling the confidence interval on the survival curves.
#'    When set to \code{TRUE}, the function displays default 95% confidence interval.
#'    If set to \code{FALSE}, confidence intervals are not displayed.
#'    If a numeric value between 0 and 1 is provided, it represents the desired
#'    coverage for the confidence interval (e.g. 0.9 for 90%).
#' @param conf.band A logical parameter indicating whether to display the
#'    confidence band on the survival curves. Default: \code{TRUE}.
#' @param conf.line  A logical parameter indicating whether to draw the confidence
#'    line on the survival curves. Default: \code{FALSE}.
#' @param conf.band.col Colour(s) for confidence band. Can accept a single value
#'    for colour, or a vector of colour values.
#' @param conf.band.transparent A numeric values from 0 to 1. Controlling the
#'    transparency of the confidence band. Default: 0.25.
#' @param conf.type Transformation type for the confidence interval.
#' `'log'`, `'log-log'` (default), `'plain'`, `'logit'`, `'arcsin'`.
#' @param grid A logical parameter specifying whether to draw a grid.
#'    Default: \code{FALSE}.
#' @param col Colour(s) for the survival curves. Can accept a single value for
#'    colour, or a vector of colour values to set colour(s).
#' @param main Title of the plot.
#' @param sub Subtitle of the plot.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param xlab.pos Defines on which MARgin line the xlab is displayed. Starting at 0 counting outwards.
#' @param ylab.pos Defines on which MARgin line the ylab is displayed. Starting at 0 counting outwards.
#' @param cex A numeric value specifying all size of the text elements at once
#'    (labels, annotations, ...).
#' @param cex.lab A numeric value specifying the size of the `xlab` and `ylab` text.
#' @param cex.axis A numeric value specifying the size of the `axis` size.
#' @param bty Determines the style of the box drawn around the plot.
#'    Options: `'n'`,`'o'`,`'7'`,`'L'`,`'C'`,`'U'`.
#' @param lty A vector of string specifying line types for each curve.
#'    The length of the vector should match the number of survival curves,
#'    assigning a specific line type to each curve. in the plot.
#'    Options: `'blank'`, `'solid'`, `'dashed'`, `'dotted'`, `'dotdash'`,
#'    `'longdash'`, `'twodash'`.
#' @param lwd A numeric value specifying the width of the line.
#' @param xlim Limits for the x-axis. Specified as
#'    `seq(starting value, end value, number of increment of the sequence)`.
#' @param ylim Limits for the y-axis. Specified as
#'    `seq(starting value, end value, number of increment of the sequence)`.
#' @param show.legend A logical parameter specifying whether to display legend.
#'    Default: \code{TRUE}.
#' @param legend.position Position of the legend.
#'    Options: `c(x,y)`, `'bottomright'`, `'bottom'`, `'bottomleft'`, '`left`',
#'    '`topleft'`, `'top'`, `'topright'`, `'right'`, `'center'`.
#' @param legend.name Renaming the name(s) of the stratum.
#' @param legend.text.font Font style of the legend text.
#'    - `1` normal
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold and italic
#' @param legend.cex A numeric value specifying the size of the legend text.
#' @param legend.title Title of the legend.
#' @param legend.title.cex A numeric value specifying the size of the legend title.
#' @param segment.type A numeric value specifying the layout of the segment.
#'    - `1` (full width)
#'    - `2` (half width)
#'    - `3` (vertical and horizontal segment)
#' @param segment.timepoint A single value or a vector of fixed time points
#'    to be drawn as segment(s).
#' @param segment.quantile A single value or a vector of fixed quantile to be
#'    drawn as segment(s) e.g. 0.5 corresponds to median.
#' @param segment.col Colour for the segment.  Can accept a single value for colour.
#' @param segment.annotation.col Colour(s) for the segment annotation.
#'    Can accept a single value for colour, or a vector of colour values to
#'    set colour(s).
#' @param segment.lty A vector of string specifying line types for each curve.
#'    Options: `'blank'`, `'solid'`, `'dashed'`, `'dotted'`, `'dotdash'`,
#'    `'longdash'`, `'twodash'`.
#' @param segment.lwd A numeric value specifying the width of the segment line.
#' @param segment.cex A numeric value specifying the size of the segment text size.
#' @param segment.font A numeric value specifying the font face.
#'    - `1` plain
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold-italic
#' @param segment.main Title of segment text.
#' @param segment.main.font A numeric value specifying the font face.
#'    - `1` plain
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold-italic
#' @param segment.annotation Position of the segment annotation.
#'    Options: `c(x,y)`,`'bottomleft'`, `'left'`, `'right'`, `'none'`.
#' @param segment.annotation.space Spacing between the text in unit of x-coordinates.
#' @param stat  Statistics which is displayed in the plot.
#'    Options:
#'    - `'logrank'` gives the p value of the conducted logrank test using `survdiff{survival}`.
#'      To tests if there is a difference between two or more survival curves.
#'
#'    - `'coxph'`  gives the hazard ratio (HR) and its 95% CI of the conducted
#'      Cox proportional hazards regression using `coxph{survival}`.
#'
#'    - `'coxmodel'` gives `N` (number of observations), `Events` (Number of events),
#'      `HR`(hazard ratio), `lwrCI` (lower 95% confidence interval),
#'      `uprCI` (upper 95% confidence interval) and `Logrank` (p-value corresponding to the Chisquare statistic)
#'      of the conduct Cox proportional hazards regression using `summary(coxph{survival})`.
#'    - `'none'` no statistic is displayed (default).
#' @param stat.position Position where the stat should be displayed.
#'    Options: specify explicit by `c(x,y)`,`'bottomleft'`, `'left'`, `'right'`, `'topright'`,`'bottomright'`, `'none'`.
#' @param stat.col Colour of the `stat` text. Can accept a single value for colour.
#' @param stat.cex A numeric value specifying the size of the `stat` text size.
#' @param stat.font The font face.
#'    - `1` plain
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold-italic
#' @param risktable A logical parameter indicating whether to draw risk table. Default: \code{TRUE}.
#' @param risktable.pos Defines on which MARgin line the xlab is displayed. Starting at 0 counting outwards. Default at line 4.
#'    should be drawn if `risktable` is drawn. Default: 2.5 line distances form the axis elements.
#' @param margin.bottom Specifies the bottom margin of the plotting area in line units.
#' @param margin.left Specifies the left margin of the plotting area for the `risktable` in line units.
#' @param risktable.title Title of risk table.
#' @param risktable.title.font Title font of risk table.
#'    - `1` normal
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold and italic
#' @param risktable.title.col Colour for the risk table title. Can accept a single value for colour.
#' @param risktable.title.position A numeric value specifying the position of the title on the x-axis.
#' @param risktable.cex A numeric value specifying the size of the risk table text size.
#' @param risktable.title.cex A numeric value specifying the size of the risk table title size.
#' @param risktable.name.cex A numeric value specifying the size of the rsik table legend name size.
#' @param risktable.col Colour(s) for the risk table. Can accept a single value for colour, or a vector of colour values to set colour(s).
#' @param risktable.name.font legend name(s) font of risk table.
#'    - `1` normal
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold and italic
#' @param risktable.name.col Colour for the risk table name. Can accept a single value for colour.
#' @param risktable.name.position A numeric value specifying the position of the legend name(s) on the x-axis.
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
#'  veteran_fit_yr <- survfit(Surv(time / 365.25, status) ~ 1, data = veteran)
#'  veteran_trt_fit_mt <- survfit(Surv(time / 365.12 * 12, status) ~ trt, data = veteran)
#'
#' # Generate survival plots
#'  survSAKK::surv.plot(fit = veteran_fit_yr)
#'
#'  survSAKK::surv.plot(
#'  fit = veteran_trt_fit_mt,
#'  col = c("#5ab4ac","#d8b365"),
#'  cex = 0.8,
#'  segment.quantile = 0.5,
#'  stat = "coxph",
#'  risktable.col = c("#5ab4ac","#d8b365"))
#'
#' @references
#' \code{vignette("surv.plot", package = "survSAKK")}
#'
#' @import survival
#' @import graphics
#' @import stats
#' @importFrom grDevices adjustcolor
#'
#' @export



surv.plot <- function(
    fit,
    # Margin area
    margin.bottom = NULL,
    margin.left= NULL,
    # Censoring
    mark.censoring = TRUE,
    # Confidence Interval options
    conf.int = fit$conf.int,
    conf.band = TRUE,
    conf.line = FALSE,
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
    xlab.pos =1.5,
    ylab.pos = 2.5,
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
    segment.annotation.space = 0.06,
    segment.font = 1,
    segment.main.font = 1,
    # Stats options
    stat = "none",
    stat.position = "bottomleft",
    stat.col = "black",
    stat.cex = 1,
    stat.font = 1,
    # risk table options
    risktable = TRUE,
    #risktable.axislab.pos = 2.5,
    risktable.pos = 3,
    risktable.title = "Number at risk",
    risktable.title.font = 2,
    risktable.title.col = "black",
    risktable.title.position = par("usr")[1] - (par("usr")[2]- par("usr")[1])*0.15,
    risktable.cex = 1,
    risktable.title.cex = 1,
    risktable.name.cex = 1,
    risktable.col = "black",
    risktable.name.font = 1,
    risktable.name.col = "#666666",
    risktable.name.position = par("usr")[1] - (par("usr")[2]- par("usr")[1])*0.15
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

  ## Display confidence Line ####
  if(is.logical(conf.line)){
    if(conf.line == FALSE){
      lty <-  c("solid","blank","blank")
    } else if(conf.line == TRUE){
      lty <-  lty
    } else{
      stop("Error in confidence Line argument")
    }
  } else {
    stop("`conf.line` expecting TRUE or FALSE as an argument!")
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

  ## Extract Group (stratum) names for legend if not manually specified ####
  if (is.null(legend.name)){
    if(is.null(fit$strata)){
      group <- "Cohort"
      legend.name <- group
    } else {
      group <- names(fit$strata)
      legend.name <- sub(".*=","",group)
      #legend.name <- group
    }
  }

  ## Define Plotting area with and without risk table. ####
  if(is.logical(risktable)){
    if (risktable == TRUE){
      # Bottom margin
      if(is.null(margin.bottom)){
        bottom.lines = 5
      } else {
        bottom.lines = margin.bottom
      }
      # Left margin
      if(is.null(margin.left)){
        left.lines = 6.5
      } else {
        left.lines = margin.left
      }
      # Set up the plot with margin (ora) and outer margins (oma)
      # c(bottom, left, top, right)
      par(mar = c(stratum + bottom.lines, stratum + left.lines, 4, 2) + 0.1,
          # distance (lines) of axis elements from plot region
          # c(axis title, axis label, axis ticks)
          mgp = c(3, 1, 0)
      )
    } else {
      # Bottom margin
      if(is.null(margin.bottom)){
        bottom.lines = 5
      } else{
        bottom.lines = margin.bottom
      }
      # Left margin
      if(is.null(margin.left)){
        left.lines = 4
      } else {
        left.lines = margin.left
      }
      par(mar = c(bottom.lines, left.lines, 4, 2) + 0.1,  # c(bottom, left, top, right)
          mgp = c(3,1,0)              # c(axis title, axis label, axis ticks)
      )
    }
  } else{
    stop("`risktable` expecting TRUE or FALSE as an argument!")
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
    xlab = "",                            # Draw x label
    ylab = "",                            # Draw y label
    cex.lab = cex.lab                     # Label size
  )

  # xlab and ylab closer to axis line
  mtext(paste(xlab), side = 1, line = xlab.pos)
  mtext(paste(ylab), side = 2, line = ylab.pos)

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
  } else if (segment.annotation == "none"){
    text_ypos <- NULL
    text_xpos <- NULL
    pos <- 2
  } else {
    stop(paste0("'",segment.annotation,"'"," is not a valid argument!"))
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
  } else if (stat.position == "bottomright"){
    stat_ypos <- 0.03
    stat_xpos <- max(xlim)
    pos <- 2
  } else if (stat.position == "topright"){
    stat_ypos <- max(ylim) * 0.95 # marginal smaller than max(xlim) to ensure that the text is not cut off.
    stat_xpos <- max(xlim)
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
    if(stat.position == "right"){
      # table is always written from the specified x,y pos from left to right
      # therefore tables _right position is outside of the border.
      # And has to be corrected for tables.

      # Extract infos and create data frame from model
      tbl <- data.frame(N = model$n,
                        Events = model$nevent,
                        HR = round(model$conf.int[,"exp(coef)"], digits = 2),
                        lwrCI = round(model$conf.int[,"lower .95"], digits = 2),
                        uprCI = round(model$conf.int[,"upper .95"], digits = 2),
                        Logrank = logrankpval)
      # Annotation
      # plottbl() function was written to allow to plot different tables reproducible
      plottbl(x = stat_xpos * 0.25,
              y = stat_ypos,
              tbl,
              cex = stat.cex)
    } else if(stat.position == "bottomright"){
      # table is always written from the specified x,y pos from left to right
      # therefore tables _right position is outside of the border.
      # And has to be corrected for tables.

      # Extract infos and create data frame from model
      tbl <- data.frame(N = model$n,
                        Events = model$nevent,
                        HR = round(model$conf.int[,"exp(coef)"], digits = 2),
                        lwrCI = round(model$conf.int[,"lower .95"], digits = 2),
                        uprCI = round(model$conf.int[,"upper .95"], digits = 2),
                        Logrank = logrankpval)
      # Annotation
      # plottbl() function was written to allow to plot different tables reproducible
      plottbl(x = stat_xpos * 0.25, # 0.45
              y = stat_ypos * 0.85,
              tbl,
              cex = stat.cex)
    } else if(stat.position == "topright"){
      # table is always written from the specified x,y pos from left to right
      # therefore tables _right position is outside of the border.
      # And has to be corrected for tables.

      # Extract infos and create data frame from model
      tbl <- data.frame(N = model$n,
                        Events = model$nevent,
                        HR = round(model$conf.int[,"exp(coef)"], digits = 2),
                        lwrCI = round(model$conf.int[,"lower .95"], digits = 2),
                        uprCI = round(model$conf.int[,"upper .95"], digits = 2),
                        Logrank = logrankpval)
      # Annotation
      # plottbl() function was written to allow to plot different tables reproducible
      plottbl(x = stat_xpos * 0.25, # 0.45
              y = stat_ypos * 0.90,
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
      ## Extract risktable data ####
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

      ## Add risktable.title text to the outer margin ####
      mtext(risktable.title, side = 1, outer = FALSE,
            line = risktable.pos, adj = NA, at = risktable.title.position,
            font = risktable.title.font,
            cex = risktable.title.cex,
            col = risktable.title.col)

      ## Add legend text to the outer margin for each stratum ####
      for (i in 1:stratum){
        mtext(text = legend.name[i], side = 1, outer = FALSE,
              line = i+risktable.pos, adj = NA, at = risktable.name.position,
              font = risktable.name.font,
              cex = risktable.name.cex,
              col = risktable.name.col)
      }

      ## Add vector of risk counts text to the margin ####
      mtext(text = as.vector(n.risk.matrix), side = 1, outer = FALSE,
            line = rep((1:stratum) + risktable.pos, each = length(xlim)),
            at = rep(xlim, stratum),
            cex = risktable.cex,
            col = c(rep(risktable.col, each = length(xlim)))
      )
    }
  } else {
    stop("`risktable` expecting TRUE or FALSE as an argument!")
  }
} # final closer of the function
