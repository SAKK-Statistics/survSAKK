#' A package for publication ready Kaplan-Meier plots
#'
#' The survSAKK R package provides the function [`surv.plot()`] to perform Kaplan-Meier survival analysis.
#' This package is designed to be user-friendly and efficient, offering robust tool for
#' analysing survival data and generating Kaplan-Meier plot using the results
#' from [`survival::survfit()`].
#'
#'
#' @docType package
#'
#' @param fit An object of class [survival::survfit] containing survival data.
#' @param reference.arm String defining the reference arm (optional)
#' @param time.unit The time unit of the survival curve.
#'    Options:
#'    - `'day'`
#'    - `'week'`
#'    - `'month'`
#'    - `'year'`
#' @param y.unit Unit of the y-axis. Options: `'probability'`, `'percent'`
#' @param censoring.mark A logical parameter indicating whether to mark censoring
#'    events on the survival curves. Default: \code{TRUE}.
#' @param censoring.cex A numeric value specifying the size of the marks for
#'    censored patients. Default: 1.3.
#' @param conf.int A numeric value between 0 and 1 controlling the confidence interval on the survival curves.
#'    The default is 0.95 which corresponds to the 95% confidence interval.
#'    If set to 0, no confidence intervals are not displayed.
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
#' @param sub Subtitle of the plot. A subtitle only works if no risk table is displayed.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param xlab.pos Defines on which margin line the xlab is displayed. Starting at 0 counting outwards.
#' @param ylab.pos Defines on which margin line the ylab is displayed. Starting at 0 counting outwards.
#' @param xlab.cex A numeric value specifying the size of the X-axis label.
#' @param ylab.cex A numeric value specifying the size of the y-axis label.
#' @param cex A numeric value specifying all size of the text elements at once
#'    (labels, annotations, ...).
#' @param cex.axis A numeric value specifying the size of the `axis` size.
#' @param bty Determines the style of the box drawn around the plot.
#'    Options: `'n'` (default),`'o'`,`'c'`,`'u'`.
#' @param lty A vector with three arguments specifying line types for the curve
#'    and the lower and upper confidence lines
#'    Options for the three arguments: `'blank'`, `'solid'`, `'dashed'`, `'dotted'`, `'dotdash'`,
#'    `'longdash'`, `'twodash'`.
#'    E.g. `c('solid', 'dashed', 'dashed')`.
#' @param lwd A numeric value specifying the width of the line.
#' @param xticks A numeric vector specifying the ticks of the x-axis. Can be specified as
#'    `seq(starting value, end value, number: increment of the sequence)`.
#' @param yticks A numeric vector specifying the ticks of the y-axis. Can be specified as
#'    `seq(starting value, end value, number: increment of the sequence)`.
#'     It should always be specified as probability. For percent the parameter
#'     `y.unit` can be used.
#' @param show.legend A logical parameter specifying whether to display legend.
#'    By default the legend is displayed if there is more than one arm.
#' @param legend.position Position of the legend.
#'    Options: `c(x,y)`, `'bottomright'`, `'bottom'`, `'bottomleft'`, `'left'`,
#'    '`topleft'`, `'top'`, `'topright'`, `'right'`, `'center'`.
#' @param legend.name Renaming the name(s) of the arm.
#' @param legend.text.font Font style of the legend text.
#'    - `1` normal
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold and italic
#' @param legend.cex A numeric value specifying the size of the legend text.
#' @param legend.title Title of the legend.
#' @param legend.title.cex A numeric value specifying the size of the legend title.
#' @param segment.type A numeric value specifying the layout of the segment.
#'    - `1` full width
#'    - `2` half width
#'    - `3` vertical and horizontal segment (default)
#' @param segment.timepoint A single value or a vector of fixed time points
#'    to be drawn as segment(s).
#' @param segment.quantile A single value or a vector of fixed quantile to be
#'    drawn as segment(s) e.g. 0.5 corresponds to median.
#' @param segment.col Colour for the segment. Can accept a single value for colour.
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
#'    Options: `c(x,y)`,`'bottomleft'`, `'left'`, `'right'`, `'top'`, `'none'`.
#' @param segment.confint Display the confidence interval.
#'    `segment.confint` = `FALSE` is only possible if there are two arms.
#'    Default: \code{TRUE}.
#' @param segment.annotation.space Spacing between the text in unit of x-coordinates.
#' @param stat  Statistics which is displayed in the plot.
#'    Options:
#'    - `'logrank'` gives the p value of the conducted logrank test using `survdiff{survival}`.
#'      To tests if there is a difference between two or more survival curves.
#'
#'    - `'coxph'`  gives the hazard ratio (HR) and its CI (default: 95% CI) of the conducted
#'      Cox proportional hazards regression using `coxph{survival}`. This option
#'      only works if there are two arms.
#'
#'    - `'coxph_logrank'`  combines the hazard ratio (HR), its CI (default: 95% CI) and the
#'      logrank test. This option only works if there are two arms.
#'
#'    - `'coxmodel'` gives `N` (number of observations), `Events` (Number of events),
#'      `HR`(hazard ratio), `lwrCI` (lower 95% confidence interval),
#'      `uprCI` (upper 95% confidence interval) and `Logrank` (p-value corresponding to the Chisquare statistic)
#'      of the conduct Cox proportional hazards regression using `summary(coxph{survival})`.
#'    - `'none'` no statistic is displayed (default).
#' @param stat.position Position where the stat should be displayed.
#'    Options: specify explicit by `c(x,y)`,`'bottomleft'`, `'left'`, `'right'`,
#'    `'top'`, `'topright'`,`'bottomright'`, `'none'`.
#' @param stat.conf.int Controlling the confidence interval on the hazard ratio.
#'    If a numeric value between 0 and 1 is provided, it represents the desired
#'    coverage for the confidence interval (e.g. 0.9 for 90%).
#'    Default: 0.95
#' @param stat.fit An second object of class `survfit` which is used for
#'    calculation of statistics. This allows to add stratification factors.
#'    Optional parameter.
#' @param stat.col Colour of the `stat` text. Can accept a single value for colour.
#' @param stat.cex A numeric value specifying the size of the `stat` text size.
#' @param stat.font The font face.
#'    - `1` plain
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold-italic
#' @param risktable A logical parameter indicating whether to draw risk table. Default: \code{TRUE}.
#' @param risktable.pos Defines on which margin line the xlab is displayed. Starting at 0 counting outwards. Default at line 3.
#' @param margin.bottom Specifies the bottom margin of the plotting area in line units. Default: 5
#' @param margin.left Specifies the left margin of the plotting area in line units. Default: 6 (with risktable) or 4 (without risktable)
#' @param margin.top Specifies the top margin of the plotting area in line units. Default: 3
#' @param margin.right Specifies the right margin of the plotting area in line units. Default: 2
#' @param risktable.title Title of risk table.
#' @param risktable.title.font Title font of risk table.
#'    - `1` normal
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold and italic
#' @param risktable.title.col Colour for the risk table title. Can accept a single value for colour.
#' @param risktable.title.position A numeric value specifying the position of the title on the x-axis.
#' @param risktable.name Names of the arms for the risk table only.
#' @param risktable.cex A numeric value specifying the size of the risk table text size.
#' @param risktable.title.cex A numeric value specifying the size of the risk table title size.
#' @param risktable.name.cex A numeric value specifying the size of the risk table legend name size.
#' @param risktable.col Colour(s) for the risk table. Can accept a single value for colour, or a vector of colour values to set colour(s).
#'    If it is set to \code{TRUE} then the colors of the curves are used.
#'    Default: black
#' @param risktable.name.font legend name(s) font of risk table.
#'    - `1` normal
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold and italic
#' @param risktable.name.col Colour for the risk table name. Can accept a single value for colour.
#' @param risktable.name.position A numeric value specifying the position of the legend name(s) on the x-axis.
#' @return Publication-Ready Kaplan-Meier Plot incorporating various statistics and layout customisation options to enhance the efficiency and adaptability of the Kaplan-Meier plot.
#'
#' @export
#'
#' @seealso
#'  - https://sakk-statistics.github.io/survSAKK/
#'  - [survival::survfit()] which this function wraps.
#'
#' @examples
#'  require(survival)
#'  require(survSAKK)
#'
#' # Create survival object
#'  veteran_trt_fit_mt <- survfit(Surv(time / 365.12 * 12, status) ~ trt, data = veteran)
#'
#' # Generate survival plots
#'  surv.plot(fit = veteran_trt_fit_mt,
#'    time.unit = "month",
#'    reference.arm = 1,
#'    legend.name =  c("Standard", "Test"))
#'
#' @references
#' \code{vignette("surv.plot", package = "survSAKK")}
#'
#' @author Vithersan Somasundaram and Katrin Gysel
#'
#' @import survival
#' @import graphics
#' @import stats
#' @importFrom grDevices adjustcolor
#'

surv.plot <- function(
    fit,
    reference.arm,
    time.unit,
    y.unit = "probability",
    # Margin area
    margin.bottom = 5,
    margin.left= NULL,
    margin.top= 3,
    margin.right = 2,
    # Censoring
    censoring.mark = TRUE,
    censoring.cex = 1.3,
    # Confidence Interval options
    conf.int = 0.95,
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
    xlab = NULL,
    ylab = NULL,
    xlab.pos = 1.5,
    ylab.pos = 3,
    xlab.cex = NULL,
    ylab.cex = NULL,
    cex = NULL,
    cex.axis,
    bty = "n",
    lty = c("solid","dotted","dotted"),
    lwd = 3,
    xticks,
    yticks = seq(from = 0, to = 1, by = 0.25),
    # Legend options
    show.legend,
    legend.position = "topright",
    legend.name = NULL,
    legend.text.font = 1,
    legend.cex,
    legend.title = NULL,
    legend.title.cex,
    # Segment options
    segment.type = 3,
    segment.timepoint = NULL,
    segment.quantile = NULL,
    segment.main = NULL,
    segment.annotation = "right",
    segment.col = "#666666",
    segment.annotation.col = col,
    segment.lty = "dotted",
    segment.lwd = 1.3,
    segment.cex,
    segment.confint = TRUE,
    segment.annotation.space = 0.06,
    segment.font = 1,
    segment.main.font = 1,
    # Stats option s
    stat = "none",
    stat.position = "bottomleft",
    stat.conf.int = 0.95,
    stat.fit,
    stat.col = "black",
    stat.cex,
    stat.font = 1,
    # risk table options
    risktable = TRUE,
    risktable.pos = 2,
    risktable.title = "# at risk",
    risktable.title.font = 2,
    risktable.title.col = "black",
    risktable.title.position = par("usr")[1] - (par("usr")[2]- par("usr")[1])*0.15,
    risktable.name,
    risktable.cex,
    risktable.title.cex,
    risktable.name.cex,
    risktable.col = "black",
    risktable.name.font = 1,
    risktable.name.col = "black",
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
  if(is.null(cex)){cex <- 1}
  if(missing(cex.axis)){cex.axis <- cex}
  if(missing(legend.cex)){legend.cex <- cex}
  if(missing(legend.title.cex)){legend.title.cex <- cex}
  if(missing(segment.cex)){segment.cex <- cex}
  if(missing(stat.cex)){stat.cex <- cex}
  if(missing(risktable.cex)){risktable.cex <- cex}
  if(missing(risktable.title.cex)){risktable.title.cex <- cex}
  if(missing(risktable.name.cex)){risktable.name.cex <- cex}
  if(missing(xlab.cex)){xlab.cex <- cex}
  if(missing(ylab.cex)){ylab.cex <- cex}

  ## Display confidence Line ####
  if(is.logical(conf.line)){
    if(conf.line == FALSE){
      lty[c(2, 3)] <-  c("blank","blank")
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
                       yjust = 0,
                       # The amount of padding around text in the cells as a proportion
                       # of the maximum width and height of the strings in each column
                       xpad = 0.2,
                       ypad = 0.25,
                       text.col = stat.col,
                       text.font = stat.font,
                       text.pos = NULL)
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
           cex = cex, col = text.col, font = text.font, adj = 0.5, pos = text.pos)
      xleft <- xleft + cellwidth[column]
    }

    # Draw tables cells
    for (row in 1:tabdim[1]) {
      xleft <- x - xjust * (sum(cellwidth))
      for (column in 1:tabdim[2]) {
        text(xleft + 0.5 * cellwidth[column],
             ytop - (row + 0.5) * cellheight, table[row, column],
             cex = cex, col = text.col, font = text.font, adj = 0.5, pos = text.pos)
        xleft <- xleft + cellwidth[column]
      }
    }
  }


  ### Recalculate survival object ####
  # Note: Recalculation is done to be sure that the survival object is correct,
  # and for plotting with the desired CI, transformation and reference arm.

  # recalculate the fit object based on defined `conf.type`
  fit$call$conf.type <- conf.type
  # recalculate the fit object based on defined `conf.int`
  fit$call$conf.int <- conf.int
  # recalculate the fit object based on defined 'reference.arm'
  data <- as.data.frame(eval(fit$call$data))
  if(!missing(reference.arm)){
    arm.variable <- as.character(fit$call$formula[3])
    data[,arm.variable] <- relevel(as.factor(data[,arm.variable]), ref = reference.arm)
  }
  fit$call$data <- data
  fit <- eval(fit$call)


  ### Extract level of stratum ####
  stratum <- max(1, length(fit$strata))

  ## Define colour for KM-plot if not manually specified ####
  if (is.null(col)){
    if(is.null(fit$strata)){
      col <- "black"
    } else {
      for (i in 1:stratum){
        # Colors from RColorBrewer::display.brewer.pal(n = 10, name = "Paired") are used
        col[i] <- c("#1F78B4","#33A02C","#A6CEE3","#B2DF8A","#FB9A99","#E31A1C","#FDBF6F","#CAB2D6","#6A3D9A")[i]
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
  # Note: default is par(mar = c(5, 4, 4, 2)+0.1)
  if(is.logical(risktable)){
    if (risktable == TRUE){
      # Left margin
      if(is.null(margin.left)){
        margin.left = 6
      }
     # Set up the plot with margin (ora) and outer margins (oma)
      # c(bottom, left, top, right)
      par(mar = c(stratum + margin.bottom, margin.left, margin.top, margin.right) + 0.1,
          # distance (lines) of axis elements from plot region
          # c(axis title, axis label, axis ticks)
          mgp = c(3, 1, 0)
      )
    } else {
      # Left margin
      if(is.null(margin.left)){
        margin.left = 4
      }
      par(mar = c(margin.bottom, margin.left, margin.top, margin.right) + 0.1,  # c(bottom, left, top, right)
          mgp = c(3,1,0)              # c(axis title, axis label, axis ticks)
      )
    }
  } else{
    stop("`risktable` expecting TRUE or FALSE as an argument!")
  }

  # 2. SURV.PLOT ####

  # Customize the x ticks if they were not specified in the function call
  if(missing(xticks)){
    if(!missing(time.unit)){
      if(time.unit == "month"){
        xticks <- seq(from = 0, to = max(fit$time)+max(fit$time)/20, by = 6)    # choose increases by 6 if time unit is months
      }
      if(time.unit == "year"){
        xticks <- seq(from = 0, to = ceiling(max(fit$time)), by = 1)    # choose increases by 1 if time unit is years
      }
      }
    if(missing(xticks)){
      xticks <- seq(from = 0, to = max(fit$time)+ceiling(max(fit$time)/6), by = ceiling(max(fit$time)/6))
      }
  }

  # Customize the x-axis label if it was not specified in the function call
  if(is.null(xlab)){
    if(!missing(time.unit)){
      xlab <- paste0("Time (", time.unit, "s)")
    } else {
      xlab <- "Time"
    }
  }

  ## Main Plotting Function ####
  base::plot(
    # Plot the survival curve
    fit,
    conf.int = conf.int,
    conf.type = conf.type,
    main = main,
    sub = sub,
    col = col,
    lty = rep(lty, stratum),
    lwd = lwd,
    # Add censoring information with ticks
    mark.time = censoring.mark,
    mark = "/",
    cex = censoring.cex,                  # increase mark for censored patients.
    # Modify Layout
    xaxs = "i", yaxs = "i",               # Start axis exactly from zero origin
    xaxt = "n", yaxt = "n",               # Remove the original axes
    bty = bty,                            # Remove borders
    ylim = c(0,1),                        # Set y-axis limits
    xlim = range(xticks),                 # Set x-axis limits
    xlab = "",                            # Draw x label
    ylab = ""                             # Draw y label
  )

  if(is.null(ylab)){
    if(y.unit == "percent"){
      ylab <- "Estimated survival (%)"
      yticks.labels <- yticks*100
    } else {
      ylab = "Estimated survival probability"
      yticks.labels <- yticks
    }
  }

  # xlab and ylab closer to axis line
  mtext(paste(xlab), side = 1, line = xlab.pos, cex = xlab.cex)
  mtext(paste(ylab), side = 2, line = ylab.pos, cex = ylab.cex)

  # Customize the x coordinates
  graphics::axis(
    side = 1,                             # Specifies the side
    las = 0,                              # Rotate the labels
    mgp = c(3,0.50,0),                    # Adjust the label position (axis title, axis label, axis line)
    at = xticks,                          # Specify tick mark position
    labels = xticks,                      # Draw labels
    cex.axis = cex.axis                   # Axis size
  )

  # Customize the y coordinates
  graphics::axis(
    side = 2,                             # Specifies the side
    las = 1,                              # Rotate the labels
    mgp = c(3,0.75,0),                    # Adjust the label position (axis title, axis label, axis line)
    at = yticks,                          # Specify tick mark position
    labels = yticks.labels,               # Draw labels
    cex.axis = cex.axis                   # Axis size
  )

  ## Draw grid ####
  if (is.logical(grid)) {
    if (grid == TRUE) {
      grid(nx = length(xticks)-1, ny = length(yticks)-1)
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
  # By default the legend is displayed if there is more than one stratum
  if(missing(show.legend)){
    if(stratum == 1){
      show.legend <- FALSE
    } else {
      show.legend <- TRUE
    }
  }
  if (is.logical(show.legend)){
    if(show.legend == TRUE){
      graphics::legend(
        x = legend.position[1],   # the x coordinates to position the legend
        y = legend.position[2],   # the y coordinates to position the legend
        legend = legend.name ,    # the text of the legend
        bty = "n",                # boarder type for legend fixed as "none"
        col = col,
        lty = "solid",            # line type for legend fixed as "solid"
        lwd = lwd,
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
    pos = 1 # 4
  } else if (segment.annotation == "bottomleft") {
    text_ypos <- 0.05 #0.03
    text_xpos <- min(xticks)
    pos <- 4
  } else if (segment.annotation == "left"){
    text_ypos <- 0.53
    text_xpos <- min(xticks)
    pos <- 4
  } else if (segment.annotation == "right"){
    text_ypos <- 0.53
    text_xpos <- max(xticks)
    # Position the text to the left of the specified (x,y)
    pos <- 2
  } else if (segment.annotation == "top"){
    if(stratum == 2){
      text_ypos <- 0.85
    } else {
      text_ypos <- 0.9
    }
    text_xpos <- max(xticks)*0.5
    pos <- 1
  } else if (segment.annotation == "none"){
    text_ypos <- NULL
    text_xpos <- NULL
    pos <- 2
  } else {
    stop(paste0("'",segment.annotation,"'"," is not a valid argument!"))
  }

  ## Determining the y coordinate for each text ####
   if (stratum > 1 & (segment.confint == T)){
     text_ypos <- rep(text_ypos, stratum) + (stratum-1):0*segment.annotation.space
   }

  ## Prepare the label
  if (!is.null(segment.quantile) & is.null(segment.timepoint)){
    # Code for segment at a specific quantile
    segment_y <- segment.quantile
    segment_x <- quantile(fit,probs = 1 - segment_y)

    if(!missing(time.unit)){
      time.unit_temp <- paste0(" ", time.unit, "s")
    } else {
      time.unit_temp <- ""
    }

    if(segment.confint == F & stratum == 2){
      if(segment.quantile == 0.5) {quantile.temp <- "Median"}
      else {quantile.temp <- paste0(segment.quantile, "-Quantile")}
      quantile_label <- paste0(quantile.temp, ": ",
                               round(segment_x$quantile[1],digits = 1),
                               " vs ",
                               round(segment_x$quantile[2],digits = 1),
                               time.unit_temp)
      segment.annotation.col <- "black"
    } else {
      quantile_label <- paste0(round(segment_x$quantile,digits = 1),
                               time.unit_temp,
                               " [",
                               round(segment_x$lower,digits = 1),
                               ",",
                               round(segment_x$upper,digits = 1),
                               "]")
    }
  }


  if (is.null(segment.quantile) & !is.null(segment.timepoint)){
    # Code for segment at a specific time point
    segment_x <- segment.timepoint
    segment_y <- summary(fit,time = segment_x)

    if(segment.confint == F & stratum == 2){
      if(missing(time.unit)){time_temp <- paste0("time ", segment.timepoint)}
      else {time_temp <- paste0(segment.timepoint, " ", time.unit, "s")}

      if(y.unit == "percent"){
        timepoint_label <- paste0("Survival at ", time_temp, ": ", round(segment_y$surv[1], digits = 3)*100,
                                  "% vs ",
                                  round(segment_y$surv[2], digits = 3)*100, "%")
      } else {
        timepoint_label <- paste0("Survival at ", time_temp, ": ", round(segment_y$surv[1], digits = 2),
                                  " vs ",
                                  round(segment_y$surv[2], digits = 2))
      }
      segment.annotation.col <- "black"

      # if(y.unit == "percent"){
      #   timepoint_label <- paste0(round(segment_y$surv, digits = 3)*100, "%")
      # } else {
      #   timepoint_label <- paste0(round(segment_y$surv, digits = 2))
      # }

    } else {
    #   if(y.unit == "percent"){
    #     timepoint_label <- paste0(round(segment_y$surv, digits = 3)*100,
    #                               "% [",
    #                               round(segment_y$lower, digits = 3)*100,
    #                               "%,",
    #                               round(segment_y$upper, digits = 3)*100,
    #                               "%]")
    #   } else {
    #     timepoint_label <- paste0(round(segment_y$surv, digits = 2),
    #                               " [",
    #                               round(segment_y$lower, digits = 2),
    #                               ",",
    #                               round(segment_y$upper, digits = 2),
    #                               "]")
    #   }


      if(y.unit == "percent"){
        timepoint_label <- paste0(round(segment_y$surv, digits = 3)*100,
                                  "% (95% CI: ",
                                  round(segment_y$lower, digits = 3)*100,
                                  " to ",
                                  round(segment_y$upper, digits = 3)*100,
                                  ")")
      } else {
        timepoint_label <- paste0(round(segment_y$surv, digits = 2),
                                  " (95% CI: ",
                                  round(segment_y$lower, digits = 2),
                                  " to ",
                                  round(segment_y$upper, digits = 2),
                                  ")")
      }

    }
  }



  ## Main Segment Function ####
  if (segment.type == 3){
    ### Type 3: Drawing vertical and horizontal segments ####
    if (!is.null(segment.quantile) & is.null(segment.timepoint)){

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
             labels = quantile_label,
             pos = pos,
             col = segment.annotation.col,
             cex = segment.cex,
             font = segment.font)
      }
    } else if (is.null(segment.quantile) & !is.null(segment.timepoint)){

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
             labels = timepoint_label,
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
             labels = quantile_label,
             pos = pos,
             col = segment.annotation.col,
             cex = segment.cex,
             font = segment.font)
      }
    } else if (is.null(segment.quantile ) & !is.null(segment.timepoint)){

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
             labels = timepoint_label,
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

      # Draw horizontal Line
      segments(x0 = 0,
               y0 = segment_y,
               x1 = max(xticks),
               y1 = segment_y,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd )

      # Annotate the segment
      if (!("none" %in% segment.annotation)){
        text(x = text_xpos,
             y = text_ypos,
             labels = quantile_label,
             pos = pos,
             col = segment.annotation.col,
             cex = segment.cex,
             font = segment.font)
      }
    } else if (is.null(segment.quantile ) & !is.null(segment.timepoint)){

      # Draw vertical Line
      segments(x0 = segment_x,
               y0 = 0,
               x1 = segment_x,
               y1 = max(yticks),
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)

      # Annotate the segment
      if (!("none" %in% segment.annotation)){
        text(x = text_xpos,
             y = text_ypos,
             labels = timepoint_label,
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
    } else if (is.null(segment.main) & !is.null(segment.quantile) & (segment.confint == T | stratum !=2)){
      if (segment.quantile == 0.5){
        text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0("Median [", conf.int*100, "% CI]"), pos = pos,
             col = "black", cex = segment.cex, font = segment.main.font)
      } else {text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0(segment.quantile,"-Quantile [", conf.int*100, "% CI]"), pos = pos,
                   col = "black", cex = segment.cex, font = segment.main.font)
      }
    } else if (is.null(segment.main) & !is.null(segment.timepoint) & (segment.confint == T | stratum !=2)){
      if(!missing(time.unit)){
        time_point_temp <- paste0(" at ", segment.timepoint, " ", time.unit, "s")
      } else {
        time_point_temp <- paste0(" at time ", segment.timepoint)
      }
      # text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0(segment.quantile,"Survival", time_point_temp, " [", conf.int, "% CI]"), pos = pos,
      #      col = "black", cex = segment.cex, font = segment.main.font)
      text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0(segment.quantile,"Survival", time_point_temp), pos = pos,
           col = "black", cex = segment.cex, font = segment.main.font)
    }
  }

  # 4. SURV.STATS ####

  ## Stopp function if stat = coxph or coxph_logrank is chosen but number of arms is unequal 2
  if ((stat == "coxph" | stat == "coxph_logrank") & stratum != 2) {
    stop("It is not possible to set `stat` equal to `coxph` or`coxph_logrank`
          if number of arms is unequal 2. ")
  }

  ## Define different options for stat position ####
  if (length(stat.position) == 2){
    # If it's a numeric vector (x, y coordinates)
    stat_xpos <- stat.position[1]
    stat_ypos <- stat.position[2]
    # Position the text to the right of the specified (x,y)
    pos <- 1 #vorher 1
  } else if (stat.position == "bottomleft"){
    if(stat == "coxph_logrank") {
      stat_ypos <- 0.08
    } else {
      stat_ypos <- 0.05
    }
    stat_xpos <- min(xticks)
    pos <- 4
  } else if (stat.position == "left"){
    stat_ypos <- 0.53
    stat_xpos <- min(xticks)
    pos <- 4
  }else if (stat.position == "right"){
    stat_ypos <- 0.53
    stat_xpos <- max(xticks)
    # Position the text to the left of the specified (x,y)
    pos <- 2
  } else if (stat.position == "bottomright"){
    if(stat == "coxph_logrank") {
      stat_ypos <- 0.08
    } else {
      stat_ypos <- 0.05
    }
    stat_xpos <- max(xticks)
    pos <- 2
  } else if (stat.position == "top"){
    stat_ypos <- max(yticks) * 0.95
    stat_xpos <- max(xticks) * 0.5
    pos <- 1
  } else if (stat.position == "topright"){
    stat_ypos <- max(yticks) * 0.95 # marginal smaller than max(x.ticks) to ensure that the text is not cut off.
    stat_xpos <- max(xticks)
    pos <- 2
  }

  ## recalculate the stat.fit object based on defined 'reference.arm'
  if(!missing(reference.arm) & !missing(stat.fit)){
    data <- as.data.frame(eval(stat.fit$call$data))
    arm.variable <- as.character(fit$call$formula[3])
    data[,arm.variable] <- relevel(as.factor(data[,arm.variable]), ref = reference.arm)
    stat.fit$call$data <- data
    stat.fit <- eval(stat.fit$call)
  }

  ## Log rank test ####

  # To compare the survival curves of two or more groups
  if(missing(stat.fit)){
    logrank <- fit$call
  } else {
    logrank <- stat.fit$call
  }
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
  if(missing(stat.fit)){
    model <- fit$call
  } else {
    model <- stat.fit$call
  }
  model$conf.type <- NULL
  model$conf.int <- NULL
  model[1] <- call("coxph")
  model <- summary(eval(model), conf.int = stat.conf.int)

  ## Display statistics in the plot ####

  if(stat == "logrank"){
    stats <- paste0("Logrank test: ", logrankpval)
  } else if(stat == "coxph"){
    stats <- paste0("HR ",
                    round(model$conf.int[,"exp(coef)"], digits = 2),
                    " (", stat.conf.int*100, "% CI: ",
                    round(model$conf.int[3], digits = 2),
                    " to ",
                    round(model$conf.int[4], digits = 2),
                    ")")
  } else if(stat == "coxph_logrank"){
    stats <- paste0("HR ",
                    round(model$conf.int[,"exp(coef)"], digits = 2),
                    " (", stat.conf.int*100, "% CI: ",
                    round(model$conf.int[3], digits = 2),
                    " to ",
                    round(model$conf.int[4], digits = 2),
                    ")", "\n", "logrank test: ",
                    logrankpval)
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
      plottbl(x = stat_xpos * 0.6,
              y = stat_ypos,
              tbl,
              cex = stat.cex,
              text.col = stat.col)
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
      plottbl(x = stat_xpos * 0.6, # 0.7
              y = stat_ypos * 3,
              tbl,
              cex = stat.cex,
              text.col = stat.col)
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
      plottbl(x = stat_xpos * 0.6, # 0.7
              y = stat_ypos * 0.90,
              tbl,
              cex = stat.cex,
              text.col = stat.col)
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
      n.risk.matrix <- matrix(0,nrow = length(xticks), ncol = stratum)

      # Loop over each stratum and each time point defined by 'xticks'
      for (stratum_i in 1:stratum) {
        for (x in 1:length(xticks)) {
          # Find the indices where the survival time for the current group is greater than the current 'xticks'
          index <- which(fit$time[grp == stratum_i] > xticks[x])
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
            line = risktable.pos, adj = 0, at = risktable.title.position,
            font = risktable.title.font,
            cex = risktable.title.cex,
            col = risktable.title.col)

      ## Add legend text to the outer margin for each stratum ####
      if (missing(risktable.name)) {
        ristkable.name <- legend.name
      } else {
        ristkable.name <- risktable.name
      }
      if(stratum > 1){
        for (i in 1:stratum){
          mtext(text = ristkable.name[i], side = 1, outer = FALSE,
                line = i+risktable.pos, adj = 0, at = risktable.name.position,
                font = risktable.name.font,
                cex = risktable.name.cex,
                col = risktable.name.col)
        }
      }

      ## Add vector of risk counts text to the margin ####
      if(max(risktable.col == TRUE)) { risktable.col <- col}
      mtext(text = as.vector(n.risk.matrix), side = 1, outer = FALSE,
            line = rep((1:stratum) + risktable.pos, each = length(xticks)),
            at = rep(xticks, stratum),
            cex = risktable.cex,
            col = c(rep(risktable.col, each = length(xticks)))
      )
    }
  } else {
    stop("`risktable` expecting TRUE or FALSE as an argument!")
  }
} # final closer of the function
