#' Publication Ready Kaplan-Meier Estimator
#'
#' @description
#' `surv.plot(fit, ...)` returns Kaplan-Meier survival time curves.
#'
#' @details
#' The survSAKK R package provides the [`surv.plot()`] function, facilitating
#' Kaplan-Meier survival analysis. Designed with user-friendliness and efficiency
#' in mind. Offering robust tool for analysing survival data. It utilises the
#' functionalities of [`survival::survfit()`].
#'
#' For a comprehensive manual visit: \url{https://sakk-statistics.github.io/survSAKK/articles/surv.plot.html}
#'
#'
#' @param fit An object of class [survival::survfit] containing survival data.
#'
#' @param reference.arm A character string specifying the reference arm for comparison.
#'
#' @param time.unit A character string specifying the time unit which was used to create the `fit` object.
#' *Note:* `time.unit` will not convert the time of the `fit` object.
#'
#' Option include: `"day"`, `"week"`, `"month"`,`"year"`.
#'
#' @param y.unit A character string specifying the unit of the y-axis.
#'
#' Option include: `"probability"`, `"percent"`.
#'
#' @param censoring.mark A logical parameter indicating whether censoring events
#' should be marked on the survival curves. Default is \code{TRUE}.
#'
#' @param censoring.cex A numeric value specifying the size of the marks for
#' censored patients. Default is `1.3`.
#'
#' @param conf.int A numeric value controlling the confidence interval on survival curves.
#' Default is `0.95`, corresponding to a 95% confidence interval.
#' Values between `0` and `1` represent the desired confidence interval.
#' If set to `0`, no confidence intervals are displayed.
#'
#' @param conf.band A logical parameter indicating whether to display the
#' confidence band on the survival curves. Default is \code{TRUE}.
#'
#' @param conf.band.col A colour which is used for the confidence band.
#' Can accept a single colour value or a vector of colours.
#'
#' @param conf.band.transparent A numeric value between `0` and `1` controlling the
#' transparency of the confidence band. Default is `0.25`.
#'
#' @param conf.line.lty A strings specifying the line type of the confidence lines.
#'
#' Options include: `"blank"`, `"solid"`, `"dashed"`, `"dotted"`, `"dotdash"`,
#' `"longdash"`, `"twodash"`.
#' Default is `"blank"`.
#'
#' @param conf.line.lwd A numeric value specifying the width of the confidence lines.
#'
#' @param conf.type Transformation type for the confidence interval.
#'
#' Options include: `"log"`, `"log-log"`, `"plain"`, `"logit"`, `"arcsin"`.
#' Default is `log-log`.
#'
#' @param grid A logical parameter specifying whether to draw a grid.
#' Default is \code{FALSE}.
#'
#' @param col A colour which is used for the survival curves.
#' Can accept a single colour value  or a vector of colours.
#'
#' @param main Title of the plot.
#'
#' @param sub Subtitle of the plot.
#' Note: A subtitle is only displayed if no risk table is shown.
#'
#' @param xlab X-axis label.
#'
#' @param ylab Y-axis label.
#'
#' @param xticks A numeric vector specifying the ticks of the x-axis.
#'
#' Can be specified as `seq(from = , to = , by = )`.
#' - `from`: starting value
#' - `to`: end value
#' - `by`: number; increment of the sequence
#'
#' @param yticks A numeric vector specifying the ticks of the y-axis.
#'
#' Can be specified as `seq(from = , to = , by = )`.
#' - `from`: starting value
#' - `to`: end value
#' - `by`: number; increment of the sequence
#'
#' *Note*: It should always be specified as probability.
#'
#' @param xlab.pos Defines the margin line where the X-axis label (xlab) is displayed,
#' starting at 0 and counting outwards. Default is 1.5.
#'
#' @param ylab.pos Defines the margin line the Y-axis label (ylab) is displayed,
#' starting at 0 counting outwards. Default is 3.
#'
#' @param xlab.cex A numeric value specifying the size of the X-axis label.
#'
#' @param ylab.cex A numeric value specifying the size of the Y-axis label.
#'
#' @param cex A numeric value specifying the size of all all text elements
#' (labels, annotations, etc.).
#'
#' @param axis.cex A numeric value specifying the size of the axis elements.
#'
#' @param bty Determines the style of the box drawn around the plot.
#'
#' Options include: `"n"` ,`"o"`,`"c"`,`"u"`. Default is `"n"`.
#'
#' @param lty A string specifying the line type of of the curve(s).
#'
#' Options include: `"blank"`, `"solid"`, `"dashed"`, `"dotted"`, `"dotdash"`,
#' `"longdash"`, `"twodash"`.
#'
#' @param lwd A numeric value specifying the width of the line.
#'
#' @param legend A logical parameter specifying whether to display legend.
#' By default, the legend is displayed if there is more than one arm.
#'
#' @param legend.position Position of the legend.
#'
#' Options include: "c(x,y)"`, `"bottomright"`, `"bottom"`, `"bottomleft"`, `"left"`,
#" "`topleft"`, `"top"`, `"topright"`, `"right"`, `"center"`.
#'
#' @param legend.name A vector of character string specifying of the name(s) of the arm(s).
#'
#' @param legend.text.font Font style of the legend text.
#' Possible values:
#'    - `1` normal
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold and italic
#'
#' @param legend.cex A numeric value specifying the size of the legend text.
#'
#' @param legend.title Title of the legend.
#'
#' @param legend.title.cex A numeric value specifying the size of the legend title.
#'
#' @param segment.type A numeric value specifying the layout of the segment.
#' Possible values:
#'    - `1` full width
#'    - `2` half width
#'    - `3` vertical and horizontal segment (default)
#'
#' @param segment.timepoint A single value or a vector of fixed time points
#'    to be drawn as segment(s).
#'
#' @param segment.quantile A single value or a vector of fixed quantile to be
#'    drawn as segment(s). Example: 0.5 corresponds to median.
#'
#' @param segment.col A colour which is used for the segment.
#' Can accept a single colour value.
#'
#' @param segment.annotation.col A colour which is used for the segment annotation.
#' Can accept a single colour value or a vector of colours.
#'
#' @param segment.lty A strings specifying the line type of the segment(s).
#'
#' Options include: `"blank"`, `"solid"`, `"dashed"`, `"dotted"`, `"dotdash"`,
#' `"longdash"`, `"twodash"`.
#'
#' @param segment.lwd A numeric value specifying the width of the segment line.
#'
#' @param segment.cex A numeric value specifying the size of the segment text size.
#'
#' @param segment.font A numeric value specifying the font face.
#' Possible values:
#'    - `1` plain
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold-italic
#'
#' @param segment.main Title of the segment text.
#'
#' @param segment.main.font A numeric value specifying the font face for the segment text.
#' Possible values:
#'    - `1` plain
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold-italic
#'
#' @param segment.annotation Position of the segment annotation.
#'
#' Options include: `c(x,y)`,`"bottomleft"`, `"left"`, `"right"`, `"top"`, `"none"`.
#'
#' @param segment.confint A logical parameter specifying whether to display
#' the confidence interval for the segment.
#'
#'
#' *NOTE:* Only possible to set `segment.confint = FALSE` if there are two arms.
#' Default is \code{TRUE}.
#'
#' @param segment.annotation.space Spacing between the text in units of x-coordinates.
#'
#' @param stat.fit An object of class [survival::survfit] containing survival data.
#' Used for calculation of statistics, allowing to add stratification factors.
#'
#' *Note:* If not specified the `fit` object will be used for the `stat`.
#'
#' @param stat  Statistics displayed in the plot.
#'
#'    Options:
#'
#'    - `"logrank"` gives the p value of the conducted logrank test using `survdiff{survival}`.
#'      To tests if there is a difference between two or more survival curves.
#'
#'    - `"coxph"`  gives the hazard ratio (HR) and its CI (default: 95% CI)of the conducted
#'      Cox proportional hazards regression using `coxph{survival}`. *Note*: This option
#'      only works if there are two arms.
#'
#'    - `"coxph_logrank"` combines the hazard ratio (HR), its CI (default: 95% CI) and the
#'      logrank test. *Note:* This option only works if there are two arms.
#'
#'    - `'none'` no statistic is displayed (default).
#'
#'    Note: Confidence interval can be adjusted with the argument `stat.conf.int`.
#'
#' @param stat.position Position where the `stat` should be displayed.
#'
#' Options include: `c(x,y)`,`"bottomleft"`, `"left"`, `"right"`,
#' `"top"`, `"topright"`,`"bottomright"`, `"none"`.
#'
#' @param stat.conf.int A numeric value controlling the confidence interval on
#' the `stat` (hazard ratio). Default is `0.95`, corresponds to a 95% confidence interval.
#' Values between `0` and `1` represent the desired confidence interval.
#'
#' @param stat.col A colour which is used for the statistics text.
#' Can accept a single colour value or a vector of colours.
#'
#' @param stat.cex A numeric value specifying the size of the `statistics text size.
#'
#' @param stat.font The font face of the statistics
#' Possible values:
#'
#'    - `1` plain
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold-italic
#'
#' @param risktable A logical parameter indicating whether to draw risk table.
#' Default is \code{TRUE}.
#'
#' @param risktable.pos Defines on which margin line of the xlab is displayed,
#' starting at 0 counting outwards.
#' Default is at line `3`.
#'
#' @param risktable.name Names of the arms for the risk table.
#'
#' @param risktable.cex A numeric value specifying the size of the risk table text size.
#'
#' @param risktable.col A coulour which is used for the risk table.
#' Can accept a single colour value or a vector of colours.
#' Default is `black`.
#'
#' *Note:* If `risktable.col = TRUE` then the colours of the curves are used.
#'
#' @param risktable.title.font Font style of the risk table.
#' Possible values:
#'
#'    - `1` normal
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold and italic
#'
#' @param risktable.title Title of risk table.
#'
#' @param risktable.title.col A colour which is used for the risk table title.
#' Can accept a single colour value.
#'
#' @param risktable.title.position A numeric value specifying the position of the title on the x-axis.
#'
#' @param risktable.title.cex A numeric value specifying the size of the risk table title size.
#'
#' @param risktable.name.cex A numeric value specifying the size of the risk table legend name size.
#'
#' @param risktable.name.font Font style of the risk table legend name(s).
#' Possible values:
#'
#'    - `1` normal
#'    - `2` bold
#'    - `3` italic
#'    - `4` bold and italic
#'
#' @param risktable.name.col A colour which is used for the risk table name.
#' Can accept a single colour value.
#'
#' @param risktable.name.position A numeric value specifying the position of the legend name(s) on the x-axis.
#'
#' @param margin.bottom Specifies the bottom margin of the plotting area in line units.
#' Default is `5`.
#'
#' @param margin.left Specifies the left margin of the plotting area in line units.
#' Default is `6` (with risktable) or `4` (without risktable).
#'
#' @param margin.top Specifies the top margin of the plotting area in line units.
#' Default is `3`.
#'
#' @param margin.right Specifies the right margin of the plotting area in line units.
#' Default is `2`.
#'
#' @return Kaplan-Meier curves of the input \code{fit},
#' incorporating various statistics and layout option(s).
#'
#' @export
#'
#' @usage NULL
#'
#' @examples
#'  require(survival)
#'  require(survSAKK)
#'
#' # Create survival object
#'  fit <- survfit(Surv(lung$time/365.25*12, status) ~ sex, data = lung)
#'
#' # Generate survival plots
#'  surv.plot(fit = fit,
#'    time.unit = "month",
#'    legend.name =  c("male", "female"))
#'
#' @seealso
#'  - [survival::survfit()] which this function wraps.
#' @references
#' Therneau T (2024). A Package for Survival Analysis in R. R package version 3.5-8, \url{https://CRAN.R-project.org/package=survival}.
#'
#' Terry M. Therneau, Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model. Springer, New York. ISBN 0-387-98784-3.
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
    # Censoring
    censoring.mark = TRUE,
    censoring.cex = 1.3,
    # Confidence Interval options
    conf.int = 0.95,
    conf.band = TRUE,
    conf.band.col = col,
    conf.band.transparent = 0.25,
    conf.line.lty = "blank",
    conf.line.lwd = 1,
    conf.type = "log-log",
    # Layout options
    grid = FALSE,
    col = NULL,
    main = NULL,
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    xticks,
    yticks = seq(from = 0, to = 1, by = 0.25),
    xlab.pos = 1.5,
    ylab.pos = 3,
    xlab.cex = NULL,
    ylab.cex = NULL,
    cex = NULL,
    axis.cex,
    bty = "n",
    lty = "solid",
    lwd = 3,
    # Legend options
    legend,
    legend.position = "topright",
    legend.name = NULL,
    legend.cex,
    legend.text.font = 1,
    legend.title = NULL,
    legend.title.cex,
    # Segment options
    segment.type = 3,
    segment.timepoint = NULL,
    segment.quantile = NULL,
    segment.main = NULL,
    segment.confint = TRUE,
    segment.annotation = "right",
    segment.annotation.col = col,
    segment.annotation.space = 0.06,
    segment.col = "#666666",
    segment.lty = "dotted",
    segment.lwd = 1.3,
    segment.cex,
    segment.font = 1,
    segment.main.font = 1,
    # Stats options
    stat.fit,
    stat = "none",
    stat.position = "bottomleft",
    stat.conf.int = 0.95,
    stat.col = "black",
    stat.cex,
    stat.font = 1,
    # risk table options
    risktable = TRUE,
    risktable.pos = 2,
    risktable.name,
    risktable.cex,
    risktable.col = "black",
    risktable.title = "# at risk",
    risktable.title.font = 2,
    risktable.title.col = "black",
    risktable.title.position = par("usr")[1] - (par("usr")[2]- par("usr")[1])*0.15,
    risktable.title.cex,
    risktable.name.cex,
    risktable.name.font = 1,
    risktable.name.col = "black",
    risktable.name.position = par("usr")[1] - (par("usr")[2]- par("usr")[1])*0.15,
    # Margin area
    margin.bottom = 5,
    margin.left= NULL,
    margin.top= 3,
    margin.right = 2
){
  #----------------------------------------------------------------------------#
  # 1. Preparation ####
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  ## 1.1 Function for rounding p-value ####
  #----------------------------------------------------------------------------#
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

  #----------------------------------------------------------------------------#
  ## 1.2 Global font parameter  ####
  #----------------------------------------------------------------------------#

  if(is.null(cex)){cex <- 1}
  if(missing(axis.cex)){axis.cex <- cex}
  if(missing(legend.cex)){legend.cex <- cex}
  if(missing(legend.title.cex)){legend.title.cex <- cex}
  if(missing(segment.cex)){segment.cex <- cex}
  if(missing(stat.cex)){stat.cex <- cex}
  if(missing(risktable.cex)){risktable.cex <- cex}
  if(missing(risktable.title.cex)){risktable.title.cex <- cex}
  if(missing(risktable.name.cex)){risktable.name.cex <- cex}
  if(missing(xlab.cex)){xlab.cex <- cex}
  if(missing(ylab.cex)){ylab.cex <- cex}

  #----------------------------------------------------------------------------#
  ## 1.4 Function to draw table into plot ####
  #----------------------------------------------------------------------------#
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

  #----------------------------------------------------------------------------#
  ## 1.5 Recalculating survival object ####
  #----------------------------------------------------------------------------#
  # Note: Recalculation is done to be sure that the survival object is correct,
  # and for plotting with the desired CI, transformation and reference arm.

  # recalculate the fit object based on defined `conf.type`
  fit$call$conf.type <- conf.type

  # recalculate the fit object based on defined `conf.int`
  fit$call$conf.int <- conf.int

  # recalculate the fit object based on defined `reference.arm`
  data <- as.data.frame(eval(fit$call$data))
  if(!missing(reference.arm)){
    arm.variable <- as.character(fit$call$formula[3])
    data[,arm.variable] <- relevel(as.factor(data[,arm.variable]), ref = reference.arm)
  }
  fit$call$data <- data
  fit <- eval(fit$call)

  #----------------------------------------------------------------------------#
  ## 1.6 Extraction number of arms ####
  #----------------------------------------------------------------------------#
  arm_no <- max(1, length(fit$strata))

  #----------------------------------------------------------------------------#
  ## 1.7 Define default colour(s) ####
  #----------------------------------------------------------------------------#
  if (is.null(col)){
    if(is.null(fit$strata)){
      col <- "black"
    } else {
      for (i in 1:arm_no){
        # RColorBrewer::display.brewer.pal(n = 10, name = "Paired")
        col[i] <- c("#1F78B4","#4DAF4A","#A6CEE3","#B2DF8A","#FB9A99","#E31A1C","#FDBF6F","#CAB2D6","#6A3D9A")[i]
      }
    }
  }

  #----------------------------------------------------------------------------#
  ## 1.8 Extract group (arm) names ####
  #----------------------------------------------------------------------------#
  # Extract group names if not manually specified for legend

  if (is.null(legend.name)){
    if(is.null(fit$strata)){
      group <- "Cohort"
      legend.name <- group
    } else {
      group <- names(fit$strata)
      legend.name <- substring(group, unlist(gregexpr('=', group))[1]+1, nchar(group))
      legend.name <- gsub(pattern = ">=" , replacement = "\u2265" , x = legend.name)
      legend.name <- gsub(pattern = "<=" , replacement = "\u2264" , x = legend.name)
      #legend.name <- group
    }
  }

  #----------------------------------------------------------------------------#
  ## 1.9 Plotting area with and without risktable ####
  #----------------------------------------------------------------------------#
  # Note: default is par(mar = c(5, 4, 4, 2)+0.1)

  if(is.logical(risktable)){
    if (risktable == TRUE){
      # Left margin
      if(is.null(margin.left)){
        margin.left = 6
      }
     # Set up the plot with margin (ora) and outer margins (oma)
      # c(bottom, left, top, right)
      par(mar = c(arm_no + margin.bottom, margin.left, margin.top, margin.right) + 0.1,
          # distance (lines) of axis elements from plot region
          # c(axis title, axis label, axis ticks)
          mgp = c(3, 1, 0)
      )
    } else {
      # Left margin
      if(is.null(margin.left)){
        margin.left = 4
      }
      par(mar = c(margin.bottom, margin.left, margin.top, margin.right) + 0.1,
          mgp = c(3,1,0) #c(axis title, axis label, axis ticks)
      )
    }
  } else{
    stop("`risktable` expecting TRUE or FALSE as an argument!")
  }

  #----------------------------------------------------------------------------#
  ## 1.10 Customization of xticks ####
  #----------------------------------------------------------------------------#
  # Customize the xticks if not manually specified

  if(missing(xticks)){
    if(!missing(time.unit)){
      # Check if proper option is provided
      if (!(time.unit %in% c("day", "week", "month", "year"))) {
        stop("Undefined parameter provided for argument: `time.unit`")
      } else {
        if(time.unit == "month"){
          # month: xticks by 6 unit
          xticks <- seq(from = 0, to = max(fit$time)+6, by = 6)
        }
        if(time.unit == "year"){
          # year: xticks by 1 unit
          xticks <- seq(from = 0, to = ceiling(max(fit$time)), by = 1)
        }
        if(time.unit %in% c("day", "week")){
          xticks <- seq(from = 0, to = max(fit$time)+ceiling(max(fit$time)/6),
                        by = ceiling(max(fit$time)/6))
        }
      }
    } else {
      xticks <- seq(from = 0, to = max(fit$time)+ceiling(max(fit$time)/6),
                    by = ceiling(max(fit$time)/6))
    }
  }

  #----------------------------------------------------------------------------#
  # 2. survPlot ####
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  ## 2.1 Plotting Function (main) ####
  #----------------------------------------------------------------------------#

  # Combind lty and lwd of main curve and confidence lines
  lty.main <- c(lty, rep(conf.line.lty, 2))
  lwd.main <- c(lwd, rep(conf.line.lwd, 2))

  base::plot(
    # Plot the survival curve
    fit,
    conf.int = conf.int,
    conf.type = conf.type,
    main = main,
    sub = sub,
    col = col,
    lty = rep(lty.main, arm_no),
    lwd = rep(lwd.main, arm_no),
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

  #----------------------------------------------------------------------------#
  ## 2.2 Customization of xlab and ylab ####
  #----------------------------------------------------------------------------#

  # Customize the xlab if not manually specified
  if(is.null(xlab)){
    if(!missing(time.unit)){
      xlab <- paste0("Time (", time.unit, "s)")
    } else {
      xlab <- "Time"
    }
  }

  if (y.unit %in% c("percent", "probability")){
    if (y.unit == "percent"){
      if(is.null(ylab)){
        ylab <- "Estimated survival (%)"
      }
      yticks.labels <- yticks*100
    }
    if (y.unit == "probability"){
      if(is.null(ylab)){
        ylab <- "Estimated survival probability"
      }
      yticks.labels <- yticks
    }
  } else {
    stop(paste0("'",y.unit,"'"," is not a valid argument for `y.unit`!"))
  }

  # xlab closer to axis line
  mtext(paste(xlab), side = 1, line = xlab.pos, cex = xlab.cex)
  # ylab closer to axis line
  mtext(paste(ylab), side = 2, line = ylab.pos, cex = ylab.cex)

  #----------------------------------------------------------------------------#
  ## 2.3 Customization of coordinates ####
  #----------------------------------------------------------------------------#

  # Customize the x coordinates
  graphics::axis(
    side = 1,                             # Specifies the side
    las = 0,                              # Rotate the labels
    mgp = c(3,0.50,0),                    # Adjust the label position (axis title, axis label, axis line)
    at = xticks,                          # Specify tick mark position
    labels = xticks,                      # Draw labels
    cex.axis = axis.cex                   # Axis size
  )

  # Customize the y coordinates
  graphics::axis(
    side = 2,                             # Specifies the side
    las = 1,                              # Rotate the labels
    mgp = c(3,0.75,0),                    # Adjust the label position (axis title, axis label, axis line)
    at = yticks,                          # Specify tick mark position
    labels = yticks.labels,               # Draw labels
    cex.axis = axis.cex                   # Axis size
  )
  #----------------------------------------------------------------------------#
  ## 2.4 Draw grid ####
  #----------------------------------------------------------------------------#

  if (is.logical(grid)) {
    if (grid == TRUE) {
      grid(nx = length(xticks)-1, ny = length(yticks)-1)
    }
  } else {
    stop("`gird` expecting TRUE or FALSE as an argument!")
  }

  #----------------------------------------------------------------------------#
  ## 2.5 Confidence band ####
  #----------------------------------------------------------------------------#

  if(conf.band == TRUE){
    mapping <- 0
    # Loop for drawing polygons
    for (i in 1:arm_no) {
      if(arm_no >1){
        mapping[length(mapping)+1] <- mapping[i]+fit$strata[i]
      }
      if(arm_no == 1){
        mapping[length(mapping)+1] <- length(fit$time)
      }
      if(sum(!is.na(fit$lower[(mapping[i]+1):mapping[i+1]]))>1) {
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
        y_coordinates[is.na(y_coordinates)] <- min(lower,na.rm = T)      # todo: I'm not sure if this line is really correct. Should it not be: y_coordinates[is.na(y_coordinates)] <- 0 ..?

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
  }

  #----------------------------------------------------------------------------#
  ## 2.6 Add legend to plot  ####
  #----------------------------------------------------------------------------#
  # Note: by default the legend is displayed if there is more than one arm

  if(missing(legend)){
    if(arm_no == 1){
      legend <- FALSE
    } else {
      legend <- TRUE
    }
  }

  if (is.logical(legend)){
    if(legend == TRUE){
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
    stop("`legend` expecting TRUE or FAlSE as an argument!")
  }

  #----------------------------------------------------------------------------#
  # 3. survSegment ####
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  ## 3.1 Segment annotation ####
  #----------------------------------------------------------------------------#
  #----------------------------------------------------------------------------#
  ### 3.1.1 Set segment text location by `segment.annotation` ####
  #----------------------------------------------------------------------------#

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
    if(arm_no == 2){
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

  # Determining the y coordinate for the text of each arm
   if (arm_no > 1 & (segment.confint == T)){
     text_ypos <- rep(text_ypos, arm_no) + (arm_no-1):0*segment.annotation.space
   }

  #----------------------------------------------------------------------------#
  ### 3.1.2 Preparation of the label ####
  #----------------------------------------------------------------------------#

  if(!is.null(segment.quantile) & is.null(segment.timepoint)){

    # Code for segment at a specific quantile
    segment_y <- segment.quantile
    segment_x <- quantile(fit,probs = 1 - segment_y)

    # Adjusting label of time.unit with sufix: s
    if(!missing(time.unit)){
      time.unit_temp <- paste0(" ", time.unit, "s")
    } else {
      time.unit_temp <- ""
    }

    # Short annotation without confidence interval (only possible if number of arms = 2)
    if(segment.confint == F & arm_no == 2){
      if(segment.quantile == 0.5) {quantile.temp <- "Median"}
      else {quantile.temp <- paste0(segment.quantile, "-Quantile")}
      quantile_label <- paste0(quantile.temp, ": ",
                               round(segment_x$quantile[1],digits = 1),
                               " vs ",
                               round(segment_x$quantile[2],digits = 1),
                               time.unit_temp)
      segment.annotation.col <- "black"

    # Error message if no confidence interval should be displayed but number of arms is not equal to 2
    } else if(segment.confint == F & arm_no != 2) {
      stop("The parameter `segment.confint` cannot be set to FALSE when number of arms is unequal 2.")

    # Long annotation with confidence interval
    } else {
      quantile_label <- paste0(round(segment_x$quantile,digits = 1),
                               time.unit_temp,
                               " (",
                               round(segment_x$lower,digits = 1),
                               " to ",
                               round(segment_x$upper,digits = 1),
                               ")")
    }
  }

  if(is.null(segment.quantile) & !is.null(segment.timepoint)){

    # Code for segment at a specific time point
    segment_x <- segment.timepoint
    segment_y <- summary(fit,time = segment_x)

    # Short annotation without confidence interval (only possible if number of arms = 2)
    if(segment.confint == F & arm_no == 2){
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

    # Error message if no confidence interval should be displayed but number of arms is not equal to 2
    } else if(segment.confint == F & arm_no != 2) {
      stop("The parameter `segment.confint` cannot be set to FALSE when number of arms is unequal 2.")

    # Long annotation with confidence interval
    } else {
       if(y.unit == "percent"){
        timepoint_label <- paste0(round(segment_y$surv, digits = 3)*100,
                                  "% (", conf.int * 100, "% CI: ",
                                  round(segment_y$lower, digits = 3)*100,
                                  " to ",
                                  round(segment_y$upper, digits = 3)*100,
                                  ")")
      } else {
        timepoint_label <- paste0(round(segment_y$surv, digits = 2),
                                  " (", conf.int * 100, "% CI: ",
                                  round(segment_y$lower, digits = 2),
                                  " to ",
                                  round(segment_y$upper, digits = 2),
                                  ")")
      }

    }
  }

  if(!is.null(segment.quantile) & !is.null(segment.timepoint)){
    stop(paste0("The parameters `segment.quantile` and `segment.timepoint` cannot be used simultaneously"))
  }

  #----------------------------------------------------------------------------#
  ## 3.2 Segment Function (main) ####
  #----------------------------------------------------------------------------#
  # Drawing segments with different options

  if (segment.type == 3){

    ### 3.2.1 Drawing vertical and horizontal segments ####
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

    } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
      stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options.")
    }

  } else if (segment.type == 2){

    ### 3.2.2 Drawing specified segment (half bandwidth) ####
    if (!is.null(segment.quantile ) & is.null(segment.timepoint)){

      # Horizontal Line
      segments(x0 = 0,
               y0 = segment_y,
               x1 = segment_x$quantile,
               y1 = segment_y,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)

    } else if (is.null(segment.quantile ) & !is.null(segment.timepoint)){

      # Vertical Line
      segments(x0 = segment_x,
               y0 = 0,
               x1 = segment_x,
               y1 = segment_y$surv,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)

    } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
      stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options.")
    }

  } else if (segment.type == 1){

    ### 3.2.3 Drawing specified segment (full bandwidth) ####
    if (!is.null(segment.quantile ) & is.null(segment.timepoint)){

      # Draw horizontal Line
      segments(x0 = 0,
               y0 = segment_y,
               x1 = max(xticks),
               y1 = segment_y,
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd )

    } else if (is.null(segment.quantile ) & !is.null(segment.timepoint)){

      # Draw vertical Line
      segments(x0 = segment_x,
               y0 = 0,
               x1 = segment_x,
               y1 = max(yticks),
               col = segment.col,
               lty = segment.lty,
               lwd = segment.lwd)
    } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
      stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options. ")
    }
  } else {
    stop(paste0("`segment.type`", " = ", segment.type, " is no valid option."))
  }

  ### 3.2.4 Add segment annotation ####

  # Segment annotation is displayed if segment.quantile or segment.timepoint was chosen (and not
  # both of them) and segment.annotation is not "none".
  if (segment.type %in% c(1,2,3) & !("none" %in% segment.annotation) &
      (!is.null(segment.quantile ) & is.null(segment.timepoint) |
       is.null(segment.quantile ) & !is.null(segment.timepoint))) {

    # Long annotation with confidence interval
    #if(segment.confint == F & arm_no == 2) {
      if (is.null(segment.timepoint)) {
        segment_label <- quantile_label
      } else if (is.null(segment.quantile)){
        segment_label <- timepoint_label
      }
      text(x = text_xpos,
           y = text_ypos,
           labels = segment_label,
           pos = pos,
           col = segment.annotation.col,
           cex = segment.cex,
           font = segment.font)

    # Error message if no confidence interval should be displayed but number of arms is not equal to 2
    #}} else if(segment.confint == F & arm_no != 2) {
    #  stop("The parameter `segment.confint` cannot be set to FALSE when number of arms is unequal 2.")

    # Short annotation without confidence interval
    #} else {
    #}


  }

  #----------------------------------------------------------------------------#
  ### 3.3 Segment title ####
  #----------------------------------------------------------------------------#
  # Title for the segment text

  if (!("none" %in% segment.annotation)){
    # Display `setment.main` as title of segment annotation if it was specified
    if (!is.null(segment.main)){
      text(text_xpos, max(text_ypos) + segment.annotation.space, label = segment.main, pos = pos,
           col = "black", cex = segment.cex, font = segment.main.font)
    # Display corresponding title if `segment.quantile` was specified and confidence interval is displayed
    } else if (!is.null(segment.quantile) & (segment.confint == T | arm_no !=2)){
      # For median
      if (segment.quantile == 0.5){
        text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0("Median (", conf.int * 100, "% CI)"), pos = pos,
             col = "black", cex = segment.cex, font = segment.main.font)
      # For other quantiles
      } else {text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0(segment.quantile,"-Quantile (", conf.int * 100, "% CI)"), pos = pos,
                   col = "black", cex = segment.cex, font = segment.main.font)
      }
    # Display corresponding title if `segment.timepoint` was specified and confidence interval is displayed
    } else if (!is.null(segment.timepoint) & (segment.confint == T | arm_no !=2)){
      # If time unit was specified
      if(!missing(time.unit)){
        time_point_temp <- paste0(" at ", segment.timepoint, " ", time.unit, "s")
      } else {
        time_point_temp <- paste0(" at time ", segment.timepoint)
      }
      text(text_xpos, max(text_ypos) + segment.annotation.space, label = paste0(segment.quantile,"Survival", time_point_temp), pos = pos,
           col = "black", cex = segment.cex, font = segment.main.font)
    }
  }

  #----------------------------------------------------------------------------#
  # 4. survStats ####
  #----------------------------------------------------------------------------#

  # Stop function if stat = coxph or coxph_logrank is chosen
  # but number of arms is unequal 2
  if ((stat == "coxph" | stat == "coxph_logrank") & arm_no != 2) {
    stop("It is not possible to set `stat` equal to `coxph` or`coxph_logrank`
          if number of arms is unequal 2.")
  }

  #----------------------------------------------------------------------------#
  ## 4.1 Stat position ####
  #----------------------------------------------------------------------------#
  # Define different options for stat position

  if (length(stat.position) == 2){
    # If it's a numeric vector (x, y coordinates)
    stat_xpos <- stat.position[1]
    stat_ypos <- stat.position[2]
    # Position the text to the right of the specified (x,y)
    pos <- 1
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
  } else if (stat.position == "right"){
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

  #----------------------------------------------------------------------------#
  ## 4.2 Recalculate Stat ####
  #----------------------------------------------------------------------------#
  # Recalculate the stat.fit object based on defined 'reference.arm'

  if(!missing(reference.arm) & !missing(stat.fit)){
    data <- as.data.frame(eval(stat.fit$call$data))
    arm.variable <- as.character(fit$call$formula[3])
    data[,arm.variable] <- relevel(as.factor(data[,arm.variable]), ref = reference.arm)
    stat.fit$call$data <- data
    stat.fit <- eval(stat.fit$call)
  }

  #----------------------------------------------------------------------------#
  ### 4.2.1 Log rank test ####
  #----------------------------------------------------------------------------#

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
  # VSO (09.04.2024) Wird schon im 4.) kontrolliert, sollten wir das streichen.
  if(is.null(fit$strata)){
    logrank <- NULL
  } else {
    logrank <- eval(logrank)
    # Recalculating p-Value
    logrankpval <- as.numeric(format.pval(1 - pchisq(logrank$chisq, df = length(logrank$n) - 1), esp = 0.001))
    logrankpval <- round.pval(logrankpval)
  }

  #----------------------------------------------------------------------------#
  ### 4.2.2 Cox regression ####
  #----------------------------------------------------------------------------#
  # To describe the effect of variables on survival

  # To compare the survival curves of two or more groups
  if(missing(stat.fit)){
    model <- fit$call
  } else {
    model <- stat.fit$call
  }
  model$conf.type <- NULL
  model$conf.int <- NULL
  model[1] <- call("coxph")
  model <- summary(eval(model), conf.int = stat.conf.int)

  #----------------------------------------------------------------------------#
  ## 4.3 Stat Function (main) ####
  #----------------------------------------------------------------------------#
  # Display statistics in the plot

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
  }
  if (stat != "none"){
    # Annotate the stats in the plot when stat = "coxph, loglik etc.
    text(x = stat_xpos,
         y = stat_ypos,
         labels = stats,
         pos = pos,
         col = stat.col,
         cex = stat.cex,
         font = stat.font)
  }

  #----------------------------------------------------------------------------#
  # 5. survRisktable ####
  #----------------------------------------------------------------------------#

  if(is.logical(risktable)){
    if (risktable == TRUE){
  #----------------------------------------------------------------------------#
  ### 5.1 Extract data ####
  #----------------------------------------------------------------------------#
      obsStrata <- if(is.null(fit$strata)){
        obsStrata <- 1
      } else {
        obsStrata <- fit$strata
      }

      grp <- rep(1:arm_no, times=obsStrata)

      # Initialize a matrix 'n.risk.matrix' with zeros
      n.risk.matrix <- matrix(0,nrow = length(xticks), ncol = arm_no)

      # Loop over each arm and each time point defined by 'xticks'
      for (stratum_i in 1:arm_no) {
        for (x in 1:length(xticks)) {
          # Find the indices where the survival time for the current group is
          # greater than the current 'xticks'
          index <- which(fit$time[grp == stratum_i] > xticks[x])
          # If there are no such indices,
          # set the corresponding element in 'n.risk.matrix' to 0
          if (length(index) == 0)
            n.risk.matrix[x,stratum_i] <- 0
          else
            # Otherwise, set the element to the minimum number at risk
            # for the specified group and time point
            n.risk.matrix[x,stratum_i] <- fit$n.risk[grp == stratum_i][min(index)]
        }
      }
  #----------------------------------------------------------------------------#
  ### 5.1.1 Add risktable.title text to the outer margin ####
  #----------------------------------------------------------------------------#
      mtext(risktable.title, side = 1, outer = FALSE,
            line = risktable.pos, adj = 0, at = risktable.title.position,
            font = risktable.title.font,
            cex = risktable.title.cex,
            col = risktable.title.col)
  #----------------------------------------------------------------------------#
  ### 5.1.2 Add legend text to the outer margin for each arm ####
  #----------------------------------------------------------------------------#
      if (missing(risktable.name)) {
        ristkable.name <- legend.name
      } else {
        ristkable.name <- risktable.name
      }
      if(arm_no > 1){
        for (i in 1:arm_no){
          mtext(text = ristkable.name[i], side = 1, outer = FALSE,
                line = i+risktable.pos, adj = 0, at = risktable.name.position,
                font = risktable.name.font,
                cex = risktable.name.cex,
                col = risktable.name.col)
        }
      }
  #----------------------------------------------------------------------------#
  ### 5.1.3 Add vector of risk counts text to the margin ####
  #----------------------------------------------------------------------------#
      if(max(risktable.col == TRUE)) { risktable.col <- col}
      mtext(text = as.vector(n.risk.matrix), side = 1, outer = FALSE,
            line = rep((1:arm_no) + risktable.pos, each = length(xticks)),
            at = rep(xticks, arm_no),
            cex = risktable.cex,
            col = c(rep(risktable.col, each = length(xticks)))
      )
    }
  } else {
    stop("`risktable` expecting TRUE or FALSE as an argument!")
  }
} # final closer of the function
