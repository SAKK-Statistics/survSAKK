
# optional parameter
segment.type <- 1                           # segment.tye: A numeric value specifying the layout of the segment (1: Draws specified segment (full bandwidth), 2: Draws specified segment, 3: Drawing vertical and horizontal segment)
segment.timepoint <-  NULL                  # segment.timepoint: Draw segment(s) at a fixed time point (e.g. "6 month")
segment.quantile <- NULL                    # segment.quantile: Draw segment(s) at a fixed quantile (e.g. "median")
segment.col <- "#666666"                    # segment.col: Can accept a single value for color, or a vector of color values to set color(s)
segment.text.col <- col                     # segment.text.col: Can accept a single value for color, or a vector of color values to set color(s)
segment.lty <- "dashed"                     # segment.lty: A vector of string specifying line types for each curve (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”).
segment.lwd <- 1                            # segment.lwd: A vector of numeric values for line widths
segment.text.cex <- 0.75                    # segment.text.cex: A numeric values specifying the size of the segment text size
segment.text.main <- NULL
segment.text.position <- c("bottomleft")    # segment.text.position: Position of the legend, c(x,y), "bottomleft"
segment.text.space <- 0.03                  # segment.text.space: Spacing between the text

#segment.quantile <- 0.50
segment.timepoint <- 1.5
segment.text.main <- c("Survival Time (95%) ")

# Annotate the segment
if (length(segment.text.position) == 2) {
  # If it's a numeric vector (x, y coordinates)
  text_xpos <- segment.text.position[1]
  text_ypos <- segment.text.position[2]
} else if (segment.text.position == "bottomleft") {
  # If it's a string "bottomleft"
  text_ypos <- 0.05
  text_xpos <- 0
}

for (i in 2:stratum){
  text_ypos[i] <- text_ypos[i-1]+ segment.text.space
}

if (segment.type == 3){
  ### Drawing vertical and horizontal segments ####
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
    text(x = text_xpos,
         y = text_ypos,
         labels = paste0(round(segment_x$quantile,digits = 2),
                         " [",
                         round(segment_x$lower,digits = 2),
                         ",",
                         round(segment_x$upper,digits = 2),
                         "]"),
         pos = 4,                                                               # position to the right of the specifiex (x,y)
         col = segment.text.col,
         cex = segment.text.cex)
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
             lwd = segment.lwd )

    # Annotate the segment
    text(x = text_xpos,
         y = text_ypos,
         labels = paste0(round(segment_y$surv, digits = 2),
                         " [",
                         round(segment_y$lower, digits = 2),
                         ",",
                         round(segment_y$upper, digits = 2),
                         "]"),
         pos = 4,  # Position the text to the left of the point
         col = segment.text.col,
         cex = 0.75) #legend.cex? or segment.cex?
  } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
    stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options.")
  }
} else if (segment.type == 2){
  ### Draw specified segment ####
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
             lwd = segment.lwd )

    text(x = text_xpos,
         y = text_ypos,
         labels = paste0(round(segment_x$quantile,digits = 2),
                         " [",
                         round(segment_x$lower,digits = 2),
                         ",",
                         round(segment_x$upper,digits = 2),
                         "]"),
         pos = 4,                                                               # position to the left of the specifiex (x,y)
         col = segment.text.col,
         cex = segment.text.cex)
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

    # Annotate the segment (Survival rate at specific timepoint)
    text(x = text_xpos,
         y = text_ypos,
         labels = paste0(round(segment_y$surv, digits = 2),
                         " [",
                         round(segment_y$lower, digits = 2),
                         ",",
                         round(segment_y$upper, digits = 2),
                         "]"),
         pos = 4,  # Position the text to the left of the point
         col = segment.col,
         cex = 0.75) #legend.cex? or segment.cex?
  } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
    stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options.")
  }
} else if (segment.type == 1){
  ### Drawing specified segment (full bandwidth) ####
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
    text(x = text_xpos,
         y = text_ypos,
         labels = paste0(round(segment_x$quantile,digits = 2),
                         " [",
                         round(segment_x$lower,digits = 2),
                         ",",
                         round(segment_x$upper,digits = 2),
                         "]"),
         pos = 4,                                                               # Position the text to the right of the specified (x,y)
         col = segment.text.col,
         cex = segment.text.cex)
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
    text(x = text_xpos,
         y = text_ypos,
         labels = paste0(round(segment_y$surv, digits = 2),
                         " [",
                         round(segment_y$lower, digits = 2),
                         ",",
                         round(segment_y$upper, digits = 2),
                         "]"),
         pos = 4,                                                               # Position the text to the left of the specified (x,y)
         col = segment.text.col,
         cex = segment.text.cex)
  } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
    stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options.")
  }
}

# Draw title stat
if (!is.null(segment.text.main)){
  text(text_xpos, max(text_ypos) + segment.text.space, label = segment.text.main, pos = 4,
       col = "black", cex = segment.text.cex)
}



