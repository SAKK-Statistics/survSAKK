
# optional parameter
segment.type <- 3                           # segment.tye: A numeric value specifying the layout of the segment (1: Draws specified segment (full bandwidth), 2: Draws specified segment, 3: Drawing vertical and horizontal segment)
segment.timepoint <-  NULL                  # segment.timepoint: Draw segment(s) at a fixed time point (e.g. "6 month")
segment.quantile <- NULL                    # segment.quantile: Draw segment(s) at a fixed quantile (e.g. "median")
segment.col <- "#666666"                    # segment.col: Can accept a single value for color, or a vector of color values to set color(s)
segment.annotation.col <- col               # segment.annotation.col: Can accept a single value for color, or a vector of color values to set color(s)
segment.lty <- "dashed"                     # segment.lty: A vector of string specifying line types for each curve (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”).
segment.lwd <- 1                            # segment.lwd: A vector of numeric values for line widths
segment.cex <- 0.75                         # segment.cex: A numeric values specifying the size of the segment text size
segment.font <- 1                           # segment.font: A numeric value specifying the font face (1 = plain, 2 = bold, 3 = italic, 4 = bold-italic)
segment.main <- NULL                        # Title of segment text
segment.main.font <- 2                      # segment.main.font A numeric value specifying the fon face (1 = plain, 2 = bold, 3 = italic, 4 = bold-italic, ...)
segment.annotation <- "none"                # segment.annotation: Position of the segment annotation, c(x,y), "bottomleft", ...
segment.annotation.space <- 0.03            # segment.annotation.space: Spacing between the text

#segment.quantile <- 0.50
segment.timepoint = c(3, 6, 12)
#segment.annotation <- "left"
#segment.annotation <- "bottomleft"
#segment.main <- c("Survival time (95%) ")

# Define different options for segment text  ####
if (length(segment.annotation) == 2) {
  # If it's a numeric vector (x, y coordinates)
  text_xpos <- segment.annotation[1]
  text_ypos <- segment.annotation[2]
  # Position the text below of the specified (x,y)
  pos <- 4
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

# Determining the y coordinate for each segment text ####
if (stratum == 1){
  text_ypos[i] <- text_ypos
} else {
  for (i in stratum-1){
  text_ypos[i+1] <- text_ypos[i]+ segment.annotation.space
  }
}


# Draw segments ####
if (segment.type == 3){
  ###segment.type 3: Drawing vertical and horizontal segments ####
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
  ### segment.type 2: Draw specified segment ####
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
           cex = segment.cex)
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
  ### segment.type 1: Drawing specified segment (full bandwidth) ####
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
           pos = pos,                                                               # Position the text to the left of the specified (x,y)
           col = segment.annotation.col,
           cex = segment.cex)
    }
  } else if (!is.null(segment.quantile) & !is.null(segment.timepoint)) {
    stop("`segment.timepoint` AND `segment.quantile ` not applicable! Choose one of the two options.")
  }
}

# Draw title for segment text ####
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


