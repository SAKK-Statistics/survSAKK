# optional parameter
stat <- "none"                                            # stat: Statistics which is displayed in the plot ("logrank", "coxph", "coxmodel", "none")
stat.position <- "left"                            # stat.position: Position where the stat should be displayed: (c(x,y), "bottomleft", "left", "right", "none")
stat.col <- "black"
stat.cex <- 0.75
stat.font <- 1

stat <- "coxmodel"


# Define different options for stat position ####
if (length(stat.position) == 2){
  # If it's a numeric vector (x, y coordinates)
  stat_xpos <- stat.position[1]
  stat_ypos <- stat.position[2]
  # Position the text below of the specified (x,y)
  pos <- 1
} else if (stat.position == "bottomleft"){
  stat_ypos <- 0.03
  stat_xpos <- min(xlim)
  # Position the text to the right of the specified (x,y)
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

# Log rank test ####
## To compare the survival curves of two or more groups
logrank <- fit$call                                   # Extract the call from survival object
logrank$conf.type <- NULL
logrank$conf.int <- NULL
logrank[1] <- call("survdiff")                        # Modify the call from survfit() to survdiff()
logrank <- eval(logrank)

# Recalculating p-Value
logrankpval <- as.numeric(format.pval(1 - pchisq(logrank$chisq, df = length(logrank$n) - 1), esp = 0.001))
logrankpval <- round.pval(logrankpval)

# Cox proportional hazard regression ####
## To describe the effect of variables on survival
model <- fit$call                                     # Extract the call from survival object
model$conf.type <- NULL
model$conf.int <- NULL
model[1] <- call("coxph")                             # Modify the call from survfit() to coxph()
model <- summary(eval(model))

if(stat == "logrank"){
  stats <- paste0("Logrank test\n", logrankpval)
  } else if(stat == "coxph"){
    stats <- paste0("HR ",
                    round(model$conf.int[,"exp(coef)"], digits = 2),
                    " (95% CI: ",
                    round(model$conf.int[,"lower .95"], digits = 2),
                    " to ",
                    round(model$conf.int[,"upper .95"], digits = 2),
                    ")")
  }

    # } else if(stat == "model"){
    #   code: table2plot()
    # }

if (stat != "none" & stat != "model"){
  # Annotate the stats in the plot
  text(x = stat_xpos,
       y = stat_ypos,
       labels = stats,
       pos = pos,
       col = stat.col,
       cex = stat.cex,
       font = stat.font)
}

# Extract Info from model:
N <- model$n
Events <- model$nevent
HR <- round(model$conf.int[,"exp(coef)"], digits = 2)
#CI  <- c(paste(round(model$conf.int[,"lower .95"], digits = 2),"to",round(model$conf.int[,"upper .95"], digits = 2)))
lwrCI <- round(model$conf.int[,"lower .95"], digits = 2)
uprCI <- round(model$conf.int[,"upper .95"], digits = 2)
Logrank <- logrankpval

tbl <- data.frame(N, Events, HR, lwrCI, uprCI, Logrank)
# Annotation
table2plot(x = stat_xpos, y = stat_ypos, tbl, cex = stat.cex)

# Draw table
table2plot <- function (x, y,
                        table,
                        cex = 1,
                        # Positioning for the table relative to ‘⁠x,y⁠’.
                        xjust = 0,
                        yjust = 1,
                        xpad = 0.1,
                        ypad = 0.5,
                        text.col = "black",
                        display.colnames = TRUE)
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
           cex = cex, col = text.col)
      xleft <- xleft + cellwidth[column]
    }

  # Draw tables cells
  for (row in 1:tabdim[1]) {
    xleft <- x - xjust * (sum(cellwidth))
    for (column in 1:tabdim[2]) {
      text(xleft + 0.5 * cellwidth[column],
           ytop - (row + display.colnames - 0.5) * cellheight, table[row, column],
           cex = cex, col = text.col)
      xleft <- xleft + cellwidth[column]
    }
  }
}
