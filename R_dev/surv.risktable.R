# Query Graphical Parameters
legend.name <- c("group")
legend.legend <- c("group")

# Options
risktable <- TRUE
risktable.title <- "Number at risk"
risktable.title.font <- 3
risktable.title.col <- "black"
risktable.title.position <- par("usr")[1] - (par("usr")[2]- par("usr")[1])*0.1
risktable.cex <- 0.50
risktable.col <- "black"
risktable.name.font <- 2
risktable.name.col <- "#666666"
risktable.name.position <- par("usr")[1] - (par("usr")[2]- par("usr")[1])*0.1


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
    par(mar = c(3 + stratum + 2, 4, 4, 2)+0.1) # c(bottom, left, top, right)
    
    # Add risktable.title text to the outer margin
    mtext(risktable.title, side = 1, outer = FALSE,
          line = 4, adj = NA, at = risktable.title.position,
          font = risktable.title.font,
          cex = risktable.cex,
          col = risktable.title.col)
    
    # Add legend text to the outer margin for each stratum
    for (i in 1:stratum){
      mtext(text = legend.name[i], side = 1, outer = FALSE,
            line = i+4, adj = NA, at = risktable.name.position,
            font = risktable.name.font,
            cex = risktable.cex,
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

