# Query Graphical Parameters
legend.legend <- group

# Options
risk.table.cex <-  0.75
risk.table.col <- "black"

# Extract infromation from fit$strata
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
par(mfrow = c(1,1), mar = c(5, 4, 4, 2), oma = c(stratum+1, 2, 1, 1))         # c(bottom, left, top, right)

# Add "Strata" text to the outer margin
mtext(text = "Strata", side = 2, outer = TRUE,
      line = 1, adj = 1, at = 0,
      cex = risk.table.cex,
      col = risk.table.col)

# Add "Number at risk" text to the outer margin
mtext("Nuber at risk", side = 1, outer = TRUE,
      line = 0, adj = 0, at = 0,
      cex = risk.table.cex,
      col = risk.table.col)

# Add legend text to the outer margin for each stratum
for (i in 1:stratum){
  mtext(text = legend.legend[i], side = 1, outer = TRUE,
        line = i, adj = 0, at = 0,
        cex = risk.table.cex, col = risk.table.col)
}

# Add vector of risk counts text to the margin
mtext(text = as.vector(n.risk.matrix), side = 1, outer = F,
      #line = rep((1:stratum), each = length(xlim)),
      line = rep((1:stratum) + 5, each = length(xlim)),                         # Since Outer = TRUE not working, +5 to get to the outer margin area. ORA(Margin) has 4 line. 
      at = rep(xlim, stratum),
      cex = risk.table.cex,
      col = risk.table.col)


#dev.off()
