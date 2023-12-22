# Query Graphical Parameters

#par(mar=c(1*2+6, 6, 2, 0.5) + 0.1, mgp=c(2.5, 0.5, 0), oma=c(0,0,0,0))
# mar for margin
# oma for outer margina area

par(mfrow = c(2,1), mar = c(5, 4, 4, 2), oma = c(0, 0, 2, 0))
survSAKK::surv.plot(survobject1,grid = TRUE)
surv.plot(survobject1)

dev.off()
