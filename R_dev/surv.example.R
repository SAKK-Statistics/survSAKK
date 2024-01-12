# Load Data and library
library(survival)
library(survSAKK)

veteran$time_yr <- veteran$time/365.25
veteran$time_mt <- veteran$time_yr*12

# Define the reference arm
veteran$trt <- factor(veteran$trt,
                      levels = c(1,2),
                      labels = c("Standard","Test"))

# Create survival object
veteran_fit_yr <- survfit(Surv(time_yr, status) ~ 1, data = veteran)
veteran_fit_mt <- survfit(Surv(time_mt, status) ~ 1, data = veteran)
veteran_trt_fit_yr <- survfit(Surv(time_yr, status) ~ trt, data = veteran)
veteran_trt_fit_mt <- survfit(Surv(time_mt, status) ~ trt, data = veteran)


# With no Risktable
png(file="C:/Users/vithersans/Desktop/survSAKK_example1.png",
    width = 20.5, height = 16, units = "cm", res = 200)
par(mfrow=c(2,2))
# Plot 1
surv.plot(fit = veteran_fit_mt,
                    risktable = FALSE,
                    xlim = seq(0,34, by = 3),
                    xlab = ("Time (month)"),
                    segment.type = 3,
                    segment.quantile = 0.5,
                    segment.annotation.space = 0.06)
# Plot 2
surv.plot(fit = veteran_fit_yr,
                    risktable = FALSE,
                    xlim = seq(0,3, by = 0.5),
                    xlab = c("Time (year)"),
                    ylab = c("Survival"),
                    legend.name = c("Veterans'Lung Cancer Study"),
                    segment.type = 3,
                    segment.timepoint = 1.0,
                    segment.col = "darkred")
# Plot 3
surv.plot(fit = veteran_trt_fit_mt,
                    risktable = FALSE,
                    legend.position = c(23.7,0.3),
                    xlim = seq(0,34, by = 6),
                    xlab = ("Time (month)"),
                    segment.timepoint = 6,
                    segment.main = "Survival at 6mt [95%]",
                    segment.annotation = c(19.5, 0.85),
                    segment.col = c("#666666","#a6761d"),
                    stat = "coxph",
                    stat.position = "bottomleft",
                    stat.font = 2)

# Plot 4
surv.plot(fit = veteran_trt_fit_mt,
                    risktable = FALSE,
                    grid =TRUE,
                    col = c("pink","lightblue"),
                    xlab = "Time (year)",
                    xlim = seq(0,3),
                    legend.title = "Treatment Regimens",
                    legend.name = c("LT60","OV60"),
                    stat = "coxmodel",
                    stat.position = "bottomleft")
dev.off()

# With Risktable
png(file="C:/Users/vithersans/Desktop/survSAKK_example2.png",
    width = 20.5, height = 16, units = "cm", res = 200)
par(mfrow=c(2,3))
# Plot 5
surv.plot(fit = veteran_fit_mt,
          risktable = TRUE,
          xlim = seq(0,34, by = 3),
          xlab = ("Time (month)"),
          segment.type = 3,
          segment.quantile = 0.5)

# Plot 6
surv.plot(fit = veteran_fit_yr,
          risktable = TRUE,
          xlim = seq(0,3, by = 0.5),
          xlab = c("Time (year)"),
          ylab = c("Survival"),
          main = "Kaplan-Meier Pots With Different Opitons",
          legend.name = c("Veterans"),
          segment.type = 3,
          segment.timepoint = 1.0,
          segment.annotation = c(1.58,0.75),
          segment.col = "darkred")

# Plot 7
surv.plot(fit = veteran_trt_fit_mt,
          risktable = TRUE,
          legend.name = c("LT60","OV60"),
          legend.position = c(23.7,0.3),
          xlim = seq(0,34, by = 6),
          xlab = ("Time (month)"),
          segment.timepoint = 6,
          segment.main = "Survival at 6mt [95%]",
          segment.annotation = c(18, 0.85),
          segment.col = c("#666666","#a6761d"),
          stat = "coxph",
          stat.position = "bottomleft",
          stat.font = 2)

# Plot 8
surv.plot(fit = veteran_trt_fit_mt,
          risktable = TRUE,
          grid =FALSE,
          col = c("pink","lightblue"),
          xlab = "Time (year)",
          xlim = seq(0,3),
          legend.title = "Treatment Regimens",
          legend.name = c("LT60","OV60"),
          stat = "coxmodel",
          stat.position = "bottomleft",
          segment.annotation.space = 0.6)

# Plot 9
surv.plot(fit = veteran_trt_fit_mt,
          risktable = TRUE,
          grid =TRUE,
          col = c("#5ab4ac","#d8b365"),
          xlab = "Time (year)",
          xlim = seq(0,3, by = 0.25),
          legend.title = "Treatment Regimens",
          legend.title.cex = 0.5,
          legend.name = c("LT60","OV60"),
          stat = "coxph",
          stat.position = "bottomleft",
          risktable.col = c("#5ab4ac","#d8b365"),
          risktable.name.font = 4)

# Plot 10
surv.plot(fit = veteran_trt_fit_mt,
          risktable = TRUE,
          grid =TRUE,
          col = c("#fc8d59","#99d594"),
          xlab = "Time (year)",
          xlim = seq(0,3),
          legend.title = "Treatment Regimens",
          #legend.name = c("LT60","OV60"),
          segment.type = 1,
          segment.quantile = 0.5,
          segment.annotation = "left",
          stat = "logrank",
          stat.position = "bottomleft",
          risktable.title = "Number at risk",
          risktable.title.cex =  0.75,
          risktable.name.cex = 0.75
)
dev.off()
