
# For testing Purpose ---------------------------------------------------------

library(survival)

lung <- survival::lung
lung$time_yr <- lung$time/365.25
lung$time_mt <- lung$time_yr*12
# Define male as the reference arm
lung$sex <- factor(lung$sex,
                   levels = c(1,2),
                   labels = c("Male","Female"))

# Survival Object with two arms
survobject1 <- survival::survfit(Surv(time_mt, status) ~ sex, data = lung)
survobject2 <- survival::survfit(Surv(time_yr, status) ~ sex, data = lung)
# Survival Object with one arm
survobject3 <- survival::survfit(Surv(time_yr, status) ~ 1, data = lung)
survobject4 <- survival::survfit(Surv(time_mt, status) ~ 1, data = lung)


## Example using the Package
# Base Plot
survSAKK::surv.plot(fit = survobject1)

# Modify Colour
survSAKK::surv.plot(fit = survobject1, col = c("pink", "#666666"))

# Add a title and subtitle
survSAKK::surv.plot(fit = survobject1, main = "KM Curve of the Lung Cancer", sub = "Datasource - NCCTG Lung Cancer Data")

# Rename legend
survSAKK::surv.plot(fit = survobject1,legend.name = c("Male", "Female"))

# Modify x axis
survSAKK::surv.plot(fit = survobject1,legend.name = c("Male", "Female"),
                    xlim = seq(0,35, 3),
                    xlab = "Time (Month)")
# Add Segments
# Median
survSAKK::surv.plot(fit = survobject1,legend.name = c("Male", "Female"),
                    xlim = seq(0,35, 3),
                    xlab = "Time (Month)",
                    segment.quantile = 0.5)

# Survival at 12month
survSAKK::surv.plot(fit = survobject1, legend.name = c("Male", "Female"),
                    xlim = seq(0,35, 3),
                    xlab = "Time (Month)h",
                    segment.timepoint = 12,
                    segment.annotation = "bottomleft")

# Survival at 3, 6 and 12 month
# survSAKK::surv.plot(fit = survobject1,
#                     legend.name = c("Male", "Female"),
#                     xlim = seq(0,35, 3),
#                     xlab = "Time (Month)",
#                     segment.timepoint = c(3, 6, 12),
#                     segment.col = c("darkred","darkblue","darkgreen"),
#                     segment.annotation = "top")

# Specifying the location of annotation manually
survSAKK::surv.plot(fit = survobject1, legend.name = c("Male", "Female"),
                    xlim = seq(0,35, 3),
                    xlab = "Time (Month)",
                    segment.timepoint = 12,
                    segment.annotation = c(3,0.25))

# Add statistics: Cox proportional hazard ratio
survSAKK::surv.plot(fit = survobject1, legend.name = c("Male", "Female"),
                    xlim = seq(0,35, 3),
                    xlab = "Time (Month)",
                    segment.quantile = 0.5,
                    stat = "coxph",
                    stat.position = "bottomleft")

# Add  statistics table
survSAKK::surv.plot(fit = survobject1, legend.name = c("Male", "Female"),
                    xlim = seq(0,35, 3),
                    xlab = "Time (Month)",
                    legend.position = "bottomleft",
                    segment.quantile = 0.5,
                    segment.annotation = "left",
                    segment.font = 1,
                    segment.main.font = 2,
                    stat = "coxmodel",
                    stat.position = c(23,0.905),
                    stat.font = 1,
                    stat.col = "black",
                    stat.cex = 0.75)

# Different themes
# survSAKK::surv.plot(fit = , theme =  )


