
# Some examples for documentation ----------------------------------------------


# Load required libraries
library(survSAKK)
library(survival)

# Load data
veteran <- survival::veteran
str(veteran)

veteran$time_m <- veteran$time/365.25*12
veteran$time_w <- veteran$time/7
veteran$time_y <- veteran$time/365.25

# Create survival objects
fit_d <- survfit(Surv(time, status) ~ 1, data = veteran)
fit_m <- survfit(Surv(time_m, status) ~ 1, data = veteran)
fit_w <- survfit(Surv(time_w, status) ~ 1, data = veteran)
fit_y <- survfit(Surv(time_y, status) ~ 1, data = veteran)

fit_arm_d <- survfit(Surv(time, status) ~ trt, data = veteran)
fit_arm_m <- survfit(Surv(time_m, status) ~ trt, data = veteran)
fit_arm_w <- survfit(Surv(time_w, status) ~ trt, data = veteran)
fit_arm_y <- survfit(Surv(time_y, status) ~ trt, data = veteran)

fit_celltype_d <- survfit(Surv(time, status) ~ celltype, data = veteran)
fit_celltype_m <- survfit(Surv(time_m, status) ~ celltype, data = veteran)
fit_celltype_w <- survfit(Surv(time_w, status) ~ celltype, data = veteran)
fit_celltype_y <- survfit(Surv(time_y, status) ~ celltype, data = veteran)

View(lung)

lung$time_m <- lung$time/365.25*12
lung$time_y <- lung$time/365.25

fit_lung_d <- survfit(Surv(time, status) ~ 1, data = lung)
fit_lung_m <- survfit(Surv(time_m, status) ~ 1, data = lung)
fit_lung_y <- survfit(Surv(time_y, status) ~ 1, data = lung)

fit_lung_arm_d <- survfit(Surv(time, status) ~ sex, data = lung)
fit_lung_arm_m <- survfit(Surv(time_m, status) ~ sex, data = lung)
fit_lung_arm_y <- survfit(Surv(time_y, status) ~ sex, data = lung)





## Basic plot ------------------------------------------------------------------

surv.plot(fit_d)
surv.plot(fit_m)
surv.plot(fit_w)
surv.plot(fit_y)

surv.plot(fit_arm_d)
surv.plot(fit_arm_m)
surv.plot(fit_arm_w)
surv.plot(fit_arm_y)

surv.plot(fit_celltype_d)
surv.plot(fit_celltype_m)
surv.plot(fit_celltype_w)
surv.plot(fit_celltype_y)

surv.plot(fit_lung_d)
surv.plot(fit_lung_m)
surv.plot(fit_lung_y)

surv.plot(fit_lung_m, time.unit = "month") # Sollte default mässig nicht abgeschnitten werden!!
surv.plot(fit_lung_y, time.unit = "year")
surv.plot(fit_lung_arm_m, time.unit = "month")

# use col, legend names, change "at risk", add title, change axis
surv.plot(fit_lung_arm_m,
          time.unit = "month",
          col = c("cadetblue2", "cadetblue"),
          main = "Kaplan-Meier plot",
          xlab = "Time since treatment start (months)",
          legend.position = "bottomleft",
          legend.name = c("male", "female"),
          legend.title = "sex",
          risktable.name = c("m", "f"),
)

# funktioniert nicht.. Weshalb??
surv.plot(fit_lung_arm_m,
          time.unit = "month",
          col = c("cadetblue2", "cadetblue"),
          main = "Kaplan-Meier plot",
          ylab = "OS")






### margins ------------------------------------------------------------------


### legend ------------------------------------------------------------------


### risk table -----------------------------------------------------------------


### y-axis unit ----------------------------------------------------------------




## Add segments ----------------------------------------------------------------




## Add statistics --------------------------------------------------------------




## Multiple plots in one figure ------------------------------------------------



## Save figure as png ----------------------------------------------------------





