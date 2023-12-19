#' Example
#'
#'lung <- survival::lung
#'lung$time_yr <- lung$time/365.25
#'lung$time_mt <- lung$time_yr*12
#'lung$sex <- factor(lung$sex,
#'                   levels = c(1,2),
#'                   labels = c("Male","Female"))
#'
#' # Survival Object with two arms
#'survobject1 <- survival::survfit(Surv(time_mt, status) ~ sex, data = lung)
#'survobject2 <- survival::survfit(Surv(time_yr, status) ~ sex, data = lung)
#' # Survival Obhect with one arm
#'survobject3 <- survival::survfit(Surv(time_yr, status) ~ 1, data = lung)
#'survobject4 <- survival::survfit(Surv(time_mt, status) ~ 1, data = lung)
#'
#'surv.plot(fit = survobject)
#'survSAKK::surv.plot(fit = survobject1)
#'survSAKK::surv.plot(fit = survobject2, xlim = seq(0,3), legend.legend = c("Female","Male"))
#'survSAKK::surv.plot(fit = survobject3, xlim = seq(0,3))
#'survSAKK::surv.plot(fit = survobject4)
#'survSAKK::surv.plot(fit = survobject2, xlim = seq(0,3), segment.type = 3, segment.timepoint = 0.5, segment.text.position = "left")
#'survSAKK::surv.plot(fit = survobject2, xlim = seq(0,3), segment.type = 3, segment.quantile = 0.5, segment.text.position = "bottomleft")
#'survSAKK::surv.plot(fit = survobject2, xlim = seq(0,3), segment.type = 3, segment.quantile = 0.25, segment.text.position =c(0.35,0.4))
#'survSAKK::surv.plot(fit = survobject2, xlim = seq(0,3),
#'                    col =c("pink","lightblue"),
#'                    segment.type = 3,
#'                    segment.quantile = 0.5,
#'                    segment.text.position = "none")
#'
#'survSAKK::surv.plot(fit = survobject2, xlim = seq(0,3), segment.type = 3, segment.timepoint = 0.5, segment.text.position = "none")
#'
#'survSAKK::surv.plot(fit = survobject2,
#'                    col =c("purple","green"),
#'                    segment.type = 3,
#'                    segment.timepoint = 1.5,
#'                    segment.text.position = "right",
#'                    segment.col = c("black","grey"),
#'                    conf.band.col = c("red","yellow"))
