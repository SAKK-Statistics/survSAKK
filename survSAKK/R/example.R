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
