
###### testing the surv.plot function with different data sets

# Consulting case mesothelioma #################################################

date <- '20230501'
rootdir <- file.path('L:', 'Consulting', 'Clin', 'Mesothelioma_Registry_Schmid', 'analysis')
exp_path <- file.path('output', date)

# set working directory
setwd(rootdir)
getwd() ## check if OK

# load packages/macros
library(haven)
library(survival)
library(tidyverse)
library(RColorBrewer)

# data
data.subgroup <- as.data.frame(read_sas(file.path('data', 'derived', date, "subgroups.sas7bdat")))


# PFS

# overall
S1 <- survfit(Surv(pfs_time, pfs) ~ 1, data = data.subgroup, conf.type = "log-log")

# by treatment line
S2 <- survfit(Surv(pfs_time, pfs) ~ subgroup_line, data = data.subgroup, conf.type = "log-log")


as.character(S2$call$formula[3])

# chose another reference arm
ds_new <- data.subgroup
str(ds_new$subgroup_line)
ds_new$arm <- factor(ds_new$subgroup_line, levels = c("1L", "2L/later line"))
ds_new$arm <- factor(ds_new$subgroup_line, levels = c("2L/later line", "1L"))
S4 <- survfit(Surv(pfs_time, pfs) ~ arm, data = ds_new, conf.type = "log-log", conf.int = 0.025)
coxph(Surv(pfs_time, pfs) ~ arm, data = ds_new)

data <- as.data.frame(eval(S4$call$data))
names(S4$strata)[2]
str(S4$strata)

# by survival status
S3 <- survfit(Surv(pfs_time, pfs) ~ fuss, data = data.subgroup, conf.type = "log-log")




surv.plot(S1)
surv.plot(S1, xticks = seq(0, 24, by = 3))
surv.plot(S1, time.unit = "month")
surv.plot(S1, censoring.mark = F, censoring.cex = 2)
surv.plot(S1, censoring.mark = T, censoring.cex = 2)
surv.plot(S1, censoring.mark = T)
surv.plot(S1, yticks = seq(0,1,0.2))
surv.plot(S1, yticks = seq(0,1,0.2), y.unit = "percent")

surv.plot(S2)
surv.plot(S2, time.unit ="month")
surv.plot(S2, time.unit ="month", risktable.title.position = -4, risktable.name.position = -4)

surv.plot(S3, conf.band = F) # not working!!

?surv.plot

# display log-rank
surv.plot(S2, stat = "logrank")





# Beispiel Vither ##############################################################

# Load Data
veteran$time_yr <- veteran$time/365.25
veteran$time_mt <- veteran$time_yr*12

# Create survival object
veteran_fit_yr <- survfit(Surv(time / 365.25, status) ~ 1, data = veteran)
veteran_trt_fit_yr <- survfit(Surv(time / 365.25, status) ~ trt, data = veteran)
veteran_trt_fit_mt <- survfit(Surv(time / 365.12 * 12, status) ~ trt, data = veteran)

# Generate survival plots
fit <- veteran_fit_yr
surv.plot(fit = veteran_fit_yr)
surv.plot(fit = veteran_trt_fit_yr)
surv.plot(fit = veteran_fit_yr, main = "Test KM plot ......")
surv.plot(fit = veteran_fit_yr, xticks = seq(0,3, by = 0.5))
surv.plot(fit = veteran_fit_yr, xticks = seq(0,2.5, by = 0.5))

surv.plot(fit = veteran_trt_fit_yr, segment.timepoint=1, segment.annotation = "top", y.unit = "percent")


# Give statistics in % #########################################################

surv.plot(S1, segment.quantile=0.5) # todo: round to one decimal
surv.plot(S1, segment.timepoint=8) # todo: adapt if % is chosen
surv.plot(S1, segment.timepoint=8, y.unit = "percent")
?surv.plot



# test segment.quantile and segment.timepoint ##################################

# quantile
surv.plot(S1, segment.quantile=0.5)
surv.plot(S1, segment.quantile=0.5, segment.type = 1)
surv.plot(S1, segment.quantile=0.5, segment.type = 2)
surv.plot(S1, segment.quantile=0.5, segment.type = 3)
surv.plot(S1, segment.quantile=0.5, segment.annotation = "bottomleft")
surv.plot(S1, segment.quantile=0.5, segment.annotation = "left")
surv.plot(S1, segment.quantile=0.5, segment.annotation = "right")
surv.plot(S1, segment.quantile=0.5, segment.annotation = c(8, 0.75))
surv.plot(S2, segment.quantile=0.5)
surv.plot(S2, segment.quantile=0.5, segment.annotation = "bottomleft")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "sddffd")
surv.plot(S1, segment.quantile=0.5, segment.annotation = "top")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "top")

surv.plot(S2, segment.quantile=0.5, segment.annotation = "top", segment.lwd = 1.2, segment.lty = "dashed")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "top", segment.lwd = 2, segment.lty = "dashed")

surv.plot(S2, segment.quantile=0.5, segment.annotation = "top", segment.lwd = 1.2, segment.lty = "dotted")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "top", segment.lwd = 1.5, segment.lty = "dotted")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "top", segment.lwd = 2, segment.lty = "dotted")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "top", segment.lty = "dashed")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "top", segment.lty = "dashed", time.unit = "month")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "bottomleft", time.unit = "month")
surv.plot(S2, segment.quantile=0.5, time.unit = "month")

surv.plot(S1, time.unit = "month")

surv.plot(S1, segment.quantile=0.5, time.unit = "month")
surv.plot(S2, segment.quantile=0.5, time.unit = "month")
surv.plot(S2, segment.quantile=0.25, segment.annotation.short = T)
surv.plot(S2, segment.quantile=0.25)
# choose the reference arm
surv.plot(S2, segment.quantile=0.25)


# no lines displayed:
surv.plot(S2, segment.quantile=0.5, time.unit = "month")
surv.plot(S2, segment.quantile=0.5, time.unit = "month", segment.lty = "blank")
surv.plot(S2, segment.quantile=0.5, time.unit = "month", segment.lty = "blank")

# short text (median: .. vs ..)
surv.plot(S2, segment.quantile=0.5)
surv.plot(S2, segment.quantile=0.5, time.unit = "month", segment.annotation.short = T)
surv.plot(S2, segment.quantile=0.5, segment.annotation.short = T)
surv.plot(S2, segment.quantile=0.5, segment.annotation.short = T, segment.lty = "blank", segment.annotation = "bottomleft")


# segment.timepoint
surv.plot(S1, segment.timepoint=8)
surv.plot(S1, segment.timepoint=8, segment.type = 2)
surv.plot(S1, segment.timepoint=8, segment.annotation = "bottomleft")
surv.plot(S1, segment.timepoint=8, y.unit = "percent")
surv.plot(S1, segment.timepoint=8, y.unit = "percent", segment.annotation = "bottomleft")
surv.plot(S2, segment.timepoint=8, y.unit = "percent", segment.annotation = "bottomleft")
surv.plot(S1, segment.timepoint=8, segment.annotation = "top")
surv.plot(S2, segment.timepoint=8, segment.annotation = "top")
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", y.unit = "percent")
surv.plot(S1, segment.timepoint=8, segment.annotation = "top", time.unit = "month")
surv.plot(S1, segment.timepoint=8, segment.annotation = "top", time.unit = "month", y.unit = "percent")

surv.plot(S2, segment.timepoint=8, segment.annotation = "top")
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", segment.annotation.short = T)
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", y.unit = "percent", segment.annotation.short = T)
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", y.unit = "percent", segment.annotation.short = T, time.unit = "month")
surv.plot(S1, segment.timepoint=8, segment.annotation = "top", y.unit = "percent", segment.annotation.short = T, time.unit = "month")


# test statistics ###############################################

surv.plot(S2, stat = "logrank")
surv.plot(S2, stat = "coxph")
surv.plot(S2, stat = "coxmodel", stat.position = c(1, 0.1)) # disappears
surv.plot(S2, stat = "coxmodel", stat.position = "left")


# test risktable ###############################################
surv.plot(S2, time.unit ="month")
surv.plot(S2, time.unit ="month", risktable.col = "black")
surv.plot(S2, time.unit ="month", risktable.col = c("red", "blue"))
surv.plot(S2, time.unit ="month", risktable.col = c("red", "blue", "green"))

surv.plot(S1, time.unit ="month")
surv.plot(S2, time.unit ="month")
surv.plot(S1, time.unit ="month", show.legend = TRUE)

# test stratum name ###############################################
surv.plot(S2, time.unit ="month")
surv.plot(S2, time.unit ="month", legend.name = c("Arm A", "Arm B"))
surv.plot(S2, time.unit ="month", legend.name = c("Arm A", "Arm B"), risktable.name.short = c("aaa", "bbb"))
surv.plot(S2, time.unit ="month", legend.name = c("Arm A"))
surv.plot(S1, time.unit ="month", legend.name = c("Arm A"), show.legend = T)
surv.plot(S2, time.unit ="month", legend.name = c("Arm A", "Arm B"), legend.title = "Treatment arm", risktable.name.short = c("A", "B"))


# test reference arm, CI and conf.type ###############################################

S2

surv.plot(S2)
surv.plot(S2, conf.int = 0.8)
surv.plot(S2, conf.type = "log")

surv.plot(S2, segment.quantile=0.5, time.unit = "month")
surv.plot(S2, reference.arm = "2L/later line", segment.quantile=0.5, time.unit = "month")
ds <- data.subgroup
ds$arm <- 1
ds$arm[ds$subgroup_line == 1L] <- 2
S2b <- survfit(Surv(pfs_time, pfs) ~ subgroup_line, data = data.subgroup, conf.type = "log-log")








