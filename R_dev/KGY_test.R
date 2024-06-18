
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

# by survival status
S3 <- survfit(Surv(pfs_time, pfs) ~ fuss, data = data.subgroup, conf.type = "log-log")


surv.plot(S1)
surv.plot(S1, xticks = seq(0, 24, by = 1))
surv.plot(S1, xticks = seq(0, 20, by = 1))
surv.plot(S1, xticks = seq(0, 16, by = 1))
surv.plot(S1, xticks = seq(0, 14, by = 1))
surv.plot(S1, xticks = seq(0, 14, by = 1), risktable.title = "Number at risk")
surv.plot(S1, xticks = seq(0, 14, by = 1), risktable.censoring = TRUE)
surv.plot(S1, xticks = seq(0, 14, by = 1), risktable.censoring = TRUE, risktable.title = "Number at risk")



surv.plot(S1, xticks = seq(0, 24, by = 2))
surv.plot(S1, xticks = seq(0, 24, by = 3))
surv.plot(S1, xticks = seq(0, 24, by = 4))
surv.plot(S1, xticks = seq(0, 24, by = 5))


surv.plot(S1)
surv.plot(S1, xticks = seq(0, 24, by = 3))
surv.plot(S1, time.unit = "month")
surv.plot(S1, censoring.mark = F, censoring.cex = 2)
surv.plot(S1, censoring.mark = T, censoring.cex = 2)
surv.plot(S1, censoring.mark = T)
surv.plot(S1, yticks = seq(0,1,0.2))
surv.plot(S1, yticks = seq(0,1,0.2), y.unit = "percent")

# Testing segment annotation for one arm: ####
# Start Vither
surv.plot(S1,risktable.censoring = TRUE)
surv.plot(S1,risktable.censoring = FALSE, theme = "Lancet")
surv.plot(S1,risktable.censoring = TRUE, theme = "ESMO")
surv.plot(S1,risktable.censoring = FALSE, theme = "ESMO")
surv.plot(S1,risktable.censoring = TRUE, theme = "SAKK", segment.timepoint = c(4,8,12))
surv.plot(S1,risktable.censoring = TRUE, theme = "JCO", segment.timepoint = c(4,8,12), segment.annotation.col = c("black"))
surv.plot(S1,risktable.censoring = TRUE, theme = "JCO", segment.timepoint = c(4,8,12), segment.annotation.col = c("black"), segment.annotation = "top")
surv.plot(S1,risktable.censoring = FALSE, theme = "JCO", segment.timepoint = c(4,8,12), segment.annotation.col = c("blue"))
surv.plot(S1,risktable.censoring = FALSE, theme = "JCO", segment.timepoint = c(4,8,12), segment.type = 3, segment.annotation = "top")
surv.plot(S1,risktable.censoring = TRUE, theme = "SAKK", segment.quantile = c(0.5))
surv.plot(S1,risktable.censoring = TRUE, theme = "SAKK", segment.quantile = c(0.5), segment.confint = F) # Expecting Error message: Check passed
surv.plot(S1,risktable.censoring = TRUE, theme = "SAKK", segment.quantile = c(0.5), segment.confint = T)
surv.plot(S1,risktable.censoring = TRUE, theme = "SAKK", segment.quantile = c(0.5), segment.annotation.two.lines = TRUE)
surv.plot(S1,risktable.censoring = TRUE, theme = "SAKK", segment.quantile = c(0.5), segment.main = "ABC", segment.annotation.two.lines = TRUE)

surv.plot(S1,risktable.censoring = TRUE, theme = "SAKK", segment.quantile = c(0.25))
surv.plot(S1,risktable.censoring = TRUE, theme = "SAKK", segment.quantile = c(0.25,0.5))
surv.plot(S1,risktable.censoring = TRUE, theme = "SAKK", segment.quantile = c(0.25,0.5, 0.10))
surv.plot(S1, segment.quantile = c(0.25,0.5, 0.10), segment.annotation.col = "black")
surv.plot(S1, segment.quantile = c(0.25,0.5, 0.10), segment.annotation.col = "black", segment.type = 2)
# end Vither;

surv.plot(S2, legend.title.cex = 3, legend.title = "title")
surv.plot(S2, segment.cex = 3, segment.timepoint = 8)
surv.plot(S2, stat = "logrank", stat.cex = 3)
surv.plot(S2, risktable.name.cex = 3)
surv.plot(S2, xlab.cex = 3)
surv.plot(S2, cex = 1.2)

surv.plot(S2)
surv.plot(S2, time.unit ="month")
surv.plot(S2, time.unit ="month", risktable.title.position = -4, risktable.name.position = -4)

# VSO Testing segment annotation for two arms:
surv.plot(S2,risktable.censoring = TRUE)
surv.plot(S2,risktable.censoring = FALSE, theme = "Lancet")
surv.plot(S2,risktable.censoring = TRUE, theme = "ESMO")
surv.plot(S2,risktable.censoring = FALSE, theme = "ESMO")
surv.plot(S2,risktable.censoring = TRUE, theme = "SAKK", segment.timepoint = c(4,8,12))
surv.plot(S2,risktable.censoring = TRUE, theme = "JCO", segment.timepoint = c(4,8,12), segment.annotation.col = c("black"))
surv.plot(S2,risktable.censoring = FALSE, theme = "JCO", segment.timepoint = c(4,8,12), segment.annotation.col = c("blue"))
surv.plot(S2,risktable.censoring = FALSE, theme = "JCO", segment.timepoint = c(4,8,12), segment.annotation = "left")
surv.plot(S2,risktable.censoring = FALSE, theme = "JCO", segment.timepoint = c(4,8,12), segment.annotation = "right")
surv.plot(S2,risktable.censoring = FALSE, theme = "JCO", segment.timepoint = c(4,8,12), segment.annotation = "top")
surv.plot(S2,risktable.censoring = FALSE, theme = "JCO", segment.timepoint = c(4,8,12), segment.annotation = "right",segment.type = 1)
surv.plot(S2,risktable.censoring = TRUE, theme = "SAKK", segment.quantile = c(0.25))
surv.plot(S2, segment.quantile = c(0.5), segment.confint = FALSE,  segment.annotation.two.lines = FALSE)
surv.plot(S2, segment.quantile = c(0.5),  segment.annotation.two.lines = TRUE)
surv.plot(S2, segment.quantile = c(0.5),  segment.annotation.two.lines = FALSE)
surv.plot(S2, segment.quantile = c(0.5), segment.confint = TRUE)
surv.plot(S2, segment.quantile = c(0.25), segment.confint = FALSE)
surv.plot(S2, segment.quantile = c(0.25), segment.confint = TRUE)
surv.plot(S2, segment.quantile = c(0.5,0.25), segment.confint = TRUE)
surv.plot(S2, segment.quantile = c(0.5,0.25), segment.confint = TRUE, segment.annotation.col = "black")
surv.plot(S2, segment.quantile = c(0.10, 0.25, 0.5), segment.type = 2, segment.confint = TRUE)
surv.plot(S2, segment.quantile = c(0.10, 0.25, 0.5), segment.confint = TRUE) # Expect:  Option of segment.confint has no effect: Passed
surv.plot(S2, segment.quantile = c(0.10, 0.25, 0.5), segment.confint = FALSE) # Passed
surv.plot(S2, segment.quantile = c(0.10, 0.25, 0.5), segment.type = 3) # Passed
# end VSO;

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
surv.plot(S2, segment.quantile=0.5, segment.annotation = "none")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "bottomleft")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "sddffd")
surv.plot(S1, segment.quantile=0.5, segment.annotation = "top")
surv.plot(S2, segment.quantile=0.5, segment.annotation = "top")
surv.plot(S2, segment.quantile=0.5, segment.annotation = c(10, 0.75))

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
surv.plot(S2, segment.quantile=0.25, segment.confint = F)
surv.plot(S2, segment.quantile=0.5, segment.confint = F)
surv.plot(S2, segment.quantile=0.5, segment.confint = F, time.unit= "month")
surv.plot(S2, segment.quantile=0.25)
surv.plot(S2, segment.quantile=0.25)


# no lines displayed:
surv.plot(S2, segment.quantile=0.5, time.unit = "month")
surv.plot(S2, segment.quantile=0.5, time.unit = "month", segment.lty = "blank")
surv.plot(S2, segment.quantile=0.5, time.unit = "month", segment.lty = "blank")

# short text (median: .. vs ..)
surv.plot(S2, segment.quantile=0.5)
surv.plot(S2, segment.quantile=0.5, reference.arm = "2L/later line", segment.confint = F)
surv.plot(S2, segment.quantile=0.5, time.unit = "month", segment.confint = F)
surv.plot(S2, segment.quantile=0.5, segment.confint = F)
surv.plot(S2, segment.quantile=0.5, segment.confint = F, segment.lty = "blank", segment.annotation = "bottomleft")

surv.plot(S2, segment.quantile=0.5, segment.annotation = "top", segment.type = 4)

# segment.timepoint
surv.plot(S1, segment.timepoint=8)
surv.plot(S1, segment.timepoint=8, segment.quantile=0.5)
surv.plot(S1, segment.timepoint=8, segment.type = 2)
surv.plot(S1, segment.timepoint=8, segment.annotation = "bottomleft")
surv.plot(S1, segment.timepoint=8, y.unit = "percent")
surv.plot(S1, segment.timepoint=8, y.unit = "percent", segment.annotation = "bottomleft")
surv.plot(S2, segment.timepoint=8, y.unit = "percent", segment.annotation = "bottomleft")
surv.plot(S1, segment.timepoint=8, segment.annotation = "top")
surv.plot(S2, segment.timepoint=8, segment.annotation = "top")
surv.plot(S2, segment.timepoint=8, segment.annotation = "none")
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", y.unit = "percent")

surv.plot(S1, segment.timepoint=8, segment.annotation = "top", time.unit = "month")
surv.plot(S1, segment.timepoint=8, segment.annotation = "top", time.unit = "month", y.unit = "percent")

surv.plot(S2, segment.timepoint=8, segment.annotation = "top")
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", segment.main = "Survival test")
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", segment.confint = F)
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", y.unit = "percent", segment.confint = F)
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", y.unit = "percent", segment.confint = F, time.unit = "month")
surv.plot(S1, segment.timepoint=8, segment.annotation = "top", y.unit = "percent", segment.confint = F, time.unit = "month")

surv.plot(S2, segment.timepoint=8, segment.annotation = "top", segment.type = "kk")


# test statistics ###############################################

surv.plot(S2, stat = "logrank")
surv.plot(S2, stat = "coxph")
surv.plot(S2, stat = "coxph", stat.position = c(8, 0.75))
surv.plot(S2, stat = "dsdf", stat.position = c(8, 0.75))
surv.plot(S2, stat = "coxph", stat.position = "bottomleft")
surv.plot(S2, stat = "coxph", stat.position = c(8, 0.75), conf.int = 0.9)
surv.plot(S2, stat = "coxph_logrank")
surv.plot(S2, stat = "coxph_logrank") # too close tho x-axis
surv.plot(S2, stat = "coxph_logrank", stat.position = "right")
surv.plot(S2, stat = "coxph_logrank", stat.position = "topright")
surv.plot(S2, stat = "coxph_logrank", stat.position = "top")
surv.plot(S2, stat = "coxph_logrank", stat.position = "bottomleft")
surv.plot(S2, stat = "coxph_logrank", stat.position = "bottomleft")
surv.plot(S2, stat = "coxph_logrank", stat.position = "top")
surv.plot(S2, stat = "coxph", stat.position = "top")
surv.plot(S2, stat = "coxph_logrank", stat.position = c(12, 0.9))

surv.plot(S2, stat = "coxph_logrank", stat.position = "bottomleft", reference.arm = "2L/later line")
surv.plot(S2, stat = "coxph", stat.position = "bottomleft")
surv.plot(S2, stat = "coxph", stat.position = "left")
surv.plot(S2, stat = "coxph", stat.position = "top")
surv.plot(S2, stat = "coxph", stat.position = "topright")
surv.plot(S2, stat = "coxph", stat.position = "bottomright")


# test risktable ###############################################
surv.plot(S2, time.unit ="month")
surv.plot(S2, time.unit ="month", risktable.col = TRUE)
surv.plot(S2, time.unit ="month", risktable.col = "black")
surv.plot(S2, time.unit ="month", risktable.col = c("red", "blue"))
surv.plot(S2, time.unit ="month", risktable.col = c("red", "blue", "green"))

surv.plot(S1, time.unit ="month")
surv.plot(S2, time.unit ="month")
surv.plot(S1, time.unit ="month", legend = TRUE)


# test stratum name ###############################################
surv.plot(S2, time.unit ="month")
surv.plot(S2, time.unit ="month", legend.name = c("Arm A", "Arm B"))
surv.plot(S2, time.unit ="month", legend.name = c("Arm A", "Arm B"), risktable.name = c("aaa", "bbb"))
surv.plot(S2, time.unit ="month", legend.name = c("Arm A"))

surv.plot(S2, time.unit ="month", legend.name="")
surv.plot(S1, time.unit ="month", legend.name = c("Arm A"))
surv.plot(S1, time.unit ="month", legend.name = c("Arm A"), show.legend = T)
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
ds$arm <- 2
ds$arm[ds$subgroup_line == "1L"] <- 1
S2b <- survfit(Surv(pfs_time, pfs) ~ arm, data = ds, conf.type = "log-log")
surv.plot(S2b, segment.quantile=0.5, time.unit = "month")
surv.plot(S2b, segment.quantile=0.5, time.unit = "month", reference.arm = "2")


# test conf.int, does it also change statistics? ###############################################

surv.plot(S2, segment.quantile=0.5)
surv.plot(S2, segment.quantile=0.5, conf.int = 0.8)

surv.plot(S2, segment.timepoint=8, segment.annotation = "top")
surv.plot(S2, segment.timepoint=8, segment.annotation = "top", conf.int = 0.8)


# test 3 strata ###############################################

ds <- data.subgroup
str(ds)
ds$arm <- c(rep("A", 35), rep("B", 35), rep("C", 39))
S4 <- survfit(Surv(pfs_time, pfs) ~ arm, data = ds, conf.type = "log-log")
surv.plot(S4)
surv.plot(S4, segment.quantile=0.5, time.unit = "month")
surv.plot(S3, segment.quantile=0.5, time.unit = "month")
surv.plot(S2, segment.quantile=0.5, time.unit = "month")
surv.plot(S4, segment.timepoint=8, time.unit = "month")
surv.plot(S4, stat = "logrank", time.unit = "month")
surv.plot(S4, stat = "logrank", time.unit = "month", conf.line.lty = "solid")

surv.plot(S2, stat = "coxph", time.unit = "month")
surv.plot(S2, stat = "coxph_logrank", time.unit = "month")
surv.plot(S4, stat = "coxph", time.unit = "month")

# check segment.confint for 3 arms
surv.plot(S4, segment.timepoint=8, segment.annotation = "top", segment.confint = F)
surv.plot(S4, segment.quantile=0.5, segment.annotation = "top", segment.confint = F)
surv.plot(S1, segment.timepoint=8, segment.annotation = "top", segment.confint = F)
surv.plot(S1, segment.quantile=0.5, segment.annotation = "top", segment.confint = F)


# test stat with SAKK 08/15 ###############################################

analysis <- '20220808'

rootdir <- file.path('L:', 'Trials', 'Terminated_trials', 'SAKK_08_15_PROMET', '_Files_after_termination', 'Stat', '2_primary_analysis')
derived_path <- file.path('data', 'derived', analysis)
setwd(rootdir)

library(haven)
library(survival)
ttp <- read_sas(file.path(derived_path,  "ttp.sas7bdat"))
ttp_fas <- ttp[ttp$fas==1,]


# Estimate Kaplan-Meier curve
S_0815 <- survfit(Surv(ttp_m, ttp_event) ~ ra2, data = ttp_fas, conf.type="log-log")
surv.plot(S_0815)
surv.plot(S_0815, risktable = FALSE)
surv.plot(S_0815, conf.int = TRUE)
surv.plot(S_0815, conf.int = FALSE)
surv.plot(S_0815, conf.int = 0.95)
surv.plot(S_0815, conf.int = 0.9)
surv.plot(S_0815, conf.int = 0)
surv.plot(S_0815, time.unit = "month")
surv.plot(S_0815, time.unit = "month", stat = "coxph")
surv.plot(S_0815, time.unit = "month", stat = "logrank")
surv.plot(S_0815, time.unit = "month", stat = "coxph_logrank")
surv.plot(S_0815, time.unit = "month", stat = "coxph", stat.conf.int = 0.9)
surv.plot(S_0815, time.unit = "month", stat = "coxph_logrank", stat.conf.int = 0.9)

S_0815_strata <- survfit(Surv(ttp_m, ttp_event) ~ ra2+strata(gleason, resmarg, psa_ra, adtplyn), data = ttp_fas, conf.type="log-log")
surv.plot(S_0815, time.unit = "month", stat = "coxph", stat.fit = S_0815_strata)
surv.plot(S_0815, time.unit = "month", stat = "logrank", stat.fit = S_0815_strata)
surv.plot(S_0815, time.unit = "month", stat = "coxph_logrank", stat.fit = S_0815_strata)
surv.plot(S_0815, time.unit = "month", stat = "coxph", stat.conf.int = 0.9, stat.fit = S_0815_strata)
surv.plot(S_0815, time.unit = "month", stat = "coxph_logrank", stat.conf.int = 0.9, stat.fit = S_0815_strata)


surv.plot(S_0815, time.unit = "month", stat = "coxph", stat.fit = S_0815_strata, reference.arm = "RT")
surv.plot(S_0815, time.unit = "month", stat = "coxph", stat.fit = S_0815_strata, reference.arm = "RT + Metformin")

ttp_fas$ra_new <- as.factor(ttp_fas$ra2)
S_0815_temp <- survfit(Surv(ttp_m, ttp_event) ~ ra_new, data = ttp_fas, conf.type="log-log")
S_0815_strata_temp <- survfit(Surv(ttp_m, ttp_event) ~ ra_new+strata(gleason, resmarg, psa_ra, adtplyn), data = ttp_fas, conf.type="log-log")
surv.plot(S_0815_temp, time.unit = "month", stat = "coxph", stat.fit = S_0815_strata_temp)
surv.plot(S_0815_temp, time.unit = "month", stat = "coxph", stat.fit = S_0815_strata_temp, reference.arm = "RT")
surv.plot(S_0815_temp, time.unit = "month", stat = "coxph", stat.fit = S_0815_strata_temp, reference.arm = "RT + Metformin")




# test margins #################################################################

surv.plot(S_0815)
surv.plot(S_0815, risktable = FALSE)
surv.plot(S_0815, margin.bottom = 4)
surv.plot(S_0815, margin.top = 4)
surv.plot(S_0815, margin.right = 1)



# test to plot multiple curves in one figure ###############################################

split.screen(c(2,2))
screen(1)
surv.plot(S1, margin.left = 4, margin.top = 1)
screen(2)
surv.plot(S2, margin.left = 4, margin.top = 1)
screen(3)
surv.plot(S3, margin.left = 4, margin.top = 1)
screen(4)
surv.plot(S_0815, margin.left = 4, margin.top = 1)
close.screen(all = TRUE)




# test different parameters ###############################################

surv.plot(S1, lty = c("dotted","dotted","dotted"))
surv.plot(S1, conf.line = TRUE, lty = c("dotted","dotted","dotted"))
surv.plot(S1, conf.line = TRUE, lty = c("solid","solid","solid"))
surv.plot(S1, conf.line = TRUE, lty = c("solid","solid","solid"), lwd = 1, col = "grey")
surv.plot(S1, conf.line = TRUE, lty = c("solid","dotted","dotted"), lwd = 1, col = "grey")
surv.plot(S1, conf.int = TRUE)

surv.plot(S1, conf.int = 0.95)
surv.plot(S1, conf.int = 0.8)
surv.plot(S1, conf.int = 0)


S_temp <- survfit(Surv(pfs_time, pfs) ~ 1, data = data.subgroup, conf.type = "log-log", conf.int = 0.9)
summary(S_temp)
surv.plot(S_temp)
surv.plot(S_temp, conf.int = 0.9)


ds <- data.subgroup
ds$arm <- 1
ds$arm[ds$subgroup_line == "1L"] <- 2
S_temp <- survfit(Surv(pfs_time, pfs) ~ arm, data = ds, conf.type = "log-log", conf.int = 0.9)
surv.plot(S_temp)
surv.plot(S_temp, reference.arm = "2")
surv.plot(S_temp, reference.arm = 2)
surv.plot(S_temp, reference.arm = 2, col = c("red", "blue"), conf.band.col = c("lightblue", "grey"))
surv.plot(S_temp, reference.arm = 2, col = c("red", "blue"), conf.band.col = c("lightblue", "grey"), main = "Test title", sub = "Subtitle")



base::plot(
  # Plot the survival curve
  S_temp,
  conf.int = 0.95,
  main = "Title",
  sub = "Subtitle",
  #cex.lab = 3
)

base::plot(
  # Plot the survival curve
  S_temp,
  main = "test",
  # Add censoring information with ticks
  mark.time = T,
  mark = "/",
  cex = 3,                  # increase mark for censored patients.
  # Modify Layout
  xaxs = "i", yaxs = "i",               # Start axis exactly from zero origin
  xaxt = "n", yaxt = "n",               # Remove the original axes
  ylim = c(0,1),                        # Set y-axis limits
  xlim = seq(1,24,by = 6),                 # Set x-axis limits
  xlab = "Hallo ",                            # Draw x label
  ylab = "Hallo",                            # Draw y label
  cex.lab = 4
)


surv.plot(S2)
surv.plot(S2, xlab.cex = 2)
surv.plot(S2, bty = "n")


surv.plot(S2, segment.quantile = 0.5, segment.col="grey")
surv.plot(S2, segment.quantile = 0.5, segment.annotation.col = c("blue"))
surv.plot(S2, segment.quantile = 0.5, segment.lty = c("twodash"), segment.lwd = 3)
surv.plot(S2, segment.quantile = 0.5, segment.lty = c("dotted"), segment.lwd = 3)
surv.plot(S2, segment.quantile = 0.5, segment.lty = c("dotted"), segment.lwd = 3, segment.cex = 1.2)
surv.plot(S2, segment.quantile = 0.5, segment.lty = c("dotted"), segment.lwd = 3, segment.cex = 1.2, segment.annotation = "top")


surv.plot(S2, segment.quantile = 0.5, segment.confint = F)
surv.plot(S1, segment.quantile = 0.5, segment.confint = T)
surv.plot(S4, segment.quantile = 0.5, segment.confint = F)
surv.plot(S4, segment.quantile = 0.5, segment.annotation.space = 0.05)


surv.plot(S2, segment.quantile = 0.5, cex.lab = 3)


# test lwd, lty and conf.line.lwd, conf.line.lty ###############################################
surv.plot(S2)
surv.plot(S2, lwd = 2)
surv.plot(S2, lwd = 2, lty = "dotted")
surv.plot(S2, conf.line.lty = "dotted")
surv.plot(S2, conf.line.lty = "solid")
surv.plot(S2, conf.line.lty = "solid", conf.line.lwd = 3)
surv.plot(S2, lty = "dotted", conf.line.lty = "solid", conf.line.lwd = 3)
surv.plot(S2, conf.line.lty = "dotted", conf.line.lwd = 2.5)
surv.plot(S2, conf.line.lty = "solid")


# test x-ticks

fit <- survfit(Surv(time_m, status) ~ sex, data = lung)
xticks <- seq(from = 0, to = max(fit$time)+6, by = 6)
xticks <- seq(from = 0, to = max(fit$time)+ceiling(max(fit$time)/6),
              by = ceiling(max(fit$time)/6))



# test special characters <= and >= ############################################

data.subgroup$test <- "<= 1"
data.subgroup$test[data.subgroup$subgroup_line != "1L"] <- ">= 0.3"

S4 <- survfit(Surv(pfs_time, pfs) ~ test, data = data.subgroup, conf.type = "log-log")

surv.plot(S4)



# test NR instead of NA ############################################

surv.plot(S2, segment.quantile=0.5, time.unit = "month")
surv.plot(S2, segment.quantile=0.25, time.unit = "month")
surv.plot(S1, segment.quantile=0.5, time.unit = "month")

surv.plot(S2, segment.quantile=0.25, time.unit = "month", segment.confint = F)


setwd("L:/Trials/Terminated_trials/SAKK_08_15_PROMET/_Files_after_termination/Stat/2_primary_analysis/data/derived/20220808")
getwd()

# data
ttp <- as.data.frame(read_sas("ttp.sas7bdat"))

# TTP
S <- survfit(Surv(ttp_m, ttp_event) ~ ra2, data = ttp, conf.type="log-log")

surv.plot(S)
surv.plot(S, segment.quantile = 0.5)
surv.plot(S, segment.quantile=0.5, segment.confint = F)



# segment: annotate on one line for one arm (by default..?) ###################


surv.plot(S1, segment.quantile=0.5, time.unit = "month")

surv.plot(S1, segment.quantile=0.25, time.unit = "month")
surv.plot(S1, segment.quantile=0.25, time.unit = "month", segment.annotation.two.lines = TRUE)
surv.plot(S1, segment.quantile=0.5, time.unit = "month", segment.annotation.two.lines = TRUE)

surv.plot(S1, segment.quantile=0.25, time.unit = "month", segment.main = "Test")
surv.plot(S1, segment.quantile=0.25, time.unit = "month", segment.main = "Test", segment.annotation.two.lines = TRUE)


surv.plot(S1, segment.timepoint = 6, time.unit = "month")
surv.plot(S1, segment.timepoint = 6, time.unit = "month", segment.annotation.two.lines = TRUE)
surv.plot(S1, segment.timepoint = 6, segment.annotation.two.lines = TRUE)

surv.plot(S1, segment.timepoint = 6, time.unit = "month", segment.main = "Test")
surv.plot(S1, segment.timepoint = 6, time.unit = "month", segment.main = "Test", segment.annotation.two.lines = TRUE)

