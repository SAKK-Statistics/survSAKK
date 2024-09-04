
library(survival)
survObject <- survfit(Surv(time, status) ~ sex, data = lung)



# Test 1: Basic functionality - lot without error
test_that("surv.plot can plot without error", {
  expect_silent(surv.plot(fit = survObject))
})

# Test 2: Invalid `y.unit` parameter should throw error
test_that("surv.plot throws error for invalid y.unit parameter", {
  expect_error(surv.plot(fit = survObject,
                         time.unit = "month",
                         y.unit = "invalid"),
               "'invalid' is not a valid argument for `y.unit`!")
})


# Test 3:  Invalid `theme` parameter should throw error
test_that("surv.plot throws error for invalid theme parameter", {
  expect_error(surv.plot(fit = survObject,
                         reference.arm = "1",
                         time.unit = "month",
                         theme = "invalid"),
               "Provided theme argument does not exist!")
})


# Test 4: Invalid `risktable` parameter should throw error
test_that("surv.plot throws error for invalid risktable parameter", {
  expect_error(surv.plot(fit =survObject,
                         reference.arm = "1",
                         time.unit = "month",
                         risktable = "invalid"),
               "`risktable` expecting TRUE or FALSE as an argument!")
})

## Test 5: Custom colour assignment should not raise errors
test_that("surv.plot handles default color assignment", {
  expect_silent(surv.plot(fit = survObject,
                          reference.arm = "1",
                          time.unit = "month",
                          col = c("#E41A1C", "#4DAF4A", "#377EB8")))
})

# Test 6: Ensure survival plot handles empty survival object gracefully
test_that("Plotting an empty survival object should raise an error", {
  expect_error(surv.plot(fit = survfit(Surv() ~ 1, data = NULL)))
})

# Test 7: Verify that setting an incorrect reference arm raises error
test_that("Setting an incorrect reference arm should raise an error.", {
  expect_error(surv.plot(fit = survObject,reference.arm = "3"))
})


# Test 8: Verify that statistics is given in %
test_that("Convert y unit into percent.",{
          expect_silent(surv.plot(fit = survObject, y.unit = "percent"))
})

test_that("Convert y unit into percent and check label",{
  expect_silent(surv.plot(fit = survObject,
                          y.unit = "percent",
                          segment.quantile = 0.5,
                          time.unit = "day"))
})

# Test 9: Ensure survival plot can handle quantiles and timepoints
test_that("Ensure `segment.quanitle` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.quantile = 0.5, segment.type = 1))
})

test_that("Ensure `segment.quanitle` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.quantile = 0.5, segment.type = 2))
})

test_that("Ensure `segment.quanitle` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.quantile = 0.5, segment.type = 3))
})

test_that("Ensure `segment.timpoint` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.timepoint = 360, segment.type = 1))
})

test_that("Ensure `segment.timpoint` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.timepoint = 360, segment.type = 2))
})

test_that("Ensure `segment.timpoint` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.timepoint = 360, segment.type = 3))
})


# Test 10: Check `segment.annotation` parameters
test_that("surv.plot throws error for invalid risktable parameter", {
  expect_error(surv.plot(fit =survObject, segment.annotation = "invalid"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survObject,
                          segment.timepoint = 0.5 ,
                          segment.annotation = c(300, 0.125)))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survObject,
                          segment.timepoint = 0.5 ,
                          segment.annotation = "left"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survfit(Surv(time, status) ~ 1, data = lung),
                          segment.timepoint = 0.5 ,
                          segment.annotation = "top"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = 0.5 ,
                          segment.annotation = "top"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = 0.5 ,
                          segment.annotation = "bottomleft"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survfit(Surv(time, status) ~ 1, data = lung),
                          segment.timepoint = 0.5 ,
                          segment.annotation = "none"))
})

# Test 11: Test risktable
test_that("Check if risktable colour can be modified", {
  expect_silent(surv.plot(fit =survObject,
                         risktable.col = c("red", "yellow")))
})

test_that("Ensure that legend can handle invalid argument", {
  expect_error(surv.plot(fit =survObject, legend = "invalid"))
})

test_that("Ensure that legend can be modified manually", {
  expect_silent(surv.plot(fit =survObject, legend.name = "legend"))
})



# Test 12: Test Statistics
test_that("Check logrank statistics", {
  expect_silent(surv.plot(fit =survObject, stat = "logrank"))
})

test_that("Check coxph statistics", {
  expect_silent(surv.plot(fit =survObject, stat = "coxph", stat.position = "bottomleft"))
})

test_that("Check coxph_logrank statistics", {
  expect_silent(surv.plot(fit =survObject, stat = "coxph_logrank", stat.position = "topright"))
})

test_that("Check coxph_logrank statistics", {
  expect_silent(surv.plot(fit =survObject, stat = "coxph_logrank", stat.position = c(170, 0.5)))
})


# Test 13: Test pre defined themes
test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "ESMO"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "ESMO"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "ESMO",
                          risktable = FALSE))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "SAKK",
                          risktable = FALSE))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "SAKK",
                          risktable = FALSE))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "Lancet"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "Lancet"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "JCO"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "JCO"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "WCLC"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "WCLC"))
})



# Test 14: Test grid
test_that("Setting an invalid grid argument.", {
  expect_error(surv.plot(fit = survObject, grid = "invalid"))
})

test_that("Setting an invalid grid argument.", {
  expect_silent(surv.plot(fit = survObject, grid = TRUE))
})


# Test 15: Test statistics for one arm
test_that("Setting an invalid stats argument.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung), stat = "coxph"))
})


# Test 16: Check risktable.title
test_that("Check if risktable.title gives a valid output.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung), risktable.censoring = TRUE))
})

test_that("Check if risktable.title gives a valid output.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung), risktable.censoring = "invalid"))
})

# Test 17: Check confidence band colour
test_that("Check if conf.band.col gives a valid output.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung), conf.band.col = NULL))
})

# Test 18: Invalid `time.unit` parameter should throw error rest not
test_that("Ensure `time.unit` is handles without error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung), time.unit = "invalid"))
})


test_that("Check if conf.band.col gives a valid output.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung), time.unit = "day"))
})

test_that("Check if conf.band.col gives a valid output.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung), time.unit = "year"))
})


# Test 19: Check for short annotation
test_that("Check if short annotation works without an error.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.quantile = 0.5,
                          segment.confint = FALSE))
})

test_that("Check if short annotation works without an error.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.quantile = 0.5,
                          segment.confint = FALSE,
                          time.unit = "day"))
})

test_that("Check if short annotation works without an error.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.quantile = 0.25,
                          segment.confint = FALSE))
})

test_that("Check if short annotation can handle error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.quantile = 0.25,
                          segment.confint = FALSE))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = FALSE,
                          segment.quantile = 0.50))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = FALSE,
                          segment.quantile = 0.25,
                          segment.main = "Quantile"))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = 360,
                          segment.confint = FALSE))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = 360,
                          segment.confint = FALSE,
                          y.unit = "percent"))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.quantile = 0.25,
                          segment.confint = TRUE,
                          y.unit = "percent"))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = FALSE,
                          segment.main = "Segment Title",
                          segment.quantile = 0.50))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = TRUE,
                          segment.quantile = 0.50))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = TRUE,
                          segment.quantile = 0.50,
                          y.unit = "percent"))
})

# Test 20: Check Error of segment
test_that("Check if segment can handle error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.timepoint = 360))
})


# Test 21: Check Error message if no confidence interval should be displayed but number of arms is not equal to 2
test_that("Check if CI with one arm can handle error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.confint = FALSE))
})

# Test 22:
test_that("Check if error with different segment type can handle error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.timepoint = 360,
                         segment.type = 1))
})

test_that("Check if error with different segment type can handle error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.timepoint = 360,
                         segment.type = 2))
})

test_that("Check if error with different segment type can handle error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.timepoint = 360,
                         segment.type = 3))
})

# Test 23: Check Warning message if segment at several time points or quantiles should be added and segment.main is not null
test_that("Check if error with different segment type can handle error.", {
  expect_warning(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                           segment.quantile = c(0.25, 0.5),
                           segment.main = "Test"))
})

test_that("Check if error with different segment type can handle error.", {
  expect_warning(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                           segment.timepoint = c(300, 650),
                           segment.main = "Test"))
})













