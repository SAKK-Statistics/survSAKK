

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

## Test 5: Custom color assignment should not raise errors
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


