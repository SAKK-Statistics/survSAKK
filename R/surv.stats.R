# Function for rounding p-value ####
## two significant digit e.g. p = 0.43 or 0.057
## if 0.001 > p > 0.0001, then round to one significant digit
## else p < 0.0001

round.pval <- function(x){
  if (x < 0.0001){
    pval <- "p < 0.0001"
  } else if (x <= 0.001 && x >= 0.0001){
    pval <- paste("p =", format(signif(x, digits = 1), scientific = FALSE))
  } else {
    pval <- paste("p =", format(signif(x, digits = 2), scientific = FALSE))
  }
  return(pval)
}


surv.stats <- function(stat = "logrank"){
    # Logrank test ####
    ## To compare the survival curves of two or more groups
    logrank <- fit$call                     # Extract the call from survival object
    logrank$conf.type <- NULL
    logrank$conf.int <- NULL
    logrank[1] <- call("survdiff")          # Modify the call from survfit() to survdiff()
    logrank <- eval(logrank)

    # Recalculating p-Value
    logrankpval <- as.numeric(format.pval(1 - pchisq(logrank$chisq, df = length(logrank$n) - 1), esp = 0.001))
    logrankpval <- round.pval(logrankpval)

    # Cox proportional hazard regression ####
    ## To describe the effect of variables on survival
    model <- fit$call                       # Extract the call from survival object
    model$conf.type <- NULL
    model$conf.int <- NULL
    model[1] <- call("coxph")               # Modify the call from survfit() to coxph()
    model <- summary(eval(model))

    if(stat == "logrank"){
      stats <- logrankpval
      } else if(stat == "coxph"){
        stats <- paste0("HR = ",
                        round(model$conf.int[,"exp(coef)"], digits = 2),
                        " (95% CI:",
                        round(model$conf.int[,"lower .95"], digits = 2),
                        " to ",
                        round(model$conf.int[,"upper .95"], digits = 2),
                        ",", logrankpval,
                        ")")
      }
}

