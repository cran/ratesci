## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(digits = 3)

## ----setup--------------------------------------------------------------------
library(ratesci)

## -----------------------------------------------------------------------------
scoreci(x1 = 5, n1 = 56, x2 = 0, n = 29, skew = FALSE)$pval[, 1:2]
scoreci(x1 = 5, n1 = 56, x2 = 0, n = 29, skew = FALSE, contrast = "RR")$pval[, 1:2]
scoreci(x1 = 5, n1 = 56, x2 = 0, n = 29, skew = FALSE, contrast = "OR")$pval[, 1:2]
suppressWarnings(k_pearson <- chisq.test(x = matrix(c(5, 51, 0, 29), nrow = 2), correct = FALSE)$statistic)
pchisq(k_pearson * ((56+29-1)/(56+29)), df = 1, lower.tail = FALSE)

## -----------------------------------------------------------------------------
scoreci(x1 = 7, n1 = 34, x2 = 1, n = 34, skew = FALSE)$pval[, 1:2]
scoreci(x1 = 7, n1 = 34, x2 = 1, n = 34, skew = FALSE, contrast = "RR")$pval[, 1:2]
scoreci(x1 = 7, n1 = 34, x2 = 1, n = 34, skew = FALSE, contrast = "OR")$pval[, 1:2]
suppressWarnings(k_pearson <- chisq.test(x = matrix(c(7, 1, 27, 33), nrow = 2), correct = FALSE)$statistic)
pchisq(k_pearson * ((34+34-1)/(34+34)), df = 1, lower.tail = FALSE)

## -----------------------------------------------------------------------------
scoreci(x1 = 7, n1 = 34, x2 = 1, n = 34, skew = TRUE)$pval[, 1:2]

## -----------------------------------------------------------------------------
reprove <- scoreci(x1 = 245, n1 = 356, x2 = 270, n2 = 370, theta0 = -0.125)
reprove$estimates
reprove$pval[, 3:6]

## -----------------------------------------------------------------------------
x1 = c(21, 76, 73, 75) 
n1 = c(29, 96, 124, 107) 
x2 = c(19, 73, 91, 87) 
n2 = c(27, 95, 130, 118)
data_array <- aperm(array(c(x1, x2, n1 - x1, n2 - x2), dim = c(4, 2, 2)), c(2, 3, 1))

reprove_strat <- scoreci(x1 = c(21, 76, 73, 75), 
                         n1 = c(29, 96, 124, 107), 
                         x2 = c(19, 73, 91, 87), 
                         n2 = c(27, 95, 130, 118),
                         stratified = TRUE,
                         theta0 = -0.125) 
reprove_strat$pval
reprove_cmh <- mantelhaen.test(data_array, correct = FALSE)
reprove_cmh$p.value


## -----------------------------------------------------------------------------
pairbinci(x = c(1, 1, 7, 12), skew = TRUE)$pval
pairbinci(x = c(1, 1, 7, 12), skew = FALSE)$pval
pairbinci(x = c(1, 1, 7, 12), skew = FALSE, contrast = "RR")$pval
mcnem <- mcnemar.test(x = matrix(c(1, 1, 7, 12), nrow = 2), correct = FALSE)$statistic
names(mcnem) <- NULL
pchisq(mcnem * (21-1)/21, df = 1, lower.tail = FALSE)

## -----------------------------------------------------------------------------
scoreci(x1 = 7, n1 = 34, contrast = "p", theta0 = 0.1)$pval

