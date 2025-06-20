## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(digits = 3)

## ----setup--------------------------------------------------------------------
library(ratesci)

## -----------------------------------------------------------------------------
data(compress, package = "ratesci")
strat_rd <- scoreci(x1 = compress$event.gcs, 
                   n1 = compress$n.gcs, 
                   x2 = compress$event.control, 
                   n2 = compress$n.control, 
                   contrast = "RD", 
                   stratified = TRUE)
strat_rd$estimates

## -----------------------------------------------------------------------------
strat_rd$pval

## -----------------------------------------------------------------------------
strat_rd$Qtest

## -----------------------------------------------------------------------------
strat_rr <- scoreci(x1 = compress$event.gcs, 
                   n1 = compress$n.gcs, 
                   x2 = compress$event.control, 
                   n2 = compress$n.control, 
                   contrast = "RR", 
                   stratified = TRUE)
strat_rr$Qtest

## -----------------------------------------------------------------------------
strat_rd$stratdata

## -----------------------------------------------------------------------------
strat_rd_rand <- scoreci(x1 = compress$event.gcs, 
                   n1 = compress$n.gcs, 
                   x2 = compress$event.control, 
                   n2 = compress$n.control, 
                   contrast = "RD", 
                   stratified = TRUE,
                   random = TRUE,
                   prediction = TRUE)
strat_rd_rand$estimates

## -----------------------------------------------------------------------------
strat_rd_rand$prediction

