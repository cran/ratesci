#' ratesci: A package for computing confidence intervals for various
#' comparisons of independent binomial and Poisson rates.
#'
#' Computes confidence intervals for the rate difference (RD), rate ratio (RR),
#' or odds ratio (OR), or for the single rate (p), for independent binomial or
#' Poisson rates. Includes score-based methods with (or without) skewness
#' correction, developed from the Miettinen-Nurminen and Gart-Nam methods,
#' and the "Method of Variance Estimates Recovery", originating from Newcombe.
#' For the single-stratum case, the SCAS (skewness-corrected asymptotic score)
#' method is recommended (Laud 2017), on the basis of superior equal-tailed
#' coverage.
#'
#' @section ratesci functions:
#' \itemize{
#'   \item scoreci: for score-based confidence intervals
#'   \item scasci: wrapper function to compute SCAS interval
#'   \item tdasci: wrapper function to compute TDAS stratified
#'   interval
#'   \item moverci: for the MOVER method
#'   \item moverbci: wrapper function to compute MOVER-B interval
#'   \item jeffreysci: wrapper function to compute Jeffreys interval for a
#'   single rate
#'   \item pairbinci: for paired binomial data
#'   \item scaspci: non-iterative SCAS method for a single rate
#'   \item rateci: wrapper function for SCAS, Jeffreys or 'exact' methods
#'   for a single rate
#' }
#'
#' @docType package
#' @name ratesci-package
#' @author Pete Laud, \email{p.j.laud@@sheffield.ac.uk}
#' @references
#' Laud PJ. Equal-tailed confidence intervals for comparison of
#' rates. Pharmaceutical Statistics 2017; 16:334-348.
#'
#' Miettinen OS, Nurminen M. Comparative analysis of two rates. Statistics in
#' Medicine 1985; 4:213-226.
#'
#' Gart JJ. Analysis of the common odds ratio: corrections for bias and
#' skewness. Bulletin of the International Statistical Institute 1985,
#' 45th session, book 1, 175-176.
#'
#' Gart JJ, Nam JM. Approximate interval estimation of the ratio of binomial
#' parameters: A review and corrections for skewness. Biometrics 1988;
#' 44(2):323-338.
#'
#' Gart JJ, Nam JM. Approximate interval estimation of the difference in
#' binomial parameters: correction for skewness and extension to multiple
#' tables. Biometrics 1990; 46(3):637-643.
#'
#' Newcombe RG. Interval estimation for the difference between independent
#' proportions: comparison of eleven methods. Statistics in Medicine 1998;
#' 17(8):873-890.
#'
#' Donner A, Zou G. Closed-form confidence intervals for functions of the
#' normal mean and standard deviation. Statistical Methods in Medical Research
#' 2012; 21(4):347-359.
#'
NULL
#"_PACKAGE"
