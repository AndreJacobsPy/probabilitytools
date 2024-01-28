#' pr_binomial
#'
#' Function for calculating probability for Bernouli Trials.
#'
#' @param n -> the number of trials
#' @param j -> the number of events to occur
#' @param p -> the probability of j
#' @return probability of Binomial Random Variable
#' @examples
#' pr_binomial(10, 2, 1/6)
#' > 0.29071
#'
#' pr_binomial(n = 5, j = 5, p = 1/2)
#' > 0.03125
#'
#' @export
pr_binomial <- function(n, j, p) {
  combinations <- choose(n, j)
  return(combinations * p^j * (1 - p)^(n - j))
}

#' mean_binomial
#'
#' function that calculates the mean of outcomes, or the
#' expected value.
#'
#' @param n -> number of trials
#' @param p -> the probability of success
#' @return the mean or expected value
#' @export
mean_binomial <- function(n, p) {
  return(n * p)
}

#' var_binomial
#'
#' function that calculates the variance of outcomes.
#'
#' @param n -> number of trials
#' @param p -> the probability of success
#' @return the variance of trials
#' @export
var_binomial <- function(n, p) {
  return(n * p * (1 - p))
}