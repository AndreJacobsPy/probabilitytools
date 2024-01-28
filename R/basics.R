#' pr.binomial
#'
#' Function for calculating probability for Bernouli Trials.
#'
#' @param n - the number of trials
#' @param j - the number of events to occur
#' @param p - the probability of j
#' @return probability of Binomial Random Variable
#' @examples
#' pr.binomial(10, 2, 1/6)
#' > 0.29071
#'
#' pr.binomial(n = 5, j = 5, p = 1/2)
#' > 0.03125
#'
#' @export
pr.binomial <- function(n, j, p) {
  combinations <- choose(n, j)
  return(combinations * p^j * (1 - p)^(n - j))
}