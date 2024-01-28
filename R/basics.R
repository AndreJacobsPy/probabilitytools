#' simple function for cleaner roots.
#'
#' @param value the value to apply root to.
#' @param power the value to raise value to.
#' @return the new value raised to the power.
#' @examples
#' power(2, 2)
#' > 4
#'
#' power(2, 3)
#' > 8
#'
power <- function(value, power) value^power