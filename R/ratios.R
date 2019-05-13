#' Leverage ratio
#'
#' @param t1_capital
#' @param exposure
#' @export
#' @examples
#' leverage_ratio(10, 100)

leverage_ratio <- function(t1_capital, exposure) t1_capital / exposure

#' Capital ratio
#'
#' @param capital total capital
#' @param rwa risk weighted assets
#' @export
#' @examples
#' capital_ratio(10, 100)

capital_ratio <- function(capital, rwa) capital / rwa
