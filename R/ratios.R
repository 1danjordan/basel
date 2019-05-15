#' Leverage ratio
#'
#' @param t1_capital Tier 1 Capital
#' @param exposure Total exposure
#' @export
#' @examples
#' leverage_ratio(10, 100)

leverage_ratio <- function(t1_capital, exposure) t1_capital / exposure

#' Capital ratio
#'
#' @param capital Capital
#' @param rwa Risk weighted assets
#' @export
#' @examples
#' capital_ratio(10, 100)

capital_ratio <- function(capital, rwa) capital / rwa
