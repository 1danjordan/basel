#' Required Market Risk Capital
#'
#'
#'
#' @param var 10-day 99% VaR
#' @param var_avg 60 day average 10-day 99% VaR
#' @param svar stressed 10-day 99% VaR
#' @param svar_avg 60 day average stressed 10-day 99% VaR
#' @param m regulatory multiplier
#' @export

market_capital <- function(var, var_avg, svar, svar_avg, m) {
  max(var, m * var_avg) + max(svar, m * svar_avg)
}
