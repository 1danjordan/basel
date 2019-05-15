#' Basic Indicator Approach for Operational Risk
#'
#' @param gross_income Annual ross income over previous 3 years
#' @param alpha
#' @export

basic_indicator <- function(gross_income, alpha = 0.15) {

  if(length(gross_income != 3)) stop("`gross_income` must contain 3 years of data")
  # number of years gross income was positive
  n <- sum(gross_income > 0)
  sum(gross_income) * alpha / n
}

#' Standardised Approach for Operational Risk
#'
#' @param corp_fi
#' @param sales_trading
#' @param retail
#' @param commercial
#' @param payments
#' @param agency
#' @param asset_mngt
#' @param brokerage
#' @export
#'
standardised_approach <- function(corp_fi, sales_trading, retail, commercial,
                                  payments, agency, asset_mngt, brokerage) {
  sum(
    corp_fi * 0.18,
    sales_trading * 0.18,
    retail *  0.12,
    commercial * 0.15,
    payments * 0.18,
    agency * 0.15,
    asset_mngt * 0.12,
    brokerage * 0.12
  ) / 3
}
