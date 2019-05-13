#' Expected Loss
#'
#' @param PD  Probability of Default
#' @param EAD Exposure at Default
#' @param LGD Loss Given Default
#' @export
#' @examples
#' expected_loss(100, 0.05, 0.8)

expected_loss <- function(EAD, PD, LGD) {
  EAD * PD * LGD
}

#' Capital Requirement
#'
#' Compute the capital requirement for an exposure
#' according to its probabiltiy of default, loss given default
#' and asset class. This capital requirement is not weighted
#' by exposure amount.
#'
#' @references
#' An Explanatory Note on the Basel II IRB Risk Weight Functions,
#' Basel Committee on Banking Supervision
#'
#'
#' @param portfolio The type of portfolio
#' @inheritParams expected_loss
#' @export
#' @examples
#' capital_requirement("corporate", 0.05, 0.8)

capital_requirement <- function(portfolio, PD, LGD, M = 2.5) {

  R <- asset_corr(portfolio, PD)
  K(PD, LGD, R, M)
}

#' Capital requirement formula
#'
#' @param PD probability of default
#' @param LGD loss given default
#' @param R correlation
#' @param M maturity (defaults to 2.5)
#' @references
#' An Explanatory Note on the Basel II IRB Risk Weight Functions,
#' Basel Committee on Banking Supervision
#' @export
#' @examples
#' R <- asset_corr("mortgage")
#' K(0.01, 0.8, R)

K <- function(PD, LGD, R, M = 2.5) {

  b <- (0.11852 - 0.05478 * log(PD)) ^ 2
  maturity_adj <-  ((1 + (M - 2.5)) * b) / (1 - 1.5 * b)

  # worst case default rate via Gaussian copula
  wcdr <- pnorm(sqrt(1/(1-R)) * qnorm(PD) + sqrt(R/(1-R)) * qnorm(0.999))

  (LGD * (wcdr - PD)) * maturity_adj
}

#' Risk Weighted Assets
#'
#' According to capital of 8% of RWAs ie. 1/12.5 = 0.08
#'
#' @param K   Capital Requirement
#' @param EAD Exposure at Default
#' @export
#' @examples
#' K <- capital_requirement("mortgage", PD = 0.01, LGD = 0.8)
#' rwa(K, 100)

rwa <- function(K, EAD) {
  K * 12.5 * EAD
}

#' Get the asset correlations according to asset class
#'
#' Basel II defines five asset classes for computing
#' capital requirements:
#'
#' \itemize{
#'   \item corporate
#'   \item SME
#'   \item revolving
#'   \item mortgage
#'   \item other
#' }
#'
#' @param class the asset class
#' @param PD PD (required for 'corporate', 'SME' and 'other')
#' @export
#' @examples
#' asset_corr("corporate", 0.01)
#' asset_corr("revolving")
#' asset_corr("mortgage")
#' asset_corr("other", 0.01)

asset_corr <- function(class, PD = NULL) {

  if (class %in% c("corporate", "SME")) {

    if(is.null(PD)) stop("PD must be supplied 'corporate' and 'SME' classes")

    # technically should include size adjustment in here...

    weighting <- (1 - exp(-50 * PD)) / (1 - exp(-50))
    0.12 * weighting + 0.24 * (1 - weighting)

  } else if (class == "revolving") {

    0.04

  } else if (class == "mortgage") {

    0.15

  } else if (class == "other"){

    if(is.null(PD)) stop("PD must be supplied 'other' asset classe")

    weighting <- (1 - exp(-35 * PD)) / (1 - exp(-35))
    0.03 * weighting + 0.16 * (1 - weighting)

  } else {
    stop(class, " is not a valid asset class")
  }
}
