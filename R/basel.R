#' Expected Loss
#'
#' @param PD  Probability of Default
#' @param EAD Exposure at Default
#' @param LGD Loss Given Default
#'
#' @example
#'
#' @export

expected_loss <- function(PD, EAD, LGD) {
  PD * EAD * LGD
}

#' Unexpected Loss
#'
#' @inheritParams expected_loss

unexpected_loss <- function(PD, EAD, LGD) {
  lgd_var <- var(LGD)
  pd_var <- var(PD)

  EAD * sqrt(PD * lgd_var + LGD^2 * pd_var)
}

#' Capital Requirement
#'
#' @references An Explanatory Note on the Basel II IRB
#' Risk Weight Functions, Basel Committee on Banking Supervision
#'
#' @inheritParams expected_loss
#' @param portfolio The type of portfolio

capital_requirement <- function(PD, EAD, LGD, portfolio) {

  if(portfolio == "corporate") {
    # compute correlation
    weighting <- (1 - exp(-50 * PD)) / (1 - exp(-50))
    R <- 0.12 * weighting + 0.24 * (1 - weighting)

    # maturity adjustment
    b <- (0.11852 - 0.05478 * log(PD))^2
  }

  K <- LGD * dnorm()

}

#' Risk Weighted Assets
#'
#' @param K   Capital Requirement
#' @param EAD Exposure at Default

rwa <- function(K, EAD) {
  K * 12.5 * EAD
}
