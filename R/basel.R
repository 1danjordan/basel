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
#' Under the advanced approach, banks must calculate the effective
#' maturity (M)9 and provide their own estimates of PD, LGD and EAD.
#'
#' M is calculated as the maximum remaining time (in years) that
#' the obligor is permitted to take to fully discharge its contractual
#' obligations, including principal, interest and fees under the
#' terms of the loan agreement.
#'
#' Effective maturity is measured as a Macaulay duration under the
#' assumption that interest rates are zero.
#'
#' Check out [this explanation](https://financetrain.com/effective-maturity-in-basel-ii/)
#'
#' @references
#' An Explanatory Note on the Basel II IRB Risk Weight Functions,
#' Basel Committee on Banking Supervision
#'
#'
#' @inheritParams expected_loss
#' @param M Effective Maturity
#' @param portfolio The type of portfolio

capital_requirement <- function(PD, EAD, LGD, M, portfolio) {

  if (portfolio == "corporate") {
    # compute correlation
    weighting <- (1 - exp(-50 * PD)) / (1 - exp(-50))
    R <- 0.12 * weighting + 0.24 * (1 - weighting)

    # b is the maturity adjustment
    b <- (0.11852 - 0.05478 * log(PD))^2

    # In the paper  N() is the normal CDF and G() is the inverse normal CDF
    systematic_factor <- sqrt(1/(1-R)) * qnorm(PD) + sqrt(R/(1-R)) * qnorm(0.999)

    # this needs a better/more accurate name
    maturity_adjustment <- ((1 + (M - 2.5)) * b) / (1 - 1.5 * b)

    K <- (LGD * pnorm(systematic_factor) - LGD * PD) * maturity_adjustment

  } else if (portfolio == "SME") {
    weighting <- (1 - exp(-50 * PD)) / (1 - exp(-50))

    # S is "size adjustment for annual sales between €5M and €50M"
    # Need to make this ... parameter?
    R <- 0.12 * weighting + 0.24 * (1 - weighting) - 0.04 * (1 - (S-5)/45)


  } else if (portfolio == "revolving") {
    R <- 0.04


  } else if (portfolio == "mortgage") {
    R <- 0.15


  } else if (portfolio == "other"){
    # See the Basel explanatory note
    # Correlation is same as Basel but low/high correlations different
  } else {
    # Raise error that `portfolio` didn't match any options
  }
  K
}

#' Risk Weighted Assets
#'
#' @param K   Capital Requirement
#' @param EAD Exposure at Default

rwa <- function(K, EAD) {
  K * 12.5 * EAD
}
