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
#' @param S Annual Sales in millions of Euros
#' @param portfolio The type of portfolio

capital_requirement <- function(PD, LGD, M, S = NULL, portfolio) {

  # In these functions:
  #  * R is the correlation
  #  * b is the maturity adjustment
  #  * pnorm() refers to N() the normal CDF
  #  * qnorm() refers to G() the inverse normal CDF

  K <- function(PD, LGD, R) {
    LGD * pnorm(sqrt(1/(1-R)) * qnorm(PD) + sqrt(R/(1-R)) * qnorm(0.999)) - LGD * PD
  }


  if (portfolio %in% c("corporate", "SME")) {

    # For small and medium enterprises with annual Sales Turnover below €50M,
    # the correlation may be adjusted. S is the enterprises annual sales
    if(portfolio == "SME" && !is.null(S)) {
      R <- 0.12 * weighting + 0.24 * (1 - weighting) - 0.04 * (1 - (S-5)/45)
    }
    # Should probably include a an option for Asset Value Correlation
    # AVC = 1.25 for LGFI

    weighting <- (1 - exp(-50 * PD)) / (1 - exp(-50))
    R <- 0.12 * weighting + 0.24 * (1 - weighting)

    b <- (0.11852 - 0.05478 * log(PD))^2

    K(PD, LGD, R) * ((1 + (M - 2.5)) * b) / (1 - 1.5 * b)

  } else if (portfolio == "revolving") {
    R <- 0.04
    K(PD, LGD, R)

  } else if (portfolio == "mortgage") {
    R <- 0.15
    K(PD, LGD, R)

  } else if (portfolio == "other"){
    # See the Basel explanatory note
    # Correlation is same as Basel but low/high correlations different
  } else {
    # Raise error that `portfolio` didn't match any options
    stop("Error: you did not pass a valid `portfolio`")
  }
}


#' Risk Weighted Assets
#'
#' @param K   Capital Requirement
#' @param EAD Exposure at Default

rwa <- function(K, EAD) {
  K * 12.5 * EAD
}
