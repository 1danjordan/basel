context("Capital Requirements")
library(basel)

EAD <- 100
PD <- 0.05
LGD <- 0.8
M <- 2.5

# Testing that capital_adequacy runs for:
#  * corporate
#  * SME
#  * revolving
#  * mortgage
#  * other

expect_gt(capital_requirement(PD, EAD, LGD, M, portfolio = "corporate"), 0)
expect_gt(capital_requirement(PD, EAD, LGD, M, portfolio = "SME"), 0)
expect_gt(capital_requirement(PD, EAD, LGD, M, portfolio = "revolving"), 0)
expect_gt(capital_requirement(PD, EAD, LGD, M, portfolio = "mortgage"), 0)
# Leave until worked this one out
# expect_gt(capital_requirement(PD, EAD, LGD, M, portfolio = "other"), 0)
