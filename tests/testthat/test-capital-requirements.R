context("test-capital-calcs")

EAD <- 100
PD  <- 0.05
LGD <- 0.8

test_that("capital_requirement is correct", {

  corp  <- capital_requirement("corporate", PD, LGD)
  sme   <- capital_requirement("SME", PD, LGD)
  rev   <- capital_requirement("revolving", PD, LGD)
  mort  <- capital_requirement("mortgage", PD, LGD)
  other <- capital_requirement("other", PD, LGD)

  expect_equal(corp, 0.01702401)
  expect_equal(sme, 0.01702401)
  expect_equal(rev, 0.007065785)
  expect_equal(mort, 0.01913075)
  expect_equal(other, 0.008572082)
  expect_error(capital_requirement(portfolio = "gobbledygook", PD, LGD, M))

})


test_that("RWA is correct", {

  expect_equal(rwa(0.01, 100), 12.5)

})

test_that("expected loss fn works", {

  expect_equal(expected_loss(EAD, PD, LGD), 4)

})

test_that("asset correlations are correct", {

  corp  <- asset_corr("corporate", c(0, 1))
  rev   <- asset_corr("revolving")
  mort  <- asset_corr("mortgage")
  other <- asset_corr("other", c(0, 1))

  expect_equal(corp, c(0.24, 0.12))
  expect_equal(rev, 0.04)
  expect_equal(mort, 0.15)
  expect_equal(other, c(0.16, 0.03))

})
