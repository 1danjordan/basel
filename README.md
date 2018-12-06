# Basel

Basel is an R package for calculating capital requirements according to Basel III. It's primary purpose is as a learning device. I'm developing it to better understand how capital requirements are calculated, why they are calculated this way and what it is sensitive to.

This package follows the standardised approach for measuring counterparty credit risk (SA-CCR) exposures which took affect in January 2017. You can find out more about SA-CCR [here](https://www.bis.org/publ/bcbs279.htm).

SA-CRR is a bottom up risk framework, where the loss distribution for each position is estimated and capital is computed by combining these loss distributions together to define the portfolio's loss distribution. There are many advantages and disadvantages to this approach. 

## Asymptotic Single Factor Model

See the [explanatory note](https://www.bis.org/bcbs/irbriskweight.pdf) on the IRB risk weights.

The ASFM is the theoretical model that underpins SA-CRR. Plan is to understand this model, it's strengths and weaknesses. 

We want to derive and explain PD, LGD, EAD and the reasoning for certain decisions in the definitiions of those - for example the constant LGD and undrawn amount EAD portion for standardised IRB - where do they come from? Maturity is another example.

Then expected loss and unexpected loss, which comes from the variance of a binomial model.

Also look at time horizons and confidence levels. Time horizon of 1 year with 99.9% confidence level? How does this coincide with reality and correlated losses? How sensitive is the capital calculation to time horizon changes or confidence level changes? 

Also an opportunity to examine diversification e.g. risk of the portfolio is much less than the sum of the individual risk levels by the UL equation. 

## Economic Capital

How is economic capital defined in this framework?

## Previous Capital Models

There were older models used in the past - what were those and how did they work?

## Alternative Capital Adequacy Frameworks

What might be the next step in SA-CCR? How is capital calculated for banks in other jurisdictions that don't follow Basel III? 

## Rounding Errors

This is probably silly, but I wonder what effect rounding has on total capital. Banks have lots of assets and these are sensitive calculations. My calculator is spitting out the wrong answer by 21,000 for UL because it doesn't have enough decimal places.

# Operational Risk

Three frameworks currently:

  * Basic indicator approach
  * Standardised approach
  * Advanced measurement approach

# Market Risk

I have't a clue about this...

Check out this [BIS page](https://www.bis.org/bcbs/publ/d437.htm). BIS published new market risk guidelines in 2016. 