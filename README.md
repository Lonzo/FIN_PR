# FIN_PR


INPUT
- Portfolio
-- Gesamtwert
-- Einzelne Aktien
--- Gewichtung
--- Probability
--- expected returns
--- Volatilität



OUTPUT
- Verteilungsgraphen der simulierten Gewinne





HOWTO
https://www.investopedia.com/walkthrough/corporate-finance/4/return-risk/expected-return.aspx

Risk
To calculate the risk of a two-stock portfolio, first take the square of the weight of asset A and multiply it by square of standard deviation of asset A. Repeat the calculation for asset B. Next, multiply all of the following: the weight of asset A, the weight of asset B, standard deviation of asset A, standard deviation of asset B and the number 2. You will have three figures thus far; add these figures and take the square-root of the result. 


Variance
Portfolio variance measures the dispersion of returns of a portfolio. It is calculated using the standard deviation of each security in the portfolio and the correlation between securities in the portfolio.
To calculate the portfolio variance of securities in a portfolio, multiply the squared weight of each security by the corresponding variance of the security and add two multiplied by the weighted average of the securities multiplied by the covariance between the securities.
Assume that an analyst writes a report on a company and, based on the research, assigns the following probabilities to next year's sales:
Scenario	Probability	Sales ($ Millions)
1	0.10	$16
2	0.30	$15
3	0.30	$14
3	0.30	$13
The analyst's expected value for next year's sales is (0.1)*(16.0) + (0.3)*(15.0) + (0.3)*(14.0) + (0.3)*(13.0) = $14.2 million.
Calculating variance starts by computing the difference in each potential sales outcome from $14.2 million, then squaring:
Scenario	Probability	Deviation from Expected Value	Squared
1	0.1	(16.0 - 14.2) = 1.8	3.24
2	0.30	(15.0 - 14.2) = 0.8	0.64
3	0.30	(14.0 - 14.2) = - 0.2	0.04
4	0.30	(13.0 - 14.2) = - 1.2	1.44
Variance then weights each squared deviation by its probability, giving us the following calculation:
(0.1)*(3.24) + (0.3)*(0.64) + (0.3)*(0.04) + (0.3)*(1.44) = 0.96


Standard Deviation
Standard deviation (σ) is found by taking the square root of variance.


