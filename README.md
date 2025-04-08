# Hedge Fund Alpha: Factor Selection, AUM Optimization, and Crisis Stress Testing

------------


This project models hedge fund performance using factor-based regressions and asset pricing frameworks. It applies CAPM, Fama-French, and Carhart models alongside Elastic Net for variable selection, examining both traditional factor premia and Vanguard fund returns. The analysis evaluates alpha generation, AUM capacity, and downside resilience during the 2008–2009 crisis.

## Data

- WRDS: Fama-French factors, Momentum, Vanguard funds returns, macro fund data 
- AQR Capital Management: [Market Premia Factors Data Set](https://www.aqr.com/Insights/Datasets/Century-of-Factor-Premia-Monthly)

Note: Proprietary WRDS data cannot be publicly shared

## Objectives
- Identify key drivers of fund's alpha 
- Evaluate alpha vs. AUM tradeoffs
- Evaluate added value to investors 
- Test crisis-period performance for downside protection claims


## Methods
- Data cleaning and monthly merging

- Regressions: CAPM, Fama-French 3-factor, Carhart 4-factor

- Elastic Net and OLS on Vanguard and premia-based predictors

- Alpha estimation and optimal AUM via Berk & Green (2004)

- 2008–2009 crisis stress testing using historical factor returns

## Files

- [Fund Performance Factor Modeling.R](https://github.com/omarbenkaddour/fund-performance-factor-modeling/blob/main/Macro%20Fund%20Analysis.R) : Full analysis pipeline

- [Modeling Hedge Fund Alpha: Factor Selection, AUM Optimization, and Crisis Stress Testing.pdf](https://github.com/omarbenkaddour/fund-performance-factor-modeling/blob/main/Modeling%20Alpha%20and%20Capacity%20Constraints%20in%20Macro%20Hedge%20Fund.pdf) : write-up of methods and findings 


