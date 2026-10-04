# Week 3 – Statistical Analysis and Predictive Modeling using R

## Project
Statistical analysis and multiple linear regression using the public STAR98 California school district dataset.

## Objective
- Perform hypothesis testing
- Analyze correlations and assumptions
- Build a multiple linear regression model
- Use an 80/20 train-test split
- Perform 10-fold cross-validation
- Evaluate MAE, RMSE and R²
- Diagnose residuals and multicollinearity

## Target
`PercentAbove` = percentage of students above the national median on the mathematics section.

## Key Results
- Pearson r (PCTAF vs PercentAbove): 0.500
- Test MAE: 6.28
- Test RMSE: 7.32
- Test R²: 0.863
- Mean 10-fold CV R²: 0.689

## Repository Structure
```text
data/
R/
outputs/
reports/
README.md
```

## Important Note
This project describes statistical associations and predictive performance. It does not establish causal relationships between school characteristics and student outcomes.
