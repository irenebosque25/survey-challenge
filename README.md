# Support for Transgender Rights Across Europe — A Survey-Based Analysis
## Project for Survey Methodology II
Authors: Pablo Aísa, Irene Bosque, Mafalda González, and David Pereiro

Date: March 14, 2025

### Project Objectives
This project addresses two main goals:

- Explaining Cross-Country Differences in Support for Transgender Rights:
  
We analyze data from the Eurobarometer 493 to explain why support for transgender individuals obtaining official identity documents varies across countries. The analysis combines individual-level indicators with country-level factors, such as legal protections, cultural values, economic indicators, and social attitudes (sourced from Eurostat, FRA, ILGA-Europe, etc.).

- Building a Predictive Model for Other Countries:
  
Using the insights from the first part, we apply machine learning techniques (logistic regression, decision trees, etc.) to develop a model that predicts support levels in other contexts or countries not included in the original survey. We rigorously calibrate and validate the model to ensure reliability.

### Data Sources
- Special Eurobarometer 493 (2019)
  
A survey across 28 EU countries measuring perceptions of discrimination, including sexual orientation and gender identity.

- Country-Level Indicators
  
Sourced from Eurostat, ILGA-Europe, and similar databases. Includes variables like legal protection scores, LGBTI acceptance indices, GDP, and inequality metrics.

### Methodology
**Data Preprocessing and Missing Data**
We used Multiple Imputation by Chained Equations (MICE) to handle missing data across key variables. This approach helps ensure robustness and reduces bias in the multivariate models.

**Part 1: Multilevel Analysis**
We use multilevel logistic regression to assess support for the question QC19:
_“Transgender people should be allowed to change their official documents to reflect their gender identity.”_

Includes individual fixed effects and country-level random effects.

**Part 2: Predictive Modeling**
We compare different models (logistic regression, random forests, etc.).
Calibration and validation are conducted using cross-validation and metrics such as AUC and accuracy.
The final model enables us to predict support levels in other or future contexts.

### Key Findings

Regarding the results:
- Gender: Being a woman significantly increases support.
- Age: Shows a curvilinear effect—support decreases at older ages.
- Retirement & Religiosity: Being retired or highly religious lowers support; non-religious individuals tend to be more supportive.
- Personal Satisfaction & Contact with LGBTI individuals: These have strong and significant positive effects, highlighting the importance of social interaction.
- Political Ideology: Conservatism reduces support, but this effect is moderated by GDP per capita.

In terms of random effects, we find variation between countries in both intercepts and slopes (age, gender, ideology), indicating that these factors do not influence attitudes uniformly across contexts.

Overall, the model confirms that support for trans rights is a multifaceted phenomenon, shaped by both individual characteristics and broader societal structures.

### Final Output

The full step-by-step process, including code, figures, model diagnostics, and interpretation, is documented in the HTML report:

📄 challenge.html

### Reproducibility

This repository includes:

- Fully documented R code for data cleaning, analysis, and modeling.
- Use of MICE for imputing missing data.
- Simulated or anonymized data when needed.
- Clear structure via RMarkdown (.Rmd) files.
