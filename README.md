# CaseStudy1DDS-
FRITOLAY DDS CASE STUDY

#INTRODUCTION

Employee attrition is an important business challenge for Frito-Lay because employee turnover can create significant recruiting, replacement, and productivity costs. The objective of this project was to identify employee characteristics associated with attrition and develop a predictive model that could help Frito-Lay identify employees who may be at higher risk of leaving.

Using employee-level data containing demographic, job, compensation, satisfaction, and tenure-related characteristics, we first explored patterns in attrition and then conducted statistical tests to identify variables significantly associated with employee turnover. We subsequently developed and compared multiple classification models to determine which approach could best predict attrition while meeting the requirement of at least 60% sensitivity and 60% specificity.

#EXECUTIVE SUMMARY

The analysis identified several employee characteristics that were significantly related to attrition. In the initial statistical screening, significant associations were found for factors including OverTime, StockOptionLevel, JobRole, TotalWorkingYears, MonthlyIncome, JobInvolvement, JobLevel, YearsAtCompany, MaritalStatus, YearsInCurrentRole, YearsWithCurrManager, Age, WorkLifeBalance, Department, EnvironmentSatisfaction, JobSatisfaction, DistanceFromHome, and BusinessTravel.
When these factors were considered together in a multivariable logistic regression model, three factors stood out as the strongest independent predictors of attrition: OverTime, JobInvolvement, and StockOptionLevel. Employees who worked overtime had substantially higher odds of attrition, while higher job involvement was associated with lower attrition. Stock option level also showed a significant but nonlinear relationship with attrition. JobRole, particularly the Sales Representative role, was another important factor in the model, but was not considered in the top 3.
I evaluated three predictive approaches: K-nearest neighbors (KNN), Naive Bayes(NB), and Logistic Regression(LR). All three models were evaluated using sensitivity, specificity, accuracy, and balanced accuracy. Logistic regression provided the strongest overall balance while also offering greater interpretability for business decision-making.

#Table Comparing Model Outcomes
Model	Sensitivity	Specificity	Accuracy	Balanced Accuracy
KNN	    71.4%	      63.5%	    64.8%	    67.4%
NB	    61.9%	      69.4%	    68.2%	    65.7%
LR      69.1%	      76.3%	    75.1%	    72.7%

The final logistic regression model used a 20% probability threshold and met the required minimum of 60% sensitivity and 60% specificity. Its 69.1% sensitivity means it correctly identified approximately 69% of employees who actually left, while its 76.3% specificity means it correctly identified approximately 76% of employees who stayed.
Overall, the results suggest that Frito-Lay could use employee characteristics such as overtime, job involvement, and stock option level to identify employees at elevated risk of attrition and support targeted retention strategies. Because the analysis is observational, these factors should be interpreted as associations rather than proof of causation.

#ADDITIONAL FINDINGS AND INFERENCE

Attrition Is Highly Imbalanced: Only 16.1% of employees in the dataset experienced attrition, while 83.9% remained with the company. This imbalance is important when evaluating predictive models because a model could achieve high overall accuracy simply by predicting that most employees will stay. For this reason, we emphasized sensitivity, specificity, and balanced accuracy rather than relying on accuracy alone.

Statistical Significance Does Not Necessarily Mean Predictive Importance: Several variables were statistically significant during the initial screening but did not remain statistically significant in the multivariable logistic regression model. This suggests that some of the observed relationships may overlap with other employee characteristics.
The multivariable analysis therefore provided additional insight beyond individual statistical tests by evaluating the factors simultaneously. This is why OverTime, JobInvolvement, and StockOptionLevel were identified as the strongest independent factors rather than simply selecting the variables with the smallest individual p-values.

The Model Can Support Targeted Retention Rather Than Company-Wide Intervention: The final logistic regression model classified 64 of the 300 competition employees as higher-risk for attrition using the 20% probability threshold. This suggests that predictive analytics could be used to focus retention resources on a smaller group of employees rather than applying the same intervention to the entire workforce.

Potential Financial Impact: The analysis also demonstrated the potential economic value of targeted retention. Under the given financial scenario using the model-identified employees, a 50% successful retention rate and a replacement cost equal to 100% of annual salary would produce approximately $2.13 million in potential net savings, after accounting for the $200 intervention cost per targeted employee.
