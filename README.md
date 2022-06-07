# Marketing Campaign in Portuguese Bank - Logistic Regression 

This dataset refers to the direct marketing campaigns of a Portuguese banking institution based on phone calls. 
It comprises of 45,211 bank customers’ information which was collected from May 2008 to November 2010.


Dataset 

Variable Name | Description 
--------------|------------
Age           | Age of the client 
Balance       | Average yearly balance in euros 
Day           | Last contact day of the month 
Duration      | Last contact duration in seconds 
campaign      | number of contacts performed during this campaign and for this client 
Pdays         | number of days that passed by after the client was last contacted from the previous campaign 
previous      | number of contacts performed before this campaign and for this client 
job           | type of job 
marital       | marital status 
education     | education level 
default       | has credit in default?
housing       | has housing loan?
Loan          | has personal loan?
contact       | contact communication type 
month         | last conact month of the year 
poutcome      | outcome of previous marketing campaign 
y             | client subscribed to term deposit or not(binary response)

Here, y is response variable and the rest are predictor variables. Logistic regression is used because the response is binary and predictor variables are quantitative and categorical 

Data Cleaning and manipulation 

Here, the dataset is checked for missing values. But it is observed that there are no missing values;
Duration is converted from seocnds to minutes 
Month is converted to numeric(for example :"jan" to 1 );
'unknown','primary','secondary','tertiary' is converted to 0,1,2,3;
Binary variables like y, default, housing, loan has been converted from "No" to 0 and "yes" to 1 

Research Questions

what are the attributes that the clients will be more likely to subscribe to a term deposit?
Which predictors can accurately predict which client will subscribe a term deposit?

For initial selection of variables, chi square test is performed and based on the p value, it can be concluded that all the above variables except " default" will be chosen as initial variables 

Multicolinearity 

For detecting multicollinearity, Correlation plot and Variance Inflation factor tests are used. From the plots it can be observed that there is no significant correlation of any predictor variable with response except for duration. All the predictor variables have VIF less than 5. Hence no multicollinearity exists. 

