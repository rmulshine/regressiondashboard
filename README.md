# regressiondashboard
Creates a dashboard to perform several different regression analyses using R Shiny.

Note: Following the guidance of the Notre Dame Athletic Department and Department of ACMS, the datasets for these projects will be held private.

Purpose: The purpose of this dashboard is to identify training and condition metrics that are most correlated with on-field success for numerous different sports. The dashboard connects Notre Dame's strength & conditioning coaches to their data by taking simple inputs from the coaches and returning easy-to-read insights into their data. Coaches are able to find correlations between training metrics of their players that were not previously understood with this dashboard.

Features:

1. GPS Data Batch Regression

The batch regression feature for the GPS data contains 2 datasets. The user selects the desired dataset, from which the dashboard updates variable names and filter options. The user then selects one desired X variable and any number of desired Y variables. The user also sets the filter on the dataset, including observations from specific player positions (ex. midfielder) and statuses (ex. starter). Once the batch regression is run, the application will return a table, giving the r^2, adj. r^2, p-value, and linear equation of each simple linear equation (X variable vs. the first Y variable selected, then X vs. the second Y selected, etc.). The user has the option to download this table as a .csv file.


2. GPS Data Multiple Linear Regression

The multiple linear regression feature for the GPS data contains 2 datasets. The user selects the desired dataset, from which the dashboard updates variable names and filter options. The user then selects any number of desired X variables and one Y variable. The user also sets the filter on the dataset, including observations from specific player positions (ex. midfielder) and statuses (ex. starter). The output will include 4 tabs: 1) equation and table of predictor names, p-values and slopes; 2) a table of each predictor's standard error and the model's r^2 and adj. r^2 values; 3) a plot of the fitted values versus residual values to check regression assumptions; 4) the normal Q-Q plot and Q-Q line to check remaining regression assumptions.


3. GPS Data Simple Linear Regression

The simple linear regression feature for the GPS data contains 2 datasets. The user selects the desired dataset, from which the dashboard updates variable names and filter options. The user then selects one desired X variable and one desired Y variable. The user also sets the filter on the dataset, including observations from specific player positions (ex. midfielder) and statuses (ex. starter). The application returns the results of a simple linear regression model run according to the given x and y variable inputs. The output includes 4 tabs: 1) a scatter plot with the trend line, along with the regression equation; 2) a table of the intercept term and x-variable values, p-values, and the model's r^2 value; 3) a plot of the fitted values versus residual values to check regression assumptions; 4) the normal Q-Q plot and Q-Q line to check remaining regression assumptions.


4. Force Plate Data Batch Regression

The batch regression feature for the Force Plate data has the same attributes and outputs as the GPS Data Batch Regression feature, except it is localized to one specific dataset, rather than having the user choose between 2 datasets.
