
## The Purpose of the Application and Data Set

The purpose of this app is to provide a basic exploratory data analysis (EDA) and modeling using a multiple linear regression, regression tree, and random forest models. The app has guidance tabs that will lead the user from the general information about the app, data set, and models to the final predictions made on the best model chosen by the user dynamically.

The data set used in this application was the famous data set created by the statistician Ronald Fisher (1936) in his paper *The use of multiple measurements in taxonomic problems as an example of linear discriminant analysis*. Fisher is responsible for several advances in statistics and this data set can be considered one of the excelent sources of numbers for testing new methods and models. Therefore, this data set was chosen to be contemplated in this app as an example of its use. The iris data set has 5 variables, each representing a characteristic of iris flowers, as shown below.

   * **Sepal.Length** - the length of the sepal of iris flowers.
   * **Sepal.Width** - the width of the sepal of iris flowers.
   * **Petal.Length** - the length of the petal of iris flowers.
   * **Petal.Width** - the width of the petal of iris flowers.
   * **Species** - the particular species of iris flowers, being setosa, versicolor, and virginica the sampled examples.

For this application, we will try to model *Sepal.Length* in function of the other numerical variables. The categorical variable *Species* is not considered in the modeling step, but it is contemplated in the **Data Exploration** tab for filtering the categories.

## User Information

This app contains 3 operating tabs: **Data Exploration**, **Modeling**, and **Data**. The **Data exploration** tab contains univariate and multivariate subtabs that allow the user to perform a more in-depth graphical and numerical analysis for the modeling step.    
   The **Modeling** tab contains 3 subtabs, *Modeling Info* that will describe the models utilized in this app, *Model Fitting* tab which takes care of the data split, variable selection by the user for each model, testing errors, and choice of best model. Finally, the **Prediction** subtab will dynamically select the predictor variables chosen by the user as requisite of **Model Fitting** and the best model, also chosen by the user, for a prediction task in which the user has to input values for the pre-selected predictor variables according to the selected model as the best one among the 3 tested.    
   The last tab, **Data** regards to manipulation of the iris data set. The user can filter the data set and export this filter as an excel or csv file to their local machine easily by following the friendly looking interface provided.
