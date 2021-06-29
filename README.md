# ShinyApp

Repository with the application script and the example files used to test the app.
This prject has been made in the framework of the subject Model evaluation, display and monitoring. So, it have not an advance scripting level.

The application itself get a .csv or .arff file with the following conditions:

- The file must be a dataset where the rows are individuals and the columns the attributes of these individuals, with the last column been a binary classification, in a character factor format (ex.: "good"/"bad" instead 1/0).
- The data must have been cleaned and don't have missing values. 

Once the file is given, the user can select a classification method within a list and start the creation of the model. When the process ends shows a graph with the [ROC curve](https://en.wikipedia.org/wiki/Receiver_operating_characteristic) with a 5-fold cross-validation and the area under the curve (AUC).

In addition, is possible give an instance (individual/row) of the given dataset in .csv format, without the classification class, to predict the class an show a weights plot to explain which attributes support and contradicts the conclusion.

In the following link you can see online how is the app.
[https://datascience-edm.shinyapps.io/ShinyApp_EDM/](https://datascience-edm.shinyapps.io/ShinyApp_EDM/)
