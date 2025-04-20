# DMAS (Data Mining Analysis System)
DMAS is a web-based data mining software developed using the R programming language and incorporates various data mining techniques. The initial development utilized a coal dataset; however, the system is designed to support any dataset that follows the same structure.

The system is divided into seven sections, which can be broadly categorized into four main groups:

1-Data Preparation and Evaluation

2-Feature Selection

3-Model Evaluation

4-Prediction

In the first step, users can upload their own dataset. Once uploaded, the entire dataset is displayed in a tabular format. Users can then specify the training and testing split percentages, which will be applied in subsequent data mining and modeling processes, as illustrated in the figure below.

The second step focuses on feature dependency, where users can explore correlations among variables using the Pearson correlation coefficient.

In the feature ranking step, users can choose between two techniques—Cubist and Bagging—to determine the importance of variables within the dataset.

The prediction section enables users to train models using the previously selected features. Two different prediction techniques are available for this purpose.

In the model evaluation section, users can assess the performance of their models using metrics such as MAPE (Mean Absolute Percentage Error) and R-squared. The system also allows users to compare the model’s predicted target values with the actual values from the dataset to evaluate the system’s reliability.

Finally, in the Prediction on New Data section, users can input a new dataset to be processed by the trained model. This step follows the evaluation and accuracy analysis presented in the previous section.

In the visualization section, a scatterplot is generated to illustrate the relationships among the variables, providing a clear and intuitive visual analysis.


