# Version 0.32 (2022-03-??)

* Handling of sex in the user interface is now either pooled, stratified, male or female. Basically, the last two
  are new options and allow running only one sex.
  
* Visualize the dependence of deferral on the age group.

* Added Dutch hyperparameter values

* Histograms of donation specific variables are stored into a dataframe on a file. Bins with
  less than five points are set to count zero.
  
* "Final mode"" is enabled.

# Version 0.31 (2022-03-03)

* Reduced the memory usage dramatically in the LMM model.

# Version 0.30 (2022-02-10)

* The Finnish hyperparameters have now been reoptimized.

* Now using package `ranger` instead of `randomForest` for random forests.

* Tried to fix the output problem of LMMs in windows.

* Use SMOTE sampling instead of upsampling in RF and SVM to handle class imbalance.

* Export dataset sizes to sizes.cvs file.

* Use SVM's raw decision values instead of probabilities, because Platt scaling randomly fails. 

* To ease debugging one can download (most of) the fitted models and their train/validate input datas with
  `docker cp nameoftherunningcontainer: /tmp/tmp_rds.zip .`
  Note that these container private data.

# Version 0.29 (2022-01-12)

* The shap values of linear models are now really exported to the shap-value.csv file.

* SVM is now using radial kernel instead of polynomial. Also, the Finnish hyperparameters have been optimized again.

# Version 0.28 (2022-01-04)

* Reordered the operations in preprocessing. Preprocessing must be done again, because of this change.

* Optimize the probability threshold that is used to compute the F1 score and the confusion matrix. The threshold 
  is such that it maximizes the F1 score on the train data. Hopefully this will get rid of the NA F1 scores.
  
* Shap value computation of the linear mixed models now working.
  
* Reoptimized Finnish random forest hyperparameters (`mtry`, `nodesize`, `ntree`). Had to modify Caret to
  allow extra hyperparameters.
  
* The id column from file `shap-value.csv` was removed. In addition, the rows in `shap-value.csv` and prediction.csv
  are permuted. The shap values are computed from a sample of 1000 donors. In file `shap-value.csv` only the normalized
  variables computed from the sample are shown. So, if there are more than 1000 donors in the test set, the individual data
  should be messed up enough to prevent finding out the original data or ids.

# Version 0.27 (2021-11-17)

* The number of cores used in parallel computation can now be specified in the user interface.
  By reducing the number of cores one may try to reduce the memory usage.
  
* Fixed a bug in stratified sampling. Now if you specify sample size, e.g. 10 000 and stratify by sex is selected.
  then in the sample there will 10 000 male donors and 10 000 female donors.
  
* Variable `nb_donat` is no longer included as a predictor by default. Fixed small upsampling problem with baseline model.
  Fixed logging of data exclusions. 

* The downloadable preprocessed data is no longer filtered by time-series length.

* The final number of donations, donors and deferrals are now reported in a table form in each Rmd.

* Don't drop donor anymore even if date_first_donation field is NA.

* Implemented the five-year time window. And the recent donations variable now refers to the last five years.

# Version 0.26 (2021-10-29)

* Disabled parallelism in computing confidence intervals of AUPR and F1. This avoids crashing due to insufficient memory
  on systems for little memory.
  
# Version 0.25 (2021-10-27)

* Removed overlapping and overflowing content from pdf reports.

* Elapsed time in the web UI now works when it is longer than one day.

* Added a downloadable zip file that contains all the results except for the preprocessed data.

* The minority class (deferrals) are now oversampled in the training data so that the 50% of the last donations
  are deferrals. This is done in baseline, random forest, and support vector machines.
  
* Drop too short time series after preprocessing but before subsetting.

* The shap value computation works baseline, rf, and svm.

* Belgian data is now handled differently.

* The container can now also run preprocessing only without fitting any models.

# Version 0.24 (2021-09-13)
