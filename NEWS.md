# Version 0.25 (2021-10-xx)

* Removed overlapping and overflowing content from pdf reports.

* Elapsed time in the web UI now works when it is longer than one day.

* Added a downloadable zip file that contains all the results except for the preprocessed data.

* The minority class (deferrals) are now oversampled in the training data so that the 50% of the last donations
  are deferrals. This is done in baseline, random forest, and support vector machines.
  
* Drop too short time series after preprocessing but before subsetting.

* The shap value computation works baseline, rf, and svm.

* 

# Version 0.24 (2021-09-13)
