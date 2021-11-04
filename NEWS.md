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
