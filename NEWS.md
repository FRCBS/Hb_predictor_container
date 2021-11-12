# Version 0.27 (2021-11-xx)

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
