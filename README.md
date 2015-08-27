# K2 Ideas

Benchmark at 20%
GLM benchmark Val AUC: 0.7094192 Dev AUC: 0.8668802
* earlier introduced extra predictor for NA count per row
* not even reading train ID anymore
* num NAs 'xtraNumNAs' - review performance
* add K-S and overlap statistics in DA
* refactored DA function
* replaced integers nDistinctTresholdForSymbinIntegers by check for 100% overlap to recode as symbolic

GLM benchmark Val AUC: 0.7103337 Dev AUC: 0.8655112

* symbinResidualThreshold <- 0.001
GLM benchmark Val AUC: 0.6990035 Dev AUC: 0.8826152

* symbinResidualThreshold <- 0.005
GLM benchmark Val AUC: 0.7053411 Dev AUC: 0.8705255

* symbinResidualThreshold <- 0.02
GLM benchmark Val AUC: 0.7158952 Dev AUC: 0.854645

* correlationThreshold <- 0.80 (ipv .96)

Feature engineering
-------------------

* Combine pairs of date variables (d1-d2, d1-d3, …, d2 - d3, …)
[Done]. E.g. VAR_0217-VAR_0073 comes up high in selected predictors

* Count NA’s per row as extra var
[Done]. Seems to help.

* Count consecutive booleans as extra var (VAR_0008 .. VAR_0012)

* Join City/County info as extra vars (e.g. size, population, ...)

* Professions columns - organize manually; derive some values

* Some numerics have strange ranges - special values 9999 etc.

Data Analysis
-------------

* Predictor selection based on univariate performance threshold instead of nzv heuristics 
[Done]

* Apply symbin to integers with < 100 distinct values
[Done]

* Numbinning (equiweight + recoding) for remaining numerics
[Done]

* Check unique values in test vs train to determine whether categorical binning is ok
check also K-S for numerics

* Choose between categorical and symbolic binning based on performance instead of heuristics

* NA treatment. No NA imputation or removal of rows or cols with many NAs - or use caret
imputation for preprocessing (maybe scale as well - pca??). Consider other 
values as NA as well (-1, 999, 999996 etc). Maybe ‘isMissing’ field for each var?

* Symbolics with small nDistinct as dummy variables, rest symbolic recoding
caret:
dummies <- dummyVars(target ~ ., data = train_dev)
head(predict(dummies, newdata = etitanic))

* Remove linear combinations (caret)


Model building
--------------
* XGBoost 
[Done]. Seems better than gbm but perhaps not a big difference. Need to re-compare at some point.
Parameters need to be tuned further to prevent overfitting.


Tuning
------
* GLM benchmark
[Done]. Helps in univariate analysis verification.

* Tune DA symbin threshold

*****
