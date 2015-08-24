# K2

20% sample baseline
gbm: trees 150 / shrink 0.02 / depth 20 / 3-fold cv

AUC val = 0.7529 (0.24 hrs)

Variations
==========

* Predictor selection univariate AUC >= .52 instead of nzv
Validation set AUC: 0.7554171

* Combine pairs of date variables (d1-d2, d1-d3, …, d2 - d3, …)
Validation set AUC: 0.7540101 #predictors: 626 total time: 0.3912383 minutes
VAR_0217-VAR_0073 comes up high in selected predictors

* DA recoding with same threshold as in actual binning
20% sample : Val set AUC: 0.7554135 #predictors: 605 total time: 0.3704926 minutes
100% sample: Val set AUC: 0.7638571 #predictors: 631 total time: 0.2003584 minutes
Leaderboard:              0.76896
Development set AUC:      0.7754364 (big difference suggests overfitting)

* Same with n.trees = 300 / depth 20 / shrinkage 0.02 / cv.folds = 3
20% sample :
Val AUC: 0.7614085 #predictors: 605 total time: 0.1435924 minutes
Dev AUC: 0.8479056
==> overfitting

*****

* Fix bug in binning - apply binning code not correct

* Num binning (equi-weight not interval) e.g. for vars with > 100 fields
class numeric/integer (is.numeric) and nDistinct > 100 then *replace* with
apply numbinner() to field
bv xx <- numbinner( train_dev$VAR_0002, train_val$VAR_0002, test$VAR_0002, train_dev$target, train_val$target, 10)
TODO: add list of ‘special values’ to treat as NA? or replace by NA before.

* NA treatment. No NA imputation or removal of rows or cols with many NAs - or use caret
imputation for preprocessing (maybe scale as well - pca??). Consider other 
values as NA as well (-1, 999, 999996 etc). Maybe ‘isMissing’ field for each var?

* Count NA’s per row as extra var

* Without symbolic recoding

* With recoding for numerics

* Some numerics have strange ranges - special values 9999 etc.

* Different thresholds for residual bin

* Symbolics with small nDistinct as dummy variables, rest symbolic recoding
caret:
dummies <- dummyVars(target ~ ., data = train_dev)
head(predict(dummies, newdata = etitanic))

* Remove linear combinations (caret)

* Add counts of booleans in subsequent columns
(VAR_0008 .. VAR_0012)

* XGBoost instead of gbm


