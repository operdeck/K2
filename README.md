# K2

20% sample baseline
gbm: trees 150 / shrink 0.02 / depth 20 / 3-fold cv

AUC val = 0.7529 (0.24 hrs)

==========
before weekend merge from  b459341..e2b9f7e

Duration: 6.380494 hrs
gbm(formula = target ~ ., distribution = "bernoulli", data = train_dev, 
    n.trees = 300, interaction.depth = 30, shrinkage = 0.02, 
    cv.folds = 5, verbose = T)
A gradient boosted model with bernoulli loss function.
300 iterations were performed.
The best cross-validation iteration was 300.
There were 673 predictors of which 591 had non-zero influence.

Validation set AUC: 0.7734578
LB: 0.77844 (! big improvement)


=> after merge; with trees 150 / shrink 0.02 / depth 20 / 3-fold cv 20% sample result in
Val AUC: 0.7554135 #predictors: 605 total time: 0.4384021 minutes
Dev AUC: 0.8126395

300 trees:
Val AUC: 0.7614224 #predictors: 605 total time: 0.6780234 minutes
Dev AUC: 0.8479096

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

* 24/8. Apply symbin to integers as well (assume they're really categorical)
20%:
Val AUC: 0.7591139 #predictors: 1125 total time: 0.01985351 minutes
Dev AUC: 0.8510494
==> worse; even more overfitting

* 24/8. Same, only symbin to integers with < 100 distinct values
Val AUC: 0.7629714 #predictors: 1030 total time: 0.01752806 minutes
Dev AUC: 0.8537177
==> better; but still strongly overfitting

* 24/8. Fix bug in binning
Done - binning bug was no real issue, but binindex now reflects behaviour order. Saving plots on disk.
NB: plots do not use the binindex as expected, see VAR_0001 plot. Need
to re-order by binindex first?

* 24/8. Overnight run. DA residual threshold 0.005; ntrees = 1000; interaction.depth = 30; cv.folds 5
GLM benchmark Val AUC: 0.7655162 Dev AUC: 0.7801752
GBM aborted for unknown reason

* 25/8. Re-benchmarking. Residual 0.005, gbm 300/20/3/0.02. 100% sample.
GLM benchmark Val AUC: 0.7655162 Dev AUC: 0.7801752
GBM: Out of memory
XGB: ...

*****

* More experiments in DA symbin residual setting (1%, 2%, 0.05%, 0%)? 0% should
be same as no symbolic recoding.

* Num binning (equi-weight not interval) e.g. for vars with > 100 fields
class numeric/integer (is.numeric) and nDistinct > 100 then *replace* with
apply numbinner() to field
bv xx <- numbinner( train_dev$VAR_0002, train_val$VAR_0002, test$VAR_0002, train_dev$target, train_val$target, 10)
TODO: add list of ‘special values’ to treat as NA? or replace by NA before.
TODO: 'integer' cols are probably symbolic - treat as such

* NA treatment. No NA imputation or removal of rows or cols with many NAs - or use caret
imputation for preprocessing (maybe scale as well - pca??). Consider other 
values as NA as well (-1, 999, 999996 etc). Maybe ‘isMissing’ field for each var?

* Count NA’s per row as extra var

* Count consecutive booleans as extra var (VAR_0008 .. VAR_0012)

* Join state info as extra vars (e.g. size, population, ...)

* Some numerics have strange ranges - special values 9999 etc.

* Symbolics with small nDistinct as dummy variables, rest symbolic recoding
caret:
dummies <- dummyVars(target ~ ., data = train_dev)
head(predict(dummies, newdata = etitanic))

* Remove linear combinations (caret)

* XGBoost instead of gbm
