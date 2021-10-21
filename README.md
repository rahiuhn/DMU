# Dynamic Model Updating (DMU): A short tutorial
Dynamic model updating (DMU) approach develops statistical models with missing data. DMU uses only the information available in the dataset to prepare the statistical models. The basic framework is to divide the dataset into smaller datasets containing a smaller number of predictors but complete information, and sequentially build the model for each dataset followed by updating the estimates of the predictors after each model (https://doi.org/10.1186/s12859-021-04138-z).

This tutorial will explain how to run the DMU code. The current DMU algorithm can only provide the predictive performance of the test data. It does not provide the final model built for the prediction.

## Installing the packages
This algorithm uses many R packages which needs to be installed or loaded before using this algorithm. 
```
# Versions used in the current algorithm are:
# memoise (>= 1.1.0), dplyr (>= 0.8.4), stringr (>= 1.4.0), stats (>= 3.6.2), scales (>= 1.1.0), caret (>= 6.0-86), 
magrittr (>= 1.5), janitor (>= 1.2.1), MLmetrics (>= 1.1.1), mice (>= 3.8.0), GA (>= 3.2), MCMCpack (>= 1.4-8), 
copula (>= 1.0-0), gtools (>= 3.8.2), mosaic (>= 1.8.2), future (>= 1.20.1), future.apply (>= 1.6.0), missForest (>= 1.4), 
pkgcond (>= 0.1.0), bnstruct (>= 1.0.8), DMwR (>= 0.4.1), VIM (>= 6.1.0), SimMultiCorrData (>= 0.2.2), pbapply (>= 1.4-3)

install.packages("pacman")
library(pacman)
pacman::p_load(memoise, dplyr, stringr, stats, scales, caret, magrittr, janitor, MLmetrics, mice, GA, MCMCpack, copula, 
gtools, mosaic, future, future.apply, missforest, pkgcond, bnstruct, DMwR, VIM, SimMultiCorrData, pbapply)
```

## Generate simulated dataset
The algorithm allows the user to generate an artificial dataset but with limited functionality. The dataset generated contains 20 input features ```varnum``` with 80% of missing values ```maxmiss_per``` in each of the 20 features is missing completely at random. The correlation among 20 features lies in the range of [-0.5,0.5]. Currently, only first three features are allowed to have effect on the model. The coefficient values are (0.2, 0.3, 0.4) with interaction coefficient of 10. The training dataset generated has no complete rows (NCR).
```
dataset = data_sim(varnum =20, # 20 features 
                   maxmiss_per= 0.8, # 80% missing per column 
                   corr_seed=1, # seed number to ensure reproducibility
                   testsize = 1050, # sample size of test data
                   samplesize=4200, # total number of samples generated training +test samples
                   effect="Mar", # Only marginal effects are present in the original model. 
                   max_corr= 0.5) # Correlation among features will lie in between [-0.5, 0.5]
traindata = dataset$train
testdata = dataset$test
truetrain = dataset$truedata

write.csv(traindata,"traindata_SCR_Art.csv")
write.csv(testdata,"testdata_SCR_Art.csv")
write.csv(truetrain,"originaldata_SCR_Art.csv")
```

## Run DMU and compare its performance
Once the training and test dataset are obtained. One can run the DMU algorithm. The function ```sim_fit``` allows the user to run and compare 6 missing imputation methods namely DMU (```DMU```), mean imputation (```mean```), kNN impute (```knn```), Random Forest impute (```rf```), complete case analysis (```reg```), mice (only pmm) (```mice```). Additionally, it provides the true model ```true``` if ```truetrain``` variable contains complete data. In case we want to add some complete rows to training data to change it from NCR to SCR (Some complete rows) dataset, ```datatype``` should be assigned value ```createSCR``` which will randomly transfer 50 samples with complete information from test data to training data. Otherwise, give ```datatype``` suitable value like ```NCR``` or ```SCR```. 
```
# Get Parameters
clustersize= 4 # number of small complete dataset that will created for DMU
sample_to_featureratio = 2 # Minimum sample size to feature ratio to be maintained in small complete datasets
para= list(seed=1, sample_clustersize = clustersize, sample_to_featureratio=sample_to_featureratio, datatype="createSCR")

## Results
res = sim_fit(x = para, traindata= traindata, testdata = testdata, truetrain = truetrain, 
              technique= c("DMU", "knn", "mice", "reg","mean", "rf"), splitype = "old")
res$datatype = "SCR"
finalres = res
finalres
Output:
          Corr        MSE   rsquare         Approach seed datatype
DMU  0.8287515 0.10094285 0.7324693        beta_Prop    1      SCR
knn  0.8247175 0.09526723 0.6425749         beta_knn    1      SCR
mice 0.8496872 0.17535876 0.1247191 beta_Imputed_Reg    1      SCR
reg  0.8278213 0.09995927 0.7302156         beta_Reg    1      SCR
mean 0.8480322 0.11734702 0.7806281        beta_mean    1      SCR
true 0.8810794 0.06532334 0.7796712        beta_true    1      SCR

```

## Hyperparameter optimization
In case, the number of small datasets, k ```clustersize``` is not known. Algorithm can provide its optimal value using genetic algorithm based hyperparameter optimization function ```hyperpara-optimize```.
```
clustersize = hyperpara_optimize(optimtype = "opt_GA", seed=1, traindata= traindata, testdata = testdata, datatype = "createSCR")
clustersize
Output: 4
```

## Limitations of Algorithm
1) This algorithm can only process continuous data
2) Currently, output is only the predictive performance in three metrics namely correlation, mean square error and r-squared
3) It doe not directly provide the final model or feature weights. However, one can determine the weights by looking at function ```DMU```. The result is stored in the data.frame ```bay_reg```.
