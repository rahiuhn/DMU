## Define some parameters
varnum=20; miss=0.8; correlation=0.5; tr_multi=157.5; sample_to_featureratio=2; test_size = 1050

## Prepare Artificial Train and Test Dataset
dataset = DMUlite:::data_sim(varnum, maxmiss_per=miss, corr_seed=1, testsize = test_size,
                         samplesize=((varnum*tr_multi)+test_size), effect="Mar", max_corr= correlation, high_dim = F)

traindata = dataset$train
testdata = dataset$test
truetrain = dataset$truedata

write.csv(traindata,"traindata_SCR_Art.csv")
write.csv(testdata,"testdata_SCR_Art.csv")
write.csv(truetrain,"originaldata_SCR_Art.csv")

## DMU optimization
clustersize = DMUlite::hyperpara_optimize(optimtype = "opt_GA", seed=1, traindata= traindata, testdata = testdata, datatype = "createSCR")
clustersize


## Define Parameters
clustersize= 4
para= list(seed=1, sample_clustersize = clustersize, sample_to_featureratio=sample_to_featureratio, datatype="createSCR")

## DMU Results
res = DMUlite::sim_fit(x = para, traindata= traindata, testdata = testdata, truetrain = truetrain, technique= c("DMU", "knn", "mice", "reg","mean", "rf"), splitype = "old") #
res$datatype = "SCR"
finalres = res
finalres
