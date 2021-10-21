#' The functions in this file prepare the models
#' @param miss is the maximum percentage of missing values allowed in the feature. All features with more missing values will be dropped from the analysis process.
#' @param correlation is the maximum positive and negative correlation allowed in the predictor correlation matrix. All features with higher correlation values are dropped from the analysis process.
#' @param dataset is the code for the dataset for which this study will be performed. 1: is for CHSI dataset and 3: is for SWAN dataset.
#' @param seed is for result repetition.
#' @param sample_clustersize is the number of pieces the samples has to be fragmented for DMU.
#' @param sample_to_featureratio is the minimum sample to feature ratio allowed in each of the cluster. Any cluster with less than the minimum sample to feature ratio is dropped from the analysis process.
#' @param datatype determines whether the training data will have any sample with complete data or not. "Both" means that training data will have samples containing complete and incomplete data. "Incomplete" means that training data will have samples containing only incomplete data.
#'
#'@export
runmodel = function(technique, traindata, testdata, trainset=datatype, seed=seed_run, cluster_number=sample_clustersize, ...){

  # Default Feature Values
  fullvarlist = list(data_code=1, splitype = c("old"), cutter=ncol(traindata)-1, cutratio=2, corr=0.52,miss = 0.8)
  varlist= list(...)
  ## Replace standard variable values with user defined values
  if(length(varlist) >0){
    targetvar = names(fullvarlist)[names(fullvarlist) %in% names(varlist)]
    dumpvar = sapply(targetvar, function(x) fullvarlist[[x]]<<-varlist[[x]])
  }

  #Split Type
  splitlist = list(old = datasplit, Old = datasplit)

  ## All Standard Values
  # data_code= fullvarlist$data_code;
  splitype= fullvarlist$splitype;
  # cutter = as.numeric(fullvarlist$cutter);
  cutratio= as.numeric(fullvarlist$cutratio);
  # corr = as.numeric(fullvarlist$corr);
  # miss = as.numeric(fullvarlist$miss)

  hyperpara = data.frame(miss = NA, corr=NA, cutter=NA, trainset=trainset, seed=seed, data_code=NA, stringsAsFactors = F)
  # Model adds 50 complete rows from test data to training data
  if(trainset == "createSCR"){
    set.seed(seed);index=sample(1:nrow(testdata), nrow(testdata)-50)
    testset=testdata[index,]
    train_comp=testdata[-index,]
    len_test_y=length(unique(testset$y))
    if(len_test_y>1){
      testdata=testset
      traindata=rbind(train_comp,traindata)
    }else{print("Cannot join complete rows with training data. Please check the test dataset")}
  }
  res = list()
  if(any(technique %in% "reg")){
    data_prepare=list(list(NA, comp_set = testdata,incomplete = traindata))
    res[["reg"]]=reg_results(hyperpara, data_prep=data_prepare)[[1]]
  }
  if(any(technique %in% "mice")){
    data_prepare=list(list(NA, comp_set = testdata,incomplete = traindata))
    res[["mice"]]=mice_results(hyperpara, data_prep=data_prepare)[[1]]
  }
  if(any(technique %in% "rf")){
    data_prepare=list(list(NA, comp_set = testdata,incomplete = traindata))
    res[["rf"]]=rf_imp_results(hyperpara, data_prep=data_prepare)[[1]]
  }
  if(any(technique %in% "mean")){
    data_prepare=list(list(NA, comp_set = testdata,incomplete = traindata))
    res[["mean"]]=mean_imp_results(hyperpara, data_prep=data_prepare)[[1]]
  }
  if(any(technique %in% "knn")){
    data_prepare=list(list(NA, comp_set = testdata,incomplete = traindata))
    res[["knn"]]=knn_results(hyperpara, data_prep=data_prepare)[[1]]
  }
  if(any(technique %in% "DMU")){
    # hyperpara$trainset = "incomplete"
    dfsplit=splitlist[[splitype]](traindata, cutratio=cutratio, cluster_number=cluster_number)

    # Make sure not to loose complete rows added in the model
    comp_rows = length(which(complete.cases(traindata)))
    if(any(trainset %in% c("createSCR")) & comp_rows-ncol(traindata)>1 & comp_rows/(ncol(traindata)-1) < cutratio & length(dfsplit) >0 & splitype == "old"){
      realclus = length(dfsplit[[1]])
      dfsplit[[1]][[realclus+1]] = traindata[complete.cases(traindata),]
    }

    # Run DMU
    data_prepare=list(list(dfsplit,comp_set = testdata, incomplete = traindata))
    res[["DMU"]]=mDMU(hyperpara, data_prep=data_prepare)[[1]]
  }
  fin_res=do.call(rbind, res)
  return(fin_res)
}

#' @export
sim_fit=function(x = list(varnum=10, miss=0.1, correlation=0.52, seed=1, tr_multi=90,  sample_clustersize = 8, sample_to_featureratio=2, datatype="createSCR"), technique= "DMU", traindata = NA, testdata =NA, truetrain = NA, splitype = "old"){

  # Define Parameters
  # varnum=x$varnum
  # missing_percent=x$miss
  # corr_range=x$correlation
  seed_run=x$seed
  # tr_multi=x$tr_multi
  sample_clustersize=x$sample_clustersize
  cutratio=x$sample_to_featureratio
  datatype=x$datatype

  # Check the dataset
  if(all(is.na(traindata))|all(is.na(testdata))){
    print("No training/testings Data. Exit")
    return("Exit because training/testing data is not available")
  }

  # Run the Models
  # Define Hyperpara to label the df
  hyperpara=data.frame(miss = NA, corr=NA, cutter=ncol(traindata)-1, trainset=datatype, seed=seed_run, data_code=NA, stringsAsFactors = F)

  res = list()
  modelfunction = list(runmodel = runmodel)

  res = lapply(technique, function(x){
    modelfunction[[1]](technique = x, traindata = traindata, testdata = testdata, trainset = datatype, cutratio=cutratio,seed = seed_run, cluster_number = sample_clustersize, splitype = splitype)
  })

  # Get True Res
  data_prepare=list(list(NA, comp_set = testdata,incomplete = truetrain))
  res[["true"]]=true_results(hyperpara, data_prep=data_prepare)[[1]]

  fin_res=do.call(rbind, res)
  fin_res = fin_res[, c(-1,-3)]
  return(fin_res)
}
