# Hyperparameter Optimization Technique: Genetic Algorithm
#' @export
hpo_sup_simdata_ds_ori = function(x = list(seed=1, sample_clustersize = NA, sample_to_featureratio=2, datatype="createSCR"), traindata = NA, testdata =NA, splitype = c("old")){

  datasplitfun = list(old = mdatasplit)

  # Define Parameters
  seed_run=x$seed
  sample_clustersize=x$sample_clustersize
  cutratio=x$sample_to_featureratio
  datatype=x$datatype

  # Check if dataset is provided
  if(all(is.na(traindata))|all(is.na(testdata))){
    print("No training/testings Data. Exit")
    return("Exit because training/testing data is not available")
  }

  # Model adds 50 complete rows from test data to training data
  if(datatype == "createSCR"){
    set.seed(seed_run);index=sample(1:nrow(testdata), nrow(testdata)-50)
    testset=testdata[index,]
    train_comp=testdata[-index,]
    len_test_y=length(unique(testset$y))
    if(len_test_y>1){
      testdata=testset
      traindata=rbind(train_comp,traindata)
    }else{print("Cannot join complete rows with training data. Please check the test dataset")}
  }

  # Define Hyperpara to label the df
  hyperpara=data.frame(miss = NA, corr=NA, cutter=ncol(traindata)-1, trainset=datatype, seed=seed_run, data_code=NA, stringsAsFactors = F)

  maximum_clusters=nrow(traindata)/cutratio

  # Define Fitness Function
  GA_FUNCT=function(bin){
    dec_code = GA::binary2decimal(bin)

    # Run the function
    if(dec_code<(maximum_clusters+1) & dec_code>0){
      dfsplit=datasplitfun[[splitype]](traindata, cutratio=cutratio, cluster_number=dec_code)

      # Make sure not to loose complete rows added in the model
      comp_rows = length(which(complete.cases(traindata)))
      if(any(datatype %in% c("createSCR")) & comp_rows-ncol(traindata)>1 & comp_rows/(ncol(traindata)-1) < cutratio & length(dfsplit) >0 & splitype == "old"){

        realclus = length(dfsplit[[1]])
        cdf = traindata[complete.cases(traindata),]
        dfsplit[[1]][[realclus+1]] = traindata[complete.cases(traindata),]
      }

      # Run DMU
      data_prepare=list(list(dfsplit,testdata,traindata))
      bay_res=DMU(hyperpara, data_prep=data_prepare)

      if(length(bay_res[[1]]) != 0){
        if(!is.na(bay_res[[1]]$MSE)){res=-bay_res[[1]]$MSE}
        else{res=-1e6}}
      else{res=-1e6}
    }else(res=-1e6)
    return(Score=res)
  }
  nBits = length(GA::decimal2binary(maximum_clusters))
  set.seed(3)
  GA <<- GA::ga(type = "binary", fitness = GA_FUNCT, nBits = nBits, popSize = 10, maxiter = floor(maximum_clusters/20), maxFitness = -0.001, run=10, pcrossover = 0.8, pmutation = 0.8, parallel = F, monitor = F)
  best_cluster = GA::binary2decimal(GA@solution[1,])
  # print(best_cluster)
  return(best_cluster)
}
