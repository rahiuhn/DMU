# Supervised optimization and dataset data structure and datasplit
#' @export
hpo_sup_data_ds = function(x = list(varnum=100, miss=0.1, correlation=0.52, seed=1, tr_multi=90,  sample_clustersize = NA, sample_to_featureratio=2, datatype="incomplete"), traindata, testdata, setting="Correlation", var="Mar", main_var=10, var_effect=0.5, correlation_var=15, high_dim=T, test_size=1000, Missingdata = T){

  #Decode
  varnum=x[[1]]
  missing_percent=x[[2]]
  corr_range=x[[3]]
  seed_run=x[[4]]
  tr_multi=x[[5]]
  sample_clustersize=x[[6]]
  cutratio=x[[7]]
  datatype=x[[8]]

  # Generate the dataset
  dataset = dataset(varnum = varnum, maxmiss_per=missing_percent, seed=seed_run, train_sample=varnum*tr_multi, setting=setting, var=var,  main_var=main_var, var_effect=var_effect, correlation_var=correlation_var, correlation_val=corr_range, high_dim=high_dim, test_size=test_size, Missingdata = Missingdata)

  traindata = dataset$train
  testdata = dataset$test

  # Define Hyperpara to label the df
  hyperpara = data.frame(miss = missing_percent, corr=corr_range, cutter=ncol(traindata)-1, trainset=datatype, seed=seed_run, data_code=1, stringsAsFactors = F)

  maximum_clusters=nrow(traindata)/cutratio
  GA_FUNCT=function(bin){
    dec_code = GA::binary2decimal(bin)
    if(dec_code<(maximum_clusters+1) & dec_code>0){
      dfsplit=mdatasplit(traindata, cutratio=cutratio, cluster_number=dec_code)
      data_prepare=list(list(dfsplit,testdata,traindata))
      bay_res=mDMU(hyperpara, data_prep=data_prepare)
      if(length(bay_res[[1]]) != 0){
        if(!is.na(bay_res[[1]]$MSE)){res=-bay_res[[1]]$MSE}
        else{res=-10}}
      else{res=-10}
    }else(res=-10)
    return(Score=res)
  }
  nBits = length(GA::decimal2binary(maximum_clusters))
  set.seed(3)
  GA <<- GA::ga(type = "binary", fitness = GA_FUNCT, nBits = nBits, popSize = 10, maxiter = floor(maximum_clusters/20), maxFitness = 0.1*100000, run=10, pcrossover = 0.8, pmutation = 0.8, parallel = F, monitor = F)
  best_cluster = GA::binary2decimal(GA@solution[1,])
  return(best_cluster)
}

# Unsupervised optimization and dataset data structure and datasplit
#' @export
hpo_unsup_data_ds = function(x = list(varnum=100, miss=0.1, correlation=0.52, seed=1, tr_multi=9,  sample_clustersize = NA, sample_to_featureratio=2, datatype="incomplete"), traindata, testdata, setting="Correlation", var="Mar", main_var=10, var_effect=0.5, correlation_var=15, high_dim=T, test_size=1000, Missingdata = T){

  #Decode
  varnum=x[[1]]
  missing_percent=x[[2]]
  corr_range=x[[3]]
  seed_run=x[[4]]
  tr_multi=x[[5]]
  sample_clustersize=x[[6]]
  cutratio=x[[7]]
  datatype=x[[8]]

  # Generate the dataset
  dataset = dataset(varnum = varnum, maxmiss_per=missing_percent, seed=seed_run, train_sample=varnum*tr_multi, setting=setting, var=var,  main_var=main_var, var_effect=var_effect, correlation_var=correlation_var, correlation_val=corr_range, high_dim=high_dim, test_size=test_size, Missingdata = Missingdata)

  traindata = dataset$train
  testdata = dataset$test
  vectraindata = as.vector(as.matrix(traindata[,!names(traindata) %in% "y"]))
  vec_td_len = length(vectraindata[!is.na(vectraindata)])
  # str(traindata)

  # Define Hyperpara to label the df
  hyperpara = data.frame(miss = missing_percent, corr=corr_range, cutter=ncol(traindata)-1, trainset=datatype, seed=seed_run, data_code=1, stringsAsFactors = F)

  maximum_clusters=nrow(traindata)/cutratio
  GA_FUNCT=function(bin){
    # dec_code = GA::binary2decimal(bin)
    # if(dec_code<(maximum_clusters+1) & dec_code>0){
    dfsplit=mdatasplit(traindata, cutratio=cutratio, cluster_number=round(bin))
    data_prepare=list(list(dfsplit,testdata,traindata))
    if(length(dfsplit) == 0){res=0/vec_td_len}
    else{
      dfspltc = bind_rows(dfsplit[[1]]) #do.call(cbind.fill, dfsplit[[1]])
      # cat(nrow(dfspltc))
      vecdfsplit = as.vector(dfspltc[,!names(dfspltc) %in% "y"])
      vec_dfs_len = length(vecdfsplit[!is.na(vecdfsplit)])
      res= vec_dfs_len/vec_td_len
    }
    return(Score=res)
  }
  # nBits = length(GA::decimal2binary(maximum_clusters))
  set.seed(3)
  GA <<- GA::ga(type = "real-valued", fitness = GA_FUNCT, lower = 1, upper = maximum_clusters, popSize = 10, maxiter = floor(maximum_clusters/20), run=10, pcrossover = 0.8, pmutation = 0.4, parallel = F, monitor = T)
  best_cluster = round(GA::binary2decimal(GA@solution[1,]))
  return(best_cluster)
}

# Unsupervised optimization and dataset data structure and mixed datasplit
#' @export
hpo_unsup_data_mixed = function(x = list(varnum=100, miss=0.1, correlation=0.52, seed=1, tr_multi=9,  sample_clustersize = NA, sample_to_featureratio=2, datatype="incomplete"), traindata, testdata, setting="Correlation", var="Mar", main_var=10, var_effect=0.5, correlation_var=15, high_dim=T, test_size=1000, Missingdata = T){

  #Decode
  varnum=x[[1]]
  missing_percent=x[[2]]
  corr_range=x[[3]]
  seed_run=x[[4]]
  tr_multi=x[[5]]
  sample_clustersize=x[[6]]
  cutratio=x[[7]]
  datatype=x[[8]]

  # Generate the dataset
  dataset = dataset(varnum = varnum, maxmiss_per=missing_percent, seed=seed_run, train_sample=varnum*tr_multi, setting=setting, var=var,  main_var=main_var, var_effect=var_effect, correlation_var=correlation_var, correlation_val=corr_range, high_dim=high_dim, test_size=test_size, Missingdata = Missingdata)

  traindata = dataset$train
  testdata = dataset$test
  vectraindata = as.vector(as.matrix(traindata[,!names(traindata) %in% "y"]))
  vec_td_len = length(vectraindata[!is.na(vectraindata)])
  # str(traindata)

  # Define Hyperpara to label the df
  hyperpara = data.frame(miss = missing_percent, corr=corr_range, cutter=ncol(traindata)-1, trainset=datatype, seed=seed_run, data_code=1, stringsAsFactors = F)

  maximum_clusters=nrow(traindata)/cutratio
  # print(maximum_clusters)
  GA_FUNCT=function(bin){
    # dec_code = GA::binary2decimal(bin)
    # if(dec_code<(maximum_clusters+1) & dec_code>0){
    # cat(round(bin), " ")
    dfsplit=sdatasplit(traindata, cutratio=cutratio, cluster_number=round(bin))
    data_prepare=list(list(dfsplit,testdata,traindata))
    if(length(dfsplit[[1]]) == 0){res=0/vec_td_len}
    else{
      dfspltc = bind_rows(dfsplit[[1]]) #do.call(cbind.fill, dfsplit)
      # cat(nrow(dfspltc), " ")
      vecdfsplit = as.vector(dfspltc[,!names(dfspltc) %in% "y"])
      vec_dfs_len = length(vecdfsplit[!is.na(vecdfsplit)])
      res= vec_dfs_len/vec_td_len
    }
    # }else(res=-10)
    return(Score=res)
  }
  # nBits = length(GA::decimal2binary(maximum_clusters))
  set.seed(3)
  GA <<- GA::ga(type = "real-valued", fitness = GA_FUNCT, lower = 1, upper = maximum_clusters, popSize = 10, maxiter = floor(maximum_clusters/20), run=10, pcrossover = 0.8, pmutation = 0.4, parallel = F, monitor = T)
  best_cluster = round(GA::binary2decimal(GA@solution[1,]))
  return(best_cluster)
}

# Unsupervised optimization and dataset data structure and univariate datasplit
#' @export
hpo_unsup_data_uni = function(x = list(varnum=100, miss=0.1, correlation=0.52, seed=1, tr_multi=9,  sample_clustersize = NA, sample_to_featureratio=2, datatype="incomplete"), traindata, testdata, setting="Correlation", var="Mar", main_var=10, var_effect=0.5, correlation_var=15, high_dim=T, test_size=1000, Missingdata = T){

  #Decode
  varnum=x[[1]]
  missing_percent=x[[2]]
  corr_range=x[[3]]
  seed_run=x[[4]]
  tr_multi=x[[5]]
  sample_clustersize=x[[6]]
  cutratio=x[[7]]
  datatype=x[[8]]

  # Generate the dataset
  dataset = dataset(varnum = varnum, maxmiss_per=missing_percent, seed=seed_run, train_sample=varnum*tr_multi, setting=setting, var=var,  main_var=main_var, var_effect=var_effect, correlation_var=correlation_var, correlation_val=corr_range, high_dim=high_dim, test_size=test_size, Missingdata = Missingdata)

  traindata = dataset$train
  testdata = dataset$test
  vectraindata = as.vector(as.matrix(traindata[,!names(traindata) %in% "y"]))
  vec_td_len = length(vectraindata[!is.na(vectraindata)])
  # str(traindata)

  # Define Hyperpara to label the df
  hyperpara = data.frame(miss = missing_percent, corr=corr_range, cutter=ncol(traindata)-1, trainset=datatype, seed=seed_run, data_code=1, stringsAsFactors = F)

  maximum_clusters=nrow(traindata)/cutratio
  # print(maximum_clusters)
  GA_FUNCT=function(bin){
    # dec_code = GA::binary2decimal(bin)
    # if(dec_code<(maximum_clusters+1) & dec_code>0){
    # cat(round(bin), " ")
    dfsplit=ssdatasplit(traindata, cutratio=cutratio, cluster_number=round(bin))
    data_prepare=list(list(dfsplit,testdata,traindata))
    if(length(dfsplit[[1]]) == 0){res=0/vec_td_len}
    else{
      dfspltc = bind_rows(dfsplit[[1]]) #do.call(cbind.fill, dfsplit)
      # Calculate MVA dfs
      mvadf = sapply(dfsplit[[1]], function(x) ifelse(ncol(x)>2,1,0))
      nmva = sum(mvadf)
      # cat(nrow(dfspltc), " ")
      vecdfsplit = as.vector(dfspltc[,!names(dfspltc) %in% "y"])
      vec_dfs_len = length(vecdfsplit[!is.na(vecdfsplit)])
      res= vec_dfs_len/vec_td_len

      # if(res >0.2){res = res + res*nmva}
    }
    # }else(res=-10)
    return(Score=res)
  }
  # nBits = length(GA::decimal2binary(maximum_clusters))
  set.seed(3)
  GA <<- GA::ga(type = "real-valued", fitness = GA_FUNCT, lower = 1, upper = maximum_clusters, popSize = 10, maxiter = floor(maximum_clusters/20), run=10, pcrossover = 0.8, pmutation = 0.4, parallel = F, monitor = T)#, maxFitness = 1.0
  best_cluster = round(GA::binary2decimal(GA@solution[1,]))
  return(best_cluster)
}
