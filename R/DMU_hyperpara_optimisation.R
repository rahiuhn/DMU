#' The function in this file perform hyperparameter optimisation for DMU.

#' @export
hyperpara_optimize = function(optimtype = "opt_GA", traindata=NA, testdata=NA , ...){

  # Check if dataset is provided
  if(all(is.na(traindata))|all(is.na(testdata))){
    print("No training/testings Data. Exit")
    return("Exit because training/testing data is not available")
  }

  fullvarlist = list(seed=1, sample_clustersize = NA, sample_to_featureratio=2, datatype="NCR", splitype = "old")

  varlist= list(...)

  # Replace standard variable values with user defined values
  if(length(varlist) >0){
    targetvar = names(fullvarlist)[names(fullvarlist) %in% names(varlist)]
    dumpvar = sapply(targetvar, function(x) fullvarlist[[x]]<<-varlist[[x]])
  }

  methodlist = list(opt_GA = hpo_sup_simdata_ds_ori)

  best_cluster = methodlist[[optimtype]](x = list(seed=as.numeric(fullvarlist$seed),
                                                  sample_clustersize = as.numeric(fullvarlist$sample_clustersize),
                                                  sample_to_featureratio=as.numeric(fullvarlist$sample_to_featureratio),
                                                  datatype=fullvarlist$datatype),
                                         traindata = traindata, testdata = testdata,
                                         splitype = fullvarlist$splitype)
  return(best_cluster)
}






