#' The functions in this file contain other additional functions to support DMU
#' Cbind function source: http://stackoverflow.com/questions/7962267/cbind-a-df-with-an-empty-df-cbind-fill
#'
cbind.fill <- function(...){
  nm <- list(...)
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}


## Ineffective Functions
dfcomplete = function(inputdflist){
  cleandf = lapply(inputdflist, function(x){
    # print(x)
    fullcols <- colSums(!is.na(x)) == nrow(x)
    df <- x[fullcols]
  })
  df = na.omit.list(cleandf)
  return(df)
}

uvadf = function(inputdflist, cutratio=2, outcome = "y"){
  # create univariate data.frame
  dflist = lapply(inputdflist, function(x) {
    # Cond: Less cutratio
    cond3 = nrow(x)/(ncol(x)-1) >= cutratio
    # print(c(length(unique(x[,outcome])),!all(is.na(x[,!names(x) %in% outcome])), nrow(x)))
    if(cond3){x}
    else{y = singledf(df = x, outcome = outcome, cutratio = cutratio); bind_rows(y)}
  })

  # Create Single layer of lists
  # dflist = unlist(dflist, recursive = F)

  return(dflist)
}

dupdfwrapper = function(codedf = code_set, realdf = norm_set, outcome = "y", cutratio = 2, operations = c("all", "completecols", "others")){

  # Create Clusters of rows with identical patterns
  duplist = dupdflist(codedf = codedf, realdf = realdf)

  dupdf_code = duplist[[1]]
  duprows = duplist[[2]]
  dupdf_ori = duplist[[3]]
  # str(dupdf_ori)

  if(operations == "all"){
    # Select input columns without missing data
    df_comp = dfcomplete(inputdflist = dupdf_ori)

    # Check df if they contain constant y, samples less than cut ratio or empty input
    cleandf = dfcleaner(inputdflist = df_comp, outcome = outcome, cutratio = cutratio)
  }
  else{
    # Select input columns without missing data
    df_comp = dfcomplete(inputdflist = dupdf_ori)
    cleandf = df_comp
  }

  return(list(cleandf, duprows, dupdf_code))
}

singlerowlist = function(duprows = duprows, realdf = norm_set, outcome = "y", cutratio = 2){
  # Short list non-duplicate rows
  sdf = realdf[-duprows,]

  # create univariate data.frame
  clist = setdiff(names(sdf),outcome)
  dflist = lapply(clist, function(x) {y = sdf[,x] ;
  if(length(!is.na(y))>=cutratio){z = sdf[,c(x,outcome)]; z[complete.cases(z),]}
  else{NA}})
  dflist = na.omit.list(dflist)

  # Clean df
  dflist = dfcleaner(inputdflist = dflist, outcome = outcome, cutratio = cutratio)

  return(dflist)

}

swdatasplit = function(inputdf, cutratio=5, cluster_number=NA){
  if(is.na(cluster_number)){print("Check the Cluster Size"); out=NULL; dfcut = out; return(dfcut)}
  else{
    # Convert dataset to code
    norm_set=inputdf
    code_set=inputdf
    code_set[!(is.na(code_set))]<-1
    code_set[is.na(code_set)]<-0

    # Create Clusters of rows with identical patterns
    duplist = dupdfwrapper(codedf = code_set, realdf = norm_set, outcome = "y", cutratio = 2, operations = "completecols")
    cleandf = NULL #duplist[[1]]
    duprows = NULL #duplist[[2]]
    dupdf_code = duplist[[3]]

    # Df with no duplicates
    if(length(duprows) > 0){
      nodup_norm_set = norm_set[-duprows,]
      nodup_code_set = code_set[-duprows,]
    }
    else{
      nodup_norm_set = norm_set
      nodup_code_set = code_set
    }

    if(nrow(nodup_code_set) == 0){ out = NULL}
    else{
      ## Make HC row cluster
      hc = mhc_fun(inputdf = nodup_norm_set)
      ## Create Clusters
      maxcut=floor(nrow(nodup_code_set)/cutratio)

      ### Make sure number of clusters is no more than the maximum cluster limit
      if(cluster_number>maxcut | cluster_number<1){print(c("Check the Cluster Size:", cluster_number)); out=NULL}
      else{
        # Generate the cluster
        fitvalue=cutree(hc, cluster_number)
        nodup_clusdf = lapply(1:cluster_number, function(x) nodup_norm_set[which(fitvalue == x),])

        # Remove Incomplete Columns
        nodup_clusdf_comp = dfcomplete(inputdflist = nodup_clusdf)

        # Remove clusters with no input column
        nodup_clusdf_comp = nodup_clusdf_comp[lapply(nodup_clusdf_comp, ncol)>1]


        if(length(nodup_clusdf_comp) == 0){out=NULL}
        else{out = nodup_clusdf_comp}
      }
    }

    if(is.null(out) & is.null(cleandf)){dfcut = list()}
    else if(is.null(out)){dfcut = cleandf}
    else{
      fullcluster = c(cleandf, out)
      fulldf = bind_rows(fullcluster)
      newnames = names(inputdf)[names(inputdf) %in% names(fulldf)]
      fulldf = fulldf[,newnames]
      # print(fulldf)
      norm_set=fulldf
      code_set=fulldf
      code_set[!(is.na(code_set))]<-1
      code_set[is.na(code_set)]<-0
      cluslist = dupdfwrapper(codedf = code_set, realdf = norm_set, outcome = "y", cutratio = 2, operations = "all")

      # Single Rows data: Segregate by columns to create multiple univariate datasets
      singledflist = singlerowlist(duprows = cluslist[[2]], realdf = norm_set, outcome = "y", cutratio = 2)
      dfcut = c(cluslist[[1]], singledflist)
    }
  }
  return(list(dfcut))
}

sdatasplit2 = function(inputdf, cutratio=5, cluster_number=NA){
  if(is.na(cluster_number)){print("Check the Cluster Size"); out=NULL; dfcut = out; return(dfcut)}
  else{
    # Convert dataset to code
    norm_set=inputdf
    code_set=inputdf
    code_set[!(is.na(code_set))]<-1
    code_set[is.na(code_set)]<-0

    # Create Clusters of rows with identical patterns
    duplist = dupdfwrapper(codedf = code_set, realdf = norm_set, outcome = "y", cutratio = 2, operations = "completecols")
    cleandf = duplist[[1]]
    duprows = duplist[[2]]
    dupdf_code = duplist[[3]]

    # Df with no duplicates
    if(length(duprows) > 0){
      nodup_norm_set = norm_set[-duprows,]
      nodup_code_set = code_set[-duprows,]
    }
    else{
      nodup_norm_set = norm_set
      nodup_code_set = code_set
    }

    if(nrow(nodup_code_set) == 0){ out = NULL}
    else{
      ## Make HC row cluster
      hc = mhc_fun(inputdf = nodup_norm_set)
      ## Create Clusters
      maxcut=floor(nrow(nodup_code_set)/cutratio)

      ### Make sure number of clusters is no more than the maximum cluster limit
      if(cluster_number>maxcut | cluster_number<1){print("Check the Cluster Size"); out=NULL}
      else{
        # Generate the cluster
        fitvalue=cutree(hc, cluster_number)
        nodup_clusdf = lapply(1:cluster_number, function(x) nodup_norm_set[which(fitvalue == x),])
        # Remove Incomplete Columns
        nodup_clusdf_comp = dfcomplete(inputdflist = nodup_clusdf)
        # Remove clusters with no input column
        nodup_clusdf_comp = nodup_clusdf_comp[lapply(nodup_clusdf_comp, ncol)>1]
        if(length(nodup_clusdf_comp) == 0){out=NULL}
        else{out = nodup_clusdf_comp}
      }
    }

    if(is.null(out) & is.null(cleandf)){dfcut = list()}
    else if(is.null(out)){dfcut = cleandf}
    else{
      fullcluster = c(cleandf, out)
      fulldf = bind_rows(fullcluster)
      newnames = names(inputdf)[names(inputdf) %in% names(fulldf)]
      fulldf = fulldf[,newnames]
      # print(fulldf)
      norm_set=fulldf
      code_set=fulldf
      code_set[!(is.na(code_set))]<-1
      code_set[is.na(code_set)]<-0
      cluslist = dupdfwrapper(codedf = code_set, realdf = norm_set, outcome = "y", cutratio = 2, operations = "all")
      dfcut = cluslist[[1]]
    }
  }
  return(list(dfcut))
}

ssdatasplit2 = function(inputdf, cutratio=5, cluster_number=NA){
  if(is.na(cluster_number)){print("Check the Cluster Size"); out=NULL; dfcut = out}
  else{
    # Convert dataset to code
    norm_set=inputdf
    code_set=inputdf
    code_set[!(is.na(code_set))]<-1
    code_set[is.na(code_set)]<-0

    # Perform Hierarchical Clustering

    if(nrow(code_set) == 0){ out = NULL}
    else{
      ## Make HC row cluster
      hc = mhc_fun(inputdf = norm_set)

      ## Create Clusters
      maxcut=floor(nrow(code_set))

      ### Make sure number of clusters is no more than the maximum cluster limit
      if(cluster_number>maxcut | cluster_number<1){
        print(c("Check the Cluster Size:", cluster_number));
        out=NULL}
      else{
        # Generate the cluster
        fitvalue=cutree(hc, cluster_number)
        nodup_clusdf = lapply(1:cluster_number, function(x) norm_set[which(fitvalue == x),])

        # Remove Incomplete Columns
        nodup_clusdf_comp = dfcomplete(inputdflist = nodup_clusdf)

        # Remove clusters with no input column
        nodup_clusdf_comp = nodup_clusdf_comp[lapply(nodup_clusdf_comp, ncol)>1]

        if(length(nodup_clusdf_comp) == 0){out=NULL}
        else{
          # Create univariate df for clusters less than cutratio
          # uva_clus_list = uvadf(inputdflist = nodup_clusdf_comp, cutratio=cutratio, outcome = "y")
          uva_clus_list = nodup_clusdf_comp
          # Combine duplicate df and clean the dataset
          fulldf = bind_rows(uva_clus_list)
          newnames = names(inputdf)[names(inputdf) %in% names(fulldf)]
          fulldf = fulldf[,newnames]
          # print(fulldf)

          norm_set=fulldf
          code_set=fulldf
          code_set[!(is.na(code_set))]<-1
          code_set[is.na(code_set)]<-0
          cluslist = dupdfwrapper(codedf = code_set, realdf = norm_set, outcome = "y", cutratio = 2, operations = "all")
          out = cluslist
        }
      }
    }
  }

  dfcut = out
  return(dfcut)
}

# Supervised optimization and old (data_sim) data structure and mixed
hpo_sup_simdata_mixed = function(x = list(varnum=100, miss=0.1, correlation=0.52, seed=1, tr_multi=90,  sample_clustersize = NA, sample_to_featureratio=2, datatype="incomplete"), traindata = NA, testdata =NA, setting="Correlation", var="Mar", main_var=10, var_effect=0.5, correlation_var=15, high_dim=T, test_size=1000, Missingdata = T, splitype = c("old", "mixed", "uni")){

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
  if(is.na(traindata)){
    dataset = data_sim(varnum, maxmiss_per=missing_percent, corr_seed=seed_run, miss_seed=seed_run,
                       samplesize=((varnum*tr_multi)+test_size), setting=setting, effect=var, max_corr=corr_range,
                       testsize = test_size, Missingdata = Missingdata,
                       metric = FALSE, missingset= "incomplete", copseed=NULL)
    traindata = dataset$train
    testdata = dataset$test
  }

  # testdata = dataset$test[-c(1:floor(nrow(dataset$test)/2)),]
  # validationdata = dataset$test[1:floor(nrow(dataset$test)/2),]

  # Define Hyperpara to label the df
  hyperpara = data.frame(miss = missing_percent, corr=corr_range, cutter=ncol(traindata)-1, trainset=datatype, seed=seed_run, data_code=1, stringsAsFactors = F)

  maximum_clusters=nrow(traindata)/cutratio
  GA_FUNCT=function(bin){
    dec_code = GA::binary2decimal(bin)
    cat(dec_code, " ")
    if(dec_code<(maximum_clusters+1) & dec_code>0){
      dfsplit=sdatasplit(traindata, cutratio=cutratio, cluster_number=dec_code)
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
  GA <- GA::ga(type = "binary", fitness = GA_FUNCT, nBits = nBits, popSize = 10, maxiter = floor(maximum_clusters/20), maxFitness = -0.07, run=1, pcrossover = 0.8, pmutation = 0.4, parallel = F, monitor = T)
  best_cluster = GA::binary2decimal(GA@solution[1,])
  return(best_cluster)
}

# Supervised optimization and old (data_sim) data structure and univariate
hpo_sup_simdata_uni = function(x = list(varnum=100, miss=0.1, correlation=0.52, seed=1, tr_multi=90,  sample_clustersize = NA, sample_to_featureratio=2, datatype="incomplete"), traindata = NA, testdata =NA, setting="Correlation", var="Mar", main_var=10, var_effect=0.5, correlation_var=15, high_dim=T, test_size=1000, Missingdata = T, splitype = c("old", "mixed", "uni")){

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
  if(is.na(traindata)){
    dataset = data_sim(varnum, maxmiss_per=missing_percent, corr_seed=seed_run, miss_seed=seed_run,
                       samplesize=((varnum*tr_multi)+test_size), setting=setting, effect=var, max_corr=corr_range,
                       testsize = test_size, Missingdata = Missingdata,
                       metric = FALSE, missingset= "incomplete", copseed=NULL)
    traindata = dataset$train
    testdata = dataset$test
  }

  # testdata = dataset$test[-c(1:floor(nrow(dataset$test)/2)),]
  # validationdata = dataset$test[1:floor(nrow(dataset$test)/2),]

  # Define Hyperpara to label the df
  hyperpara = data.frame(miss = missing_percent, corr=corr_range, cutter=ncol(traindata)-1, trainset=datatype, seed=seed_run, data_code=1, stringsAsFactors = F)

  maximum_clusters=nrow(traindata)/cutratio
  GA_FUNCT=function(bin){
    dec_code = GA::binary2decimal(bin)
    if(dec_code<(maximum_clusters+1) & dec_code>0){
      dfsplit=ssdatasplit(traindata, cutratio=cutratio, cluster_number=dec_code)
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
  GA <<- GA::ga(type = "binary", fitness = GA_FUNCT, nBits = nBits, popSize = 10, maxiter = floor(maximum_clusters/20), maxFitness = -0.07, run=10, pcrossover = 0.8, pmutation = 0.8, parallel = F, monitor = T)
  best_cluster = GA::binary2decimal(GA@solution[1,])
  return(best_cluster)
}
