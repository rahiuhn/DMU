#' The functions in this file generate the artificial dataset for the DMU modeling

#
#
# # Create Artificial Dataset
# cormatgenerator_old=function(variablesize=non_zerocorr, corr_values=corr_list, output=c("cormat", "data", "both"),
#                          datapoint=1000, copseed=NULL){
#   #print(c(variablesize,corr_values))
#   if(!is.null(copseed)==T){set.seed(copseed)}
#
#   myCop=copula::normalCopula(param=corr_values, dim = variablesize, dispstr = "un")
#   distrib=c("beta", "norm", "unif", rep("norm",max(variablesize-3,0)))
#   distrib_para=lapply(distrib, function(x) {
#     if (x=="beta"){list(shape1=7, shape2=2)}
#     else if(x=="unif"){list(min=0, max=2)}
#     else {
#       set.seed(sample(seq(1,5*variablesize),1));
#       list(mean=sample(seq(-0.01, 0.01, 0.02/variablesize),1), sd=1)}
#   })
#   myMvd <- copula::mvdc(copula=myCop, margins=distrib, paramMargins=distrib_para )
#   #set.seed(1)
#   Z2 <- copula::rMvdc(datapoint,myMvd)
#   #str(Z2)
#   cor_mat=cor(Z2)
#   if(output=="cormat"){return(cor_mat)}
#   else if(output=="data"){return(Z2)}
#   else{return(list(cor_mat,Z2))}
# }
# svd_struc_old=function(inputmatrix){
#   MMT=inputmatrix %*% t(inputmatrix)
#   return(eigen(MMT)$values[1])
# }

# ## Create simulated linear dataset
# data_sim_old=function(varnum,  corr_seed=1, max_corr=0.5, effect=c("Mar", "Int", "Both", "None"),  samplesize=((varnum*20)+1000), maxmiss_per=0.3,  testsize=1000, ...){
#
#   # Default Feature Values
#   fullvarlist = list(setting="Correlation", copseed=NULL, Missingdata = T, missingset="incomplete", metric = F, miss_seed = corr_seed)
#   varlist= list(...)
#   ## Replace standard variable values with user defined values
#   if(length(varlist) >0){
#     targetvar = names(fullvarlist)[names(fullvarlist) %in% names(varlist)]
#     dumpvar = sapply(targetvar, function(x) fullvarlist[[x]]<<-varlist[[x]])
#   }
#   ## All Standard Features
#   setting= fullvarlist$setting;
#   copseed=fullvarlist$copseed;
#   Missingdata = fullvarlist$Missingdata;
#   missingset=fullvarlist$missingset;
#   metric = fullvarlist$metric;
#   miss_seed = fullvarlist$miss_seed
#
#   # Define the Correlation Structure
#   if(setting == "Correlation"){
#     # print(corr_seed)
#     Poss_cor=setdiff(seq(-max_corr,max_corr,0.1), 0)
#     # print(Poss_cor)
#     #varnum=9
#     # Number of variables with non-zero correlation
#     non_zerocorr=min(5, varnum)
#     corr_num=choose(non_zerocorr,2)
#     # print(corr_num)
#     # Get the possible non-zero correlation values
#     set.seed(corr_seed);
#     non_zerocorr_list=sample(Poss_cor, corr_num, replace = TRUE)
#     # print(non_zerocorr_list)
#     # Number of variables with zero correlation
#     zerocorr=max(0, varnum-non_zerocorr)
#     corr_num=choose(zerocorr,2)
#     zerocorr_list=rep(0, corr_num)
#     paralist=list(non_zero=list(non_zerocorr,non_zerocorr_list), zerocorr=list(zerocorr, zerocorr_list))
#     cormat=lapply(paralist, function(x) cormatgenerator(variablesize=x[[1]], corr_values = x[[2]], output = "cormat"))
#     cormat[[2]][cormat[[2]]!=1]=0
#     colnames(cormat[[1]]) <- 1:non_zerocorr
#     #print(cormat[[1]])
#     colnames(cormat[[2]]) <- (non_zerocorr+1):varnum
#     cormat=do.call(gtools::smartbind,cormat)
#     cormat[is.na(cormat)] =0
#     cormat_list=cormat[lower.tri(cormat)]
#     cormat_list=round(cormat_list, digits = 2)
#   }
#   else{
#     corr_num=choose(varnum,2)
#     non_zerocorr_list=rep(0,corr_num)
#     cormat_list=non_zerocorr_list
#   }
#   data=cormatgenerator(variablesize = varnum, corr_values = cormat_list, output = "both", datapoint = samplesize, copseed = copseed)
#   dataset=data[[2]]
#   cormat=data[[1]]
#
#   # str(dataset)
#   # Z scaling of input parameters
#   dataset=apply(dataset, 2, mosaic::zscore)
#   colnames(dataset) <- lapply(1:varnum, function(x) paste0("X",x))
#   dataset=data.frame(dataset)
#
#   # create output values
#   if(effect=="Mar"){betamultiplier=c(1,1,1,1,0)}
#   else if(effect=="Int"){betamultiplier=c(1,0,0,0,1)}
#   else if(effect=="both"){betamultiplier=c(1,1,1,1,1)}
#   else{betamultiplier=c(1,0,0,0,0)}
#   coef=c(10,0.2, 0.3, 0.4, 0.3)*1
#   beta_coef=coef*betamultiplier
#   dataset$y= apply(dataset[,1:3],1, function(x) {
#     a=c(1,x,prod(x[1:2]));
#     b=crossprod(a,beta_coef);
#     b})
#   dataset$y=dataset$y+rnorm(n=samplesize, mean=0, sd=0.25)
#   #print("y")
#
#   # Create Missing Data
#   #print(nrow(dataset))
#   trainingdata=dataset[-1:-testsize,]
#   truedata = trainingdata
#   testdata=dataset[1:testsize,]
#   if(Missingdata == TRUE){
#     temp_df=trainingdata[,-ncol(trainingdata)]
#     missing_nrows= floor(maxmiss_per*nrow(temp_df))
#     set.seed(miss_seed); temp_df=apply(temp_df, 2, function(x) {x[sample(1:length(x), missing_nrows)] <-NA; x})
#     trainingdata[,-ncol(trainingdata)]=temp_df
#     in_data=trainingdata[!complete.cases(trainingdata),]
#     trainingdata=in_data
#   }
#   #print("MD")
#
#   # Perform Metric Evaluation
#   if(metric==TRUE){
#     inputdf=trainingdata[,-ncol(trainingdata)]
#     cor_mat=cor(inputdf, use="pairwise.complete.obs")
#     if(anyNA(as.matrix(cor_mat))==T){svd=500}else{svd=svd_struc(as.matrix(cor_mat))}
#     rows=nrow(inputdf)
#     cols=ncol(inputdf)
#     row_col_ratio=rows/cols
#     met=c(lambda=svd,r=rows,c=cols,rc_ratio=row_col_ratio)
#     if(svd<=100){
#       return(list(train=trainingdata, test=testdata, cormat=cormat, metric=met, corr_struc=non_zerocorr_list, lamb=svd, truedata = truedata))
#     }else{return(list(train="error", test=testdata, cormat=cormat, metric=met, truedata = truedata))}
#   }
#   else{return(list(train=trainingdata, test=testdata, cormat=cormat, truedata = truedata))}
# }

# # Revised Dataset Generators
# randcorr = function(varnum = 5, maxcorr = 0.5, type = c("upperlist", "matrix"), seed = 1){
#   a = varnum
#   set.seed(seed)
#   cormat <- matrix(runif(a*a, min=-maxcorr,max=maxcorr),a,a)
#   diag(cormat) <- 1
#   cormat[lower.tri(cormat)] = t(cormat)[lower.tri(cormat)]
#   if(type == "upperlist"){
#     cormat = unlist(cormat[lower.tri(cormat)])
#   }
#   return(cormat)
# }
# param_dist = function(varnum = 5, distrib = NA, ...){
#
#   # Default Feature Values
#   fullvarlist = list(shape1=7, shape2=2, min = 0, max =2, mean = 0.1, sd = 1)
#   varlist= list(...)
#   ## Replace standard variable values with user defined values
#   if(length(varlist) >0){
#     targetvar = names(fullvarlist)[names(fullvarlist) %in% names(varlist)]
#     dumpvar = sapply(targetvar, function(x) fullvarlist[[x]]<<-varlist[[x]])
#   }
#
#   ## All Standard Features
#   shape1= fullvarlist$shape1;
#   shape2=fullvarlist$shape2;
#   min = fullvarlist$min;
#   max = fullvarlist$max;
#   mean = fullvarlist$mean;
#   sd = fullvarlist$sd
#
#   # Prepare distribution
#
#   if(is.na(distrib)){distrib=c("beta", "norm", "unif", rep("norm",max(varnum-3,0)))}
#   distrib_para=lapply(distrib, function(x) {
#     if (x=="beta"){list(shape1=7, shape2=2)}
#     else if(x=="unif"){list(min=0, max=2)}
#     else {
#       set.seed(sample(seq(1,5*varnum),1));
#       list(mean=sample(seq(-0.01, 0.01, 0.02/varnum),1), sd=1)}
#   })
#
#   return(list(distrib, distrib_para))
# }
#
# cormatgenerator=function(variablesize=non_zerocorr, corr_values=corr_list, output=c("cormat", "data", "both"),
#                          datapoint=1000, copseed=NULL, ...){
#
#   # Default Feature Values
#   fullvarlist = list(shape1=7, shape2=2, min = 0, max =2, mean = 0.1, sd = 1, distrib = NA)
#   varlist= list(...)
#   ## Replace standard variable values with user defined values
#   if(length(varlist) >0){
#     targetvar = names(fullvarlist)[names(fullvarlist) %in% names(varlist)]
#     dumpvar = sapply(targetvar, function(x) fullvarlist[[x]]<<-varlist[[x]])
#   }
#
#   ## All Standard Features
#   shape1= fullvarlist$shape1;
#   shape2=fullvarlist$shape2;
#   min = fullvarlist$min;
#   max = fullvarlist$max;
#   mean = fullvarlist$mean;
#   sd = fullvarlist$sd
#   distrib = fullvarlist$distrib
#
#   # print(c(variablesize,corr_values))
#   if(!is.null(copseed)==T){set.seed(copseed)}
#
#   myCop=copula::normalCopula(param=corr_values, dim = variablesize, dispstr = "un")
#
#   distribution_para = param_dist(varnum = variablesize, distrib = distrib, shape1 = shape1, shape2 = shape2, min = min, max = max, mean = mean, sd = sd)
#   distrib = distribution_para[[1]]
#   distrib_para = distribution_para[[2]]
#   myMvd <- copula::mvdc(copula=myCop, margins=distrib, paramMargins=distrib_para )
#   #set.seed(1)
#   Z2 <- copula::rMvdc(datapoint,myMvd)
#   # str(Z2)
#   cor_mat=cor(Z2)
#   if(output=="cormat"){return(cor_mat)}
#   else if(output=="data"){return(Z2)}
#   else{return(list(cor_mat,Z2))}
# }
# svd_struc=function(inputmatrix){
#   MMT=inputmatrix %*% t(inputmatrix)
#   return(eigen(MMT)$values[1])
# }
#
# # Create simulated linear dataset
# data_sim=function(varnum,  corr_seed=1, max_corr=0.5, effect=c("Mar", "Int", "Both", "None"),  samplesize=((varnum*20)+1000), maxmiss_per=0.3,  testsize=1000, ...){
#
#   # Default Feature Values
#   fullvarlist = list(setting="Correlation", copseed=NULL, Missingdata = T, missingset="incomplete", metric = F, miss_seed = corr_seed, high_dim= T, main_var = 3, var_effect = c(0.2, 0.3, 0.4))
#   varlist= list(...)
#   ## Replace standard variable values with user defined values
#   if(length(varlist) >0){
#     targetvar = names(fullvarlist)[names(fullvarlist) %in% names(varlist)]
#     dumpvar = sapply(targetvar, function(x) fullvarlist[[x]]<<-varlist[[x]])
#   }
#
#   ## All Standard Features
#   setting= fullvarlist$setting;
#   copseed=fullvarlist$copseed;
#   Missingdata = fullvarlist$Missingdata;
#   missingset=fullvarlist$missingset;
#   metric = fullvarlist$metric;
#   miss_seed = fullvarlist$miss_seed
#   high_dim = fullvarlist$high_dim
#   main_var = fullvarlist$main_var
#   var_effect = fullvarlist$var_effect
#
#   # Define the Correlation Structure
#   if(high_dim){
#     cormat_list = randcorr(varnum = varnum, maxcorr = max_corr, type = c("upperlist"), seed = corr_seed)
#   }
#   else{
#     if(setting == "Correlation"){
#       Poss_cor=setdiff(seq(-max_corr,max_corr,0.1), 0)
#
#       # Number of variables with non-zero correlation
#       non_zerocorr=min(5, varnum)
#       corr_num=choose(non_zerocorr,2)
#
#       # Get the possible non-zero correlation values
#       set.seed(corr_seed);
#       non_zerocorr_list=sample(Poss_cor, corr_num, replace = TRUE)
#
#       # Number of variables with zero correlation
#       zerocorr=max(0, varnum-non_zerocorr)
#       corr_num=choose(zerocorr,2)
#       zerocorr_list=rep(0, corr_num)
#       paralist=list(non_zero=list(non_zerocorr,non_zerocorr_list), zerocorr=list(zerocorr, zerocorr_list))
#       cormat=lapply(paralist, function(x) cormatgenerator(variablesize=x[[1]], corr_values = x[[2]], output = "cormat"))
#       cormat[[2]][cormat[[2]]!=1]=0
#       colnames(cormat[[1]]) <- 1:non_zerocorr
#       #print(cormat[[1]])
#       colnames(cormat[[2]]) <- (non_zerocorr+1):varnum
#       cormat=do.call(gtools::smartbind,cormat)
#       cormat[is.na(cormat)] =0
#       cormat_list=cormat[lower.tri(cormat)]
#       cormat_list=round(cormat_list, digits = 2)
#       # print(cormat_list)
#     }
#     else{
#       corr_num=choose(varnum,2)
#       non_zerocorr_list=rep(0,corr_num)
#       cormat_list=non_zerocorr_list
#     }
#   }
#
#   data=cormatgenerator(variablesize = varnum, corr_values = cormat_list, output = "both", datapoint = samplesize, copseed = copseed)
#   dataset=data[[2]]
#   cormat=data[[1]]
#
#   # Z scaling of input parameters
#   dataset=apply(dataset, 2, mosaic::zscore)
#   colnames(dataset) <- lapply(1:varnum, function(x) paste0("X",x))
#   dataset=data.frame(dataset)
#
#   # create output values
#   if(high_dim){
#     ta = dataset
#
#     intercept = 10
#     betas = c(rep(var_effect,(2*main_var)-1))
#     beta_value = betas[1:((2*main_var)-1)]
#     if(effect=="Mar"){beta_value=beta_value*rep(1,(2*main_var)-1)}
#     else if(effect=="No_Mar"){beta_value=beta_value*c(rep(0,2), rep(1,main_var-2), rep(1,main_var-1))}
#     else if(effect=="No_Var"){beta_value=beta_value*c(rep(0,main_var), rep(0,main_var-1))}
#     else if(effect=="only_int"){beta_value=beta_value*c(rep(0,main_var), rep(1,main_var-1))}
#     else {beta_value = beta_value*c(rep(1,main_var), rep(0,main_var-1))}
#
#     # Generate theinteraction term
#     mar_var= paste(c(names(ta)[1:main_var]), collapse = "+")
#     int_var= paste(names(ta)[1:(main_var-1)],"*" , names(ta)[2:main_var], collapse = " + ")
#     f = as.formula(paste("~",mar_var ," +" , int_var))
#
#     main_mat = model.matrix(f, ta)
#     # print(main_mat[1:5,])
#     # Get Outcome
#     set.seed(2)
#     random_value = rnorm(n=samplesize, mean=0, sd=0.25)
#     # coef_value = apply(main_mat,1, function(x) {sum(x*c(intercept, beta_value))})
#
#     ta$y = main_mat %*% c(intercept, beta_value) + random_value
#     # ta$y  = coef_value + random_value
#
#     dataset = ta
#   }
#   else{
#     if(effect=="Mar"){betamultiplier=c(1,1,1,1,0)}
#     else if(effect=="Int"){betamultiplier=c(1,0,0,0,1)}
#     else if(effect=="both"){betamultiplier=c(1,1,1,1,1)}
#     else{betamultiplier=c(1,0,0,0,0)}
#     coef=c(10,0.2, 0.3, 0.4, 0.3)*1
#     beta_coef=coef*betamultiplier
#     dataset$y= apply(dataset[,1:3],1, function(x) {
#       a=c(1,x,prod(x[1:2]));
#       b=crossprod(a,beta_coef);
#       b})
#     dataset$y=dataset$y+rnorm(n=samplesize, mean=0, sd=0.25)
#   }
#
#   # Create Missing Data
#   #print(nrow(dataset))
#   trainingdata=dataset[-1:-testsize,]
#   truedata = trainingdata
#   testdata=dataset[1:testsize,]
#   if(Missingdata == TRUE){
#     temp_df=trainingdata[,-ncol(trainingdata)]
#     missing_nrows= floor(maxmiss_per*nrow(temp_df))
#     set.seed(miss_seed); temp_df=apply(temp_df, 2, function(x) {x[sample(1:length(x), missing_nrows)] <-NA; x})
#     trainingdata[,-ncol(trainingdata)]=temp_df
#     in_data=trainingdata[!complete.cases(trainingdata),]
#     trainingdata=in_data
#   }
#   #print("MD")
#   return(list(train=trainingdata, test=testdata, cormat=cormat, truedata = truedata))
#   # # Perform Metric Evaluation
#   # if(metric==TRUE){
#   #   inputdf=trainingdata[,-ncol(trainingdata)]
#   #   cor_mat=cor(inputdf, use="pairwise.complete.obs")
#   #   if(anyNA(as.matrix(cor_mat))==T){svd=500}else{svd=svd_struc(as.matrix(cor_mat))}
#   #   rows=nrow(inputdf)
#   #   cols=ncol(inputdf)
#   #   row_col_ratio=rows/cols
#   #   met=c(lambda=svd,r=rows,c=cols,rc_ratio=row_col_ratio)
#   #   if(svd<=100){
#   #     return(list(train=trainingdata, test=testdata, cormat=cormat, metric=met, corr_struc=non_zerocorr_list, lamb=svd, truedata = truedata))
#   #   }else{return(list(train="error", test=testdata, cormat=cormat, metric=met, truedata = truedata))}
#   # }
#   # else{return(list(train=trainingdata, test=testdata, cormat=cormat, truedata = truedata))}
# }


#' dataset=function(varnum, setting="No_Correlation", var=c("Mar", "No_Mar", "No_Var"), seed=2, main_var=10, var_effect=0.5, correlation_var=15, correlation_val=5, high_dim=T, train_sample=500, test_size=1000, Missingdata = T, maxmiss_per=0.3){
#'
#'   # Create the Covariance Matrix
#'   Sigma=matrix(rep(0,varnum), nrow=varnum, ncol=varnum, byrow=F)
#'   if(high_dim){
#'
#'     for(i in 1:varnum){
#'       for(j in 1:varnum){
#'         if(i==j){Sigma[i,j]=10}
#'         else if(i<=correlation_var & j<=correlation_var & setting == "Correlation"){
#'           if(length(correlation_val) == 1){Sigma[i,j]=Sigma[j,i]=correlation_val}
#'           else{set.seed(2); Sigma[i,j]=Sigma[j,i] = runif(1, min = min(correlation_val), max = max(correlation_val))}
#'           }
#'         else{Sigma[i,j]=Sigma[j,i]=1}
#'       }
#'     }
#'   }
#'   else{
#'     for(i in 1:varnum){Sigma[i,i]=10}
#'     # Correlation Settings
#'     if(setting=="Correlation"){
#'       Sigma[1,2]=3;Sigma[1,3]=3;Sigma[1,4]=6;Sigma[1,5]=6
#'       Sigma[2,1]=3;Sigma[3,1]=3;Sigma[4,1]=6;Sigma[5,1]=6
#'       Sigma[2,3]=3;Sigma[2,4]=2;Sigma[2,5]=1
#'       Sigma[3,2]=3;Sigma[4,2]=2;Sigma[5,2]=1
#'       Sigma[3,4]=2;Sigma[3,5]=1
#'       Sigma[4,3]=2;Sigma[5,3]=1
#'       Sigma[4,5]=1
#'       Sigma[5,4]=1
#'     }
#'   }
#'
#'   # Create the input dataset
#'   #print(Sigma)
#'   set.seed(seed)
#'   ta=data.frame(MASS::mvrnorm(n = train_sample+test_size, rep(0, varnum), Sigma/10))
#'   variablelist=list()
#'   for(i in 1:varnum){
#'     variablelist[[i]]=gsub(" ", "",paste("X",i))
#'     ta[,i]=mosaic::zscore(ta[,i])
#'   }
#'   variablelist=unlist(variablelist)
#'
#'   # Create the outcome Variable
#'   intercept=1
#'   if(high_dim){
#'     betas = c(rep(var_effect,(2*main_var)-1))
#'     beta_value = betas[1:((2*main_var)-1)]
#'     if(var=="Mar"){beta_value=beta_value*rep(1,(2*main_var)-1)}
#'     else if(var=="No_Mar"){beta_value=beta_value*c(rep(0,2), rep(1,main_var-2), rep(1,main_var-1))}
#'     else if(var=="No_Var"){beta_value=beta_value*c(rep(0,main_var), rep(0,main_var-1))}
#'     else if(var=="only_int"){beta_value=beta_value*c(rep(0,main_var), rep(1,main_var-1))}
#'     else {beta_value = beta_value*c(rep(1,main_var), rep(0,main_var-1))}
#'
#'     # Generate theinteraction term
#'     mar_var= paste(c(names(ta)[1:main_var]), collapse = "+")
#'     int_var= paste(names(ta)[1:(main_var-1)],"*" , names(ta)[2:main_var], collapse = " + ")
#'     f = as.formula(paste("~",mar_var ," +" , int_var))
#'     # print(f)
#'     main_mat = model.matrix(f, ta)
#'     # print(main_mat[1:5,])
#'     # Get Outcome
#'     set.seed(2)
#'     random_value = rnorm(n=train_sample+500, mean=0, sd=0.25)
#'     # coef_value = apply(main_mat,1, function(x) {sum(x*c(intercept, beta_value))})
#'
#'     ta$y = main_mat %*% c(intercept, beta_value) + random_value
#'     # ta$y  = coef_value + random_value
#'   }
#'   else{
#'     if(var=="Mar"){beta_a=1; beta_b=1}
#'     else if(var=="No_Mar"){beta_a=0; beta_b=1}
#'     else{beta_a=0; beta_b=0}
#'     b1=0.2*beta_a
#'     b2=0.3*beta_a
#'     b3=0.4*beta_b
#'     b4=0.3*beta_b
#'     set.seed(2)
#'     ta$y= intercept + (b1*ta$X1) + (b2*ta$X2) + (b3*ta$X3) + (b4*ta$X1*ta$X2) + rnorm(n=train_sample+500, mean=0, sd=0.25)
#'   }
#'   set.seed(2)
#'   index=sample(1:nrow(ta), test_size, replace = F)
#'   originaldf=traindf=ta[-index,]
#'   validationdf=ta[index,]
#'
#'   if(Missingdata == TRUE){
#'     temp_df=traindf[,-ncol(traindf)]
#'     missing_nrows= floor(maxmiss_per*nrow(temp_df))
#'     set.seed(2); temp_df=apply(temp_df, 2, function(x) {x[sample(1:length(x), missing_nrows)] <-NA; x})
#'     traindf[,-ncol(traindf)]=temp_df
#'     in_data=traindf[!complete.cases(traindf),]
#'     traindf=in_data
#'   }
#'   return(list(train=traindf,test=validationdf, original = originaldf))
#' }
#'
#' #' @export
#' hidataset = function(varnum,setting="No_Correlation", high_dim=T, max_corr = 0.5, samplesize=((varnum*20)+1000), test_size=1000, skew = c(0), skurt = c(0), var=c("Mar", "No_Mar", "No_Var", "only_int", NA), main_var=10, var_effect=0.5,Missingdata = T, maxmiss_per=0.3, seed = 1){
#'
#'   train_sample = samplesize - test_size
#'   # Prepare Correlation Matrix
#'   if(high_dim){
#'     # Generate Correlation matrix
#'     a=varnum
#'     set.seed(seed)
#'     cormat <- matrix(runif(a*a, min=-max_corr,max=max_corr),a,a)
#'     diag(cormat) <- 1
#'     cormat[lower.tri(cormat)] = t(cormat)[lower.tri(cormat)]
#'   }
#'   else{
#'     if(settings == "Correlation"){
#'       # Generate Correlation matrix
#'       a=varnum
#'       cormat <- matrix(runif(a*a, min=-correlation_val,max=correlation_val),a,a)
#'       diag(cormat) <- 1
#'       cormat[lower.tri(cormat)] = t(cormat)[lower.tri(cormat)]
#'     }
#'     else{
#'       a=varnum
#'       cormat <- matrix(runif(a*a, min=-correlation_val,max=correlation_val),a,a)
#'       }
#'   }
#'
#'   # print(cormat) # Doe not work for varnum more than 6 if correlation is 0.5 or more
#'   # Prepare the dataset
#'   set.seed(seed)
#'   skewness = rep(skew,varnum)[1:varnum]
#'   kurtosis = rep(skurt,varnum)[1:varnum]
#'   set.seed(seed)
#'   datagen = SimMultiCorrData::rcorrvar(n=samplesize, k_cont = varnum, rho = cormat, means = rep(0,varnum), vars = rep(1,varnum), skews = skewness, skurts = kurtosis)
#'   ta = datagen$continuous_variables
#'   variablelist=list()
#'   for(i in 1:varnum){
#'     variablelist[[i]]=gsub(" ", "",paste("X",i))
#'     ta[,i]=mosaic::zscore(ta[,i])
#'   }
#'   variablelist=unlist(variablelist)
#'
#'   # Create the outcome Variable
#'   intercept=1
#'   if(high_dim){
#'     betas = c(rep(var_effect,(2*main_var)-1))
#'     beta_value = betas[1:((2*main_var)-1)]
#'     if(var=="Mar"){beta_value=beta_value*rep(1,(2*main_var)-1)}
#'     else if(var=="No_Mar"){beta_value=beta_value*c(rep(0,2), rep(1,main_var-2), rep(1,main_var-1))}
#'     else if(var=="No_Var"){beta_value=beta_value*c(rep(0,main_var), rep(0,main_var-1))}
#'     else if(var=="only_int"){beta_value=beta_value*c(rep(0,main_var), rep(1,main_var-1))}
#'     else {beta_value = beta_value*c(rep(1,main_var), rep(0,main_var-1))}
#'
#'     # Generate theinteraction term
#'     mar_var= paste(c(names(ta)[1:main_var]), collapse = "+")
#'     int_var= paste(names(ta)[1:(main_var-1)],"*" , names(ta)[2:main_var], collapse = " + ")
#'     f = as.formula(paste("~",mar_var ," +" , int_var))
#'
#'     main_mat = model.matrix(f, ta)
#'     # print(main_mat[1:5,])
#'     # Get Outcome
#'     set.seed(2)
#'     random_value = rnorm(n=samplesize, mean=0, sd=0.25)
#'     # coef_value = apply(main_mat,1, function(x) {sum(x*c(intercept, beta_value))})
#'
#'     ta$y = main_mat %*% c(intercept, beta_value) + random_value
#'     # ta$y  = coef_value + random_value
#'   }
#'   else{
#'     if(var=="Mar"){beta_a=1; beta_b=1}
#'     else if(var=="No_Mar"){beta_a=0; beta_b=1}
#'     else{beta_a=0; beta_b=0}
#'     b1=0.2*beta_a
#'     b2=0.3*beta_a
#'     b3=0.4*beta_b
#'     b4=0.3*beta_b
#'     set.seed(2)
#'     ta$y= intercept + (b1*ta$X1) + (b2*ta$X2) + (b3*ta$X3) + (b4*ta$X1*ta$X2) + rnorm(n=train_sample+500, mean=0, sd=0.25)
#'   }
#'   set.seed(2)
#'   index=sample(1:nrow(ta), test_size, replace = F)
#'   originaldf=traindf=ta[-index,]
#'   validationdf=ta[index,]
#'
#'   if(Missingdata == TRUE){
#'     temp_df=traindf[,-ncol(traindf)]
#'     missing_nrows= floor(maxmiss_per*nrow(temp_df))
#'     set.seed(2); temp_df=apply(temp_df, 2, function(x) {x[sample(1:length(x), missing_nrows)] <-NA; x})
#'     traindf[,-ncol(traindf)]=temp_df
#'     in_data=traindf[!complete.cases(traindf),]
#'     traindf=in_data
#'   }
#'   return(list(train=traindf,test=validationdf, original = originaldf))
#' }
#'
