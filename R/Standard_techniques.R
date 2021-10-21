#' The funcstions in this file focus determining the performance of the models using linear regression and predictive mean matching based MICE.


# Run the regression
reg_results=function(hyperpara, data_prep){
  reg_res=lapply(1:(nrow(hyperpara)-0), function(x) {
    seed=hyperpara[x,5]
    data=data_prep[[x]]

    if(length(data) == 3){
      if(nrow(data[[3]][complete.cases(data[[3]]),])>1){
        inputdata=data[[2]]
        trainset=data[[3]]
        reg=lm(y~., data = trainset)
        lm_reg=data.frame(beta_Reg=summary(reg)$coefficients[,1],
                          sd_lm=summary(reg)$coefficients[,2],
                          Variable=attributes(summary(reg)$coefficients)$dimnames[[1]],
                          stringsAsFactors = F)
        y_estimate=y_est_nointerac(betadf = lm_reg,xtestdf = inputdata, meanbeta = "beta_Reg")
        res=predict_metric(actualy = inputdata[,"y"], predictedy=y_estimate)
        res$Approach = "beta_Reg";
        res$seed = seed;
        res
      }
      else{res=NULL}
    }else{res=NULL}
  })
  return(reg_res)
}

# Run the Mice regression
mice_results=function(hyperpara, data_prep){
  mice_res=lapply(1:(nrow(hyperpara)), function(x) {
    seed = hyperpara[x,5]
    data = data_prep[[x]]
    if(length(data) == 3){
      if((nrow(data[[2]])/ncol(data[[2]]))>1){
        trainset=data[[3]]
        testset=data[[2]]
        res=1
      }
      else{res=NULL}
    }
    else{res=NULL}

    if(!is.null(res)){
      inputdata=testset
      drop_yindex=which(colnames(trainset)=="y")
      imp=mice::mice(trainset[,c(-drop_yindex)], seed = 1, printFlag = F)
      mice_lm_res=lapply(1:5, function(imputer){
        comp_imp=mice::complete(imp,imputer)
        Var_model=names(comp_imp)
        comp_imp$y=trainset$y
        allcompcols <- sapply(comp_imp, function(x) all(!is.na(x)))
        comp_imp[ ,-which(allcompcols)] <- 0

        mice_res=lm(y~., data=comp_imp)
        res=summary(mice_res)$coefficients[,1]
        res
      })

      mice_lm=t(apply(data.frame(mice_lm_res),1,function(x) {c(beta_Imputed_Reg=mean(x, na.rm=TRUE),sd_mice=sd(x, na.rm = TRUE))}))
      mice_lm=data.frame(mice_lm, Variable=rownames(mice_lm), stringsAsFactors = F)
      mice_lm[is.na(mice_lm)]=0
      y_estimate=y_est_nointerac(betadf = mice_lm,xtestdf = inputdata, meanbeta = "beta_Imputed_Reg")
      res=predict_metric(actualy = inputdata[,"y"], predictedy=y_estimate)
      res$Approach="beta_Imputed_Reg"

       res$seed=seed;
      res
    }
  })
  return(mice_res)
}

# Run KNN IMP
knn_results=function(hyperpara, data_prep){
  mice_res=lapply(1:(nrow(hyperpara)), function(x) {
    seed = hyperpara[x,5]
    data = data_prep[[x]]
    if(length(data) == 3){
      if((nrow(data[[2]])/ncol(data[[2]]))>1){
          trainset=data[[3]]
          testset=data[[2]]
          res= 1
      }else{res=NULL}
    }else{res=NULL}
    if(!is.null(res)){
      inputdata=testset
      drop_yindex=which(colnames(trainset)=="y")
      imp = VIM::kNN(trainset[,c(-drop_yindex)], k=10)
      comp_imp = imp[,1:(ncol(trainset)-1)]
      Var_model=names(comp_imp)
      comp_imp$y=trainset$y
      knn_res=lm(y~., data=comp_imp)
      res=summary(knn_res)$coefficients[,1]
      knn_reg=data.frame(beta_knn=res,
                        sd_lm=summary(knn_res)$coefficients[,2],
                        Variable=attributes(summary(knn_res)$coefficients)$dimnames[[1]],
                        stringsAsFactors = F)

      y_estimate=y_est_nointerac(betadf = knn_reg, xtestdf = inputdata, meanbeta = "beta_knn")

      res=predict_metric(actualy = inputdata[,"y"], predictedy=y_estimate)
      res$Approach = "beta_knn";

      res$seed = seed;
      res
    }
  })
  return(mice_res)
}

# Run mean Imp
mean_imp_results = function(hyperpara, data_prep){
  mice_res=lapply(1:(nrow(hyperpara)), function(x) {
    seed = hyperpara[x,5]
    data = data_prep[[x]]
    if(length(data) == 3){
      if((nrow(data[[2]])/ncol(data[[2]]))>1){
          trainset=data[[3]]
          testset=data[[2]]
          res=1
      }else{res=NULL}
    }else{res=NULL}

    if(!is.null(res)){
      inputdata=testset
      drop_yindex=which(colnames(trainset)=="y")
      imp=mice::mice(trainset[,c(-drop_yindex)], seed = 1, method = "mean", printFlag = F, m = 1)
      comp_imp=mice::complete(imp,1)

      Var_model=names(comp_imp)
      comp_imp$y=trainset$y

      allcompcols <- sapply(comp_imp, function(x) all(!is.na(x)))
      comp_imp[ ,-which(allcompcols)] <- 0

      rf_res=lm(y~., data=comp_imp)
      res=summary(rf_res)$coefficients[,1]
      lm_mean=data.frame(beta_mean=res,
                        sd_lm=summary(rf_res)$coefficients[,2],
                        Variable=attributes(summary(rf_res)$coefficients)$dimnames[[1]],
                        stringsAsFactors = F)

      y_estimate=y_est_nointerac(betadf = lm_mean,xtestdf = inputdata, meanbeta = "beta_mean")
      res=predict_metric(actualy = inputdata[,"y"], predictedy=y_estimate)
      res$Approach="beta_mean"

       res$seed=seed;
      res
    }
  })
  return(mice_res)
}

# Run rf Imp
rf_imp_results = function(hyperpara, data_prep){
  mice_res=lapply(1:(nrow(hyperpara)), function(x) {
    seed = hyperpara[x,5]
    data = data_prep[[x]]
    if(length(data) == 3){
      if((nrow(data[[2]])/ncol(data[[2]]))>1){
          trainset=data[[3]]
          testset=data[[2]]
          res=1
      }else{res=NULL}
    }else{res=NULL}

    if(!is.null(res)){
      inputdata=testset
      drop_yindex=which(colnames(trainset)=="y")
      imp = missForest::missForest(trainset[,c(-drop_yindex)])
      comp_imp = imp$ximp
      Var_model=names(comp_imp)
      comp_imp$y=trainset$y
      rf_res=lm(y~., data=comp_imp)
      res=summary(rf_res)$coefficients[,1]
      lm_reg=data.frame(beta_Rf=res,
                        sd_lm=summary(rf_res)$coefficients[,2],
                        Variable=attributes(summary(rf_res)$coefficients)$dimnames[[1]],
                        stringsAsFactors = F)

      y_estimate=y_est_nointerac(betadf = lm_reg, xtestdf = inputdata, meanbeta = "beta_Rf")

      res=predict_metric(actualy = inputdata[,"y"], predictedy=y_estimate)
      res$Approach = "beta_Rf";

      res$seed = seed;
      res
    }
  })
  return(mice_res)
}

# Run True Data
true_results = function(hyperpara, data_prep){
  reg_res=lapply(1:(nrow(hyperpara)-0), function(x) {
    seed=hyperpara[x,5]

    data=data_prep[[x]]
    if(length(data) == 3){
      if((nrow(data[[2]])/ncol(data[[2]]))>1){
          trainset=data[[3]]
          testset=data[[2]]
          res=1
      }else{res=NULL}
    }else{res=NULL}

    if(!is.null(res)){
      inputdata=testset
      reg=lm(y~., data = trainset)
      lm_reg=data.frame(beta_Reg=summary(reg)$coefficients[,1],
                        sd_lm=summary(reg)$coefficients[,2],
                        Variable=attributes(summary(reg)$coefficients)$dimnames[[1]],
                        stringsAsFactors = F)
      y_estimate=y_est_nointerac(betadf = lm_reg,xtestdf = inputdata, meanbeta = "beta_Reg")
      res=predict_metric(actualy = inputdata[,"y"], predictedy=y_estimate)
      res$Approach = "beta_true";
      res$seed = seed;
      res
    }

  })
  return(reg_res)
}

# Memoise the data
mreg_results=memoise::memoise(reg_results)
mmice_results=memoise::memoise(mice_results)
