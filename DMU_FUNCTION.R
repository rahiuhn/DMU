# Cut the data for DMU
hc_fun = function(inputdf){
  code_set=inputdf
  code_set[!(is.na(code_set))]<-1
  code_set[is.na(code_set)]<-0

  ## Make rowcluster
  # hc=stats::hclust(dist(code_set),method="ward.D2")
  hc = mhc(dist(code_set),method="ward.D2")
  return(hc)
}

datasplit=function(inputdf, cutratio=5, cluster_number=NA){

  norm_set=inputdf
  code_set=inputdf
  code_set[!(is.na(code_set))]<-1
  code_set[is.na(code_set)]<-0

  ## Make rowcluster
  # hc=stats::hclust(dist(code_set),method="ward.D2")
  hc = mhc_fun(inputdf = inputdf)
  ### Optimize the number of cuts
  cut=floor(nrow(code_set)/cutratio)
  # scan for the cuts which give variable to samplesize ratio of atleast 5
  if(is.na(cluster_number)){
    cutlist=lapply(1:cut, function(no_of_clusters) {
      x = no_of_clusters
      fit=cutree(hc, x)
      # Those cuts are processed which meet minimum cutratio criteria
      if(min(table(fit))<cutratio){out=NULL}
      else{
        norm_set$fit=fit
        code_set$fit=fit
        store=list()
        # For each cut size
        ini_list=lapply(1:x, function(i){
          clus_len=length(fit[which(fit==i)])
          a=code_set[which(code_set$fit==i),1:(ncol(code_set)-2)]
          a = a[colSums(a)==nrow(a)]
          constant_y=unique(norm_set[which(norm_set$fit==i),"y"])
          # Make sure only those cuts are retained which does not have constant "y" and sufficient data
          if(ncol(a)>0 & (nrow(a)/ncol(a)) >= cutratio & length(constant_y)>1){
            tempdf=norm_set[which(norm_set$fit==i),c(names(a),"y")]
            tempdf=tempdf[lapply(tempdf, function(x) length(unique(x)))>1]
            if(ncol(tempdf)>1){store[[i]]=tempdf}
            else{store[[i]]=NULL}
          }else{store[[i]]=NULL}
          store=store[lapply(store,length)>0]
        })
        ini_list=ini_list[lapply(ini_list,length)>0]
        out=ini_list
        out
      }
      out=out[lapply(out,length)>0]
      out
    })
  }
  else{
    cutlist=lapply(cluster_number, function(no_of_clusters) {
      x = no_of_clusters
      if(cluster_number>cut | cluster_number<1){out=NULL}
      else{
        fit=cutree(hc, x)
        # Those cuts are processed which meet minimum cutratio criteria
        ## Step 1: Eliminate the cluster groups containing any cluster whose row to column ratio is less than cut ratio
        if(min(table(fit))<cutratio){out=NULL}
        else{
          norm_set$fit=fit
          code_set$fit=fit

          # For each cut size or cluster, find the variables with complete rows
          ini_list=lapply(1:x, function(i){

            numb_rows_in_clus = length(fit[which(fit==i)])

            a=code_set[which(code_set$fit==i),1:(ncol(code_set)-2)]

            a = a[colSums(a)==nrow(a)]
            constant_y=unique(norm_set[which(norm_set$fit==i),"y"])

            # Step 2: Make sure only those cuts are retained which does not have constant "y" and sufficient data
            if(ncol(a) > 0 & (nrow(a)/ncol(a)) >= cutratio & length(constant_y) > 1){
              tempdf=norm_set[which(norm_set$fit==i),c(names(a),"y")]

              tempdf=tempdf[lapply(tempdf, function(x) length(unique(x)))>1]

              if(ncol(tempdf)>1){store=tempdf}
              else{store=NULL}
            }else{store=NULL}
            #str(store)
            store
          })

          ini_list=ini_list[lapply(ini_list,length)>0]
          out=ini_list
          out
        }
        out=out[lapply(out,length)>0]
        #str(out)
        out
      }

    })
    #cutlist=list(cutlist)
  }
  #str(cutlist)
  dfcut=cutlist[lapply(cutlist,length)>0]
  return(dfcut)
}

# Run the Bayesian
bay_reg_mcmc=function(inputdf, outvar="y", df_beta, run=1){

  # Get df for the step and get predictor name
  Bay_Trial_Train=inputdf

  Var_model=names(Bay_Trial_Train)[which(names(Bay_Trial_Train)!=outvar)]

  #Prepare the formula
  inputvar=paste0(Var_model,collapse = "+")

  equation=as.formula(paste0(c(outvar,inputvar), collapse = "~"))

  # Prepare the input values
  prior_beta=subset(df_beta, Variable %in% c("(Intercept)",Var_model))$Prior_mu
  prior_beta_precision=1/subset(df_beta, Variable %in% c("(Intercept)",Var_model))$Prior_sd^2
  c0=5; d0=50

  # Run the model and get the results
  model=MCMCpack::MCMCregress(equation, data=Bay_Trial_Train, mcmc=5000, seed=1,b0=prior_beta, B0=prior_beta_precision, c0=c0, d0=d0)

  out=summary(model)$statistics
  # Store the results and update the prior values
  out_df=data.frame(Variable=c("(Intercept)", Var_model),
                    Prior_mu=out[1:(nrow(out)-1),"Mean"],
                    Prior_sd=out[1:(nrow(out)-1),"SD"],
                    posterior_mean=out[1:(nrow(out)-1),"Mean"],
                    posterior_sd=out[1:(nrow(out)-1),"SD"],
                    run=run, stringsAsFactors = F)

  update_var=c("Prior_mu", "Prior_sd","posterior_mean","posterior_sd","run")
  df_beta[match(out_df$Variable,df_beta$Variable),update_var]=out_df[,update_var]
  return(df_beta)
}

# library(foreach)
# Perform DMU
#' @export
DMU=function(hyperpara, data_prep){
  bay_res= lapply(1:(nrow(hyperpara)), function(x) {
    seed=hyperpara[x,5]
    data=data_prep[[x]]

    if(length(data) == 3){
      if((nrow(data[[2]])/ncol(data[[2]]))>1 & length(data[[1]])>0){
          trainset=data[[3]]
          testset=data[[2]]
          res=1
      }else{res=NULL}
    }else{res=NULL}
    if(!is.null(res)){
      inputdata=testset
      datacut=data[[1]]

      bay_resul=lapply(1:length(datacut), function(round){
        dflist=datacut[[round]]
        cut=length(dflist)
        var_model=names(trainset)[which(names(trainset)!="y")]
        Variable=union("(Intercept)", var_model)
        df_beta=data.frame(Variable=Variable, Prior_mu=0, Prior_sd=1e1, posterior_mean=NA, posterior_sd=NA, run=0, stringsAsFactors = F)

        # Run Bayesian
        for(l in 1:length(dflist)){
          miss_data=data.frame(dflist[[l]])
          miss_data_var=intersect(names(trainset), names(miss_data))
          df_beta=bay_reg_mcmc(inputdf= miss_data[,miss_data_var], outvar = "y", df_beta = df_beta, run=l)
        }
        bay_reg = data.frame(Variable=Variable,beta_Prop=df_beta$posterior_mean, sd_prop=df_beta$posterior_sd,  stringsAsFactors = F)
        bay_reg[is.na(bay_reg)]=0

        numb_varb=bay_reg$beta_Prop[1:5]
        names(numb_varb)=bay_reg$Variable[1:5]

        # Get the Performance
        y_estimate=y_est_nointerac(betadf = bay_reg,xtestdf = inputdata, meanbeta = "beta_Prop")
        bay_r=predict_metric(actualy = inputdata[,"y"], predictedy=y_estimate)
        bay_r$Approach="beta_Prop"
        bay_r$seed=seed;
        bay_r
      })
      res=do.call(rbind, bay_resul)
      res
    }
  })
  return(bay_res)
}

mhc = memoise::memoise(stats::hclust)
mhc_fun = memoise::memoise(hc_fun)
mdatasplit=memoise::memoise(datasplit)
mDMU=memoise::memoise(DMU)
