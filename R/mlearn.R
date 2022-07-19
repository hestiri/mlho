#' the MLHO ML training and testing
#'
#' @param save.model do you want to save the model?
#' @param classifier the classification algorithm
#' @param note add a descriptive word about your modeling
#' @param aoi outcome of interest
#' @param multicore do you want to parallelize the process?
#' @param dat.train training data
#' @param dat.test testing data
#' @param dems demographic variables
#' @param cv cross validatio method
#' @param nfold nfold cross validation folds
#' @param preProc preprocessig on the train data or not
#'
#' @return
#' @export
#'
mlearn <- function(dat.train,
                  dat.test,
                  dems = NULL,
                  save.model=FALSE,
                  classifier="glmboost",
                  note="mlho_terst_run",
                  cv="cv",
                  nfold=5,
                  aoi="label",
                  multicore=FALSE,
                  preProc=TRUE)
{

    if(multicore==TRUE){
    ###setup parallel backend
    cores<-detectCores()
    cl <- makeCluster(cores[1]-2)
    registerDoParallel(cl)}

  if (!is.null(dat.test)) {
  if(!is.null(dems)){
    dat.train <- merge(dat.train,dems, by="patient_num")
    dat.test <- merge(dat.test,dems, by="patient_num")
    }


      #modeling
      print("the modeling!")
      dat.train$label <- ifelse(dat.train$label ==1 , "Y","N")
      dat.test$label <- ifelse(dat.test$label ==1 , "Y","N")

      dat.train$label <- as.factor(dat.train$label)
      dat.test$label <- as.factor(dat.test$label)


      goldstandard <- "label"
      dat.train$patient_num <- NULL

      # set.seed(1395)
      Sys.setenv(R_MAX_NUM_DLLS = 999)

      options(java.parameters = "-Xmx8048m")

      ###starting prediction and testing
      train_control <- caret::trainControl(method=cv, number=nfold,#method="boot", number=nrow(dat.test),
                                           summaryFunction = twoClassSummary,
                                           classProbs = T,
                                           savePredictions = T)


      if(preProc == FALSE) {

        model <- caret::train(as.formula(paste(goldstandard, "~ .")),
                              data=dat.train
                              , trControl=train_control
                              , method = classifier)}

      if(preProc == TRUE) {preProc=c("center", "scale")

      model <- caret::train(as.formula(paste(goldstandard, "~ .")),
                            data=dat.train
                            , trControl=train_control
                            , method = classifier
                            ,preProc)}



      test_feats <- c(as.character(names(dat.test)))
      test.miss <- setdiff(names(dat.train),names(dat.test))
      dat.test[test.miss] <- 0


      ROC <- metrix(datval = dat.test,model=model,label.col = which( colnames(dat.test)=="label" ),note=note,op=0.5,phenx = aoi,topn = ncol(dat.train)-1)
      ROC$cv_roc <- mean(model$results$ROC)
      ROC$cv_roc_sd <- mean(model$results$ROCSD)
      ROC$classifier <- classifier

      if(classifier=="glmboost"){
      ##coefficients
      coefficients <- data.frame(unlist(coef(model$finalModel, model$bestTune$lambda)))
      colnames(coefficients) <- "value"
      coefficients$features <- rownames(coefficients)
      rownames(coefficients) <- NULL
      coefficients <- subset(coefficients,coefficients$features != "(Intercept)")
      coefficients$features <- sub('`\\`', '', coefficients$features, fixed = TRUE)
      coefficients$features <- sub('\\``', '', coefficients$features, fixed = TRUE)
      coefficients$value <- exp(coefficients$value*2)
      coefficients$classifier <- classifier
      coefficients$aoi <- aoi
      }
      if(classifier!="glmboost"){
      coefficients <- data.frame(varImp(model,scale = TRUE)$importance)
      coefficients$features <- as.character(rownames(coefficients))
      rownames(coefficients) <- NULL
      coefficients$features <- sub('`', '', coefficients$features, fixed = TRUE)
      coefficients$features <- sub('`', '', coefficients$features, fixed = TRUE)
      coefficients <- subset(coefficients,coefficients$Overall > 0)
      coefficients$classifier <- classifier
      coefficients$aoi <- aoi
      }


      test.miss <- setdiff(coefficients$features,test_feats)

      ###calibration
      cali <- caliber(datval = dat.test,model=model,label.col = which( colnames(dat.test)=="label" ))
      cali$classifier <- classifier
      cali$phenx <- aoi

      ##error
      err <- absErr(datval = dat.test,model=model,label.col = which( colnames(dat.test)=="label" ))
      err$classifier <- classifier
      err$phenx <- aoi


      if(save.model==TRUE){
        ##save the model
        saveRDS(model, paste0(getwd(),"/results/model_",classifier,"_",note,"_",aoi,".rds"))
      }




  return(list(
    ROC=ROC,
    features=coefficients,
    calibrations=cali,
    AE=err,
    missing.features=test.miss
  ))
  }


if (is.null(dat.test)) {
  if(!is.null(dems)){
    dat.train <- merge(dat.train,dems, by="patient_num")
  }


  #modeling
  print("the modeling!")
  dat.train$label <- ifelse(dat.train$label ==1 , "Y","N")

  dat.train$label <- as.factor(dat.train$label)


  goldstandard <- "label"
  dat.train$patient_num <- NULL

  # set.seed(1395)
  Sys.setenv(R_MAX_NUM_DLLS = 999)

  options(java.parameters = "-Xmx8048m")

  logitMod <- glm(as.formula(paste(goldstandard, "~ .")),
                  data=dat.train, family=binomial(link="logit"))


  summary(logitMod)
  # car::vif(logitMod)##multicolinearity is fine if vif values are below 4

  # logitMod$coefficients
  lreg.or <-exp(cbind(OR = coef(logitMod), confint(logitMod))) ##CIs using profiled log-likelihood
  output <- data.frame(round(lreg.or, digits=4))
  output$features <- rownames(output)
  rownames(output) <- NULL
  ps <- data.frame(
    round(
      coef(summary(logitMod))[,4],4))#P(Wald's test)
  ps$features <- rownames(ps)
  rownames(ps) <- NULL
  output <- merge(output,ps,by="features")
  output$features <- sub('`', '', output$features, fixed = TRUE)
  output$features <- sub('`', '', output$features, fixed = TRUE)
  output <- subset(output,output$features != "(Intercept)")
  colnames(output) <- c("features","OR","low","high","P")
  output$classifier <- "GLM"
  output$aoi <- aoi

  if(save.model==TRUE){
    ##save the model
    saveRDS(model, paste0(getwd(),"/results/model_",classifier,"_",note,"_",aoi,".rds"))
  }




  return(features=output)
}
}
