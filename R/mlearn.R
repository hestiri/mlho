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

  if(!is.null(dems)){
    dat.train <- merge(dat.train,dems, by="patient_num")
    dat.test <- merge(dat.test,dems, by="patient_num")
    }


      #modeling
      print("to the modeling!")
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



      if(preProc == TRUE) {preProc=c("center", "scale")}

      model <- caret::train(as.formula(paste(goldstandard, "~ .")),
                            data=dat.train
                            , trControl=train_control
                            , method = classifier
                            ,preProc)

      test.miss <- setdiff(names(dat.train),names(dat.test))
      dat.test[test.miss] <- 0


      ROC <- metrix(datval = dat.test,model=model,label.col = which( colnames(dat.test)=="label" ),note=note,op=0.5,phenx = aoi,topn = ncol(dat.train)-1)
      ROC$cv_roc <- mean(model$results$ROC)
      ROC$cv_roc_sd <- mean(model$results$ROCSD)
      ROC$classifier <- classifier

      ##coefficients
      coefficients <- data.frame(unlist(coef(model$finalModel, model$bestTune$lambda)))
      colnames(coefficients) <- "value"
      coefficients$features <- rownames(coefficients)
      rownames(coefficients) <- NULL
      coefficients <- subset(coefficients,coefficients$features != "(Intercept)")
      coefficients$classifier <- classifier
      coefficients$phenx <- aoi

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
    coefficients=coefficients,
    calibrations=cali,
    MAE=err,
    missing.features=test.miss
  ))
}
