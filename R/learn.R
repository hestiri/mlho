#' the ML part
#'
#' @param data
#' @param demographics
#' @param augment if you want to add demographic variables to your model set it to TRUE
#' @param tst.size test/train set ratio -- default set at 0.2 (20%)
#' @param save.model do you want to save the model?
#' @param classifier the classification algorithm
#' @param note add a descriptive word about your modeling
#' @param aoi outcome of interest
#' @param multicore do you want to parallelize the process?
#'
#' @return
#' @export
#'
#' @examples
learn <- function(data,
                  demographics,
                  augment=FALSE,
                  tst.size=0.2,
                  save.model=FALSE,
                  classifier="glmboost",
                  note="mlho_terst_run",
                  aoi,
                  multicore=FALSE)
{
  dbmart <- data
  dems <- demographics

  if(multicore==TRUE){
    ###setup parallel backend
    cores<-detectCores()
    cl <- makeCluster(cores[1]-2)
    registerDoParallel(cl)
  }
  if(augment==TRUE){
    dems2 <- dems[,demographic_variables]
  }

  labeldt <- dplyr::select(dems,patient_num,label=sever)

  for (j in 1:9){
    tryCatch({

      test_ind <- sample(unique(labeldt[,"patient_num"]),
                         round(tst.size*nrow(labeldt)))

      ###let's identify the test and training sets
      ###we want to split data into train and validation (test) sets
      test_labels <- subset(labeldt,labeldt$patient_num %in% c(test_ind))
      table(test_labels$label)
      train_labels <- subset(labeldt,!(labeldt$patient_num %in% c(test_ind)))
      table(train_labels$label)

      if (augment == TRUE){
        ###add demographics
        train_labels <- merge(train_labels,dems2,by="patient_num")
        test_labels <- merge(test_labels,dems2,by="patient_num")

      }

      ##now create the test and training data subsets.
      dat.VAL <- merge(test_labels,AVR,by="patient_num")
      dat.train <- merge(train_labels,AVR,by="patient_num")
      dat.train[,c("patient_num")] <- NULL

      #modeling
      print("to the modeling!")
      goldstandard <- "label"


      # set.seed(1395)
      Sys.setenv(R_MAX_NUM_DLLS = 999)

      options(java.parameters = "-Xmx8048m")

      ###starting prediction and testing
      train_control <- caret::trainControl(method="cv", number=5,#method="boot", number=nrow(dat.VAL),
                                           summaryFunction = twoClassSummary,
                                           classProbs = T,
                                           savePredictions = T)


      preProc=c("center", "scale")

      model <- caret::train(as.formula(paste(goldstandard, "~ .")),
                            data=dat.train
                            , trControl=train_control
                            , method = classifier
                            ,preProc)
      ROC <- metrix(datval = dat.VAL,model=model,label.col = which( colnames(dat.VAL)=="label" ),note=note,op=0.5,phenx = aoi,topn = ncol(dat.train)-1)
      ROC$cv_roc <- mean(model$results$ROC)
      ROC$cv_roc_sd <- mean(model$results$ROCSD)
      ROC$iteration <- j
      ROC$classifier <- classifier

      ##coefficients
      coefficients <- data.frame(unlist(coef(model$finalModel, model$bestTune$lambda)))
      colnames(coefficients) <- "value"
      coefficients$features <- rownames(coefficients)
      rownames(coefficients) <- NULL
      coefficients <- subset(coefficients,coefficients$features != "(Intercept)")
      coefficients$iteration <- j
      coefficients$classifier <- classifier
      coefficients$phenx <- aoi

      ###calibration
      cali <- caliber(datval = dat.VAL,model=model,label.col = which( colnames(dat.VAL)=="label" ))
      cali$iteration <- j
      cali$classifier <- classifier
      cali$phenx <- aoi

      if(save.model==TRUE){
        ##save the model
        saveRDS(model, paste0(getwd(),"/results/model_",classifier,"_",note,"_",aoi,j,".rds"))
      }
      write.csv(ROC,file = paste0(getwd(),"/results/ROC_",classifier,"_",note,"_",aoi,j,".csv"))
      write.csv(coefficients,file = paste0(getwd(),"/results/coeffs_",classifier,"_",note,"_",aoi,j,".csv"))
      write.csv(cali,file = paste0(getwd(),"/results/cali_",classifier,"_",note,"_",aoi,j,".csv"))

    },
    error = function(m) {cat("ERROR :",conditionMessage(m), "\n")})
  }


  return(list(
    ROC,
    coefficients,
    cali
  ))
}
