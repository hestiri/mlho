#' iterative modeling with MLHO
#'
#' @param multicore if you want to parallelize the process
#' @param dems table containing the demographic variables
#' @param preProc preprocessig on the train data or not
#' @param dbmart dbmart table
#' @param labels should be the labeldt table
#' @param test.sample put 20 if you want to use 20 percent for testing and 80 percent for training
#' @param MSMR.binarize MSMR.lite parameter
#' @param MSMR.sparsity MSMR.lite parameter
#' @param MSMR.jmi MSMR.lite parameter
#' @param MSMR.topn MSMR.lite parameter
#' @param mlearn.save.model mlearn parameter
#' @param mlearn.note mlearn parameter
#' @param mlearn.aoi mlearn parameter
#' @param mlearn.cv mlearn parameter
#' @param mlearn.nfold mlearn parameter
#' @param iterations number of iterations you want. recommended at least 5. needs to be numeric
#'
#' @return
#' @export
#'
mlho.it <- function(dbmart,
                  labels = labeldt,
                  dems = NULL,
                  test.sample = 30,
                  MSMR.binarize=FALSE,
                  MSMR.sparsity=0.005,
                  MSMR.jmi=TRUE,
                  MSMR.topn=200,
                  mlearn.save.model=FALSE,
                  mlearn.note="mlho_phewas run",
                  mlearn.aoi="demo",
                  mlearn.cv="cv",
                  mlearn.nfold=5,
                  multicore=FALSE,
                  preProc=TRUE,
                  iterations=5)
{

    if(multicore==TRUE){
    ###setup parallel backend
    cores<-detectCores()
    cl <- makeCluster(cores[1]-2)
    registerDoParallel(cl)}


  feats <- list()
  for (i in 1:iterations){
  tryCatch({
    print(paste0("iteration ",i))

  uniqpats <- c(as.character(unique(dbmart$patient_num)))
  #
  test_ind <- sample(uniqpats,
                     round((test.sample/100)*length(uniqpats)))

  test_labels <- subset(labels,labels$patient_num %in% c(test_ind))
  # print("test set lables:")
  table(test_labels$label)
  train_labels <- subset(labels,!(labels$patient_num %in% c(test_ind)))
  # print("train set lables:")
  table(train_labels$label)
  # train and test sets
  dat.train  <- subset(dbmart,!(dbmart$patient_num %in% c(test_ind)))
  dat.test <- subset(dbmart,dbmart$patient_num %in% c(test_ind))

  uniqpats.train <- c(as.character(unique(dat.train$patient_num)))

  ##here is the application of MSMR.lite
  dat.train <- MSMSR.lite(MLHO.dat=dat.train,
                          patients = uniqpats.train,
                          sparsity=MSMR.sparsity,
                          labels,
                          topn=MSMR.topn,
                          jmi=MSMR.jmi,
                          binarize=MSMR.binarize
                          )
  dat.test <- subset(dat.test,dat.test$phenx %in% colnames(dat.train))

  uniqpats.test <- c(as.character(unique(dat.test$patient_num)))

  dat.test <- MSMSR.lite(MLHO.dat=dat.test,patients = uniqpats.test,sparsity=NA,jmi = FALSE,labels,binarize=MSMR.binarize)


  if(!is.null(dems)){
    dat.train <- merge(dat.train,dems, by="patient_num")
    dat.test <- merge(dat.test,dems, by="patient_num")
    }

  model.i <- mlearn(dat.train,
                    dat.test,
                    dems=dems,
                    save.model=mlearn.save.model,
                    classifier="glmboost",
                    note=paste0(mlearn.note," ",i),
                    cv=mlearn.cv,
                    nfold=mlearn.nfold,
                    aoi=mlearn.aoi)


  dbmart.concepts <- dbmart[!duplicated(paste0(dbmart$phenx)), c("phenx","DESCRIPTION")]
  mlho.features <- data.frame(merge(model.i$features,dbmart.concepts,by.x="features",by.y = "phenx"))
  mlho.features$iteration <- i
  mlho.features$model.i.roc <- model.i$ROC$roc
  colnames(mlho.features)[2] <- "OR"

  feats[[i]] <- mlho.features
  rm(mlho.features)
  },
  error = function(fr) {cat("ERROR :",conditionMessage(fr), "\n")})
    print(paste0("iteration ",i, " done!"))
    closeAllConnections()
  }
  features <- do.call(rbind, lapply(feats, data.frame, stringsAsFactors=FALSE))
  features$X <- NULL

  return(features)
}
