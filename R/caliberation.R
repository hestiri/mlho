#' Computes calibration data for classification
#'
#' @param datval testing data
#' @param model model
#' @param label.col the column number with labels
#'
#' @return a data frame with calibration information
#' @export
#'
caliber <- function(datval,#data on which validation will be computed
                    model,#model!
                    label.col#column number with labels
){
  library(dplyr);library(pROC);library(PRROC)
  ##predicting on validation data and storing the metrics
  datval[c("N","Y")] <- data.frame(predict(model, newdata = datval, type = "prob"))
  datval$actual <- ifelse(datval[,label.col] == "Y", 1,0)

  pred.probs <-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  dat <- list()
  for (i in 2:length(pred.probs)){
    tp <- data.frame(mean(subset(datval$actual,datval$Y<=pred.probs[i] & datval$Y>pred.probs[i-1])))
    colnames(tp) <- "true.bin.probability"
    tp$predicted.probability <- pred.probs[i]
    dat[[i]] <- tp
    rm(tp)
  }
  calibration <- do.call(rbind, lapply(dat, data.frame, stringsAsFactors=FALSE))
  return(calibration)
}
