#' Title
#'
#' @param datval data on which validation will be computed
#' @param model the model
#' @param label.col the column number with labels
#' @param note model description
#' @param op cutoff for custom ppv/npv/sensitivity/specificity
#' @param phenx outcome of the study or phenotype of the interest -- default is "phenotype"
#' @param topn number of features used in the training data
#' @param class binary or continuous prediction
#'
#' @return output includes performance model metrics
#' @export
#'
#' @examples metrix(datval = dat.VAL,model=MODEL,label.col = which( colnames(dat.VAL)=="label" ),note="MLHO model",op=0.5,phenx = "aoi"severity,topn = 123)
metrix <- function(datval,
                   model,
                   label.col,
                   note,
                   op,
                   phenx="phenotype",
                   topn,
                   class="binary"
){

  if (class == "binary"){
    datval[c("N","Y")] <- data.frame(predict(model, newdata = datval, type = "prob"))
    # calculating the AUC ROC
    roc_obj1 <- roc(datval[,label.col], datval$Y)
    datval$actual <- ifelse(datval[,label.col] == "Y", 1,0)
  } else if (class !="binary"){
    datval[c("Y")] <- data.frame(predict(model, newdata = datval))
    # calculating the AUC ROC
    roc_obj1 <- roc(datval[,label.col], datval$Y)
    datval$actual <- ifelse(datval[,label.col] == "Y", 1,0)
  }
  ROC <- data.frame(paste0(note))
  colnames(ROC) <- c("model")
  ROC$roc <- as.numeric(roc_obj1$auc)
  sensificities1 <- data.frame(cbind(roc_obj1$sensitivities,roc_obj1$specificities,roc_obj1$thresholds))
  colnames(sensificities1) <- c("sensitivities","specificities","threshold")
  sensificities1$J <- sensificities1$sensitivities+sensificities1$specificities-1
  ROC$J.specificity <- as.numeric(sensificities1[sensificities1$J == as.numeric(max(sensificities1$J)), "specificities"][1])
  ROC$J.sensitivity <- as.numeric(sensificities1[sensificities1$J == as.numeric(max(sensificities1$J)), "sensitivities"][1])###
  ROC$thresholdj <- as.numeric(sensificities1[sensificities1$J == as.numeric(max(sensificities1$J)), "threshold"][1])
  ROC$ppv.j <- ppv(datval$actual, datval$Y, cutoff = ROC$thresholdj)
  ROC$npv.j <- InformationValue::npv(datval$actual, datval$Y, threshold = ROC$thresholdj)
  ROC$cutoff <- op
  ROC$ppv.cutoff <- InformationValue::precision(datval$actual, datval$Y, threshold = op)
  ROC$npv.cutoff <- InformationValue::npv(datval$actual, datval$Y, threshold = op)
  ROC$sensitivity.cutoff <- InformationValue::sensitivity(datval$actual, datval$Y, threshold = op)
  ROC$specificity.cutoff <- InformationValue::specificity(datval$actual, datval$Y, threshold = op)

  #### calculating ROC and PRROC using another package
  roc2 <- PRROC::roc.curve(datval[(datval[,label.col] == "Y"),"Y"],
                           datval[(datval[,label.col] == "N"),"Y"])

  # PR
  pr <- PRROC::pr.curve(datval[(datval[,label.col] == "Y"),"Y"],
                        datval[(datval[,label.col] == "N"),"Y"])

  ROC$roc2 <- as.numeric(roc2$auc)
  ROC$pr.integral <- as.numeric(pr$auc.integral)
  ROC$pr.davis.goadrich <- as.numeric(pr$auc.davis.goadrich)
  ROC$phenx <- phenx
  ROC$topn <- topn

  return(ROC)
}
