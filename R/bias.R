#' Bias evaluation metrics
#'
#' @param group #demographic or patient groups, defaults at all
#' @param month #temporal segmentation of the data, defaults at all -- i.e., no time stamps
#' @param err ##the error file generated from mlho learn
#' @param model.name ##name of the model
#' @param keys ##phenotype and model information for merging extracted from err
#' @param plot ##wanna plot out reliability diagrams?
#' @param calibrate ##wanna calibrate the computed probabilities?
#'
#' @return
#' @export
#'

bias <- function(group="all",
                 month="all",
                 err,#patient error file from mlho
                 model.name,#model name
                 keys,#information about the model
                 plot=FALSE, #plot reliability diagram?
                 calibrate="RAW"####if we were to calibrate the probabilities. options are ("BBQ","GUESS","BIN","ISO")
){

  if (!require("CalibratR")) install.packages('CalibratR', repos = "http://cran.rstudio.com/")
  if(month == "all") {
    err.x <- subset(err,err$patient_num %in% eval(parse(text = group)))} else if(month != "all") {
      err.x <- subset(err,err$patient_num %in% eval(parse(text = group)) & err$patient_num %in% eval(parse(text = month)) )}

  if(dim(table(err.x$actual)) < 2) {
    out.x <- data.frame("ECE_equal_width"=99999,"MCE_equal_width"=99999,"ECE_equal_freq"=99999,"MCE_equal_freq"=99999,"RMSE"=99999,
                        "CLE_class_1"=99999,"CLE_class_0"=99999,"brier"=99999,"brier_class_1"=99999,"brier_class_0"=99999,"sens"=99999,
                        "spec"=99999,"acc"=99999,"ppv"=99999,"npv"=99999,"cutoff"=99999,"auc"=99999)} else if (dim(table(err.x$actual)) == 2) {

                          if(calibrate=="RAW"){print("uncalibrated probabilities")} else if(calibrate=="BBQ"){
                            model <- CalibratR:::build_BBQ(err.x$actual,err.x$Y)
                            err.x$Y <- CalibratR:::predict_BBQ(model,err.x$Y,option=1)$predictions} else if(calibrate=="GUESS"){
                              model <- CalibratR:::build_GUESS(err.x$actual,err.x$Y)
                              err.x$Y <- CalibratR:::predict_GUESS(model,err.x$Y)$predictions} else if(calibrate=="BIN"){
                                model <- CalibratR:::build_hist_binning(err.x$actual,err.x$Y)
                                err.x$Y <- CalibratR:::predict_hist_binning(model,err.x$Y)$predictions} else if(calibrate=="ISO"){
                                  if (!require("rfUtilities")) install.packages('rfUtilities', repos = "http://cran.rstudio.com/")
                                  tryCatch({
                                    o <- rfUtilities::probability.calibration(err.x$actual,err.x$Y)
                                    if ("o" %in% ls() == TRUE) {err.x$Y <- o} else if ("o" %in% ls() == FALSE) {calibrate <- "RAW"}
                                  },
                                  error = function(frisko) {cat("ERROR :",conditionMessage(frisko), "\n")})
                                }

                          out.x <- CalibratR:::reliability_diagramm(err.x$actual,err.x$Y,bins = 10,plot_rd = plot)
                          out.x.temp <- out.x
                          out.x <- data.frame(out.x$calibration_error,out.x$discrimination_error)

                        }

  if (plot==TRUE) {
    cali <- data.frame(cbind(out.x.temp[["mean_pred_per_bin"]],out.x.temp[["accuracy_per_bin"]]))
    colnames(cali) <- c("mean_pred_per_bin","accuracy_per_bin")
    # out.x[["diagram_plot"]][["plot_env"]][["pvalue_per_bin"]]
    # cali=subset(cali,cali$mean_pred_per_bin > 0)
    cali$model.name <- model.name
    cali <- merge(cali,keys,by="model.name")
    cali$group <- paste0(group)
    cali$month <- paste0(month)
    cali$n <- nrow(err.x)
    cali$calibration <- calibrate
    rm(out.x.temp)
  } else if (plot==FALSE) {
    cali <- data.frame("plot option was FALSE!!! Turn it on (TRUE) to get calibration plot data.)")
    colnames(cali) = "ERROR"
  }

  out.x$model.name <- model.name
  out.x <- merge(out.x,keys,by="model.name")
  out.x$group <- paste0(group)
  out.x$month <- paste0(month)
  out.x$n <- nrow(err.x)
  out.x$calibration <- calibrate


  return(
    list(metrics=out.x,
         calibrations=cali))
}

