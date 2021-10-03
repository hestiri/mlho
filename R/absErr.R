#' patient-level prediction error
#'
#' @param datval #data on which validation will be computed
#' @param model #model!
#' @param label.col #column number with labels
#'
#' @return
#' @export
#'

absErr <- function(datval,
                   model,
                   label.col
){

  datval[c("N","Y")] <- data.frame(predict(model, newdata = datval, type = "prob"))
  datval$actual <- ifelse(datval[,label.col] == "Y", 1,0)
  datval <- datval[,c("patient_num","actual","Y")]
  datval$absErr <- abs(datval$actual-datval$Y)



  return(datval)
}
