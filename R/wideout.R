#' wideout
#'
#' @param data the dbmart
#' @param binarize if you want to use the features as binary, set it to TRUE -- default is FALSE
#' @param sparsity if the first step of MSMR, set a threshold between 0 and 1 -- 1 is 100 percent!
#' @param patients
#'
#' @return
#' @export
#'
wideout <- function(data,
                    binarize=FALSE,
                    sparsity=NA,
                    patients)
{
  dbmart <- data
  setDT(dbmart)
  dbmart[,row := .I]
  dbmart$value.var <- 1


  # aggregating by unique patients
  dbmart.agg <- ddply(dbmart, ~ phenx,summarise,distinct_patients=length(unique(patient_num)))

  dbmart.wide <- reshape2::dcast(dbmart, patient_num ~ phenx, value.var="value.var", fun.aggregate = length)
  dbmart.wide <- as.data.frame(dbmart.wide)
  dbmart.wide <- dbmart.wide[, !(names(dbmart.wide) %in% c("NA"))]
  #binarize?
  if(binarize==TRUE){
    dbmart.wide[,2:dim(dbmart.wide)[2]] <- +(dbmart.wide[,2:dim(dbmart.wide)[2]] > 0)}

  if(!is.na(sparsity)){
    #remove low-prevalence features
    avrs <- c(as.character(subset(dbmart.agg$phenx,dbmart.agg$distinct_patients > round(length(patients)*sparsity))))
    dbmart.wide <- dbmart.wide[, names(dbmart.wide) %in% avrs | names(dbmart.wide) %in% c("patient_num")]
  }

  return(dbmart.wide)


}
