
#' MSMR lite
#'
#' @param MLHO.dat your dbmart goes here
#' @param labels your labeldt goes here
#' @param binarize if you want the outcome to be binary
#' @param sparsity if you want to apply sparsity
#' @param jmi if you want to do jmi
#' @param topn the number of features to be selected
#' @param patients vector of patients
#' @param multicore if you want to parallelize the jmi
#'
#' @return
#' @export
#'

MSMSR.lite <- function(MLHO.dat,
                       labels,
                       binarize=FALSE,
                       sparsity=NA,
                       jmi=TRUE,
                       topn=200,
                       patients,
                       multicore=FALSE)
{

  # require("plyr");
  require("dplyr")
  require("DT")

  ## aggregating by unique patients
  # MLHO.dat.agg <- plyr::ddply(MLHO.dat, ~ phenx,summarise,distinct_patients=length(unique(patient_num)))
  MLHO.dat.table <- MLHO.dat %>% select(patient_num,phenx) %>% unique() %>% pull(phenx) %>% table()
  MLHO.dat.agg <- data.frame(phenx=names(MLHO.dat.table),distinct_patients=as.vector(MLHO.dat.table)) # For JMI
  if(!is.na(sparsity)){
    print("step - 1: sparsity screening!")
    ##remove low-prevalence features
    # avrs <- c(as.character(subset(MLHO.dat.agg$phenx,MLHO.dat.agg$distinct_patients > round(length(patients)*sparsity))))
    avrs <- MLHO.dat.table[MLHO.dat.table > round(length(patients) * sparsity)] %>% names()
    MLHO.dat <- subset(MLHO.dat,MLHO.dat$phenx %in% avrs)
  }
  # setDT(MLHO.dat)
  # MLHO.dat[,row := .I]
  # MLHO.dat$value.var <- 1

  # MLHO.dat.wide <- reshape2::dcast(MLHO.dat, patient_num ~ phenx, value.var="value.var", fun.aggregate = length)
  # MLHO.dat.wide <- as.data.frame(MLHO.dat.wide)
  MLHO.dat.wide <- MLHO.dat %>%
    dplyr::group_by(patient_num,phenx) %>%
    dplyr::summarise(n=n(),.groups = "drop") %>%
    tidyr::pivot_wider(id_cols = "patient_num",names_from = "phenx",values_from = n,values_fill = 0)
  MLHO.dat.wide <- MLHO.dat.wide[, !(names(MLHO.dat.wide) %in% c("NA"))]

  AVR <- MLHO.dat.wide

  if(jmi == TRUE){
  if(multicore==TRUE){
    ###setup parallel backend for case 4 loops
    cores<-detectCores()
    cl <- makeCluster(cores[1]-2)
    registerDoParallel(cl)
      }

  print("step 2: JMI dimensionality reduction!")
  # AVR <- merge(AVR,labels,by="patient_num")
  AVR <- dplyr::left_join(AVR,labels,by="patient_num")
  require("praznik")
  # AVR <- as.data.frame(AVR)
  ##calculating JOINT mutual information
  JMIs.AVR <- as.data.frame(JMI(AVR[,!(names(AVR) %in% names(labels))],
                                      as.factor(AVR$label),k = dim(AVR)[2]-ncol(labels), threads = 0)$score)

  JMIs.AVR$features <- rownames(JMIs.AVR)
  rownames(JMIs.AVR) <- NULL
  colnames(JMIs.AVR) <- c("JMI.score","phenx")

  # JMIs.AVR <- merge(JMIs.AVR,MLHO.dat.agg,by="phenx",all.x = TRUE)
  JMIs.AVR <- dplyr::left_join(JMIs.AVR,MLHO.dat.agg,by="phenx")
  JMIs.AVR$JMI.score <- round(JMIs.AVR$JMI.score,3)
  JMIs.AVR$rank <- rank(order(order(JMIs.AVR$JMI.score,-JMIs.AVR$distinct_patients)))


  topfeatures.AVR <- subset(JMIs.AVR,JMIs.AVR$rank <= topn)
  topfeatures.AVR <- c(as.character(unique(topfeatures.AVR$phenx)))
  AVR <- AVR[, names(AVR) %in% topfeatures.AVR | names(AVR) %in% names(labels)]

  }

  if (jmi==FALSE){
    # AVR <- merge(AVR,labels,by="patient_num")
    AVR <- dplyr::left_join(AVR,labels,by="patient_num")
  }

  #binarize?
  if(binarize==TRUE){
    AVR[,2:dim(AVR)[2]] <- +(AVR[,2:dim(AVR)[2]] > 0)}

  AVR <- as.data.frame(AVR)

  return(AVR)


}
