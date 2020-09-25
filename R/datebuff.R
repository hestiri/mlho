#' datebuff
#'
#' @param patients a vector of unique patients numbers
#' @param data dbmart
#' @param demographics demographics table
#' @param eve the column in the dems table indicating the date for event of interest
#' @param numdays number of buffer days -- defaults at 0
#' #' @param use use data from before or after the buffer days
#'
#'
#' @return the updated data mart
#'
#' @examples see vignettes
datebuff <- function(data,
                     demographics,
                     patients,
                     eve,
                     numdays=0,
                     use)
{
  after <- list() #after the event
  before <- list() #before the event

  for (i in 1: length(patients)){
    tryCatch({
      print(paste0("patient ", i, " of ", length(patients)))
      dbmart <- data
      dems <- demographics
      dbmart.i <- subset(dbmart,dbmart$patient_num == patients[i])
      dbmart.i$start_date <- as.POSIXct(dbmart.i$start_date, "%Y-%m-%d")

      evedate <- dems[dems$patient_num == patients[i],which( colnames(dems)==eve )]
      evedate <- as.POSIXct(evedate, "%Y-%m-%d")
      evedate <- evedate - as.difftime(numdays, unit="days")

      if(use =="after"){
      dbmart.i.after <- subset(dbmart.i, dbmart.i$start_date >= evedate)

      days_old <- data.frame(floor(as.numeric(difftime(Sys.Date() ,evedate , units = c("days")))))
      colnames(days_old) <- "days_old"
      days_old$patient_num <- patients[i]

      after[[i]] <- data.frame(dbmart.i.after,stringsAsFactors=FALSE)

      rm(dbmart.i.after,days_old,dbmart.i)}
    },
    error = function(ohshoot) {cat("ERROR :",conditionMessage(ohshoot), "\n")})
  }
  if(use =="after"){
    dbmart <-data.table::rbindlist(after)
    dbmart <- dplyr::select(subset(dbmart,dbmart$patient_num %in% patients),patient_num,phenx=concept_cd,start_date)#concept will be needed for sequencing
    dbmart$start_date <- as.POSIXct(dbmart$start_date, "%Y-%m-%d")
  }


  return(dbmart)
}
