#' datebuff
#'
#' @param uniqpats a vector of unique patients numbers
#' @param eve the column in the dems table indicating the date for event of interest
#' @param numdays number of buffer days -- defaults at 0
#' #' @param use use data from before or after the buffer days
#'
#'
#' @return the updated data mart
#'
#' @examples see vignettes
datebuff <- function(uniqpats,
                     eve,
                     numdays=0,
                     use)
{
  after <- list() #after the event
  before <- list() #before the event

  for (i in 1: length(uniqpats)){
    tryCatch({
      print(paste0("patient ", i, " of ", length(uniqpats)))
      dbmart.i <- subset(dbmart,dbmart$patient_num == uniqpats[i])
      dbmart.i$start_date <- as.POSIXct(dbmart.i$start_date, "%Y-%m-%d")

      evedate <- subset(dems,dems$patient_num == uniqpats[i] )[which( colnames(dat.VAL)==eve )]
      evedate <- as.POSIXct(evedate, "%Y-%m-%d")
      evedate <- evedate - as.difftime(numdays, unit="days")

      if(use =="after"){
      dbmart.i.after <- subset(dbmart.i, dbmart.i$start_date >= evedate)

      days_old <- data.frame(floor(as.numeric(difftime(Sys.Date() ,evedate , units = c("days")))))
      colnames(days_old) <- "days_old"
      days_old$patient_num <- uniqpats[i]

      after[[i]] <- data.frame(dbmart.i.after,stringsAsFactors=FALSE)

      rm(dbmart.i.after,days_old,dbmart.i)}
    },
    error = function(ohshoot) {cat("ERROR :",conditionMessage(ohshoot), "\n")})
  }
  if(use =="after"){dbmart <-data.table::rbindlist(after)}

  return(dbmart)
}
