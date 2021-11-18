#' MLHO's confidence scores
#'
#' @param mlho.features add your mlho.features
#'
#' @return
#' @export
#'
mlho.cs <- function(mlho.features
                  )
{

max.it <- as.numeric(max(mlho.features$iteration))
cfs <- c(unique(mlho.features$features))
require(Rmisc)
scores <- list()
for (i in 1:length(cfs)){
  dat <- subset(mlho.features,mlho.features$features == cfs[i])
  out <- data.frame(cfs[i])
  colnames(out) <- "features"
  out$mean <- exp(CI(log(dat$OR), ci=0.95))[2]
  out$up99 <- exp(CI(log(dat$OR), ci=0.99))[1]
  out$low99 <- exp(CI(log(dat$OR), ci=0.99))[3]
  out$up95 <- exp(CI(log(dat$OR), ci=0.95))[1]
  out$low95 <- exp(CI(log(dat$OR), ci=0.95))[3]
  out$seen_in <- nrow(dat)
  out$seen_negative <- nrow(dat[dat$OR < 1,])
  out$seen_positive <- nrow(dat[dat$OR > 1,])
  out$direction <- ifelse(out$mean>1,"positive","-")
  out$direction <- ifelse(out$mean<1,"negative",out$direction)

  # out$seen_in_positive <- out$seen_in - (out$negative)*2
  # out$seen_in_negative <- out$seen_in - (out$negative)*2

  scores[[i]] <- out
  rm(out,dat)
}
scores <- do.call(rbind, lapply(scores, data.frame, stringsAsFactors=FALSE))
scores$seen_adjust <- ifelse(scores$direction == "positive",scores$seen_positive-(scores$seen_negative)*2,NA)
scores$seen_adjust <- ifelse(scores$direction == "negative",scores$seen_negative-(scores$seen_positive)*2,scores$seen_adjust)
scores$cs <- round((scores$seen_adjust/max.it)*100,0)

  return(scores)
}
