#' 5 PASC cases
#'
#' @param PatientObservations 2.1 PatientObservations table should include 4-digit ICD codes
#' @param PatientClinicalCourse 2.1 PatientClinicalCourse table
#' @param PatientSummary 2.1 PatientSummary table
#' @param loyaltydays1 work as the minimum days before and after hospitalization to be assumed loyal
#'
#' @return plots for cases 1-4 and data for cases 4 and 5


pasccases <- function(PatientObservations,
                      PatientClinicalCourse,
                      PatientSummary,
                      loyaltydays1 = 90




)

{
  #beginning with cases
  #load the COVID-positive data

  LocalPatientObservationsAllDigits <- PatientObservations
  LocalPatientClinicalCourse <- PatientClinicalCourse
  LocalPatientSummary <- PatientSummary
  dys1 <- loyaltydays1
  ###

  LocalPatientObservationsAllDigits$patient_num <- as.character(LocalPatientObservationsAllDigits$patient_num)
  LocalPatientClinicalCourse$patient_num <- as.character(LocalPatientClinicalCourse$patient_num)
  LocalPatientSummary$patient_num <- as.character(LocalPatientSummary$patient_num)
  LocalPatientClinicalCourse$calendar_date <- as.POSIXct(LocalPatientClinicalCourse$calendar_date , "%Y-%m-%d")


  ### let's do a simple proxy for loyalty
  ### computing how far back and forth the data is available for each patient (benchmark is the hospitalization date)
  patients_data <- LocalPatientObservationsAllDigits %>%
    dplyr::group_by(patient_num) %>%
    dplyr::summarise(days_max=max(days_since_admission),days_min=min(days_since_admission)) %>%
    filter(days_max >= dys1 & days_min <= -(dys1)) ###limiting before and after to at least 3 months!


  ##compute the first admission and discharge dates
  first_admin <- LocalPatientClinicalCourse %>%
    dplyr::group_by(patient_num) %>%
    filter(in_hospital == 1 & patient_num %in% patients_data$patient_num) %>%
    dplyr::summarise(calendar_date=min(calendar_date))

  first_discharge <- LocalPatientClinicalCourse %>%
    dplyr::group_by(patient_num) %>%
    filter(in_hospital == 0 & patient_num %in% patients_data$patient_num) %>%
    dplyr::summarise(calendar_date=min(calendar_date)- as.difftime(1, unit="days"))


  first_admin <- merge(LocalPatientClinicalCourse[,c("patient_num","calendar_date","days_since_admission")],first_admin,by=c("patient_num","calendar_date"))
  names(first_admin)[2] <- "first_admission_date"
  first_discharge <- merge(LocalPatientClinicalCourse[,c("patient_num","calendar_date","days_since_admission")],first_discharge,by=c("patient_num","calendar_date"))
  names(first_discharge)[2]  <- "first_discharge_date"

  ###creating a new table for patients to track first hospital admission/discharge
  PatientHospitalCourse <- merge(first_discharge,first_admin,by="patient_num")
  rm(first_discharge,first_admin)
  ##Adding hospitalization by quarters
  PatientHospitalCourse$first_admission_qrt <- lubridate::quarter(PatientHospitalCourse$first_admission_date, with_year = T)

  LocalPatientObservationsAllDigits <- subset(LocalPatientObservationsAllDigits,LocalPatientObservationsAllDigits$patient_num %in% PatientHospitalCourse$patient_num)

  print("mapping to phecodes")


  ##we actually only need 4 digit ICD10s
  LocalPatientObservationsAllDigits$concept_code <- paste0("ICD10:",substr(LocalPatientObservationsAllDigits$concept_code,1,5))

  ##merge phecodes
  LocalPatientObservationsAllDigits <- merge(LocalPatientObservationsAllDigits,phe_map_unique,by="concept_code",by.y = "ICD",all.x = T)
  ###using the most granular level
  LocalPatientObservationsAllDigits$feature_desc <- LocalPatientObservationsAllDigits$feature_desc_lvl6

  ##keeping codes that mapped!
  LocalPatientObservationsAllDigits <- subset(LocalPatientObservationsAllDigits,
                                              !is.na(LocalPatientObservationsAllDigits$feature_desc))
  print("temporal segmentation")
  ##now, bucket up!
  LocalPatientObservationsAllDigits <- merge(LocalPatientObservationsAllDigits,PatientHospitalCourse,by="patient_num")
  LocalPatientObservationsAllDigits$era <- "pre_covid" ## means pre-hospitalization in non-covid patients
  LocalPatientObservationsAllDigits$era <- ifelse(LocalPatientObservationsAllDigits$days_since_admission == 0,
                                                  "admitted",LocalPatientObservationsAllDigits$era)
  LocalPatientObservationsAllDigits$era <- ifelse(LocalPatientObservationsAllDigits$days_since_admission < LocalPatientObservationsAllDigits$days_since_admission.x &
                                                    LocalPatientObservationsAllDigits$days_since_admission > 0,
                                                  "hospitalized",LocalPatientObservationsAllDigits$era)
  LocalPatientObservationsAllDigits$era <- ifelse(LocalPatientObservationsAllDigits$days_since_admission == LocalPatientObservationsAllDigits$days_since_admission.x,
                                                  "discharged",LocalPatientObservationsAllDigits$era)
  LocalPatientObservationsAllDigits$era <- ifelse(LocalPatientObservationsAllDigits$days_since_admission > LocalPatientObservationsAllDigits$days_since_admission.x,
                                                  "post_covid",LocalPatientObservationsAllDigits$era)

  ##compute days since discharge for post covid records
  # LocalPatientObservationsAllDigits$days_since_first_discharge <- LocalPatientObservationsAllDigits$days_since_admission - LocalPatientObservationsAllDigits$days_since_admission.x


  ##now we do this, we assign anything negative to -1, just to identify it as past.
  # LocalPatientObservationsAllDigits$days_since_admission_sim <- ifelse(LocalPatientObservationsAllDigits$days_since_admission<0,-1,LocalPatientObservationsAllDigits$days_since_admission)
  #or we don't because we may need the days for case 4.1
  LocalPatientObservationsAllDigits$days_since_admission_sim <- LocalPatientObservationsAllDigits$days_since_admission
  ########################################################################
  ####################################      ####################################
  # LocalPatientObservationsAllDigits$month <- ifelse(LocalPatientObservationsAllDigits$days_since_admission_sim >= 0, ceiling(LocalPatientObservationsAllDigits$days_since_admission_sim/30),-1)
  # LocalPatientObservationsAllDigits$week <- ifelse(LocalPatientObservationsAllDigits$days_since_admission_sim >= 0, ceiling(LocalPatientObservationsAllDigits$days_since_admission_sim/7),-1)

  LocalPatientObservationsAllDigits$month <- ifelse(LocalPatientObservationsAllDigits$days_since_admission_sim >= 0, ceiling(LocalPatientObservationsAllDigits$days_since_admission_sim/30),
                                                    floor(LocalPatientObservationsAllDigits$days_since_admission_sim/30))
  LocalPatientObservationsAllDigits$week <- ifelse(LocalPatientObservationsAllDigits$days_since_admission_sim >= 0, ceiling(LocalPatientObservationsAllDigits$days_since_admission_sim/7),
                                                   floor(LocalPatientObservationsAllDigits$days_since_admission_sim/7))

  print("computing case 1")
  ####################################   case 1   ####################################
  ###what are the top records
  records <- data.frame(LocalPatientObservationsAllDigits %>%
                          filter(era %in% c("hospitalized","post_covid")) %>% ##in case 1, for the bubble charts, we compared during hospital to post-discharge for ranking
                          dplyr::group_by(feature_desc,era) %>%
                          dplyr::summarise(count=length(patient_num)) %>%
                          pivot_wider(names_from = era, values_from = count) %>%
                          mutate(percent_change = ((post_covid-hospitalized)/hospitalized)*100) )

  ##selecting the top 25 by percentage to plot
  top_records <- records %>% arrange(desc(percent_change)) %>% slice(1:25) %>% dplyr::select(feature_desc)
  top_records <- merge(top_records,records,by="feature_desc")
  case1_plot <- merge(LocalPatientObservationsAllDigits,top_records,by="feature_desc")

  (pl1 <- case1_plot %>%
      dplyr::group_by(feature_desc,month) %>%
      dplyr::summarise(count=length(patient_num)) %>%
      full_join(top_records, by = "feature_desc") %>%
      ggplot(aes(x=factor(month), y=reorder(feature_desc,percent_change), size=count, fill=feature_desc)) +
      geom_point(alpha=0.5, shape=21, color="black") +
      # facet_wrap(~concept,ncol=2,scales = "free")+
      # scale_size(range = c(.1, 24), name="Population (M)") +
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
      theme_ipsum() +
      theme(legend.position="bottom") +
      # ylab("Life Expectancy") +
      xlab("Months since hospitalization") +
      labs(size="cohort size",title = "Case #1:",subtitle = "counting all codes",y="",caption="* -1 sums all months prior to hospitalization"))
  # ggsave(filename="~/workspace/PASC/4CE_Case1.png", pl1, dpi = 300, width = 10, height = 7)

  print("computing case 2")

  ########################################################################
  ####################################      ####################################
  ###let's do the 2nd case if a diagnosis was recorded before discharge, it wont be counted
  case <- LocalPatientObservationsAllDigits
  setDT(case)
  uniqpats <- c(as.character(unique(case$patient_num)))
  records_cases <- case %>%
    dplyr::group_by(feature_desc,era,patient_num) %>%
    dplyr::summarise(count=length(feature_desc)) %>%
    pivot_wider(names_from = era, values_from = count)

  records_cases[is.na(records_cases)] <- 0
  # now we want to create a vector for each patients that contains case 2 only records: predischarge observation of a record eliminates the record
  records_case2 <- subset(records_cases,records_cases$pre_covid+records_cases$hospitalized+records_cases$admitted == 0)
  records_case2 <- records_case2[!is.na(records_case2$feature_desc),]
  records_case2$key <- paste0(records_case2$feature_desc,records_case2$patient_num)


  case2 <- subset(case,case$era %in% c("post_covid","discharged"))
  case2$key <- paste0(case2$feature_desc,case2$patient_num)

  case20 <- subset(case2,case2$key %in% records_case2$key)
  case2$key <- NULL
  gc()

  ###what are the top records
  records <- data.frame(case2 %>%
                          filter(era %in% c("discharged","post_covid")) %>%##comparing discharge date with post-discharge
                          dplyr::group_by(feature_desc,era) %>%
                          dplyr::summarise(count=length(patient_num)) %>%
                          pivot_wider(names_from = era, values_from = count) %>%
                          # mutate(post_covid = replace_na(post_covid, 0),discharged = replace_na(discharged, 1)) %>%
                          mutate(percent_change = ((post_covid-discharged)/discharged)*100))

  top_records <- records %>% arrange(desc(percent_change)) %>% slice(1:25) %>% dplyr::select(feature_desc)
  top_records <- merge(top_records,records,by="feature_desc")
  case2_plot <- merge(case2,top_records,by="feature_desc")

  (pl2 <- case2_plot %>%
      dplyr::group_by(feature_desc,month) %>%
      dplyr::summarise(count=length(patient_num)) %>%
      full_join(top_records, by = "feature_desc") %>%
      ggplot(aes(x=factor(month), y=reorder(feature_desc,percent_change), size=count, fill=feature_desc)) +
      geom_point(alpha=0.5, shape=21, color="black") +
      # facet_wrap(~concept,ncol=2,scales = "free")+
      # scale_size(range = c(.1, 24), name="Population (M)") +
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
      theme_ipsum() +
      theme(legend.position="bottom") +
      # ylab("Life Expectancy") +
      xlab("Months since hospitalization") +
      labs(size="cohort size",title = "Case #2:",subtitle = "counting all new codes on adischarge and after",y=""))
  # ggsave(filename="~/workspace/PASC/4CE_Case2.png", pl2, dpi = 300, width = 10, height = 7)


  ########################################################################
  ####################################      ####################################
  ###let's do the 3rd case if a diagnosis was recorded on and before discharge, it wont be counted
  print("computing case 3")

  ########################################################################
  ####################################      ####################################
  ###creating case 3

  # now we want to create a vector for each patients that contains case 3 only records: predischarge observation of a record eliminates the record
  records_case3 <- subset(records_cases,records_cases$pre_covid+records_cases$hospitalized+records_cases$admitted+records_cases$discharged == 0)
  records_case3 <- records_case3[!is.na(records_case3$feature_desc),]
  records_case3$key <- paste0(records_case3$feature_desc,records_case3$patient_num)


  case3 <- subset(case,case$era == "post_covid")
  case3$key <- paste0(case3$feature_desc,case3$patient_num)

  case3 <- subset(case3,case3$key %in% records_case3$key)
  case3$key <- NULL
  gc()



  ###what are the top records
  records <- data.frame(case3 %>%
                          filter(month %in% c(2,6)) %>% ##comparing month 2 and 6
                          dplyr::group_by(feature_desc,month) %>%
                          dplyr::summarise(count=length(patient_num)) %>%
                          pivot_wider(names_from = month, values_from = count) %>%
                          mutate(percent_change = ((`6`-`2`)/`2`)*100) )

  top_records <- records %>% arrange(desc(percent_change)) %>% slice(1:25) %>% dplyr::select(feature_desc)
  top_records <- merge(top_records,records,by="feature_desc")
  case3_plot <- merge(case3,top_records,by="feature_desc")

  (pl3 <- case3_plot %>%
      dplyr::group_by(feature_desc,month) %>%
      dplyr::summarise(count=length(patient_num)) %>%
      full_join(top_records, by = "feature_desc") %>%
      ggplot(aes(x=factor(month), y=reorder(feature_desc,percent_change), size=count, fill=feature_desc)) +
      geom_point(alpha=0.5, shape=21, color="black") +
      # facet_wrap(~concept,ncol=2,scales = "free")+
      # scale_size(range = c(.1, 24), name="Population (M)") +
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
      theme_ipsum() +
      theme(legend.position="bottom") +
      # ylab("Life Expectancy") +
      xlab("Months since hospitalization") +
      labs(size="cohort size",title = "Case #3:",subtitle = "counting all new codes after discharge",y=""))
  # ggsave(filename="~/workspace/PASC/4CE_Case3.png", pl3, dpi = 300, width = 10, height = 7)


  print("computing case 4: parallel from now on")

  ##########################################################################################
  ##############################      ########################################     #####
  ############### ##########     #########################
  #####from here on, we will only consider the first observation of each record

  ###setup parallel backend
  cores<-detectCores()
  cl <- makeCluster(cores[1]-2)  ##leaves only 2 cores for your other processes so be careful if running on laptop!
  registerDoParallel(cl)

  uniqpats <- c(as.character(unique(case$patient_num)))

  case_0 <- foreach(p = 1: length(uniqpats),
                    .combine = "rbind",
                    .packages = c("plyr")) %dopar% {
                      tryCatch({
                        pat.dat <- subset(case,case$patient_num == uniqpats[p])
                        #store the first observation for each record
                        first.obser.day <- plyr::ddply(pat.dat,~feature_desc,summarise,days_since_admission=min(days_since_admission))
                        pat.dat$unique.key <- paste0(pat.dat$feature_desc,pat.dat$days_since_admission)
                        first.obser.day$unique.key <- paste0(first.obser.day$feature_desc,first.obser.day$days_since_admission)
                        #only grab data from the first observations
                        pat.dat <- subset(pat.dat, pat.dat$unique.key %in% first.obser.day$unique.key)
                        #remove duplicates
                        pat.dat <- pat.dat[!duplicated(pat.dat$unique.key), ]
                        pat.dat$unique.key <- NULL

                        pat.dat
                      },
                      error = function(fr) {cat("ERROR :",conditionMessage(fr), "\n")})
                    }
  gc()




  ########################################################################
  ####################################      ####################################
  ###let's do the 4th case if a diagnosis was recorded on and before discharge and repeated more than once, it wont be counted

  case4 <- subset(case_0,case_0$era == "post_covid")


  ###what are the top records
  records <- data.frame(case4 %>%
                          filter(month %in% c(2,6)) %>%
                          dplyr::group_by(feature_desc,month) %>%
                          dplyr::summarise(count=length(patient_num)) %>%
                          pivot_wider(names_from = month, values_from = count) %>%
                          mutate(percent_change = ((`6`-`2`)/`2`)*100) )


  top_records <- records %>% arrange(desc(percent_change)) %>% slice(1:30) %>% dplyr::select(feature_desc)
  top_records <- merge(top_records,records,by="feature_desc")
  case4_plot <- merge(case4,top_records,by="feature_desc")

  (pl4 <- case4_plot %>%
      dplyr::group_by(feature_desc,month) %>%
      dplyr::summarise(count=length(patient_num)) %>%
      full_join(top_records, by = "feature_desc") %>%
      ggplot(aes(x=factor(month), y=reorder(feature_desc,percent_change), size=count, fill=feature_desc)) +
      geom_point(alpha=0.5, shape=21, color="black") +
      # facet_wrap(~concept,ncol=2,scales = "free")+
      # scale_size(range = c(.1, 24), name="Population (M)") +
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
      theme_ipsum() +
      theme(legend.position="bottom") +
      # ylab("Life Expectancy") +
      xlab("Months since hospitalization") +
      labs(size="cohort size",title = "Case #4:",subtitle = "counting first records of new codes after discharge",y=""))
  # ggsave(filename="~/workspace/PASC/4CE_Case4.png", pl4, dpi = 300, width = 10, height = 7)

  print("computing case 5, pre-admission")

  ####
  ########################################################################
  ####################################   case 5   ####################################
  ###let's now do the case4/ compared to the past as controls

  ########################################################################
  ################### case 5 pre compares preadmission to case 4
  case5_pre <- subset(case_0,case_0$era == "pre_covid")

  pre_pats <- length(unique(case5_pre$patient_num))
  case5_pre <- rbind(case4,case5_pre)
  post_pats <- length(unique(case4$patient_num))
  #### we compute relative risk for each diseases
  #################### normalizing by pre-covid era patients
  ###what are the top records
  case5_pre <- data.frame(case5_pre %>%
                            # filter(era %in% c("post_covid","pre_covid")) %>%
                            dplyr::group_by(feature_desc,era) %>%
                            dplyr::summarise(counts=length(patient_num))%>%
                            spread(era,counts)%>%
                            ungroup())

  case5_pre[is.na(case5_pre)] <- 0

  case5_pre$risk_post <- (case5_pre$post_covid/post_pats)*100
  case5_pre$risk_pre <- (case5_pre$pre_covid/pre_pats)*100

  case5_pre$RR <- case5_pre$risk_post/case5_pre$risk_pre ##relative risk that ignores NAs

  print("computing case 5, pre- & during admission")

  ##################       ##################
  ################## # case 5 preduri compares preadmission to case 4
  case5_preduri <- subset(case_0,case_0$era != "post_covid")
  case5_preduri$era <- "preduring"
  preduri_pats <- length(unique(case5_preduri$patient_num))
  case5_preduri <- rbind(case4,case5_preduri)

  #### we compute relative risk for each diseases
  #################### normalizing by the pre and during admission patients
  ###what are the top records
  case5_preduri <- data.frame(case5_preduri %>%
                                # filter(era %in% c("post_covid","pre_covid")) %>%
                                dplyr::group_by(feature_desc,era) %>%
                                dplyr::summarise(counts=length(patient_num))%>%
                                spread(era,counts)%>%
                                ungroup())

  case5_preduri[is.na(case5_preduri)] <- 0

  case5_preduri$risk_post <- (case5_preduri$post_covid/post_pats)*100
  case5_preduri$risk_preduri <- (case5_preduri$preduring/preduri_pats)*100

  case5_preduri$RR <- case5_preduri$risk_post/case5_preduri$risk_preduri ##relative risk that ignores NAs

  return(list(
    case1plot=pl1,
    case2plot=pl2,
    case3plot=pl3,
    case4plot= pl4,
    case4=case4,
    case5_pre=case5_pre,
    case5_preduri=case5_preduri)

  )

}
