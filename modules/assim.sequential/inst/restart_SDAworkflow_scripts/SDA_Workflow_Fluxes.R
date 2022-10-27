# ----------------------------------------------------------------------
#------------------------------------------ Load required libraries-----
# ----------------------------------------------------------------------
library("PEcAn.all")
library("PEcAn.utils")
library("PEcAn.data.remote")
library("PEcAnAssimSequential")
library("REddyProc")
library("tidyverse")
library("furrr")
library("R.utils")
library("dynutils")
library('nimble')
library("sp")
library("sf")
library("lubridate")
library("dplyr")
#plan(multisession)


# ----------------------------------------------------------------------------------------------
#------------------------------------------Prepared SDA Settings -----
# ----------------------------------------------------------------------------------------------
# tmp = commandArgs(trailingOnly = TRUE)
# if(length(tmp) < 6){
#   logger.severe("Missing required arguments")
# }
#forecastPath points to the folder where unconstrained forecast runs can be found
forecastPath <- "/projectnb/dietzelab/ahelgeso/Site_Outputs/Harvard/Test_fluxDA/"
#SDApath points to the folder where SDA forecast runs can be found
#SDApath <- "/projectnb/dietzelab/ahelgeso/SDA/HF_SDA_Output/Fixed_PAR"
#SDApath <- tmp[1]
#manually set to previous run settings$info$date it creates the filepath to previous run
#when you run with write to BETY = FALSE the system uses the system date/time as the unique folder name for runs
#next.oldir <- "2022-09-23-11-49"
#next.oldir <- tmp[2]
#outputPath points to location where you would like to save SDA output note this path could match SDApath but does not need to
outputPath <- "/projectnb/dietzelab/ahelgeso/SDA/HF_SDA_Output/TestFluxes/"
#outputPath <- tmp[3]
#settingsPath points to location where multisite xml can be found
settingsPath <- "/projectnb/dietzelab/ahelgeso/pecan/modules/assim.sequential/inst/Site_XMLS/testingMulti_HF_fluxes.xml"
#settingsPath <- tmp[4]
#to manually change start date 
runDays <- seq(as.Date("2020-10-10"), as.Date("2020-10-11"), by="days")
#runDays <- seq(as.Date(tmp[5]), as.Date(tmp[6]), by="days")

#------------------------------------------------------------------------------------------------
#------------------------------------------ Preparing the pecan xml -----------------------------
#------------------------------------------------------------------------------------------------
for (s in 1:length(runDays)) {
#restart list will store the full filepath to previous runs and when to start SDA cut 
restart <- list()
setwd(outputPath)
#set sda.start
sda.start <- as.Date(runDays[s])
#sda.start <- as.Date("2021-07-15")

#reading xml
settings <- read.settings(settingsPath)

#grab site info
site_info <- list(
  site_id = settings$run$site$id,
  site_name = settings$run$site$name,
  lat = settings$run$site$lat,
  lon = settings$run$site$lon,
  time_zone = "UTC")

#grab old.dir filepath from previous SDA run
#sda.runs <- list.files(SDApath, full.names = TRUE, pattern = paste0("PEcAn_", next.oldir))
#add filpath to restart list
#restart$filepath <- sda.runs

# #connecting to DB
# con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)
# on.exit(db.close(con))
# 
# #query database for previous forecast run (i.e. t=0)
# query.run <- paste0("SELECT * FROM runs WHERE site_id =", site_info$site_id)
# run <- PEcAn.DB::db.query(query.run, con)
# #filter for sda.start
# run <- dplyr::filter(run, start_time == as.Date(sda.start -1))
# daydiff <- difftime(Sys.time(), run$created_at, units = "days")
# runday <- which(min(daydiff) == daydiff)
# startday <- run$created_at[runday]
# run <- dplyr::filter(run, as.Date(created_at) == as.Date(startday))
# run <- dplyr::filter(run, !is.na(finished_at))
# #add filter for model
# query.ens <- paste0("SELECT * FROM ensembles WHERE id =", run$ensemble_id)
# ens <- PEcAn.DB::db.query(query.ens, con)
# #now that we have the workflow id for forecast run we can close connection to BETY
# PEcAn.DB::db.close(con)
# #add filepath to restart object, this is where SDA will look for runs for t=1
# restart$filepath <- paste0(forecastPath, "PEcAn_", ens$workflow_id, "/")
# #restart$filepath <- "/projectnb/dietzelab/ahelgeso/Site_Outputs/Harvard/FluxPaper/PEcAn_1000022323/"
# #check if all ensemble members are present
# ensPresent <- list()
# for(k in 1:length(run$ensemble_id)){
#   ensPresent[[k]] <- file.exists(paste0(restart$filepath, "out/", run$id[k], "/2021.nc"))
# }
# if(FALSE %in% ensPresent){
#   next
# }
restart$filepath <- "/projectnb/dietzelab/ahelgeso/Site_Outputs/Harvard/Test_fluxDA/PEcAn_1000022534/"
#set met.start & met.end
met.start <- sda.start - 1
met.end <- met.start + lubridate::days(35)

# --------------------------------------------------------------------------------------------------
#---------------------------------------------- Flux DATA -------------------------------------
# --------------------------------------------------------------------------------------------------
  #read in flux dataset
  fullFlux <- read.csv("/projectnb/dietzelab/ahelgeso/Oneflux/HARV/AMF_US-Ha1_FLUXNET_FULLSET_DD_2020.csv", header = TRUE)
  #set date as class POSIXt
  fullFlux$Date <- as.POSIXct(fullFlux$Date)
  
  #filter for NEE and LE with uncertainty values
  selectFlux <- select(fullFlux, Date, LE_RANDUNC, LE_CORR_JOINTUNC, LE_F_MDS, 
                       LE_F_MDS_QC, LE_CORR, NEE_VUT_USTAR50, NEE_VUT_USTAR50_QC, 
                       NEE_VUT_USTAR50_RANDUNC, NEE_VUT_USTAR50_JOINTUNC)
  
  #replace -9999 with NA
  selectFlux[selectFlux == -9999] <- NA
  
  #select for desired day of assimilation
  dayFlux <- filter(selectFlux, Date == as.Date("2020-10-10"))
  
  #filter for NEE QC value
  QC_threshold <- 0.8 #make variable
  
  if(is.na(dayFlux$LE_F_MDS_QC)){
    qcFlux <- filter(dayFlux, NEE_VUT_USTAR50_QC >= QC_threshold)
  }else{
    qcflux <- filter(dayFlux, NEE_VUT_USTAR50_QC >= QC_threshold & LE_F_MDS_QC >= QC_threshold)
  }
  
  if(TRUE %in% is.na(qcFlux[1,])){
    #run DA with obs.mean/cov with NAs
    
  }
  
  #filter for uncertainty NA, if joint is available use otherwise use random
  if(is.na(qcFlux$LE_CORR_JOINTUNC) & is.na(qcFlux$LE_CORR)){
    #use random uncertainty for LE
    LE <- data.frame(calendar_date = qcFlux$Date, site_id = site_info$site_id, LE_sd = qcFlux$LE_RANDUNC, LE = qcFlux$LE_F_MDS)
  }else{
    #use joint uncertainty for LE
    LE <- data.frame(calendar_date = qcFlux$Date, site_id = site_info$site_id, LE_sd = qcFlux$LE_CORR_JOINTUNC, LE = qcFlux$LE_CORR)
  }
  if(is.na(qcFlux$NEE_VUT_USTAR50_JOINTUNC)){
    #use random uncertainty for NEE
    NEE <- data.frame(calendar_date = qcFlux$Date, site_id = site_info$site_id, NEE_sd = qcFlux$NEE_VUT_USTAR50_RANDUNC, NEE = qcFlux$NEE_VUT_USTAR50)
  }else{
    #use joint uncertainty for NEE
    NEE <- data.frame(calendar_date = qcFlux$Date, site_id = site_info$site_id, NEE_sd = qcFlux$NEE_VUT_USTAR50_JOINTUNC, NEE = qcFlux$NEE_VUT_USTAR50)
  }
  
  #build obs.mean/cov
  #data processing info https://fluxnet.org/data/fluxnet2015-dataset/data-processing/
  #adjusting code from Prep_OBS_SDA.R
  observed_vars = c("NEE", "Qle")
  observed_data = merge(NEE, LE, by = c("site_id", "calendar_date"), all = T)
  
  #obs.mean
  #add NA obs for 1 day after flux obs available
  na.date <- as.Date(sda.start + 1)
  na.date <- as.character(na.date)
  obs.mean = data.frame(date = c(observed_data$calendar_date, na.date), site_id = c(observed_data$site_id, observed_data$site_id), 
                        NEE = c(observed_data$NEE, NA), LE = c(observed_data$LE, NA))
  obs.mean$date = as.character(obs.mean$date, stringsAsFactors = FALSE)
  
  obs.mean <- split(obs.mean, obs.mean$date)
  
  date.obs <- names(obs.mean)
  
  obs.mean <- purrr::map(
    names(obs.mean),
    function(namesl){
      split(
        obs.mean[[namesl]],
        obs.mean[[namesl]]$site_id) %>%
        purrr::map(
          ~.x[3:4] %>%
            stats::setNames(observed_vars) %>%
            `row.names<-`(NULL))
    }
  ) %>% stats::setNames(date.obs)
  
  #obs.cov
  obs.cov = data.frame(date = c(observed_data$calendar_date, na.date), site_id = c(observed_data$site_id, observed_data$site_id), 
                       NEE_sd = c(observed_data$NEE_sd, NA), LE_sd = c(observed_data$LE_sd, NA))
  obs.cov$date = as.character(obs.cov$date, stringsAsFactors = F)
  
  obs.cov <- split(obs.cov, obs.cov$date)
  
  obs.cov <- purrr::map(
    names(obs.cov),
    function(namesl){
      purrr::map(
        split(
          obs.cov[[namesl]],
          obs.cov[[namesl]]$site_id),
        ~.x[3:4]^2 %>%
          unlist %>%
          diag(nrow = 2, ncol = 2))
    }
  ) %>% stats::setNames(date.obs)
  
  rownames(obs.cov$`2020-10-10`$`646`) <- observed_vars
  colnames(obs.cov$`2020-10-10`$`646`) <- observed_vars
  
  #add start.cut to restart list
  restart$start.cut <- lubridate::as_datetime(min(observed_data$calendar_date))
  restart$start.cut <- format(restart$start.cut, "%Y-%m-%d %H:%M:%S", tz = "EST")
  

#-----------------------------------------------------------------------------------------------
#------------------------------------------ Fixing the settings --------------------------------
#-----------------------------------------------------------------------------------------------
#Using the found dates to run - this will help to download mets
settings$run$site$met.start <- as.character(met.start)
settings$run$site$met.end <- as.character(met.end)

# Setting dates in assimilation tags - This will help with preprocess split in SDA code
settings$state.data.assimilation$start.date <-as.character(sda.start)
sda.end <- max(names(obs.mean))
settings$state.data.assimilation$end.date <-as.character(sda.end)

# --------------------------------------------------------------------------------------------------
#---------------------------------------------- PEcAn Workflow -------------------------------------
# --------------------------------------------------------------------------------------------------
#info
settings$info$date <- paste0(format(Sys.time(), "%Y/%m/%d %H:%M:%S"))
next.oldir <- paste0(format(Sys.time(), "%Y-%m-%d-%H-%M"))
#Update/fix/check settings. Will only run the first time it's called, unless force=TRUE
settings <- PEcAn.settings::prepare.settings(settings, force = TRUE)
#settings$host$rundir <- settings$rundir
#settings$host$outdir <- settings$modeloutdir
#settings$host$folder <- settings$modeloutdir
setwd(settings$outdir)
#Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")
# start from scratch if no continue is passed in
statusFile <- file.path(settings$outdir, "STATUS")
if (length(which(commandArgs() == "--continue")) == 0 && file.exists(statusFile)) {
  file.remove(statusFile)
}

# #manually add in clim files 
# con <-PEcAn.DB::db.open(settings$database$bety)
# on.exit(db.close(con), add = TRUE)
# 
# input_check <- PEcAn.DB::dbfile.input.check(
#   siteid= site_info$site_id %>% as.character(),
#   startdate = met.start %>% as.Date,
#   enddate = NULL,
#   parentid = NA,
#   mimetype="text/csv",
#   formatname="Sipnet.climna",
#   con = con,
#   hostname = PEcAn.remote::fqdn(),
#   pattern = NULL, 
#   exact.dates = TRUE,
#   return.all=TRUE
# )
# 
# #If INPUTS already exists, add id and met path to settings file
# 
# if(length(input_check$id) > 0){
#   #met paths 
#   clim_check = list()
#   for(i in 1:length(input_check$file_path)){
#     
#     clim_check[[i]] <- file.path(input_check$file_path[i], input_check$file_name[i])
#   }#end i loop for creating file paths 
#   #ids
#   index_id = list()
#   index_path = list()
#   for(i in 1:length(input_check$id)){
#     index_id[[i]] = as.character(input_check$id[i])#get ids as list
#     
#   }#end i loop for making lists
#   names(index_id) = sprintf("id%s",seq(1:length(input_check$id))) #rename list
#   names(clim_check) = sprintf("path%s",seq(1:length(input_check$id)))
#   
#   settings$run$inputs$met$id = index_id
#   settings$run$inputs$met$path = clim_check
# }else{PEcAn.utils::logger.error("No met file found")}
# #settings <- PEcAn.workflow::do_conversions(settings, T, T, T)
# 
# if(is_empty(settings$run$inputs$met$path) & length(clim_check)>0){
#   settings$run$inputs$met$id = index_id
#   settings$run$inputs$met$path = clim_check
# }
#add met files
met_paths <- list.files(path = "/projectnb/dietzelab/ahelgeso/NOAA_met_data/noaa_clim/HARV/2020-10-10/", full.names = TRUE, pattern = ".clim")
met_id <- list()
for (m in 1:length(met_paths)) {
  met_id[[m]] = as.character(met_paths[m])
}
names(met_id) = sprintf("path%s",seq(1:length(met_paths))) #rename list
settings$run$inputs$met$path = met_id

#add runs ids from previous forecast to settings object to be passed to build X
run_id <- list()
for (k in 1:length(run$id)) {
  run_id[[k]] = as.character(run$id[k])
}
names(run_id) = sprintf("id%s",seq(1:length(run$id))) #rename list
settings$runs$id = run_id

# #add run ids from previous sda to settings object to be passed to build X
# run_id <- list()
# for (k in 1:length(previous.ens)) {
#   run_id[[k]] = as.character(previous.ens[k])
# }
# names(run_id) = sprintf("id%s",seq(1:length(previous.ens))) #rename list
# settings$runs$id = run_id

#save restart object
save(restart, next.oldir, obs.mean, obs.cov, file = file.path(settings$outdir, "restart.Rdata"))
#run sda function
sda.enkf.multisite(settings = settings, 
                   obs.mean = obs.mean, 
                   obs.cov = obs.cov, 
                   Q = NULL, 
                   restart = restart, 
                   forceRun = TRUE, 
                   keepNC = TRUE, 
                   control = list(trace = TRUE,
                                  FF = FALSE,
                                  interactivePlot = FALSE,
                                  TimeseriesPlot = FALSE,
                                  BiasPlot = FALSE,
                                  plot.title = NULL,
                                  facet.plots = FALSE,
                                  debug = FALSE,
                                  pause = FALSE,
                                  Profiling = FALSE,
                                  OutlierDetection=FALSE))





}
