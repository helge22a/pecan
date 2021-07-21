#EFI Output Configuration
library("ggplot2")
library("plotly")
library("gganimate")
library("thematic")
thematic_on()
source("/projectnb/dietzelab/ahelgeso/pecan/scripts/efi_data_process.R") #change to point to your pecan folder
#Load Output args
homedir <- "/projectnb/dietzelab/ahelgeso" #change to your homedir
args = list()
args$settings = file.path(homedir, "Site_XMLS/harvard.xml") #change to point to the pecan.CONFIGS.xml of the forecast results you want to convert to .csv 

site.num <- settings$run$site$id
outdir <- settings$outdir
site.name <- settings$run$site$name
wid <- settings$workflow$id

output_args = c(as.character(wid), site.num, outdir)

data = efi.data.process(output_args)

#Run SIPNET Outputs
data.final = data %>%
  mutate(date = as.Date(date)) %>%
  filter(date < end_date) %>%
  arrange(ensemble, date) %>%
  mutate(time = as.POSIXct(paste(date, Time, sep = " "), format = "%Y-%m-%d %H %M")) %>%
  mutate(siteID = site.name,
         forecast = 1,
         data_assimilation = 0,
         time = lubridate::force_tz(time, tz = "UTC"))
#re-order columns and delete unnecessary columns in data.final
datacols <- c("date", "time", "siteID", "ensemble", "nee", "le", "vswc", "forecast", "data_assimilation")
data.final = data.final[datacols]

write.csv(data.final, file = paste0(site.name, "-", start_date, "-", end_date, ".csv"))
