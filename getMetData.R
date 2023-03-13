rm(list=ls())
setwd("~/work/People/Oleksii Kryvobok 2023/")

obs <- read.csv("Baseline_with_SM.csv")

getIt<- function(args) {
  args <- strsplit(args, "_")[[1]] 
  lat<- args[1]
  long <- args[2]
  
  # NASA POWER, Chirps rainfall 
  # https://github.com/APSIMInitiative/ApsimX/blob/master/ApsimNG/Utility/WeatherDownloadDialog.cs
  url <- paste0("https://worldmodel.csiro.au/gclimate?lat=", lat,
                    "&lon=", long,
                    "&format=apsim&start=19850101&stop=20230101")
  download.file(url, dest = paste0(paste(lat, long,sep="_"), ".met"))
  
  
  #ISRIC soils
  # https://github.com/APSIMInitiative/ApsimX/blob/master/ApsimNG/Presenters/SoilDownloadPresenter.cs  
  url <- paste0("https://worldmodel.csiro.au/apsimsoil?lat=", lat,  "&lon=", long)
  download.file(url, dest = paste0(paste(lat, long,sep="_"), ".soil"))
  
}

for (i in unique(paste(obs$Lat,obs$Long,sep="_"))) {
  if (!file.exists(paste0(i,".met"))) {
    getIt(i)
  }
}

# Stations:
print(unique(obs[,c("station", "Lat", "Long")]))

