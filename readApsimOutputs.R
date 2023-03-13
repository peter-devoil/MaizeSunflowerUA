rm(list=ls())
setwd("~/work/People/Oleksii Kryvobok 2023/")

library(ggplot2)
library(dplyr)

obs <- read.csv("Baseline_with_SM.csv")

# read apsim output files
read.apsim <- function(apsim.name) {
  header<-readLines(apsim.name, n=25)
  if (length(header) == 0 ) {return(NULL)}
  
  i <- which(grepl("^Title.*$", header))
  apsim <- read.table(apsim.name,skip=i+2,na.strings=c("NA","?"))
  if (nrow(apsim) == 0) {return(NULL)}
  names(apsim) <- unlist(strsplit(trimws(header[i+1]), " +"))
  
  # decode factor levels in header
  i <- which(grepl("^factors.*$", header))
  for (nv in unlist(strsplit(substring(header[i],11),";"))) {
    apsim[[ unlist(strsplit(nv, "="))[1] ]] <- unlist(strsplit(nv, "="))[2]
  }
  
  return(apsim)
}

apsim<-do.call(rbind, lapply(list.files(path = ".", pattern = "^Simulation_.*.out$", full.names=T) , read.apsim)) %>%
  mutate(Year = as.integer(Year))
##key = paste(Station,Year,Crop, sep="_")

all <- obs %>% 
  mutate(Crop = tools::toTitleCase(crop)) %>%
  rename (Station = station, Year = year) %>%
  #mutate(key = paste(Station,Year,Crop, sep="_")) %>%
  merge(apsim, by=c("Station", "Year", "Crop")) %>%
  mutate(sowingDate=as.Date(datsow, format="%Y-%m-%d"),
         flDAS.pred = ifelse(Crop=="Sunflower", Sunflower_FLDAS, Maize_FLDAS),
         flDAS.obs = as.Date(antheshis, format="%Y-%m-%d") - sowingDate,
         matDAS.pred = ifelse(Crop=="Sunflower", Sunflower_MATDAS, Maize_MATDAS),
         matDAS.obs = as.Date(maturity, format="%Y-%m-%d") - sowingDate)

ggplot(all) + 
  geom_point(aes(x=flDAS.pred, y = flDAS.obs)) + 
  scale_x_continuous(limits=range(c(all$flDAS.pred, all$flDAS.obs))) +
  scale_y_continuous(limits=range(c(all$flDAS.pred, all$flDAS.obs))) +
  geom_abline(slope=1) +
  labs(x="Predicted", y="Observed", title="Flowering DAS") +
  facet_wrap(~Crop) +
  theme_minimal()

ggplot(all) + 
  geom_point(aes(x=matDAS.pred, y = matDAS.obs)) + 
  scale_x_continuous(limits=range(c(all$matDAS.pred, all$matDAS.obs))) +
  scale_y_continuous(limits=range(c(all$matDAS.pred, all$matDAS.obs))) +
  geom_abline(slope=1) +
  labs(x="Predicted", y="Observed", title="Maturity DAS") +
  facet_wrap(~Crop) +
  theme_minimal()
