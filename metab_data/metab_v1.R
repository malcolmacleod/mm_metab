## A script to experiment with the LakeMetabolizer package

# clear everything
rm(list = ls(all = TRUE))

## 1. load in necessary packages
install.packages("rLakeAnalyzer")
library(LakeMetabolizer)
library(tidyverse)
library(rLakeAnalyzer)
library(readr)
library(janitor)
library(data.table)

## 3. Preparing miniDOT data

# get data folder names
foldersandfiles<-list.files() #get file/folder names
prefix<-"LakeWaco" #data folders must use this same prefix, and do not save other files (.png, .R) with this prefix
whichtouse<-grep(paste("^",prefix,sep=""),foldersandfiles, ignore.case=TRUE) #index of folders that start with the prefix
folders<-foldersandfiles[whichtouse]
print(folders)

# loop through data subfolders located in your working directory, load Cat.TXT files, do pre-processing
# creates an activityname column- a string that contains both the serial number and the date associated with the first logged sensor value 
nfolders<-length(folders)
for(i in 1:nfolders){
  folderi<-folders[i]
  pathi<-paste(folderi,"/Cat.TXT",sep="")
  datai<-fread(pathi) %>% clean_names()
  datai<-datai[-1,]
  headerinfoi<-readLines(pathi,n=6)
  seriali<-gsub(" ", "",headerinfoi[2])
  seriali<-gsub("Sensor:", "",seriali)
  datai$serial<-seriali
  firstdatei<-datai$central_standard_time[1] %>% substr(1,10)
  namei<-paste("dataloaded_",seriali,"_",firstdatei,sep="")
  namei<-gsub("-","_",namei)
  datai<-datai %>% data.frame()
  datai$temperature<-signif(as.numeric(datai$temperature),digits=4)
  datai$dissolved_oxygen<-signif(as.numeric(datai$dissolved_oxygen),digits=4)
  datai$dissolved_oxygen_saturation<-signif(as.numeric(datai$dissolved_oxygen_saturation),digits=4)
  datai$activitystartdate<-firstdatei
  datai$activityname<-namei
  assign(namei,datai)
  print(pathi)
}

# retrieve names of loaded data objects
objects<-objects()
prefix<-"dataloaded" #prefix of loaded data objects
whichdata<-grep(paste("^",prefix,sep=""),objects,ignore.case=TRUE) #index of data files that start with the prefix
dataobjects<-objects[whichdata]
print(dataobjects)

# stack all data files
nobjects<-length(dataobjects)
for(i in 1:nobjects){
  datai<-get(dataobjects[i])
  if(i==1){datastacked<-datai}
  if(i>1){datastacked<-rbind(datastacked,datai)}
}

datastacked$system<-"LakeWaco"

unique(datastacked$activityname)

# key to associated activitynames with sites and depths
sitedepth <- tribble(
  ~activityname, ~site, ~order, ~inwater_datetime, ~outwater_datetime, ~month, ~zone, ~depth,
  
  "dataloaded_7450_738779_2021_11_06", "A", "top", "2021-11-06 14:03", "2021-11-09 14:50", "november21", "river", "0.25",
  "dataloaded_7450_675903_2021_11_06", "A", "mid", "2021-11-06 14:03", "2021-11-09 14:50", "november21", "river", "2.5",
  "dataloaded_7450_839638_2021_11_06", "A", "bottom", "2021-11-06 14:03", "2021-11-09 14:50", "november21", "river", "4.5",
  
  "dataloaded_7450_131712_2021_11_06", "B", "top", "2021-11-06 13:45", "2021-11-09 15:00", "november21", "transition", "0.25",
  "dataloaded_7450_025638_2021_11_06", "B", "mid", "2021-11-06 13:45", "2021-11-09 15:00", "november21", "transition", "1",
  "dataloaded_7450_093993_2021_11_06", "B", "bottom", "2021-11-06 13:45", "2021-11-09 15:00", "november21", "transition", "1.5",
  
  "dataloaded_7450_095987_2021_11_06", "C", "top", "2021-11-06 13:05", "2021-11-09 15:48", "november21", "lake", "0.25",
  "dataloaded_7450_063974_2021_11_06", "C", "1m", "2021-11-06 13:05", "2021-11-09 15:48", "november21", "lake", "1",
  "dataloaded_7450_999280_2021_11_06", "C", "mid", "2021-11-06 13:05", "2021-11-09 15:48", "november21", "lake", "3.5",
  "dataloaded_7450_171185_2021_11_06", "C", "bottom", "2021-11-06 13:05", "2021-11-09 15:48", "november21", "lake", "6.5",
  
  "dataloaded_7450_921860_2021_11_06", "D", "top", "2021-11-06 12:22", "2021-11-09 15:30", "november21", "lake", "0.25",
  "dataloaded_7450_162132_2021_11_06", "D", "1m", "2021-11-06 12:22", "2021-11-09 15:30", "november21", "lake", "1",
  "dataloaded_7450_697789_2021_11_06", "D", "mid", "2021-11-06 12:22", "2021-11-09 15:30", "november21", "lake", "4.5",
  "dataloaded_7450_487204_2021_11_06", "D", "bottom", "2021-11-06 12:22", "2021-11-09 15:30", "november21", "lake", "8.5",
  
  "dataloaded_7450_083144_2021_11_06", "E", "top", "2021-11-06 16:23", "2021-11-09 15:48", "november21", "transition", "0.25",
  "dataloaded_7450_121548_2021_11_06", "E", "mid", "2021-11-06 16:23", "2021-11-09 15:48", "november21", "transition", "1",
  "dataloaded_7450_014636_2021_11_06", "E", "bottom", "2021-11-06 16:23", "2021-11-09 15:48", "november21", "transition", "1.5",
  
  "dataloaded_7450_667163_2021_11_06", "F", "top", "2021-11-06 16:46", "2021-11-09 16:11", "november21", "river", "0.25",
  "dataloaded_7450_720098_2021_11_06", "F", "mid", "2021-11-06 16:46", "2021-11-09 16:11", "november21", "river", "2.5",
  "dataloaded_7450_387970_2021_11_06", "F", "bottom", "2021-11-06 16:46", "2021-11-09 16:11", "november21", "river", "4.5")

sitedepth<-data.frame(sitedepth)

# merge stacked data with site/depth data
nov21_data<-merge(datastacked,sitedepth,by="activityname")
nov21_data$datetime<-as.POSIXct(monthly_data$central_standard_time)
nov21_data$inwater_datetime<-as.POSIXct(monthly_data$central_standard_time)
nov21_data$outwater_datetime<-as.POSIXct(monthly_data$central_standard_time)

dt_adj <- monthly_data$datetime


# creating in/out column
nov21_data$in_out = "out"

## defining in bucket times

obj1 = which(nov21_data$activityname == "dataloaded_7450_738779_2021_11_06" &
               (nov21_data$datetime > "2021-11-06 14:23" & 
                  nov21_data$datetime < "2021-11-09 14:50")) 

obj2 = which(nov21_data$activityname == "dataloaded_7450_675903_2021_11_06" &
               (nov21_data$datetime > "2021-11-06 14:23" & 
                  nov21_data$datetime < "2021-11-09 14:50"))

obj3 = which(nov21_data$activityname == "dataloaded_7450_839638_2021_11_06" &
               (nov21_data$datetime > "2021-11-06 14:23" &
                  nov21_data$datetime < "2021-11-09 14:50"))

obj4 = which(nov21_data$activityname == "dataloaded_7450_131712_2021_11_06" &
               (nov21_data$datetime > "2021-11-06 14:00" & 
                  nov21_data$datetime < "2021-11-09 15:00"))

obj5 = which(nov21_data$activityname == "dataloaded_7450_025638_2021_11_06" &
               (nov21_data$datetime > "2021-11-06 14:00" & 
                  nov21_data$datetime < "2021-11-09 15:00"))

obj6 = which(nov21_data$activityname == "dataloaded_7450_093993_2021_11_06" &
               (nov21_data$datetime > "2021-11-06 14:00" & 
                  nov21_data$datetime < "2021-11-09 15:00"))

obj7 = which(nov21_data$activityname == "dataloaded_7450_095987_2021_11_06" &
               (nov21_data$datetime > "2021-11-06 13:25" & 
                  nov21_data$datetime < "2021-11-09 15:48"))

obj8 = which(nov21_data$activityname == "dataloaded_7450_063974_2021_11_06" &
               (nov21_data$datetime > "2021-11-06 13:25" & 
                  monthly_data$datetime < "2021-11-09 15:48"))

obj9 = which(nov21_data$activityname == "dataloaded_7450_999280_2021_11_06" &
               (nov21_data$datetime > "2021-11-06 13:25" & 
                  nov21_data$datetime < "2021-11-09 15:48"))

obj10 = which(nov21_data$activityname == "dataloaded_7450_171185_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 13:25" & 
                   nov21_data$datetime < "2021-11-09 15:48"))

obj11 = which(nov21_data$activityname == "dataloaded_7450_921860_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 12:42" &
                   nov21_data$datetime < "2021-11-09 15:30"))

obj12 = which(nov21_data$activityname == "dataloaded_7450_162132_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 12:42" & 
                   nov21_data$datetime < "2021-11-09 15:30"))

obj13 = which(nov21_data$activityname == "dataloaded_7450_697789_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 12:42" & 
                   nov21_data$datetime < "2021-11-09 15:30"))

obj14 = which(nov21_data$activityname == "dataloaded_7450_487204_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 12:42" & 
                   nov21_data$datetime < "2021-11-09 15:30"))

obj15 = which(nov21_data$activityname == "dataloaded_7450_083144_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 16:43" & 
                   nov21_data$datetime < "2021-11-09 15:48"))

obj16 = which(nov21_data$activityname == "dataloaded_7450_121548_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 16:43" & 
                   nov21_data$datetime < "2021-11-09 15:48"))

obj17 = which(nov21_data$activityname == "dataloaded_7450_014636_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 16:43" & 
                   nov21_data$datetime < "2021-11-09 15:48"))

obj18 = which(nov21_data$activityname == "dataloaded_7450_667163_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 17:06" & 
                   nov21_data$datetime < "2021-11-09 16:11"))

obj19 = which(nov21_data$activityname == "dataloaded_7450_720098_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 17:06" & 
                   nov21_data$datetime < "2021-11-09 16:11"))

obj20 = which(nov21_data$activityname == "dataloaded_7450_387970_2021_11_06" &
                (nov21_data$datetime > "2021-11-06 17:06" & 
                   nov21_data$datetime < "2021-11-09 16:11"))

objs = c(obj1, obj2,obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, obj11, obj12, obj13, obj14, obj15, obj16, obj17, obj18,
         obj19, obj20)

nov21_data$in_out[objs] = "in"
nov21_in <- nov21_data %>% filter(in_out == "in")

# selecting relevant columns and splitting by site
ts <- nov21_in %>% select(., datetime,   site, order, depth, activityname,
                          dissolved_oxygen, dissolved_oxygen_saturation, temperature)

ts_a <- ts %>% filter(site == "A")
ts_b <- ts %>% filter(site == "B")
ts_c <- ts %>% filter(site == "C")
ts_d <- ts %>% filter(site == "D")
ts_e <- ts %>% filter(site == "E")
ts_f <- ts %>% filter(site == "F")

## 3. Preparing other necessary inputs (irr, wnd, k)

# read in irradiance/shortwave radiation and windspeed data

df_daymet <- read_csv("nov21_daymet.csv", skip = 7) %>% clean_names()
df_weather <- read_csv("nov21_weather.csv")
df_daymet_deploy <- df_daymet %>% filter(yday >309 & yday <314)

irr <- df_daymet$srad_w_m_2
wnd <- df_weather$windspeed

irr_df <- as_tibble(df_daymet$srad_w_m_2)
wind_df <- as_tibble(df_weather$windspeed)

# converting srad to par
daymet_par <- sw.to.par(df_daymet, sw.col = "srad_w_m_2", coeff = 2.114)
par <- sw.to.par.base(irr, coeff = 2.114)

# getting k using cole model
k_cole <- k.cole.base(wnd)

# k from crusius model. linear model options are power, bilinear, or constant/linear
k_crusius <- k.crusius.base(wnd, method = 'power')

# k from vachon model, lake area is in m^2
k_vachon <- k.vachon.base(wnd, lake.area = 31209356.73, params=c(2.51,1.48,0.39))

## making vectors for do.obs, do.sat, and wtr
do_obs <- nov21_in$dissolved_oxygen
do_sat <- nov21_in$dissolved_oxygen_saturation
wtr <- nov21_in$temperature

