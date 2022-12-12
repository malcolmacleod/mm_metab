## A script to experiment with the LakeMetabolizer package

# clear everything
rm(list = ls(all = TRUE))

# necessary packages
install.packages("rLakeAnalyzer")
library(LakeMetabolizer)
library(tidyverse)
library(rLakeAnalyzer)
library(readr)
library(janitor)

df_daymet <- read_csv("nov21_daymet.csv", skip = 7) %>% clean_names()
df_weather <- read_csv("nov21_weather.csv")

irr <- as_tibble(df_daymet$srad_w_m_2)
wind_hr <- as_tibble(df_weather$windspeed)

# converting srad to par
daymet_par <- sw.to.par(df_daymet, sw.col = "srad_w_m_2", coeff = 2.114)
