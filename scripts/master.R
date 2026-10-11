#Packages
library(ffscrapr)
library(tidyverse)
library(ggplot2)
library(googlesheets4)

#save IDs of sleeper leagues
id_list = c("1364697247219986432")

#make connections to API
dyno_2026 = ff_connect(platform = "sleeper", season = 2026, league_id = "1364697247219986432")