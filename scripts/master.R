#Packages
library(ffscrapr)
library(tidyverse)
library(ggplot2)

#save IDs of sleeper leagues
id_list = c("989337744398700544",
            "1049039397192130560",
            "1182727020784209920")

#make connections to API
dyno_2023 = ff_connect(platform = "sleeper", season = 2023, league_id = "989337744398700544")
dyno_2024 = ff_connect(platform = "sleeper", season = 2024, league_id = "1049039397192130560")
dyno_2025 = ff_connect(platform = "sleeper", season = 2025, league_id = "1182727020784209920")