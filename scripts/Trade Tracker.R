source("scripts/master.R")

#read in Maps
Owner_map = read.csv("data/DynastyOwnerMapping.csv")
Pick_map = read.csv("data/PickMap.csv")
Pick_OrigOwner = read.csv("data/PickOriginalOwners.csv")
Schedule_map = read.csv("data/NFL Schedule.csv") %>% 
  mutate(Timestamp = as.POSIXct(Timestamp, format = "%m/%d/%Y %H:%M:%S"))

#Bring in Transactions data
Trans_23 = ff_transactions(dyno_2023)
Trans_24 = ff_transactions(dyno_2024) %>% 
  select(-waiver_priority)
Trans_25 = ff_transactions(dyno_2025)%>% 
  select(-waiver_priority) 
Trans_26 = ff_transactions(dyno_2026)%>% 
  select(-waiver_priority)

#load in trades, and filter down to picks that were traded
trades = Trans_23 %>% 
  rbind(Trans_24) %>% 
  rbind(Trans_25) %>%
  rbind(Trans_26) %>%
  filter(grepl("trade",type),
         grepl("round",player_id),
         type_desc == "traded_for") %>% 
  #separate out components of picks
  mutate(franchise_id = as.integer(franchise_id),
         pick_year = as.integer(substr(player_id, start = 1,stop = 4)),
         pick_round = sub(".*round_", "", player_id),
         pick_round = sub("_.*", "", pick_round),
         pick_originalowner = substr(player_id, start = nchar(player_id)-1,stop = nchar(player_id)),
         pick_originalowner = as.integer(gsub("_","",pick_originalowner))) %>% 
  select(timestamp,type,type_desc,franchise_id,trade_partner,pick_year,pick_round,pick_originalowner) %>% 
  #find where pick ended up
  left_join(Owner_map %>% 
              rename(pick_originalowner = franchise_id, Owner=owner)) %>% 
  mutate(Round = ifelse(pick_round == 1, 1,2),
         Round = ifelse(pick_year == 2023, 1,Round)) %>% 
  left_join(Pick_OrigOwner %>% 
              rename(pick_year = Year)) %>% 
  rename(pick_original_owner = Owner) %>% 
  mutate(player_name = "Pick") %>% 
  select(-pick_originalowner,-Round) %>% 
  #add in players that were traded
  bind_rows(Trans_23 %>% 
              rbind(Trans_24) %>% 
              rbind(Trans_25) %>%
              rbind(Trans_26) %>% 
              filter(grepl("trade",type),
                     !grepl("round",player_id),
                     !grepl("bbid",player_id),
                     type_desc == "traded_for") %>% 
              select(-player_id,-franchise_name,-team,-bbid_amount,-comment) %>% 
              mutate(franchise_id = as.integer(franchise_id))) %>% 
  #add in faab that was traded
  bind_rows(Trans_23 %>% 
          rbind(Trans_24) %>% 
          rbind(Trans_25) %>%
          rbind(Trans_26) %>% 
          filter(grepl("bbid",player_id),
                 type_desc == "traded_for") %>% 
          mutate(player_name = gsub("bbid_","FAAB ",player_id),
                 franchise_id = as.integer(franchise_id)) %>% 
          select(-franchise_name,-player_id,-bbid_amount,-comment)) %>% 
  left_join(Owner_map) %>% 
  #create a trade ID
  select(timestamp,Owner = owner,player_name,position = pos, 
         pick_year,pick_round,pick = Pick,pick_original_owner) %>% 
  mutate(pick_round = as.integer(pick_round)) %>% 
  #add in data on what players were drafted with traded picks
  left_join(ff_draft(dyno_2023) %>% 
              rbind(ff_draft(dyno_2024)) %>% 
              rbind(ff_draft(dyno_2025)) %>% 
              rbind(ff_draft(dyno_2026)) %>% 
              select(pick_year = season,pick_round = round,pick,drafted_player = player_name) %>% 
              mutate(pick_year = as.integer(pick_year))) %>% 
  arrange(Owner) 

trades = trades %>% 
  left_join(data.frame(timestamp = unique(trades$timestamp)) %>% 
              arrange(timestamp) %>% 
              mutate(Trade_ID = 1:n())) %>% 
  arrange(Trade_ID) %>% 
  mutate(pick_date = as.POSIXct(paste0(pick_year,"/01/01 01:01:01"), format = "%Y/%d/%m %H:%M:%S"),
         PickKnown = ifelse(pick_date>timestamp,"No","Yes")) %>% 
  select(-pick_date)

Scoring = ff_scoringhistory(dyno_2025, season = 2023) %>% 
  rbind(ff_scoringhistory(dyno_2025, season = 2024)) %>% 
  rbind(ff_scoringhistory(dyno_2025, season = 2025)) %>%
  rbind(ff_scoringhistory(dyno_2025, season = 2026)) %>%
  mutate(player_name = gsub(" Jr.","",player_name),
         player_name = gsub("'","",player_name) ) %>% 
  filter(week <18)

trade_tracker = data.frame()

for(x in unique(trades$Trade_ID)) {
  trade_i =trades %>% 
    filter(Trade_ID == x) %>% 
    mutate(player_name = gsub(" Jr.","",player_name),
           player_name = gsub("'","",player_name),
           drafted_player = gsub(" Jr.","",drafted_player),
           drafted_player = gsub("'","",drafted_player)) %>% 
    mutate(player = ifelse(player_name =="Pick", drafted_player,player_name))
  
  traded_players = Scoring %>% 
    left_join(Schedule_map %>% 
                rename(season = Season, week = Week)) %>% 
    filter(player_name %in% c(unique(trade_i$drafted_player),unique(trade_i$player_name)),
           Timestamp >(unique(trade_i$timestamp))) %>% 
    group_by(player = player_name,pos) %>% 
    summarise(points = sum(points))
  
  trade_i = trade_i %>% 
    left_join(traded_players)
  trade_tracker = trade_tracker %>% 
    rbind(trade_i)
}

DateUpdated = paste0("Last Updated ",format(Sys.Date(), "%m/%d/%Y"))

trade_tracker_final = trade_tracker %>% 
  select(Timestamp = timestamp, "Trade ID" = Trade_ID, Owner, "Player Name" = player_name,
         Position = position, "Pick Year" = pick_year, "Pick Round" = pick_round,
         Pick = pick, "Pick Original Owner" = pick_original_owner,
         "Drafted Player" = drafted_player, "Points Scored Since Trade" = points,PickKnown) %>% 
  mutate(!!sym(DateUpdated) := "")%>% 
  group_by(`Trade ID`) %>% 
  mutate(Owner_num = ifelse(Owner == max(Owner),1,0)) %>% 
  ungroup

sheet_write(trade_tracker_final,
            ss = "https://docs.google.com/spreadsheets/d/1asR4OJwjrwA8DVGsgO7TZlEnq3ngEGnI6L63XV7BMOI/edit?usp=sharing",
            sheet = "Unformatted")

