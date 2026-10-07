source("scripts/master.R")

Schedule_map = read.csv("data/NFL Schedule.csv") %>% 
  mutate(Timestamp = as.POSIXct(Timestamp, format = "%m/%d/%Y %H:%M:%S"))
Owner_map = read.csv("data/DynastyOwnerMapping.csv")

#Bring in Transactions data
Trans_23 = ff_transactions(dyno_2023)
Trans_24 = ff_transactions(dyno_2024) %>% 
  select(-waiver_priority)
Trans_25 = ff_transactions(dyno_2025)%>% 
  select(-waiver_priority)
Trans_26 = ff_transactions(dyno_2026)%>% 
  select(-waiver_priority)

#Read in scoring data
Scoring = ff_scoringhistory(dyno_2025, season = 2023) %>% 
  rbind(ff_scoringhistory(dyno_2025, season = 2024)) %>% 
  rbind(ff_scoringhistory(dyno_2025, season = 2025)) %>% 
  rbind(ff_scoringhistory(dyno_2026, season = 2026)) %>% 
  mutate(player_name = gsub(" Jr.","",player_name),
         player_name = gsub("'","",player_name) ) %>% 
  filter(week <18) %>% 
  left_join(Schedule_map %>% 
              rename(season = Season,week=Week))

#Save All transactions 
All_Trans = Trans_23 %>% 
  rbind(Trans_24) %>% 
  rbind(Trans_25) %>% 
  rbind(Trans_26)

#Filter for FA and Waiver adds
waivers = All_Trans %>% 
  filter(type != "waiver_failed",
         type_desc =="added",
         !is.na(player_name)) %>% 
  mutate(num = 1:n())

#Find next transaction and add it as a new variable
waivers_2 = data.frame()
for(x in unique(waivers$num)) {
  waiver_i = waivers %>% 
    filter(num == x)
  
  boobies = All_Trans %>% 
    filter(timestamp > waiver_i$timestamp[1],
           player_name == waiver_i$player_name[1],
           franchise_id == waiver_i$franchise_id[1],
           type != "waiver_failed") %>% 
    group_by(player_name) %>% 
    filter(timestamp == min(timestamp)) %>% 
    ungroup() %>% 
    select(NextTrans = type_desc,player_name,franchise_id,timestamp_NextTrans = timestamp)
    
  waiver_i2 = waiver_i %>% 
    left_join(boobies)
  
  waivers_2 = waivers_2 %>% 
    rbind(waiver_i2)
  
}

current_roster = ff_rosters(dyno_2025)%>% 
  mutate(player_name = gsub(" Jr.","",player_name),
         player_name = gsub("'","",player_name) ) %>% 
  select(franchise_id, player_name,age)
test = waivers_2 %>% 
  filter(is.na(NextTrans)) %>% 
  left_join(current_roster)
  


#adjust names for matching and add current time for players still on roster
waivers_2 =waivers_2 %>% 
  mutate(player_name = gsub(" Jr.","",player_name),
         player_name = gsub("'","",player_name),
         NextTrans = ifelse(is.na(NextTrans), "Still on Roster",NextTrans)) %>% 
  filter(is.na(timestamp_NextTrans)) %>% 
  mutate(timestamp_NextTrans = Sys.time()) %>% 
  rbind(waivers_2 %>% 
          mutate(player_name = gsub(" Jr.","",player_name),
                 player_name = gsub("'","",player_name),
                 NextTrans = ifelse(is.na(NextTrans), "Still on Roster",NextTrans)) %>% 
          filter(!is.na(timestamp_NextTrans)))
  

#Calculate points while on team and add it as a variable
waivers_3 = data.frame()
for(x in unique(waivers$num)){
  waiver_i = waivers_2 %>% 
    filter(num == x)
  
  boobies = Scoring %>% 
    filter(player_name == waiver_i$player_name[1],
           Timestamp > waiver_i$timestamp[1],
           Timestamp < waiver_i$timestamp_NextTrans[1]) %>% 
    group_by(player_name) %>% 
    summarise(PointsOnTeam = sum(points))
    
  waiver_i2 = waiver_i %>% 
    left_join(boobies)
  
  waivers_3 = waivers_3 %>% 
    rbind(waiver_i2)
}

waiver_tracker = waivers_3 %>% 
  left_join(trade_tracker_final %>% 
              select(timestamp_NextTrans = Timestamp,`Trade ID`) %>% 
              distinct()) %>%
  mutate(franchise_id = as.integer(franchise_id)) %>% 
  left_join(Owner_map) %>% 
  select(1,2,4,7,8,10,14:18)

DateUpdated = paste0("Last Updated ",format(Sys.Date(), "%m/%d/%Y"))

waiver_tracker_final = waiver_tracker %>% 
  select(Timestamp = timestamp, Owner = owner, Type = type, Player = player_name, Position = pos,
         "FAAB Bid" = bbid_amount, "Next Transaction" = NextTrans, "Timestamp of NT" = timestamp_NextTrans,
         "Points Scored On Team" = PointsOnTeam, `Trade ID`) %>% 
  arrange(Timestamp) %>% 
  mutate(!!sym(DateUpdated) := "")

sheet_write(waiver_tracker_final,
            ss = "https://docs.google.com/spreadsheets/d/1D-9FLE_V8UEevAo_X4mCYe7SwWsUpB2xDxGlRv9ggUQ/edit?gid=0#gid=0",
            sheet = "Sheet1")
