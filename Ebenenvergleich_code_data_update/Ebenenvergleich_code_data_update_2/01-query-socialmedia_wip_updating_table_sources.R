## Necessary files 
# SETUP ========================================================================

library(RMySQL)
library(DBI) 
library(data.table)
library(openxlsx)
library(tidyverse)
options(scipen = 100)

user <- "marius2"
password <- "marius"
db_name <- "ncdfg"
db_host <- "18.156.8.93"
db_port <- 3306
db <- dbConnect(
  RMySQL::MySQL(),
  user = user, password = password,
  dbname = db_name, host = db_host, port = db_port
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#Joined ltw

#fb_ltw_bycand
#- candidacies24
candidacies24 <- dbGetQuery(db, "SELECT * FROM candidacies24")
candidacies_5_0_server <- dbGetQuery(db, "SELECT * FROM candidacies_5_0 LIMIT 200")
#core24
core24 <- dbGetQuery(db, "SELECT * FROM core24")
candidates_5_0 <- dbGetQuery(db, "SELECT * FROM candidates_5_0")


# - elections24
elections24 <- dbGetQuery(db, "SELECT * FROM elections24")
election_dates_5_0 <- dbGetQuery(db, "SELECT * FROM election_dates_5_0")
# - fb_accounts_new
fb_accounts_new <- dbGetQuery(db, "SELECT * FROM fb_accounts_new LIMIT 200")
# - posts_fin
posts_fin <- dbGetQuery(db, "SELECT * FROM posts_fin LIMIT 200") ## this is the one I decide to use
# - predict_attack
predict_attack_fb <- dbGetQuery(db, "SELECT * FROM predict_attack_fb LIMIT 200") ## seems to be long enoough to be from posts_fin?, but uses twitter_id name column?


fb_ltw_bycand_aktuell <- dbGetQuery(
  db, 
  "SELECT election_id, cand_id, SERIAL, COUNT(*) n_posts, SUM(attack) AS n_attacks FROM 
  (SELECT c.cand_id, c.election_id, c.SERIAL, e.election_date AS date_election,
  c.party_id, fb.fb_id, 
  DATE(p.created_at) AS date_created, 
  REGEXP_REPLACE(p.post_id, 'posts/', '') post_id, 
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies_5_0 c
  LEFT JOIN election_dates_5_0 e ON c.election_id = e.election_id
  LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
  LEFT JOIN posts_fin p ON p.fb_id = fb.fb_id
  LEFT JOIN predict_attack_fb a ON REGEXP_REPLACE(p.post_id, 'posts/', '') = a.tweet_id
  WHERE c.election_id in ('ltw_be_2021',
  'ltw_bw_2021',
  'ltw_mv_2021', 
  'ltw_rp_2021', 
  'ltw_st_2021', 
  'ltw_ni_2022', 
  'ltw_nw_2022', 
  'ltw_sh_2022', 
  'ltw_sl_2022', 
  'ltw_be_2023',
  'ltw_by_2023',
  'ltw_hb_2023',
  'ltw_he_2023',
  'ltw_bb_2024',
  'ltw_sn_2024',
  'ltw_th_2024',
  'ltw_hh_2025')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) BETWEEN 0 AND 90
  GROUP BY election_id, cand_id, SERIAL
  ORDER BY election_id, cand_id
") 


meta_ltw_aktuell <- dbGetQuery(
  db, 
  "SELECT c1.cand_id, c1.gender, c1.birth_year, 
  c2.incumbent, c2.party_id, c2.election_id, c2.invited, c2.participated
  FROM candidates_5_0 c1
  LEFT JOIN candidacies_5_0 c2 on c1.cand_id = c2.cand_id
  WHERE c2.election_id in ('ltw_be_2021',
  'ltw_bw_2021',
  'ltw_mv_2021', 
  'ltw_rp_2021', 
  'ltw_st_2021', 
  'ltw_ni_2022', 
  'ltw_nw_2022', 
  'ltw_sh_2022', 
  'ltw_sl_2022', 
  'ltw_be_2023',
  'ltw_by_2023',
  'ltw_hb_2023',
  'ltw_he_2023',
  'ltw_bb_2024',
  'ltw_sn_2024',
  'ltw_th_2024',
  'ltw_hh_2025')")
#TODO NOTE changed Seb 'ltw_by_2023', 'ltw_hb_2023', 'ltw_he_2023', 
fb_ltw_bycand_aktuell_safety_copy <- fb_ltw_bycand_aktuell
fb_ltw_bycand_aktuell <- fb_ltw_bycand_aktuell %>%
  right_join(., meta_ltw_aktuell, by = c("cand_id", "election_id"))
##after joining this has a row count og 10543, same length as the metw_ltw_aktuell
##this makes sense because for each meta contains the participation in all the elections
##of the participants, 


# 
# 
# tw_ltw_bycand
# - candidacies24
# - elections24
# - tw_accounts_new
tw_accounts_new <- dbGetQuery(db, "SELECT * FROM tw_accounts_new")
# - tweets
tweets <- dbGetQuery(db, "SELECT * FROM tweets")
tweets2<- dbGetQuery(db, "SELECT * FROM tweets2 LIMIT 100")

# - predict
predict <- dbGetQuery(db, "SELECT * FROM predict LIMIT 100")


tw_ltw_bycand_aktuell <- dbGetQuery(
  db, 
  "SELECT election_id, cand_id, SERIAL, COUNT(*) n_tweets, SUM(attack) AS n_attacks FROM 
  (SELECT c.cand_id, c.election_id, c.SERIAL, e.election_date AS date_election,
  c.party_id, tw.twitter_id, 
  DATE(t.created_at) AS date_created, t.tweet_id,  
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies_5_0 c
  LEFT JOIN election_dates_5_0 e ON c.election_id = e.election_id
  LEFT JOIN tw_accounts_new tw ON tw.candid = c.cand_id
  LEFT JOIN tweets2 t ON t.twitter_id = tw.twitter_id
  LEFT JOIN predict a ON t.tweet_id = a.tweet_id
  WHERE c.election_id in ('ltw_be_2021',
  'ltw_bw_2021',
  'ltw_mv_2021', 
  'ltw_rp_2021', 
  'ltw_st_2021', 
  'ltw_ni_2022', 
  'ltw_nw_2022', 
  'ltw_sh_2022', 
  'ltw_sl_2022', 
  'ltw_be_2023',
  'ltw_by_2023',
  'ltw_hb_2023',
  'ltw_he_2023',
  'ltw_bb_2024',
  'ltw_sn_2024',
  'ltw_th_2024',
  'ltw_hh_2025')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) BETWEEN 0 AND 90
  GROUP BY election_id, cand_id, SERIAL
  ORDER BY election_id, cand_id") 

tw_ltw_bycand_aktuell_safety_copy <- tw_ltw_bycand_aktuell
tw_ltw_bycand_aktuell <- tw_ltw_bycand_aktuell %>%
  right_join(., meta_ltw_aktuell, by = c("cand_id", "election_id"))

# where a match could not be made, the columns are NA
