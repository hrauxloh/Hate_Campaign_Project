
#'* THINGS TO REVISIT *
#'* right now, 90 days before election queried -> can be adapted *
#'* tweet / post with > 0.5 "prob_attack" -> negative; different cutoffs?*
#'* Berlin: only using ltw_be_2021 not ltw_be_2023 *
#'* table names will be different, e.g., "predict" *
#'* FB queries: 'posts/' issue should be solved*
#'* data formats will change, e.g., candid will change to cand_id... *
#'* gles_id_21 will be in different table (other_ids or something)*

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

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# dbListTables(db)
# dbGetQuery(db, "describe tw_accounts_new")

# BTW 2021 ==========================================================================

## Facebook ####

### by party and day ####

fb_btw_bypartyday <- dbGetQuery(
  db, 
  "SELECT 
  date_created, date_election, days_to_election, party, 
  COUNT(*) n_posts, SUM(attack) AS n_attacks FROM 
  (SELECT *, 
  TIMESTAMPDIFF(DAY, date_created, date_election) AS days_to_election
  FROM
  (SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') date_election,
  c.party, fb.fb_id, 
  STR_TO_DATE(p.created_at, '%Y-%m-%d') date_created, 
  REGEXP_REPLACE(p.post_id, 'posts/', '') post_id, 
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies24 c  
  LEFT JOIN elections24 e ON c.election_id = e.election
  LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
  LEFT JOIN posts_fin p ON p.fb_id = fb.fb_id
  LEFT JOIN predict_attack_fb a ON REGEXP_REPLACE(p.post_id, 'posts/', '') = a.tweet_id
  WHERE c.election_id in ('btw_2021')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) >= 0 
  AND TIMESTAMPDIFF(DAY, date_created, date_election) <= 90) dt2
  GROUP BY date_created, date_election, days_to_election, party 
  ORDER BY date_created, date_election, days_to_election, party") 

write.csv(fb_btw_bypartyday, "data/fb_btw_bypartyday.csv", row.names = F)

### by candidate ####

fb_btw_bycand <- dbGetQuery(
  db, 
  "SELECT election_id, cand_id, COUNT(*) n_posts, SUM(attack) AS n_attacks FROM 
  (SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') date_election,
  c.party, fb.fb_id, 
  STR_TO_DATE(p.created_at, '%Y-%m-%d') date_created, 
  REGEXP_REPLACE(p.post_id, 'posts/', '') post_id, 
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies24 c
  LEFT JOIN elections24 e ON c.election_id = e.election
  LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
  LEFT JOIN posts_fin p ON p.fb_id = fb.fb_id
  LEFT JOIN predict_attack_fb a ON REGEXP_REPLACE(p.post_id, 'posts/', '') = a.tweet_id
  WHERE c.election_id in ('btw_2021')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) >= 0 
  AND TIMESTAMPDIFF(DAY, date_created, date_election) <= 90
  GROUP BY election_id, cand_id 
  ORDER BY election_id, cand_id") 

#### for GLES merge ####

meta_btw <- dbGetQuery(
  db, 
  "SELECT c1.cand_id, c1.gender, c1.birthyear, 
  c2.incumbent, c2.party, c2.gles_id_21
  FROM core24 c1
  LEFT JOIN candidacies24 c2 on c1.cand_id = c2.cand_id
  WHERE c2.election_id in ('btw_2021')") 

fb_btw_bycand <- fb_btw_bycand %>%
  right_join(., meta_btw, by = "cand_id")

write.csv(fb_btw_bycand, "replication_mehrebenen_paper/fb_btw_bycand.csv", row.names = F)

## Twitter ####

### by party and day ####

tw_btw_bypartyday <- dbGetQuery(
  db, 
  "SELECT 
  date_created, date_election, days_to_election, party, 
  COUNT(*) n_tweets, SUM(attack) AS n_attacks FROM 
  (SELECT *, 
  TIMESTAMPDIFF(DAY, date_created, date_election) AS days_to_election
  FROM
  (SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') date_election,
  c.party, tw.twitter_id, 
  DATE(STR_TO_DATE(t.created_at, '%Y-%m-%d %H:%i:%s')) date_created, t.tweet_id, 
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies24 c
  LEFT JOIN elections24 e ON c.election_id = e.election
  LEFT JOIN tw_accounts_new tw ON tw.candid = c.cand_id
  LEFT JOIN tweets t ON t.twitter_id = tw.twitter_id
  LEFT JOIN predict a ON t.tweet_id = a.tweet_id
  WHERE c.election_id in ('btw_2021')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) >= 0 
  AND TIMESTAMPDIFF(DAY, date_created, date_election) <= 90) dt2
  GROUP BY date_created, date_election, days_to_election, party 
  ORDER BY date_created, date_election, days_to_election, party") 

write.csv(tw_btw_bypartyday, "replication_mehrebenen_paper/tw_btw_bypartyday.csv", row.names = F)

### by candidate ####

tw_btw_bycand <- dbGetQuery(
  db, 
  "SELECT cand_id, COUNT(*) n_tweets, SUM(attack) AS n_attacks FROM 
  (SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') date_election,
  c.party, tw.twitter_id, 
  DATE(STR_TO_DATE(t.created_at, '%Y-%m-%d %H:%i:%s')) date_created, t.tweet_id, 
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies24 c
  LEFT JOIN elections24 e ON c.election_id = e.election
  LEFT JOIN tw_accounts_new tw ON tw.candid = c.cand_id
  LEFT JOIN tweets t ON t.twitter_id = tw.twitter_id
  LEFT JOIN predict a ON t.tweet_id = a.tweet_id
  WHERE c.election_id in ('btw_2021')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) >= 0 
  AND TIMESTAMPDIFF(DAY, date_created, date_election) <= 90
  GROUP BY election_id, cand_id 
  ORDER BY election_id, cand_id") 

#### for GLES merge ####

tw_btw_bycand <- tw_btw_bycand %>%
  right_join(., meta_btw, by = "cand_id")

write.csv(tw_btw_bycand, "replication_mehrebenen_paper/tw_btw_bycand.csv", row.names = F)
  
# LTWs =========================================================================

## Facebook ####

### by party and day ####

fb_ltw_bypartyday <- dbGetQuery(
  db, 
  "SELECT 
  date_created, election_id, date_election, days_to_election, party, 
  COUNT(*) n_posts, SUM(attack) AS n_attacks FROM 
  (SELECT *, 
  TIMESTAMPDIFF(DAY, date_created, date_election) AS days_to_election
  FROM
  (SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') date_election,
  c.party, fb.fb_id, 
  STR_TO_DATE(p.created_at, '%Y-%m-%d') date_created, 
  REGEXP_REPLACE(p.post_id, 'posts/', '') post_id, 
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies24 c
  LEFT JOIN elections24 e ON c.election_id = e.election
  LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
  LEFT JOIN posts_fin p ON p.fb_id = fb.fb_id
  LEFT JOIN predict_attack_fb a ON REGEXP_REPLACE(p.post_id, 'posts/', '') = a.tweet_id
  WHERE c.election_id in ('ltw_be_2021', 'ltw_bw_2021',
  'ltw_by_2023', 'ltw_hb_2023', 'ltw_he_2023', 'ltw_mv_2021', 'ltw_ni_2022', 
  'ltw_nw_2022', 'ltw_rp_2021', 'ltw_sh_2022', 'ltw_sl_2022', 'ltw_st_2021')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) >= 0 
  AND TIMESTAMPDIFF(DAY, date_created, date_election) <= 90) dt2
  GROUP BY date_created, election_id, date_election, days_to_election, party 
  ORDER BY date_created, election_id, date_election, days_to_election, party") 

write.csv(fb_ltw_bypartyday, "replication_mehrebenen_paper/fb_ltw_bypartyday.csv", row.names = F)

### by candidate ####

fb_ltw_bycand <- dbGetQuery(
  db, 
  "SELECT election_id, cand_id, COUNT(*) n_posts, SUM(attack) AS n_attacks FROM 
  (SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') date_election,
  c.party, fb.fb_id, 
  STR_TO_DATE(p.created_at, '%Y-%m-%d') date_created, 
  REGEXP_REPLACE(p.post_id, 'posts/', '') post_id, 
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies24 c
  LEFT JOIN elections24 e ON c.election_id = e.election
  LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
  LEFT JOIN posts_fin p ON p.fb_id = fb.fb_id
  LEFT JOIN predict_attack_fb a ON REGEXP_REPLACE(p.post_id, 'posts/', '') = a.tweet_id
  WHERE c.election_id in ('ltw_be_2021', 'ltw_bw_2021',
  'ltw_by_2023', 'ltw_hb_2023', 'ltw_he_2023', 'ltw_mv_2021', 'ltw_ni_2022', 
  'ltw_nw_2022', 'ltw_rp_2021', 'ltw_sh_2022', 'ltw_sl_2022', 'ltw_st_2021')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) >= 0 
  AND TIMESTAMPDIFF(DAY, date_created, date_election) <= 90
  GROUP BY election_id, cand_id 
  ORDER BY election_id, cand_id") 

meta_ltw <- dbGetQuery(
  db, 
  "SELECT c1.cand_id, c1.gender, c1.birthyear, 
  c2.incumbent, c2.party, c2.ltw_id, c2.invited, c2.participated
  FROM core24 c1
  LEFT JOIN candidacies24 c2 on c1.cand_id = c2.cand_id
  WHERE c2.election_id in ('ltw_be_2021', 'ltw_bw_2021',
  'ltw_by_2023', 'ltw_hb_2023', 'ltw_he_2023', 'ltw_mv_2021', 'ltw_ni_2022', 
  'ltw_nw_2022', 'ltw_rp_2021', 'ltw_sh_2022', 'ltw_sl_2022', 'ltw_st_2021')")

fb_ltw_bycand <- fb_ltw_bycand %>%
  right_join(., meta_ltw, by = "cand_id")

write.csv(fb_ltw_bycand, "replication_mehrebenen_paper/fb_ltw_bycand.csv", row.names = F)

## Twitter ####

### by party and day 

tw_ltw_bypartyday_requeried <- dbGetQuery(
  db, 
  "SELECT 
  date_created, date_election, days_to_election, party, 
  COUNT(*) n_tweets, SUM(attack) AS n_attacks FROM 
  (SELECT *, 
  TIMESTAMPDIFF(DAY, date_created, date_election) AS days_to_election
  FROM
  (SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') date_election,
  c.party, tw.twitter_id, 
  DATE(STR_TO_DATE(t.created_at, '%Y-%m-%d %H:%i:%s')) date_created, t.tweet_id, 
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies24 c
  LEFT JOIN elections24 e ON c.election_id = e.election
  LEFT JOIN tw_accounts_new tw ON tw.candid = c.cand_id
  LEFT JOIN tweets t ON t.twitter_id = tw.twitter_id
  LEFT JOIN predict a ON t.tweet_id = a.tweet_id
  WHERE c.election_id in ('ltw_be_2021', 'ltw_bw_2021',
  'ltw_by_2023', 'ltw_hb_2023', 'ltw_he_2023', 'ltw_mv_2021', 'ltw_ni_2022', 
  'ltw_nw_2022', 'ltw_rp_2021', 'ltw_sh_2022', 'ltw_sl_2022', 'ltw_st_2021')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) >= 0 
  AND TIMESTAMPDIFF(DAY, date_created, date_election) <= 90) dt2
  GROUP BY date_created, date_election, days_to_election, party 
  ORDER BY date_created, date_election, days_to_election, party") 

write.csv(tw_ltw_bypartyday, "replication_mehrebenen_paper/tw_ltw_bypartyday.csv", row.names = F)

### by candidate ####

tw_ltw_bycand <- dbGetQuery(
  db, 
  "SELECT election_id, cand_id, COUNT(*) n_tweets, SUM(attack) AS n_attacks FROM 
  (SELECT c.cand_id, c.ltw_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') date_election,
  c.party, tw.twitter_id, 
  DATE(STR_TO_DATE(t.created_at, '%Y-%m-%d %H:%i:%s')) date_created, t.tweet_id, 
  CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
  FROM candidacies24 c
  LEFT JOIN elections24 e ON c.election_id = e.election
  LEFT JOIN tw_accounts_new tw ON tw.candid = c.cand_id
  LEFT JOIN tweets t ON t.twitter_id = tw.twitter_id
  LEFT JOIN predict a ON t.tweet_id = a.tweet_id
  WHERE c.election_id in ('ltw_be_2021', 'ltw_bw_2021',
  'ltw_by_2023', 'ltw_hb_2023', 'ltw_he_2023', 'ltw_mv_2021', 'ltw_ni_2022', 
  'ltw_nw_2022', 'ltw_rp_2021', 'ltw_sh_2022', 'ltw_sl_2022', 'ltw_st_2021')) dt1
  WHERE TIMESTAMPDIFF(DAY, date_created, date_election) >= 0 
  AND TIMESTAMPDIFF(DAY, date_created, date_election) <= 90
  GROUP BY election_id, cand_id 
  ORDER BY election_id, cand_id") 

tw_ltw_bycand <- tw_ltw_bycand %>%
  right_join(., meta_ltw, by = "cand_id")

write.csv(tw_ltw_bycand, "replication_mehrebenen_paper/tw_ltw_bycand.csv", row.names = F)

