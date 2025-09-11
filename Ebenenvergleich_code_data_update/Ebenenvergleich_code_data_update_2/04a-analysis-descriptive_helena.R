#'* THINGS TO REVISIT BELOW*
#'* what kind of graphic aggregation? right now, geom_smooth(span = 0.1) on daily aggregates *

# SETUP ========================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# PREPROCESSING ================================================================

## BTW 2021 ####

### Facebook ####

#btw_fb_bypartyday <- read.csv("data/btw_fb_bypartyday.csv")# no such file trying now fb_btw_bypartyday.csv
btw_fb_bypartyday <- read.csv("replication_mehrebenen_paper/fb_btw_bypartyday.csv")

btw_fb_bypartyday <- btw_fb_bypartyday %>%
  mutate(date_election = as.POSIXct(date_election),
         date_created = as.POSIXct(date_created), 
         post_week = format(date_created, "%W")) %>%
  mutate(party_cat = ifelse(
    party %in% c("AfD", "CDU", "CSU", "DIE LINKE", "FDP", "GRÜNE", "SPD"), party, "other")) %>%
  group_by(date_election, date_created, days_to_election, post_week, party_cat) %>%
  summarise(n_posts = sum(n_posts),
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = (n_attacks / n_posts)) %>%
  mutate(level = "BTW") 

btw_fb_byday <- btw_fb_bypartyday %>%
  group_by(date_election, date_created, days_to_election, post_week) %>%
  summarise(n_posts = sum(n_posts),
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = (n_attacks / n_posts)) %>%
  mutate(party_cat = "all")  %>%
  mutate(level = "BTW") %>%
  as.data.frame()

### Twitter #### this code works 

btw_tw_bypartyday <- read.csv("replication_mehrebenen_paper/tw_btw_bypartyday.csv")

btw_tw_bypartyday <- btw_tw_bypartyday %>%
  mutate(date_election = as.POSIXct(date_election),
         date_created = as.POSIXct(date_created), 
         post_week = format(date_created, "%W")) %>%
  mutate(party_cat = ifelse(
    party %in% c("AfD", "CDU", "CSU", "DIE LINKE", "FDP", "GRÜNE", "SPD"), party, "other")) %>%
  group_by(date_election, date_created, days_to_election, post_week, party_cat) %>%
  summarise(n_posts = sum(n_tweets),
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = (n_attacks / n_posts)) %>%
  mutate(level = "BTW") 

btw_tw_byday <- btw_tw_bypartyday %>%
  group_by(date_election, date_created, days_to_election, post_week) %>%
  summarise(n_posts = sum(n_posts), # re-coded n_tweets = sum(n_tweets) to n_posts = sum(n_tweets), so everything normalised to posts (for tw and fb)
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = (n_attacks / n_posts)) %>%
  mutate(party_cat = "all")  %>%
  mutate(level = "BTW") %>%
  as.data.frame()

## LTWs ####

### Facebook ####

ltw_fb_bypartyday <- read.csv("replication_mehrebenen_paper/fb_ltw_bypartyday.csv")

ltw_fb_bypartyday <- ltw_fb_bypartyday %>%
  mutate(date_election = as.POSIXct(date_election),
         date_created = as.POSIXct(date_created), 
         post_week = format(date_created, "%W")) %>%
  mutate(party_cat = ifelse(
    party %in% c("AfD", "CDU", "CSU", "DIE LINKE", "FDP", "GRÜNE", "SPD"), party, "other")) %>%
  group_by(date_election, date_created, days_to_election, post_week, party_cat) %>%
  summarise(n_posts = sum(n_posts),
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = (n_attacks / n_posts)) %>%
  mutate(level = "LTWs (pooled)")

ltw_fb_byday <- ltw_fb_bypartyday %>%
  group_by(date_election, date_created, days_to_election, post_week) %>%
  summarise(n_posts = sum(n_posts),
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = (n_attacks / n_posts)) %>%
  mutate(party_cat = "all") %>%
  mutate(level = "LTWs (pooled)") %>%
  as.data.frame()

### Twitter ####   

ltw_tw_bypartyday <- read.csv("replication_mehrebenen_paper/tw_ltw_bypartyday.csv")

ltw_tw_bypartyday <- ltw_tw_bypartyday %>%
  mutate(date_election = as.POSIXct(date_election),
         date_created = as.POSIXct(date_created), 
         post_week = format(date_created, "%W")) %>%
  mutate(party_cat = ifelse(
    party %in% c("AfD", "CDU", "CSU", "DIE LINKE", "FDP", "GRÜNE", "SPD"), party, "other")) %>%
  group_by(date_election, date_created, days_to_election, post_week, party_cat) %>%
  summarise(n_posts = sum(n_tweets),  ### re-coded n_posts to n_tweets
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = (n_attacks / n_posts)) %>%
  mutate(level = "LTWs (pooled)")

ltw_tw_byday <- ltw_tw_bypartyday %>%
  group_by(date_election, date_created, days_to_election, post_week) %>%
  summarise(n_posts = sum(n_posts),
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = (n_attacks / n_posts)) %>%
  mutate(party_cat = "all") %>%
  mutate(level = "LTWs (pooled)") %>%
  as.data.frame()


# FB PLOTS ====================================================================

## by day all parties ####

bind_rows(btw_fb_byday, ltw_fb_byday) %>%
  as_tibble() %>% 
  ggplot(aes(y = perc_attack, x = days_to_election*(-1))) +
  geom_smooth(span = 0.1, se = F) +
  facet_wrap(~ level, scales = "free_x") +
  scale_x_continuous(name = "Days to election") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_light() +
  labs(y = "Share of attacks among all posts") +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(0, NA))

ggsave("replication_mehrebenen_paper/negative_posts_fb_byday_plot.png", width = 8, height = 6, dpi = 600)

## by day by party ####

rbind(btw_fb_bypartyday, ltw_fb_bypartyday) %>%
  ggplot(aes(y = perc_attack, x = days_to_election*(-1),
             group = party_cat, color = party_cat)) +
  geom_smooth(span = 0.1, se = F) +
  facet_wrap(~level, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format()) +  
  scale_x_continuous(name = "Days to election") +
  scale_color_manual(
    values = c("blue", "black", "lightblue", "purple", "yellow","darkgreen", "grey", "red")) +
  theme_light() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  labs(y = "Share of attacks among all posts") +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0, NA))
ggsave("replication_mehrebenen_paper/negative_posts_fb_bypartyday_plot.png", width = 8, height = 6, dpi = 600)

# TW PLOTS ====================================================================#####
####still using fb datasets, had to be adjusted

## by day all parties ####

rbind(btw_tw_byday, ltw_tw_byday) %>%
  ggplot(aes(y = perc_attack, x = days_to_election*(-1))) +
  geom_smooth(span = 0.1, se = F) +
  facet_wrap(~level, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format()) +  
  scale_x_continuous(name = "Days to election") +
  labs(y = "Share of attacks among all posts") +
  theme_light() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(0, NA))
ggsave("replication_mehrebenen_paper/negative_posts_tw_byday_plot.png", width = 8, height = 6, dpi = 600)

## by day by party ####

rbind(btw_tw_bypartyday, ltw_tw_bypartyday) %>%
  ggplot(aes(y = perc_attack, x = days_to_election*(-1),
             group = party_cat, color = party_cat)) +
  geom_smooth(span = 0.1, se = F, n = 100) +
  facet_wrap(~ level, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format()) +  
  scale_x_continuous(name = "Days to election") +
  scale_color_manual(
    values = c("blue", "black", "lightblue", "purple", "yellow","darkgreen", "grey", "red")) +
  theme_light() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  labs(y = "Share of attacks among all posts") +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0, NA))
ggsave("replication_mehrebenen_paper/negative_posts_tw_bypartyday_plot.png", width = 8, height = 6, dpi = 600)


# INTEGRATED PLOTS ====================================================================#####
####still using fb datasets, had to be adjusted

## by day all parties ####

rbind(btw_tw_byday %>% mutate(platform = "Twitter"), 
      ltw_tw_byday %>% mutate(platform = "Twitter"), 
      btw_fb_byday %>% mutate(platform = "Facebook"),  
      ltw_fb_byday %>% mutate(platform = "Facebook")) %>%
  ggplot(aes(y = perc_attack, x = days_to_election*(-1), color = platform)) +
  geom_smooth(span = 0.1, se = F) +
  facet_wrap(~level, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format()) +  
  scale_x_continuous(name = "Days to election") +
  labs(y = "Share of attacks among all posts") +
  theme_light() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(0, NA)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values = c(
    "Facebook" = "#0D47A1", # Deeper blue
    "Twitter" = "#42A5F5"   # Lighter blue
  ))
ggsave("replication_mehrebenen_paper/negative_posts_platformIntegrated_byday_plot.png", width = 8, height = 6, dpi = 600)

# SAMPLE DESCRIPTIVES ================================================================

# TODO remore server info before OSF
candidacies <- read_csv("/Users/sebstier/Library/CloudStorage/Dropbox/nc_db_austausch/Ergebnisse/output_tables_4_0/candidacies_finalid_4_0.csv")

user <- "christopher"
password <- "welcomeatgesis"
db_name <- 'ncdfg'
db_host <- '18.156.8.93' # our server access
db_port <- 3306
us <-  dbConnect(RMySQL::MySQL(), user = user, password = password,
                 dbname = db_name, host = db_host, port = db_port)
dbSendQuery(us, "SET NAMES utf8mb4;")
dbSendQuery(us, "SET CHARACTER SET utf8mb4;")
dbSendQuery(us, "SET character_set_connection=utf8mb4;")

# Elections
df_elect <- dbGetQuery(us,"select * from election_dates;") %>% 
  dplyr::select(-row_names) %>% 
  bind_rows(
    data.frame(
      election = c("LTW Bayern", "LTW Hessen", "LTW Bremen", "LTW Berlin 2023",
                   "LTW Sachsen", "LTW Thüringen", "LTW Brandenburg"), 
      election_date = c("2023-10-08", "2023-10-08", "2023-05-14", "2023-02-12",
                        "2024-09-01", "2024-09-01", "2024-09-22"
      ) 
    )
  ) %>% 
  bind_cols(tibble(election_id = c("btw_2013", "btw_2017", "btw_2021",
                                   "ltw_mv_2021", "ltw_be_2021", "ltw_sl_2022",
                                   "ltw_bw_2021", "ltw_st_2021", "ltw_rp_2021",
                                   "ltw_nw_2022", "ltw_sh_2022", "ltw_ni_2022",
                                   "ltw_by_2023", "ltw_he_2023", "ltw_hb_2023",
                                   "ltw_be_2023", "ltw_sn_2024", "ltw_th_2024",
                                   "ltw_bb_2024")))
# Merge
joined_ltw <- read_csv("replication_mehrebenen_paper/joined_ltw.csv")
joined_btw <- read_csv("replication_mehrebenen_paper/svy_btw.csv")
# Create similar overview files for BTW
sm_btw <- read_csv("replication_mehrebenen_paper/joined_btw.csv")
btw_to_join <- candidacies %>% 
  filter(election_id == "btw_2021") %>% 
  left_join(sm_btw %>% dplyr::select(-election_id), by = "cand_id")

df_desc <- candidacies %>% 
  filter(election_id %in% c(joined_ltw$election_id)) %>% 
  left_join(joined_ltw, by = c("cand_id", "election_id")) %>% 
  bind_rows(btw_to_join) %>% 
  mutate(active_fb = n_posts > 0,
         active_tw = n_tweets > 0) %>% 
  left_join(df_elect, by = "election_id")
df_desc %>% 
  group_by(election, election_date) %>% 
  summarise(n_candidates = n_distinct(cand_id),
            share_active_fb = round(sum(active_fb, na.rm = T)/n_candidates*100, 1),
            share_active_tw = round(sum(active_tw, na.rm = T)/n_candidates*100, 1),
            share_participated = round(mean(participated_bin, na.rm = T)*100, 1)
            ) %>% 
  mutate(election = str_remove(election, "LTW "),
         share_participated = ifelse(election == "BTW 2021", 28.7, share_participated)) %>% 
  arrange(election_date) %>% 
  kable(format = "html", table.attr = "class='table table-bordered table-hover'",
        col.names = c("Election", "Date", "N Candidates", "Facebook (in %)", "Twitter (in %)", "Survey (in %)")) %>%
  kable_styling(bootstrap_options = c("striped"), full_width = F) 

