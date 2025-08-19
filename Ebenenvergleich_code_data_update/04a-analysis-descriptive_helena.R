#'* THINGS TO REVISIT BELOW*
#'* what kind of graphic aggregation? right now, geom_smooth(span = 0.1) on daily aggregates *

# SETUP ========================================================================

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
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
  mutate(perc_attack = 100 * (n_attacks / n_posts)) %>%
  mutate(level = "BTW") 

btw_fb_byday <- btw_fb_bypartyday %>%
  group_by(date_election, date_created, days_to_election, post_week) %>%
  summarise(n_posts = sum(n_posts),
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = 100 * (n_attacks / n_posts)) %>%
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
  mutate(perc_attack = 100 * (n_attacks / n_posts)) %>%
  mutate(level = "BTW") 

btw_tw_byday <- btw_tw_bypartyday %>%
  group_by(date_election, date_created, days_to_election, post_week) %>%
  summarise(n_posts = sum(n_posts), # re-coded n_tweets = sum(n_tweets) to n_posts = sum(n_tweets), so everything normalised to posts (for tw and fb)
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = 100 * (n_attacks / n_posts)) %>%
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
  mutate(perc_attack = 100 * (n_attacks / n_posts)) %>%
  mutate(level = "LTWs (pooled)")

ltw_fb_byday <- ltw_fb_bypartyday %>%
  group_by(date_election, date_created, days_to_election, post_week) %>%
  summarise(n_posts = sum(n_posts),
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = 100 * (n_attacks / n_posts)) %>%
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
  mutate(perc_attack = 100 * (n_attacks / n_posts)) %>%
  mutate(level = "LTWs (pooled)")

ltw_tw_byday <- ltw_tw_bypartyday %>%
  group_by(date_election, date_created, days_to_election, post_week) %>%
  summarise(n_posts = sum(n_posts),
            n_attacks = sum(n_attacks)) %>%
  mutate(perc_attack = 100 * (n_attacks / n_posts)) %>%
  mutate(party_cat = "all") %>%
  mutate(level = "LTWs (pooled)") %>%
  as.data.frame()


# FB PLOTS ====================================================================

## by day all parties ####

plot <- rbind(btw_fb_byday, ltw_fb_byday) %>%
  ggplot(aes(y = perc_attack, x = days_to_election*(-1))) +
  geom_smooth(span = 0.1, se = F) +
  facet_wrap(~ level, scales = "free_x") +
  scale_y_continuous(name = "% negative posts out of all posts", limits = c(0,10)) +
  scale_x_continuous(name = "Days to election") +
  theme_light() +
  geom_vline(aes(xintercept = 0), linetype = "dashed")

ggsave("replication_mehrebenen_paper/negative_posts_fb_byday_plot.png", plot = plot, width = 8, height = 6, dpi = 300)
## by day by party ####

plot <- rbind(btw_fb_bypartyday, ltw_fb_bypartyday) %>%
  ggplot(aes(y = perc_attack, x = days_to_election*(-1),
             group = party_cat, color = party_cat)) +
  geom_smooth(span = 0.1, se = F) +
  facet_wrap(~ level, scales = "free_x") +
  scale_y_continuous(name = "% negative posts out of all posts", limits = c(0,30)) +
  scale_x_continuous(name = "Days to election") +
  scale_color_manual(
    values = c("blue", "black", "lightblue", "purple", "yellow","darkgreen", "grey", "red")) +
  theme_light() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  theme(legend.position = "bottom")
ggsave("replication_mehrebenen_paper/negative_posts_fb_bypartyday_plot.png", plot = plot, width = 8, height = 6, dpi = 300)

# TW PLOTS ====================================================================#####
####still using fb datasets, had to be adjusted

## by day all parties ####

#btw plot is empty
plot <- rbind(btw_tw_byday, ltw_tw_byday) %>%
  ggplot(aes(y = perc_attack, x = days_to_election*(-1))) +
  geom_smooth(span = 0.1, se = F) +
  facet_wrap(~ level, scales = "free_x") +
  scale_y_continuous(name = "% negative posts out of all posts", limits = c(0,20)) +
  scale_x_continuous(name = "Days to election") +
  theme_light() +
  geom_vline(aes(xintercept = 0), linetype = "dashed")
ggsave("replication_mehrebenen_paper/negative_posts_tw_byday_plot.png", plot = plot, width = 8, height = 6, dpi = 300)

## by day by party ####

plot <- rbind(btw_tw_bypartyday, ltw_tw_bypartyday) %>%
  ggplot(aes(y = perc_attack, x = days_to_election*(-1),
             group = party_cat, color = party_cat)) +
  geom_smooth(span = 0.1, se = F) +
  facet_wrap(~ level, scales = "free_x") +
  scale_y_continuous(name = "% negative posts out of all posts", limits = c(0,30)) +
  scale_x_continuous(name = "Days to election") +
  scale_color_manual(
    values = c("blue", "black", "lightblue", "purple", "yellow","darkgreen", "grey", "red")) +
  theme_light() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  theme(legend.position = "bottom")
ggsave("replication_mehrebenen_paper/negative_posts_tw_bypartyday_plot.png", plot = plot, width = 8, height = 6, dpi = 300)

