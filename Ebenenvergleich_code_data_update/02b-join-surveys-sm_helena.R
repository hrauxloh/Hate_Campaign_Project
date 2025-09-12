# SETUP ========================================================================

library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# LTW ==========================================================================

# fb_ltw <- read.csv("replication_mehrebenen_paper/fb_ltw_bycand.csv")
# tw_ltw <- read.csv("replication_mehrebenen_paper/tw_ltw_bycand.csv")

svy_ltw <- read.csv("replication_mehrebenen_paper/svy_ltw.csv")

sm_ltw_old <- full_join(
  fb_ltw_bycand_old  %>% rename("n_attacks_fb" = n_attacks),
  tw_ltw_bycand_old  %>% dplyr::select(cand_id, n_tweets, "n_attacks_tw" = n_attacks),
  by = c("cand_id")) 

sm_ltw_old <- sm_ltw_old %>%
  # proportion of attacks
  mutate(prop_attacks_fb = (n_attacks_fb / n_posts),
         prop_attacks_tw = (n_attacks_tw / n_tweets)) %>%
  # participated and invited - check Mona's stuff for values
  mutate(invited_bin = ifelse(invited %in% c(1, 2), 1, 0), 
         participated_bin = ifelse(participated %in% c(1, 2, 3), 1, 0)) %>%
  # whether candidate has social media data
  mutate(any_posts_tweets = ifelse(n_tweets > 0 | n_posts > 0, 1, 0))
  
joined_ltw_old <- sm_ltw_old %>%
  full_join(., svy_ltw, by = c("ltw_id" = "SERIAL")) 

write.csv(joined_ltw, "replication_mehrebenen_paper/joined_ltw.csv", row.names = F)


# BTW / GLES ===================================================================
#Prepare aggregated social media data for merging with GLES
fb_btw <- read.csv("replication_mehrebenen_paper/fb_btw_bycand.csv") %>% 
  filter(!is.na(n_posts)) %>% 
  as_tibble()
tw_btw <- read.csv("replication_mehrebenen_paper/tw_btw_bycand.csv") %>% 
  filter(!is.na(n_tweets)) %>% 
  as_tibble()

sm_btw <- full_join(
  fb_btw %>% rename("n_attacks_fb" = n_attacks),
  tw_btw %>% dplyr::select(cand_id, n_tweets, "n_attacks_tw" = n_attacks),
  by = c("cand_id"))

sm_btw <- sm_btw %>%
  # proportion of attacks
  mutate(prop_attacks_fb = (n_attacks_fb / n_posts),
         prop_attacks_tw = (n_attacks_tw / n_tweets)) %>%
  # participated and invited - check Mona's stuff for values
  #mutate(invited_bin = ifelse(invited %in% c(1, 2), 1, 0),
         #participated_bin = ifelse(participated %in% c(1, 2, 3), 1, 0)) %>%
  # whether candidate has social media data
  mutate(any_posts_tweets = ifelse(n_tweets > 0 | n_posts > 0, 1, 0))

# joined_btw <- sm_btw %>%
#   full_join(., svy_btw, by = c("btw_id" = "SERIAL"))

write.csv(sm_btw, "replication_mehrebenen_paper/joined_btw.csv",
          row.names = F) 

