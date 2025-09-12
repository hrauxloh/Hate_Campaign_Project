
#'* THINGS TO REVISIT BELOW*
#'* size of campaign team: some implausible values -> check*
#'* Check NAs in final data set *
#'* Berlin: only using ltw_be_2021 not ltw_be_2023 *
#'* The code for GLES will only work if the combined GLES social media data set has the same variable structure*

# SETUP ========================================================================

library(haven)
library(tidyverse)
library(psych)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load file with the paths to read in the GLES candidate files that are stored locally
source("../raw_data_paths.R")
if (Sys.getenv()[4] == "unix2003") {
  path_to_gles = PATH1
}
# 
vars_to_scale <- c(
  "lr_self", "extr_self",  "lr_party", "extr_party",
  "lr_voters", "extr_voters",  "dist_party", "dist_voters",
  "big5_extra", "big5_agrea", "big5_consc", "big5_emot", "big5_open",
  "race_chance", "race_close", "media_diffic", "media_impact", "camp_personal")
# 
# # BTW GLES =====================================================================
# #svy_btw_00OLD <- read_stata(paste0(path_to_gles, "/ZA7704_v1-0-0_merge.dta"))
# svy_btw_00 <- read_stata(paste0(path_to_gles, "/ZA7704_v2-0-0_merge.dta"))
# #ncol(svy_btw_00OLD)
# ncol(svy_btw_00)
# 
# ## Recode missing into "NA" ####
# 
# svy_btw_01 <- svy_btw_00 %>%
#   mutate(across(c(
#     a2a, c5, c6b, c6c, c6d, c6h, c6e, c6g, c6f, c7,
#     e19a, e19b, e19c, e19d, e19e, e19f, e19g, e19h, e19i, e19j, 
#     b17a, b17b, b18, b20c, b9, b2, b5, b3a, b4), 
#     ~ ifelse(. < 0, NA, .), .names = "{.col}_r"))
# 
# ## Sociodemographics ####
# 
# svy_btw_10 <- svy_btw_01 %>%
#   mutate(
#     female = geschlecht - 1,
#     age = 2021 - geburtsjahr,
#     immigr = e4 - 1)
# 
# ## Political profile ####
# 
# svy_btw_11 <- svy_btw_10 %>%
#   mutate(
#     # party
#     party = partei,
#     # type of candidature
#     cand_list = ifelse(kandidaturtyp %in% c(1, 3), 1, 0),
#     cand_constit = ifelse(kandidaturtyp %in% c(2, 3), 1, 0),
#     # incumbency
#     incumb = ifelse(a2a_r == 3, 1, 0))
# 
# svy_btw_12 <- svy_btw_11 %>%
#   # part of governing party: previously governing CDU + CSU + SPD
#   ## `svy_btw_11$party %>% attr("labels")` shows labels
#   mutate(governing = ifelse(party %in% c(1, 2, 3, 4), 1, 0))
# 
# svy_btw_13 <- svy_btw_12 %>%
#   mutate(
#     # left-right ideology (self): 1... 11
#     lr_self = c5_r,
#     # ideological extremism (self): 0 ... 5
#     extr_self = abs(c5_r - 6),
#     # left-right ideology (party): 1... 11
#     lr_party = case_when(
#       party %in% c(1, 2) ~ c6b_r,
#       party == 3 ~ c6c_r,
#       party == 4 ~ c6d_r,
#       party == 5 ~ c6e_r,
#       party == 6 ~ c6f_r,
#       party == 7 ~ c6g_r,
#       party == 322 ~ c6h_r),
#     # ideological extremism (party): 0 ... 5
#     extr_party = abs(lr_party - 6),
#     # left-right ideology (voters): 1... 11
#     lr_voters = c7_r,
#     # ideological extremism (voters): 0 ... 5
#     extr_voters = abs(lr_voters  - 6)) %>%
#   # ideological distance
#   mutate(
#     # distance to party: 0... 20
#     dist_party = (lr_self - lr_party) + 10, 
#     # distance to voters: 0... 20 
#     dist_voters = (lr_self - lr_voters) + 10)
# 
# ## Personality ####
# 
# svy_btw_20 <- svy_btw_13 %>%
#   # reverse code items
#   mutate(across(
#     c(e19a_r, e19g_r, e19h_r, e19d_r, e19j_r), 
#     ~ 6 - ., .names = "{.col}_rev")) %>%
#   rowwise() %>% mutate(
#     # indices: 1... 5
#     big5_extra = mean(c(e19a_r_rev, e19f_r)), # zurueckhaltend, gesellig
#     big5_agrea = mean(c(e19b_r, e19g_r_rev)), # vertrauensvoll, kritisch
#     big5_consc = mean(c(e19c_r, e19h_r_rev)), # gruendlich, bequem
#     big5_emot = mean(c(e19d_r_rev, e19i_r)), # entspannt, nervoes
#     big5_open = mean(c(e19e_r, e19j_r_rev))) # fantasievoll, wenig kuenstlerisch
# 
# # # CHECK ALPHA  ###< a bit now maybe for all of them?
# svy_btw_20 %>% dplyr::select(e19a_r_rev, e19f_r) %>% psych::alpha() %>% summary()
# svy_btw_20 %>% dplyr::select(e19b_r, e19g_r_rev) %>% psych::alpha() %>% summary()
# svy_btw_20 %>% dplyr::select(e19c_r, e19h_r_rev) %>% psych::alpha() %>% summary()
# svy_btw_20 %>% dplyr::select(e19d_r_rev, e19i_r) %>% psych::alpha() %>% summary()
# svy_btw_20 %>% dplyr::select(e19e_r, e19j_r_rev) %>% psych::alpha() %>% summary()
# svy_btw_20 %>% dplyr::select(e19a_r_rev, e19f_r,
#                       e19b_r, e19g_r_rev,
#                       e19c_r, e19h_r_rev,
#                       e19d_r_rev, e19i_r,
#                       e19e_r, e19j_r_rev) %>% psych::alpha(check.keys = TRUE) %>% summary()
# 
# ## Campaign dynamics ####
# 
# svy_btw_30 <- svy_btw_20 %>%
#   rowwise() %>% 
#   mutate(
#     # perceived chance to win the race: 1... 5
#     race_chance = case_when(
#       kandidaturtyp == 1 ~ b17b_r,
#       kandidaturtyp == 2 ~ b17a_r,
#       kandidaturtyp == 3 ~ mean(c(b17b_r, b17a_r), na.rm = T)))
# 
# svy_btw_31 <- svy_btw_30 %>%
#   mutate(
#     # perceived closeness of the race: 0... 2
#     race_chance_fold = abs(3 - race_chance),
#     race_close = 2 - race_chance_fold,
#     # Perceived difficulty to receive media attention: 1 ... 5
#     media_diffic = 6 - b18_r,
#     # Perceived media impact on voters: 1... 5
#     media_impact = b20c_r,
#     # Personalized campaign: 0 ... 10
#     camp_personal = 12 - b9_r)
# 
# ## Campaign resources ####
# 
# svy_btw_40 <- svy_btw_31 %>%
#   mutate(
#     # Time spent for campaign: in hours
#     camp_time = as.numeric(b2_r), 
#     # Campaign budget: factor
#     camp_budget = factor(
#       case_when(
#         b5_r < 1000 ~ 1,
#         b5_r >= 1000 & b5_r < 5000 ~ 2,
#         b5_r >= 5000 & b5_r < 10000 ~ 3,
#         b5_r >= 10000 & b5_r < 30000 ~ 4,
#         b5_r >= 30000 & b5_r < 50000 ~ 5,
#         b5_r >= 50000 ~ 6), ordered = T, levels = 1:6,
#       labels = c("Unter 1.000 Euro", "1.000 Euro bis unter 5.000 Euro",
#                  "5.000 Euro bis unter 10.000 Euro", "10.000 bis unter 30.000 Euro",
#                  "30.000 bis unter 50.000 Euro", "50.000 Euro und mehr")),
#     # Size of campaign team
#     # table(svy_btw_31$b3a) reveals some implausible values
#     camp_staff = as.numeric(b3a_r),
#     # Professional campaign manager: 0/1
#     camp_manager = 2 - b4_r)
# 
# ## Scale variables ####
# 
# svy_btw_40[vars_to_scale] <- lapply(svy_btw_40[vars_to_scale], function(x) {
#   (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) 
# })
# 
# # Calculate proportion of attacks ####
# 
# svy_btw_40 <- svy_btw_40 %>% 
#   mutate(n_tweets = as.numeric(n_tweets),
#          n_attacks_tw = as.numeric(n_attacks_tw),
#          prop_attacks_fb = (n_attacks_fb / n_posts),
#          prop_attacks_tw = (n_attacks_tw / n_tweets))
# 
# svy_btw <- svy_btw_40 %>% 
#   dplyr::select(
#     bula, wei_kandi,
#     party, incumb, governing,
#     lr_self, extr_self, lr_party, extr_party, lr_voters, extr_voters, 
#     dist_party, dist_voters, female, age, immigr,
#     big5_extra, big5_agrea, big5_consc, big5_emot, big5_open,
#     race_chance, race_close, media_diffic, media_impact, camp_personal,
#     camp_time, camp_budget, camp_staff, camp_manager,
#     prop_attacks_fb, prop_attacks_tw, n_attacks_fb, n_attacks_tw, n_posts, n_tweets,
#     ) 
# 
# write.csv(svy_btw, "replication_mehrebenen_paper/svy_btw.csv", row.names = F)
# 

# LTWs =========================================================================

svy_ltw_00 <- read_sav("replication_mehrebenen_paper/Pool_Kandidaten_V10_scales_2024_08_02 (1).sav")

## election_ids ####
## svy_ltw_00$land %>% attr('labels')
svy_ltw_01 <- svy_ltw_00 %>%
  mutate(election_id = case_when(
    wahl == 11 ~ "ltw_sh_2022",
    wahl == 13 ~ "ltw_hb_2023",
    wahl == 41 ~ "ltw_ni_2022",
    wahl == 51 ~ "ltw_nw_2022",
    wahl == 61 ~ "ltw_he_2023",
    wahl == 71 ~ "ltw_rp_2021",
    wahl == 81 ~ "ltw_bw_2021",
    wahl == 91 ~ "ltw_by_2023",
    wahl == 101 ~ "ltw_sl_2022",
    wahl == 111 ~ "ltw_be_2021",
    wahl == 112 ~ "ltw_be_2023",
    wahl == 121 ~ "ltw_mv_2021",
    wahl == 141 ~ "ltw_st_2021")) %>%
  # filter out Berlin 2023 and other 2023 LTWs
  filter(election_id != "ltw_be_2023" & election_id != "ltw_by_2023" & 
         election_id != "ltw_he_2023" & election_id != "ltw_hb_2023")
# (Saxony-Anhalt 2021, Berlin 2021, Mecklenburg-Western Pomerania 2021, Schleswig-Holstein 2022, 
# North Rhine-Westphalia 2022, Lower Saxony 2022, and Saarland 2022).

## Sociodemographics ####

svy_ltw_10 <- svy_ltw_01 %>%
  mutate(
    female = w1 - 1, 
    age = jahr - w2,
    immigr = case_when(
      SD24_01 == 2 ~ 1, 
      SD24_02 == 2 ~ 1, 
      SD24_01 == 1 & SD24_02 == 1 ~ 0))

## Political profile ####

svy_ltw_11 <- svy_ltw_10 %>%
  mutate(
    # party
    party = w3, 
    # type of candidature
    ## `svy_ltw_00$w8 %>% attr("labels")` for labels
    cand_list = ifelse(w8 %in% c(2, 3), 1, 0),
    cand_constit = ifelse(w8 %in% c(1, 3), 1, 0),
    # incumbency
    incumb = w6)

svy_ltw_12 <- svy_ltw_11 %>%
  # part of governing party
  ## `svy_ltw_11$wahl %>% attr("labels")` for labels
  ## `svy_ltw_11$party %>% attr("labels")` for labels
  mutate(governing = case_when(
    # Landtagswahl Schleswig-Holstein 2022: previously governing CDU + FDP + Greens
    wahl == 11 & party %in% c(1, 4, 6) ~ 1,
    # Landtagswahl Niedersachsen 2022: previously governing CDU + SPD
    wahl == 41 & party %in% c(1, 2) ~ 1,
    # Landtagswahl Nordrhein-Westfalen 2022: previously governing CDU + FDP
    wahl == 51 & party %in% c(1, 4) ~ 1,
    # Landtagswahl Rheinland-Pfalz 2021: previously governing SPD + FDP + Greens
    wahl == 71 & party %in% c(2, 4, 6) ~ 1,
    # Landtagswahl Baden-Württemberg 2021: previously governing CDU + Greens
    wahl == 81 & party %in% c(1, 6) ~ 1,
    # Landtagswahl Saarland 2022: previously governing CDU + SPD
    wahl == 101 & party %in% c(1, 2) ~ 1,
    # Abgeordnetenhauswahl Berlin 2021: previously governing SPD + Left + Green
    wahl == 111 & party %in% c(2, 5, 6) ~ 1,
    # Landtagswahl Mecklenburg-Vorpommern 2021: previously governing CDU + SPD
    wahl == 121 & party %in% c(1, 2) ~ 1,
    # Landtagswahl Sachsen-Anhalt 2021: previously governing CDU + SPD + Green
    wahl == 141 & party %in% c(1, 2, 6) ~ 1,
    is.na(wahl) ~ NA,
    TRUE ~ 0))

svy_ltw_13 <- svy_ltw_12 %>%
  mutate(
    # left-right ideology (self): 1... 11
    lr_self = MD06_01,
    # ideological extremism (self): 0... 5
    extr_self = abs(MD06_01 - 6), 
    # left-right ideology (party): 1... 11
    lr_party = MD06_02,
    # ideological extremism (party): 0... 5
    extr_party = abs(MD06_02  - 6),
    # left-right ideology (voters): 1... 11
    lr_voters= MD06_03,
    # ideological extremism (voters): 0... 5
    extr_voters = abs(MD06_03  - 6)) %>%
  # ideological distance
  mutate(
    # distance to party: 0... 20
    dist_party = (lr_self - lr_party) + 10, 
    # distance to voters: 0... 20 
    dist_voters = (lr_self - lr_voters) + 10)
  
## Personality ####

svy_ltw_20 <- svy_ltw_13 %>%
  # reverse code items
  mutate(across(
    c(WE03_06, 
      WE03_04, WE03_13, 
      WE03_12, WE04_10, 
      WE03_16, WE04_07, 
      WE03_11), 
    ~ 6 - ., .names = "{.col}_rev")) %>%
  rowwise() %>% mutate(
    # indices: 1... 5
    big5_extra = mean(c(WE03_06_rev, WE03_15, WE04_06)), 
    big5_agrea = mean(c(WE03_04_rev, WE03_13_rev, WE04_04, WE04_11)),
    big5_consc = mean(c(WE03_03, WE03_12_rev, WE04_03, WE04_10_rev)),
    big5_emot = mean(c(WE03_07, WE03_16_rev, WE04_07_rev, WE04_14)),
    big5_open = mean(c(WE03_02, WE03_11_rev, WE04_02, WE04_09)))
    
# # CHECK ALPHA
# svy_ltw_20 %>% select(WE03_06_r, WE03_15, WE04_06) %>% psych::alpha() %>% summary()
# svy_ltw_20 %>% select(WE03_04_r, WE03_13_r, WE04_04, WE04_11) %>% psych::alpha() %>% summary()
# svy_ltw_20 %>% select(WE03_03, WE03_12_r, WE04_03, WE04_10_r) %>% psych::alpha() %>% summary()
# svy_ltw_20 %>% select(WE03_07, WE03_16_r, WE04_07_r, WE04_14) %>% psych::alpha() %>% summary()
# svy_ltw_20 %>% select(WE03_02, WE03_11_r, WE04_02, WE04_09) %>% psych::alpha() %>% summary()

## Campaign dynamics ####

svy_ltw_30 <- svy_ltw_20 %>%
  mutate(
    # perceived chance to win the race: 1... 5
    race_chance = LW06_01,
    # perceived closeness of the race: 0... 2
    LW06_02_fold = abs(3 - LW06_02),
    race_close = 2 - LW06_02_fold,
    # Perceived difficulty to receive media attention: 1 ... 5
    media_diffic = 6 - MD03,
    # Perceived media impact on voters: 1 ... 5
    media_impact = MD05,
    # Personalized campaign: 1 ... 11
    camp_personal = 12 - WS21_02)

## Campaign resources ####

svy_ltw_40 <- svy_ltw_30 %>%
  mutate(
    # Time spent for campaign: in hours
    camp_time = as.numeric(LW07_01), 
    # Campaign budget: factor
    camp_budget = factor(
      LW10, ordered = T, levels = 1:6,
      labels = c("Unter 1.000 Euro", "1.000 Euro bis unter 5.000 Euro",
                 "5.000 Euro bis unter 10.000 Euro", "10.000 bis unter 30.000 Euro",
                 "30.000 bis unter 50.000 Euro", "50.000 Euro und mehr")),
    # Size of campaign team
    # table(svy_ltw_40$LW08_01) reveals some implausible values - what to do?
    camp_staff = as.numeric(LW08_01),
    # Professional campaign manager: 0/1
    camp_manager = 2 - LW09)

## Scale variables ####

svy_ltw_40[vars_to_scale] <- lapply(svy_ltw_40[vars_to_scale], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) 
  })

## Export file ####

svy_ltw <- svy_ltw_40 %>% 
  dplyr::select(
    jahr, land, wahl, SERIAL,
    female, age, immigr,
    party, incumb, governing,
    lr_self, extr_self,  lr_party, extr_party, lr_voters, extr_voters, 
    dist_party, dist_voters,
    big5_extra, big5_agrea, big5_consc, big5_emot, big5_open,
    race_chance, race_close, media_diffic, media_impact, camp_personal,
    camp_time, camp_budget, camp_staff,
    camp_manager) 

write.csv(svy_ltw, "replication_mehrebenen_paper/svy_ltw.csv", row.names = F)

