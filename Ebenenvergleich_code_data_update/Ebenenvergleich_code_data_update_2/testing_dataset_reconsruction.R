## Query inspection quizz

# Step 1: candidacies + elections
INSP_cand_elec <- dbGetQuery(
  db, 
  "SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') AS date_election,
          c.party
   FROM candidacies24 c
   LEFT JOIN elections24 e ON c.election_id = e.election
   WHERE c.election_id IN ('ltw_be_2021', 'ltw_bw_2021',
                           'ltw_mv_2021', 'ltw_ni_2022', 
                           'ltw_nw_2022', 'ltw_rp_2021', 
                           'ltw_sh_2022', 'ltw_sl_2022', 'ltw_st_2021')"
)

cat("INSP_cand_elec: rows =", nrow(INSP_cand_elec), 
    ", cols =", ncol(INSP_cand_elec), "\n")
print(colSums(is.na(INSP_cand_elec)))

# Step 2: + fb_accounts_new
INSP_cand_fb <- dbGetQuery(
  db, 
  "SELECT c.*, fb.fb_id
   FROM candidacies24 c
   LEFT JOIN elections24 e ON c.election_id = e.election
   LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
   WHERE c.election_id IN ('ltw_be_2021', 'ltw_bw_2021',
                           'ltw_mv_2021', 'ltw_ni_2022', 
                           'ltw_nw_2022', 'ltw_rp_2021', 
                           'ltw_sh_2022', 'ltw_sl_2022', 'ltw_st_2021')"
)

cat("INSP_cand_fb: rows =", nrow(INSP_cand_fb), 
    ", cols =", ncol(INSP_cand_fb), "\n")
print(colSums(is.na(INSP_cand_fb)))

# Step 3: + posts_fin
INSP_cand_fb_posts <- dbGetQuery(
  db, 
  "SELECT c.*, STR_TO_DATE(p.created_at, '%Y-%m-%d') AS date_created,
          REGEXP_REPLACE(p.post_id, 'posts/', '') AS post_id
   FROM (SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') AS date_election,
                c.party, fb.fb_id
         FROM candidacies24 c
         LEFT JOIN elections24 e ON c.election_id = e.election
         LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
         WHERE c.election_id IN ('ltw_be_2021', 'ltw_bw_2021',
                                 'ltw_mv_2021', 'ltw_ni_2022', 
                                 'ltw_nw_2022', 'ltw_rp_2021', 
                                 'ltw_sh_2022', 'ltw_sl_2022', 'ltw_st_2021')) c
   LEFT JOIN posts_fin p ON p.fb_id = c.fb_id"
)

cat("INSP_cand_fb_posts: rows =", nrow(INSP_cand_fb_posts), 
    ", cols =", ncol(INSP_cand_fb_posts), "\n")
print(colSums(is.na(INSP_cand_fb_posts)))

# Step 4: + predict_attack_fb
INSP_final_df <- dbGetQuery(
  db, 
  "SELECT c.*, CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
   FROM (SELECT c.cand_id, c.election_id, STR_TO_DATE(e.date, '%m/%d/%Y') AS date_election,
                c.party, fb.fb_id, 
                STR_TO_DATE(p.created_at, '%Y-%m-%d') AS date_created,
                REGEXP_REPLACE(p.post_id, 'posts/', '') AS post_id
         FROM candidacies24 c
         LEFT JOIN elections24 e ON c.election_id = e.election
         LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
         LEFT JOIN posts_fin p ON p.fb_id = fb.fb_id
         WHERE c.election_id IN ('ltw_be_2021', 'ltw_bw_2021',
                                 'ltw_mv_2021', 'ltw_ni_2022', 
                                 'ltw_nw_2022', 'ltw_rp_2021', 
                                 'ltw_sh_2022', 'ltw_sl_2022', 'ltw_st_2021')) c
   LEFT JOIN predict_attack_fb a ON c.post_id = a.tweet_id"
)

cat("INSP_final_df: rows =", nrow(INSP_final_df), 
    ", cols =", ncol(INSP_final_df), "\n")
print(colSums(is.na(INSP_final_df)))

# Step 5: summarise to fb_ltw_bycand_old
INSP_fb_ltw_bycand_old <- INSP_final_df %>%
  mutate(days_diff = as.numeric(difftime(date_election, date_created, units = "days"))) %>%
  filter(days_diff >= 0, days_diff <= 90) %>%
  group_by(election_id, cand_id) %>%
  summarise(
    n_posts = n(),
    n_attacks = sum(attack, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(election_id, cand_id)

cat("INSP_fb_ltw_bycand_old: rows =", nrow(INSP_fb_ltw_bycand_old), 
    ", cols =", ncol(INSP_fb_ltw_bycand_old), "\n")
#INSP_fb_ltw_bycand_old: rows = 1825 , cols = 4 
print(colSums(is.na(INSP_fb_ltw_bycand_old)))
# election_id     cand_id     n_posts   n_attacks 
#           0           0           0           0 




###### Code inspection for new code 
library(dplyr)

# helper function
inspect_df <- function(df, name) {
  cat("\n---", name, "---\n")
  cat("Rows:", nrow(df), " Cols:", ncol(df), "\n")
  print(colSums(is.na(df)))
}

# Step 1: candidacies + election_dates
INSPEC2_cand_elec <- dbGetQuery(
  db, 
  "SELECT c.cand_id, c.election_id, c.SERIAL, c.party_id,
          e.election_date AS date_election
   FROM candidacies_5_0 c
   LEFT JOIN election_dates_5_0 e ON c.election_id = e.election_id
   WHERE c.election_id IN ('ltw_be_2021','ltw_bw_2021','ltw_mv_2021','ltw_rp_2021',
                           'ltw_st_2021','ltw_ni_2022','ltw_nw_2022','ltw_sh_2022',
                           'ltw_sl_2022','ltw_be_2023','ltw_by_2023','ltw_hb_2023',
                           'ltw_he_2023','ltw_bb_2024','ltw_sn_2024','ltw_th_2024',
                           'ltw_hh_2025')"
)
inspect_df(INSPEC2_cand_elec, "Step 1: cand_elec")
# --- Step 1: cand_elec ---
#   Rows: 10543  Cols: 5 
# cand_id   election_id        SERIAL      party_id date_election 
#       0             0             0            44             0 

# Step 2: + fb_accounts
INSPEC2_cand_fb <- dbGetQuery(
  db, 
  "SELECT c.cand_id, c.election_id, c.SERIAL, c.party_id, e.election_date,
          fb.fb_id
   FROM candidacies_5_0 c
   LEFT JOIN election_dates_5_0 e ON c.election_id = e.election_id
   LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
   WHERE c.election_id IN ('ltw_be_2021','ltw_bw_2021','ltw_mv_2021','ltw_rp_2021',
                           'ltw_st_2021','ltw_ni_2022','ltw_nw_2022','ltw_sh_2022',
                           'ltw_sl_2022','ltw_be_2023','ltw_by_2023','ltw_hb_2023',
                           'ltw_he_2023','ltw_bb_2024','ltw_sn_2024','ltw_th_2024',
                           'ltw_hh_2025')"
)
inspect_df(INSPEC2_cand_fb, "Step 2: cand_fb")
# 
# --- Step 2: cand_fb ---
#   Rows: 12626  Cols: 6 
# cand_id   election_id        SERIAL      party_id election_date         fb_id 
#       0             0             0            50             0          6923 

# Step 3: + posts_fin
INSPEC2_cand_fb_posts <- dbGetQuery(
  db, 
  "SELECT c.cand_id, c.election_id, c.SERIAL, c.party_id, e.election_date,
          fb.fb_id,
          DATE(p.created_at) AS date_created,
          REGEXP_REPLACE(p.post_id, 'posts/', '') AS post_id
   FROM candidacies_5_0 c
   LEFT JOIN election_dates_5_0 e ON c.election_id = e.election_id
   LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
   LEFT JOIN posts_fin p ON p.fb_id = fb.fb_id
   WHERE c.election_id IN ('ltw_be_2021','ltw_bw_2021','ltw_mv_2021','ltw_rp_2021',
                           'ltw_st_2021','ltw_ni_2022','ltw_nw_2022','ltw_sh_2022',
                           'ltw_sl_2022','ltw_be_2023','ltw_by_2023','ltw_hb_2023',
                           'ltw_he_2023','ltw_bb_2024','ltw_sn_2024','ltw_th_2024',
                           'ltw_hh_2025')"
)
inspect_df(INSPEC2_cand_fb_posts, "Step 3: cand_fb_posts")
# --- Step 3: cand_fb_posts ---
#   Rows: 1872238  Cols: 8 
# cand_id   election_id        SERIAL      party_id election_date         fb_id  date_created 
#       0             0             0          1290             0          6923          8187 
# post_id 
#    8187 

# Step 4: + predict_attack_fb
INSPEC2_final_df <- dbGetQuery(
  db, 
  "SELECT c.cand_id, c.election_id, c.SERIAL, c.party_id, e.election_date,
          fb.fb_id,
          DATE(p.created_at) AS date_created,
          REGEXP_REPLACE(p.post_id, 'posts/', '') AS post_id,
          CASE WHEN a.Attack > 0.5 THEN 1 ELSE 0 END AS attack
   FROM candidacies_5_0 c
   LEFT JOIN election_dates_5_0 e ON c.election_id = e.election_id
   LEFT JOIN fb_accounts_new fb ON fb.cand_id = c.cand_id
   LEFT JOIN posts_fin p ON p.fb_id = fb.fb_id
   LEFT JOIN predict_attack_fb a ON REGEXP_REPLACE(p.post_id, 'posts/', '') = a.tweet_id
   WHERE c.election_id IN ('ltw_be_2021','ltw_bw_2021','ltw_mv_2021','ltw_rp_2021',
                           'ltw_st_2021','ltw_ni_2022','ltw_nw_2022','ltw_sh_2022',
                           'ltw_sl_2022','ltw_be_2023','ltw_by_2023','ltw_hb_2023',
                           'ltw_he_2023','ltw_bb_2024','ltw_sn_2024','ltw_th_2024',
                           'ltw_hh_2025')"
)
inspect_df(INSPEC2_final_df, "Step 4: final_df (with attack)")
# --- Step 4: final_df (with attack) ---
#   Rows: 1895680  Cols: 9 
# cand_id   election_id        SERIAL      party_id election_date         fb_id  date_created 
#       0             0             0          1342             0          6923          8187 
# post_id        attack 
#    8187             0 

# Step 5: filter by date diff (0–90 days) and aggregate
INSPEC2_fb_ltw_bycand_aktuell <- INSPEC2_final_df %>%
  mutate(days_diff = as.numeric(difftime(as.Date(election_date),
                                         as.Date(date_created),
                                         units = "days"))) %>%
  filter(!is.na(days_diff), days_diff >= 0, days_diff <= 90) %>%
  group_by(election_id, cand_id, SERIAL) %>%
  summarise(n_posts = n(),
            n_attacks = sum(attack, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(election_id, cand_id)

inspect_df(INSPEC2_fb_ltw_bycand_aktuell, "Step 5: aggregated fb_ltw_bycand_aktuell")
# --- Step 5: aggregated fb_ltw_bycand_aktuell ---
#   Rows: 2497  Cols: 5 
# election_id     cand_id      SERIAL     n_posts   n_attacks 
#           0           0           0           0           0 

#### Joining test ----
## inspection of new survey data and importing
svy_2025 <- read_sav("Ebenenvergleich_code_data_update_2/Pool_Kandidaten_V12_scales_2025_06_18.sav")


##bundesland codes
# 1 = sh
# 2 = hh
# 3 = hb
# 4 = ni
# 5 = nw
# 6 = he
# 7 = rp
# 8 = bw
# 9 = by
# 10 = sl
# 11 = he
# 12 = mv
# 13 =bb
# 14 = st
# 15 = th
# 16 = sn


svy_2025_ltw_01 <- svy_2025 %>% ## 
  mutate(election_id = case_when(
    wahl == 11 ~ "ltw_sh_2022",
    wahl == 13 ~ "ltw_hb_2023", ## is this 31? ## could be something contributing to high N later on
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
    wahl == 141 ~ "ltw_st_2021",
    wahl == 21 ~ "ltw_hh_2025",
    wahl == 31 ~ "ltw_hb_2023",
    wahl == 131 ~ "ltw_bb_2024",
    wahl == 151 ~ "ltw_th_2024",
    wahl == 161 ~ "ltw_sn_2024")) #%>%
  # # REMOVED: filter out Berlin 2023 and other 2023 LTWs
  # filter(election_id != "ltw_be_2023" & election_id != "ltw_by_2023" & 
  #          election_id != "ltw_he_2023" & election_id != "ltw_hb_2023")
# Sociodemographics ####

svy_2025_ltw_10 <- svy_2025_ltw_01 %>%
  mutate(
    female = w1 - 1, 
    age = jahr - w2,
    immigr = case_when(
      SD24_01 == 2 ~ 1, 
      SD24_02 == 2 ~ 1, 
      SD24_01 == 1 & SD24_02 == 1 ~ 0))

## Political profile ####

svy_2025_ltw_11 <- svy_2025_ltw_10 %>%
  mutate(
    # party
    party = w3, 
    # type of candidature
    ## `svy_ltw_00$w8 %>% attr("labels")` for labels
    cand_list = ifelse(w8 %in% c(2, 3), 1, 0),
    cand_constit = ifelse(w8 %in% c(1, 3), 1, 0),
    # incumbency
    incumb = w6)

## Adding if candidate is from a governing party
svy_2025_ltw_12 <- svy_2025_ltw_11 %>%
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
    ######new ltws
    # Landtagswahl Hamburg 2025: previously governing SPD + Green
    wahl == 21 & party %in% c(2, 6) ~ 1,
    # Landtagswahl Bremen 2023: previously governing SPD + Green + Linke
    wahl == 31 & party %in% c(2, 6, 5) ~ 1,
    # Landtagswahl Brandenburg 2024: previously governing SPD + Green + CDU
    wahl == 131 & party %in% c(2, 6, 1) ~ 1,
    # Landtagswahl Thüringen 2024: previously governing SPD + Green + Linke
    wahl == 151 & party %in% c(2, 6, 5) ~ 1,
    # Landtagswahl Thüringen 2024: previously governing SPD + Green + CDU
    wahl == 151 & party %in% c(2, 6, 1) ~ 1,
    is.na(wahl) ~ NA,
    TRUE ~ 0))

## political position variables (distance from center and distacne from other parties)
svy_2025_ltw_13 <- svy_2025_ltw_12 %>%
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

svy_2025_ltw_14 <- svy_2025_ltw_13 %>%
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
svy_2025_ltw_14 %>% dplyr::select(WE03_06_rev, WE03_15, WE04_06) %>% psych::alpha() %>% summary()
svy_2025_ltw_14 %>% dplyr::select(WE03_04_rev, WE03_13_rev, WE04_04, WE04_11) %>% psych::alpha() %>% summary()
svy_2025_ltw_14 %>% dplyr::select(WE03_03, WE03_12_rev, WE04_03, WE04_10_rev) %>% psych::alpha() %>% summary()
svy_2025_ltw_14 %>% dplyr::select(WE03_07, WE03_16_rev, WE04_07_rev, WE04_14) %>% psych::alpha() %>% summary()
svy_2025_ltw_14 %>% dplyr::select(WE03_02, WE03_11_rev, WE04_02, WE04_09) %>% psych::alpha() %>% summary()

## Campaign dynamics ####

svy_2025_ltw_15 <- svy_2025_ltw_14 %>%
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

svy_2025_ltw_16 <- svy_2025_ltw_15 %>%
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

svy_2025_ltw_16[vars_to_scale] <- lapply(svy_2025_ltw_16[vars_to_scale], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) 
})

## Export file ####

svy_2025_ltw_17 <- svy_2025_ltw_16 %>% 
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


# Old data ----


#INSP_fb_ltw_bycand_old

#add metadata
INSP_fb_ltw_bycand_old <- INSP_fb_ltw_bycand_old %>%
right_join(., meta_ltw_old, by = c("cand_id"))

#make probability variables
INSP_fb_ltw_bycand_old <- INSP_fb_ltw_bycand_old %>%
  # proportion of attacks
  mutate(prop_attacks_fb = n_attacks / n_posts) %>%
  # keep only rows where prop_attacks_fb is not NA
  filter(!is.na(prop_attacks_fb)) %>%
  # participated and invited bins
  mutate(invited_bin = ifelse(invited %in% c(1, 2), 1, 0), 
         participated_bin = ifelse(participated %in% c(1, 2, 3), 1, 0))

#add survey data
joined_INSP_fb_ltw_bycand_old <- INSP_fb_ltw_bycand_old %>%
  full_join(., svy_ltw, by = c("ltw_id" = "SERIAL"))

#model
Model_fb_ltw_old <- lmerTest::lmer(
  prop_attacks_fb ~ 
    # political profile
    incumb + governing + lr_self + extr_self + lr_party + extr_party + lr_voters + extr_voters +
    # personality
    big5_extra + big5_agrea + big5_consc + big5_emot + big5_open +
    # social profile
    female + immigr + 
    # campaign dynamics
    race_chance + race_close + media_diffic + media_impact + camp_personal + 
    # controls
    age + (1 | election_id), data = joined_INSP_fb_ltw_bycand_old)
summary(Model_fb_ltw_old)  ### 563

# Current data ----
#add metadata
INSPEC2_fb_ltw_bycand_aktuell <- INSPEC2_fb_ltw_bycand_aktuell %>%
  right_join(., meta_ltw_aktuell, by = c("cand_id", "election_id"))

#make probability variables
# INSPEC2_fb_ltw_bycand_aktuell
INSPEC2_fb_ltw_bycand_aktuell <- INSPEC2_fb_ltw_bycand_aktuell %>%
  # proportion of attacks
  mutate(prop_attacks_fb = n_attacks / n_posts) %>%
  # keep only rows where prop_attacks_fb is not NA
  filter(!is.na(prop_attacks_fb)) %>%
  # participated and invited bins
  mutate(invited_bin = ifelse(invited %in% c(1, 2), 1, 0), 
         participated_bin = ifelse(participated %in% c(1, 2, 3), 1, 0))

#add survey data
joined_INSPEC2_fb_ltw_bycand_aktuell <- INSPEC2_fb_ltw_bycand_aktuell %>%
  left_join(., svy_2025_ltw_17, by =  "SERIAL") # N= 2497

#model
Model_fb_ltw_aktuell <- lmerTest::lmer(
  prop_attacks_fb ~ 
    # political profile
    incumb + governing + lr_self + extr_self + lr_party + extr_party + lr_voters + extr_voters +
    # personality
    big5_extra + big5_agrea + big5_consc + big5_emot + big5_open +
    # social profile
    female + immigr + 
    # campaign dynamics
    race_chance + race_close + media_diffic + media_impact + camp_personal + 
    # controls
    age + (1 | election_id), data = joined_INSPEC2_fb_ltw_bycand_aktuell)
summary(Model_fb_ltw_aktuell)  ### 583