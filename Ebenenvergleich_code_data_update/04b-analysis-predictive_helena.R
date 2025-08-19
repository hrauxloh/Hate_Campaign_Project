
# SETUP ========================================================================

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(stargazer)

# MODELING =====================================================================

# variables as in Maier & Nai 2023 (insofar as available)

######################These reuslts are on politician level 

## LTW ####


###################################facebook attacks
mod_fb_ltw <- lmerTest::lmer(
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
    age + (1 | election_id), data = joined_ltw) #### changed data = ltw to data = joined_ltw

class(mod_fb_ltw) <- "lmerMod"
stargazer(mod_fb_ltw, type = "text",  out = "replication_mehrebenen_paper/mod_fb_ltw.doc")

###################################twitter attacks

mod_tw_ltw <- lmerTest::lmer(
  prop_attacks_tw ~ 
    # political profile
    incumb + governing + lr_self + extr_self + lr_party + extr_party + lr_voters + extr_voters +
    # personality
    big5_extra + big5_agrea + big5_consc + big5_emot + big5_open +
    # social profile
    female + immigr + 
    # campaign dynamics
    race_chance + race_close + media_diffic + media_impact + camp_personal + 
    # controls
    age + (1 | election_id), data = joined_ltw) #### changed data = ltw to data = joined_ltw

class(mod_tw_ltw) <- "lmerMod"
stargazer(mod_tw_ltw, type = "text",  out = "replication_mehrebenen_paper/mod_tw_ltw.doc")

## BTW ####
###################################facebook attacks
mod_fb_btw <- lmerTest::lmer(
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
    age + (1 | election_id), data = joined_btw) #### changed data = btw to data = joined_btw

class(mod_fb_btw) <- "lmerMod"
stargazer(mod_fb_btw, type = "text",  out = "replication_mehrebenen_paper/mod_fb_btw.doc")

###################################twitter attacks

mod_tw_btw <- lmerTest::lmer(
  prop_attacks_tw ~ 
    # political profile
    incumb + governing + lr_self + extr_self + lr_party + extr_party + lr_voters + extr_voters +
    # personality
    big5_extra + big5_agrea + big5_consc + big5_emot + big5_open +
    # social profile
    female + immigr + 
    # campaign dynamics
    race_chance + race_close + media_diffic + media_impact + camp_personal + 
    # controls
    age + (1 | election_id), data = joined_btw) #### changed data = btw to data = joined_btw

class(mod_tw_btw) <- "lmerMod"
stargazer(mod_tw_btw, type = "text",  out = "replication_mehrebenen_paper/mod_tw_btw.doc")

