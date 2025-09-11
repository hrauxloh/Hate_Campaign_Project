
# SETUP ========================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(lme4)
library(MASS)
library(lmerTest)
library(broom.mixed)
library(stargazer)
library(texreg)

library(sjPlot)

library(dplyr)
library(tidyr)
# MODELING =====================================================================

# variables as in Maier & Nai 2023 (insofar as available)
# joined_ltw <- read_csv("replication_mehrebenen_paper/joined_ltw.csv")
# joined_btw <- read_csv("replication_mehrebenen_paper/svy_btw.csv")

# OLS on probability of attacks ================================================

## LTW ####

################################### Facebook attacks
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

mod_fb_ltw_update <- lmerTest::lmer(
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
    age + (1 | election_id), data = joined_ltw_wip)
# Comparison of models mod_fb_ltw and mod_fb_ltw_update



## 1. Side-by-side coefficient tables
tab_model(mod_fb_ltw, mod_fb_ltw_update, 
          show.ci = TRUE, 
          show.se = TRUE,
          show.aic = TRUE, 
          dv.labels = c("Model 1: Original", "Model 2: Update"))

## 2. Compare model fit statistics
# Likelihood ratio test (only valid if models are nested)
#anova(mod_fb_ltw, mod_fb_ltw_update)

# AIC and BIC comparison (lower is better)
AIC(mod_fb_ltw, mod_fb_ltw_update)
BIC(mod_fb_ltw, mod_fb_ltw_update)

## 3. Side-by-side fixed effects estimates
coef_compare <- broom.mixed::tidy(mod_fb_ltw) %>% 
  select(term, estimate, std.error) %>%
  mutate(model = "Full") %>%
  bind_rows(
    broom.mixed::tidy(mod_fb_ltw_update) %>%
      select(term, estimate, std.error) %>%
      mutate(model = "Reduced")
  ) %>%
  pivot_wider(names_from = model, values_from = c(estimate, std.error))

print(coef_compare)

## 4. Visual comparison of effects
plot_models(mod_fb_ltw, mod_fb_ltw_update, 
            std.est = "std2", 
            show.values = TRUE, 
            title = "Comparison of Models")

# class(mod_fb_ltw) <- "lmerMod"
#stargazer(mod_fb_ltw, type = "text",  out = "replication_mehrebenen_paper/mod_fb_ltw.doc")

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


mod_tw_ltw_update <- lmerTest::lmer(
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
    age + (1 | election_id), data = joined_ltw_wip) #### changed data = ltw to data = joined_ltw
class(mod_tw_ltw) <- "lmerMod"
stargazer(mod_tw_ltw, type = "text",  out = "replication_mehrebenen_paper/mod_tw_ltw.doc")



## BTW ####

###################################facebook attacks
mod_fb_btw <- #lmerTest::lmer(
  lm(
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
    age, data = joined_btw)  #+ (1 | election_id)

#class(mod_fb_btw) <- "lmerMod"
stargazer(mod_fb_btw, type = "text",  out = "replication_mehrebenen_paper/mod_fb_btw.doc")

###################################twitter attacks
mod_tw_btw <-# lmerTest::lmer(
  lm(
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
    age , data = joined_btw) # + (1 | election_id)

stargazer(mod_tw_btw, type = "text",  out = "replication_mehrebenen_paper/mod_tw_btw.doc")

## Print all ####
stargazer(list(
  mod_fb_ltw,
  mod_tw_ltw,
  mod_fb_btw, 
  mod_tw_btw), 
  type = "text",  out = "replication_mehrebenen_paper/mod_all.doc")



# Neg bin on count of attacks =====================================================================

## LTW ####

###################################facebook attacks
mod_fb_ltw_negbin <- glmer.nb(
  n_attacks_fb ~ 
    # political profile
    incumb + governing + lr_self + extr_self + lr_party + extr_party + lr_voters + extr_voters +
    # personality
    big5_extra + big5_agrea + big5_consc + big5_emot + big5_open +
    # social profile
    female + immigr + 
    # campaign dynamics
    race_chance + race_close + media_diffic + media_impact + camp_personal + 
    # activity
    log(n_posts) + 
    # controls
    age + (1 | election_id), data = joined_ltw) #### changed data = ltw to data = joined_ltw

###################################twitter attacks

mod_tw_ltw_negbin <- glmer.nb(
  n_attacks_tw ~ 
    # political profile
    incumb + governing + lr_self + extr_self + lr_party + extr_party + lr_voters + extr_voters +
    # personality
    big5_extra + big5_agrea + big5_consc + big5_emot + big5_open +
    # social profile
    female + immigr + 
    # campaign dynamics
    race_chance + race_close + media_diffic + media_impact + camp_personal + 
    # activity
    log(n_tweets) + 
    # controls
    age + (1 | election_id), data = joined_ltw) #### changed data = ltw to data = joined_ltw


## BTW ####

###################################facebook attacks
mod_fb_btw_negbin <- #lmerTest::lmer(
  glm.nb(
    n_attacks_fb ~ 
      # political profile
      incumb + governing + lr_self + extr_self + lr_party + extr_party + lr_voters + extr_voters +
      # personality
      big5_extra + big5_agrea + big5_consc + big5_emot + big5_open +
      # social profile
      female + immigr + 
      # campaign dynamics
      race_chance + race_close + media_diffic + media_impact + camp_personal + 
      # activity
      log(n_posts) + 
      # controls
      age, data = joined_btw)  #+ (1 | election_id)


###################################twitter attacks
mod_tw_btw_negbin <-# lmerTest::lmer(
  glm.nb(
    n_attacks_tw ~ 
      # political profile
      incumb + governing + lr_self + extr_self + lr_party + extr_party + lr_voters + extr_voters +
      # personality
      big5_extra + big5_agrea + big5_consc + big5_emot + big5_open +
      # social profile
      female + immigr + 
      # campaign dynamics
      race_chance + race_close + media_diffic + media_impact + camp_personal + 
      # activity
      log(n_tweets) + 
      # controls
      age , data = joined_btw) # + (1 | election_id)

## Print all ####
htmlreg(list(
  mod_fb_ltw, mod_fb_ltw_update,
  mod_tw_ltw, mod_tw_ltw_update), file = "replication_mehrebenen_paper/mod_all_update.doc")


htmlreg(list(
  mod_fb_ltw, mod_fb_ltw_negbin,
  mod_fb_btw, mod_fb_btw_negbin,
  mod_tw_ltw, mod_tw_ltw_negbin,
  mod_tw_btw, mod_tw_btw_negbin), 
       custom.model.names = c("LTW Share", "LTW Count",
                              "BTW Share", "BTW Count",
                              "LTW Share", "LTW Count",
                              "BTW Share", "BTW Count"),
       custom.header	= list("Facebook" = 1:4, "Twitter" = 5:8),
       caption = "Models of negative campaigning",
       file = "replication_mehrebenen_paper/mod_all.doc",
       scalebox = 0.82,
       custom.coef.map = list(   "incumb" = "Incumbent",
                                 "governing" = "Government party",
                                 "lr_self" = "Ideology (own)",
                                 "extr_self" = "Extremism (own)",
                                 "lr_party" = "Ideology (party)",
                                 "extr_party" = "Extremism (party)",
                                 "lr_voters" = "Ideology (voters)",
                                 "extr_voters" = "Extremism (voters)",
                                 "big5_extra" = "Big 5: Extraversion",
                                 "big5_agrea" = "Big 5: Agreeableness",
                                 "big5_consc" = "Big 5: Conscientiousness",
                                 "big5_emot" = "Big 5: Neuroticism",
                                 "big5_open" = "Big 5: Openness",
                                 "race_chance" = "Race: chances",
                                 "race_close" = "Race: closeness",
                                 "media_diffic" = "Media: difficulty to get coverage",
                                 "media_impact" = "Media: Impact",
                                 "female" = "Gender: female",
                                 "age" = "Age",
                                 "immigr" = "Immigration background",
                                 "log(n_posts)" = "Total Facebook posts sent (logged)",
                                 "log(n_tweets)" = "Total tweets sent (logged)",
                                 "(Intercept)" = "Intercept"),
       caption.above = TRUE,
       custom.note = "%stars. Negative binomial regression models.",
       include.adjrs = TRUE, 
       include.nobs = TRUE,
       include.bic = FALSE,
       include.dispersion = FALSE,
       include.groups = TRUE)
