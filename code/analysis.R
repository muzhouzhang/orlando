setwd("/Users/mz/Box/myBox/orlando")

library(tidyverse)
library(lubridate)
library(haven)
library(psych)
library(estimatr)
library(survey)
library(msm)
library(rlang)

# process data ----
data <- 
  read_spss("data/taps2016.sav") %>%
  transmute(
    svyweights = jun2016wt1S55,
    date = as.Date(tm_finishS55 / 86400, origin = "1582-10-14"),
    postevent = case_when(date >= ymd("2016-06-13") ~ 1, TRUE ~ 0),
    reasonable_duration = case_when(durationS55 %in% seq(15:60) ~ 1, TRUE ~ 0),
    narrow_timewindow = case_when(between(date, ymd("2016-06-11"), ymd("2016-06-14")) ~ 1, TRUE ~ 0),
    # 58 = Georgia, 59 = Florida, 63 = Alabama, verified by senators' names (Senator_1_InsertS55 and Senator_2_InsertS55)
    proximity = case_when(XPPSTATES55 %in% c(58, 59, 64) ~ 1, TRUE ~ 0), 
    hasatt = case_when(ISSUESA4GS55 == 4 ~ 0, ISSUESA4GS55 %in% c(2, 3) ~ 1),
    gunctrlatt = case_when(
      ISSUESA4GS55 == 3 ~ 0, # oppose
      ISSUESA4GS55 == 2 ~ 1 # support
    ),
    female = case_when(SEXS55 == 3 ~ 1, SEXS55 == 2 ~ 0),
    parent = case_when(CHILD1S55 == 2 ~ 1, CHILD1S55 == 3 ~ 0),
    polinterest = case_when(
      INTERESTPOLS55 == 5 ~ 1, # not at all
      INTERESTPOLS55 == 4 ~ 2, # slightly
      INTERESTPOLS55 == 3 ~ 3, # somewhat
      INTERESTPOLS55 == 2 ~ 4 # very
      # refused (1) dropped
    ),
    polinterest_binary = case_when(polinterest %in% c(3, 4) ~ 1, polinterest %in% c(1, 2) ~ 0),
    ideo = ifelse(LIBCON0S55 %in% c(1:6), LIBCON0S55, NA),
    liberal = case_when(ideo %in% c(4:6) ~ 1, ideo %in% c(1:3) ~ 0),
    newseveryday = case_when(NEWSFREQS55 == 2 ~ 1, NEWSFREQS55 %in% c(3:8) ~ 0), # refused (1) dropped
    polknow = case_when( 
      # senator term question
      POLKNOW3S55 == 4 ~ 1, 
      POLKNOW3S55 %in% c(1:3, 5:6) ~ 0 # including refused and dk
    )
  ) %>%
  filter(as.character(date) != "2016-06-12") %>%
  drop_na(svyweights)

# atab 1----
# number of survey participants by date
datetab <- 
  data %>% {table(.$date)} %>% data.frame() %>%
  mutate(cumperc = 100 * cumsum({.$Freq}) / sum({.$Freq}))

# atab 2 ----
# summary stats
sumstats <- 
  data %>% select(
    gunctrlatt, postevent, reasonable_duration, narrow_timewindow, proximity,
    female, parent, polinterest, newseveryday, ideo, polknow
  ) %>%
  describe() %>% data.frame() %>% {.[, c("min", "median", "max", "mean", "sd", "n")]}

# atab 3 ----
# covariate balance
covbal_formulae <- c(
  postevent ~ - 1 + female,
  postevent ~ - 1 + parent,
  postevent ~ - 1 + polinterest,
  postevent ~ - 1 + newseveryday,
  postevent ~ - 1 + ideo,
  postevent ~ - 1 + polknow
)
covbal_out <- map(covbal_formulae, lm_robust, data = data)

# subset expressions ----
subsetexprs <- 
  c(
    "postevent %in% c(0, 1)",
    "narrow_timewindow == 1",
    "proximity == 1",
    "reasonable_duration == 1",
    "narrow_timewindow & reasonable_duration == 1",
    "proximity == 1 & reasonable_duration == 1",
    "female == 1",
    "female == 0",
    "parent == 1",
    "parent == 0",
    "polinterest_binary == 1",
    "polinterest_binary == 0",
    "liberal == 1",
    "liberal == 0",
    "newseveryday == 1",
    "newseveryday == 0",
    "polknow == 1",
    "polknow == 0"
  ) %>% 
  parse_exprs()

# tab 1 ----
# pect of supporters before and after the shooting, descriptive
pecttab <- function(subset) {
  data %>% filter(!! subset) %>% {table(.$gunctrlatt, .$postevent)} %>% 
    prop.table(2) %>% {. * 100} %>% round(digits = 3) %>% as.matrix() %>% {.[2,]}
}
samplesize <- function(subset) {
  data %>% filter(!! subset) %>% drop_na(gunctrlatt) %>% nrow()
}
pecttab_out <- map(subsetexprs[1:6], pecttab)
samplesize_out <- map(subsetexprs[1:6], samplesize)

# atab 4 ----
# whether no opinion rate is unbalanced
hasatt_reg_out_base <- glm(
  hasatt ~ postevent,
  data = data, family = binomial (link = "logit")
)
hasatt_reg_out_full <- glm(
  hasatt ~ postevent + female + parent + polinterest + newseveryday + ideo + polknow,
  data = data, family = binomial (link = "logit")
)

# atab 5-17 & fig 1 ----
# model specification
basemod <- gunctrlatt ~ postevent
fullmod <- gunctrlatt ~ postevent + female + parent + polinterest + newseveryday + ideo + polknow

# lpm, atab 5-10
lpmreg <- function(subset, modspec) {
  data %>% filter(!! subset) %>%
    svyglm(
      modspec, data = ., family = gaussian(link = "identity"),
      design = svydesign(ids = ~ 1, data = ., weights = .$svyweights)
    )
}
lpmout_base <- map(subsetexprs[1:6], lpmreg, basemod)
lpmout_full <- map(subsetexprs[1:6], lpmreg, fullmod)

# logit, atab 11-16
logitreg <- function(subset, modspec) {
  data %>% filter(!! subset) %>%
    svyglm(
      modspec, data = ., family = binomial(link = "logit"),
      design = svydesign(ids = ~ 1, data = ., weights = .$svyweights),
    )
}
logitout_base <- map(subsetexprs[1:6], logitreg, basemod)
logitout_full <- map(subsetexprs[1:6], logitreg, fullmod)

# logit substantive effects, fig 1&2
cov_values <- data.frame(
  postevent = c(0, 1),
  female = median(data$female, na.rm = T),
  parent = median(data$parent, na.rm = T),
  polinterest = median(data$polinterest, na.rm = T),
  newseveryday = median(data$newseveryday, na.rm = T),
  ideo = median(data$ideo, na.rm = T),
  polknow = median(data$polknow, na.rm = T)
)
probs_fig1 <- map(logitout_base, predict, newdata = cov_values[1], type = "response")
probs_fig2 <- map(logitout_full, predict, newdata = cov_values, type = "response")
vcov_fig1 <- map(logitout_base, predict, newdata = cov_values[1], type = "response", vcov = T)
vcov_fig2 <- map(logitout_full, predict, newdata = cov_values, type = "response", vcov = T)
fd_fig1 <- NULL
ses_fig1 <- NULL
for (i in 1:length(logitout_full)) { # map(·) doesn't work here?
  fd_fig1[[i]] <- probs_fig1[[i]][2] - probs_fig1[[i]][1]
  vcov_fig1[[i]] <- vcov(vcov_fig1[[i]])
  ses_fig1[[i]] <- deltamethod(~ x2 - x1, probs_fig1[[i]], vcov_fig1[[i]])
  rm(i)
}
fig1_dat <- tibble(
  fd_fig1 = unlist(fd_fig1),
  ses_fig1 = unlist(ses_fig1),
  cilow95 = fd_fig1 - qnorm(0.975) * ses_fig1,
  cihigh95 = fd_fig1 + qnorm(0.975) * ses_fig1,
  cilow90 = fd_fig1 - qnorm(0.950) * ses_fig1,
  cihigh90 = fd_fig1 + qnorm(0.950) * ses_fig1
)
fd_fig2 <- NULL
ses_fig2 <- NULL
for (i in 1:length(logitout_full)) { # map(·) doesn't work here?
  fd_fig2[[i]] <- probs_fig2[[i]][2] - probs_fig2[[i]][1]
  vcov_fig2[[i]] <- vcov(vcov_fig2[[i]])
  ses_fig2[[i]] <- deltamethod(~ x2 - x1, probs_fig2[[i]], vcov_fig2[[i]])
  rm(i)
}
fig2_dat <- tibble(
  fd_fig2 = unlist(fd_fig2),
  ses_fig2 = unlist(ses_fig2),
  cilow95 = fd_fig2 - qnorm(0.975) * ses_fig2,
  cihigh95 = fd_fig2 + qnorm(0.975) * ses_fig2,
  cilow90 = fd_fig2 - qnorm(0.950) * ses_fig2,
  cihigh90 = fd_fig2 + qnorm(0.950) * ses_fig2
)

# lpm on each group, atab 17
eachgrouplpmreg <- function(subset) {
  data %>% filter(!! subset) %>%
    svyglm(
      basemod, data = ., family = gaussian(link = "identity"),
      design = svydesign(ids = ~ 1, data = ., weights = .$svyweights)
    )
}
eachgrouplpmreg_out <- map(subsetexprs[7:length(subsetexprs)], eachgrouplpmreg)

# output ----
save.image("data/output.RData")
