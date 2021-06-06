setwd("/Users/mz/Box/myBox/orlando")

library(tidyverse)
library(broom)
library(estimatr)
library(xtable)
library(magrittr)
library(texreg)

load("data/output.RData")

# names & fun ----
varnames <- c(
  "Gun control legislation support",
  "Full sample",
  "Narrow time window",
  "Proximity",
  "Reasonable duration",
  "Narrow time window \\(\\cap\\) Reasonable duration",
  "Proximity \\(\\cap\\) Reasonable duration",
  "Constant",
  "Post-shooting",
  "Female",
  "Parent",
  "Political interest",
  "Newseveryday",
  "Ideology",
  "Political knowledge"
)
xtablecolnames <- c(
  "\\(\\hat{\\beta}\\)", "SE", "\\(p\\)-value", "\\(N\\)"
)
regtab <- function(regout, filename, label, caption, footnote) {
  texreg(
    l = regout, filename, label = label,
    custom.coef.name = varnames[8:15], reorder.coef = c(2:8, 1),
    custom.model.names = c("Bivariate", "Full"),
    custom.note = footnote,
    caption = caption,
    include.dispersion = F, include.aic = F, include.bic = F, include.loglik = F,
    custom.gof.names = c("Deviance", "\\(N\\)"), digits = 3,
    caption.above = T, dcolumn = T, booktabs = T, threeparttable = T, use.packages = F
  )
}

# atab 1 ----
# number of participants by date
ft <- paste(
  "\\bottomrule\n",
  "\\multicolumn{3}{p{0.7\\linewidth}}{\\scriptsize",
  "Note:",
  "2016-06-12, when the Orlando shooting happened, is excluded from our analysis and this table.",
  "Observations without date information or survey weight are dropped as well.",
  "Some participants completed the June survey in July and they are counted.",
  "}\n"
)
datetab %>% data.frame() %>% rownames_to_column() %>% {.[, c(2:4)]} %>%
  set_colnames(c("Date", "Number of Participants", "Cumulative Percentage")) %>%
  xtable(
    align = c("l", "c", "c", "c"), digits = c(0, 0, 0, 3),
    caption = "Number of TAPS wave 55 (June 2016) participants by date",
    label = "atab1"
  ) %>%
  print.xtable(
    file = "tab/atab1.tex",
    include.rownames = F,
    add.to.row = list(pos = list(nrow(datetab)), command = ft),
    hline.after = c(-1, 0),
    table.placement = "!htbp", caption.placement = "top", booktabs = T
  )

# atab 2 ----
# sum stats
sumstats %>% {.[, c("min", "median", "max", "mean", "sd", "n")]} %>%
  set_colnames(c("Min", "Median", "Max", "Mean", "SD", "\\(N\\)")) %>%
  set_rownames(varnames[c(1, 9, 3:5, 10:15)]) %>%
  xtable(
    align = c("l", rep("c", 6)), digits = c(rep(0, 4), rep(3, 2), 0),
    caption = "Summary statistics of variables", 
    label = "atab2"
  ) %>%
  print.xtable(
    file = "tab/atab2.tex",
    sanitize.text.function = function(x) {x},
    caption.placement = "top", booktabs = T, table.placement = "htbp!"
  )

# atab 3 ----
covbal_out %>% map(tidy) %>% map(select, estimate, std.error, p.value, df) %>%
  map(mutate, n = df, .keep = "unused") %>% unlist %>%
  matrix(
    ncol = 4, byrow = T,
    dimnames = list(varnames[10:length(varnames)], xtablecolnames)
  ) %>%
  xtable(
    align = c("l", rep("c", 4)), digits = c(0, rep(3, 3), 0),
    caption = "Covariate balance between \\emph{Pre-shooting} and \\emph{Post-shooting}, difference-in-means results",
    label = "atab3"
  ) %>%
  print.xtable(
    file = "tab/atab3.tex",
    include.rownames = T,
    sanitize.text.function = function(x) {x},
    table.placement = "!htbp", caption.placement = "top", booktabs = T
  )

# atab 4 ----
regtab(
  list(hasatt_reg_out_base, hasatt_reg_out_full), 
  "tab/atab4.tex", "atab4", 
  "Having opinion, gun control legislation support or not, linear probability model results",
  "\n\\item Note: Standard errors in parentheses. %stars.\n"
  )

# tab 1 ----
# pect of supporters before and after the shooting, descriptive
ft <- paste(
  "\\bottomrule\n",
  "\\multicolumn{2}{p{0.8\\linewidth}}{\\scriptsize",
  "Note:",
  "See Appendix Table 5 for their sample sizes.",
  "}\n"
)
pecttab_out %>% unlist %>% matrix(ncol = 2, byrow = T) %>%
  set_colnames(c("Pre-shooting", "Post-shooting")) %>%
  set_rownames(varnames[2:7]) %>%
  xtable(
    digits = c(0, 3, 3), align = "lcc",
    caption = "Percentage of gun control legislation supporters before and after the 2016 Orlando shooting", 
    label = "tab1"
  ) %>%
  print.xtable(
    file = "tab/tab1.tex",
    include.rownames = T,
    sanitize.text.function = function(x) {x},
    add.to.row = list(pos = list(6), command = ft),
    hline.after = c(-1, 0),
    table.placement = "!htbp", caption.placement = "top", booktabs = T
  )

# atab 5 ----
ft <- paste(
  "\\bottomrule\n",
  "\\multicolumn{2}{p{0.7\\linewidth}}{\\scriptsize",
  "Note:",
  "\\(N\\) reflects the sample size in the bivariate specification case.",
  "}\n"
)
samplesize_out %>% unlist %>% 
  matrix(ncol = 1) %>%
  set_colnames("\\(N\\)") %>% set_rownames(varnames[2:7]) %>%  
  xtable(
    digits = c(0, 0), align = "lc",
    caption = "Sizes of samples", 
    label = "atab5"
  ) %>%
  print.xtable(
    file = "tab/atab5.tex",
    include.rownames = T,
    sanitize.text.function = function(x) {x},
    add.to.row = list(pos = list(6), command = ft),
    hline.after = c(-1, 0),
    table.placement = "!htbp", caption.placement = "top", booktabs = T
  )

# atab 6-17 ----
captions <- c(
  "The 2016 Orlando shooting and gun control legislation support, logit model results, \\emph{Full} sample",
  "The 2016 Orlando shooting and gun control legislation support, logit model results, \\emph{Narrow time window}",
  "The 2016 Orlando shooting and gun control legislation support, logit model results, \\emph{Proximity}",
  "The 2016 Orlando shooting and gun control legislation support, logit model results, \\emph{Reasonable duration}",
  "The 2016 Orlando shooting and gun control legislation support, logit model results, \\emph{Narrow time window} \\(\\cap\\) \\emph{Reasonable duration}",
  "The 2016 Orlando shooting and gun control legislation support, logit model results, \\emph{Proximity} \\(\\cap\\) \\emph{Reasonable duration}",
  "The 2016 Orlando shooting and gun control legislation support, linear probability model results, \\emph{Full} sample",
  "The 2016 Orlando shooting and gun control legislation support, linear probability model results, \\emph{Narrow time window}",
  "The 2016 Orlando shooting and gun control legislation support, linear probability model results, \\emph{Proximity}",
  "The 2016 Orlando shooting and gun control legislation support, linear probability model results, \\emph{Reasonable duration}",
  "The 2016 Orlando shooting and gun control legislation support, linear probability model results, \\emph{Proximity} \\(\\cap\\) \\emph{Reasonable duration}",
  "The 2016 Orlando shooting and gun control legislation support, linear probability model results, \\emph{Narrow time window} \\(\\cap\\) \\emph{Reasonable duration}"
)
ft <- c(
  "\n\\item Note: Standard errors in parentheses. %stars.\n",
  "\n\\item Note: Maximum likelihood estimation. Standard errors in parentheses. %stars.\n"
)
filenames <- paste("tab/atab", 6:17, ".tex", sep = "")
labels <- paste("atab", 6:17, sep = "")
for (i in 1:length(logitout_full)) {
  regtab(
    c(logitout_base[i], logitout_full[i]), 
    filenames[i], labels[i], caption = captions[i], ft[1]
  )
  regtab(
    c(lpmout_base[i], lpmout_full[i]), 
    filenames[i + length(logitout_full)], labels[i + length(logitout_full)], caption = captions[i + length(logitout_full)], ft[2]
  )
  rm(i)
}

# atab 18 ----
groupnames <- c(
  "Females",
  "Males",
  "Parents",
  "Non-parents",
  "Political interest: Yes",
  "Political interest: No",
  "News everyday: Yes",
  "News everyday: No",
  "Liberals",
  "Conservatives",
  "Political knowledge: Yes",
  "Political knowledge: No"
)
samplesize <- c()
for (i in 1:length(eachgrouplpmreg_out)) {
  samplesize[i] <- length(eachgrouplpmreg_out[[i]]$residuals)
  rm(i)
}
eachgrouplpmreg_out %<>% map(tidy) %>% map(slice, 2) %>% map(select, estimate, std.error, p.value)
for (i in 1:length(eachgrouplpmreg_out)) {
  eachgrouplpmreg_out[[i]] %<>% mutate(n = samplesize[i]) 
  rm(i)
}
eachgrouplpmreg_out %>% unlist %>%
  matrix(
    ncol = 4, byrow = T,
    dimnames = list(groupnames, xtablecolnames)
  ) %>%
  xtable(
    digits = c(0, rep(3, 3), 0), align = "lcccr",
    caption = "The 2016 Orlando shooting and gun control legislation support, bivariate linear probability models running on different subgroups defined by personal characteristics",
    label = "atab18"
  ) %>%
  print.xtable(
    file = "tab/atab18.tex",
    include.rownames = T,
    sanitize.text.function = function(x) {x}, math.style.negative = T,
    caption.placement = "top", table.placement = "!htbp", booktabs = T
  )
