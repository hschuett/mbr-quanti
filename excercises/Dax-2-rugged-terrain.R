# Imports ---------------------------------------------------------------------------
library(tidyverse)
library(stargazer)
skim <- skimr::skim


# Load Data -------------------------------------------------------------------------
# The paper
# https://scholar.harvard.edu/nunn/publications/ruggedness-blessing-bad-geography-africa
# Data description
# https://diegopuga.org/data/rugged/
rugg <- read.csv("data/rugged.csv", sep=";")
str(rugg)


# Inspect Data ----------------------------------------------------------------------
rugg$lgdppc2000 <- log(rugg$rgdppc_2000)

rugg <- rugg %>%
  mutate(lgdppc2000 = log(rgdppc_2000),
         diamond = (gemstones *10) /(land_area)) %>%
  filter(is.na(lgdppc2000) == F)
# rugg$diamond = scale(rugg$diamond)


# Descriptives --------------------------------------------------------------------------------
# A few alternatives
rugg %>%
  select(lgdppc2000, rugged, cont_africa, gemstones,
         soil, tropical, dist_coast) %>%
  summary()

skim(rugg)

stargazer(rugg,
          type="text",
          summary.stat = c("n", "mean", "sd", "p25", "median" ,"p75"),
          out="descrug.tex")

# Tests -----------------------------------------------------------------------------
tab1_1 <- lm(lgdppc2000 ~ rugged*cont_africa, data=rugg)
tab1_2 <- lm(lgdppc2000 ~ rugged*cont_africa + cont_africa*diamond, data=rugg)
tab1_3 <- lm(lgdppc2000 ~ rugged*cont_africa + cont_africa*soil, data=rugg)
tab1_4 <- lm(lgdppc2000 ~ rugged*cont_africa + cont_africa*tropical, data=rugg)
tab1_5 <- lm(lgdppc2000 ~ rugged*cont_africa + cont_africa*dist_coast, data=rugg)
tab1_6 <- lm(lgdppc2000 ~ rugged*cont_africa + cont_africa*diamond
             + rugged*soil + rugged*tropical + cont_africa*dist_coast, data=rugg)
stargazer(tab1_1, tab1_2, tab1_3, tab1_4, tab1_5, tab1_6,
          type="text", omit.stat=c("f", "ser"))


# Adding Slave Exports ------------------------------------------------------------------------

rugg <- rugg %>%
  mutate(SlaveExp = log(slave_exports + 1))

tab1_1 <- lm(lgdppc2000 ~ SlaveExp + rugged*cont_africa, data=rugg)
tab1_2 <- lm(lgdppc2000 ~ SlaveExp + rugged*cont_africa + cont_africa*diamond, data=rugg)
tab1_3 <- lm(lgdppc2000 ~ SlaveExp + rugged*cont_africa + cont_africa*soil, data=rugg)
tab1_4 <- lm(lgdppc2000 ~ SlaveExp + rugged*cont_africa + cont_africa*tropical, data=rugg)
tab1_5 <- lm(lgdppc2000 ~ SlaveExp + rugged*cont_africa + cont_africa*dist_coast, data=rugg)
tab1_6 <- lm(lgdppc2000 ~ SlaveExp + rugged*cont_africa + cont_africa*diamond
             + cont_africa*soil + cont_africa*tropical + cont_africa*dist_coast, data=rugg)
stargazer(tab1_1, tab1_2, tab1_3, tab1_4, tab1_5, tab1_6,
          type="text", omit.stat=c("f", "ser"))





