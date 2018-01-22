
# Package Import ----------------------------------------------------------
library(stargazer)
library(dplyr)
# setwd("GitRepos/QuantitativeMethods/")
# Data Import -------------------------------------------------------------
data_0 <- read.table("data/njmin/public.dat", quote="\"", comment.char="", na.strings=".", stringsAsFactors=FALSE)
str(data_0)

# SHEET           1        3     3.0   sheet number (unique store id)
# CHAIN           5        5     1.0   chain 1=bk; 2=kfc; 3=roys; 4=wendys
# CO_OWNED        7        7     1.0   1 if company owned
# STATE           9        9     1.0   1 if NJ; 0 if Pa
#
# Dummies for location:
# SOUTHJ         11       11     1.0   1 if in southern NJ
# CENTRALJ       13       13     1.0   1 if in central NJ
# NORTHJ         15       15     1.0   1 if in northern NJ
# PA1            17       17     1.0   1 if in PA, northeast suburbs of Phila
# PA2            19       19     1.0   1 if in PA, Easton etc
# SHORE          21       21     1.0   1 if on NJ shore
#

# First Interview
# NCALLS         23       24     2.0   number of call-backs*
# EMPFT          26       30     5.2   # full-time employees
# EMPPT          32       36     5.2   # part-time employees
# NMGRS          38       42     5.2   # managers/ass't managers
# WAGE_ST        44       48     5.2   starting wage ($/hr)
# INCTIME        50       54     5.1   months to usual first raise
# FIRSTINC       56       60     5.2   usual amount of first raise ($/hr)
# BONUS          62       62     1.0   1 if cash bounty for new workers
# PCTAFF         64       68     5.1   % employees affected by new minimum
# MEALS          70       70     1.0   free/reduced price code (See below)
# OPEN           72       76     5.2   hour of opening
# HRSOPEN        78       82     5.2   number hrs open per day
# PSODA          84       88     5.2   price of medium soda, including tax
# PFRY           90       94     5.2   price of small fries, including tax
# PENTREE        96      100     5.2   price of entree, including tax
# NREGS         102      103     2.0   number of cash registers in store
# NREGS11       105      106     2.0   number of registers open at 11:00 am
#
# Second Interview
# TYPE2         108      108     1.0   type 2nd interview 1=phone; 2=personal
# STATUS2       110      110     1.0   status of second interview: see below
# DATE2         112      117     6.0   date of second interview MMDDYY format
# NCALLS2       119      120     2.0   number of call-backs*
# EMPFT2        122      126     5.2   # full-time employees
# EMPPT2        128      132     5.2   # part-time employees
# NMGRS2        134      138     5.2   # managers/ass't managers
# WAGE_ST2      140      144     5.2   starting wage ($/hr)
# INCTIME2      146      150     5.1   months to usual first raise
# FIRSTIN2      152      156     5.2   usual amount of first raise ($/hr)
# SPECIAL2      158      158     1.0   1 if special program for new workers
# MEALS2        160      160     1.0   free/reduced price code (See below)
# OPEN2R        162      166     5.2   hour of opening
# HRSOPEN2      168      172     5.2   number hrs open per day
# PSODA2        174      178     5.2   price of medium soda, including tax
# PFRY2         180      184     5.2   price of small fries, including tax
# PENTREE2      186      190     5.2   price of entree, including tax
# NREGS2        192      193     2.0   number of cash registers in store
# NREGS112      195      196     2.0   number of registers open at 11:00 am
# Codes:
#
#   Free/reduced Meal Variable:
#   0 = none
# 1 = free meals
# 2 = reduced price meals
# 3 = both free and reduced price meals
#
#
# Second Interview Status
# 0 = refused second interview (count = 1)
# 1 = answered 2nd interview (count = 399)
# 2 = closed for renovations (count = 2)
# 3 = closed "permanently" (count = 6)
# 4 = closed for highway construction (count = 1)
# 5 = closed due to Mall fire (count = 1)
#
#
# *Note: number of call-backs = 0 if contacted on first call

var_names <- c(
"SHEET",
"CHAIN",
"CO_OWNED",
"STATE",
"SOUTHJ",
"CENTRALJ",
"NORTHJ",
"PA1",
"PA2",
"SHORE",
"NCALLS",
"EMPFT",
"EMPPT",
"NMGRS",
"WAGE_ST",
"INCTIME",
"FIRSTINC",
"BONUS",
"PCTAFF",
"MEALS",
"OPEN",
"HRSOPEN",
"PSODA",
"PFRY",
"PENTREE",
"NREGS",
"NREGS11",
"TYPE2",
"STATUS2",
"DATE2",
"NCALLS2",
"EMPFT2",
"EMPPT2",
"NMGRS2",
"WAGE_ST2",
"INCTIME2",
"FIRSTIN2",
"SPECIAL2",
"MEALS2",
"OPEN2R",
"HRSOPEN2",
"PSODA2",
"PFRY2",
"PENTREE2",
"NREGS2",
"NREGS112")

colnames(data_0) <- var_names



# Replicating Key Table3 and Table4 from SAS code -------------------------

# %>% take what's on the left an pipe it to the next step
data_1 <- data_0

# data_1$FTE <- data_1$EMPPT*.5 + data_1$EMPFT + data_1$NMGRS
# data_1$FTE <- 2

data_1 <- data_0  %>%
  mutate(FTE = EMPPT*.5 + EMPFT + NMGRS,
         FTE2 = EMPPT2*.5 + EMPFT2 + NMGRS2) %>%
  mutate(DEMP = FTE2 - FTE) %>%
  mutate(DWAGE = WAGE_ST2 - WAGE_ST) %>%
  mutate(CLOSED = if_else(STATUS2 == 3, 1, 0)) %>%
  rename(NJ = STATE) %>%
  mutate(BK   = if_else(CHAIN == 1, 1, 0),
         KFC  = if_else(CHAIN == 2, 1, 0),
         ROYS = if_else(CHAIN == 3, 1, 0)) %>%
  mutate(GAP  = if_else(NJ == 0, 0, case_when(WAGE_ST >= 5.05 ~ 0,
                                              WAGE_ST >= 0 ~ (5.05 - WAGE_ST)/WAGE_ST,
                                              TRUE ~ NA_real_)))

table3_data <- data_1 %>%
  mutate(grp = if_else(NJ == 0, "PA", "NJ")) %>%
  group_by(grp) %>%
  summarize(FTE = round(mean(FTE, na.rm=TRUE),2),
            FTE2 = round(mean(FTE2, na.rm=TRUE),2))
table3_data

summary(lm(DEMP ~ NJ, data=data_1))

table_4_data <- data_1 %>%
  filter(is.na(DEMP) == FALSE) %>%
  filter(CLOSED == 1 | (CLOSED == 0 & is.na(DWAGE) == FALSE))
tab4_col1 <- lm(DEMP ~ NJ, data=table_4_data)
tab4_col2 <- lm(DEMP ~ NJ + BK + KFC + ROYS + CO_OWNED, data=table_4_data)
tab4_col3 <- lm(DEMP ~ GAP, data=table_4_data)
tab4_col4 <- lm(DEMP ~ GAP + BK + KFC + ROYS + CO_OWNED, data=table_4_data)
tab4_col5 <- lm(DEMP ~ GAP + BK + KFC + ROYS + CO_OWNED + CENTRALJ + SOUTHJ + PA1 + PA2, data=table_4_data)
stargazer(tab4_col1, tab4_col2, tab4_col3, tab4_col4, tab4_col5,
          type="text", omit.stat=c("f", "ser"))


# Diff-in-Diff Regression -------------------------------------------------
 ## table_4_data %>% select(FTE, NJ)
 # table_4_data[, c("FTE", "NJ")]
wave_1 <- table_4_data[ ,c("FTE", "NJ", "BK", "KFC", "ROYS", "CO_OWNED", "CENTRALJ", "SOUTHJ", "PA1", "PA2")]
wave_1$PERIOD <- "0"
wave_2 <- table_4_data[ ,c("FTE2", "NJ", "BK", "KFC", "ROYS", "CO_OWNED", "CENTRALJ", "SOUTHJ", "PA1", "PA2")]
colnames(wave_2) <- c("FTE", "NJ", "BK", "KFC", "ROYS", "CO_OWNED", "CENTRALJ", "SOUTHJ", "PA1", "PA2")
wave_2$PERIOD <- "1"
long_data <- rbind(wave_1, wave_2)

Atab4_col1 <- lm(FTE ~ NJ * PERIOD, data=long_data)
Atab4_col2 <- lm(FTE ~ NJ * PERIOD + BK + KFC + ROYS + CO_OWNED, data=long_data)  # not the correct comparison
Atab4_col3 <- lm(FTE ~ NJ * PERIOD + BK * PERIOD + KFC * PERIOD + ROYS * PERIOD + CO_OWNED * PERIOD, data=long_data)
stargazer(Atab4_col1, Atab4_col2, Atab4_col3, type="text", omit.stat=c("f", "ser"))

# Question: Why do you get different standard errors?
# changes and level residuals have different qualities. It depends which one is better behaved...
