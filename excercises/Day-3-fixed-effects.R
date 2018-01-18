# RQ: What is the causal effect of market concentration on airfares?
#  1. Draw a graph and think of the theoretical connection
#  2. Load the dataset and check the data. What variables are in there?
#     What is in there that helps you identify the causal effect?
#   (id: Route)
#  3. Design a regression
#  4. Estimate the effect
#  5. Interpret
#  6. Is your estimate capturing the causal effect, or is still something missing?


library(tidyverse)
library(lfe)


# Load Data ---------------------------------------------------------------
d1 <- read.csv("data/airfare.csv")
summary(lm(lfare ~ lpassen, data=d1))

# Let them try distance too and answer why that won't work in a fixed effects setting
# conentration has the wrong sign without fixed effects...

summary(felm(lfare ~ lpassen + concen + ldist | id + year | 0 | 0, data=d1))
# Check frame to see that dist is fixed per id, so this is the id of
# the route. Distance does not change per rout.


# why does concentration work??
summary(felm(lfare ~ lpassen + concen | id + year | 0 | 0, data=d1))
# compare to OLS
summary(lm(lfare ~ lpassen + concen, data=d1))

# This explains a lot of stuff
# how important is year ?
summary(felm(lfare ~ lpassen + concen | id| 0 | 0, data=d1))
# seems mostly rout fixed effect?
summary(felm(lfare ~ lpassen + concen | year| 0 | 0, data=d1))
# yupp
# but this means, fixed effects is actually tricky here. Most of the variation in fares is route fixed.

# what happens if we leave leave firm-FE out and just use distance?
summary(felm(lfare ~ lpassen + concen + ldist + ldistsq | year| 0 | 0, data=d1))

# Question, if you want to know the "causal" effect of concentration, which estimator would you rather believe? Why?
