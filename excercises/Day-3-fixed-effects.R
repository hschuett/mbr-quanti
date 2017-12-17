library(lfe)

d1 <- read.csv(airfare, "data/airfare.csv")
summary(lm(lfare ~ lpassen, data=d1))
# let themm try distace too and answer why that won't work in a fixed effects setting
# conentration has the wrong sign without fixed effects...
summary(lm(lfare ~ lpassen + concen, data=d1))
# why does concentration work??
summary(felm(lfare ~ lpassen + concen | id + year | 0 | 0, data=d1))
