# Create Population -------------------------------------------------------
# key determinants
set.seed(12345)
nr_people <- 10000
nr_schools <- 6
popul <- data.frame(ability = rnorm(n=nr_people, mean=10, sd=3),
                         parent_education = sample(10:18, size=nr_people, replace=TRUE),
                         health = rnorm(n=nr_people, mean=2, sd=5),
                         other_determinants = rnorm(n=nr_people, mean=0, sd=20))
school_quality <- seq(1, nr_schools)*2 + rnorm(n=nr_schools, mean=0, sd=0.05)


# ability, health, and parent education determine school assignment
school_score <- 0.5*popul$ability +
                0.5*popul$parent_education +
                0.1*popul$health +
                rnorm(n=nr_people, mean=0, sd=1)
school_prob <- (school_score - min(school_score)) /(max(school_score) - min(school_score))
popul$school <- rbinom(n=nr_people, size=5, prob=school_prob) + 1
popul$school_quality <- school_quality[popul$school]

# ability, health, and school assignment determine job category
jobs <- c("retail", "blue collar", "teacher", "academic", "Lawyer", "banker")
job_wage_level <- seq(1, length(jobs), 1)
job_score <- 0.9*popul$ability +
  0.5*popul$health +
  popul$school_quality +
  rnorm(n=nr_people, mean=0, sd=2)
job_prob <- (job_score - min(job_score)) /(max(job_score) - min(job_score))
job_assignment <- rbinom(n=nr_people, size=length(jobs)-1, prob=job_prob) + 1
popul$occupation <- jobs[job_assignment]
popul$job_wage_level <- job_wage_level[job_assignment]

# ability, years education, school assignment and job category determine
# wages AND years of education
edu_score <- popul$ability +
                   0.4 * popul$school_quality +
                   0.8 * popul$job_wage_level +
                   rnorm(n=nr_people, mean=0, sd=0.5)
edu_prob <- (edu_score - min(edu_score)) /(max(edu_score) - min(edu_score))
popul$education <- rbinom(n=nr_people, size=10, prob=edu_prob) + 10

# monthly wages in $100
popul$wage <- exp((1.2*popul$ability +
                        0.08 * popul$education +
                        popul$school_quality +
                        popul$job_wage_level +
                        popul$other_determinants) /10)
# truncate wages that are too large or small:
popul <- popul[popul$wage < 100 & popul$wage > 1,]


# Population Descriptives -------------------------------------------------
# Somme checks:
barcolor <- rgb(0.2, 0.3, 0.4, 0.9)
hist(popul$wage, breaks=100, col=barcolor, border=NA)
summary(popul$wage)
summary(lm(log(wage)~education, data=popul))
summary(lm(log(wage)~education + ability, data=popul))
# Looks good. Now, we need to take out three datasets with identical, observable values.


# Create datasets ---------------------------------------------------------
# a bit awkward, but I wanted to do this without dplyr
observables <- c("parent_education", "school", "occupation", "education", "wage")
sample_size <- 100

sample1_rows <- sample(1:nrow(popul), size=sample_size)
sample1 <- popul[sample1_rows, observables]
leftover_pop <- popul[-sample1_rows, observables]
exact_obs_matches <- merge(sample1, leftover_pop,
                           by=c("parent_education", "school", "occupation", "education"),
                           suffixes=c("_sample1", "_new"))
nr_matches <- aggregate(wage_new ~ wage_sample1, exact_obs_matches, length)
selected_matches <- nr_matches[nr_matches$wage_new >2, "wage_sample1"]
exact_obs_matches <- exact_obs_matches[exact_obs_matches$wage_sample1 %in% selected_matches, ]
# using group average function to create running indices per group
exact_obs_matches$wage <- exact_obs_matches$wage_new
exact_obs_matches$sample_index <- ave(exact_obs_matches$wage,
                                      exact_obs_matches$wage_sample1,
                                      FUN=seq_along)

sample1 <- exact_obs_matches[exact_obs_matches$sample_index == 1, observables]
sample2 <- exact_obs_matches[exact_obs_matches$sample_index == 2, observables]
sample3 <- exact_obs_matches[exact_obs_matches$sample_index == 3, observables]

par(mfrow=c(1,3))
hist(sample1$wage, breaks=20, col=barcolor, border=NA, main="")
hist(sample2$wage, breaks=20, col=barcolor, border=NA, main="")
hist(sample3$wage, breaks=20, col=barcolor, border=NA, main="")

write.csv(sample1, "data/wage_sample1.csv")
write.csv(sample2, "data/wage_sample2.csv")
write.csv(sample3, "data/wage_sample3.csv")
