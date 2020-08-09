# Required packages

library(psych)
library(pastecs)
library(ggplot2)
library(ggplotgui)
library(rstanarm)
library(loo) 
library(dplyr)
library(tidybayes)
library(brms)
library(tidyverse)  # ggplot, dplyr, %>%, and friends
library(ggdag)  # Make DAGs with ggplot
library(dagitty)  # Do basic DAG math
library(broom)  # For converting model output to data frames
library(caret) # model comparison
library(plyr) # helps with model comparison computation
library(performance)

#### Directed Acyclic Graph ####

node.details <- tribble(
  ~name, ~label, ~x, ~y,
  "lidar_use", "Lidar Use", 5, 1,
  "currentmap", "Current Map Accuracy", 5, 3, 
  "gov_trust", "Trust in Government", 1, 3,
  "science_trust", "Trust in Science", 2, 3,
  "gov_involve", "Government Involvement", 3, 3,  
  "risk_percp_mean", "Risk Perception", 4, 3,
  "demo", "Demographics", 8, 1,
  "age", "Age", 9, 2, 
  "gender", "Gender", 10, 2, 
  "education", "Education", 11, 2,
  "barriers", "Structural Barriers", 6, 3,  
  "soep", "Risk Preference", 6, 2,
  "network", "Network", 7, 2,
  "alter_lidar_prop", "Propotion of Alters that Use Lidar", 8, 3,
  "alter_strength_mean", "Alter Strength", 9, 3,
  "alter_exp_mean", "Alter Expertise", 10, 3,
  "pol_support", "Political Support", 4, 4,  
  "know", "Lack of Knowledge", 3, 4,
  "pop", "Lack of Population", 5, 4,
  "dev", "Low Development Rate", 6, 4,
  "low_risk", "Low Flood Risk", 7, 4,
  "fund", "Lack of Funding", 8, 4,
  "staff", "Lack of Staffing", 9, 4,
  "no_data", "No Publically Available Lidar", 10, 4,
  "experience", "Direct Experience", 1, 2,
  "prepared", "Community Preparation", 2, 2)

node.labels <- node.details$label
names(node.labels) <- node.details$name

lidar.use.dag <- dagify(lidar_use ~ gov_trust + science_trust + gov_involve +
                          risk_percp_mean + demo + barriers + soep + network + experience + prepared + currentmap,
                        barriers ~ know + pol_support + pop + dev + low_risk + fund + staff + no_data,
                        risk_percp_mean ~ experience + prepared + low_risk,
                        science_trust ~ know,
                        prepared ~ low_risk,
                        gov_involve ~ fund + pol_support,
                        network ~ alter_lidar_prop + alter_exp_mean + alter_strength_mean,                        outcome = "lidar_use",
                        coords = node.details,
                        labels= node.labels)
## Let's look at paths to see if there are any confounding variables for each predictor
# because we have multiple predictors, we need to run the test for each one seperately
paths(lidar.use.dag, from = "risk_percp_mean", to = "lidar_use")
paths(lidar.use.dag, from = "barriers", to = "lidar_use")
paths(lidar.use.dag, from = "network", to = "lidar_use")
paths(lidar.use.dag, from = "gov_involve", to = "lidar_use")
paths(lidar.use.dag, from = "science_trust", to = "lidar_use")
paths(lidar.use.dag, from = "prepared", to = "lidar_use")

# Turn DAG into a tidy data frame for plotting
lidar.use.dag.tidy <- lidar.use.dag %>%
  tidy_dagitty() %>%
  node_status()   # Add column for exposure/outcome/latent

status.colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")

# Fancier graph
ggplot(lidar.use.dag.tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status.colors, na.value = "grey20") +
  scale_fill_manual(values = status.colors, na.value = "grey20") +
  guides(color = FALSE, fill = FALSE) +
  theme_dag()

# takeaways: it appears as if several of the structural barriers could be confounding variables such as lack of knowledge, political support, low flood risk, and lack of funding. Do we want to keep these in?

#### Simulating Survey Data ####
## STEP 1: Data Simulation ##
set.seed(123) 
N=200 # number of survey respondensim.data
K=19 # number of predictors

## 1) Set the intercept ##

intercept=0 ## mean value of lidar use when all predictors are equal to 0

## 2) Set the predictor variables ##
## set simulate experience with binomial for each type of experience
experience_1 <- rbinom(N, 1, .5) # damage to property in community # yes (1) no (0)
# not sure what the appropriate probability is here... I think it is probably different for each level of experience closeness
experience_2 <- rbinom(N, 1, .5) # deaths or injury to people in your community # yes (1) no (0)
experience_3 <- rbinom(N, 1, .5) # damage to home # yes (1) no (0)
experience_4 <- rbinom(N, 1, .5) # deaths or injuries to you or members of your immediate family # yes (1) no (0)
experience_5 <- rbinom(N, 1, .5) # disruption to your electric, water, phone, and other basic services # yes (1) no (0)
gov_trust <- sample(1:5, N, replace=TRUE) # not at all (1) completely (5)
currentmap <- sample(1:5, N, replace=TRUE) # not at all (1) completely (5)
science_trust <- sample(1:5, N, replace=TRUE) # not at all (1) completely (5)
gov_involve <- sample(1:5, N, replace=TRUE) # not at all (1) completely (5)
future_1 <- sample(1:5, N, replace=TRUE) # damge to property in the community within next 30 years 0% (1) 100%(5)
future_2 <- sample(1:5, N, replace=TRUE) # deaths or injury to people in community within next 30 years 0% (1) 100%(5)
future_3 <- sample(1:5, N, replace=TRUE) # damage to home in the next 30 years 0% (1) 100%(5)
future_4 <- sample(1:5, N, replace=TRUE) # deaths or injuries to your or members of your immediate family within next 30 years 0% (1) 100%(5)
future_5 <- sample(1:5, N, replace=TRUE) # disruption to your electric, water, phone, and other basic services within the next 30 years 0% (1) 100% (5)
incr_no_flood<- sample(1:3, N, replace=TRUE) # no (1) stay the same (2) yes (3)
incr_sev_flood<- sample(1:3, N, replace=TRUE) # no (1) stay the same (2) yes (3)
prepared<- sample(1:5, N, replace=TRUE) # not at all (1) completely (5)
age <- sample(1:5, N, replace=TRUE) # les than 20 years (1) 50+ years (5)
gender <- sample(1:3, N, replace=TRUE) # male (1) female(2) prefer to self-describe (3)
education <- sample(1:6, N, replace=TRUE) # some high school (1) advanced degree (6)
barrier_1 <- sample(1:5, N, replace=TRUE) # lack of funding strongly disagree (1) strongly agree (5)
barrier_2 <- sample(1:5, N, replace=TRUE) # lack of expertise strongly disagree (1) strongly agree (5)
barrier_3 <- sample(1:5, N, replace=TRUE) # sparse population strongly disagree (1) strongly agree (5)
barrier_4 <- sample(1:5, N, replace=TRUE) # low rate of economic development strongly disagree (1) strongly agree (5)
barrier_5 <- sample(1:5, N, replace=TRUE) # low flooding risk strongly disagree (1) strongly agree (5)
barrier_6 <- sample(1:5, N, replace=TRUE) # lack of political support strongly disagree (1) strongly agree (5) 
#barrier_7 <- sample(1:5, N, replace=TRUE) # "other" response on the survey strongly disagree (1) strongly agree (5)
  ## How should I account for this in analysis of the actual survey data?
barrier_8 <- rbinom (N, 1,.2) # this is whether respondent is in an area with publically available lidar or not...the probobaly will change once I calculate the % of publically avaiable data across FEMA Region X
soep <-  sample(0:10, N, replace=TRUE) # range of risk preference from 0 to 10, where 0 = I generally prefer to take risks to 10 = I generally prefer to avoid risks
alter_lidar_prop <- runif(N, 0, 1) # this simulates the a range of potential proportion of lidar users in alters
alter_exp_mean <- runif(N, 0, 10) # this will rep the mean expertise of alters 
alter_comm_mean <- runif(N, 1, 6) # reps the mean communication of alters with respondent

# Make data frame with raw data
raw.data <- data.frame(experience_1, experience_2, experience_3, experience_4, experience_5, gov_trust, currentmap, science_trust, gov_involve, future_1, 
                       future_2, future_3, future_4, future_5, incr_no_flood, incr_sev_flood, prepared, age, gender, education, barrier_1, barrier_2, 
                       barrier_3, barrier_4, barrier_5, barrier_6, barrier_8, soep, alter_lidar_prop, alter_comm_mean, alter_exp_mean)

raw.data <- tibble::rowid_to_column(raw.data, "ID")

## 3) Now that we have our raw data, let's do the indexing process ##
# Summation of direct experiences. First we need to quantify the "level of closeness" of the direct experience
raw.data$experience_2[raw.data$experience_2==1] <- 1
raw.data$experience_2[raw.data$experience_2==1] <- 2
raw.data$experience_3[raw.data$experience_3==1] <- 4
raw.data$experience_4[raw.data$experience_4==1] <- 5
raw.data$experience_5[raw.data$experience_5==1] <- 3
raw.data$direct_exp_sum <- apply(raw.data[,c(2:6)],1,sum) # closer the experience, higher the number


# Average risk perception
# QUESTION: do I need to rescale the two likert scales to the same scale?
raw.data$risk_percp_mean <- (future_1*1 + future_2*2 + future_3*4 + future_4*5 +future_5*3 + # future predictors are scaled based on the "level of closness" # closer the future event, higher the number
                               incr_no_flood + incr_sev_flood)/3

  
# Total strength of respondent's relationship with alters that use lidar. In this case there is only one alter.
raw.data$alter_strength_tot <- (alter_lidar_prop*alter_comm_mean)

# Total expertise of respondent's alters that use lidar
raw.data$alter_expertise_tot <- (alter_lidar_prop*alter_exp_mean)

# Look at indexed data frame

str(raw.data)

# In order for this next step to work, we need to turn these indexed predictors into "values"
direct_exp_sum <- as.numeric(raw.data$direct_exp_sum)
risk_percp_mean <- as.numeric(raw.data$risk_percp_mean)
alter_strength_tot <- as.numeric(raw.data$alter_strength_tot)
alter_expertise_tot <- as.numeric(raw.data$alter_expertise_tot)

## Now let's set the effect size ##
b_direct_exp_sum <- 1
b_gov_trust <- 0
#b_currentmap <- 0 # I think this could be left out of statisical analysis
b_science_trust <- 0
b_gov_involve <- 0
b_risk_percp_mean <- 0
#b_prepared<- .03 # I think this could be left out of statisical analysis
b_age <- 0
b_gender <- 0 
b_education <- 0
b_barrier_1 <- 0
b_barrier_2 <- 0
b_barrier_3 <- 0
b_barrier_4 <- 0
b_barrier_5 <- 0
b_barrier_6 <- 0
b_barrier_8 <- 0
b_soep <-  0
b_alter_lidar_prop <- 0 
b_alter_strength_tot <- 0
b_alter_expertise_tot <- 0

  
## 4) Set the response variable ##
p <- intercept + direct_exp_sum*b_direct_exp_sum + gov_trust*b_gov_trust + science_trust*b_science_trust + gov_involve*b_gov_involve +
  risk_percp_mean*b_risk_percp_mean + age*b_age + gender*b_gender + education*b_education + barrier_1*b_barrier_1 + 
  barrier_2*b_barrier_2 + barrier_3*b_barrier_3 + barrier_4*b_barrier_4 + barrier_5*b_barrier_5 + barrier_6*b_barrier_6 + 
  barrier_8*b_barrier_8 + soep*b_soep + alter_lidar_prop*b_alter_lidar_prop + alter_strength_tot*b_alter_strength_tot +
  alter_expertise_tot*b_alter_expertise_tot

pr <- plogis(p) # convert from log odds to probability

lidaruse <- rbinom(N,1,pr)

## 5) Combine data into dataframe ##

sim.data <- data.frame(lidaruse, direct_exp_sum, gov_trust, science_trust, gov_involve, 
                         risk_percp_mean, age, gender, education, barrier_1, 
                         barrier_2, barrier_3, barrier_4, barrier_5, barrier_6,  
                         barrier_8, soep, alter_lidar_prop, alter_strength_tot, 
                         alter_expertise_tot)
write.csv(sim.data, "sim_data.csv")

#### Descriptive Stasim.data ####
# Check for correlations
cor(sim.data) # nothing > |.5| so we are good!

# plot the raw data & check for outliers
boxplot(sim.data[-24])

###D Data Analysis ####

# Let's start small and see how well our model is picking up the set effect

mod1 <-  brm(lidaruse~ mo(direct_exp_sum), data = sim.data, family = bernoulli)

mod1.stan <- stan_glm(lidaruse ~ direct_exp_sum, data=sim.data, family= binomial)

# Let's compare the resulsim.data
summary(mod1)
summary(mod1.stan)
prior_summary(mod1.stan)

# Compare plosim.data
mcmc_plot(mod1) # only works for brms
plot(mod1.stan, "areas", prob=0.95) # only works for stan models

# Let's see how these models compare to one another
median(bayes_R2(mod1))
median(bayes_R2(mod1.stan)) # this has a slightly better fit to it

# Loo compare
loo_compare(loo(mod1), loo(mod1.stan)) # this suggessim.data that the stan model is less bad

## So far both models are picking up the effect well, however the rstanarm model is slighty better
# Let's see if this is true when the model gesim.data more complicated

mod2 <-  brm(lidaruse~ mo(direct_exp_sum) + mo(gov_trust), data = sim.data, family = bernoulli)

mod2.stan <- stan_glm(lidaruse ~ direct_exp_sum + gov_trust, data=sim.data, family= binomial)

# Let's compare the resulsim.data
summary(mod2)
summary(mod2.stan)

# Compare plosim.data
mcmc_plot(mod2) # only works for brms
plot(mod2.stan, "areas", prob=0.95) # only works for stan models

# Let's see how these models compare to one another
median(bayes_R2(mod2))
median(bayes_R2(mod2.stan)) # this has a slightly better fit to it

# Loo compare
loo_compare(loo(mod2), loo(mod2.stan)) # this suggessim.data that the stan model is less bad

# Let's add a couple more predictors and see what happens
mod3 <-  brm(lidaruse~ mo(direct_exp_sum) + mo(gov_trust) + alter_lidar_prop + alter_strength_tot +
               alter_expertise_tot, data = sim.data, family = bernoulli) # the network predictors are continous so no need to specify as monotonic ordered

mod3.stan <- stan_glm(lidaruse ~ direct_exp_sum + gov_trust + alter_lidar_prop +alter_strength_tot +
                        alter_expertise_tot, data=sim.data, family= binomial)
# Let's compare the resulsim.data
summary(mod3)
summary(mod3.stan)

# Compare plosim.data
mcmc_plot(mod3) # only works for brms
plot(mod3.stan, "areas", prob=0.95) # only works for stan models

# Let's see how these models compare to one another
median(bayes_R2(mod3))
median(bayes_R2(mod3.stan)) # this has a slightly better fit to it

# Loo compare
loo_compare(loo(mod3), loo(mod3.stan)) # this suggessim.data that the stan model is less bad
# Model is startin to get a bit noisy, however each of the predictors that were set at 0 initially do overlap 0 so that's good

## Since the rstanarm model appears to have a better fit, I am going to play around with some parameters with that model specificaly

# Let's trying running the model with higher observation and see if we see the effect better
# with N=1000
mod3.stan <- stan_glm(lidaruse ~ direct_exp_sum + gov_trust + alter_lidar_prop +alter_strength_tot +
                        alter_expertise_tot, data=sim.data, family= binomial)

summary(mod3.stan)
plot(mod3.stan, "areas", prob=0.95) 
median(bayes_R2(mod3.stan))


# with N=1000, the effecsim.data are picked up much closer to their initial specification 
# is that true if we run it with all the predictors?
## Run the Full Model: 
# need to set N=1000 at the top and rerun simulation data
full.mod.stan.1000 <- stan_glm(lidaruse ~ direct_exp_sum + gov_trust + science_trust + gov_involve + 
                      risk_percp_mean + age + gender + education + barrier_1 + 
                      barrier_2 + barrier_3 + barrier_4 + barrier_5 + barrier_6 + 
                      barrier_8 + soep + alter_lidar_prop + alter_strength_tot +
                      alter_expertise_tot, data=sim.data, family= binomial)


summary(full.mod.stan.1000)
plot(full.mod.stan.1000, "areas", prob=0.95) 
median(bayes_R2(full.mod.stan.1000))

# the resulsim.data pick up the effecsim.data well except for barrier_8 which is a bit elevated. I am going to run it now with 200 because that is around the number of responses we got
# need to set N=200 at the top and rerun simulation data
full.mod.stan.200 <- stan_glm(lidaruse ~ direct_exp_sum + gov_trust + science_trust + gov_involve + 
                            risk_percp_mean + age + gender + education + barrier_1 + 
                            barrier_2 + barrier_3 + barrier_4 + barrier_5 + barrier_6 + 
                            barrier_8 + soep + alter_lidar_prop + alter_strength_tot +
                            alter_expertise_tot, data=sim.data, family= binomial)


summary(full.mod.stan.200)
plot(full.mod.stan.200, "areas", prob=0.95) 
median(bayes_R2(full.mod.stan.200))

# Let's run a full mod using brms just so that we can run a LOOCV with it
full.mod.brms <- brm(lidaruse ~ mo(direct_exp_sum) + mo(gov_trust) + mo(science_trust) + mo(gov_involve) + 
                           risk_percp_mean + mo(age) + gender + mo(education) + mo(barrier_1) + 
                           mo(barrier_2) + mo(barrier_3) + mo(barrier_4) + mo(barrier_5) + mo(barrier_6) + 
                           mo(barrier_8) + mo(soep) + alter_lidar_prop + alter_strength_tot +
                           alter_expertise_tot, data=sim.data, family= bernoulli)


summary(full.mod.brms)
plot(full.mod.brms, "areas", prob=0.95) 
median(bayes_R2(full.mod.brms))

# brms is super slow compared to stan
# as expected, there is a lot more noise in the slopes here especially with barrier_8 and alter_lidar_prop. I think this is because the data is on a smaller scale for these predictors. Barrier 8 is binomial and alter_lidar_prop is a proportion between 0 and 1
# all predictors do cross 0 except direct_exp_sum which is how the effecsim.data are set. So maybe it is okay?

#### Cross Validation ####
# this is not to decide whether to include a predictor or not, rather this is to compare various model structure. It could be used to compare different sesim.data of covariates though
# https://datascienceplus.com/k-fold-cross-validation-in-stan/

## There are a couple of ways for running a cross validation
# 1) K-fold cross validation for when there are a lot of points
    # choosing K: typically k=5 or 10 have been used so let's try it using k=10
kfold(full.mod.stan.200, K=10) # elpd_kfold=-77.4, SE=12.2
kfold(full.mod.stan.200, K=40) # elpd_kfold= -82.4, se=12.7
# as K increases, it seems like the predictive density does as well. This suggests that we should just do the tradition LOOCV with k=1
kfold(full.mod.brms, K=10) # takes forever, wouldn't suggest running this!
kfold(full.mod.stan.1000, K=10) # this is just out of curiousity: full.mod.1000 has more than 3x the predictive density as full.mod.200
# Not sure how helpful this calculation is because they are relative to one another

# 3) Absolute metric: A traditional LOOCV with k=1, this example does not have grouping. Do you think we should add it? https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.02881 
# Need to split our data into training and test sesim.data
# make a df to put the resulsim.data in

Result_G <- data.frame(matrix(nrow=nrow(sim.data), ncol=6))

#set the column names
colname(Result_G) <- c("MedianG", "LowerBound1_G", "LowerBound2_G", "UpperBound1_G", "UpperBound2_G", "R2_Bayes")

#do the leave one out cross validation
# create for loop function
for(i in 1:length(sim.data$lidaruse)){ # for all the data poinsim.data
  sub_dat <- sim.data[-i,] #remove each one at a time
  m_sub <-  stan_glm(lidaruse ~ direct_exp_sum + gov_trust + science_trust + gov_involve + 
                       risk_percp_mean + age + gender + education + barrier_1 + 
                       barrier_2 + barrier_3 + barrier_4 + barrier_5 + barrier_6 + 
                       barrier_8 + soep + alter_lidar_prop + alter_strength_tot +
                       alter_expertise_tot, data=sim.data, family= binomial) # this is saying run the model without that point
  post=posterior_predict(m_sub, sim.data[i,], draws=4000)
  r2=r2_bayes(m_sub)# predict that point
  Result_G[i,1]=quantile(post, 0.5) # fill a df with the credibility intervals
  Result_G[i,2]=quantile(post, 0.25) 
  Result_G[i,3]=quantile(post, 0.025)
  Result_G[i,4]=quantile(post, 0.75)
  Result_G[i,5]=quantile(post,0.975)
  Result_G[i,6]=r2
}

write.csv(Result_G, "G:/Everyone-Temp/TaraPozzi/Pre_Registration/Analysis//Result_G.csv")
# we are getting a mean of R2=39%, is this acceptable?
# The reason we are interested in r2 from model predictions is because it displays our model's predictive power when we don't know the response
# An r2 based on our full dataset is unrealistic. 
# Note: Although a lower R-squared can be disappointing, it is a more defensible and realistic measure of your model's likely performance on new data.
# Helpful blog: https://www.zevross.com/blog/2017/09/19/predictive-modeling-and-machine-learning-in-r-with-the-caret-package/ 
# One other way... that uses bootstrapping instead of cross validation
# we are going to use the Caret package for this
# lidaruse~. means use lidaruse as the model response
# and use all other variables as predictors
train1 <- train(lidaruse~., data = sim.data, method = "bayesglm")
class(train1)
attributes(train1)
train1$finalModel
train1
train1$results
# cv using caret package
tc <- trainControl(method="cv", number=10)
train1.cv <- train(lidaruse~., data = sim.data, method = "bayesglm", trControl=tc)
train1.cv

#### Model Selection within rstanarm ####
# We can use a step BIC to determine, within the stan model, the best combination of predictors
#https://statswithr.github.io/book/bayesian-model-choice.html 
# Compute the total number of observations
n = nrow(sim.data)

# Full model using all predictors
full.mod.bic = glm(lidaruse ~ ., data=sim.data, family='binomial')

# Perform BIC elimination from full model
# k = log(n): penalty for BIC rather than AIC
mod.step = step(full.mod.bic, k=log(n)) 
# these results suggest that a model with just alter_strenth_tot, gov_trust, & direct_exp_sum has the lowest AIC

# let's run a model with just those three variables and see what the r2 is 
Result_G_AIC <- data.frame(matrix(nrow=nrow(sim.data), ncol=6))

#set the column names
colname(Result_G_AIC) <- c("MedianG", "LowerBound1_G", "LowerBound2_G", "UpperBound1_G", "UpperBound2_G", "R2_Bayes")

#do the leave one out cross validation
# create for loop function
for(i in 1:length(sim.data$lidaruse)){ # for all the data poinsim.data
  sub_dat <- sim.data[-i,] #remove each one at a time
  m_sub <-  stan_glm(lidaruse ~ direct_exp_sum + gov_trust + alter_strength_tot, data=sim.data, family= binomial) # this is saying run the model without that point
  post=posterior_predict(m_sub, sim.data[i,], draws=4000)
  r2=r2_bayes(m_sub)# predict that point
  Result_G_AIC[i,1]=quantile(post, 0.5) # fill a df with the credibility intervals
  Result_G_AIC[i,2]=quantile(post, 0.25) 
  Result_G_AIC[i,3]=quantile(post, 0.025)
  Result_G_AIC[i,4]=quantile(post, 0.75)
  Result_G_AIC[i,5]=quantile(post,0.975)
  Result_G_AIC[i,6]=r2
}

write.csv(Result_G_AIC, "G:/Everyone-Temp/TaraPozzi/Pre_Registration/Analysis//Result_G_AIC.csv")
# however, when I run the LOOCV, there is a much lower r2 of 21%, therefore it seems like keeping the full model is the best option?


#### Data viz ####

# The only predictor I set with an effect is direct_exp_sum so let's see how this effect looks
max(raw.data$direct_exp_sum) #15

direct.exp.sim <-  rep(c(1:15), each=100)

# set other predictor variables to their lowest affect at their minimum

simdata1 <-add_fitted_draws(newdata=data.frame(direct_exp= direct.exp.sim, 
                                               gov_trust= 1,
                                               science_trust= 1,
                                               gov_involve= 1,
                                               risk_percp_mean= 1,
                                               age= 1,
                                               gender= 1,
                                               education= 1,
                                               barrier_1= 1,
                                               barrier_2= 1,
                                               barrier_3= 1,
                                               barrier_4= 1,
                                               barrier_5= 1,
                                               barrier_6= 1,
                                               barrier_8= 1,
                                               soep= 1,
                                               alter_lidar_prop= 0,
                                               alter_strength_tot= 0,
                                               alter_expertise_tot=0),# held at min
                                        full.mod.stan.200) 

## Plot the results 
ggplot(simdata1, aes(as.factor(direct_exp), .value)) + 
  geom_boxplot() + 
  labs(x = "Closeness of Direct Experience \nby survey respondent", y = "Effect on probability of LiDAR adoption") +
  theme_bw() 

#### OLD CODE ####
## IGNORE this section: create model with ordered categorical predictors ##
# let's make sure our data is in an easy to use format that the model will understand
#risk.pref.options <- c("0","1","2","3","4","5","6","7","8","9","10")
#risk <- factor(sample(risk.pref.options,200,TRUE),
#levels=risk.pref.options, ordered=TRUE)
#lidaruse.options <- c(0,1)
#lidaruse <- lidaruse.options[risk] + rbinom(200,1,1)
#lidaruse <- sim.risk.pref$lidaruse
#dat <- data.frame(lidaruse, risk)
# How well did our model do?
loo(mod1)
# RMSE function:
rmse <- function(y, ypred) {
  rmse = sqrt(mean((y - ypred)^2))
  return(rmse)
}

#MAE function:
mae <- function(y, ypred) {
  mae = (mean(abs(y - ypred)))
  return(mae)
}

yhat.full <- posterior_predict(sim.survey.fullmod) 
yhat.full <- apply(yhat.full, 2, median)
rmse(mod1)
mae(mod1)

# Let's look at the effect risk preference on lidar use 
# first, make a sequence of risk preference
risk.pref.gradient <- round(rep(seq(min(dat$risk),
                                    max(dat$risk),length.out=200),1)) 

preds.risk.pref <- add_fitted_draws(fit1, 
                              newdata=data.frame(risk.pref.g=risk.pref.gradient), #this creates a new data frame of predictions based on our existing data 
                              re_formula=NA,
                              draws = 200, type="response")

#This line loads the original data (actual collected poinsim.data)
ggplot(preds.risk.pref, aes(x=risk.pref.g, y=.value)) +  
  stat_lineribbon(.width = c(0.5, 0.95)) +   
  scale_fill_brewer(palette = "Greys") + 
  labs(y="Lidar Use", x = "Effect of Risk Preference") +
  geom_point(data=dat, aes(x=risk_pref,y=lidaruse)) +
  ggtitle("The effect of risk preference on lidar use.") +
  theme_bw()  

mean(preds.risk.pref$.value)
mean(sim.risk.pref$risk_pref)



Result_G <- data.frame(matrix(nrow=length(sim.data), ncol=7))

#set the column names
colnames(Result_G) <- c("Iteration", "MedianG", "LowerBound1_G", "LowerBound2_G", "UpperBound1_G", "UpperBound2_G", "R2_Bayes")

str(Result_G)

#do the leave one out cross validation
# create for loop function
for(i in 1:length(sim.data)){ # for all the data poinsim.data
  sub_dat <- sim.data[-i,] #remove each one at a time
  m_sub <-  stan_glm(lidaruse ~ direct_exp_sum + gov_trust + science_trust + gov_involve + 
                       risk_percp_mean + age + gender + education + barrier_1 + 
                       barrier_2 + barrier_3 + barrier_4 + barrier_5 + barrier_6 + 
                       barrier_8 + soep + alter_lidar_prop + alter_strength_tot +
                       alter_expertise_tot, data=sim.data, family= binomial) # this is saying run the model without that point
  post=posterior_predict(m_sub, sim.data[i,], draws=4000) # predict that point
  Result_G[i,"MedianG"]=quantile(post, 0.5) # fill a df with the credibility intervals
  Result_G[i,"LowerBound1_G"]=quantile(post, 0.25) 
  Result_G[i,"LowerBound2_G"]=quantile(post, 0.025)
  Result_G[i,"UpperBound1_G"]=quantile(post, 0.75)
  Result_G[i,"UpperBound2_G"]=quantile(post,0.975)
}


# Alternative LOOCV that didn't work
split_index <- createDataPartition(sim.data$lidaruse, p=0.8, list=FALSE, times=5) # 5 folds # this keeps 80% of our data 
head(split_index, 10)

# make the for loop to run the CV
error_df <- data.frame(matrix(ncol = 2, nrow = ncol(split_index)))
colnames(error_df) <- c('test_error', 'fold')

# need lidaruse to be a factor
sim.data$lidaruse <- as.factor(sim.data$lidaruse)
str(sim.data)

# for loop
for(i in 1:nrow(error_df)){
  # use ith column of split_index to create feature and target training/test sesim.data
  features_train <- sim.data[ split_index[,i], !(names(sim.data) %in% c('lidaruse'))] 
  features_test  <- sim.data[-split_index[,i], !(names(sim.data) %in% c('lidaruse'))]
  target_train <- sim.data[ split_index[,i], "lidaruse"]
  target_test <- sim.data[-split_index[,i], "lidaruse"]
  
  # Still need to preprocess each new set!
  preprocess_object <- preProcess(features_train, 
                                  method = c('scale', 'center', 'knnImpute'))
  features_train <- predict(preprocess_object, features_train)
  features_test <- predict(preprocess_object, features_test)
  
  # Fit the model and predict
  knn_fit <- knn3(features_train, target_train, k = 10)
  knn_pred <- predict(knn_fit, features_test, type = 'class' )
  
  # Calculate error and store it
  error <- mean(ifelse(target_test != knn_pred, 1, 0))
  error_df[i,'test_error'] <- error
  error_df[i, 'fold'] <- i
}

# About 10% error for each resample of the raw data 

# Now let's do a LOOCV for the model
# I can't get this to run. I am using this example for reference: https://rpubs.com/nicholas_dirienzo/cross_validation_fall2019
error_calc <- function(split_pro, folds, kn) {
  
  split_index <- createDataPartition(sim.data$lidaruse, p = split_pro, list = FALSE, times = folds)
  
  error_df <- data.frame(matrix(ncol = 3, nrow = ncol(split_index)))
  colnames(error_df) <- c('test_error_knn', 'test_error_log', 'fold')
  
  for(i in 1:nrow(error_df)){
    # use ith column of split_index to create feature and target training/test sets
    features_train <- sim.data[ split_index[,i], !(names(sim.data) %in% c('lidaruse'))] 
    features_test  <- sim.data[-split_index[,i], !(names(sim.data) %in% c('lidaruse'))]
    target_train <-sim.data[ split_index[,i], "lidaruse"]
    target_test <- sim.data[-split_index[,i], "lidaruse"]
    # Still need to preprocess each new set!
    preprocess_object <- preProcess(features_train, 
                                    method = c('scale', 'center', 'knnImpute'))
    features_train <- predict(preprocess_object, features_train)
    features_test <- predict(preprocess_object, features_test)
    # Fit the model and predict
    knn_fit <- knn3(features_train, target_train, k = kn)
    knn_pred <- predict(knn_fit, features_test, type = 'class' )
    # Calculate error and store it
    error <- mean(ifelse(target_test != knn_pred, 1, 0))
    error_df[i,'test_error_knn'] <- error
    error_df[i, 'fold'] <- i
    
    # Join data back into one df for glm() function
    full_train <- cbind(features_train, target_train)
    #full_train <- full_train %>% # rename
    #rename(lidaruse = target_train)
    # Train model
    log_train <- stan_glm(lidaruse ~ ., family = 'binomial', data = full_train)
    log_pred <- predict(log_train, newdata = features_test, type = 'response')
    #  Convert to classes
    log_pred <- ifelse(log_pred >= 0.5, 1, 0)
    error_log <- mean(ifelse(target_test != log_pred, 1, 0))
    # Add to df
    error_df[i,'test_error_log'] <- error_log
  }
  return(error_df)
}

log_v_knn <- error_calc(split_pro = 0.8, folds = 10, kn = 10)
log_v_knn

