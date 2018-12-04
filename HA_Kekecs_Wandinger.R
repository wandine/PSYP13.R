rm(list=ls())
graphics.off()

setwd("D:/PSYP13/R")

require(lsr)
require(psych)
require(car)
require(sciplot)
require(gsheet)
require(ggplot2)
require(plyr)

source("GraphPlot.R")

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")
data = data_sample_1

#first look at data
summary(data)
describe(data)

describe(data$age)
hist(data$age)
describe(data$pain)
hist(data$pain)
describe(data$pain_cat)
hist(data$pain_cat)
describe(data$STAI_trait)
hist(data$STAI_trait)
describe(data$mindfulness)
hist(data$mindfulness)
describe(data$cortisol_serum)
hist(data$cortisol_serum)
describe(data$cortisol_saliva)
hist(data$cortisol_saliva)
describe(data$weight)
hist(data$weight)

#excluding coding errors 
data = data[-which(data[, "ID"] == "ID_28"), ] 
data = data[-which(data[, "ID"] == "ID_112"), ] 
data = data[-which(data[, "ID"] == "ID_146"), ]

#checking again
describe(data$age)
hist(data$age)
describe(data$mindfulness)
hist(data$mindfulness)

#linear model: age and sex as predictors 
mod1 <- lm(pain ~ age + sex , data = data)
sm1 = summary(mod1)
sm1

AIC(mod1)
require(lm.beta)
confint(mod1) 
lm.beta(mod1)

#linear model: more predictors 
mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data)
sm2 = summary(mod2)
sm2

AIC(mod2)
confint(mod2)
lm.beta(mod2)

summary(mod1)$adj.r.squared 
summary(mod2)$adj.r.squared

anova(mod1, mod2)

#checking for outliers using Cook's Distance
plot(mod1, which = 4)
plot(mod2, which = 4)

#multivariate normality assumption
plot(mod1, which = 2)
plot(mod2, which = 2)
describe(residuals(mod1))
describe(residuals(mod2))
shapiro.test(residuals(mod1))
shapiro.test(residuals(mod2))

residualPlots(mod1)
residualPlots(mod2)

#linearity assumption
plot(mod1, which = 1)
plot(mod2, which = 1)

#multicollinearity
vif(mod1)
vif(mod2)
cor(data$cortisol_saliva, data$cortisol_serum)

#homoscedasticity
plot(mod1, which = 3)
plot(mod2, which = 3)
ncvTest(mod1)
ncvTest(mod2)
require(lmtest)
bptest(mod1)
bptest(mod2)

#examination of leverage
lev = hat(model.matrix(mod1))
plot(lev)
data[lev > .055,]
N = nrow(data)
mahad = (N-1)*(lev-1/N)
qchisq(.999, df=2) 
tail(sort(mahad), 5)
order(mahad, decreasing = T)[c(5,4,3,2,1)]

lev2 = hat(model.matrix(mod2))
plot(lev2)
data[lev > .15,]
N2 = nrow(data)
mahad2 = (N2-1)*(lev2-1/N2)
qchisq(.999, df=6)
tail(sort(mahad2), 5)
order(mahad2, decreasing = T)[c(5,4,3,2,1)]

#new model without salivary cortisol 
mod3 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data)
sm3 = summary(mod3)
sm3

AIC(mod3)
confint(mod3)
lm.beta(mod3)

anova(mod1, mod3)

#Cook's Distance
plot(mod3, which = 4)

#multivariate normality assumption
plot(mod3, which = 2)
describe(residuals(mod3))
shapiro.test(residuals(mod3))

residualPlots(mod3)

#linearity assumption
plot(mod3, which = 1)

#multicollinearity
vif(mod3)

#homoscedasticity
plot(mod3, which = 3)
ncvTest(mod3)
bptest(mod3)

############# PART 2 ##############

#initial model
mod_sub_reg <- lm(pain ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data)
mod_sub_reg
summary(mod_sub_reg)
AIC(mod_sub_reg)
confint(mod_sub_reg)
lm.beta(mod_sub_reg)

#checking for outliers 
plot(mod_sub_reg, which = 4) #Cook's Distance
lev = hat(model.matrix(mod_sub_reg)) #examination of leverage
plot(lev)
data[lev > .11,]
N = nrow(data)
mahad = (N-1)*(lev-1/N)
tail(sort(mahad), 5)
order(mahad, decreasing = T)[c(5,4,3,2,1)]

#assumptions of linear regression for initial model
plot(mod_sub_reg, which = 4)
plot(mod_sub_reg, which() = 2)
describe(residuals(mod_sub_reg))
shapiro.test(residuals(mod_sub_reg))
residualPlots(mod_sub_reg)
plot(mod_sub_reg, which = 1)
vif(mod_sub_reg)
plot(mod_sub_reg, which = 3)
ncvTest(mod_sub_reg)
bptest(mod_sub_reg)

#assumptions of linear regression for backwards model 
shapiro.test(residuals(backward.mod))
residualPlots(backward.mod)
vif(backward.mod)
ncvTest(backward.mod)
bptest(backward.mod)

#assumptions of linear regression for theory-based model 
shapiro.test(residuals(theory.based.mod))
residualPlots(theory.based.mod)
vif(theory.based.mod)
ncvTest(theory.based.mod)
bptest(theory.based.mod)

#stepwise selection 
mod_back = step(mod1, direction = "backward")

#building backwards model 
backward.mod <- lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum, data = data)
summary(backward.mod)
backward.mod
confint(backward.mod)
lm.beta(backward.mod)
AIC(backward.mod)

theory.based.mod = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data)
summary(theory.based.mod)
confint(theory.based.mod)
lm.beta(theory.based.mod)
AIC(theory.based.mod)

#comparing models
anova(mod1, backward.mod)
AIC(mod1)

anova(backward.mod, theory.based.mod) 
AIC(backward.mod)
AIC(theory.based.mod)

summary(backward.mod)$adj.r.squared
summary(theory.based.mod)$adj.r.squared 

#loading new data set 
data.new = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv")

View(data.new)
table(data.new$sex)
hist(data.new$weight)
hist(data.new$pain)
hist(data.new$pain_cat)
hist(data.new$STAI_trait)
hist(data.new$cortisol_serum)
hist(data.new$age)
hist(data.new$mindfulness)
summary(data.new)

#excluding coding errors 
data.new = data.new[-which(data.new[, "ID"] == "ID_123"), ] 
data.new = data.new[-which(data.new[, "ID"] == "ID_113"), ] 

#comparing predicted value with actual values 
test.backw <- predict(backward.mod, data.new)
test.backw

test.theory <- predict(theory.based.mod, data.new)
test.theory

plot(test.backw, data.new$pain)
cor(test.backw, data.new$pain)

plot(test.theory, data.new$pain) 
cor(test.theory, data.new$pain)

RSS_test = sum((data.new[, "pain"] - test.theory)^2) 
RSS_test_back = sum((data.new[, "pain"] - test.backw)^2)
RSS_test 
RSS_test_back 

#checking for outliers: Cook's Distance and leverage examination 
plot(backward.mod, which = 4)
lev = hat(model.matrix(backward.mod))
plot(lev)
data.new[lev > .10,]
N = nrow(data)
mahad = (N-1)*(lev-1/N)
tail(sort(mahad), 5) 
order(mahad, decreasing = T)[c(5,4,3,2,1)]

plot(theory.based.mod, which = 4)
lev = hat(model.matrix(theory.based.mod))
plot(lev)
data.new[lev > .10,]
N = nrow(data)
mahad = (N-1)*(lev-1/N)
tail(sort(mahad), 5) 
order(mahad, decreasing = T)[c(5,4,3,2,1)]

############# PART 3 #################

#loading additional packages
require(MASS)
require(smacof)
library(cAIC4)
library(r2glmm)
library(lme4) 
library(lmerTest)
library(psycho)

#function for standardized coefficients
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

data3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv")
View(data3)

summary(data3)
describe(data3)

hist(data3$pain1)
hist(data3$pain2)
hist(data3$pain3)
hist(data3$pain4)
hist(data3$STAI_trait)
hist(data3$pain_cat)
hist(data3$mindfulness)
hist(data3$cortisol_serum)
hist(data3$cortisol_saliva)

#correlation between pain ratings over time 
repeated_variables = c("pain1", "pain2", "pain3", "pain4")
cor(data3[, repeated_variables])

#formating data set from wide to long format
require(reshape2)
data_long = melt(data3, measure.vars = repeated_variables, variable.name = "time", value.name = "pain_rating")
data_long = data_long[order(data_long[, "ID"]),]
View(data_long)
data_long$time = as.numeric(data_long$time)

#building models 
mod_int = lmer(pain_rating ~ age + sex + STAI_trait + pain_cat + 
mindfulness + cortisol_serum + time + (1 | ID), data = data_long)

mod_slope = lmer(pain_rating ~ age + sex + STAI_trait + pain_cat + 
mindfulness + cortisol_serum + time + (time | ID), data = data_long)

#new data with predictions of models 
data_long_withpreds = data_long
data_long_withpreds$pred_int = predict(mod_int)
data_long_withpreds$pred_slope = predict(mod_slope)

#random intercept model
ggplot(data_long_withpreds, aes(y = pain_rating, x = time,
group = ID)) + geom_point(size = 3) + geom_line(color = "red",
aes(y = pred_int, x = time)) + facet_wrap(~ID, ncol = 5)

#slope model
ggplot(data_long_withpreds, aes(y = pain_rating, x = time,
group = ID)) + geom_point(size = 3) + geom_line(color = "red",
aes(y = pred_slope, x = time)) + facet_wrap(~ID, ncol = 5)

#comparing models 
cAIC(mod_int)$caic
cAIC(mod_slope)$caic

anova(mod_int, mod_slope)

#centering variable time 
data_long_centered_time = data_long
data_long_centered_time$time_centered = data_long_centered_time$time -
mean(data_long_centered_time$time)

#building quadratic model
mod_slope_quad = lmer(pain_rating ~ age + sex + STAI_trait + pain_cat + 
mindfulness + cortisol_serum + time_centered + I(time_centered^2) + (time | ID), data = data_long_centered_time)

#adding predictions to new data frame 
data_long_withpreds$pred_slope_quad = predict(mod_slope_quad)

#plotting quadratic model
plot_quad = ggplot(data_long_withpreds, aes(y = pain_rating,
x = time, group = ID)) + geom_point(size = 3) + geom_line(color = "red",
aes(y = pred_slope_quad, x = time)) + facet_wrap(~ID, ncol = 5)
plot_quad

#model comparison 
cAIC(mod_slope)$caic
cAIC(mod_slope_quad)$caic

data_long_with_resid = data_long_centered_time
data_long_with_resid$resid = residuals(mod_slope_quad)

#coefficients of quadratic model 
r2beta(mod_slope_quad, method = "nsj", data = data_long_centered_time)
cAIC(mod_slope_quad)$caic
summary(mod_slope_quad)
confint(mod_slope_quad)
stdCoef.merMod(mod_slope_quad)

results <- analyze(mod_slope_quad, CI = 95)
summary(results)
mutate(p = psycho: :format_p(p))
print(results)

#assumptions of the model 
require(influence.ME)
influence_observation = influence(mod_slope_quad, obs = T)$alt.fixed
influence_group = influence(mod_slope_quad, group = "ID")$alt.fixed
boxplot(influence_observation[, "time_centered"])

#influential outliers
pred_names = colnames(influence_group)
par(mfrow = c(1,length(pred_names)))
for (i in 1:length(pred_names)) {
  boxplot(influence_observation[, pred_names[i]], main = pred_names[i])
}

#normality 
require(lattice)
qqmath(mod_slope_quad, id = 0.05)
qqmath(ranef(mod_slope_quad))

#linearity
plot(mod_slope_quad, arg = "pearson")
plot(resid ~ time_centered, data = data_long_with_resid)
plot(resid ~ time_centered_2, data = data_long_with_resid)
plot(resid ~ pain_cat, data = data_long_with_resid)
plot(resid ~ STAI_trait, data = data_long_with_resid)
plot(resid ~ mindfulness, data = data_long_with_resid)
plot(resid ~ age, data = data_long_with_resid)
plot(resid ~ sex, data = data_long_with_resid)
plot(resid ~ cortisol_serum, data = data_long_with_resid)

#homoscedasticity across clusters
homosced_mod = lm(data_long_with_resid$resid^2 ~ data_long_with_resid$ID)
summary(homosced_mod)

#multicollinearity
pairs.panels(data_long_centered_time[, c("time_centered",
"time_centered_2", "age", "sex", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum")], col = "red",
lm = T)

#model excluding STAI_trait
mod_slope_quad_new = lmer(pain_rating ~ age + sex +  pain_cat + 
mindfulness + cortisol_serum + time_centered + I(time_centered^2) + (time | ID), data = data_long_centered_time)
mod_slope_quad_new
summary(mod_slope_quad_new)
cAIC(mod_slope_quad_new)

data_long_centered_time$time_centered_2 = data_long_centered_time$time_centered^2