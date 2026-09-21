#PART 0: Load In Data and DATA EXPLORATION

  #Packages
library(MASS)
library(ggplot2)
library(dplyr)
library(lme4)
library(splines)
library(Metrics)

  #Dataset
setwd("C:/Users/Mark.Williamson.2/OneDrive - North Dakota University System/Desktop/Williamson Data/R/R_data")
  #You'll need to set your own directory

nl_ds <-read.csv("non_linear_dataset.csv")  #<- Get this dataset in the Github repository

  #Exploration

    #scatter plot
head(nl_ds)
ggplot(data=nl_ds, aes(x=X, y=Y))+
  geom_point() +
  labs(title="Scatter Plot")

    #histogram
ggplot(data=nl_ds, aes(x=Y))+
  geom_histogram(color = "black", fill = "lightblue", bins=10)+
  labs(title="Histogram")

    #QQ plot
qqnorm(nl_ds$Y)
qqline(nl_ds$Y)
###############################################################################


#PART 1: GENERAL LINEAR MODEL

  #linear model
lm1 <- lm(Y ~ X, data=nl_ds)
summary(lm1)

  #graphing
fig1 <- ggplot(data=nl_ds, aes(x=X, y=Y))+
  geom_point()+
  labs(title="Linear Model")
fig1 + geom_smooth(method = "lm")
###############################################################################


#PART 2: TRANSFORMED MODELS

  #new variables for log and square root transformations
nl_ds$log_Y <- log(nl_ds$Y)

nl_ds$sqrt_Y <- sqrt(nl_ds$Y)

#------

  #log transformed model
lm2 <- lm(log_Y ~ X, data=nl_ds)
summary(lm2)

  #graphing
fig2 <- ggplot(data=nl_ds, aes(x=X, y=log_Y))+
  geom_point()+
  labs(title="Log-Transformed (Y) Model")
fig2 + geom_smooth(method = "lm")

#------

  #square-root transformed model
lm3 <- lm(sqrt_Y ~ X, data=nl_ds)
summary(lm3)
  
  #graphing
fig3 <- ggplot(data=nl_ds, aes(x=X, y=sqrt_Y))+
  geom_point()+
  labs(title="Sqrt-Transformed (Y) Model")
fig3 + geom_smooth(method = "lm")
###############################################################################


#PART 2: GENERALIZED MODELS

  #Logistic Regression

    #cutoff metric
lg_cutoff <- mean(nl_ds$Y)

    #creating binary version of Y variable with cutoff metric
nl_ds <- nl_ds %>%
  mutate(binary_Y = case_when(Y >= lg_cutoff ~ 1,
                              Y <  lg_cutoff ~ 0))

    #Logistic regression model
lm4 <-glm(binary_Y ~ X, family=binomial(), data=nl_ds)
summary(lm4)
    
    #graphing
fig4 <- ggplot(data=nl_ds, aes(x=X, y=binary_Y))+
  geom_point()+
  labs(title="Logistic Model")
fig4 + geom_smooth(method = "glm", method.args = list(family = binomial)) 

#------

  #Poisson regression model
lm5 <- glm(Y~X, family=poisson(), data=nl_ds)
summary(lm5)

  #graphing
fig5 <- ggplot(data=nl_ds, aes(x=X, y=Y))+
  geom_point()+
  labs(title="Poisson Model")
fig5 + geom_smooth(method = "glm", method.args = list(family = poisson)) 

#------

  #Negative binomial regression model
lm6 <- glm.nb(Y~X, data=nl_ds)
summary(lm6)

  #graphing
fig6 <- ggplot(data=nl_ds, aes(x=X, y=Y))+
  geom_point()+
  labs(title="Negative Binomial Model")
fig6 + geom_smooth(method = "gam", method.args = list(family ="nb")) 
###############################################################################


#PART 3: NON-LINEAR MODELS

  #Polynomial regression model
lm7 <- lm(Y ~ poly(X, 2), data =nl_ds)
summary(lm7)

  #graphing
fig7 <- ggplot(data=nl_ds, aes(x=X, y=Y))+
  geom_point()+
  labs(title="Polynomial model (degrees=2)")
fig7 + geom_smooth(method="lm", formula = y ~ poly(x, 2))

#------

  #Spine regression model
lm8 <-lm(Y ~ bs(X), data=nl_ds)
summary(lm8)

  #graphing
fig8 <- ggplot(data=nl_ds, aes(x=X, y=Y))+
  geom_point()+
  labs(title="Splines Model (knots=3)")
fig8 + geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3))

#------

  #Loess regression model
lm9 <- loess(Y~X, span=0.9, data=nl_ds)
summary(lm9)

  #graphing
fig9 <- ggplot(data=nl_ds, aes(x=X, y=Y))+
  geom_point()+
  labs(title="Loess Model (span=0.9)")
fig9 + geom_smooth(method = "loess", span=0.9)
###############################################################################


#PART 5: MODEL FITTING

#Model fit - just using AIC scores for now
AIC(lm1)
AIC(lm2)
AIC(lm3)
AIC(lm4)
AIC(lm5)
AIC(lm6)
AIC(lm7)
AIC(lm8)
AIC(lm9)


