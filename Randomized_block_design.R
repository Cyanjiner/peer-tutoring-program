cloud_data <- read.csv("/Volumes/courses/QAC/qac307/Data/cloud_data.csv")
resp_rate <- read.csv("/Volumes/courses/QAC/qac307/Data/resp_rate.csv")


# Cloud_data example: Randomized Block Design -----------------------------

# visually determine whether there is a relationship beyween treatment and rainfall

library(ggplot2)

ggplot(data=cloud_data)+
  geom_boxplot(aes(x=Treatment, y = Rainfall))

ggplot(data=cloud_data)+
  geom_histogram(aes(x=Rainfall))

ggplot(data=cloud_data)+
  geom_histogram(aes(x=log(Rainfall)),binwidth = 1)

cloud_data$log_rainfall <- log(cloud_data$Rainfall)

# Determine whether there is a significant difference in mean rainfall

tapply(cloud_data$Rainfall, cloud_data$Treatment, mean)

model <- aov(Rainfall~Treatment+Type, data=cloud_data)
summary(model)

# check assumptions of the model
layout(matrix(c(1,2,3,4),2,2))
plot(model)

# see whether log transform of rain helps homogeneity
ggplot(data = cloud_data)+
  geom_boxplot(aes(x=Treatment,y=log(Rainfall)))

# run our model with log transformed rainfall
model2 <- aov(log(Rainfall)~Treatment+Type, data=cloud_data)
summary(model2)
plot(model2)

# regarding the homogeneity: regardless of combinations of treatment and type, 
# four fitted values almost have the same amount of uncertainty (spread around 0).

# Normal QQ plot is not perfectly linear but its okay because there is no significant
# variance and our observations were more than 30.



# Resp_rate example : Completely Randomized Design with covariance ----------------------------------------

# completely randomized design and since age is correlated, we should include
# age as a covariance --> ANCOVA

# Is there is visual relationship between device and respiratory rate?
ggplot(data=resp_rate)+
  geom_boxplot(aes(x=device, y=Rate))

# Is there a significant relationship between device type and Rate?
model3 <- aov(Rate~device+Age, data = resp_rate)
summary(model3)

# check assumptions 
plot(model3)

# there seems to be some violation because of age 
# pattern: increased fitted values, increased uncertainty

# Look at the relationship between covariate (age) and respiratory rate
ggplot(data=resp_rate)+
  geom_point(aes(x=Age, y=Rate))

ggplot(resp_rate)+
  geom_density(aes(x=Rate))

# the higher the predicted value of rate, the higher the uncertainty

# Analyze with transformed y
model4 <- aov(log(Rate)~device+Age, data=resp_rate)
summary(model4)

plot(model4)

# Suppose device A and C are not invasive and device B is.
# Test the contrast that compares invase against non-invasive.

## H_0: (\mu_A + \mu_B)/2 - \mu_B = 0

library(gmodels)

fit.contrast(model4, "device", c(1/2,-1,1/2),conf.int=0.95)

# Controlling for Age, we expect with 95% confidence, devices that are non-invasive
# have a mean that is between 0.015 and 0.117 log bpm higher than invasive.









