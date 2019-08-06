library(ggplot2)
library(dplyr)

# Read Data
EmpAttrData <- read.csv("CaseStudy2-data.csv", header = TRUE)

# Decide what to factor, if needed
#EmpAttrData$Attrition <- as.factor(EmpAttrData$Attrition)
#EmpAttrData$OverTime <- as.factor(EmpAttrData$OverTime)

# Average Attrition Rate (16%)
attritionRate <- sum(EmpAttrData$Attrition == "Yes")/nrow(EmpAttrData)

# Remove the column with only one factor, otherwise glm won't work.
# You'll get a contrasts can be applied only to factors with 2 or more levels.
# Since all values are the same, this affects nothing
EmpAttrData$Over18 <- NULL

# Let's do some analysis to see what affects attrition
glmInfo <- glm(EmpAttrData$Attrition ~ ., family = binomial(link="logit"), data = EmpAttrData)
#glmInfo <- glm(EmpAttrData$Attrition ~ EmpAttrData$JobRole, family = binomial(link="logit"), data = EmpAttrData)
summary(glmInfo)

# Overtime
EmpAttrData %>% ggplot() +
  geom_bar(aes(OverTime, fill= Attrition), position = "stack")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(label = 'The Effect of Overtime on Attrition')

EmpAttrData %>% ggplot(aes(OverTime, fill= Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of Overtime on Attrition Rate')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Percentages')

#Job Role & Attrition
EmpAttrData %>% ggplot() +
  geom_bar(aes(JobRole, fill= Attrition), position = "stack")+
  scale_fill_manual(values = c("Yes" = "red","No" = "dark green"))+
  ggtitle(label = 'The Effect of Job role on Attrition')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

EmpAttrData %>%ggplot(aes(JobRole, fill= Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of Overtime on Attrition Rate')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ylab('Percentages')

#Job Role & Overtime
EmpAttrData %>% ggplot() +
  geom_bar(aes(JobRole, fill= OverTime), position = "stack")+
  scale_fill_manual(values = c("Yes" = "red","No" = "dark green"))+
  ggtitle(label = 'The Effect of Job role on Overtime')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

EmpAttrData %>%ggplot(aes(JobRole, fill= OverTime)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of Overtime on Overtime Rate')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ylab('Percentages')

# Job Satisfaction
EmpAttrData %>% ggplot() +
  geom_bar(aes(JobSatisfaction, fill= Attrition), position = "stack")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(label = 'The Effect of Job Satisfaction on Attrition')

EmpAttrData %>% ggplot(aes(JobSatisfaction, fill= Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of Job Satisfaction on Attrition Rate')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Percentages')

# Job Involvement
EmpAttrData %>% ggplot() +
  geom_bar(aes(JobInvolvement, fill= Attrition), position = "stack")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(label = 'The Effect of Job Involvement on Attrition')

EmpAttrData %>% ggplot(aes(JobInvolvement, fill= Attrition)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  ggtitle(label = 'The Effect of Job Involvement on Attrition Rate')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Percentages')

# If the scope argument is missing the default for direction is "backward". 
backward_model <- step(glmInfo)
forward_model <- step(glmInfo, direction = 'forward', k=2)
stepwise_model <- step(glmInfo, direction = 'both', k=2)

summary(stepwise_model)
confint(stepwise_model)