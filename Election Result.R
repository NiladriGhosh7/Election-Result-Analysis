primary_data = read.csv("primary_winner.csv")
county_data = read.csv("county_data.csv")



library(plyr)
library(caTools)

# Inner Join the datasets
install.packages("dplyr")
library(dplyr)

joined=join(primary_data,county_data,type="inner",by = "fips")

# Handling charracter variables
joined$state_abbreviation=factor(joined$state_abbreviation,levels = c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
                                       ,labels = c(1:49))

# Grouping the data by winner

primary_data %>% 
  group_by(winner) %>% 
  summarise(number_wins = n())

split=sample.split(joined$winner,SplitRatio=0.8)
training_set=subset(joined,split==TRUE)
testing_set=subset(joined,split==FALSE)

training_set %>% 
  group_by(winner) %>% 
  summarise(percent_won = n()/nrow(training_set))
testing_set %>% 
  group_by(winner) %>% 
  summarise(percent_won = n()/nrow(testing_set))

levels(joined$winner)
levels(joined$party)

attach(joined)

#Logistic Regression
classifier = glm(formula=winner~state_abbreviation+
                   age_over_65+
                   female_perc+
                   foreign_born_perc+
                   bachelors_perc+
                   household_income,family=binomial,data = joined)

summary(classifier)

classifier1 = glm(formula=winner~age_over_65+
                   female_perc+
                   foreign_born_perc+
                   bachelors_perc+
                   household_income,family=binomial,data = joined)

summary(classifier1)

classifier2 = glm(formula=winner~age_over_65+
                    female_perc+
                    foreign_born_perc+
                    bachelors_perc+
                    household_income,family=binomial,data = joined)

summary(classifier2)

classifier3 = glm(formula=winner~female_perc+
                    foreign_born_perc+
                    bachelors_perc+
                    household_income,family=binomial,data = joined)

summary(classifier3)

classifier_optimised = glm(formula=winner~female_perc+
                    foreign_born_perc+
                    bachelors_perc,family=binomial,data = joined)

summary(classifier_optimised)

#Predicting the probablity of training set using different threshold & making confusion matrix

prob_pred=predict(classifier_optimised,type = "response",newdata = training_set)
y_pred=ifelse(prob_pred>0.4,"Hilary Clinton","Bernie Sanders")
cm= table(training_set$winner,y_pred)
cm_predpercentage = (sum(diag(cm))/sum(cm))*100
print(cm)
print(cm_predpercentage)

prob_pred=predict(classifier_optimised,type = "response",newdata = training_set)
y_pred=ifelse(prob_pred>0.5,"Hilary Clinton","Bernie Sanders")
cm= table(training_set$winner,y_pred)
cm_predpercentage = (sum(diag(cm))/sum(cm))*100
print(cm)
print(cm_predpercentage)

prob_pred=predict(classifier_optimised,type = "response",newdata = training_set)
y_pred=ifelse(prob_pred>0.6,"Hilary Clinton","Bernie Sanders")
cm= table(training_set$winner,y_pred)
cm_predpercentage = (sum(diag(cm))/sum(cm))*100
print(cm)
print(cm_predpercentage)

prob_pred=predict(classifier_optimised,type = "response",newdata = training_set)
y_pred=ifelse(prob_pred>0.7,"Hilary Clinton","Bernie Sanders")
cm= table(training_set$winner,y_pred)
cm_predpercentage = (sum(diag(cm))/sum(cm))*100
print(cm)
print(cm_predpercentage)

#Predicting the probablity of testing set using different threshold & 
#making confusion matrix using optimised threshold

prob_pred=predict(classifier_optimised,type = "response",newdata = testing_set)
y_pred=ifelse(prob_pred>0.6,"Hilary Clinton","Bernie Sanders")
cm= table(testing_set$winner,y_pred)
cm_predpercentage = (sum(diag(cm))/sum(cm))*100
print(cm)
print(cm_predpercentage)