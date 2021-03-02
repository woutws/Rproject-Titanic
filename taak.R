#team assignment titanic fs4 (Aaron Roofthoofd, Wout Sips, Seppe Van Heurck)

#Install required 
#Section install packages
if(!require(dplyr)) {
  install.packages("dplyr")
}
if(!require(ggplot2)) {
  install.packages("ggplot2")
}
if(!require(readr)) {
  install.packages("readr")
}
if(!require(scales)){
  install.packages("scales")
}
if(!require(data.table)){
  install.packages("data.table")
}
#endsection
#comment
#Load libraries
#section libraries
library(scales)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
#endsection

#Section Read csv and clean data
titanic<-read_csv('train.csv',
  col_types = list(col_double(),
                   col_double(),
                   readr::col_factor(),
                   col_character(),
                   readr::col_factor(),
                   col_double(),
                   col_double(),
                   col_double(),
                   col_character(),
                   col_double(),
                   col_character(),
                   readr::col_factor()),col_names = c("PassengerId","Survived","Class","Name","Sex","Age","Siblings_and_spouse","Parents_children",
                                               "Ticket","Fare","Cabin","Embarked"),skip=1)

#discard variables
titanic<-titanic %>%
  select(-Fare,-Ticket,-Name,-Parents_children)

#Order classes 
titanic$Class <- factor(titanic$Class, 
                       ordered = T, 
                       levels = c(1,2,3))

#make age classes
titanic <- titanic %>% mutate(Agegroup = case_when(Age <= 21  ~ 'child',
                                             Age > 21  ~ 'adult'))
#change survived to true and false
titanic<- mutate(titanic, Survived = as.logical(Survived))
#endsection

#Survival based on age group and gender
ggplot(data = titanic) + geom_bar(mapping = aes(x = Survived,fill=Sex),position="dodge") + facet_wrap(~Agegroup)

#this graph is the same but visualized differently
#ggplot(data = titanic) + geom_bar(mapping = aes(x= Agegroup, fill = Survived),position = "fill")


#Overview age per class

ggplot(data=titanic) + geom_histogram(mapping = aes(x = Age, fill= Class),color='black', binwidth = 10,position = "dodge")


#Survival based on boarding place
ggplot(data=titanic) + geom_bar(mapping = aes(x = Survived,fill=Embarked),position = "dodge")

#Boarding based on classes
ggplot(data=titanic) + geom_bar(mapping = aes(x = Class,y = (stat(count))/sum(stat(count)),fill=Embarked), position = "dodge")+ scale_y_continuous(labels=percent) + labs( y = "Percentage", x = "Class")

 
#Survival based on age
ggplot(data=titanic) + geom_histogram(mapping = aes(x = Age,fill=Survived),color='black',binwidth = 5,position = "dodge")


#Overview Classes and number of siblings and spouses
ggplot(data = titanic) + geom_bar(mapping = aes(x=Siblings_and_spouse, y = (..count..)/sum(..count..) ,fill=Class),position="dodge")+ scale_y_continuous(labels=percent) + labs( y = "Percentage", x = "Siblings and spouse")


# Survival based on number of siblings and spouses
# ..count.. returned by a stat transformation of the original data set

ggplot(data = titanic) + geom_bar(mapping = aes(x=Siblings_and_spouse,y = (..count..)/sum(..count..) ,fill=Survived),position = 'dodge')+ scale_y_continuous(labels=percent) + labs( y = "Percentage", x = "Siblings and spouse")






