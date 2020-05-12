#___________________________________________________________________________________________________________________
#________________________________________________Exploring Employees’ Attrition __________________________________________________
#___________________________________________________________________________________________________________________

install.packages("ggpubr")
install.packages("wesanderson")
library(wesanderson)
library(dslabs)
library(tidyverse)    
library(ggplot2)
library(dplyr)
library(GGally)
library(rpart)
library(rpart.plot)
library(ggpubr)
# Imort the Data
df=read.csv('~/Desktop/Rfiles/IBMdata.csv')
#  attach the dataset to access variables localy in R 
attach(df)
#explore the data structure
str(df)

#===================Integer to Factor===========================
#convert int variables to ordered factors 
names <- c('RelationshipSatisfaction', 'PerformanceRating', 'WorkLifeBalance', 
           'JobInvolvement', 'JobSatisfaction', 'JobLevel', 'StockOptionLevel')
df[,names] <- lapply(df[,names] , factor, ordered = TRUE)
str(df)

# explore missing values in each columns
sapply(df, function(x) sum(is.na (x)))

#compute the frequencies of each unique values in Marital status column and save it into object maritaltab.
maritaltab <- table(df$MaritalStatus)
maritaltab

#=============================Bar Plot=============================
#Create a bar plot for Attrition
df %>%
  group_by(Attrition) %>%
  tally() %>%
  ggplot(aes(x = Attrition, 
             y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Attrition",  
       y="Count of Attriation")+
  ggtitle("Attrition")+
  geom_text(aes(label = n), 
            vjust = -0.5, 
            position = position_dodge(0.9))
#=========================Compare Densities========================

# Define Involvement labels for each level 
df <- df %>%
  mutate(group = case_when(
    .$JobInvolvement == 1 ~ "Barely Involved",
    .$JobInvolvement == 2 ~ "Slightly Involved",
    .$JobInvolvement == 3 ~ "Involved",
    .$JobInvolvement == 4 ~ "Very Involved"))

# define years with current manager as variable for ploting
ywmngr_atr <- df %>%
  ggplot(aes(YearsWithCurrManager, fill = group)) +
  scale_x_continuous(trans = "log10") +
  ggtitle("Attrition Based on Year with Current Manager and Involvement") + theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#386CB0", size = 16),
        axis.text = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))
# stacked density plot
ywmngr_atr + geom_density(alpha = 0.3, bw = 0.75, position = "stack") +
  facet_grid(Attrition ~ .) 

#=======================Density Plot================================

# Define Jobsatisfaction labels for each level 
df <- df %>%
  mutate(Job_Satisfaction = case_when(
    .$JobSatisfaction == 1 ~ "Not Satisfied",
    .$JobSatisfaction == 2 ~ "Slightly Satisfied",
    .$JobSatisfaction == 3 ~ "Satisfied",
    .$JobSatisfaction == 4 ~ "Very Satisfied"))
# define the variables for ploting
incm_atr <- df %>%
  ggplot(aes(MonthlyIncome, fill = Job_Satisfaction)) +
  scale_x_continuous(trans = "log2") + 
  scale_fill_manual(values=c("#D53E4F", "#ABDDA4", "#2166AC", "#FFFF33")) +
  ggtitle("Attrition Based on Monthly Income and Job Satisfaction") + theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#386CB0", size = 16),
        axis.text = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  geom_vline(aes(xintercept= mean(MonthlyIncome)),
             linetype="dashed", color = "blue")
# stack density plots
incm_atr + geom_density(alpha = 0.3, bw = 0.75, position = "stack") +
  facet_grid(Attrition ~ .) 

#=======================Stacked Barplots===========================
# Define the plots 
travel_atr <- ggplot(df, aes(BusinessTravel,fill=Attrition))+geom_bar()+scale_fill_manual(values=c("#2166AC", "#B3CDE3" ))
dep_atr <- ggplot(df, aes(Department,fill = Attrition))+geom_bar()+scale_fill_manual(values=c("#2166AC", "#B3CDE3" ))
dist_atr <- ggplot(df,aes(DistanceFromHome,fill=Attrition))+geom_bar()+scale_fill_manual(values=c("#2166AC", "#B3CDE3" ))
#arragne plots side by side
ggarrange(travel_atr, dep_atr, dist_atr + rremove("x.text"),
          labels = c("Business Travel", "Department", "Distance from Home"),
          ncol = 3, nrow = 1)

#=======================
# Define the plots 
eduPlot <- ggplot(df,aes(Education,fill=Attrition))+geom_bar()+scale_fill_manual(values=c("#BEAED4", "#FFFF99"))+theme_bw(base_family = "Times")
edufieldPlot <- ggplot(df,aes(EducationField,fill=Attrition))+geom_bar()+scale_fill_manual(values=c("#BEAED4", "#FFFF99"))+theme_bw(base_family = "Times")
envPlot <- ggplot(df,aes(EnvironmentSatisfaction,fill=Attrition))+geom_bar()+scale_fill_manual(values=c("#BEAED4", "#FFFF99"))+theme_bw(base_family = "Times")
genPlot <- ggplot(df,aes(Gender,fill=Attrition))+geom_bar()+scale_fill_manual(values=c("#BEAED4", "#FFFF99"))+theme_bw(base_family = "Times")
# arrange the plots in 2 rows and 2 columns
ggarrange(eduPlot,edufieldPlot,envPlot,genPlot + rremove("x.text"), 
          ncol = 2, nrow = 2) 

#========================Scatter Plot==============================
#explore the relationship between age and years at company while comparing Gender
tnr_age <- ggplot(df, aes(Age, YearsAtCompany, shape = Gender, colour = Gender)) + geom_point(size=3) + theme_test() + 
  ggtitle("Age vs Tenure for Male and Female") + theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#386CB0", size = 16)) 
print(tnr_age + scale_shape_manual(values = c(18, 21)) + 
        scale_colour_manual(values = c("#7570B3", "#1B9E77")))
#explore the relationship between age and year at company while comparing maritalstatus
mrate_tnr <- ggplot(df, aes(MonthlyRate, YearsAtCompany, color = MaritalStatus)) + 
  scale_x_continuous(trans = "log10") + 
  geom_point(size = 2) + theme_test() + 
  ggtitle("Monthly Rate vs Tenure for Marital Status") + theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#386CB0", size = 16)) 
mrate_tnr
#arragne plots in rows and columns
ggarrange(tnr_age, mrate_tnr + rremove("x.text"),
          ncol = 2, nrow = 1)
#===========================Box Plot=================================
set_palette(p, "Set1")
p <- df %>%
  ggplot(aes(JobRole, YearsAtCompany, fill = JobRole)) + theme_bw() +  
  geom_boxplot() +
  ggtitle("Year at Company for Departments") + 
  theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#386CB0", size = 16)) +
  xlab("")
p







#================================




























