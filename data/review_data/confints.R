# Confidence intervals throughout the paper
library(fastR2)
library(dplyr)
# Rate of sequestration
a=0.995
wilson.ci(41,n=187,conf.level=a)
41/188

# Rate of favoring surgery
wilson.ci(36,n=103,conf.level=a)
36/103

# Rate of favoring drugs
wilson.ci(15,n=103,conf.level=a)

15/103
table(df_summary$direction)

# Rate of direction being inconclusive
wilson.ci(52,n=103,conf.level=a)
52/103

# read summary data csv
df_summary=read.csv('./summary_data.csv')
df_all=read.csv('./included_studies.csv')
dat_contain=unique(df_summary$X.source..id)
df_all=df_all%>%mutate(Dat=ifelse(X.source..id %in% dat_contain,1,0))
table(df_all$specialty, df_all$Dat)

# Rate of directions stratified for outcome type
## mortality
table(df_summary%>%filter(outcome_type=='mortality')%>%select(direction))
table(df_summary%>%filter(outcome_type=='mortality')%>%select(direction))/12

## composite
table(df_summary%>%filter(outcome_type=='composite')%>%select(direction))
table(df_summary%>%filter(outcome_type=='composite')%>%select(direction))/11

##non-mortality
table(df_summary%>%filter(outcome_type=='nonmortality')%>%select(direction))
table(df_summary%>%filter(outcome_type=='nonmortality')%>%select(direction))/80

# Same for reanalysis

# to check in the table again
View(df_reanal%>%filter(direction_re=='drug'))

## mortality
table(df_reanal%>%filter(outcome_type=='mortality')%>%select(direction_re))
table(df_reanal%>%filter(outcome_type=='mortality')%>%select(direction_re))/12

## composite
table(df_reanal%>%filter(outcome_type=='composite')%>%select(direction_re))
table(df_reanal%>%filter(outcome_type=='composite')%>%select(direction_re))/11

##non-mortality
table(df_reanal%>%filter(outcome_type=='nonmortality')%>%select(direction_re))
table(df_reanal%>%filter(outcome_type=='nonmortality')%>%select(direction_re))/80


table(df_reanal%>%filter(GRADE=='Moderate')%>%select(direction_re))
table(df_reanal%>%filter(GRADE=='Moderate')%>%select(direction_re))/22
table(df_reanal%>%filter(GRADE=='High')%>%select(direction_re))


# G
x=65
y=30
z=8  
wilson.ci(65,n=103,conf.level=a)
wilson.ci(30,n=103,conf.level=a)
wilson.ci(8,n=103,conf.level=a)
x/103
y/103
z/103

df_corr_au=read.csv('./characteristics_of_reviews.csv')

# Search date
median(df_corr_au$latest_search_date)
IQR(df_corr_au$latest_search_date)



# The rate of high GRADE evidence
x=nrow(df_summary%>%filter(GRADE.assessment=='High'))
wilson.ci(x,n=103,conf.level=a)
x/103

# The rate of moderate GRADE evidence
x=nrow(df_summary%>%filter(GRADE.assessment=='Moderate'))
x=(df_summary%>%filter(GRADE.assessment=='Moderate'))

wilson.ci(x,n=103,conf.level=a)
x/103

# The rate of low GRADE evidence
x=nrow(df_summary%>%filter(GRADE.assessment=='Low'))
wilson.ci(x,n=103,conf.level=a)
x/103

# The rate of very low GRADE evidence
x=nrow(df_summary%>%filter(GRADE.assessment=='Very Low'))
wilson.ci(x,n=103,conf.level=a)
x/103

# The rate of no assessment
x=nrow(df_summary%>%filter(GRADE.assessment==''))
wilson.ci(x,n=103,conf.level=a)
x/103




# RoB percentages
library(readxl)
library(tidyverse)
rob_df=readxl::read_excel('../rct_data/RoB_RCTs_08112023.xlsx')
rob_pivot=rob_df[,5:19] %>%
  pivot_longer(cols=c(1:15),names_to = "income", values_to = "cat")

summarize=rob_pivot%>%filter(!is.na(cat))%>%group_by(income)%>%
  summarise(high=sum(cat=='high risk')*1, 
            low=sum(cat=='low risk')*1,
            unclear=sum(cat=='unclear')*1,
            total=high+low+unclear,
            high=paste0(high,'/',total,'(',round(high/total,2)*100,')'),
            low=paste0(low,'/',total,'(',round(low/total,2)*100,')'),
            unclear=paste0(unclear,'/',total,'(',round(unclear/total,2)*100,')')
            )
rob_sum=t(summarize%>%select(-total))%>%as.data.frame()
names(rob_sum) <- rob_sum[1,]
rob_sum=rob_sum[2:4,]

current_date=Sys.Date()%>%as.character%>%str_replace_all('-','')
rob_sum%>%write.csv(paste0('./ROBSTATS',current_date,'.csv'))
