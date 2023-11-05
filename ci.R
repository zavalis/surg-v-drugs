# Confidence intervals throughout the paper
library(fastR2)
library(dplyr)
# Rate of sequestration
a=0.995
wilson.ci(41,n=187,conf.level=a)
41/187

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
df_summary=read.csv('./input/summary_data.csv')
df_all=read.csv('./input/included_studies.csv')
dat_contain=unique(df_summary$X.source..id)
df_all=df_all%>%mutate(Dat=ifelse(X.source..id %in% dat_contain,1,0))
table(df_all$specialty, df_all$Dat)

power.chisq.test(ncp = 15, df = 20,
                 alpha = 0.05, plot = TRUE)

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
df_reanal=read.csv('./input/re_meta_manual.csv')
table(df_reanal$direction_re)

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

# Search date
median(df_corr_au$latest_search_date)
IQR(df_corr_au$latest_search_date)

# RCT publication
RCT=df_data[!(duplicated(df_data$IDstudy)),]

median(RCT$year.of.study)
IQR(RCT$year.of.study)



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

# for interrater reliability
irr=read.csv('./manual_review/abstract_screening_combined.csv')
library(irr)
x=irr %>% filter_all(any_vars(. %in% c(1)))
nrow(x[x['jockew1989']==x['zavalis'],])/nrow(x)

# agreement on the reviews coded by either as exclude
x=irr %>% filter_all(any_vars(. %in% c(2)))
nrow(x[x['jockew1989']==x['zavalis'],])/nrow(x)
check=x[x['jockew1989']!=x['zavalis'],]
table(check$zavalis,check$jockew1989)

x=irr %>% filter_all(any_vars(. %in% c(3)))
nrow(x[x['jockew1989']==x['zavalis'],])/nrow(x)


x=irr %>% filter_all(any_vars(. %in% c(1,3)))
y=irr %>% filter_all(any_vars(. %in% c(2)))
x['zavalis']='1'
x['jockew1989']='1'
joined=rbind(x,y)
joined=(joined[!duplicated(joined$X.source..id),])


kappa2(joined[, c("zavalis", "jockew1989")], weight = "unweighted")


