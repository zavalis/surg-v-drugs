library(dplyr)
library(readr)


# METHODS
# we automatically removed the articles that did not contain surg* in their 
# abstract and removed the ones mentioning 'Reasons for withdrawal from publication'
# independent screening was conducted and we resolved for the common inclusions
# and the common unclear cases that were subsequently discussed for inclusion or exclusion
# (if any) the remainder of unclear cases were forwarded to JPAI for final judgement

df=read.csv('./labels.csv')
# remove the withdrawn publications
df=df[!grepl('Reason for withdrawal from publication',df$abstract),]
# to check such with no mention of surg or any of its variation in the abstract (manually)
dfnsurg=df[!grepl('surg',df$abstract),]
# filter for such that mention surg
df=df[grepl('surg',df$abstract),]

write.csv(df,'for_manual_screening.csv')

# INTER-RATER RELIABILITY
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

# testing to turn all the include and unsure as include.
x=irr %>% filter_all(any_vars(. %in% c(1,3)))
y=irr %>% filter_all(any_vars(. %in% c(2)))
x['zavalis']='1'
x['jockew1989']='1'
joined=rbind(x,y)
joined=(joined[!duplicated(joined$X.source..id),])

kappa2(joined[, c("zavalis", "jockew1989")], weight = "unweighted")
