library(dplyr)
library(readr)


# METHODS
# we automatically removed the articles that did not contain surg* in their 
# abstract and removed the ones mentioning 'Reasons for withdrawal from publication'
# independent screening was conducted and we resolved for the common inclusions
# and the common unclear cases that were subsequently discussed for inclusion or exclusion
# (if any) the remainder of unclear cases were forwarded to JPAI for final judgement

df=read.csv('./labels.csv')

df=df[!grepl('Reason for withdrawal from publication',df$abstract),]
dfnsurg=df[!grepl('surg',df$abstract)& grepl('surg',df$keywords),]
df=df[grepl('surg',df$abstract),]

recheck=read.csv('~/Downloads/redo_new.csv')
new=(recheck[recheck$zavalis==1,])
write.csv(new,'wooooow.csv')

write.csv(dfnsurg,'~/Downloads/redo_new.csv')
write.csv(x, 'abstract_screening_blank.csv' )

write.csv(dfnsurg[sample(nrow(dfnsurg), 20), ],'~/Downloads/checkagain.csv')
