#install.packages("epitools")

library(metafor)
library(epitools)
library(meta)
library(dplyr)
library(tidyr)
library(plyr)

# Read the csv of extracted data:
df=read.delim('../rct_data/extracted_data.csv', sep=',')

# creates a unique comparison identifier column named INFO
df$INFO=paste(df$X.source..id,df$Outcome,df$surgical_arm,df$medical_arm)

# divide the data into a group w/ categoric variables and continuous
## CATEGORIC
df_nonmd<-subset(df, event.surg!="NA")

## CONTINUOUS
df_md=subset(df, mean.surg!="NA")

# Changes the output it is not presented in scientific notation
options(scipen=999)

library(tidyverse)

# Using the same type of effect size as in the original meta-analysis the 
# meta-analyses are reperformed the saved outputs are the comparison information, 
#effect type, effect, and the confidence interval of the effect

## CONTINUOUS EFFECT TYPE META-ANALYSIS
cont_ma=df_md%>%
  group_split(INFO=factor(INFO))%>%
  map_dfr(~.x%>%summarise(INFO=first(INFO),
                                 title=first(title),
                                 surgical_arm=first(surgical_arm),
                                 medical_arm=first(medical_arm),
                                 Outcome=first(Outcome),
                                 fx_type=first(fx_type),
                                 effect=(unlist(summary(metacont(data=.x,
                                                                 n.e=all.surg,
                                                          mean.e=mean.surg,
                                                          sd.e=SD.surg, 
                                                          n.c=all.control,
                                                          mean.c=mean.control,
                                                          sd.c=SD.control,
                                                          sm=first(fx_type),
                                                          method.random.ci = "HK"
                                                          )
                                                          )$random)[1]),
                                 
                                 ci.lb=(unlist(summary(metacont(data=.x,
                                                                n.e=all.surg,
                                                         mean.e=mean.surg,
                                                         sd.e=SD.surg, 
                                                         n.c=all.control,
                                                         mean.c=mean.control,
                                                         sd.c=SD.control,
                                                         sm=first(fx_type),
                                                         method.random.ci = "HK"))$random)[3]),
                                 
                                 ci.ub=(unlist(summary(metacont(data=.x,
                                                                n.e=all.surg,
                                                         mean.e=mean.surg,
                                                         sd.e=SD.surg, 
                                                         n.c=all.control,
                                                         mean.c=mean.control,
                                                         sd.c=SD.control,
                                                         sm=first(fx_type),
                                                         method.random.ci = "HK"))$random)[4]),
                                 
                                 I2=(unlist(summary(metacont(data=.x,
                                                             n.e=all.surg,
                                                         mean.e=mean.surg,
                                                         sd.e=SD.surg, 
                                                         n.c=all.control,
                                                         mean.c=mean.control,
                                                         sd.c=SD.control,
                                                         sm=first(fx_type),
                                                         method.random.ci = "HK")))[['I2']])))



## CATEGORICAL EFFECT TYPE META-ANALYSIS
cat_ma=df_nonmd%>%
  group_split(INFO=factor(INFO))%>%
  map_dfr(~.x%>%summarise(INFO=first(INFO),
                          title=first(title),
                          surgical_arm=first(surgical_arm),
                          medical_arm=first(medical_arm),
                          Outcome=first(Outcome),
                          fx_type=first(fx_type),
                          
                          effect=exp(unlist(summary(metabin(data=.x,
                                                            event.surg,
                                                            all.surg, 
                                                            event.control,
                                                            all.control, 
                                                            method='MH',
                                                            method.random.ci = "HK", 
                                                            sm=first(fx_type)))$random)[1]),
                          
                          ci.lb=exp(unlist(summary(metabin(data=.x,
                                                           event.surg,
                                                           all.surg, 
                                                           event.control,
                                                           all.control, 
                                                           method='MH',
                                                           method.random.ci = "HK",
                                                           sm=first(fx_type)))$random)[3]),
                          
                          ci.ub=exp(unlist(summary(metabin(data=.x,
                                                           event.surg,
                                                           all.surg, 
                                                           event.control,
                                                           all.control, 
                                                           method='MH',
                                                           method.random.ci = "HK",
                                                           sm=first(fx_type)))$random)[4]),
                         
          
                          I2=(unlist(summary(
                                             metabin(data=.x,
                                                        event.surg,
                                                        all.surg, 
                                                        event.control,
                                                        all.control, 
                                                        method='MH',
                                                        method.random.ci = "HK",
                                                        sm=first(fx_type)))[['I2']]))))

re_ma=rbind(cat_ma,cont_ma)


# get i2 median and IQR
i2_rema=re_ma%>%filter(I2!='NA')
median(i2_rema$I2)
quantile(i2_rema$I2, probs = c(0.25,0.75))

# to write it to then manually assess and call re_meta.csv
write.csv(re_ma,'re_meta.csv')
# Following this the effects are classified according to direction 
# (either inconclusive, favoring surgery or favoring drugs)


# SOME REVIEWS NEEDED TO BE RE-ANALYZED (USING THE REVIEWS' ORIGINAL METHODS) 
# TO GET THE SUMMARY EFFECT FOR THE INTERVENTION AS THE SUMMARY EFFECT OF THE 
# PRIMARY OUTCOMES WASN'T PRESENTED
# Their CDSR-IDs are CD001802.PUB3, CD001081.PUB4, CD012602.PUB2, CD009526.PUB2
## Example of how this was performed:
temp=df_nonmd[df_nonmd['X.source..id']=='CD012602.PUB2',]
temp=df_nonmd%>%filter(X.source..id=='CD012602.PUB2')
temp=temp[temp['Outcome']=='Complete miscarriage\n',]
temp=temp[temp['medical_arm']=='Misoprostol and mifepristone',]

# FOR BINARY OUTCOMES
summary(metabin(event.surg,all.surg, event.control,all.control,data=temp, sm='RR', random=TRUE, method='MH'))$random


# FOR CONTINUOUS OUTCOMES
summary(metacont(n.e=all.surg,mean.e=mean.control,sd.e=SD.surg, n.c=all.control,mean.c=mean.surg,sd.c=SD.control, data=temp,
         sm='SMD'))$random








