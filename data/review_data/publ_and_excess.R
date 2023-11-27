library(dplyr)
library(metafor)
library(tidyverse)

# load the df with the extracted data
df=read.delim('../rct_data/extracted_data.csv', sep=',')
df$INFO=paste(df$X.source..id,df$Outcome,df$surgical_arm,df$medical_arm)

# when writing files run this
current_date=Sys.Date()%>%as.character%>%str_replace_all('-','')

# separate continuous and dichotomous outcomes
dataDichotomous  <- df[is.na(df$mean.surg),]
dataContinuous <- df[is.na(df$event.surg),]

dataContinuous  <- dataContinuous[!(dataContinuous$all.surg  <= 1 | 
                                      dataContinuous$all.control  <= 1 | 
                                      dataContinuous$SD.surg  == 0 |  
                                      dataContinuous$SD.control  == 0),]
dataDichotomous <- dataDichotomous[!(dataDichotomous$all.surg  <= 1 | dataDichotomous$all.control  <= 1),]

# compute effect sizes based on summary statistics
tempEffectCalcContinuous <- with(
  dataContinuous,
  metafor::escalc(
    measure = "SMD",
    m1i     = mean.surg,
    m2i     = mean.control,
    n1i     = all.surg,
    n2i     = all.control,
    sd1i    = SD.surg,
    sd2i    = SD.control
  ))
# switched up the 1 and 2 above

tempEffectCalcDichotomous <- with(
  dataDichotomous,
  metafor::escalc(
    measure = "OR",
    ai      = event.surg,
    ci      = event.control,
    n1i     = all.surg,
    n2i     = all.control
  ))

# convert logOR to Cohen's d
tempEffectCalcDichotomous <- data.frame(
  yi = tempEffectCalcDichotomous$yi * sqrt(3)/pi,
  vi = tempEffectCalcDichotomous$vi * 3/pi^2
)

dataContinuous  <- cbind(dataContinuous,  tempEffectCalcContinuous)
dataDichotomous <- cbind(dataDichotomous, tempEffectCalcDichotomous)

df <- rbind(dataContinuous, dataDichotomous)

# Starting data for downstream TESS calculations
df=df%>%mutate(yi=ifelse(dir_neg=='n',-(yi),yi)) # switches the direction of effect sizes.
df$d = df$yi # Cohen's d here used for both OR and SMD
df$sed = sqrt(df$vi) # standard error of Cohen's d
df$t=df$d/df$sed # t value
df$Precision=1/df$sed # precision

# filters for comparisons with >1 included study
geq2s_idx=df%>%dplyr::count(INFO)%>%filter(n>1)
df_TESS_r <- filter(df, df$INFO %in% geq2s_idx$INFO)

df_TESS_r%>%filter(t>(1.96))%>%nrow()

# DONE prior to this filter
# get the outcomes that are positive and adverse.
# get the direction supporting surgery for all
# make it so that all positive effects in t are pro surgery

# Use the code to get INFO from those where negative means surgery and those were positive means surgery
directionality=read.csv('./summary_data.csv')

# turn the below values positive
low_favoring_surg=directionality%>%filter(direction=='surgical')%>%filter(log(effect)<0)

# keep the below values
high_favoring_surg=directionality%>%filter(direction=='surgical')%>%filter(log(effect)>0)

# turn the below t-values negative
high_favoring_med=directionality%>%filter(direction=='drug')%>%filter(log(effect)>0)

# add n to dir_neg column (new) manually for the ones that need change t-value  
#(for directionality purposes)
c(low_favoring_surg$Outcome, high_favoring_med$Outcome )



TESS=df_TESS_r%>%
  group_split(INFO=factor(INFO))%>%
  map_dfr(~.x%>%dplyr::reframe(
                              # Get baseline information per review comparison
                              INFO=first(INFO),
                              title=first(title),
                              surgical_arm=first(surgical_arm),
                              medical_arm=first(medical_arm),
                              Outcome=first(Outcome),
                              orig_fx_type=first(fx_type),
                              
                              # number of studies
                              k=n(),
                              
                              # heterogeneity statistic
                              tau=as.numeric(rma(yi=d,sei=sed,data=.x,method="DL")['tau2']),
                              #dir_neg=first(dir_neg),
                              
                              # get UWLS mean
                              #UWLS = as.numeric(lm(ifelse(dir_neg=='n',abs(t),t) ~ 0 + Precision)$coefficients),
                              UWLS = as.numeric(lm(t ~ 0 + Precision)$coefficients),
                              
                              # get expected Esigtot and observed SStot significant findings
                              Esigtot=sum(1-pnorm(((1.96*sed-UWLS)/(sed^2+tau)^.5)))  ,
                              
                              # removed the absolute from here 
                              # as it didnt consider directionality
                              #SStot = sum(ifelse(dir_neg=='n',abs(t),t)>1.96)*1,
                              SStot = sum(t>1.96)*1,
                              
                              # Excess statistical significance
                              ESS=(SStot-Esigtot)/k,
                              
                              # Modified TESS test (not the original chisq)
                              TESS= (ESS-.05)/(.0475/k)^.5
                          
                          ))
          
df_TESS_r%>%filter(t<(-1.96))%>%filter(dir_neg!='n')#%>%nrow()
table(tes$dir_neg, useNA='ifany')

TESS%>%write.csv(paste0('./TESS',current_date,'.csv'))

# number of significant excess significance
TESS%>%filter(TESS>1.645)%>%nrow()
TESS%>%nrow()
16/53 #(TESS sign/total outcomes assessed)

# excess significance over all studies.
Esigtot=sum(TESS$Esigtot) # ~54
SStot=sum(TESS$SStot) # 93
ESS=(SStot-Esigtot)/df_TESS_r%>%nrow()# ~0.16
P=SStot/k
pi=Esigtot/k
PSST=(P-pi)/sqrt((pi*(1-pi))/k)

k=245
TESS_all= (ESS-.05)/(.0475/k)^.5 # ~7.95
TESS_all>1.64 # TRUE
pt(q=TESS_all, df=1, lower.tail=TRUE)



# Egger's regression
# 5/296 have 10 or more studies
# therefore we do for 3 or more studies knowing that power will not be enough!

df=read.delim('../rct_data/extracted_data.csv', sep=',')
df$INFO=paste(df$X.source..id,df$Outcome,df$surgical_arm,df$medical_arm)

# separate continuous and dichotomous outcomes
dataDichotomous  <- df[is.na(df$mean.surg),]
dataContinuous <- df[is.na(df$event.surg),]

dat_cat=escalc(measure="RR", ai=event.surg, n1i=all.surg, ci=event.control, n2i=all.control, data=dataDichotomous)
dat_cont=escalc(measure="SMD", m1i=mean.surg, sd1i=SD.surg ,n1i=all.surg, m2i=mean.control,sd2i=SD.control, n2i=all.control, data=dataContinuous)


egg_prep_cat=dat_cat%>%dplyr::count(INFO)%>%filter(n>2)
egg_prep_cont=dat_cont%>%dplyr::count(INFO)%>%filter(n>2)
egg_cat <- filter(dat_cat, dat_cat$INFO %in% egg_prep_cat$INFO)
egg_cont <- filter(dat_cont, dat_cont$INFO %in% egg_prep_cont$INFO)


cat_egg=egg_cat%>%
  group_split(INFO=factor(INFO))%>%
  map_dfr(~.x%>%dplyr::summarise(INFO=first(INFO),
                          title=first(title),
                          surgical_arm=first(surgical_arm),
                          medical_arm=first(medical_arm),
                          Outcome=first(Outcome),
                          fx_type=first(fx_type),
                          EGGer_p=(regtest(yi, vi, data=.x,model="rma")$pval)[1]))

cont_egg=egg_cont%>%
  group_split(INFO=factor(INFO))%>%
  map_dfr(~.x%>%summarise(INFO=first(INFO),
                          title=first(title),
                          surgical_arm=first(surgical_arm),
                          medical_arm=first(medical_arm),
                          Outcome=first(Outcome),
                          fx_type=first(fx_type),
                          EGGer_p=(regtest(yi, vi, data=.x,model="rma")$pval)[1]))

Egger_test=rbind(cat_egg,cont_egg)

write.csv(Egger_test,paste0('./EGGER',current_date,'.csv'))
table(Egger_test$EGGer_p<0.05)
5/31







