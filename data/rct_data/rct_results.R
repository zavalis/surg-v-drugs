library(dplyr)

# Read extracted data per RCT
df=read.csv('./journal_rct.csv')
colnames(df)


# CREATES UNIQUE RCT IDs
df$RCT_ID=paste(df$study,df$year.of.study)

# unique RCTs
unique(df$RCT)%>%length()


# find RCTs across different cochrane reviews that are duplicates
# for the duplicates use extracted_data.csv that has the raw outcome data for discrepancy check
extr=read.csv('./input/extracted_data.csv')
extr$RCT_ID=paste(extr$study,extr$year.of.study)

# to count the number of studies per outcome for the overlapping studies
# for the death or serious complication there is just one comparison (not multiple different surgical arms)
dup_rct=extr%>%group_by(RCT_ID)%>%dplyr::summarise(rev=length(unique(X.source..id)))%>%filter(rev>1)
extr%>%filter(RCT_ID%in% dup_rct$RCT_ID)%>%arrange(RCT_ID,Outcome)%>%write.csv('duplicate_RCTs_assess.csv')

# To get the overall number of outcomes for the manually found overlap
extr%>%filter(X.source..id %in% c('CD007223.PUB4', 'CD012602.PUB2' ))%>%
  filter(Outcome%in%c('Composite outcome of death or serious complication'))#%>%distinct(RCT_ID)

# change between 'Dilatation and curretage', and 'Suction aspiration' to get total of studies
sarm='Dilatation and curretage' # 4 studies 
sarm='Suction aspiration' # 24 studies
extr%>%filter(X.source..id %in% c( 'CD007223.PUB4','CD012602.PUB2' ))%>%
  filter(Outcome%in%c('Complete miscarriage\n'))%>%
  filter(surgical_arm==sarm)%>%
  group_by(X.source..id)%>%
  dplyr::summarise(rev=length((X.source..id)))


# Get for extraction of RoB assessment
df%>%distinct(RCT_ID, .keep_all = T)%>%select(c(X.source..id,RCT_ID,journal))%>%write.csv('./RoB_RCTs.csv')
unique(df$RCT_ID)%>%write.csv('./RoB_RCTs.csv')
nrow(df)

# removes duplicate RCTs, which is used for journal calculations overall
RCTs=df[!duplicated(df$RCT_ID),]

# shows the distribution of journal focus for unique RCTs
table(RCTs$journal_type)

# this is done to get the no. of comparison for each journal focus
table(df$journal_type)


# Direction of effects based on journal focus for the COMPARISONS
# (this as diffferent reviews use different outcomes)
## MOSTLY SURGICAL
table(df%>%filter(journal_type=='mostly surgical')%>%select(direction))
table(df%>%filter(journal_type=='mostly surgical')%>%select(direction))/133

## GENERAL
table(df%>%filter(journal_type=='general')%>%select(direction))
table(df%>%filter(journal_type=='general')%>%select(direction))/69

## MOSTLY NON-SURGICAL
table(df%>%filter(journal_type=='mostly non-surgical')%>%select(direction))
table(df%>%filter(journal_type=='mostly non-surgical')%>%select(direction))/93

# Fishers' exact test of association of journal type to direction of effects
fisher.test(table(df$journal_type, df$direction))

# Fishers' exact test of association of journal type to direction of effects
# this is the same but using the classifying all with not a mostly surgical focus
# as "other".
temp=df
temp['journal_type'][temp$journal_type!='mostly surgical',]='other'
fisher.test(table(temp$journal_type, temp$direction))
