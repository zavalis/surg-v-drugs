library(dplyr)

# Read extracted data
df=read.csv('./input/outcomes_RCT_basis.csv')
colnames(df)

# CREATES UNIQUE RCT IDs
df$RCT_ID=paste(df$study,df$year.of.study)

# Number of unique RCTs
length(unique(df$RCT_ID)) 
nrow(df)

# creates the df
df%>%group_by(RCT_ID)%>%
  summarise(journal=journal)

# removes duplicate RCTs
RCTs=df[!duplicated(df[c(20)]),]

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
