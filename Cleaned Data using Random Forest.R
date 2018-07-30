H1B_Visa = read.csv(file.choose())

library(dplyr)
library(data.table)
library(caret)
library(randomForest)

H1B_Clean = H1B_Visa
names(H1B_Clean)
 
H1B_Clean<-H1B_Clean%>%
  filter(VISA_CLASS %in% "H-1B")

H1B_Clean<-H1B_Clean%>%
  mutate(PREVAILING_WAGE = case_when(PW_UNIT_OF_PAY == 'Hour' ~ PREVAILING_WAGE*2080,PW_UNIT_OF_PAY == 'Year' ~
                                       PREVAILING_WAGE,PW_UNIT_OF_PAY=='Week'~PREVAILING_WAGE*52,PW_UNIT_OF_PAY=='Bi-Weekly'~PREVAILING_WAGE*26))

H1B_Clean<-H1B_Clean%>%
    filter(PREVAILING_WAGE > 5000 & (PREVAILING_WAGE < 200000))

H1B_Clean=H1B_Clean[-c(1,5,8,9,10,11,13,14,15,16,17,18,19,20,21,23,25,26:32,36,38,39,41,43:49,51,52)]

H1B_Clean=H1B_Clean[complete.cases(H1B_Clean),]

H1B<- H1B_Clean

H1B$CASE_SUBMITTED=as.Date(H1B$CASE_SUBMITTED,"%m/%d/%Y")
H1B$DECISION_DATE=as.Date(H1B$DECISION_DATE,"%m/%d/%Y")
H1B$EMPLOYMENT_START_DATE=as.Date(H1B$EMPLOYMENT_START_DATE,"%m/%d/%Y")
H1B$EMPLOYMENT_END_DATE=as.Date(H1B$EMPLOYMENT_END_DATE,"%m/%d/%Y")

H1B$SUBMIT_DECISION=abs(H1B$DECISION_DATE-H1B$CASE_SUBMITTED)
H1B$START_END=abs(H1B$EMPLOYMENT_START_DATE-H1B$EMPLOYMENT_END_DATE)
H1B$START_DECISION=abs(H1B$EMPLOYMENT_START_DATE-H1B$DECISION_DATE)
H1B$END_DECISION=abs(H1B$EMPLOYMENT_END_DATE-H1B$DECISION_DATE)
H1B$SUBMIT_END=abs(H1B$CASE_SUBMITTED-H1B$EMPLOYMENT_END_DATE)
H1B$SUBMIT_START=abs(H1B$CASE_SUBMITTED-H1B$EMPLOYMENT_START_DATE)

H1B$CASE_SUBMITTED=format(as.Date(H1B$CASE_SUBMITTED,"%m/%d/%Y"),"%Y")
H1B$DECISION_DATE=format(as.Date(H1B$DECISION_DATE,"%m/%d/%Y"),"%Y")
H1B$EMPLOYMENT_START_DATE=format(as.Date(H1B$EMPLOYMENT_START_DATE,"%m/%d/%Y"),"%Y")
H1B$EMPLOYMENT_END_DATE=format(as.Date(H1B$EMPLOYMENT_END_DATE,"%m/%d/%Y"),"%Y")

summary(H1B)
unique(H1B$CASE_STATUS)



H1B$SUBMIT_DECISION=as.integer(H1B$SUBMIT_DECISION)
H1B$START_END=as.integer(H1B$START_END)
H1B$START_DECISION=as.integer(H1B$START_DECISION)
H1B$END_DECISION=as.integer(H1B$END_DECISION)
H1B$SUBMIT_END=as.integer(H1B$SUBMIT_END)
H1B$SUBMIT_START=as.integer(H1B$SUBMIT_START)

names(H1B)


H1B=H1B[-c(7,11,14)]

H1B$SOC_NAME = NA  
H1B <- within(H1B, SOC_NAME[SOC_NAME=="Computer Systems Analysts" | SOC_NAME=="Computer Programmers" | grepl("Engineers", SOC_NAME) | SOC_NAME=="Software Developers, Applications"] <- 1)
H1B$SOC_NAME[is.na(H1B$SOC_NAME)] <- 0


H1B_Trial = H1B

H1B_Trial$CASE_SUBMITTED<-as.factor(H1B_Trial$CASE_SUBMITTED)
H1B_Trial$DECISION_DATE<-as.factor(H1B_Trial$DECISION_DATE)
H1B_Trial$EMPLOYMENT_START_DATE<-as.factor(H1B_Trial$EMPLOYMENT_START_DATE)
H1B_Trial$EMPLOYMENT_END_DATE<-as.factor(H1B_Trial$EMPLOYMENT_END_DATE)

H1B_Trial<-H1B_Trial[!(H1B_Trial$EMPLOYER_STATE == "" | H1B_Trial$EMPLOYER_STATE == "FM" | H1B_Trial$EMPLOYER_STATE == "MP" | H1B_Trial$EMPLOYER_STATE == "FM" | H1B_Trial$EMPLOYER_STATE == "VI" | H1B_Trial$EMPLOYER_STATE == "AS" | H1B_Trial$EMPLOYER_STATE == "PW"), ]
H1B_Trial<-H1B_Trial[!(H1B_Trial$WORKSITE_STATE == "" | H1B_Trial$WORKSITE_STATE == "FM" | H1B_Trial$WORKSITE_STATE == "MP" | H1B_Trial$WORKSITE_STATE == "FM" | H1B_Trial$WORKSITE_STATE == "VI" | H1B_Trial$WORKSITE_STATE == "AS" | H1B_Trial$WORKSITE_STATE == "PW"), ]

H1B_Trial<-H1B_Trial[-c(7)]
H1B_Trial$PREVAILING_WAGE<-as.numeric(H1B_Trial$PREVAILING_WAGE)


H1B_Trial$EMPLOYER_STATE <- as.factor(H1B_Trial$EMPLOYER_STATE)
H1B_Trial$EMPLOYER_STATE<-droplevels(H1B_Trial$EMPLOYER_STATE)
H1B_Trial$WORKSITE_STATE <- as.factor(H1B_Trial$WORKSITE_STATE)
H1B_Trial$WORKSITE_STATE<-droplevels(H1B_Trial$WORKSITE_STATE)
H1B_Trial$CASE_STATUS <- as.factor(H1B_Trial$CASE_STATUS)
H1B_Trial$CASE_STATUS<-droplevels(H1B_Trial$CASE_STATUS)

smp_size <- floor(0.7 * nrow(H1B_Trial))


set.seed(123)
H1B_Trial_Ind <- sample(seq_len(nrow(H1B_Trial)), size = smp_size)

train <- H1B_Trial[H1B_Trial_Ind, ]
test <- H1B_Trial[-H1B_Trial_Ind, ]



levels(test$EMPLOYMENT_END_DATE)<- levels(train$EMPLOYMENT_END_DATE)
levels(test$EMPLOYER_STATE)<- levels(train$EMPLOYER_STATE)
levels(test$WORKSITE_STATE)<- levels(train$WORKSITE_STATE)

train=train[complete.cases(train),]
test=test[complete.cases(test),]

table(train$EMPLOYER_STATE)
table(train$WORKSITE_STATE)
length(unique(train$EMPLOYER_STATE))
length(unique(train$WORKSITE_STATE))


train.rf<-randomForest(CASE_STATUS~., data = train)

test.rf= predict(train.rf,test)
confusionMatrix(test.rf,test$CASE_STATUS)

##END OF TRAIN/TEST DATA

