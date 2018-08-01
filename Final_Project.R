H1B = read.csv("C:/Users/yamaw/OneDrive/Desktop/Stats 101C Final Project/TrainH1BLast.csv")

library(dplyr)
library(data.table)
library(caret)
library(randomForest)

#Data Cleaning 
H1B<-H1B%>%
  filter(VISA_CLASS %in% "H-1B")

H1B<-H1B%>%
  mutate(PREVAILING_WAGE = case_when(PW_UNIT_OF_PAY == 'Hour' ~ PREVAILING_WAGE*2000,PW_UNIT_OF_PAY == 'Year' ~
                                       PREVAILING_WAGE,PW_UNIT_OF_PAY=='Week'~PREVAILING_WAGE*50,PW_UNIT_OF_PAY=='Bi-Weekly'~PREVAILING_WAGE*25))

H1B<-H1B%>%
    filter(PREVAILING_WAGE > 5000 & (PREVAILING_WAGE < 200000))

H1B$CASE_SUBMITTED=as.Date(H1B$CASE_SUBMITTED,"%m/%d/%Y")
H1B$DECISION_DATE=as.Date(H1B$DECISION_DATE,"%m/%d/%Y")
H1B$EMPLOYMENT_START_DATE=as.Date(H1B$EMPLOYMENT_START_DATE,"%m/%d/%Y")
H1B$EMPLOYMENT_END_DATE=as.Date(H1B$EMPLOYMENT_END_DATE,"%m/%d/%Y")

H1B$DECISION_TIME=abs(H1B$DECISION_DATE-H1B$CASE_SUBMITTED)
H1B$EMPLOYMENT_TIME=abs(H1B$EMPLOYMENT_END_DATE-H1B$EMPLOYMENT_START_DATE)

H1B <- H1B %>%
    select(c ("CASE_STATUS",
             "DECISION_TIME",
             "EMPLOYMENT_TIME",
             "AGENT_REPRESENTING_EMPLOYER",
             "PREVAILING_WAGE",
             "PW_SOURCE",
             "PW_WAGE_LEVEL",
             "H1B_DEPENDENT",
             "WORKSITE_STATE",
             "FULL_TIME_POSITION"))

H1B<-H1B[!(H1B$WORKSITE_STATE == "" | 
             H1B$WORKSITE_STATE == "GU" | 
             H1B$WORKSITE_STATE == "MP" | 
             H1B$WORKSITE_STATE == "PR" | 
             H1B$WORKSITE_STATE == "VI"  ),]

                         

H1B=H1B[complete.cases(H1B),]


H1B$DECISION_TIME=as.integer(H1B$DECISION_TIME)
H1B$EMPLOYMENT_TIME=as.integer(H1B$EMPLOYMENT_TIME)


H1B_Trial = H1B

H1B_Trial <- H1B_Trial %>%
  na.omit(H1B_Trial) 


H1B_Trial$PREVAILING_WAGE<-as.numeric(H1B_Trial$PREVAILING_WAGE)

H1B_Trial$WORKSITE_STATE <- as.factor(H1B_Trial$WORKSITE_STATE)
H1B_Trial$WORKSITE_STATE<-droplevels(H1B_Trial$WORKSITE_STATE)

H1B_Trial$CASE_STATUS <- as.factor(H1B_Trial$CASE_STATUS)
H1B_Trial$CASE_STATUS<-droplevels(H1B_Trial$CASE_STATUS)

H1B_Trial$AGENT_REPRESENTING_EMPLOYER <- as.factor(H1B_Trial$AGENT_REPRESENTING_EMPLOYER)
H1B_Trial$AGENT_REPRESENTING_EMPLOYER<-droplevels(H1B_Trial$AGENT_REPRESENTING_EMPLOYER)

H1B_Trial$PW_SOURCE <- as.factor(H1B_Trial$PW_SOURCE)
H1B_Trial$PW_SOURCE<-droplevels(H1B_Trial$PW_SOURCE)

H1B_Trial$PW_WAGE_LEVEL <- as.factor(H1B_Trial$PW_WAGE_LEVEL)
H1B_Trial$PW_WAGE_LEVEL<-droplevels(H1B_Trial$PW_WAGE_LEVEL)

H1B_Trial$H1B_DEPENDENT <- as.factor(H1B_Trial$H1B_DEPENDENT)
H1B_Trial$H1B_DEPENDENT<-droplevels(H1B_Trial$H1B_DEPENDENT)

H1B_Trial$FULL_TIME_POSITION <- as.factor(H1B_Trial$FULL_TIME_POSITION)
H1B_Trial$FULL_TIME_POSITION<-droplevels(H1B_Trial$FULL_TIME_POSITION)

smp_size <- floor(0.7 * nrow(H1B_Trial))


set.seed(222)
H1B_Trial_Ind <- sample(seq_len(nrow(H1B_Trial)), size = smp_size)

train <- H1B_Trial[H1B_Trial_Ind, ]
test <- H1B_Trial[-H1B_Trial_Ind, ]



levels(test$AGENT_REPRESENTING_EMPLOYER)<- levels(train$AGENT_REPRESENTING_EMPLOYER)
levels(test$PW_SOURCE)<- levels(train$PW_SOURCE)
levels(test$PW_WAGE_LEVEL)<- levels(train$PW_WAGE_LEVEL)
levels(test$H1B_DEPENDENT)<- levels(train$H1B_DEPENDENT)
levels(test$WORKSITE_STATE)<- levels(train$WORKSITE_STATE)
levels(test$FULL_TIME_POSITION)<- levels(train$FULL_TIME_POSITION)

train=train[complete.cases(train),]
test=test[complete.cases(test),]

#RandomForest (86%)
train.rf<-randomForest(CASE_STATUS~., data = train)
test.rf= predict(train.rf,test)
confusionMatrix(test.rf,test$CASE_STATUS)
#END OF RF


#GLM MODEL (Very inaccurate)
glm.train <- glm(CASE_STATUS~.,data=train, family=binomial)
glm.test <-predict(glm.train, newdata= test, type="response")
glm.pred=rep ("Denied " ,1412)
glm.pred[glm.test >.4]=" Certified"
summary(glm.pred)
table(glm.pred,test$CASE_STATUS)
#END OF GLM

#LDA(Needs Work)
lda.train=lda(CASE_STATUS~., data=train)

library(tree
        )
#TREE (89%)
tree.train =tree(CASE_STATUS~. -WORKSITE_STATE,train )
tree.test <- predict(tree.train, test, type="class")
table(tree.test,test$CASE_STATUS)

##END OF TRAIN/TEST DATA


#####

final <-read.csv("C:/Users/yamaw/OneDrive/Desktop/Stats 101C Final Project/TestH1BLast No Y values.csv")

#final<-final%>%
  #filter(VISA_CLASS %in% "H-1B")

final<-final%>%
  mutate(PREVAILING_WAGE = case_when(PW_UNIT_OF_PAY == 'Hour' ~ PREVAILING_WAGE*2000,PW_UNIT_OF_PAY == 'Year' ~
                                       PREVAILING_WAGE,PW_UNIT_OF_PAY=='Week'~PREVAILING_WAGE*50,PW_UNIT_OF_PAY=='Bi-Weekly'~PREVAILING_WAGE*25))

#final<-final%>%
  #filter(PREVAILING_WAGE > 5000 & (PREVAILING_WAGE < 200000))

final$CASE_SUBMITTED=as.Date(final$CASE_SUBMITTED,"%m/%d/%Y")
final$DECISION_DATE=as.Date(final$DECISION_DATE,"%m/%d/%Y")
final$EMPLOYMENT_START_DATE=as.Date(final$EMPLOYMENT_START_DATE,"%m/%d/%Y")
final$EMPLOYMENT_END_DATE=as.Date(final$EMPLOYMENT_END_DATE,"%m/%d/%Y")

final$DECISION_TIME=abs(final$DECISION_DATE-final$CASE_SUBMITTED)
final$EMPLOYMENT_TIME=abs(final$EMPLOYMENT_END_DATE-final$EMPLOYMENT_START_DATE)

final <- final %>%
  select(c (
            "DECISION_TIME",
            "EMPLOYMENT_TIME",
            "AGENT_REPRESENTING_EMPLOYER",
            "PREVAILING_WAGE",
            "PW_SOURCE",
            "PW_WAGE_LEVEL",
            "H1B_DEPENDENT",
            "WORKSITE_STATE",
            "FULL_TIME_POSITION"))

#final<-final[!(final$WORKSITE_STATE == "" | 
               #  final$WORKSITE_STATE == "GU" | 
               #  final$WORKSITE_STATE == "MP" | 
               #  final$WORKSITE_STATE == "PR" | 
               #  final$WORKSITE_STATE == "VI"  ),]



#final=final[complete.cases(final),]


final$DECISION_TIME=as.integer(final$DECISION_TIME)
final$EMPLOYMENT_TIME=as.integer(final$EMPLOYMENT_TIME)


final <- final %>%
  na.omit(final) 


final$PREVAILING_WAGE<-as.numeric(final$PREVAILING_WAGE)

final$WORKSITE_STATE <- as.factor(final$WORKSITE_STATE)
final$WORKSITE_STATE<-droplevels(final$WORKSITE_STATE)

final$CASE_STATUS <- as.factor(final$CASE_STATUS)
final$CASE_STATUS<-droplevels(final$CASE_STATUS)

final$AGENT_REPRESENTING_EMPLOYER <- as.factor(final$AGENT_REPRESENTING_EMPLOYER)
final$AGENT_REPRESENTING_EMPLOYER<-droplevels(final$AGENT_REPRESENTING_EMPLOYER)

final$PW_SOURCE <- as.factor(final$PW_SOURCE)
final$PW_SOURCE<-droplevels(final$PW_SOURCE)

final$PW_WAGE_LEVEL <- as.factor(final$PW_WAGE_LEVEL)
final$PW_WAGE_LEVEL<-droplevels(final$PW_WAGE_LEVEL)

final$H1B_DEPENDENT <- as.factor(final$H1B_DEPENDENT)
final$H1B_DEPENDENT<-droplevels(final$H1B_DEPENDENT)

final$FULL_TIME_POSITION <- as.factor(final$FULL_TIME_POSITION)
final$FULL_TIME_POSITION<-droplevels(final$FULL_TIME_POSITION)

tree.final <- predict(tree.train, final, type="class")
summary(tree.final)

write.csv(tree.final, "tree.final.csv")

rf.final= predict(train.rf,final)
