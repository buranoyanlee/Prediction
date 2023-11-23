library(caret)
library(pROC)
train_df<-read.csv('Data.csv',header=T,row.names = 1)
train_df$Group<-as.factor(train_df$Group)
logit_model <- glm(Group~.,data=train_df, family = binomial) 
logit_step <- step(logit_model, direction = 'backward') 
summary(logit_step)
varImp(logit_step)

nrow<-nrow(train_df)
rowname<-row.names(train_df)
set.seed(1234)
train<-sample(rowname,floor(nrow*0.6))
train_data<-train_df[train,]
test<-setdiff(rowname,train)
test_data<-train_df[test,]

fitControl<-trainControl(method="repeatedcv",
                         number = 10,
                         repeats = 10,
                         classProbs = TRUE)
#Classification error matrix
glm.model.cv <- train(Group~ Symbol1 + Symbol2 + Symbol3 + Symbol4,
                      data= train_data,
                      method = "glm",
                      metric = "ROC",
                      trControl = fitControl)
glm.probs <- predict(glm.model.cv,train_data)
confusionMatrix(glm.probs,train_data$Group)

glm.probs <- predict(glm.model.cv,test_data)
confusionMatrix(glm.probs,test_data$Group)

#ROC
n <- colnames(train_df)
f <- as.formula(paste('Group ~', paste(n[!n %in% 'Group'], collapse = ' + '))) 
logit_model <- glm(Group ~ Symbol1 + Symbol2 + Symbol3 + Symbol4 , data=train_df,family = binomial) 
pred_train <- data.frame(Prob = round(predict(logit_model, newdata = train_data/test_data,type="response"),4), GoldStandard = train_data/test_data$Group, stringsAsFactors = F)
roc.train <- plot.roc(pred_train[,2], pred_train[,1], ylim=c(0,1),xlim=c(1,0), smooth=F,    ci=TRUE,  legacy.axes=T,print.auc=T)