################### ============== ????????? =================== #################

library(plyr)          # Rmisc????????????,?????????????????????dplyr???,???????????????plyr???
library(dplyr)         # filter()
library(ggplot2)       # ggplot()             
library(DT)            # datatable()           ????????????????????????
library(caret)         # createDataPartition() ??????????????????
library(rpart)         # rpart()
library(e1071)         # naiveBayes()in
library(pROC)          # roc()
library(Rmisc)         # multiplot()           ??????????????????
library(corrplot)

################### ============= Import data ================== #################

hr <- read.csv("~/Downloads/HR_comma_sep.csv")

################### ============= Understand the data ================== #################

#Get the variables and data type of the variables, as well as their records
#str(hr)
#head(hr)
#Get the general information of the records
#summary(hr)

# Add the salary influence to the left
# hr$salary <-as.character(hr$salary)
# HR_test <- hr %>% select(satisfaction_level:promotion_last_5years,salary)
# HR_test$salary <- as.numeric(ifelse(HR_test$salary=="low",0,
#                                     ifelse(HR_test$salary=="medium",1,2))
#                             ) 
# M <- cor(HR_test)
# corrplot(M,method="circle")
HR_correlation <- hr %>% select(satisfaction_level:promotion_last_5years)
M <- cor(HR_correlation)
corrplot(M,method='circle')
################## ============= Pre-process data ================== #################

# Treat the "left" variable as categorial variable, which is ordinal
hr$left <- factor(hr$left,levels = c('0','1'))

################### ============= Visualization data ================ #################




box_sat <- ggplot(hr, aes(x = left, y = satisfaction_clearlevel, fill = left)) +
  geom_boxplot() + 
  theme_bw() +  # 一种ggplot的主题
  labs(x = 'left', y = 'satisfaction_level') # 设置横纵坐标标签

# box_sat
# plot eva and left

box_eva <- ggplot(hr,aes(x=left, y = hr$last_evaluation,fill=left))+
  geom_boxplot()+
            theme_bw()+
            labs(x="left",y="last_evaluation")
box_mon <- ggplot(hr,aes(x=left, y = average_montly_hours,fill = left))+
          theme_bw()+
          geom_boxplot()+
          labs(x='left',y= 'average_montly_hours')

box_mon
  
# Plot the working years vs left

box_time <- ggplot(hr,aes(x=left, y=time_spend_company,fill=left))+
            geom_boxplot()+
            theme_bw()+
            labs(x= "left",y= "time_spend_company")
box_time
# Merge these figures, cols = 2
#multiplot(box_sat,box_eva,box_mon,box_time,cols=2)

## The number of attended projects and the promotion in the latest 5 years, the salary 

hr$number_project <- factor(hr$number_project,
                            levels = c('2', '3', '4', '5', '6', '7'))
hr$number_project <- factor(hr$number_project,
                            levels = c('2', '3', '4', '5', '6', '7'))

# 绘制参与项目个数与是否离职的百分比堆积条形图
bar_pro <- ggplot(hr, aes(x = number_project, fill = left)) +
  geom_bar(position = 'fill') + # position = 'fill'即绘制百分比堆积条形图
  theme_bw() + 
  labs(x = 'left', y = 'number_project')

bar_pro


# plot the relationship between the promotion and left

bar_5years <- ggplot(hr, aes(x = as.factor(promotion_last_5years), fill = left)) +
  geom_bar(position = 'fill') + 
  theme_bw() + 
  labs(x = 'left', y = 'promotion_last_5years')

bar_5years


bar_salary <- ggplot(hr, aes(x = salary, fill = left)) +
  geom_bar(position = 'fill') + 
  theme_bw() + 
  labs(x = 'left', y = 'salary')

bar_salary


multiplot(bar_pro, bar_5years, bar_salary, cols = 3)


############## =============== 提取优秀员工 =========== ###################

# filter()用来筛选符合条件的样本
hr_model <- filter(hr, last_evaluation >= 0.70 | time_spend_company >= 4
                   | hr$number_project > 5)
train_control <- trainControl(method = 'cv', number = 5)

set.seed(1234) 
# Genera process: 1) data:train data, test data 2) train model 3) predict with test data 4)evaluate the modle with the test data 
index <- createDataPartition(hr_model$left,p=0.7, list = F)
traindata <- hr_model[index,]
testdata <- hr_model[-index,]


# ## 
# train(x,y,method='',...)
# train(formular,data,...)
# train(x,data,method='',...)
# rpart method from library(rpart)
rpartmodel <- train(left ~ ., # formular y ~ x1 + x2 + ...
                   data = traindata, # data fram from which variables specified in formular/recipe are preferentially to be taken
                   trControl=train_control,#
                   method = "rpart")

# use the the trianed model to predict the data based on the test data
pred_rpart <- predict(rpartmodel,testdata[-7])

# evaluate the model 
# positive = '1' 
con_rpart <- table(pred_rpart,testdata$left)

con_rpart
############ Naives Bayes #####################

nbmodel <- train(left ~ ., #parameters ~ variables
                 data = traindata,# data
                 trControl = train_control, #method
                 method = 'nb')


pred_nb <- predict(nbmodel,testdata[-7])

con_nb <- table(pred_nb,testdata$left)

con_nb  

#####################################################

pred_rpart <- as.numeric(as.character(pred_rpart))
pred_nb <- as.numeric(as.character(pred_nb))

#library(pROC)          # roc() receiver operating chracteristic 
roc_rpart <- roc(testdata$left,pred_rpart)

# false positive rate
Specificity <- roc_rpart$specificities # speinscific for a particular condition(true negative rate)

Sensitivity <- roc_rpart$sensitivities # true positive rate


# plot ROC 
p_rpart <- ggplot(data=NULL, aes(x=1-Specificity, y=Sensitivity))+
          geom_line(color = 'red')+
          geom_abline()+
          annotate('text',x=0.4,y=0.5,label=paste('AUC',
                                                  round(roc_rpart$auc,3)))+
          theme_bw()+
          labs(x="1-Specificity", y = "Sensitivities")


p_rpart

######################### =============Application =============####################

# 使用回归树模型预测分类的概率，type=‘prob’设置预测结果为离职的概率和不离职的概率
pred_end <- predict(rpartmodel, testdata[-7], type = 'prob')

# 合并预测结果和预测概率结果
data_end <- cbind(round(pred_end, 3), pred_rpart)

# 为预测结果表重命名
names(data_end) <- c('pred.0', 'pred.1', 'pred') 

# 生成一个交互式数据表
datatable(data_end)

######################### =============Analysis most valuable employees =============####################
#Select the people who left
#hr_leaving_people <- hr %>% filter(left==1)
hr_hist <- hr %>% filter(left==1)
par(mfrow=c(1,3))
hist(hr_hist$satisfaction_level,col="#3090C7", main = "Satisfaction level") 
hist(hr_hist$last_evaluation,col="#3090C7", main = "Last evaluation")
hist(hr_hist$average_montly_hours,col="#3090C7", main = "Average montly hours")
par(mfrow=c(1,2))
hist(hr_hist$Work_accident,col="#3090C7", main = "Work accident")
plot(hr_hist$salary,col="#3090C7", main = "Salary")
hr_leaving_people <- hr %>% filter(left==1)
nrow(hr_leaving_people)
hr_good_leaving_people <- hr_leaving_people %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
nrow(hr_good_leaving_people)

hr_good_leaving_people2 <- hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
hr_good_people_select <- hr_good_leaving_people2 %>% select(satisfaction_level, number_project: promotion_last_5years)
M <- cor(hr_good_people_select)
par(mfrow=c(1,1))
corrplot(M, method="circle")
summary(hr_good_leaving_people2)

#Model: original data with conditions
hr_model <- hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
summary(hr_model)
#Train 
#library("caret")
hr_model$left <- as.factor(hr_model$left)
train_control<- trainControl(method="cv", number=5)
head(train_control)
rpartmodel<- train(left~., data=hr_model, trControl=train_control, method="rpart")
#Predict
predictions<- predict(rpartmodel,hr_model) #? why don't we git rid of the "left" variable here?
predictions2<- predict(rpartmodel,hr_model[-7])
hr_model_tree<- cbind(hr_model,predictions)
# summarize results
confusionMatrix<- confusionMatrix(hr_model_tree$predictions,hr_model_tree$left)
confusionMatrix
#con_table <-table(as.numeric(hr_model_tree$predictions),as.numeric(hr_model_tree$left))
#con_table
#################################################
inTraining <- createDataPartition(hr_model$left, p = .75, list = FALSE)
training <- hr_model[ inTraining,]
testing  <- hr_model[-inTraining,]
#head(inTraining)

logreg = glm(left ~ ., family=binomial(logit), data=training)

probaToLeave=predict(logreg,newdata=testing,type="response")
predattrition = data.frame(probaToLeave)
                     
predattrition$performance=testing$last_evaluation
plot(predattrition$probaToLeave,predattrition$performance)
predattrition$priority=predattrition$performance*predattrition$probaToLeave
orderpredattrition=predattrition[order(predattrition$priority,decreasing = TRUE),]
orderpredattrition <- head(orderpredattrition, n=300)
datatable(orderpredattrition)
