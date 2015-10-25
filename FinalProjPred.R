library(caret)
library(corrplot)
library(ggplot2)

trainData <- read.csv('pml-training.csv')

keepVars <- c('classe','accel_arm_x','accel_arm_y','accel_arm_z','accel_belt_x','accel_belt_y','accel_belt_z','accel_dumbbell_x','accel_dumbbell_y','accel_dumbbell_z','accel_forearm_x','accel_forearm_y','accel_forearm_z')
gyroVars <- c('gyros_arm_x','gyros_arm_y','gyros_arm_z','gyros_belt_x','gyros_belt_y','gyros_belt_z','gyros_dumbbell_x','gyros_dumbbell_y','gyros_dumbbell_z','gyros_forearm_x','gyros_forearm_y','gyros_forearm_z')
magnetVars <- c('magnet_arm_x','magnet_arm_y','magnet_arm_z','magnet_belt_x','magnet_belt_y','magnet_belt_z','magnet_dumbbell_x','magnet_dumbbell_y','magnet_dumbbell_z','magnet_forearm_x','magnet_forearm_y','magnet_forearm_z')

subTrain <- trainData[,rbind(keepVars, gyroVars,magnetVars)]

set.seed(34234)
inTrain <- createDataPartition(y=subTrain$classe,
                               p=0.7, list=FALSE)
training <- subTrain[inTrain,]
testing <- subTrain[-inTrain,]

rfModel<-train(classe~.,data=training,method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)

pTree = predict(TreeModel,testing)
pRF = predict(rfModel, testing)
confusionMatrix(pRF, testing$classe)

confusionMatrix(plda, testing$classe)
confusionMatrix(pTree, testing$classe)
confusionMatrix(pTree,plda)

fancyRpartPlot(TreeModel$finalModel,main="Tree from Random Forest", sub="")

corrTrain <- subset(training, select=-c(classe))
corMat <- cor(corrTrain)

