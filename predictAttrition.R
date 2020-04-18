
library(tidyverse)

df <- read.csv("CaseStudy2-data.csv")
test <- read.csv("CaseStudy2CompSet No Attrition.csv")

# dataset with "no"
data_no = df[which(df$Attrition=="No"),]
# dataset with "yes"
data_yes = df[which(df$Attrition=="Yes"),]

#making more folds on No to balance the number with Yes 
folds_no = createFolds(data_no$Attrition, k=8)
folds_yes = createFolds(data_yes$Attrition, k=2)
length(folds_no$Fold1)
length(folds_no$Fold2)
length(folds_yes$Fold1)
length(folds_yes$Fold2)

#Train
train_no = data_no[c(folds_no$Fold1,folds_no$Fold2),]
train_yes = data_yes[c(folds_yes$Fold1,folds_yes$Fold2),]

train = rbind(train_no, train_yes)

  
#RF
library(randomForest)

model.rf <- randomForest(Attrition ~ ., data = train, importance = TRUE)
preds.rf <- predict(model.rf, test)

results <- data.frame(test$ID, preds.rf)
head(results)

write.csv(results, "Case2PredictionsPai Attrition.csv")
