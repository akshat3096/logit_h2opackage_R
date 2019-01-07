## Simple GLM model 
library(caret)
setwd(("C:/Users/arora/Downloads/Course_Script_1/section19"))
data <- read.csv("voice.csv")
data$label <- factor(data$label)
names(data)
str(data)
set.seed(99)
Train=createDataPartition(data$label,p=0.75, list=F)
training <- data[Train,]
testing <- data[-Train,]
fit <- glm(label~Q25+Q75+sp.ent+sfm+mode+meanfun+minfun,data=training,family = binomial(link = "logit"))
summary(fit)

#confusion matrix 
p <- predict(fit,newdata = testing,type = "response")
head(p)
pred1 <- ifelse(p>0.5,1,0)
tab <- table(pred1,testing$label)
tab
sum(diag(tab))/sum(tab) # accuracy 

#ROC and Area under the curve 
library(ROCR)
pr <- prediction(p, testing$label)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values
auc

## USing the h20 package ## 
###########################
library(h2o)
h2o.init(max_mem_size = "2G",
         nthreads = 2,
         ip="localhost",
         port=54321)  
setwd(("C:/Users/arora/Downloads/Course_Script_1/section19"))
data <- read.csv("voice.csv")
names(data)
data1 <- data[,c(4,5,9,10,11,13,14,21)]
data1$label <-factor(data1$label)
table(data1$label)

d.hex <- as.h2o(data1,destination_frame = "d.hex")
head(d.hex)

## data partition
set.seed(99)
split <- h2o.splitFrame(data = d.hex,ratios = 0.75)
train <- split[[1]]
test <- split[[2]]
fit3 <- h2o.glm(x=1:7,y=8,training_frame = train,family = "binomial",link="logit")
h2o.performance(fit3,test)
(390+373)/(390+373+13+8)
y <- h2o.predict(fit3,test)
head(y)


# gives a better accuracy than the cross validation set (of 98%), 
#hence a better model is found out by the h2o package
