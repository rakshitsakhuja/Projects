library(tidyverse)
library(DataExplorer)
library(ggplot2)
#Refrence : https://ianmadd.github.io/pages/multiplot.html
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#Read CSV File
creditcard=read.csv("/home/qubaic/AnalyticsVidhya/Kaggle/CreditCardFraudDetection/creditcard.csv",header = TRUE,stringsAsFactors = F)

plot_str(creditcard)
str(creditcard)
creditcard$Class=as.factor(creditcard$Class)
#All data seems good no missing values found
plot_missing(creditcard)
#Summary Stats
summary(creditcard)

#Bar Plot of Classes
creditcard%>%
  group_by(Class)%>%
  ggplot(aes(Class,fill=Class))+
  geom_bar()


summary(subset(creditcard,Class==1))
summary(subset(creditcard,Class==1)$Amount)
# Maximum Amount retreived from Fraudlent data is 2125 
#and mean of the transactions range around 122
FrdAmt=as.data.frame(table(subset(creditcard,Class==1)$Amount))
str(FrdAmt)
FrdAmt$Var1=as.numeric(as.character(FrdAmt$Var1))
#FrdAmt%>%count(Var1<10)
FrdAmt %>% filter((Var1)<=10) %>% summarise(sum(Freq) )
FrdAmt %>% filter((Var1)>10) %>% summarise(sum(Freq) )
FrdAmt=FrdAmt%>%mutate(Refrence=if_else(Var1<=10, "Lesserthan10", "Greaterthan10"))
#Almost 50%(249) of Transacations are lesser than Amount=10

#Bar Plot of Frauds with there Amount 
FrdAmt%>% group_by(Refrence) %>%
  summarise(sum(Freq))%>%
  ggplot(aes(Refrence,`sum(Freq)`,fill=as.factor(`sum(Freq)`))) + 
  geom_bar(stat="identity",width = .50)
#Fraudlent Amount is of smaller values
creditcard %>% ggplot(aes(Time,Amount,fill=Class,color=Class))+geom_point()+facet_grid(Class~.)

#Since its 2 day data and same is represented by the density plot
#for time
ggplot(creditcard,aes(Time))+geom_density()
#Amount is Right Skewed
ggplot(creditcard,aes(Amount))+geom_density()

#Most of the Fraudlent Transactions below 10 lies between 0 and 2.5
creditcard %>%filter(Amount<10) %>%ggplot(aes(Class,Amount))+geom_violin()
#Visual Representation of Amount an Class
creditcard %>%filter(Amount<2000) %>%ggplot(aes(Class,Amount))+geom_violin()



p1=ggplot(creditcard,aes(V1,V2,fill=Class,color=Class))+geom_point(size=0.5)
p2=ggplot(creditcard,aes(V2,V3,fill=Class,color=Class))+geom_point(size=0.5)
p3=ggplot(creditcard,aes(V3,V4,fill=Class,color=Class))+geom_point(size=0.5)
p4=ggplot(creditcard,aes(V4,V5,fill=Class,color=Class))+geom_point(size=0.5)
p5=ggplot(creditcard,aes(V5,V6,fill=Class,color=Class))+geom_point(size=0.5)
p6=ggplot(creditcard,aes(V6,V7,fill=Class,color=Class))+geom_point(size=0.5)
p7=ggplot(creditcard,aes(V7,V8,fill=Class,color=Class))+geom_point(size=0.5)
p8=ggplot(creditcard,aes(V8,V9,fill=Class,color=Class))+geom_point(size=0.5)
p9=ggplot(creditcard,aes(V9,V10,fill=Class,color=Class))+geom_point(size=0.5)
p10=ggplot(creditcard,aes(V10,V11,fill=Class,color=Class))+geom_point(size=0.5)
p11=ggplot(creditcard,aes(V11,V12,fill=Class,color=Class))+geom_point(size=0.5)
p12=ggplot(creditcard,aes(V12,V13,fill=Class,color=Class))+geom_point(size=0.5)
p13=ggplot(creditcard,aes(V13,V14,fill=Class,color=Class))+geom_point(size=0.5)
p14=ggplot(creditcard,aes(V14,V15,fill=Class,color=Class))+geom_point(size=0.5)
p15=ggplot(creditcard,aes(V15,V16,fill=Class,color=Class))+geom_point(size=0.5)
p16=ggplot(creditcard,aes(V16,V17,fill=Class,color=Class))+geom_point(size=0.5)
p17=ggplot(creditcard,aes(V17,V18,fill=Class,color=Class))+geom_point(size=0.5)
p18=ggplot(creditcard,aes(V18,V19,fill=Class,color=Class))+geom_point(size=0.5)
p19=ggplot(creditcard,aes(V19,V20,fill=Class,color=Class))+geom_point(size=0.5)
p20=ggplot(creditcard,aes(V20,V21,fill=Class,color=Class))+geom_point(size=0.5)
p21=ggplot(creditcard,aes(V21,V22,fill=Class,color=Class))+geom_point(size=0.5)
p22=ggplot(creditcard,aes(V22,V23,fill=Class,color=Class))+geom_point(size=0.5)
p23=ggplot(creditcard,aes(V23,V24,fill=Class,color=Class))+geom_point(size=0.5)
p24=ggplot(creditcard,aes(V24,V25,fill=Class,color=Class))+geom_point(size=0.5)
p25=ggplot(creditcard,aes(V25,V26,fill=Class,color=Class))+geom_point(size=0.5)
p26=ggplot(creditcard,aes(V26,V27,fill=Class,color=Class))+geom_point(size=0.5)
p27=ggplot(creditcard,aes(V27,V28,fill=Class,color=Class))+geom_point(size=0.5)
#Since the data is highly dimensional and there can not be much inferences from the plots
#Donot plot it takes  lot of time
#multiplot(p1, p2, p3, p4,p5,p6,p7,p8, cols=2)

#library(psych)
#pairs.panels(creditcard[1:4])
#Correlation of all the points
cor(creditcard[,unlist(lapply(creditcard, is.numeric))])
plot_correlation(creditcard)
#BarPlot for all Variables
# ggplot(gather(creditcard), aes(value)) + 
#   geom_histogram(bins = 10) + 
#   facet_wrap(~key, scales = 'free_x')
# 
# plot_bar(creditcard)

nrow(creditcard)*2
table(creditcard$Class)
require(caTools)
set.seed(101)            #This is used to create same samples everytime
criteria=sample.split(creditcard$Class,SplitRatio=.7)
train=subset(creditcard,criteria==TRUE)
test=subset(creditcard,criteria==FALSE)
y_train <- train$Class
y_test <- test$Class
X_train <- train %>% select(-one_of(c("Time","Class")))
X_test <- test %>% select(-one_of(c("Time","Class")))
names(train)
baseline_accuracy=(table(creditcard$Class))[1]/((table(creditcard$Class))[1]+(table(creditcard$Class))[2])
glm_model <- glm(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = train, family = "binomial")
glm_predict <- predict(glm_model, test, type = "response")
d=table(test$Class, glm_predict > 0.5)
glm_accuracy=sum(diag(d))/sum(d)
glm_accuracy



library(ROCR)
pred <-prediction(glm_predict ,test$Class)

# Maximum Accuracy and prob. cutoff against it
acc.perf <- performance(pred, "acc")
acc.perf
ind = which.max( slot(acc.perf, "y.values")[[1]])
ind
acc = slot(acc.perf, "y.values")[[1]][ind]
acc
cutoff = slot(acc.perf, "x.values")[[1]][ind]
cutoff

print(c(accuracy= acc, cutoff = cutoff))


perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]


perf_val2 <- performance(pred, "tpr", "fpr")
plot(perf_val2, col = "green", lwd = 1.5)
abline(a = 0, b = 1)


#Decision Tree

library(rpart)
library(rpart.plot)

tree1=rpart(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = train,method="class")
prp(tree1) 
tree_predict <- predict(tree1, test,type="class")
d=table(test$Class, tree_predict)
#d=table(test$Class, glm_predict > 0.5)
accuracy=sum(diag(d))/sum(d)
accuracy
as.numeric(as.character(tree_predict))

pred <-prediction(as.numeric(as.character(tree_predict)) ,test$Class)

# Maximum Accuracy and prob. cutoff against it
acc.perf <- performance(pred, "acc")
acc.perf
ind = which.max( slot(acc.perf, "y.values")[[1]])
ind
acc = slot(acc.perf, "y.values")[[1]][ind]
acc
cutoff = slot(acc.perf, "x.values")[[1]][ind]
cutoff

print(c(accuracy= acc, cutoff = cutoff))


perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]


perf_val2 <- performance(pred, "tpr", "fpr")
plot(perf_val2, col = "green", lwd = 1.5)
abline(a = 0, b = 1)




#Methods to Handle Imbalance in dataSet
library(ROSE)

#over sampling
data_balanced_over <- ovun.sample(Class ~ ., data = train, method = "over",N = 568630)$data
table(data_balanced_over$Class)

#Under
data_balanced_under <- ovun.sample(Class ~ ., data = train, method = "under", N = 984, seed = 10)$data
table(data_balanced_under$Class)


#Both
data_balanced_both <- ovun.sample(Class ~ ., data = train, method = "both", p=0.5,N=30000, seed = 10)$data
table(data_balanced_both$Class)

#Rose
data.rose <- ROSE(Class ~ ., data = train, seed = 10)$data
table(data.rose$Class)

#build logistic models
rose <- glm(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = data.rose, family = "binomial",control = list(maxit = 50))
over <- glm(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = data_balanced_over, family = "binomial",control = list(maxit = 50))
under <- glm(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = data_balanced_under, family = "binomial",control = list(maxit = 50))
both <- glm(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = data_balanced_both, family = "binomial",control = list(maxit = 50))

#make predictions on unseen data
pred.rose <- predict(rose, newdata = test)
pred.over <- predict(over, newdata = test)
pred.under <- predict(under, newdata = test)
pred.both <- predict(both, newdata = test)


pred <-prediction(pred.rose ,test$Class)
perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]


pred <-prediction(pred.over ,test$Class)
perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]

pred <-prediction(pred.under ,test$Class)
perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]

pred <-prediction(pred.both ,test$Class)
perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]




#build tree models
rose <- rpart(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = data.rose, method="class")
over <- rpart(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = data_balanced_over, method="class")
under <- rpart(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = data_balanced_under, method="class")
both <- rpart(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+Amount, data = data_balanced_both, method="class")

#make predictions on unseen data
pred.rose <- predict(rose, newdata = test,type="class")
pred.over <- predict(over, newdata = test,type="class")
pred.under <- predict(under, newdata = test,type="class")
pred.both <- predict(both, newdata = test,type="class")


pred <-prediction(as.numeric(as.character(pred.rose)),test$Class)
perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]


pred <-prediction(as.numeric(as.character(pred.over)) ,test$Class)
perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]

pred <-prediction(as.numeric(as.character(pred.under)) ,test$Class)
perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]

pred <-prediction(as.numeric(as.character(pred.both)) ,test$Class)
perf_val <- performance(pred,"auc")
perf_val@y.values[[1]]


