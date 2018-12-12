library(e1071)
data = read.csv('cleaned_data.csv',stringsAsFactors = F)
names(data)
set.seed(1)
sample_row = sample(1:dim(data)[1],0.8*dim(data[1]))
train.sub = data[sample_row,]
test.sub = data[-sample_row,]
score = read.csv('cleaned_score_data.csv',stringsAsFactors=F)
myBoost = gbm(formula=price~., data=train.sub, shrinkage=0.001, distribution="gaussian",interaction.depth=5, n.trees=6000)
pred = predict(myBoost,test.sub,n.trees=6000)
sqrt(mean(sum((pred-test.sub$price)^2)))

#svm.fit = svm(price~., data=train.sub, kerne="radial",cost=1e-3,gamma=1,scale=FALSE)
#pred = predict(svm.fit,test.sub)
sqrt(mean(sum((pred-test.sub$price)^2)))
summary(svm.fit)
plot(svm.fit,data)


svm.tune = tune(svm,price~., data=data,kernel="radial",ranges=list(cost=c(0.001,0.01,0.1,1,10,100),gamma=c(0.5,1,2,3,4)))

pred.score = predict(svm.tune,score)