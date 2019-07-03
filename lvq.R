Sys.setenv(LANG = "en")
library(class)
library(caret)
data=read.csv("car/car.data",header=FALSE)
#data=read.csv("bank/bank-additional-full.csv",sep=";")
#data=read.csv("iris/iris.data",header=FALSE)
ordinal_data=data
to_predic=""
for (i in colnames(data)) {
	if(! all(is.numeric(data[,i]))){
		ordinal_data[,i]=factor(data[,i],levels=c(matrix(unique(data[,i]))),labels=1:length(unique(data[,i])))
	}
	p=i
}
index = createDataPartition(data[,i], p = .8, list = F)
train_data=ordinal_data[index,]
test_data=ordinal_data[-index,]
train=data.matrix(train_data[,colnames(train_data)[0:(length(colnames(data))-1)]])
test=data.matrix(test_data[,colnames(test_data)[0:(length(colnames(data))-1)]])

train_label=factor(data[index,i])
test_label=factor(data[-index,i])

codebook=lvqinit(train,train_label)
#codebook=lvqinit(train,train_label,prior=rep(1/length(unique(train_label)),length(unique(train_label))))
#codebook=lvqinit(train,train_label,size=length(unique(train_label))*4,prior=rep(1/length(unique(train_label)),length(unique(train_label))))
#codebook=lvqinit(train,train_label,size=length(unique(train_label))*4)
codebook
buildcode=olvq1(train,train_label,codebook)
buildcode

predict=lvqtest(buildcode,test)

confusionMatrix(test_label,predict)