Sys.setenv(LANG = "en")
##Libraries
library(class)
library(caret)
##Load data
#data=read.csv("car/car.data",header=FALSE)
data=read.csv("bank/bank-additional-full.csv",sep=";")
#data=read.csv("iris/iris.data",header=FALSE)
##Ordinal data
ordinal_data=data
to_predic=""
for (i in colnames(data)) {
	if(! all(is.numeric(data[,i]))){
		ordinal_data[,i]=factor(data[,i],levels=c(matrix(unique(data[,i]))),labels=1:length(unique(data[,i])))
	}
	p=i
}
##Split data between training and testing
index = createDataPartition(data[,i], p = .8, list = F)
ordinal_data[,i]=data[,i]
train_data=ordinal_data[index,]
test_data=ordinal_data[-index,]
write.csv(train_data,file="bank/train.csv",row.names=FALSE)
write.csv(test_data,file="bank/test.csv",row.names=FALSE)