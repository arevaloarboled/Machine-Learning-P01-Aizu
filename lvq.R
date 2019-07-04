Sys.setenv(LANG = "en")
##Libraries
library(class)
library(caret)
#ptm<-proc.time()
##Load data
#data=read.csv("car/car.data",header=FALSE)
#data=read.csv("bank/bank-additional-full.csv",sep=";")
data=read.csv("iris/iris.data",header=FALSE)
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
#ordinal_data[,i]=data[,i]
train_data=ordinal_data[index,]
test_data=ordinal_data[-index,]
#write.csv(train_data,file="iris/train.csv",row.names=FALSE)
#write.csv(test_data,file="iris/test.csv",row.names=FALSE)
train=data.matrix(train_data[,colnames(train_data)[0:(length(colnames(data))-1)]])
test=data.matrix(test_data[,colnames(test_data)[0:(length(colnames(data))-1)]])

train_label=factor(data[index,i])
test_label=factor(data[-index,i])

##Make the codebook
#codebook=lvqinit(train,train_label)
#codebook=lvqinit(train,train_label,prior=rep(1/length(unique(train_label)),length(unique(train_label))))
#codebook=lvqinit(train,train_label,size=length(unique(train_label))*4,prior=rep(1/length(unique(train_label)),length(unique(train_label))))
#codebook=lvqinit(train,train_label,size=length(unique(train_label))*4)
codebook=lvqinit(train,train_label,size=length(unique(train_label))*2,prior=rep(1/length(unique(train_label)),length(unique(train_label))),k=7)
#codebook
##Train the codebook
buildcode=olvq1(train,train_label,codebook)
#buildcode

##PCA and plot of vectors
# pca=prcomp(t(rbind(train,buildcode$x)),center=TRUE)
# to_p=data.frame(pca1=pca$rotation[,"PC1"],pca2=pca$rotation[,"PC2"],cl=c(as.character(train_label),as.character(buildcode$cl)),sh=c(rep("no_lvq",length(train_label)),rep("lvq",dim(buildcode$x)[1])),sz=c(rep(0.5,length(train_label)),rep(0.6,dim(buildcode$x)[1])))
# ggplot(to_p,aes(pca1,pca2))+geom_point(aes(colour=factor(to_p$cl),shape=factor(to_p$sh),size=to_p$sz))
# ggsave("lvq_plot.png",plot=last_plot())

##Test
predict=lvqtest(buildcode,test)

##Matrix confusion
cfm=confusionMatrix(test_label,predict)
#proc.time()-ptm

library(scales)

ggplotConfusionMatrix <- function(m){
  #mytitle <- paste("Accuracy", percent_format()(m$overall[1]), "Kappa", percent_format()(m$overall[2]))
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

ggplotConfusionMatrix(cfm)
ggsave("lvq_cfm.png",plot=last_plot())
##Plot perceptron
library(reticulate)
source_python('graphviz.py')
plot_qvl(buildcode$x,as.character(buildcode$cl),colnames(train))