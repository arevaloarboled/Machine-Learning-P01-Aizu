Sys.setenv(LANG = "en")
##Libraries
library(class)
library(caret)
#ds="car"
ds="iris"
train_data=read.csv(paste(ds,"/train.csv",sep=""))
test_data=read.csv(paste(ds,"/test.csv",sep=""))
i=colnames(train_data)[length(colnames(train_data))]
train_label=factor(train_data[,i])
test_label=factor(test_data[,i])
train=data.matrix(train_data[,colnames(train_data)[0:(length(colnames(train_data))-1)]])
test=data.matrix(test_data[,colnames(test_data)[0:(length(colnames(test_data))-1)]])

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
pca=prcomp(t(rbind(train,buildcode$x)),center=TRUE)
to_p=data.frame(pca1=pca$rotation[,"PC1"],pca2=pca$rotation[,"PC2"],cl=c(as.character(train_label),as.character(buildcode$cl)),sh=c(rep("no_lvq",length(train_label)),rep("lvq",dim(buildcode$x)[1])),sz=c(rep(0.5,length(train_label)),rep(0.6,dim(buildcode$x)[1])))
ggplot(to_p,aes(pca1,pca2))+geom_point(aes(colour=factor(to_p$cl),shape=factor(to_p$sh),size=to_p$sz))
ggsave(paste(ds,"_lvq_plot.png",sep=""),plot=last_plot())

##Test
predict=lvqtest(buildcode,test)

##Matrix confusion
cfm=confusionMatrix(test_label,predict)

diag = diag(cfm$table) # number of correctly classified instances per class 
rowsums = apply(cfm$table, 1, sum) # number of instances per class
colsums = apply(cfm$table, 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 

#proc.time()-ptm

library(scales)

ggplotConfusionMatrix <- function(m,p,r){
  #mytitle <- paste("Accuracy", percent_format()(m$overall[1]), "Kappa", percent_format()(m$overall[2]))
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),"Precision",percent_format()(p),"recall",percent_format()(r))
  #mytitle <- paste("Accuracy", percent_format()(m$overall[1]))
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

ggplotConfusionMatrix(cfm,mean(precision),mean(recall))
ggsave(paste(ds,"_lvq_cfm.png",sep=""),plot=last_plot())
##Plot perceptron
#library(reticulate)
#source_python('graphviz.py')
#plot_qvl(buildcode$x,as.character(buildcode$cl),colnames(train))