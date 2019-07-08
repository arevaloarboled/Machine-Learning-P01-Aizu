Sys.setenv(LANG = "en")
##Libraries
library(class)
library(caret)
library(scales)
#ptm=proc.time()
#ds="car"
#ds="iris"
ds="bank"
train_data=read.csv(paste(ds,"/train.csv",sep=""))
test_data=read.csv(paste(ds,"/test.csv",sep=""))
i=colnames(train_data)[length(colnames(train_data))]
train_label=factor(train_data[,i])
test_label=factor(test_data[,i])
train=data.matrix(train_data[,colnames(train_data)[0:(length(colnames(train_data))-1)]])
test=data.matrix(test_data[,colnames(test_data)[0:(length(colnames(test_data))-1)]])

##Make the codebook
findk<- function(){
  accuracy=c()
  k=c()
  mk=length(unique(train_label))*5
  title="Probability:"
  for (j in as.character(unique(train_label))) {
    title=paste(title,j,percent_format()(length(train_data[train_data[,i]==j,1])/length(train_label)))
  }
  for (i in seq(length(unique(train_label)),mk)) {
    codebook=lvqinit(train,train_label,size=i)   
    buildcode=olvq1(train,train_label,codebook)
    predict=lvqtest(buildcode,test)
    cfm=confusionMatrix(predict,test_label)
    k=c(k,i)
    accuracy=c(accuracy,percent_format()(cfm$overall[1]))
  }
  ggplot(data=data.frame(accuracy=accuracy,k=k), aes(x=k, y=accuracy, group=1)) +scale_x_discrete(breaks = seq(length(unique(train_label)),mk,by=3), labels= seq(length(unique(train_label)),mk,by=3),limits=seq(length(unique(train_label)),mk,by=3))+ geom_line()+ geom_point()+ theme(legend.position = "none", axis.text.x= element_text(size = 18), axis.text.y= element_text(size = 18), axis.title.x= element_text(size = 18,vjust = 0.1), axis.title.y= element_text(size = 18,angle = 90,vjust = 1.1), strip.text.x = element_text(size =18))+ggtitle(title)
  ggsave(paste(ds,"_lvq_k.png",sep=""),plot=last_plot())
  q()
}
#findk()
codebook=lvqinit(train,train_label,size=5,k=7)
#codebook=lvqinit(train,train_label,prior=rep(1/length(unique(train_label)),length(unique(train_label))))
#codebook=lvqinit(train,train_label,size=length(unique(train_label))*4,prior=rep(1/length(unique(train_label)),length(unique(train_label))))
#codebook=lvqinit(train,train_label,size=length(unique(train_label))*4)
#codebook=lvqinit(train,train_label,size=length(unique(train_label))*2,prior=rep(1/length(unique(train_label)),length(unique(train_label))),k=7)
#codebook
##Train the codebook
buildcode=olvq1(train,train_label,codebook)
#buildcode
#proc.time()-ptm
#ptm=proc.time()
##PCA and plot of vectors
pca_train<-function(){
pca=prcomp(t(rbind(train,buildcode$x)),center=TRUE)
to_p=data.frame(pca1=pca$rotation[,"PC1"],pca2=pca$rotation[,"PC2"],cl=c(as.character(train_label),as.character(buildcode$cl)),sh=c(rep("no_lvq",length(train_label)),rep("lvq",dim(buildcode$x)[1])),sz=c(rep("no_lvq",length(train_label)),rep("lvq",dim(buildcode$x)[1])))
#ggplot(to_p,aes(pca1,pca2))+geom_point(aes(colour=factor(to_p$cl),shape=factor(to_p$sh),size=to_p$sz))
ggplot(to_p,aes(pca1,pca2))+geom_point(aes(colour=factor(to_p$cl),shape=factor(to_p$sh),size=to_p$sz))+scale_shape_manual(values = c(15,1))+scale_size_manual(values = c(3,1))+ theme(legend.position = "none",
      axis.text.x= element_text(size = 18),
      axis.text.y= element_text(size = 18),
      axis.title.x= element_text(size = 18,vjust = 0.1),
      axis.title.y= element_text(size = 18,angle = 90,vjust = 1.1),
      strip.text.x = element_text(size =18)
    )
ggsave(paste(ds,"_lvq_train_plot.png",sep=""),plot=last_plot())
}
#pca_train()
##Test
predict=lvqtest(buildcode,test)
#proc.time()-ptm
pca_test<-function(){
pca=prcomp(t(rbind(test,buildcode$x)),center=TRUE)
to_p=data.frame(pca1=pca$rotation[,"PC1"],pca2=pca$rotation[,"PC2"],cl=c(as.character(test_label),as.character(buildcode$cl)),sh=c(predict==test_label,rep("lvq",dim(buildcode$x)[1])),sz=c(rep("no_lvq",length(test_label)),rep("lvq",dim(buildcode$x)[1])))
ggplot(to_p,aes(pca1,pca2))+geom_point(aes(colour=factor(to_p$cl),shape=factor(to_p$sh),size=to_p$sz))+scale_shape_manual(values = c(4,1, 15))+scale_size_manual(values = c(3,1)) + theme(legend.position = "none",
      axis.text.x= element_text(size = 18),
      axis.text.y= element_text(size = 18),
      axis.title.x= element_text(size = 18,vjust = 0.1),
      axis.title.y= element_text(size = 18,angle = 90,vjust = 1.1),
      strip.text.x = element_text(size =18)
    )
ggsave(paste(ds,"_lvq_test_plot.png",sep=""),plot=last_plot())
}
#pca_test()
##Matrix confusion
cfm=confusionMatrix(predict,test_label)

diag = diag(cfm$table) # number of correctly classified instances per class 
rowsums = apply(cfm$table, 1, sum) # number of instances per class
colsums = apply(cfm$table, 2, sum) # number of predictions per class
recall = diag / colsums 
precision = diag / rowsums 





plotcfm <- function(m,p,r){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),"Precision",percent_format()(p),"recall",percent_format()(r))
  ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none",
      axis.text.x= element_text(size = 18),
      axis.text.y= element_text(size = 18),
      axis.title.x= element_text(size = 18,vjust = 0.1),
      axis.title.y= element_text(size = 18,angle = 90,vjust = 1.1),
      strip.text.x = element_text(size =18)
    ) +
    ggtitle(mytitle)
  ggsave(paste(ds,"_lvq_cfm.png",sep=""),plot=last_plot())  
}

#plotcfm(cfm,mean(precision),mean(recall))
##Plot perceptron
#library(reticulate)
#source_python('graphviz.py')
#plot_qvl(buildcode$x,as.character(buildcode$cl),colnames(train))