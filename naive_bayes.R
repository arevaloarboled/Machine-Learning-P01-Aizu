Sys.setenv(LANG = "en")
##Libraries
library(class)
library(caret)
library(stats4)
library(scales)
#library(e1071)
library(klaR)
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

if(ds=="car"){
	train_data=train_data[,-6]
	test_data=test_data[,-6]
}
model=NaiveBayes(as.formula(paste(i,"~.")),train_data)
#proc.time()-ptm
#ptm=proc.time()
pred=predict(model,test)
#proc.time()-ptm
cfm=confusionMatrix(pred$class,test_label)
cfm

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
  ggsave(paste(ds,"_bayes_cfm.png",sep=""),plot=last_plot())  
}

plotcfm(cfm,mean(precision),mean(recall))

#png(filename=paste(ds,"_bayes_model.png",sep=""))
plot(model)
#dev.off()