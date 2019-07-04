Sys.setenv(LANG = "en")
library(ggplot2)
library(caret)
#ds="car"
ds="iris"
test_data=read.csv(paste(ds,"/test.csv",sep=""))
predict=read.csv(paste(ds,"/knn.csv",sep=""),header=FALSE)

cfm=confusionMatrix(test_data[,(length(colnames(test_data)))],factor(predict$V1))

diag = diag(cfm$table) # number of correctly classified instances per class 
rowsums = apply(cfm$table, 1, sum) # number of instances per class
colsums = apply(cfm$table, 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 
#mean(precision)
#mean(recall)
#proc.time()-ptm

library(scales)

ggplotConfusionMatrix <- function(m,p,r){
  #mytitle <- paste("Accuracy", percent_format()(m$overall[1]), "Kappa", percent_format()(m$overall[2]))
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),"Precision",percent_format()(p),"recall",percent_format()(r))
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
ggsave(paste(ds,"_knn_cfm.png",sep=""),plot=last_plot())
