Sys.setenv(LANG = "en")
library(ggplot2)
library(caret)
#ds="car"
#ds="iris"
ds="bank"
test_data=read.csv(paste(ds,"/test.csv",sep=""))
predict=read.csv(paste(ds,"/knn.csv",sep=""),header=FALSE)

cfm=confusionMatrix(factor(predict$V1),test_data[,(length(colnames(test_data)))])

diag = diag(cfm$table) # number of correctly classified instances per class 
rowsums = apply(cfm$table, 1, sum) # number of instances per class
colsums = apply(cfm$table, 2, sum) # number of predictions per class
 recall= diag / colsums 
precision = diag / rowsums 
#mean(precision)
#mean(recall)
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
    theme(legend.position = "none",
      axis.text.x= element_text(size = 18),
      axis.text.y= element_text(size = 18),
      axis.title.x= element_text(size = 18,vjust = 0.1),
      axis.title.y= element_text(size = 18,angle = 90,vjust = 1.1),
      strip.text.x = element_text(size =18)
    ) +
    ggtitle(mytitle)
  return(p)
}


ggplotConfusionMatrix(cfm,mean(precision),mean(recall))
ggsave(paste(ds,"_knn_cfm.png",sep=""),plot=last_plot())
