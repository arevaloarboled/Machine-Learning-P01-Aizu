Sys.setenv(LANG = "en")
##Libraries
library(class)
library(caret)
library(stats4)
#ds="car"
ds="iris"
#ds="bank"
train_data=read.csv(paste(ds,"/train.csv",sep=""))
test_data=read.csv(paste(ds,"/test.csv",sep=""))
i=colnames(train_data)[length(colnames(train_data))]
train_label=factor(train_data[,i])
test_label=factor(test_data[,i])
train=data.matrix(train_data[,colnames(train_data)[0:(length(colnames(train_data))-1)]])
test=data.matrix(test_data[,colnames(test_data)[0:(length(colnames(test_data))-1)]])
dd <- function(mu, sigma) {
     R = dnorm(x, mu, sigma)
     -sum(log(R))
 }
#x=train_data[train_data$V5==train_data[1,5],1]
#est=mle(LL, start = list(mu = 1, sigma=1))
#pnorm(test_data[1,1],coef(est)["mu"],coef(est)["sigma"])
coefficients=list()
for (j in unique(train_label)) {
	tmp=list()
	for (k in colnames(train_data[colnames(train_data)[0:(length(colnames(train_data))-1)]])) {
		x=train_data[train_data[,i]==j,k]
		#est=mle(dd, start = list(mu = mean(x), sigma=sd(x)),method="Nelder-Mead")
		s=sd(x)
		if(s==0){
			s=0.00001
		}
		est=mle(dd, start = list(mu = mean(x), sigma=s))
		#est=mle(dd, start = list(mu = mean(x), sigma=sd(x)), lower = c(-Inf, 0), upper = c(Inf, Inf))
		tmp[k]=list((c(coef(est)["mu"],coef(est)["sigma"],p=length(x)/length(train_label))))
		#tmp[k]=list((c(mu=mean(x),sigma=s,p=length(x)/length(train_label))))
	}
	coefficients[j]=list(tmp)
}
coefficients
#coefficients[[j]][["V2"]]["mu"]
p_cl <- function(z,c,cl) {
     p=1
     for (j in colnames(z)) {
     	p=p+pnorm(as.numeric(z[j]),as.numeric(c[[cl]][[j]]["mu"]),as.numeric(c[[cl]][[j]]["sigma"]))#*as.numeric(c[[cl]][[j]]["p"])
     	print(j)
     	print(pnorm(as.numeric(z[j]),as.numeric(c[[cl]][[j]]["mu"]),as.numeric(c[[cl]][[j]]["sigma"])))
     }
     #return((p/length(colnames(z)))*as.numeric(c[[cl]][[j]]["p"]))
     return((p/length(colnames(z))))
     #return(p*as.numeric(c[[cl]][[j]]["p"]))
     #return(p*as.numeric(c[[cl]][[j]]["p"]))
 }
#p_cl(test_data[15,0:(length(colnames(train_data))-1)],coefficients,as.character(test_data[1,i]))
#p_cl(test_data[15,0:(length(colnames(train_data))-1)],coefficients,as.character(test_data[11,i]))
#p_cl(test_data[15,0:(length(colnames(train_data))-1)],coefficients,as.character(test_data[21,i]))
P<-function(y,c,clss){
	q=0
	v=""
	for (k in clss) {
		tmp=p_cl(y,c,k)
		if(tmp>q){
			v=k
			q=tmp
		}
	}
	return(v)
}
predict=c()
for (l in seq(1,length(test_label))) {
	predict=c(predict,P(test_data[l,0:(length(colnames(train_data))-1)],coefficients,as.character(unique(train_label))))
}
cfm=confusionMatrix(factor(predict),test_label)
cfm