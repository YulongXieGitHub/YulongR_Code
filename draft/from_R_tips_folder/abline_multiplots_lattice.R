
#
# check the correlation between small group of variables
#
# 1. Ch_W_Full and CH_W_Nolsl
myData.subset <- myData[,c("Ch_W_Full","Ch_W_NoIsl","Reach")]
cor.cef.all   <- cor(myData.subset[,c("Ch_W_Full","Ch_W_NoIsl")])[1,2]
cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Ch_W_Full","Ch_W_NoIsl")])[1,2]
cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Ch_W_Full","Ch_W_NoIsl")])[1,2]

plot.all   <- xyplot(myData.subset[,"Ch_W_Full"]                                   ~ myData.subset[,"Ch_W_NoIsl"],                                   type = "p", pch=16,col="black",xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Correlation between the two Channel Width variables (corcoef=",round(cor.cef.all,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_NoIsl"], type = "p", pch=16,col="red",  xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_NoIsl"], type = "p", pch=16,col="red",  xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

plot(plot.all,  split=c(1,1,1,2))
plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)

# 2. Grad10RM and GradBend
myData.subset <- myData[,c("Grade10RM","GradeBend","Reach")]
cor.cef.all   <- cor(myData.subset[,c("Grade10RM","GradeBend")])[1,2]
cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Grade10RM","GradeBend")])[1,2]
cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Grade10RM","GradeBend")])[1,2]

plot.all   <- xyplot(myData.subset[,"Grade10RM"]                                   ~ myData.subset[,"GradeBend"],                                   type = "p", pch=16,col="black",xlab="GradeBend",ylab="Grade10RM",main=paste("Correlation between the two River Garde Variables (corcoef=",round(cor.cef.all,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Grade10RM"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","GradeBend"], type = "p", pch=16,col="red",  xlab="GradeBend",ylab="Grade10RM",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Grade10RM"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","GradeBend"], type = "p", pch=16,col="red",  xlab="GradeBend",ylab="Grade10RM",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

plot(plot.all,  split=c(1,1,1,2))
plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)

# 3. Grad10RM and GradBend
myData.subset <- myData[,c("MedFlow","MeanFlow","Reach")]
cor.cef.all   <- cor(myData.subset[,c("MedFlow","MeanFlow")])[1,2]
cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("MedFlow","MeanFlow")])[1,2]
cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("MedFlow","MeanFlow")])[1,2]

plot.all   <- xyplot(myData.subset[,"MedFlow"]                                   ~ myData.subset[,"MeanFlow"],                                   type = "p", pch=16,col="black",xlab="MeanFlow",ylab="MedFlow",main=paste("Correlation between the two River Flows (corcoef=",round(cor.cef.all,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","MedFlow"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","MeanFlow"], type = "p", pch=16,col="red",  xlab="MeanFlow",ylab="MedFlow",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","MedFlow"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","MeanFlow"], type = "p", pch=16,col="red",  xlab="MeanFlow",ylab="MedFlow",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

plot(plot.all,  split=c(1,1,1,2))
plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)
dev.off()

# 4. Mean_z, min_z, max_z
myData.subset <- myData[,c("Mean_z","Min_z","Max_z","Reach")]
cor.cef.all   <- cor(myData.subset[,c("Mean_z","Min_z","Max_z")])[1,2]
cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Mean_z","Min_z","Max_Z")])[1,2]
cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Mean_z","Min_z","Max_z")])[1,2]

plot.all   <- xyplot(myData.subset[,"Mean_z"]                                   ~ myData.subset[,"Min_z"],                                   type = "p", pch=16,col="black",xlab="Min_z",ylab="Mean_z",main=paste("Correlation between the three river elevation variables (corcoef=",cor.cof.all,")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Mean_z"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","Min_z"], type = "p", pch=16,col="red",  xlab="Min_z",ylab="Mean_z",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Mean_z"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","Min_z"], type = "p", pch=16,col="red",  xlab="Min_z",ylab="Mean_z",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

plot(plot.all,  split=c(1,1,1,2))
plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)

