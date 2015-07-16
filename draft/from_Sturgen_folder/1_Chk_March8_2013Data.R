#
# 1_Chk_March8_2013Data.R
#
#
# eliminate all stuff
rm(list = ls(all = TRUE))


# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}


# today's month, day and year in the format of "Thu Jun 16 08:48:36 2011", 5 fields separated by space
today.month  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
today.day    <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
today.year   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]
today.hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
today.minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
today.second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]


# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_scripts"
}
setwd(Path.Current)

# Data Folder
Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/_data_received/sturg_spatial/data/Habitat_Metrics_Output"
Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/1_Chk_March8_2013Data"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}


FL.Data.IN <- paste(Path.Data.IN,"Task_Progress_CRVedit_2013Mar8_YLX.csv",sep="/")
FL.LOG     <- paste(Path.log,"1_Chk_March8_2013Data.log",sep="/")	
FL.PDF     <- paste(Path.Out,"1_Chk_March8_2013Data.pdf",sep="/")	
FL.SUM.cat <- paste(Path.Out,"1_Chk_March8_2013Data_cat.sum",sep="/")
FL.SUM.num <- paste(Path.Out,"1_Chk_March8_2013Data_num.sum",sep="/")
if (!file.exists(FL.Data.IN))  {stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.LOG))      {print(paste(FL.LOG,    "exist.Delete it!")); file.remove(FL.LOG)}
if  (file.exists(FL.PDF))      {print(paste(FL.PDF,    "exist.Delete it!")); file.remove(FL.PDF)}
if  (file.exists(FL.SUM.cat))  {print(paste(FL.SUM.cat,"exist.Delete it!")); file.remove(FL.SUM.cat)}
if  (file.exists(FL.SUM.num))  {print(paste(FL.SUM.num,"exist.Delete it!")); file.remove(FL.SUM.num)}


library("lattice")

# open pdf file for outputting plots
pdf(file = FL.PDF,         paper="a4r",width=0,height=0)	


# read March 8, 2013 data 
myData <- read.csv(file=FL.Data.IN,sep=",",stringsAsFactors=TRUE,header=TRUE)
myData.upper <- subset(myData,Reach=="Upper")
myData.lower <- subset(myData,Reach=="Lower")



# check and re-assign missing data


# check each individual variable
mySummary <- NULL
idx <- 1
for (var in names(myData))
{
	if (is.factor(myData[,var]))
	{	
		no.NA <- sum(is.na(myData[,var]))
		no.NA.upper <- sum(is.na(myData[myData[,"Reach"] == "Upper",var]))
		no.NA.lower <- sum(is.na(myData[myData[,"Reach"] == "Lower",var]))
		
		myData.tmp <- myData[!(is.na(myData[,var])),var]
		myData.tmp.upper <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Upper",var]
		myData.tmp.lower <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Lower",var]
		
		# plot.all <- barplot(table(myData.tmp),xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		# layout(matrix(c(1,2),nrow=2,ncol=1),height = c(1,1))	
		# layout(rbind(c(1,1),c(0,2)),respect=rbind(FALSE,TRUE))		             
	             
	             
		plot.all   <- barchart(myData.tmp,      xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		plot.upper <- barchart(myData.tmp.upper,xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA.upper," missing values)\nUpper",sep=""))
		plot.lower <- barchart(myData.tmp.lower,xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA.lower," missing values)\nLower",sep=""))
		
		plot(plot.all,  split=c(1,1,1,2))
		plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
		plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)
		
		# output to the summary file
		cat(paste(var,",\n",sep=""),file=FL.SUM.cat,append=TRUE)
		write.table(as.data.frame(table(myData[,var])),file=FL.SUM.cat,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)		
	}else{
		myData[myData[,var] == -9999,var] <- NaN
		
		no.NA <- sum(is.na(myData[,var]))
		no.NA.upper <- sum(is.na(myData[myData[,"Reach"] == "Upper",var]))
		no.NA.lower <- sum(is.na(myData[myData[,"Reach"] == "Lower",var]))
		
		myData.tmp <- myData[!(is.na(myData[,var])),var]
		myData.tmp.upper <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Upper",var]
		myData.tmp.lower <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Lower",var]

		
		# hist(myData.tmp,freq=FALSE,nclass=100,xlab=var,ylab="Density",main=paste("Distribution of ",var,"(with ",no.NA," missing values)",sep=""))
		plot.all   <- histogram(myData.tmp,      freq=FALSE,nint=100, xlab=var,ylab="Density",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		plot.upper <- histogram(myData.tmp.upper,freq=FALSE,nint=100, xlab=var,ylab="Density",main=paste("Distribution of ",var,"(with ",no.NA.upper," missing values)\nUpper",sep=""))
		plot.lower <- histogram(myData.tmp.lower,freq=FALSE,nint=100, xlab=var,ylab="Density",main=paste("Distribution of ",var,"(with ",no.NA.lower," missing values)\nLower",sep=""))
		
		plot(plot.all,  split=c(1,1,1,2))
		plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
		plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)		
		
		#
		# get summary of the data
		mySummary <- cbind(
			     data.frame(min.all = apply(myData[,var,drop=FALSE],2,min,na.rm=TRUE),
					med.all = apply(myData[,var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
					max.all = apply(myData[,var,drop=FALSE],2,max,na.rm=TRUE),
					sd.all  = apply(myData[,var,drop=FALSE],2,sd,na.rm=TRUE),
					no.all  = apply(myData[,var,drop=FALSE],2,function(x){length(x)}),
					NA.all  = apply(myData[,var,drop=FALSE],2,function(x){sum(is.na(x))})),
			     data.frame(min.upper = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,min,na.rm=TRUE),
					med.upper = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
					max.upper = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,max,na.rm=TRUE),
					sd.upper  = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,sd,na.rm=TRUE),
					no.upper  = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,function(x){length(x)}),
					NA.upper  = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,function(x){sum(is.na(x))})),
			     data.frame(min.lower = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,min,na.rm=TRUE),
					med.lower = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
					max.lower = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,max,na.rm=TRUE),
					sd.lower  = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,sd,na.rm=TRUE),
					no.lower  = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,function(x){length(x)}),
					NA.lower  = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,function(x){sum(is.na(x))})))                        
		
		if (idx == 1)
		{
			cat(",",file=FL.SUM.num,append=TRUE)
			write.table(mySummary,file=FL.SUM.num,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
		}else{
			write.table(mySummary,file=FL.SUM.num,sep=",",col.names=FALSE,row.names=TRUE,append=TRUE)
		}
		
	}
	
	# make a table for missing data
	idx <- idx + 1
}


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


# 4. Mean_z, min_z, max_z
myData.subset <- myData[,c("Mean_z","Min_z","Max_z","Reach")]
cor.cef.all   <- cor(myData.subset[,c("Mean_z","Min_z","Max_z")])
cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Mean_z","Min_z","Max_z")])
cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Mean_z","Min_z","Max_z")])

cor.string.all   <- paste("cor among elevation vars are ",round(cor.cef.all[1,2],digits=2),  " and ",round(cor.cef.all[1,3],digits=2),  " and ",round(cor.cef.all[2,3],digits=2),  " (all data)",sep="")
cor.string.upper <- paste("cor among elevation vars are ",round(cor.cef.upper[1,2],digits=2)," and ",round(cor.cef.upper[1,3],digits=2)," and ",round(cor.cef.upper[2,3],digits=2)," (UpperStream)",sep="")
cor.string.lower <- paste("cor among elevation vars are ",round(cor.cef.lower[1,2],digits=2)," and ",round(cor.cef.lower[1,3],digits=2)," and ",round(cor.cef.lower[2,3],digits=2)," (Lower Stream)", sep="")



par(mfrow = c(3,1))
plot(myData.subset[,c("Mean_z")],col=c("red"),type="l",lty=1,ylab="mean/min/max elevation (all data)",main = cor.string.all)
lines(myData.subset[,c("Min_z")],col=c("blue"),type="l",lty=1)
lines(myData.subset[,c("Max_z")],col=c("green"),type="l",lty=1)

# for upper stream
plot(myData.subset[myData.subset[,"Reach"] == "Upper",c("Mean_z")],col=c("red"),type="l",lty=1,ylab="mean/min/max elevation (Upper Stream)",main = cor.string.upper)
lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Min_z")],col=c("blue"),type="l",lty=1)
lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Max_z")],col=c("green"),type="l",lty=1)

# for lower stream
plot(myData.subset[myData.subset[,"Reach"] == "Lower",c("Mean_z")],col=c("magenta"),type="l",lty=1,ylab="mean/min/max elevation (Lower Stream)",main = cor.string.lower)
lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Min_z")],col=c("cyan"),type="l",lty=1)
lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Max_z")],col=c("light green"),type="l",lty=1)


dev.off()

# get summary of the data

