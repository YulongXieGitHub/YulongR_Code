#
# 1b_CrownePlaza_ReturnAirT.R 
# 
#
# October 7, 2009
# Checked  on Nov 11, 2009
# Reconciled on Nov 13, 2009
# -------------------------------------------------------------------------
#
# 	eliminate all stuff
# 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number


# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "/phome/d3l143/FY2010_NAP_DataAnalysis/CrownePlaza_allData/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2010_NAP_DataAnalysis/CrownePlaza_allData/0_scripts"
}
setwd(Path.Current)

# -------------------------------------------------------------------------------------------------
# 	include needed objects
# -------------------------------------------------------------------------------------------------
source(paste(Path.Current,"0_CrownePlaza_Function.R",sep="/"))


# -------------------------------------------------------------------------------------------------
#	setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.out   <- "../1_CrownePlaza_ReturnAirT"	# OUTPUT processed result directory
Path.log   <- "../0_log"			# OUTPUT log  directory
Path.Data  <- "../1_CrownePlaza_ReturnAirT"	# INPUT  data folder retrieved from "nac" on "nac.pnl.gov" server

if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Data)){stop(paste(" INPUT  data folder retrieved from \"nac\" on \"nac.pnl.gov\" server does NOT existing\n",sep=""))}

# ------------------------------------------------------------------------------------------------- 
# 	create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
FL.LOG <- paste(Path.log,"1b_CrownePlaza_ReturnAirT_BaseCase.log",sep="/")	# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for data processing script [1b_CrownePlaza_ReturnAirT_BaseCase.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
          "*                            [1b_CrownePlaza_ReturnAirT_BaseCase.R]                            *",
          "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)


# -------------------------------------------------------------------------------------------------
# 	define OUTPUT files
# -------------------------------------------------------------------------------------------------
FL.rawData.OBJ      	<- paste(Path.Data,paste("CrownePlaza_ReturnAirT.Rdata",sep=""),sep="/")			# INPUT  DATA in Rdata Objects
FL.aggData.OBJ      	<- paste(Path.Data,paste("CrownePlaza_ReturnAirT_Sum.Rdata",sep=""),sep="/")			# INPUT  SUM  in Rdata Objects

FL.PDF.TimeSeries   	<- paste(Path.out,paste("CrownePlaza_ReturnAirT_TimeSeries.pdf",sep=""),sep="/")		# OUTPUT PDF general
FL.PDF.PR_roomWise  	<- paste(Path.out,paste("CrownePlaza_ReturnAirT_Profile_roomWise.pdf",sep=""),sep="/")		# OUTPUT PDF general
FL.PDF.PR_roomAcross	<- paste(Path.out,paste("CrownePlaza_ReturnAirT_Profile_roomAcross.pdf",sep=""),sep="/")	# OUTPUT PDF general
FL.PDF.BoxPlot_rawData	<- paste(Path.out,paste("CrownePlaza_ReturnAirT_Boxplot_rawData.pdf",sep=""),sep="/")		# OUTPUT PDF general
FL.PDF.BoxPlot_aggData	<- paste(Path.out,paste("CrownePlaza_ReturnAirT_Boxplot_aggData.pdf",sep=""),sep="/")		# OUTPUT PDF general

if (!file.exists(FL.rawData.OBJ)){stop(paste(FL.rawData.OBJ," should exist. Find out why it is missing!"));file.remove(FL.rawData.OBJ)}				# remove existing OUTPUT files
if (!file.exists(FL.aggData.OBJ)){stop(paste(FL.aggData.OBJ," should exist. Find out why it is missing!"));file.remove(FL.aggData.OBJ)}				# remove existing OUTPUT files
if (file.exists(FL.PDF.PR_roomWise)){print(paste(FL.PDF.PR_roomWise," exist. Delete it!"));file.remove(FL.PDF.PR_roomWise)}					# remove existing OUTPUT files
if (file.exists(FL.PDF.PR_roomAcross)){print(paste(FL.PDF.PR_roomAcross," exist. Delete it!"));file.remove(FL.PDF.PR_roomAcross)}				# remove existing OUTPUT files
if (file.exists(FL.PDF.BoxPlot_rawData)){print(paste(FL.PDF.BoxPlot_rawData," exist. Delete it!"));file.remove(FL.PDF.BoxPlot_rawData)}				# remove existing OUTPUT files
if (file.exists(FL.PDF.BoxPlot_aggData)){print(paste(FL.PDF.BoxPlot_aggData," exist. Delete it!"));file.remove(FL.PDF.BoxPlot_aggData)}				# remove existing OUTPUT files
if (file.exists(FL.PDF.TimeSeries)){print(paste(FL.PDF.TimeSeries," exist. Delete it!"));file.remove(FL.PDF.TimeSeries)}					# remove existing OUTPUT files
cat(paste("\nAll paths and files are defined\n"),file=FL.LOG,append=TRUE)
cat(paste("\nAll paths and files are defined\n"))


# -------------------------------------------------------------------------------------------------
# load the data files
# -------------------------------------------------------------------------------------------------
load(FL.rawData.OBJ)	# raw data
load(FL.aggData.OBJ)	# hourly aggregated data objects
cat(paste("\nLoaded the data retrieved from the database and the subsequently hourly aggregated data!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nLoaded the data retrieved from the database and the subsequently hourly aggregated data!\n"))


myData[,"day.week"] <- factor(myData[,"day.week"],levels=week.label,   labels=week.names)	# convert 1-7 to Sun,Mon,...,Sat
myData[,"day.type"] <- factor(myData[,"day.type"], levels=dayType.label,labels=dayType.names)	# convert 1 & 7 to "wkend and 2-6 to "wkday"



# -------------------------------------------------------------------------------------------------
# add a "month-day" field
# -------------------------------------------------------------------------------------------------
myData <- cbind(myData,month.day = paste(myData[,"month"],myData[,"day"],sep="-"))
Data.daily.roomWise <- cbind(Data.daily.roomWise,month.day = paste(Data.daily.roomWise[,"month"],Data.daily.roomWise[,"day"],sep="-"))

# -------------------------------------------------------------------------------------------------
# plot time series
# -------------------------------------------------------------------------------------------------
cat(paste("\nStart plotting the time series\n"),file=FL.LOG,append=TRUE)
cat(paste("\nStart plotting the time series!\n"))

pdf(file = FL.PDF.TimeSeries,paper="a4r", width=0, height=0)
	# plot time series of all gues room together in a single page (free scale)
	plot.obj <- xyplot(Value ~ chronDate | as.factor(myData[,"room.name"]),
	                   data=myData,type="l",
         		   xlab="Date/time",ylab = expression("Return Air T F"^{o}),
			   main="Time Series (free scale)",
    		           col="red",
    		           # layout = c(1,2),
    		           scales=list(y="free"),					# free scale
		   	   panel = function(x,y,...) {
			           panel.xyplot(x, y,type="l",lty=1,col=c("red"))
			   panel.abline(h=c(60,80),lty=c(2,2),col=c("blue","cyan"))}
			   # panel.abline(v=day4plot,lty=2,lwd=0.2,col="grey")}		# indiscenible
    		           )
	plot(plot.obj)
	
	# plot time series of all gues room together in a single page (same scale)
	plot.obj <- xyplot(Value ~ chronDate | as.factor(myData[,"room.name"]),
	                   data=myData,type="l",
         		   xlab="Date/time",ylab = expression("Return Air T F"^{o}),
			   main="Time Series (same scale)",
    		           col="red",
		   	   panel = function(x,y,...) {
			           panel.xyplot(x, y,type="l",lty=1,col=c("red"))
			   panel.abline(h=c(60,80),lty=c(2,2),col=c("blue","cyan"))}
			   # panel.abline(v=day4plot,lty=2,lwd=0.2,col="grey")}		# indiscenible
    		           )
	plot(plot.obj)	
	
	# one page for each individual room
	for (lab.meter in as.character(unique(myData[,"meter.name"])))
	{
		ID.meter <- myMeter.unique[as.character(lab.meter),"meter.ID"]

		plot(myData[myData[,"meter.name"]==as.character(lab.meter),"chronDate"],
		     myData[myData[,"meter.name"]==as.character(lab.meter),"Value"],
		     type="l",lty=1,col="cyan",cex=0.3,
		     main = paste(paste("Time Series of [",lab.meter,"](meterID_",ID.meter,")",sep=""),"\n(cyan: raw data & red: hourly average)",sep=""),
		     xlab = "date/time",ylab = expression("Return Air T F"^{o}),
		     ylim = c(50,95))
		lines(Data.daily.roomWise[Data.daily.roomWise[,"meter.name"]==as.character(lab.meter),"chronDate"],
		      Data.daily.roomWise[Data.daily.roomWise[,"meter.name"]==as.character(lab.meter),"Value"],
		      type="l",lty=1,col="red",cex=0.7)
		      
		abline(h=c(50,95),lty=2,col="black")
		abline(v=day4plot,lty=2,lwd=0.2,col="grey")
	}
dev.off()
cat(paste("\nTime Series plotted!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nTime Series plotted!\n"))



# -------------------------------------------------------------------------------------------------
# plot the pdf WITHIN each guest room
# -------------------------------------------------------------------------------------------------
cat(paste("\nPlot overall averaged profile for each guest room!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nPlot overall averaged profile for each guest room!\n"))
pdf(file = FL.PDF.PR_roomWise,paper="a4r", width=0, height=0)
	# *************************************************************************************************
	# plot [Data.allDays.roomWise] the overall averaged profile of across all day for each room (meter.name) together in a single page
	# *************************************************************************************************
	# get the y limit for plotting
	y.limit <- range(Data.allDays.roomWise[,"Value"])

	# round to tenth place		
	y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)

	key.dailyCurve <- list(title="Room",columns=5,space="top",cex=0.75,
			       text=list(paste(levels(as.factor(Data.allDays.roomWise[,"room.name"])),sep=" ")),
			       type = "b",
			       lines=list(lty=lty.array[1: nlevels(as.factor(Data.allDays.roomWise[,"room.name"]))],
					  col=col.array[1: nlevels(as.factor(Data.allDays.roomWise[,"room.name"]))],
					  pch=pch.array[1: nlevels(as.factor(Data.allDays.roomWise[,"room.name"]))]))

	# 				   
	plot.obj <- xyplot(Data.allDays.roomWise[,"Value"] ~ Data.allDays.roomWise[,"hour"]  ,
			   group = Data.allDays.roomWise[,"room.name"],
			   xlab="Hour",ylab = expression("Return Air T F"^{o}),
			   main="Profile of each guest room across multiple days",
			   cex=0.5,type="b",
			   key = key.dailyCurve,
			   lty=lty.array[1: nlevels(as.factor(Data.allDays.roomWise[,"room.name"]))],		   
			   col=col.array[1: nlevels(as.factor(Data.allDays.roomWise[,"room.name"]))],
			   pch=pch.array[1: nlevels(as.factor(Data.allDays.roomWise[,"room.name"]))],
			   scales=list(x = list(at     = seq(from=0,to=23,by=1),
						labels = seq(from=0,to=23,by=1),
						limits = c(-1,25)),
				       y = y.limit)
			    )	
	plot(plot.obj)  

	# 
	# plot [Data.allDays.roomWise] the overall averaged profile of across all day for each room (meter.name) separately 
	# 
	# all plots in a single page
	plot.obj <- xyplot(Data.allDays.roomWise[,"Value"] ~ Data.allDays.roomWise[,"hour"]  | as.factor(Data.allDays.roomWise[,"room.name"]),
			   xlab="Hour",ylab = expression("Return Air T F"^{o}),
			   main="Profile of each guest room across multiple days",
			   cex=0.5,type="b",
			   lty=1,
			   col="red",
			   pch=16,
			   between = list(y=0.5),
			   scales=list(x = list(at     = seq(from=0,to=23,by=1),
						labels = seq(from=0,to=23,by=1),
						limits = c(-1,25)),
				       y = y.limit)
			   )	
	plot(plot.obj) 

	cat(paste("\nProfile average on all data is plotted!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nProfile average on all data is plotted!\n"))



	# *************************************************************************************************	
	# plot [Data.dayType.roomWise]  the averaged profile separately across all "weekday" OR "weekend" days for each guest room [meter.name] 
	# *************************************************************************************************
	cat(paste("\nPlot overall averaged profile at [weekday] & [weekend] for each guest room!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nPlot overall averaged profile at [weekday] & [weekend] for each guest room!\n"))

	# get the y limit for plotting
	y.limit <- range(Data.dayType.roomWise[,"Value"])

	# round to tenth place		
	y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)

	key.dailyCurve <- list(title="Room",columns=5,space="top",cex=0.75,
			       text=list(paste(levels(as.factor(Data.dayType.roomWise[,"room.name"])),sep=" ")),
			       type = "b",
			       lines=list(lty=lty.array[1: nlevels(as.factor(Data.dayType.roomWise[,"room.name"]))],
					  col=col.array[1: nlevels(as.factor(Data.dayType.roomWise[,"room.name"]))],
					  pch=pch.array[1: nlevels(as.factor(Data.dayType.roomWise[,"room.name"]))]))

	# 				   
	plot.obj <- xyplot(Data.dayType.roomWise[,"Value"] ~ Data.dayType.roomWise[,"hour"] | as.factor(Data.dayType.roomWise[,"day.type"]),
			   group = Data.dayType.roomWise[,"room.name"],
			   xlab="Hour",ylab = expression("Return Air T F"^{o}),
			   main="Profile of each guest room across multiple [weekdays] or [weekends]",
			   cex=0.5,type="b",
			   key = key.dailyCurve,
			   lty=lty.array[1: nlevels(as.factor(Data.dayType.roomWise[,"room.name"]))],		   
			   col=col.array[1: nlevels(as.factor(Data.dayType.roomWise[,"room.name"]))],
			   pch=pch.array[1: nlevels(as.factor(Data.dayType.roomWise[,"room.name"]))],
			   scales=list(x = list(at     = seq(from=0,to=23,by=1),
						labels = seq(from=0,to=23,by=1),
						limits = c(-1,25)),
				       y = y.limit),
			   layout = c(1,2)
			    )	
	plot(plot.obj)  

	# 
	# plot [Data.dayType.roomWise] the overall averaged profile of across all day for each room (meter.name) separately 
	# 
	# all plots in a single page
	key.dailyCurve <- list(title="Day Type",columns=2,space="top",cex=0.75,
			       text=list(paste(levels(as.factor(Data.dayType.roomWise[,"day.type"])),sep=" ")),
			       type = "b",
			       lines= list(lty=rep("solid",100)[1: nlevels(as.factor(Data.dayType.roomWise[,"day.type"]))],
					   col=col.array[1: nlevels(as.factor(Data.dayType.roomWise[,"day.type"]))],
					   pch=pch.array[1: nlevels(as.factor(Data.dayType.roomWise[,"day.type"]))]))
			       # points=list(pch=pch.array[1: nlevels(as.factor(Data.dayType.roomWise[,"day.type"]))]))
			       #	   col=col.array[1: nlevels(as.factor(Data.dayType.roomWise[,"day.type"]))])
			       #		  ))

	plot.obj <- xyplot(Data.dayType.roomWise[,"Value"] ~ Data.dayType.roomWise[,"hour"]  | as.factor(Data.dayType.roomWise[,"room.name"]),
			   group = Data.dayType.roomWise[,"day.type"],
			   xlab="Hour",ylab = expression("Return Air T F"^{o}),
			   main="Profile of each guest room across multiple [weekdays] or [weekends]",
			   cex=0.5,type="b",
			   key = key.dailyCurve,
			   lty=lty.array[1: nlevels(as.factor(Data.dayType.roomWise[,"day.type"]))],		   
			   col=col.array[1: nlevels(as.factor(Data.dayType.roomWise[,"day.type"]))],
			   pch=pch.array[1: nlevels(as.factor(Data.dayType.roomWise[,"day.type"]))],
			   scales=list(x = list(at     = seq(from=0,to=23,by=1),
						labels = seq(from=0,to=23,by=1),
						limits = c(-1,25)),
				       y = y.limit)
			    )		
	plot(plot.obj) 

	cat(paste("\nProfile average on all [weekday] & [weekend] data is plotted!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nProfile average on all [weekday] & [weekend] data is plotted!\n"))

	# *************************************************************************************************			   	
	# plot the overall averaged profile for each individual day of each guest room [meter.name]
	# *************************************************************************************************
	cat(paste("\nPlot profile at each individual day for each guest room!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nPlot profile at each individual day for each guest room!\n"))
	# get the y limit for plotting
	y.limit <- range(Data.daily.roomWise[,"Value"])

	# round to tenth place		
	y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)

	# all plots in a single page
	key.dailyCurve <- list(title="dayType",columns=1,space="right",cex=0.75,
			       text=list(paste(levels(as.factor(Data.daily.roomWise[,"month.day"])),sep=" ")),
			       type = "l",
			       lines=list(lty=lty.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
					  col=col.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
					  pch=pch.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))]))

	plot.obj <- xyplot(Data.daily.roomWise[,"Value"] ~ Data.daily.roomWise[,"hour"] | as.factor(Data.daily.roomWise[,"room.name"]),
			   group =Data.daily.roomWise[,"month.day"],
			   # key = key.dailyCurve,
			   xlab="Hour",ylab = expression("Return Air T F"^{o}),
			   main="Profile of each guest room at each individual day",
			   cex=0.5,
			   type="l",
			   lty=rep("solid",100)[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
			   col=col.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
			   # pch=pch.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
			   between = list(y=0.5),
			   scales=list(x = list(at     = seq(from=0,to=23,by=1),
						labels = seq(from=0,to=23,by=1),
						limits = c(-1,25)),
				       y= y.limit)
			   )	
	plot(plot.obj)  



	# all plots in a single page
	key.dailyCurve <- list(title="month.day",columns=1,space="right",cex=0.75,
			       text=list(paste(levels(as.factor(Data.daily.roomWise[,"month.day"])),sep=" ")),
			       type = "b",
			       lines=list(lty=lty.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
					  col=col.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
					  pch=pch.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))]))

	plot.obj <- xyplot(Data.daily.roomWise[,"Value"] ~ Data.daily.roomWise[,"hour"] | as.factor(Data.daily.roomWise[,"day.type"]) +as.factor(Data.daily.roomWise[,"room.name"]) ,
			   group =Data.daily.roomWise[,"month.day"],
			   # key = key.dailyCurve,
			   xlab="Hour",ylab = expression("Return Air T F"^{o}),
			   main="Profile of each guest room at each individual [weekday] and [weekend]",
			   cex=0.5,type="l",
			   lty=rep("solid",100)[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
			   col=col.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
			   pch=pch.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
			   between = list(y=0.5),
			   scales=list(x = list(at     = seq(from=0,to=23,by=1),
						labels = seq(from=0,to=23,by=1),
						limits = c(-1,25)),
				       y= y.limit),
			   layout = c(6,5)
			   )	
	plot(plot.obj)  

	# one plot in a separate page
	plot.obj <- xyplot(Data.daily.roomWise[,"Value"] ~ Data.daily.roomWise[,"hour"] | as.factor(Data.daily.roomWise[,"day.type"]) +as.factor(Data.daily.roomWise[,"room.name"]) ,
			   group =Data.daily.roomWise[,"month.day"],
			   key = key.dailyCurve,
			   xlab="Hour",ylab = expression("Return Air T F"^{o}),
			   main="Each Individual Day of Each Room",
			   cex=0.5,type="b",
			   lty=rep("solid",100)[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
			   col=col.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
			   pch=pch.array[1: nlevels(as.factor(Data.daily.roomWise[,"month.day"]))],
			   between = list(y=0.5),
			   scales=list(x = list(at     = seq(from=0,to=23,by=1),
						labels = seq(from=0,to=23,by=1),
						limits = c(-1,25)),
				       y= y.limit),
			   layout = c(1,2)
			   )	
	plot(plot.obj) 


	cat(paste("\nDaily hourly averaged profile is plotted!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nDaily hourly averaged profile is plotted!\n"))



dev.off()
cat(paste("\nEnd of plot WITHIN each guest room!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nEnd of plot WITHIN each guest room!\n"))



# -------------------------------------------------------------------------------------------------
# plot the pdf ACROSS all guest rooms
# -------------------------------------------------------------------------------------------------
cat(paste("\nStart plotting ACROSS multiple guest room!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nStart plotting ACROSS multiple guest room!\n"))
pdf(file = FL.PDF.PR_roomAcross,paper="a4r", width=0, height=0)
	# get the y limit for plotting
	y.limit <- c(65,85)


	plot(Data.dayType.roomAll[as.character(Data.dayType.roomAll[,"day.type"])=="wkend","hour"],
	     Data.dayType.roomAll[as.character(Data.dayType.roomAll[,"day.type"])=="wkend","Value"],
	     type="b",lty=1,pch=16,lwd=2,col="red",cex=1.5,
	     xlab="Hour",ylab = expression("Return Air T F"^{o}),
	     main="Averaged On Multiple Days Across All Guest Rooms",
	     ylim = y.limit,
	     lab= c(23,23,7))
	lines(Data.dayType.roomAll[as.character(Data.dayType.roomAll[,"day.type"])=="wkday","hour"],
	      Data.dayType.roomAll[as.character(Data.dayType.roomAll[,"day.type"])=="wkday","Value"],
	      type="b",lty=1,pch=17,lwd=2,col="blue",cex=1.5)

	lines(Data.allDays.roomAll[,"hour"],
	      Data.allDays.roomAll[,"Value"],
	      type="l",lty=1,pch=1,lwd=2,col="black",cex=1.5)

	legend(1, max(y.limit),legend=c("WeekEnd","WeekDay","Overall"),
				  col=c("red","blue","black"),
				  lty=c(1,1,1),
				  text.col = c("red","blue","black")) 	 
dev.off()
cat(paste("\nEnd of plotting ACRlOSS multiple guest room!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nEnd of plotting ACROSS multiple guest room!\n"))


# -------------------------------------------------------------------------------------------------
# BOXPLOT based on raw data
# -------------------------------------------------------------------------------------------------
#
# plot day variation boxplot for each individual room separately
#
pdf(file = FL.PDF.BoxPlot_rawData,paper="a4r", width=0, height=0)

	# -------------------------------------------------------------------------------------------------
	# Variation Across Days for Each Individual Room [myData] 
	# -------------------------------------------------------------------------------------------------
	cat(paste("\nStart of boxplot WITHIN each guest room!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nStart of boxplot WITHIN each guest room!\n"))
	# get the y limit for plotting
	y.limit <- range(myData[,"Value"])
	# round to tenth place		
	y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)
	y.limit <- c(50,100)	# overwrite with this fixed limits for return air temperature

	for (lab.room in as.character(unique(myData[,"room.name"])))
	{	
		# weekend
		idx.row <- as.character(myData[,"room.name"]) == lab.room & as.character(myData[,"day.type"]) == "wkend"
		boxplot(myData[idx.row,"Value"] ~ factor(myData[idx.row,"hour"]),
			boxwex = 0.15, notch=FALSE, at = c(0:23) - 0.20,cex=1.0,
			xaxt="n",	# 
			main=paste("Variation Across Days for room ",lab.room,"(raw data)",sep=""),
			xlab="Hour",
			ylab = expression("Return Air T F"^{o}), 
			ylim=y.limit,
			outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="green")

		# weekeday
		idx.row <- as.character(myData[,"room.name"]) == lab.room & as.character(myData[,"day.type"]) == "wkday"
		boxplot(myData[idx.row,"Value"] ~ factor(myData[idx.row,"hour"]), add = TRUE, 
			boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.20,cex=1.0,
			col="red",outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="green")         


		# all data
		idx.row <- as.character(myData[,"room.name"]) == lab.room
		boxplot(myData[idx.row,"Value"] ~ factor(myData[idx.row,"hour"]), add = TRUE, 
			boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.00,cex=1.0,
			xaxt="n",
			col="black",outcex=0.7,outcol="black",staplecol="black",whiskcol="black",boxcol="black",boxfill="white",medlwd=2.0,medcol="green")         
	        abline(h=c(50,55,60,65,70,75,80,85,90,95),lty=2)
		# if (flag.hor){abline(h=value.hor,lty=2)}

		# legends (from package gplots)
		smartlegend(x="right",y="top", inset = 0.05,
			    c("wkend","all","wkday"),
			    col=c("blue","black","magenta"),
			    fill = c("blue","black","magenta"))
	}


	# -------------------------------------------------------------------------------------------------
	# Variation Across Rooms [myData] 
	# -------------------------------------------------------------------------------------------------
	cat(paste("\nStart of boxplot ACROSS each guest room!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nStart of boxplot ACROSS each guest room!\n"))
	# get the y limit for plotting
	y.limit <- range(myData[,"Value"])
	# round to tenth place		
	y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)
	y.limit <- c(50,100)	# overwrite with this fixed limits for return air temperature

	# weekend
	idx.row <- as.character(myData[,"day.type"]) == "wkend"
	boxplot(myData[idx.row,"Value"] ~ factor(myData[idx.row,"hour"]),
		boxwex = 0.15, notch=FALSE, at = c(0:23) - 0.20,cex=1.0,
		xaxt="n",	# 
		main=paste("Variation Across Rooms ","(raw data)",sep=""),
		xlab="Hour",
		ylab = expression("Return Air T F"^{o}), 
		ylim=y.limit,
		outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="green")

	# weekeday
	idx.row <- as.character(myData[,"day.type"]) == "wkday"
	boxplot(myData[idx.row,"Value"] ~ factor(myData[idx.row,"hour"]), add = TRUE, 
		boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.20,cex=1.0,
		col="red",outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="green")         


	# all data
	boxplot(myData[,"Value"] ~ factor(myData[,"hour"]), add = TRUE, 
		boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.00,cex=1.0,
		xaxt="n",
		col="black",outcex=0.7,outcol="black",staplecol="black",whiskcol="black",boxcol="black",boxfill="white",medlwd=2.0,medcol="green")         
	abline(h=c(50,55,60,65,70,75,80,85,90,95),lty=2)
	# if (flag.hor){abline(h=value.hor,lty=2)}

	# legends (from package gplots)
	smartlegend(x="right",y="top", inset = 0.05,
		    c("wkend","all","wkday"),
		    col=c("blue","black","magenta"),
		    fill = c("blue","black","magenta"))

dev.off()
 


# -------------------------------------------------------------------------------------------------
# BOXPLOT based on hourly aggregated data
# -------------------------------------------------------------------------------------------------
#
# plot day variation boxplot for each individual room separately
#
pdf(file = FL.PDF.BoxPlot_aggData,paper="a4r", width=0, height=0)

	# -------------------------------------------------------------------------------------------------
	# Variation Across Days for Each Individual Room
	# -------------------------------------------------------------------------------------------------
	cat(paste("\nStart of boxplot WITHIN each guest room!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nStart of boxplot WITHIN each guest room!\n"))
	# get the y limit for plotting
	y.limit <- range(Data.daily.roomWise[,"Value"])
	# round to tenth place		
	y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)
	y.limit <- c(50,100)	# overwrite with this fixed limits for return air temperature


	for (lab.room in as.character(unique(Data.daily.roomWise[,"room.name"])))
	{	
		# weekend
		idx.row <- as.character(Data.daily.roomWise[,"room.name"]) == lab.room & as.character(Data.daily.roomWise[,"day.type"]) == "wkend"
		boxplot(Data.daily.roomWise[idx.row,"Value"] ~ factor(Data.daily.roomWise[idx.row,"hour"]),
			boxwex = 0.15, notch=FALSE, at = c(0:23) - 0.20,cex=1.0,
			xaxt="n",	# 
			main=paste("Variation Across Days for room ",lab.room,"(hourly aggregated data for each day)",sep=""),
			xlab="Hour",
			ylab = expression("Return Air T F"^{o}), 
			ylim=y.limit,
			outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="green")

		# weekeday
		idx.row <- as.character(Data.daily.roomWise[,"room.name"]) == lab.room & as.character(Data.daily.roomWise[,"day.type"]) == "wkday"
		boxplot(Data.daily.roomWise[idx.row,"Value"] ~ factor(Data.daily.roomWise[idx.row,"hour"]), add = TRUE, 
			boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.20,cex=1.0,
			col="red",outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="green")         


		# all data
		idx.row <- as.character(Data.daily.roomWise[,"room.name"]) == lab.room
		boxplot(Data.daily.roomWise[idx.row,"Value"] ~ factor(Data.daily.roomWise[idx.row,"hour"]), add = TRUE, 
			boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.00,cex=1.0,
			xaxt="n",
			col="black",outcex=0.7,outcol="black",staplecol="black",whiskcol="black",boxcol="black",boxfill="white",medlwd=2.0,medcol="green")         
		abline(h=c(50,55,60,65,70,75,80,85,90,95),lty=2)

		# if (flag.hor){abline(h=value.hor,lty=2)}

		# legends (from package gplots)
		smartlegend(x="right",y="top", inset = 0.05,
			    c("wkend","all","wkday"),
			    col=c("blue","black","magenta"),
			    fill = c("blue","black","magenta"))
	}


	# -------------------------------------------------------------------------------------------------
	# Variation Across Rooms
	# -------------------------------------------------------------------------------------------------
	cat(paste("\nStart of boxplot ACROSS each guest room!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nStart of boxplot ACROSS each guest room!\n"))
	# get the y limit for plotting
	y.limit <- range(Data.daily.roomWise[,"Value"])
	# round to tenth place		
	y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)
	y.limit <- c(50,100)	# overwrite with this fixed limits for return air temperature

	# weekend
	idx.row <- as.character(Data.daily.roomWise[,"day.type"]) == "wkend"
	boxplot(Data.daily.roomWise[idx.row,"Value"] ~ factor(Data.daily.roomWise[idx.row,"hour"]),
		boxwex = 0.15, notch=FALSE, at = c(0:23) - 0.20,cex=1.0,
		xaxt="n",	# 
		main=paste("Variation Across Rooms ","(hourly aggregated data for each day)",sep=""),
		xlab="Hour",
		ylab = expression("Return Air T F"^{o}), 
		ylim=y.limit,
		outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="green")

	# weekeday
	idx.row <- as.character(Data.daily.roomWise[,"day.type"]) == "wkday"
	boxplot(Data.daily.roomWise[idx.row,"Value"] ~ factor(Data.daily.roomWise[idx.row,"hour"]), add = TRUE, 
		boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.20,cex=1.0,
		col="red",outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="green")         


	# all data
	boxplot(Data.daily.roomWise[,"Value"] ~ factor(Data.daily.roomWise[,"hour"]), add = TRUE, 
		boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.00,cex=1.0,
		xaxt="n",
		col="black",outcex=0.7,outcol="black",staplecol="black",whiskcol="black",boxcol="black",boxfill="white",medlwd=2.0,medcol="green")         
	abline(h=c(50,55,60,65,70,75,80,85,90,95),lty=2)
	# if (flag.hor){abline(h=value.hor,lty=2)}

	# legends (from package gplots)
	smartlegend(x="right",y="top", inset = 0.05,
		    c("wkend","all","wkday"),
		    col=c("blue","black","magenta"),
		    fill = c("blue","black","magenta"))



	# -------------------------------------------------------------------------------------------------
	# Variation Across Rooms
	# -------------------------------------------------------------------------------------------------
	cat(paste("\nStart of boxplot ACROSS each guest room!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nStart of boxplot ACROSS each guest room!\n"))
	# get the y limit for plotting
	y.limit <- range(Data.dayType.roomWise[,"Value"])
	# round to tenth place		
	y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)
	y.limit <- c(50,100)	# overwrite with this fixed limits for return air temperature

	# weekend
	idx.row <- as.character(Data.dayType.roomWise[,"day.type"]) == "wkend"
	boxplot(Data.dayType.roomWise[idx.row,"Value"] ~ factor(Data.dayType.roomWise[idx.row,"hour"]),
		boxwex = 0.15, notch=FALSE, at = c(0:23) - 0.20,cex=1.0,
		xaxt="n",	# 
		main=paste("Variation Across Rooms ","(hourly aggregated data for all weekdays and weekends)",sep=""),
		xlab="Hour",
		ylab = expression("Return Air T F"^{o}), 
		ylim=y.limit,
		outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="green")

	# weekeday
	idx.row <- as.character(Data.dayType.roomWise[,"day.type"]) == "wkday"
	boxplot(Data.dayType.roomWise[idx.row,"Value"] ~ factor(Data.dayType.roomWise[idx.row,"hour"]), add = TRUE, 
		boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.20,cex=1.0,
		col="red",outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="green")         


	# all data
	boxplot(Data.dayType.roomWise[,"Value"] ~ factor(Data.dayType.roomWise[,"hour"]), add = TRUE, 
		boxwex = 0.15, notch=FALSE, at = c(0:23) + 0.00,cex=1.0,
		xaxt="n",
		col="black",outcex=0.7,outcol="black",staplecol="black",whiskcol="black",boxcol="black",boxfill="white",medlwd=2.0,medcol="green")         
	abline(h=c(50,55,60,65,70,75,80,85,90,95),lty=2)
	# if (flag.hor){abline(h=value.hor,lty=2)}

	# legends (from package gplots)
	smartlegend(x="right",y="top", inset = 0.05,
		    c("wkend","all","wkday"),
		    col=c("blue","black","magenta"),
		    fill = c("blue","black","magenta"))
dev.off()


	    
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n1b_CrownePlaza_ReturnAirT_BaseCase.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n1b_CrownePlaza_ReturnAirT_BaseCase.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [1b_CrownePlaza_ReturnAirT_BaseCase.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [1b_CrownePlaza_ReturnAirT_BaseCase.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

