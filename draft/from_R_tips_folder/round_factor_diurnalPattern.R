#
# 36_RH_T_DP.R (CST time which is CDT + 1)  (or CDT = CST - 1)
#
# Received T, RH, Solar Radiation & Dew Point  data on June 5, 2009 from Shelly
# This script is created on June 15, 2009 to plot the time series of these environmental Variables
#
#
# INPUT:
# "../../FY2008_TCEQ/DataReceived/DataReceived_June5_2009"
# "ylx_houston_aldine_sept_06_1.csv"		NOTE:  There are empty lines when saved the excel file to csv file, and the empty lines are deleted manually
# "ylx_houston_bayland_sept_06.csv"		NOTE:  There are empty lines when saved the excel file to csv file, and the empty lines are deleted manually
# "ylx_houston_deerpark_sept_06.csv"		NOTE:  There are empty lines when saved the excel file to csv file, and the empty lines are deleted manually
#
# OUTPUT:
# "../36_RH_T_DP"
#
# Time System: Assume it is CST.  Confirmed by Shellly on June 15, 2009
# 
# CAMS station ID:
# 8:  Aldine
# 53: Bayland
# 18: Deer Park
#
# Date/Time system for gaseous data is:LST which is the same CST (Shelly Thomas Sep 23, 2008)
# UTC = CST + 6
# UTC = CDT + 5 in Summer
# UTC = CDT + 6 in winter
# So our data (in Summer) CST = CDT-1 or CDT = CST + 1)
#
# A new folder "\Analysis_withNegativeRemoved\" is created on this date to accomodate the analysis when negative values are deleted!!
# -------------------------------------------------------------------------
#
# 
# ------------------------------------------------------------------------------------------------- 
# 0. eliminate all stuff
# ------------------------------------------------------------------------------------------------- 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# 0. correlation coeeficient cutoff
corr.cut <- 0.7

# ------------------------------------------------------------------------------------------------- 
# 1. load libraries
# ------------------------------------------------------------------------------------------------- 
library(lattice)
library(chron)	
library(graphics)

# date/time information
time.info <- "LST which is CST: according to Shelly Sep 23, 2008 email.  UTC=CST+6, UTC=CDT+5 in Summer, UTC=CDT+6 in Winter"


# ------------------------------------------------------------------------------------------------- 
# 2. setup plotting limits for x axis
# ------------------------------------------------------------------------------------------------- 
xlim4plot <- c(chron(dates="9/1/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="10/1/2006",times="23:55:0",format=c('m/d/y','h:m:s')))
xat4plot  <- c(chron(dates="9/5/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/10/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/15/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/20/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/25/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),	       
               chron(dates="9/30/2006",times="0:0:0",  format=c('m/d/y','h:m:s')))
               
day4plot  <- c(chron(dates="9/1/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/2/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/3/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/4/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/5/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/6/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/7/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/8/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/9/2006", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/10/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/11/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/12/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/13/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/14/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/15/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/16/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/17/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/18/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/19/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/20/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/21/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/22/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/23/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/24/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/25/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/26/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/27/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/28/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/29/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/30/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="10/1/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="10/2/2006",times="0:0:0",  format=c('m/d/y','h:m:s')))
               
               
# date/time information
time.info <- "LST which is CST: according to Shelly Sep 23, 2008 email.  UTC=CST+6, UTC=CDT+5 in Summer, UTC=CDT+6 in Winter"

month.label <- c( 1,    2,    3,    4,    5,    6,    7,    8,    9,    10,   11,   12)
month.names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month.fullNames <- c("January","February","March","April","May","June","July","August","September","October","November","December")
names(month.fullNames) <- month.names

day.label <- c( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)	# no 31 in September of 2006
day.names <- c( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)	# no 31 in September of 2006





# ------------------------------------------------------------------------------------------------- 
# 3. define arrays
# ------------------------------------------------------------------------------------------------- 
xlim4plot <- c(chron(dates="9/1/2006", times="0:0:0",format=c('m/d/y','h:m:s')),chron(dates="10/1/2006",times="23:55:0",format=c('m/d/y','h:m:s')))
sites     <- c("Aldine","Bayland","DeerPark")
gases     <- c("CO","NOx","NOy","O3")

# ------------------------------------------------------------------------------------------------- 
# 4. change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_withNegativeRemoved/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_withNegativeRemoved/0_scripts"
}
setwd(Path.Current)


# ---------------------------------------------------------------------------------------------------
# 5. load the summary function
# ---------------------------------------------------------------------------------------------------
source(paste(Path.Current,"my_functions.R",sep="/"))

# --------------------------------------------------------------------------------------------------- 
# 6. setup output and log directory
# --------------------------------------------------------------------------------------------------- 
Path.out   <- "../36_RH_T_DP"	# OUTPUT processed result directory
Path.log   <- "../0_log"	# OUTPUT log  directory

if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ------------------------------------------------------------------------------------------------- 
# 7. create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
FL.TIME    <- paste(Path.log,"time.log",sep="/")		# OUTPUT Time Log file for all scripts
FL.LOG     <- paste(Path.log,"36_RH_T_DP.log",sep="/")	# OUTPUT Log file

if (file.exists(FL.LOG)) {print(paste(FL.LOG, " exist. Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for processing gaseous data at Aldine (AD), Bayland (BL) and DeerPark (DP)!\n",sep=""),file=FL.LOG, append=TRUE)

# --------------------------------------------------------------------------------------------------- 
# 8.define OUTPUT files
# ---------------------------------------------------------------------------------------------------
# create a R object for output
FL.OBJ            <- paste(Path.out,paste("enviromentVar.Rdata",sep=""),sep="/")		# OUTPUT Rdata
FL.TimeSeries.PDF <- paste(Path.out,paste("enviromentVar_TimeSeries.pdf",sep=""),sep="/")	# OUTPUT Scatter Plot in PS file
FL.DiurnalPat.PDF <- paste(Path.out,paste("enviromentVar_DiurnalPattern.pdf",sep=""),sep="/")	# OUTPUT Scatter Plot in PS file

if (file.exists(FL.OBJ)){print(paste(FL.OBJ," exist. Delete it!"));file.remove(FL.OBJ)}			# remove existing OUTPUT files
if (file.exists(FL.TimeSeries.PDF)){print(paste(FL.TimeSeries.PDF," exist. Delete it!"));file.remove(FL.TimeSeries.PDF)}			
if (file.exists(FL.DiurnalPat.PDF)){print(paste(FL.DiurnalPat.PDF," exist. Delete it!"));file.remove(FL.DiurnalPat.PDF)}			
cat(paste("\nDefined an output R object file!\n",sep=""),file=FL.LOG, append=TRUE)


# ---------------------------------------------------------------------------------------------------
# 9. Path and file of the environmental data (i.e., T.db, dew point, RH etc
# ---------------------------------------------------------------------------------------------------
Path.Environ  <- "../../DataReceived/DataReceived_June5_2009"
cat("load data\n")
for (site in sites)
{
	if (site == "Aldine")
	{
		FL.Environ <- paste(Path.Environ,"ylx_houston_aldine_sept_06_1.csv",sep="/")
	}
	if (site == "Bayland")
	{
		FL.Environ <- paste(Path.Environ,"ylx_houston_bayland_sept_06.csv",sep="/")
	}	
	if (site == "DeerPark")
	{
		FL.Environ <- paste(Path.Environ,"ylx_houston_deerpark_sept_06.csv",sep="/")
	}
	
	data.Environ <- read.table(FL.Environ, sep=",",header=TRUE,stringsAsFactors=FALSE,blank.lines.skip = TRUE)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files
	
	# change the field names
	names(data.Environ) <- c("CAMS","dates","value","err.flag","var.name")
	
	# change the variable names in the [var.Name] field to abbreviations
	var.longName <- c("Dew Point Temperature","relative humidity","solar radiation","Temperature")	# used as names
	var.abbrev   <- c("dp.F"                 ,"RH"               ,"solar"          ,"T.F")		# used as labels
	
	# convert the [var.name] field into a factor type using short name as the labels
	data.Environ[,"var.name"] <- factor(data.Environ[,"var.name"],levels=var.longName,labels=var.abbrev)	

	
	# split the date-time filed into details 
	dates.times <- unlist(strsplit(as.character(data.Environ[,"dates"])," ")) 	# time is separated by " "
	dates       <- dates.times[seq(from=1,to=length(dates.times),by=2)]		# get dates
	times       <- dates.times[seq(from=2,to=length(dates.times),by=2)]		# get times
	date.MDY    <- unlist(strsplit(as.character(dates),"/")) 			# Month, Day and Year are separated by "/"
	date.HMS    <- unlist(strsplit(as.character(times),":")) 			# Hour, Minute, Second are separated by ":"
	date.month  <- date.MDY[seq(from=1,to=length(date.MDY),by=3)]			# get Month
	date.day    <- date.MDY[seq(from=2,to=length(date.MDY),by=3)]			# get Day
	date.year   <- date.MDY[seq(from=3,to=length(date.MDY),by=3)]			# get Year
	date.hour   <- date.HMS[seq(from=1,to=length(date.HMS),by=2)]			# get Hour
	date.minute <- date.HMS[seq(from=2,to=length(date.HMS),by=2)]			# get Minute

	
	# create a chron data field
	chron.Environ <- chron(dates=dates, times=paste(times,rep("0",length(times)),sep=":"),format=c('m/d/y','h:m:s'))
	
	# append the chron date and detail dates/times fields into the data frame
	data.Environ <- cbind(chron.Environ  = chron.Environ,
	                      month.Environ  = date.month,
	                      day.Environ    = date.day,
	                      hour.Environ   = date.hour,
	                      minute.Environ = date.minute,
	                      data.Environ)
	                      

	# convert day into ordered factor for plotting in sequency 
	data.Environ[,"day.Environ"]   <- factor(data.Environ[,"day.Environ"],levels=day.label,labels=day.names)
	
	# make sure ["day.Environ"] is a numeric field
	data.Environ[,"hour.Environ"]  <- as.numeric(as.character(data.Environ[,"hour.Environ"]))
	                      

	# give distinct obejct name for the environmental data at each site
	if (site == "Aldine")
	{
		data.Environ.Aldine <- data.Environ
	}
	if (site == "Bayland")
	{
		data.Environ.Bayland <- data.Environ
	}	
	if (site == "DeerPark")
	{
		data.Environ.DeerPark <- data.Environ
	}	
}

# now the environmental data are ready for all 3 sites and they are:
# [data.Environ.Aldine], [data.Environ.Bayland], [data.Environ.DeerPark]

# ---------------------------------------------------------------------------------------------------
# 10. prepare a median object for the median of all 4 environmental variables at all 3 sites
# ---------------------------------------------------------------------------------------------------
Environ.Median <-  rbind(tapply(data.Environ.Aldine[,"value"],data.Environ.Aldine[,"var.name"],median,na.rm=TRUE),
			 tapply(data.Environ.Bayland[,"value"],data.Environ.Bayland[,"var.name"],median,na.rm=TRUE),
			 tapply(data.Environ.DeerPark[,"value"],data.Environ.DeerPark[,"var.name"],median,na.rm=TRUE))
row.names(Environ.Median) <- c("Aldine","Bayland","DeerPark")


# ---------------------------------------------------------------------------------------------------
# 11. plot time series
# ---------------------------------------------------------------------------------------------------
cat("plotting time series\n")
cat("plotting time series\n",file=FL.LOG,append=TRUE)

# loopping through each of the 4 environmental variables
pdf(file = FL.TimeSeries.PDF,paper="a4r", width=0, height=0)
for (var.idx in var.abbrev)
{
	var.string  <- var.longName[var.abbrev %in% var.idx]	# get the long names in [var.longName] corresponds to the shortname in [var.abbrev]
	cat(paste("plotting time series of ",var.string,"\n",sep=""))
	cat(paste("plotting time series of ",var.string,"\n",sep=""),file=FL.LOG,append=TRUE)
	
	# we need the min/max of the variable in all three sites when plotting 3 sites together
	ymin.all3sites <-  1e21
	ymax.all3sites <- -1e21
	
	# loopping through the 3 sites
	for (site in sites)
	{
		cat(paste(site,"\n",sep=""))
		cat(paste(site,"\n",sep=""),file=FL.LOG,append=TRUE)
		if (site == "Aldine")
		{
			data.Environ <- data.Environ.Aldine
		}
		if (site == "Bayland")
		{
			data.Environ <- data.Environ.Bayland
		}	
		if (site == "DeerPark")
		{
			data.Environ <- data.Environ.DeerPark
		}
	
		# plot the time series current variable in each sit
		data.subset <- subset(data.Environ,var.name == var.idx)
		if (dim(data.subset)[1] !=0)
		{
			min.Environ <- min(data.subset[,"value"],na.rm=TRUE)
			max.Environ <- max(data.subset[,"value"],na.rm=TRUE)
			ymin <- trunc(floor(min.Environ/10))*10		# ***************** round to the 10th place of the maximum solar radiation *****************
			ymax <- trunc(ceiling(max.Environ/10))*10	# ***************** round to the 10th place of the maximum solar radiation *****************

			# update the min/max of all 3 sites of current variable according to the min/max of the variable at current site
			ymin.all3sites <-  min(1e21,ymin)
			ymax.all3sites <- max(-1e21,ymax)

			# plot the time series of current variable
			plot(data.subset[,"chron.Environ"],data.subset[,"value"],type="l",lty=1,col="red",
			     xlab = "CST date/time",
			     ylab = var.string,				# use the long name corresponds to the abbrevation
			     main = paste(var.string," at ",site,sep=""),
			     ylim=c(ymin,ymax))
			     abline(v=day4plot,lty=2,lwd=0.2,col="grey")
			     abline(h=Environ.Median[site,var.idx],lty=1,lwd=.2,col=c("cyan"))	# plot median as horizontal lines

			# legend(xat4plot[1],ymax,legend=c("raw","median"),col=c("red","cyan"),lty=c(1,1),text.col = c("red","blue")) 
		}
	}

	
	# plot the environmental variables of all three sites together
	cat("plot time series at all 3 sites together\n")
	cat("plot time series at all 3 sites together\n",file=FL.LOG,append=TRUE)

	# the row indcies of current variable which is used to constraint the data to be plotted
	idx.subset.Aldine   <- data.Environ.Aldine[,"var.name"]   == var.idx		# [TRUE] / [FALSE] serve as index of the subset data
	idx.subset.Bayland  <- data.Environ.Bayland[,"var.name"]  == var.idx		# [TRUE] / [FALSE] serve as index of the subset data
	idx.subset.DeerPark <- data.Environ.DeerPark[,"var.name"] == var.idx		# [TRUE] / [FALSE] serve as index of the subset data
	
	# since we have missing variable in Bayland, the following conditional loops are used to make the plotting right
	if (sum(idx.subset.Aldine) != 0)
	{
		plot(data.Environ.Aldine[idx.subset.Aldine,"chron.Environ"],data.Environ.Aldine[idx.subset.Aldine,"value"],type="l",lty=1,col="red",
			  xlab = "CST date/time",ylab=var.string,main=paste(var.string,sep=""),
			  ylim=c(ymin.all3sites,ymax.all3sites))
	}
	
	# if current variable is missing at Aldine but existing at Bayland, need to {plot} Byaland data
	if (sum(idx.subset.Bayland) != 0)
	{
		if (sum(idx.subset.Aldine) != 0)
		{
			lines(data.Environ.Bayland[idx.subset.Bayland, "chron.Environ"],data.Environ.Bayland[idx.subset.Bayland,"value"], type="l",lty=1,col="blue")
		}else{
			plot(data.Environ.Bayland[idx.subset.Bayland, "chron.Environ"],data.Environ.Bayland[idx.subset.Bayland,"value"], type="l",lty=1,col="blue",
			     xlab = "CST date/time",ylab=var.string,main=paste(var.string,sep=""),
			     ylim=c(ymin.all3sites,ymax.all3sites))			
		}
	}

	# if current variable is missing at both Aldine and Bayland but existing at DeerPark, need to {plot} DeerPark data	
	if (sum(idx.subset.DeerPark) != 0)
	{
		if ((sum(idx.subset.Aldine) != 0) | (sum(idx.subset.Bayland) != 0))
		{
			lines(data.Environ.DeerPark[idx.subset.DeerPark,"chron.Environ"],data.Environ.DeerPark[idx.subset.DeerPark,"value"],type="l",lty=1,col="green")
		}else{
			plot(data.Environ.DeerPark[idx.subset.DeerPark,"chron.Environ"],data.Environ.DeerPark[idx.subset.DeerPark,"value"],type="l",lty=1,col="green",
			     xlab = "CST date/time",ylab=var.string,main=paste(var.string,sep=""),
			     ylim=c(ymin.all3sites,ymax.all3sites))			
		}
	}
	abline(v=day4plot,lty=2,lwd=0.2,col="grey")
	abline(h=Environ.Median[,var.idx],lty=c(1,1,1),lwd=c(0.2,0.2,0.2),col=c("red","blue","green"))

	legend(xat4plot[4], ymax,legend=c("Aldine","Bayland","DeerPark"),col=c("red","blue","green"),lty=c(1,1,1),text.col = c("red","blue","green")) 	
}
dev.off()


# ---------------------------------------------------------------------------------------------------
# 12. plot diurnal pattern of the environmental variables
# ---------------------------------------------------------------------------------------------------
cat("plotting time series\n")
cat("plotting time series\n",file=FL.LOG,append=TRUE)

lty.array  <- c(rep(1,10),rep(3,10),rep(5,10))		# solid symbols
pch.array  <- c(rep(16,30))
cex.array  <- c(rep(2,30))
lwd.array  <- c(rep(2,10),rep(2,10),rep(2,10))
col.array  <- c("red","blue","green","magenta","cyan","purple","azure","black","brown","aliceblue","beige","bisque","chocolate","darkgrey",
                 "burlywood","aquamarine","deepskyblue","gold","ivory","maroon","orange","orchid","pink","tomato","rosybrown","violet","plum","yellow","salmon","springgreen")
col.array  <- rep(c("red","blue","green","magenta","cyan","black","pink","violet","grey","brown"),3)



# loopping through each of the 4 environmental variables
pdf(file = FL.DiurnalPat.PDF,paper="a4r", width=0, height=0)
for (var.idx in var.abbrev)
{
	var.string  <- var.longName[var.abbrev %in% var.idx]	# get the long names in [var.longName] corresponds to the shortname in [var.abbrev]
	cat(paste("plotting time series of ",var.string,"\n",sep=""))
	cat(paste("plotting time series of ",var.string,"\n",sep=""),file=FL.LOG,append=TRUE)
	
	# we need the min/max of the variable in all three sites when plotting 3 sites together
	ymin.all3sites <-  1e21
	ymax.all3sites <- -1e21
	
	# loopping through the 3 sites
	for (site in sites)
	{
		cat(paste(site,"\n",sep=""))
		cat(paste(site,"\n",sep=""),file=FL.LOG,append=TRUE)
		if (site == "Aldine")
		{
			data.Environ <- data.Environ.Aldine
		}
		if (site == "Bayland")
		{
			data.Environ <- data.Environ.Bayland
		}	
		if (site == "DeerPark")
		{
			data.Environ <- data.Environ.DeerPark
		}
	
		# plot the time series current variable in each sit
		data.subset <- subset(data.Environ,var.name == var.idx)
		if (dim(data.subset)[1] !=0)
		{
			min.Environ <- min(data.subset[,"value"],na.rm=TRUE)
			max.Environ <- max(data.subset[,"value"],na.rm=TRUE)*1.1
			ymin <- trunc(floor(min.Environ/10))*10		# ***************** round to the 10th place of the maximum solar radiation *****************
			ymax <- trunc(ceiling(max.Environ/10))*10	# ***************** round to the 10th place of the maximum solar radiation *****************

			# plot the raw and one-hour running average of the O3 at the three sites
			key.string <- list(title="day",space="right",columns=1,text=list(levels(as.factor(data.subset[,"day.Environ"]))),lines = list(lty=lty.array),col=col.array)

			plot.obj <- xyplot(data.subset[,"value"] ~ data.subset[,"hour.Environ"],data=data.subset,
				       groups = day.Environ,
				       xlab = "CST (Hour)",
				       ylab = var.string,
				       main = paste(var.string," at ",site,sep=""),
				       type = rep("l",30),
				       lty  = lty.array,
				       lwd  = lwd.array,
				       pch  = pch.array,
				       col  = col.array,
				       cex  = cex.array,
				       ylim = c(ymin,ymax),
				       xlim = c(0,24),
				       xat  = c(0,2,4,6,8,10,12,14,16,18,20,24),
				       key  = key.string,

				   panel = function(x,y,...) {
					   panel.xyplot(x, y,...)
					   panel.abline(h=Environ.Median[site,var.idx],lty=c(2),col="black")
					   panel.abline(v=seq(0,24),lty=2,lwd=0.2,col="black")}	


				       )
			plot(plot.obj)	
		}
	}	
}
dev.off()




# ---------------------------------------------------------------------------------------------------
# 30. write all data objects into a R data object file
# ---------------------------------------------------------------------------------------------------	
save(data.Environ.Aldine,data.Environ.Bayland,data.Environ.DeerPark,file = FL.OBJ)
cat(paste("all important data objects are saved in ",FL.OBJ,"\n",sep=""),file=FL.LOG,append=TRUE)


# ---------------------------------------------------------------------------------------------------
# 40. time used for completing this script
# ---------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("36_RH_T_DP.R is finished successfully!\n",sep=" "))
cat(paste("36_RH_T_DP.R is finished successfully!\n",sep=" "),file=FL.LOG,append=TRUE)


cat(paste("Processing time for 36_RH_T_DP.R is",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)

