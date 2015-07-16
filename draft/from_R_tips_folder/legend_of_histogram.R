#
# 31_O3Analysis.R (CST time which is CDT + 1)  (or CDT = CST - 1)
#
# Note: the summary calculated in "9a_gasDataProcess.R" includes the data from October.  So
# new summary was calculated here by "31_O3Analysis" after excluding the data from October
# June 15, 2009
#
# See "plotmath" for subscript and superscripts
#
# Create this new series of folder for analysis focused on task 3
#
# Modified on "28_OzoneAnalysis_O3Incre.R"
# The scope of task 3 changed (see Carl's TERC Project H-90 May 20,2009 SOW)
# (1) The analysis will be on the running hourly average instead of raw 5 minute gaseous data
# (2) previous tried 6 and 15 hours of trajectories, now changed to 6 and 12 hours.  The reason is that the recent trajectories got from Fantine for Sep 1 to 9, 2006 are only 12 hours (145 points)
#
# -------------------------------------------------------------------------------
#
# This analysis is based on the conc increment of ozone 
# There is a companion analysis based on ozone concentration itself which is "29_OzoneAnalysis_O3Conc.R"
#
# INPUT
# "../9a_gasDataProcess/DataGaseous.Rdata"	# gas data including ozone
# 		[data.gas.wide],[data.gas.long]
#		[corr.coef.pearson],[corr.coef.spearman],[corr.bySite],[corr.byGas],[time.info]
#
# OUTPUT
# "../31_O3Analysis"
# [data.1hr.runAvg],[data.1hr.runAvg_O3_longFM]
# -------------------------------------------------------------------------
#

# -------------------------------------------------------------------------------------------------
# 0. eliminate all stuff
# ------------------------------------------------------------------------------------------------- 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

O3.cutoff <- 0		# arbitrary to set a cutoff of 80 ppbv.  If want to use all data set this cutoff to 0.


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


hour4plot_sep01  <- c(chron(dates="9/1/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/1/2006",times="1:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/1/2006",times="2:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/1/2006",times="3:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/1/2006",times="4:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/1/2006",times="5:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/1/2006",times="6:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/1/2006",times="7:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/1/2006",times="8:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="9:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="10:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="11:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="12:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="13:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="14:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="15:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="16:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="17:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="18:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="19:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="20:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="21:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="22:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/1/2006",times="23:0:0", format=c('m/d/y','h:m:s')))


hour4plot_sep08  <- c(chron(dates="9/8/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/8/2006",times="1:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/8/2006",times="2:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/8/2006",times="3:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/8/2006",times="4:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/8/2006",times="5:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/8/2006",times="6:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/8/2006",times="7:0:0",  format=c('m/d/y','h:m:s')),
               chron(dates="9/8/2006",times="8:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="9:0:0",  format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="10:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="11:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="12:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="13:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="14:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="15:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="16:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="17:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="18:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="19:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="20:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="21:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="22:0:0", format=c('m/d/y','h:m:s')),
	       chron(dates="9/8/2006",times="23:0:0", format=c('m/d/y','h:m:s')))


               
               
# ------------------------------------------------------------------------------------------------- 
# specify names of sites and gaseous 
# -------------------------------------------------------------------------------------------------
sites     <- c("Aldine","Bayland","DeerPark")
gases     <- c("CO","NOx","NOy","O3")

# ------------------------------------------------------------------------------------------------- 
# 5. change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_withNegativeRemoved/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_withNegativeRemoved/0_scripts"
}
setwd(Path.Current)


# -------------------------------------------------------------------------------------------------
# 6. load summary function
# -------------------------------------------------------------------------------------------------
source(paste(Path.Current,"my_functions.R",sep="/"))

# ------------------------------------------------------------------------------------------------- 
# 7. setup output and log directory
# ------------------------------------------------------------------------------------------------- 
Path.IN.O3   <- "../9a_gasDataProcess"	# INPUT  O3 and other gaseous data
Path.OUT     <- "../31_O3Analysis"	# OUTPUT processed result directory
Path.log     <- "../0_log"		# OUTPUT log  directory

if (!file.exists(Path.IN.O3)){stop(paste("NOT existing:",Path.IN.O3," You must have an input folder of Ozone for this analysis!",sep=""))}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ------------------------------------------------------------------------------------------------- 
# 8. create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
FL.TIME	<- paste(Path.log,"time.log",sep="/")				# OUTPUT Time Log file for all scripts
FL.LOG	<- paste(Path.log,"31_O3Analysis.log",sep="/")			# OUTPUT Log file
FL.OBJ	<- paste(Path.OUT,"DataGaseous_oneHr_runAvg.Rdata",sep="/")	# OUTPUT one-hour running average data
FL.SUM  <- paste(Path.OUT,paste("Gaseous_Summary.csv",sep=""),sep="/")	# OUTPUT Summary 

if (file.exists(FL.LOG)) {print(paste(FL.LOG, " exist. Delete it!"));file.remove(FL.LOG)}
if (file.exists(FL.OBJ)) {print(paste(FL.OBJ, " exist. Delete it!"));file.remove(FL.OBJ)}
if (file.exists(FL.SUM)){print(paste(FL.SUM," exist. Delete it!"));file.remove(FL.SUM)}			# remove existing OUTPUT files

cat(paste("Log file for analyzing O3 increment with trajectories at Aldine (AD), Bayland (BL) and DeerPark (DP)!\n",sep=""),file=FL.LOG, append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 9. load INPUT files
# -------------------------------------------------------------------------------------------------
FL.O3 <- paste(Path.IN.O3,paste("DataGaseous.Rdata",sep=""),sep="/")	# INPUT Rdata
if (!file.exists(FL.O3)){stop(paste("NOT existing:",FL.O3," You must have an input folder of ozone data for this analysis!",sep=""))}
load(FL.O3)

#
# use the date/time stampe as the row names
#
row.names(data.gas.wide) <- data.gas.wide[,"Date.chron"]
names(data.gas.wide)[1] <- "CST.chron.O3"			# rename "Date.Chron" as "CST.chron.O3"

# -------------------------------------------------------------------------------------------------
# 10. get the names of gases and the names of other fields in the data object
# -------------------------------------------------------------------------------------------------
names.gas <- c(grep("CO\\.",names(data.gas.wide),value=TRUE),grep("O3\\.",names(data.gas.wide),value=TRUE),grep("NOx\\.",names(data.gas.wide),value=TRUE),grep("NOy\\.",names(data.gas.wide),value=TRUE))
names.other <- setdiff(names(data.gas.wide),names.gas)


#
# only keep data in September (the existence of October data add artifact int he diurnal pattern plots)
# NOTE: original summary calculated in 9a_gasDataProcess.R included thedata of October 1st!!!!!
data.gas.wide <- subset(data.gas.wide,month==9)		# !!!!!!!!! NOTE: October data excluded !!!!!!!!!!
gas.Sum <- data.frame(t(apply(data.gas.wide[,names.gas],2,sumfn)))
names(gas.Sum) <- c("no","no.NA","no.NEG","min","p10","p25","median","p75","p90","max","mean","stdev","sum","NA%","NA%.cls","NEG%","NEG%.cls")

O3.median <- c(median(data.gas.wide[,"O3.Aldine"],na.rm=TRUE),median(data.gas.wide[,"O3.Bayland"],na.rm=TRUE),median(data.gas.wide[,"O3.DeerPark"],na.rm=TRUE))
names(O3.median) <- c("Aldine","Bayland","DeerPark")

cat(paste("-------------------------- Summary  --------------------------\n",sep=""),	
	    file=FL.SUM,
	    append=TRUE)
	    
cat(paste("sum,",sep=""),file=FL.SUM,append=TRUE)
write.table(gas.Sum, 
	    file = FL.SUM,      
	    sep = ",", 
	    col.names = TRUE, 
	    row.names = TRUE,
	    append = TRUE)
cat(paste(paste("\nSummary of the chemicals are written out to ",FL.SUM,sep=""),"\n",sep=""),file=FL.LOG,append=TRUE)





#
# split the data object into two: one with only gases and another with other fields
#
T1 <- subset(data.gas.wide,select=names.gas)		# those gaseous species columns
T2 <- subset(data.gas.wide,select=names.other)		# those data/time columns

# initialize the gas portion with NaN priori to one hour average calculation
T1[!is.na(T1)]<-NaN					# re-assign to NaN because I want to use T2 an the initialization of the one-hour running average of all gaseous species

# initialize a one hour average data frame the same size as the loaded data
data.1hr.runAvg <- cbind(T2,T1)
rm(T1,T2)

# ---------------------------------------------------------------------------
# 11. replace gaseous data fields in [data.1hr.runAvg] with the calculated one-hour running means
# ---------------------------------------------------------------------------
for (idx in seq(1:dim(data.gas.wide)[1]))
{
	time.current  <- data.gas.wide[idx,"hour_0901"]		# current time
	oneHour.begin <- time.current - 0.5			# half hour before
	oneHour.end   <- time.current + 0.5			# half hour later
	
	data.tmp <- subset(data.gas.wide,hour_0901>=oneHour.begin & hour_0901 <= oneHour.end,select = names.gas)

	# average of the data in the one hour window (since it is running average, so the date/time stamp does not change)
	data.1hr.runAvg[idx,names.gas] <- apply(data.tmp,2,mean,na.rm=TRUE)
}


# -------------------------------------------------------------------------------------------------
# 12. supplement [data.1hr.runAvg] with the calculated "increment" of one-hour running mean of the gaseous data
# -------------------------------------------------------------------------------------------------
for (lab.gas in names.gas)
{
	delta.1hrAvg        <- as.numeric(c(data.1hr.runAvg[2:dim(data.1hr.runAvg)[1],lab.gas] - data.1hr.runAvg[1:dim(data.1hr.runAvg)[1]-1,lab.gas],NaN))	
	names(delta.1hrAvg) <- row.names(data.1hr.runAvg)
	
	# since the field name is assigned on the fly (inc.O3.Aldine, inc.O3.Bayland,inc.O3.DeerPark)), need to use eval
	command.string      <- paste("data.1hr.runAvg <- cbind(data.1hr.runAvg,",paste("inc.",lab.gas,sep=""),"=delta.1hrAvg)",sep="")
	eval(parse(text=command.string))
	
	# Now one hour running average data and their positive increments
	# which is the data object [data.1hr.runAvg]
}

# -------------------------------------------------------------------------------------------------
# 13.  make a long format of the one hour running average data and their positive increments
# -------------------------------------------------------------------------------------------------
T1 <- data.1hr.runAvg[,c(names.other,"O3.Aldine","inc.O3.Aldine")]	# one hour running average and increment of O3 at Aldine
T2 <- data.1hr.runAvg[,c(names.other,"O3.Bayland","inc.O3.Bayland")]	# one hour running average and increment of O3 at Bayland
T3 <- data.1hr.runAvg[,c(names.other,"O3.DeerPark","inc.O3.DeerPark")]	# one hour running average and increment of O3 at DeeraPark

T1 <- cbind(T1,site = rep("Aldine",  dim(T1)[1])); names(T1) <- c(names.other,"O3","inc.O3","site")	# add a site name field and change the filed name of all site to common filed names for assembling
T2 <- cbind(T2,site = rep("Bayland", dim(T2)[1])); names(T2) <- c(names.other,"O3","inc.O3","site")
T3 <- cbind(T3,site = rep("DeerPark",dim(T3)[1])); names(T3) <- c(names.other,"O3","inc.O3","site")

# assembling the three sites to a long format
data.1hr.runAvg_O3_longFM <- rbind(T1,T2,T3)
rm(T1,T2,T3)

# -------------------------------------------------------------------------------------------------
# 14. for increment, we only forcus on positive increment and reassign negative increment to NaN
# -------------------------------------------------------------------------------------------------
# wide format	  (i.e., inc.O3.Aldine, inc.O3.ayland, inc.O3.DeerPark)
for (lab.inc in grep("inc\\.",names(data.1hr.runAvg),value=TRUE))
{
	data.1hr.runAvg[!is.na(data.1hr.runAvg[,lab.inc]) & data.1hr.runAvg[,lab.inc] < 0,lab.inc] <- NaN
}

# long format	(i.e., inc.O3.Aldine, inc.O3.ayland, inc.O3.DeerPark)	       
data.1hr.runAvg_O3_longFM[!is.na(data.1hr.runAvg_O3_longFM[,"inc.O3"]) & (data.1hr.runAvg_O3_longFM[,"inc.O3"]<0),"inc.O3"] <- NaN


# -------------------------------------------------------------------------------------------------
# 15. save the one-hour running average gaseous data
# -------------------------------------------------------------------------------------------------
save(data.1hr.runAvg,data.1hr.runAvg_O3_longFM,file = FL.OBJ)

# -------------------------------------------------------------------------------------------------
# 16. Plotting time series
# -------------------------------------------------------------------------------------------------
FL.PDF  <- paste(Path.OUT,paste("Ozone_1hr_runningAvg_timeSeries.pdf",sep=""),sep="/")		# OUTPUT in PDF  files of the sinuosity/quadrant on the O3 increment bins
if (file.exists(FL.PDF)) {print(paste(FL.PDF, " exist. Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,paper="a4r", width=0, height=0)
# -------------------------------------------------------------------------------------------------
# A. plot the time series of one hour running average of O3
# -------------------------------------------------------------------------------------------------
for (site in sites)
{
	# -------------------------------------------------------------------------------------------
	# Ozone data of current site
	# -------------------------------------------------------------------------------------------
	lab.O3 <- paste("O3",site,sep=".")
	inc.O3 <- paste("inc",lab.O3,sep=".")	
	
	ymax <- max(data.1hr.runAvg[,lab.O3],na.rm=TRUE)*1.1
	ymax <- trunc(ceiling(ymax/10))*10

	
	#
	# plot the raw and one-hour running average of the O3 at the three sites
	#
	plot(data.gas.wide[,"CST.chron.O3"],data.gas.wide[,lab.O3],type="l",lty=1,col="red",
	     xlab = "CST date/time",ylab=expression("O"[{3}] (ppbv)),main=paste("Ozone at ",site,sep=""),
	     ylim=c(0,ymax))
	lines(data.1hr.runAvg[,"CST.chron.O3"],data.1hr.runAvg[,lab.O3],type="l",lty=1,col="blue")
	abline(v=day4plot,lty=2,lwd=0.2,col="grey")
	abline(h=120,lty=2,lwd=0.2,col="black")
	abline(h=O3.median[site],lty=1,lwd=.2,col=c("cyan"))

	legend(xat4plot[1],ymax,legend=c("raw","1hr running avg"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue")) 	
}

#
# plot all three sites together
#
ymax <- max(max(data.1hr.runAvg[,"O3.Bayland"],na.rm=TRUE),max(data.1hr.runAvg[,"O3.Bayland"],na.rm=TRUE),max(data.1hr.runAvg[,"O3.Bayland"],na.rm=TRUE))*1.1
ymax <- trunc(ceiling(ymax/10))*10	# ***************** round to the 10th place of the maximum O3 *****************

plot(data.1hr.runAvg[,"CST.chron.O3"],data.1hr.runAvg[,"O3.Aldine"],type="l",lty=1,col="red",
	  xlab = "CST date/time",ylab=expression("O"[{3}] (ppbv)),main="one hour running average of Ozone",
	  ylim=c(0,ymax))
lines(data.1hr.runAvg[,"CST.chron.O3"],data.1hr.runAvg[,"O3.Bayland"], type="l",lty=1,col="blue")
lines(data.1hr.runAvg[,"CST.chron.O3"],data.1hr.runAvg[,"O3.DeerPark"],type="l",lty=1,col="green")
abline(v=day4plot,lty=2,lwd=0.2,col="grey")
abline(h=120,lty=2,lwd=0.2,col="black")
abline(h=O3.median,lty=c(1,1,1),lwd=c(.2,0.2,0.2),col=c("red","blue","green"))

legend(xat4plot[4], ymax,legend=c("Aldine","Bayland","DeerPark"),col=c("red","blue","green"),lty=c(1,1,1),text.col = c("red","blue","green")) 	


# -------------------------------------------------------------------------------------------------
# B. plot the time series of the increment of the one hour running average of O3
# -------------------------------------------------------------------------------------------------
for (site in sites)
{
	# -------------------------------------------------------------------------------------------
	# Ozone data of current site
	# -------------------------------------------------------------------------------------------
	lab.O3 <- paste("O3",site,sep=".")
	inc.O3 <- paste("inc",lab.O3,sep=".")
	ymax <- max(data.1hr.runAvg[,inc.O3],na.rm=TRUE)*1.1
	ymax <- trunc(ceiling(ymax/10))*10
	
	#
	# plot the raw and one-hour running average of the O3 at the three sites
	#
	plot(data.1hr.runAvg[data.1hr.runAvg[,inc.O3]>0,"CST.chron.O3"],data.1hr.runAvg[data.1hr.runAvg[,inc.O3]>0,inc.O3],type="l",lty=1,col="red",
	     xlab = "CST date/time",ylab=expression(Delta(O[{3}])/Delta(t)),main=paste("Ozone increment at ",site,sep=""),
	     ylim=c(0,ymax))
	abline(v=day4plot,lty=2,lwd=0.2,col="grey")
	legend(xat4plot[1],ymax,legend=c("1hr running avg"),col=c("red"),lty=c(1),text.col = c("red")) 	

}

#
# plot all three sites together
#
ymax <- max(max(data.1hr.runAvg[,"inc.O3.Bayland"],na.rm=TRUE),max(data.1hr.runAvg[,"inc.O3.Bayland"],na.rm=TRUE),max(data.1hr.runAvg[,"inc.O3.Bayland"],na.rm=TRUE))*1.1
ymax <- trunc(ceiling(ymax/10))*10	# ***************** round to the 10th place of the maximum O3 *****************

plot(data.1hr.runAvg[,"CST.chron.O3"],data.1hr.runAvg[,"inc.O3.Aldine"],type="l",lty=1,col="red",
	  xlab = "CST date/time",ylab=expression(Delta(O[{3}])/Delta(t)),main="increment of one hour running average of Ozone",
	  ylim=c(0,ymax))
lines(data.1hr.runAvg[,"CST.chron.O3"],data.1hr.runAvg[,"inc.O3.Bayland"],  type="l",lty=1,col="blue")
lines(data.1hr.runAvg[,"CST.chron.O3"],data.1hr.runAvg[,"inc.O3.DeerPark"],type="l",lty=1,col="green")
abline(v=day4plot,lty=2,lwd=0.2,col="grey")
legend(xat4plot[4], ymax,legend=c("Aldine","Bayland","DeerPark"),col=c("red","blue","green"),lty=c(1,1,1),text.col = c("red","blue","green")) 	
dev.off()





# -------------------------------------------------------------------------------------------------
# 17. Plotting diurnal pattern in lines
# -------------------------------------------------------------------------------------------------
lty.array  <- c(rep(1,10),rep(3,10),rep(5,10))		# solid symbols
pch.array  <- c(rep(16,30))
cex.array  <- c(rep(2,30))
lwd.array  <- c(rep(2,10),rep(2,10),rep(2,10))
col.array  <- c("red","blue","green","magenta","cyan","purple","azure","black","brown","aliceblue","beige","bisque","chocolate","darkgrey",
                 "burlywood","aquamarine","deepskyblue","gold","ivory","maroon","orange","orchid","pink","tomato","rosybrown","violet","plum","yellow","salmon","springgreen")
col.array  <- rep(c("red","blue","green","magenta","cyan","black","pink","violet","grey","brown"),3)


FL.PDF  <- paste(Path.OUT,paste("Ozone_1hr_runningAvg_diurnal_lineplot.pdf",sep=""),sep="/")		# OUTPUT in PDF  files of the sinuosity/quadrant on the O3 increment bins
if (file.exists(FL.PDF)) {print(paste(FL.PDF, " exist. Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,paper="a4r", width=0, height=0)
# -------------------------------------------------------------------------------------------------
# A. plot diurnal pattern of O3
# -------------------------------------------------------------------------------------------------
for (site in sites)
{
	# -------------------------------------------------------------------------------------------
	# Ozone data of current site
	# -------------------------------------------------------------------------------------------
	lab.O3 <- paste("O3",site,sep=".")
	inc.O3 <- paste("inc",lab.O3,sep=".")

	ymax <- max(data.1hr.runAvg[,lab.O3],na.rm=TRUE)*1.1
	ymax <- trunc(ceiling(ymax/10))*10
	
	#
	# plot the raw and one-hour running average of the O3 at the three sites
	#
	key.string <- list(title="day",space="right",columns=1,text=list(levels(as.factor(data.1hr.runAvg[,"day"]))),lines = list(lty=lty.array),col=col.array)
	
	plot.obj <- xyplot(data.1hr.runAvg[,lab.O3]~data.1hr.runAvg[,"hourInDay"],data=data.1hr.runAvg,
		       groups = day,
		       xlab = "CST (Hour)",ylab=expression("O"[{3}] (ppbv)),main=paste(site),
		       type = rep("l",30),
        	       lty  = lty.array,
        	       lwd  = lwd.array,
        	       pch  = pch.array,
		       col  = col.array,
		       cex  = cex.array,
		       ylim = c(0,ymax),
		       xlim = c(0,24),
		       xat  = c(0,2,4,6,8,10,12,14,16,18,20,24),
		       key  = key.string,
		 
		   panel = function(x,y,...) {
			   panel.xyplot(x, y,...)
			   panel.abline(h=c(120),lty=c(2),col="black")
			   panel.abline(h=O3.median[site],lty=c(2),col="black")
			   panel.abline(v=seq(0,24),lty=2,lwd=0.2,col="black")}	
			   

		       )
	plot(plot.obj)		      
}


# -------------------------------------------------------------------------------------------------
# B. plot diurnal pattern of increment of O3
# -------------------------------------------------------------------------------------------------
for (site in sites)
{
	# -------------------------------------------------------------------------------------------
	# Ozone data of current site
	# -------------------------------------------------------------------------------------------
	lab.O3 <- paste("O3",site,sep=".")
	inc.O3 <- paste("inc",lab.O3,sep=".")

	ymax <- max(data.1hr.runAvg[,inc.O3],na.rm=TRUE)*1.1
	ymax <- trunc(ceiling(ymax/10))*10

	
	#
	# plot the raw and one-hour running average of the O3 at the three sites
	#
	key.string <- list(title="day",space="right",columns=1,text=list(levels(as.factor(data.1hr.runAvg[,"day"]))),lines = list(lty=lty.array),col=col.array)
	
	plot.obj <- xyplot(data.1hr.runAvg[data.1hr.runAvg[,inc.O3]>0,inc.O3]~data.1hr.runAvg[data.1hr.runAvg[,inc.O3]>0,"hourInDay"],data=data.1hr.runAvg,
		       groups = day,
		       xlab = "CST (Hour)",ylab=expression(Delta(O[{3}])/Delta(t)),main=paste(site),
		       type = rep("l",30),
        	       lty  = lty.array,
        	       lwd  = lwd.array,
        	       pch  = pch.array,
		       col  = col.array,
		       ylim = c(0,ymax),
		       xlim = c(0,24),
		       xat  = c(0,2,4,6,8,10,12,14,16,18,20,24),
		       key  = key.string,
		   	
		   panel = function(x,y,...) {
			   panel.xyplot(x, y,...)
			   panel.abline(h=c(120),lty=c(2),col="black")
			    panel.abline(h=O3.median[site],lty=c(2),col="black")
			   panel.abline(v=seq(0,24),lty=2,lwd=0.2,col="black")}	

		       )
	plot(plot.obj)		       
}
dev.off()

# -------------------------------------------------------------------------------------------------
# 18. Plotting diurnal pattern in points
# -------------------------------------------------------------------------------------------------
FL.PDF  <- paste(Path.OUT,paste("Ozone_1hr_runningAvg_diurnal_dotplot.pdf",sep=""),sep="/")		# OUTPUT in PDF  files of the sinuosity/quadrant on the O3 increment bins
if (file.exists(FL.PDF)) {print(paste(FL.PDF, " exist. Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,paper="a4r", width=0, height=0)
# -------------------------------------------------------------------------------------------------
# A. plot diurnal pattern of O3
# -------------------------------------------------------------------------------------------------
for (site in sites)
{
	# -------------------------------------------------------------------------------------------
	# Ozone data of current site
	# -------------------------------------------------------------------------------------------
	lab.O3 <- paste("O3",site,sep=".")
	inc.O3 <- paste("inc",lab.O3,sep=".")

	ymax <- max(data.1hr.runAvg[,lab.O3],na.rm=TRUE)*1.1
	ymax <- trunc(ceiling(ymax/10))*10
	
	#
	# plot the raw and one-hour running average of the O3 at the three sites
	#
	key.string <- list(title="day",space="right",columns=1,text=list(levels(as.factor(data.1hr.runAvg[,"day"]))),lines = list(lty=lty.array),col=col.array)
	
	plot.obj <- xyplot(data.1hr.runAvg[,lab.O3]~data.1hr.runAvg[,"hourInDay"],data=data.1hr.runAvg,
		       groups = day,
		       xlab = "CST (Hour)",ylab=expression("O"[{3}] (ppbv)),main=paste(site),
		       type = rep("p",30),
        	       lty  = lty.array,
        	       lwd  = lwd.array,
        	       pch  = pch.array,
		       col  = col.array,
		       # cex  = cex.array,
		       ylim = c(0,ymax),
		       xlim = c(0,24),
		       xat  = c(0,2,4,6,8,10,12,14,16,18,20,24),
		       key  = key.string,
		   	
		   panel = function(x,y,...) {
			   panel.xyplot(x, y,...)
			   panel.abline(h=c(120),lty=c(2),col="black")
			    panel.abline(h=O3.median[site],lty=c(2),col="black")
			   panel.abline(v=seq(0,24),lty=2,lwd=0.2,col="black")}	

		       )
		       
	plot(plot.obj)		      
}


# -------------------------------------------------------------------------------------------------
# B. plot diurnal pattern of increment of O3
# -------------------------------------------------------------------------------------------------
for (site in sites)
{
	# -------------------------------------------------------------------------------------------
	# Ozone data of current site
	# -------------------------------------------------------------------------------------------
	lab.O3 <- paste("O3",site,sep=".")
	inc.O3 <- paste("inc",lab.O3,sep=".")

	ymax <- max(data.1hr.runAvg[,inc.O3],na.rm=TRUE)*1.1
	ymax <- trunc(ceiling(ymax/10))*10

	
	#
	# plot the raw and one-hour running average of the O3 at the three sites
	#
	key.string <- list(title="day",space="right",columns=1,text=list(levels(as.factor(data.1hr.runAvg[,"day"]))),lines = list(lty=lty.array),col=col.array)
	
	plot.obj <- xyplot(data.1hr.runAvg[data.1hr.runAvg[,inc.O3]>0,inc.O3]~data.1hr.runAvg[data.1hr.runAvg[,inc.O3]>0,"hourInDay"],data=data.1hr.runAvg,
		       groups = day,
		       xlab = "CST (Hour)",ylab=expression(Delta(O[{3}])/Delta(t)),main=paste(site),
		       type = rep("p",30),
        	       lty  = lty.array,
        	       lwd  = lwd.array,
        	       pch  = pch.array,
		       col  = col.array,
		       ylim = c(0,ymax),
		       xlim = c(0,24),
		       xat  = c(0,2,4,6,8,10,12,14,16,18,20,24),
		       key  = key.string,
		   
		   panel = function(x,y,...) {
			   panel.xyplot(x, y,...)
			   panel.abline(h=c(120),lty=c(2),col="black")
			    panel.abline(h=O3.median[site],lty=c(2),col="black")
			   panel.abline(v=seq(0,24),lty=2,lwd=0.2,col="black")}	

		       )
		       
	plot(plot.obj)		       
}
dev.off()


# -------------------------------------------------------------------------------------------------
# 19. Plotting histogram of one hour average O3
# -------------------------------------------------------------------------------------------------
FL.PDF  <- paste(Path.OUT,paste("Ozone_1hr_runningAvg_histogram.pdf",sep=""),sep="/")		# OUTPUT in PDF  
if (file.exists(FL.PDF)) {print(paste(FL.PDF, " exist. Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,paper="a4r", width=0, height=0)
# -------------------------------------------------------------------------------------------------
# A. histgram of O3
# -------------------------------------------------------------------------------------------------
plot.obj1 <- densityplot(data.1hr.runAvg[,"O3.Aldine"],  plot.points = FALSE,endpoints = c(0,160),xlim = c(0,160), ylim = c(0,0.030),freq=FALSE,nclass=50,col="red",  border="white",xlab=expression("O"[{3}] (ppbv)),nclass=50,main="Aldine")
plot.obj2 <- densityplot(data.1hr.runAvg[,"O3.Bayland"], plot.points = FALSE,endpoints = c(0,160),xlim = c(0,160), ylim = c(0,0.030),freq=FALSE,nclass=50,col="blue", border="white",xlab=expression("O"[{3}] (ppbv)),nclass=50,main="Bayland")
plot.obj3 <- densityplot(data.1hr.runAvg[,"O3.DeerPark"],plot.points = FALSE,endpoints = c(0,160),xlim = c(0,160), ylim = c(0,0.030),freq=FALSE,nclass=50,col="green",border="white",xlab=expression("O"[{3}] (ppbv)),nclass=50,main="DeerPark")
plot(plot.obj1,split=c(1,1,3,1))
plot(plot.obj2,split=c(2,1,3,1),newpage=FALSE)
plot(plot.obj3,split=c(3,1,3,1),newpage=FALSE)


# -------------------------------------------------------------------------------------------------
# B. histgram of O3 increment
# -------------------------------------------------------------------------------------------------
plot.obj1 <- densityplot(data.1hr.runAvg[data.1hr.runAvg[,"inc.O3.Aldine"]>0,  "inc.O3.Aldine"],  plot.points = FALSE,endpoints = c(0,10),xlim = c(0,10), ylim = c(0,1.5),freq=FALSE,nclass=50,col="red",  border="white",xlab=expression(Delta(O[{3}])/Delta(t)),nclass=50,main="Aldine")
plot.obj2 <- densityplot(data.1hr.runAvg[data.1hr.runAvg[,"inc.O3.Bayland"]>0, "inc.O3.Bayland"], plot.points = FALSE,endpoints = c(0,10),xlim = c(0,10), ylim = c(0,1.5),freq=FALSE,nclass=50,col="blue", border="white",xlab=expression(Delta(O[{3}])/Delta(t)),nclass=50,main="Bayland")
plot.obj3 <- densityplot(data.1hr.runAvg[data.1hr.runAvg[,"inc.O3.DeerPark"]>0,"inc.O3.DeerPark"],plot.points = FALSE,endpoints = c(0,10),xlim = c(0,10), ylim = c(0,1.5),freq=FALSE,nclass=50,col="green",border="white",xlab=expression(Delta(O[{3}])/Delta(t)),nclass=50,main="DeerPark")
plot(plot.obj1,split=c(1,1,3,1))
plot(plot.obj2,split=c(2,1,3,1),newpage=FALSE)
plot(plot.obj3,split=c(3,1,3,1),newpage=FALSE)

# -------------------------------------------------------------------------------------------------
# C. histgram of O3 
# -------------------------------------------------------------------------------------------------
key.string <- list(space="top",columns=3,text=list(levels(as.factor(data.1hr.runAvg_O3_longFM[,"site"]))),lines = list(lty=c(1,1,1)),col=c("red","blue","green"))

plot.obj1 <-  densityplot(~O3,     data=data.1hr.runAvg_O3_longFM,groups=site,plot.points=FALSE,endpoints = c(0,160),xlim = c(0,160), ylim = c(0,0.030),ref=TRUE,xlab=expression("O"[{3}] (ppbv)),key=key.string,col=c("red","blue","green"))
plot(plot.obj1)

plot.obj2 <-  densityplot(~inc.O3, data=data.1hr.runAvg_O3_longFM,groups=site,plot.points=FALSE,endpoints = c(0,10), xlim = c(0,10),  ylim = c(0,1.5),  ref=TRUE,xlab=expression(Delta(O[{3}])/Delta(t)),key=key.string,col=c("red","blue","green"))
plot(plot.obj2)



dev.off()




# -------------------------------------------------------------------------------------------------
# 20. Plotting diuranal patter for the day with the highest O3 concentration
# -------------------------------------------------------------------------------------------------
FL.PDF  <- paste(Path.OUT,paste("Ozone_diurnalPatter_peakday.pdf",sep=""),sep="/")		# OUTPUT in PDF  files of the sinuosity/quadrant on the O3 increment bins
if (file.exists(FL.PDF)) {print(paste(FL.PDF, " exist. Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,paper="a4r", width=0, height=0)
# -------------------------------------------------------------------------------------------------
# A. plot diurnal pattern of O3
# -------------------------------------------------------------------------------------------------
for (site in sites)
{
	# -------------------------------------------------------------------------------------------
	# Ozone data of current site
	# -------------------------------------------------------------------------------------------
	lab.O3 <- paste("O3",site,sep=".")
	inc.O3 <- paste("inc",lab.O3,sep=".")

	ymax <- max(data.1hr.runAvg[,lab.O3],na.rm=TRUE)*1.1
	ymax <- trunc(ceiling(ymax/10))*10
	
	if ((site == "Aldine") | (site == "DeerPark"))
	{
		day.peak <- 1	# the peak O3 occurred on Sep 1st at Aldine and DeerPark
	}else
	{
		day.peak <- 7	# the peak O3 occurred on Sep 8th at Bayland
	}
	data.subset <- data.1hr.runAvg[((data.1hr.runAvg[,"day"] == day.peak) & (data.1hr.runAvg[,"month"] == 9)),]	
	
	#
	# plot the raw and one-hour running average of the O3 at the three sites
	#
	# key.string <- list(title="day",space="right",columns=1,text=list(levels(as.factor(data.1hr.runAvg[,"day"]))),lines = list(lty=lty.array),col=col.array)
	
	plot(data.subset[,"hourInDay"],data.subset[,lab.O3],
		       xlab = "CST (Hour)",ylab=expression("O"[{3}] (ppbv)),
		       main=paste(site,paste("(Sep ",day.peak,")",sep=" "),sep=""),
		       type = "b",
		       lty=1,
		       pch=16,
		       col  = "red",
		       cex  = 0.5,
		       ylim = c(0,ymax),
		       xlim = c(0,24))
	abline(v=seq(0,24),lty=2,lwd=0.2,col="grey")
	abline(h=O3.median[site],lty=c(2),col="black")
	abline(h=120,lty=2,lwd=0.2,col="black")	      
}


# -------------------------------------------------------------------------------------------------
# B. plot diurnal pattern of increment of O3
# -------------------------------------------------------------------------------------------------
for (site in sites)
{
	# -------------------------------------------------------------------------------------------
	# Ozone data of current site
	# -------------------------------------------------------------------------------------------
	lab.O3 <- paste("O3",site,sep=".")
	inc.O3 <- paste("inc",lab.O3,sep=".")

	ymax <- max(data.1hr.runAvg[,inc.O3],na.rm=TRUE)*1.1
	ymax <- trunc(ceiling(ymax/10))*10

	if ((site == "Aldine") | (site == "DeerPark"))
	{
		day.peak <- 1	# the peak O3 occurred on Sep 1st at Aldine and DeerPark
	}else
	{
		day.peak <- 8	# the peak O3 occurred on Sep 8th at Bayland
	}
	data.subset <- data.1hr.runAvg[((data.1hr.runAvg[,"day"] == day.peak) & (data.1hr.runAvg[,"month"] == 9) & (data.1hr.runAvg[,inc.O3]>0)),]	

	#
	# plot the raw and one-hour running average of the O3 at the three sites
	#
	# key.string <- list(title="day",space="right",columns=1,text=list(levels(as.factor(data.1hr.runAvg[,"day"]))),lines = list(lty=lty.array),col=col.array)
	
	plot(data.subset[,"hourInDay"],data.subset[,inc.O3],
		       xlab = "CST (Hour)",ylab=expression(Delta(O[{3}])/Delta(t)),
		       main=paste(site,paste("(Sep ",day.peak,")",sep=" "),sep=""),
		       type = "b",
		       lty=1,
		       pch=16,
		       col  = "red",
		       cex  = 0.5,
		       ylim = c(0,ymax),
		       xlim = c(0,24))
	abline(v=seq(0,24),lty=2,lwd=0.2,col="grey")
	
	abline(h=120,lty=2,lwd=0.2,col="black")	   		       
}
dev.off()





# -------------------------------------------------------------------------------------------------
# 40. time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("31_O3Analysis.R is finished successfully!\n",sep=" "))
cat(paste("31_O3Analysis.R is finished successfully!\n",sep=" "),file=FL.LOG,append=TRUE)


cat(paste("Processing time for 31_O3Analysis.R is",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)

