#
# 2_plot_monitoringData.R 
# 
#
# March 9, 2009: A major revision and re-organization of the scripts based on the discussion with Bing and Steven in the meeting of March 6, 2009
# --------------------------------------------------------------------------------------------------------------------------
# recap of the meeting:
# (1)	Taking B316 out from the cooling load calculation formula and re-calculate .  B316 will be calculated and analyzed separately (see item 5).
# (2)	Besides current treatment of the building as a whole, treat each wing separately as well, which will end up with a4 tables, one for total and one for each wing as well.
# (3)	For chiller efficiency, plot the overall cooling load against kWchiller for both pre- and post-installation.  For pre-installation, use the entire time series and for the post-installation, use the last portion of the time series with the temperature drop. Since we may need to use efficiency profile later, we need to get numbers out of such scatter plot for each temperature bin for both pre- and post-installation period, right?
# (4)	Aggregate the electricity power at each floor to each wing and plot wing-wise boxplots of the electricity power kW side by side for both pre- and post-installation periods in terms of month (like current Figure 10, but sum all levels of each wing) .  
#       Better to plot them together with the cooling loads (kind of put together Figure 9A and Figure 10 together, but (a) aggregate kW of three levels of each wing, (b) cooling load also at each wing also treated separately.
#       Could be too busy if you want to plot all these data in one chart.  I recommend three plots for each wing:
#       1. Electricity kW for baseline and post-installation by month
#       2. Cooling load for baseline and post-installation by month
#       3. Heating load for baseline and post-installation by month 
# (5)	Calculate the cooling load at B316 separately, do weather normalization and compare pre- and post-installation periods.  Hope they are similar.
# (6)	Get occupancy data from Bing and make dot or bar plot to show the month-wise occupancy in pre- and post-installation periods.
#       See attachment. Column E.  It appears we were able to get the monthly occupancy rate only for the baseline period.  I have sent a note to Tom to see if he could get the post-installation occupancy rate.
# (7)	Units: use kBtu and may add one additional column for cooling after convert to kW(?).
#       Use KBtu for heating load and cooling load.  Will add additional column for chiller KWh later on.
# (8)	Need to have utility rate data for the calculation of cost.
#       I have sent email to my POC at NSA Oceana for utility rate. 
# --------------------------------------------------------------------------------------------------------------------------
# only changed "treatment" to "installation" on this particular script on March 9, 2009
# --------------------------------------------------------------------------------------------------------------------------
# 
# Created on November 18, 2008
# 
# This script is to plot the RAW pre- and post-installation data
# These plots are used for identify problematic data (outlier, data due to device malfunction etc)
#
# November 22, 2008: Review scripts and simplify as much as possible
# Feb 8, 2009 (checked)
# -------------------------------------------------------------------------
#
# eliminate all stuff
# 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# ----------------------------------------------- 
# load libraries
# -----------------------------------------------
library(lattice)
library(chron)	

# ----------------------------------------------- 
# 	setup plotting limits for x axis
# note: the pre-installation  period is from 11/8/2006 21:00 -> 10/10/2007 8;30
#       the post-installation period is from 11/1/2007 0:45  -> 11/13/2008 7:00
# ----------------------------------------------- 
# ----------------------------------------------- 
# setup plotting limits for x axis
# ----------------------------------------------- 
xlim4plot_pre  <- c(chron(dates="10/30/2006", times="0:0:0",format=c('m/d/y','h:m:s')),chron(dates="10/15/2007",times="23:59:59",format=c('m/d/y','h:m:s')))
xlim4plot_post <- c(chron(dates="10/30/2007", times="0:0:0",format=c('m/d/y','h:m:s')),chron(dates="11/15/2008",times="23:59:59",format=c('m/d/y','h:m:s')))


xat4plot_pre  <- c(chron(dates="11/1/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="12/1/2006",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="1/1/2007", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="2/1/2007", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="3/1/2007", times="0:0:0",  format=c('m/d/y','h:m:s')),	       
               	   chron(dates="4/1/2007", times="0:0:0",  format=c('m/d/y','h:m:s')),	       	   
               	   chron(dates="5/1/2007", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="6/1/2007", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="7/1/2007", times="0:0:0",  format=c('m/d/y','h:m:s')),	       
               	   chron(dates="8/1/2007", times="0:0:0",  format=c('m/d/y','h:m:s')),
               	   chron(dates="9/1/2007", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="10/1/2007",times="0:0:0",  format=c('m/d/y','h:m:s')))


xat4plot_post <- c(chron(dates="11/1/2007",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="12/1/2007",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="1/1/2008", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="2/1/2008", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="3/1/2008", times="0:0:0",  format=c('m/d/y','h:m:s')),	       
               	   chron(dates="4/1/2008", times="0:0:0",  format=c('m/d/y','h:m:s')),	       	   
               	   chron(dates="5/1/2008", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="6/1/2008", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="7/1/2008", times="0:0:0",  format=c('m/d/y','h:m:s')),	       
               	   chron(dates="8/1/2008", times="0:0:0",  format=c('m/d/y','h:m:s')),
               	   chron(dates="9/1/2008", times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="10/1/2008",times="0:0:0",  format=c('m/d/y','h:m:s')),
	       	   chron(dates="11/1/2008",times="0:0:0",  format=c('m/d/y','h:m:s')))
	       	   
xlab4plot_pre  <- c("Nov","Dec","2007","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct")
xlab4plot_post <- c("Nov","Dec","2008","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov")

# ----------------------------------------------- 
# change to the script directory
# ----------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2009_Techval/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2009_Techval/0_scripts"
}
setwd(Path.Current)

# -----------------------------------------------
# define a summary function
# -----------------------------------------------
source(paste(Path.Current,"my_functions.R",sep="/"))

# -----------------------------------------------
# setup output and log directory
# -----------------------------------------------
Path.in    <- "../1_data_monitoring"		# INPUT processed result directory
Path.out   <- "../2_plot_monitoringData"	# OUTPUT processed result directory
Path.log   <- "../0_log"			# OUTPUT log  directory

if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ----------------------------------------------- 
# create a LOG file and a TIME Recording file
# ----------------------------------------------- 
FL.TIME <- paste(Path.log,"time.log",sep="/")			# OUTPUT Time Log file for all scripts
FL.LOG  <- paste(Path.log,"2_plot_monitoringData.log",sep="/")	# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist. Delete it!"));file.remove(FL.LOG)}
cat(paste("Log file for plotting raw pre- and post-installation data with [2_plot_monitoringData.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
          "*                            [2_plot_monitoringData.R]                           *",
          "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)

# -----------------------------------------------
# define OUTPUT files
# -----------------------------------------------
FL.OBJ <- paste(Path.in,paste("rawData.Rdata",sep=""),sep="/")		# INPUT Rdata
load(FL.OBJ)
rm(data.merged.wide,data.merged.long)
str.title = "raw"


# ---------------------------------------------------------------------------------------------------
# time series of pre-installation
# ---------------------------------------------------------------------------------------------------
FL.PDF <- paste(Path.out,paste("timeseries_pre.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- paste("timeseries_pre.pdf",sep="")
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)
plot.obj <- xyplot(value ~ time.chron | as.factor(vari),data=data.pre.long,
		   xlab="date/time",ylab="",
		   type="l",lty=1,cex=0.5,col="red",
		   main=paste("pre-installation:"," (",str.title,")",sep=""),
		   scales=list(x=list(limits=xlim4plot_pre,at = xat4plot_pre),y="free"),
		   layout=c(0,12),
		   as.table=TRUE,
		   
		   panel = function(x,y,...) {
			   panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="red")
			   panel.abline(h=0,lty=2,col="black")}
		   )		 
plot(plot.obj)  
dev.off()


# ---------------------------------------------------------------------------------------------------
# time series of post-installation
# ---------------------------------------------------------------------------------------------------	
FL.PDF <- paste(Path.out,paste("timeseries_post.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- paste("timeseries_post.pdf",sep="")
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)
plot.obj <- xyplot(value ~ time.chron | as.factor(vari),data=data.post.long,
		   xlab="date/time",ylab="",
		   type="l",lty=1,cex=0.5,col="red",
		   main=paste("post-installation:"," (",str.title,")",sep=""),
		   scales=list(x=list(limits=xlim4plot_post,at = xat4plot_post),y="free"),
		   layout=c(0,12),
		   as.table=TRUE,
		  
		   panel = function(x,y,...) {
			   panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="red")
			   panel.abline(h=0,lty=2,col="black")}
		   )
plot(plot.obj)  
dev.off()



# ---------------------------------------------------------------------------------------------------
# time series of pre- and post-installation
# ---------------------------------------------------------------------------------------------------	
FL.PDF <- paste(Path.out,paste("timeseries_pre_post.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- paste("timeseries_pre_post.pdf",sep="")
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)

for (lab in lab.post)
{
	plot.obj.pre <- xyplot(data.pre.wide[,lab] ~ data.pre.wide[,"time.chron"],
			       xlab="date/time",ylab=lab,
			       type="l",lty=1,cex=0.5,col="red",
			       main=paste("pre-installation: ",lab," (",str.title,")",sep=""),
			       scales=list(x=list(limits=xlim4plot_pre,at = xat4plot_pre),y="free"),
			       
		   	       panel = function(x,y,...) {
				       panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="red")
				       panel.abline(h=0,lty=2,col="black")}
		   		)

	plot.obj.post<- xyplot(data.post.wide[,lab] ~ data.post.wide[,"time.chron"],
			       xlab="date/time",ylab=lab,
			       type="l",lty=1,cex=0.5,col="blue",
			       main=paste("post-installation: ",lab," (",str.title,")",sep=""),
			       scales=list(x=list(limits=xlim4plot_post,at = xat4plot_post),y="free"),
			       
		               panel = function(x,y,...) {
		   	               panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="blue")
			               panel.abline(h=0,lty=2,col="black")}
		   		)

	plot(plot.obj.pre, split=c(1,1,1,2))
	plot(plot.obj.post,split=c(1,2,1,2),newpage=FALSE)
}	
dev.off()


# ---------------------------------------------------------------------------------------------------
# time used for completing this script
# ---------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time


cat(paste("\n2_plot_monitoringData.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n2_plot_monitoringData.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)
cat(paste("Processing time for [2_plot_monitoringData.R] is",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)

