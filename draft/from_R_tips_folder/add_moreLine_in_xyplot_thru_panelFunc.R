#
# 3_clean_monitoringData.R 
# 
# ---------------------
# INPUT of this script:
#
# "../1_data_monitoring/rwaData.Rdata" which contains the following data objects
# [data.merged.wide]		!!!! ALL CLEANING WAS DONE ON THIS DATA OBJECT !!!!!
# [data.merged.long]		this data object is NOT used here
# [data.pre.wide]		this data object is NOT used here
# [data.pre.long]		this data object is NOT used here
# [data.post.wide]		this data object is NOT used here
# [data.post.long]		this data object is NOT used here
# [lab.pre]			this data object is NOT used here
# [lab.post]			this data object is NOT used here

# OUTPUT of this script:
# ---------------------
# "../3_clean_monitoringData" with outputs of the following data objects:
# [data.cleaned.wide]
# [data.cleaned.long]
# [lab.pre]
# [lab.post]
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
# Created on November 21, 2008
# 
# This script is to clean the data
#
# The cleaning is based on the monitoring log document "NAS Oceana BEQ423" provided by Bing Liu and the inspection of the data file in excel
#
# For flow rate data, all negative values less are replaced with NA
# 
# 
# This script is used 
#      to clean the raw pre- and post-installation data
#
# November 22, 2008: Review scripts and simplify as much as possible
# Feb 8, 2009 (cleaned)
# INPUT
# "../1_data_monitoring/rawData.Rdata"		
#
# OUTPUT
# "../3_clean_monitoringData/cleanedData.Rdata"	
# "../3_clean_monitoringData/cleanedData.CSV"	
#
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
Path.in    <- "../1_data_monitoring"		# INPUT  processed result directory
Path.out   <- "../3_clean_monitoringData"	# OUTPUT processed result directory
Path.log   <- "../0_log"			# OUTPUT log  directory
if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ----------------------------------------------- 
# create a LOG file and a TIME Recording file
# ----------------------------------------------- 
FL.TIME     <- paste(Path.log,"time.log",sep="/")			# OUTPUT Time Log file for all scripts
FL.LOG      <- paste(Path.log,"3_clean_monitoringData.log",sep="/")	# OUTPUT Log file

if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file of [3_clean_monitoringData.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
          "*                    [3_clean_monitoringData.R]                            *",
          "***************************************************************************\n",sep="\n"),file=FL.LOG, append=TRUE)

# -----------------------------------------------
# output file names
# -----------------------------------------------
FL.OBJ.cleaned <- paste(Path.out,paste("cleanedData.Rdata",sep=""),sep="/")	# OUTPUT Rdata
FL.CSV.cleaned <- paste(Path.out,paste("cleanedData.csv",sep=""),sep="/")	# OUTPUT Rdata
if (file.exists(FL.OBJ.cleaned)){print(paste(FL.OBJ.cleaned,"exist.Delete it!"));file.remove(FL.OBJ.cleaned)}
if (file.exists(FL.CSV.cleaned)){print(paste(FL.CSV.cleaned,"exist.Delete it!"));file.remove(FL.CSV.cleaned)}




# -----------------------------------------------
# define INPUT files
# -----------------------------------------------
FL.OBJ.raw <- paste(Path.in,paste("rawData.Rdata",sep=""),sep="/")	# INPUT Rdata
load(FL.OBJ.raw)
rm(data.merged.long,data.pre.wide,data.pre.long,data.post.wide,data.post.long,lab.pre,lab.post)


# -----------------------------------------------
# cleaning step 1:  delete data in the following periods (based on log documents)
# -----------------------------------------------
period1.start <- chron(dates="6/5/2007", times="11:15:00",format=c('m/d/y','h:m:s'))	# All EMCO flow meter dived down to -500 gpm between 6/5/2007 - 6/11/2007
period1.end   <- chron(dates="6/11/2007",times="11:00:00",format=c('m/d/y','h:m:s'))
period2.start <- chron(dates="4/8/2008", times="10:30:00",format=c('m/d/y','h:m:s'))	# Failure of EMCO flow meters and T_hws between 4/8/2008 and 4/28/2008
period2.end   <- chron(dates="4/28/2008",times="5:30:00",format=c('m/d/y','h:m:s'))

# clean the wide format (index of data to be kept)
idx.wide <- seq(1:dim(data.merged.wide)[1])[ (data.merged.wide[,"time.chron"] < period1.start) | 
                                            ((data.merged.wide[,"time.chron"] > period1.end)   & (data.merged.wide[,"time.chron"] < period2.start)) |
                                             (data.merged.wide[,"time.chron"] > period2.end)]
# -------------------------------------------------------------------------------------------------
# wide format of cleaned data
# -------------------------------------------------------------------------------------------------
data.cleaned.wide <- data.merged.wide[idx.wide,]


# -----------------------------------------------
# cleaning step 2:  replace chiller flow rate "Fcwr##" with NA due to a chiller shut off (based on log documents)
# -----------------------------------------------
var.flowRate  <- c(grep("Fcwr",names(data.cleaned.wide),value=TRUE))
period3.start <- chron(dates="5/9/2008", times="20:15:00",format=c('m/d/y','h:m:s'))	# Due to chiller shut off
period3.end   <- chron(dates="5/27/2008",times="8:15:00",format=c('m/d/y','h:m:s'))
idx.wide      <- seq(1:dim(data.cleaned.wide)[1])[(data.cleaned.wide[,"time.chron"] >= period3.start) & 
                                                  (data.cleaned.wide[,"time.chron"] <= period3.end)]

# replacing flowrate with NA 
for (lab.flowRate in var.flowRate)
{                                                         # after 5/9/2008 20:15                             and before 5/27/2008 8:15                           and for current flowRate variable            and current flowRate is not NA
	data.cleaned.wide[idx.wide,lab.flowRate ] <- ifelse(is.na(data.cleaned.wide[idx.wide,lab.flowRate]),data.cleaned.wide[idx.wide,lab.flowRate],NA)
}



# -----------------------------------------------
# cleaning step 3:  replace all negative flow rate data with NA (based on data inspection)
# -----------------------------------------------
var.flowRate <- c(grep("Fcwr",names(data.cleaned.wide),value=TRUE),grep("Fhws",names(data.cleaned.wide),value=TRUE))
for (lab.flowRate in var.flowRate)
{ 
	data.cleaned.wide[(data.cleaned.wide[,lab.flowRate] <= 0)      & !(is.na(data.cleaned.wide[,lab.flowRate])),lab.flowRate ] <- NA
}

cat(paste("\nnegative values  in the flow rate fields are replaced with \"NA\"","!\n",sep=""),file=FL.LOG,append=TRUE)


# -----------------------------------------------
# cleaning step 4:  replacing 7 rows of the fields of "kWa1", "kWa2", "kWa3", "kWb1", "kWb2", "kWb3", "kWchiller" between 12/14/2006 8:00 to 9:30 with NA (based on log document and data inspection)
# -----------------------------------------------
dates.outliers <- rep("12/14/2006",7)
times.outliers <- c("8:0:0","8:15:0","8:30:0","8:45:0","9:0:0","9:15:0","9:30:0")
chron.outliers <- chron(dates=dates.outliers, times=times.outliers,format=c('m/d/y','h:m:s'))
idx.wide       <- charmatch(chron.outliers,data.cleaned.wide[,"time.chron"])

var.kW <- c("kWa1(kW)", "kWa2(kW)", "kWa3(kW)", "kWb1(kW)", "kWb2(kW)", "kWb3(kW)", "kWchiller(kW)")
for (lab.kW in var.kW)
{ 
	data.cleaned.wide[idx.wide,lab.kW ] <- NA
}

cat(paste("\n7 rows between 12/14/2006 8:00 and 9:30 in the following fileds of the pre-installation data are replaced with \"NA\"",
          "\t\"kWa1(kW)\", \"kWa2(kW)\", \"kWa3(kW)\", \"kWb1(kW)\", \"kWb2(kW)\", \"kWb3(kW)\", \"kWchiller(kW)\"",
          "\n",sep="\n"),file=FL.LOG,append=TRUE)
cat(paste("\n7 rows in some of the kW fields of the pre-installation data are replaced with \"NA\""," \n",sep=""),file=FL.LOG,append=TRUE)

# -----------------------------------------------
# cleaning step 5:  replacing 1 row of the fields of "kWa3","kWb1","kWb2", "kWb3", "kWc1", "kWc2", "kWc3", "kWchiller" at 4/19/2008 7:45 with missing value (ver high values not real)
# -----------------------------------------------
dates.outliers <- "4/19/2008"
times.outliers <- "7:45:0"
chron.outliers <- chron(dates=dates.outliers, times=times.outliers,format=c('m/d/y','h:m:s'))
idx.wide       <- charmatch(chron.outliers,data.cleaned.wide[,"time.chron"])
idx.wide       <- idx.wide[!is.na(idx.wide)]	# this outlier actually has been already deleted by step 1 (April 8-April 28, 2008)

if (length(idx.wide)>0)
	{
	var.kW <- grep("^kW",names(data.cleaned.wide),value=TRUE)
	for (lab.kW in var.kW)
	{ 
		data.cleaned.wide[idx.wide,lab.kW ] <- NA
	}
	cat(paste("\n7 rows at 4/19/2008 7:45 in the following fileds of the post-installation data are replaced with \"NA\"",
			"\t\"kWa3\",\"kWb1\",\"kWb2\", \"kWb3\", \"kWc1\", \"kWc2\", \"kWc3\", \"kWchiller\"",
			"\n",sep="\n"),file=FL.LOG,append=TRUE)
	cat(paste("\n1 row in some of the kW fields of the post-installation data are replaced with \"NA\""," (see",FL.MEMO," for details)\n",sep=""),file=FL.LOG,append=TRUE)
}

# -----------------------------------------------
# cleaning step 6:  the ten kW fields each has 33-38 0 values (no negative values)
# inspect the data found that most of the 0 values occureed when flow rate are negative.  So decide to convert these 0s to NA
# (1)  3 points: 12/14/2006 7:15 -- 7:45
# (2)  1 point:  (07/05/07 05:45:00)
# (3)  8 points: 2/3/2008 4:40 - 6:15
# (4)  4 points: 6/25/2008 4:45 -- 5:30
# (5)  1 point:  (07/10/08 07:00:00)
# (6)  2 points: 9/6/2008 4:45 - 5:00
# (7) 14 points: 9/14/2008 14:15 -- 17:30
# (8)  8 points: 9/25/2008 4:30 -- 6:15
# -----------------------------------------------
var.kW <- grep("^kW",names(data.cleaned.wide),value=TRUE)
tmp <- data.cleaned.wide[,var.kW]
tmp[tmp==0 & !is.na(tmp)] <-NA
data.cleaned.wide[,var.kW] <- tmp


# -----------------------------------------------
# cleaning step 7:  discussed with Bing and decide to de-spike Tcws.
# The following rules are used based on visual inspection of the time series of Tcws.
# (1)  for pre-installation  period (             date < 10/10/2007 8:30), Tcws >= 13.5 are replaced with NA
# (2a) for post-installation period (12/10/2007 9:00 <--> 5/27/2008 8:30), Tcws >= 14.5 are replaced with NA
# (2b) for post-installation period ( 5/27/2008 8:45 <--> 8/4/2008 15:00), Tcws >= 11.0 are replaced with NA
# (2c) for post-installation period ( 8/4/2008 15:15),                     Tcws >=  9.0 are replaced with NA
# -----------------------------------------------
cut.date1 <- chron(dates="10/10/2007",times="8:30:00", format=c('m/d/y','h:m:s'))	# cutoff date for eliminating peaks in Tcws
cut.Tcws1 <- 13.5

cut.date2 <- chron(dates="12/10/2007",times="9:00:00", format=c('m/d/y','h:m:s'))	# cutoff date for eliminating peaks in Tcws
cut.Tcws2 <- 14.5

cut.date3 <- chron(dates="5/27/2008", times="8:45:00", format=c('m/d/y','h:m:s'))	# cutoff date for eliminating peaks in Tcws
cut.Tcws3 <- 11.0

cut.date4 <- chron(dates="8/4/2008",  times="15:15:00",format=c('m/d/y','h:m:s'))	# cutoff date for eliminating peaks in Tcws
cut.Tcws4 <-  9.0

# supply chilled water
var.Tcws <- grep("Tcws",names(data.cleaned.wide),value=TRUE)
for (lab.Tcws in var.Tcws)
{ 
	data.cleaned.wide[ ((data.cleaned.wide[,"time.chron"] <  cut.date1)                                                   & (data.cleaned.wide[,lab.Tcws] > cut.Tcws1) & (!is.na(data.cleaned.wide[,lab.Tcws])) & (!is.na(data.cleaned.wide[,"time.chron"]))),lab.Tcws ] <- NA
	data.cleaned.wide[(((data.cleaned.wide[,"time.chron"] >= cut.date2) & (data.cleaned.wide[,"time.chron"] < cut.date3)) & (data.cleaned.wide[,lab.Tcws] > cut.Tcws2) & (!is.na(data.cleaned.wide[,lab.Tcws])) & (!is.na(data.cleaned.wide[,"time.chron"]))),lab.Tcws ] <- NA
	data.cleaned.wide[(((data.cleaned.wide[,"time.chron"] >= cut.date3) & (data.cleaned.wide[,"time.chron"] < cut.date4)) & (data.cleaned.wide[,lab.Tcws] > cut.Tcws3) & (!is.na(data.cleaned.wide[,lab.Tcws])) & (!is.na(data.cleaned.wide[,"time.chron"]))),lab.Tcws ] <- NA
	data.cleaned.wide[ ((data.cleaned.wide[,"time.chron"] >= cut.date4)                                                   & (data.cleaned.wide[,lab.Tcws] > cut.Tcws4) & (!is.na(data.cleaned.wide[,lab.Tcws])) & (!is.na(data.cleaned.wide[,"time.chron"]))),lab.Tcws ] <- NA
}

# return chilled water
var.Tcwr <- grep("Tcwr",names(data.cleaned.wide),value=TRUE)
for (lab.Tcwr in var.Tcwr)
{ 
	data.cleaned.wide[ ((data.cleaned.wide[,"time.chron"] <  cut.date1)                                                   & (data.cleaned.wide[,lab.Tcwr] > cut.Tcws1) & (!is.na(data.cleaned.wide[,lab.Tcwr])) & (!is.na(data.cleaned.wide[,"time.chron"]))),lab.Tcwr ] <- NA
	data.cleaned.wide[(((data.cleaned.wide[,"time.chron"] >= cut.date2) & (data.cleaned.wide[,"time.chron"] < cut.date3)) & (data.cleaned.wide[,lab.Tcwr] > cut.Tcws2) & (!is.na(data.cleaned.wide[,lab.Tcwr])) & (!is.na(data.cleaned.wide[,"time.chron"]))),lab.Tcwr ] <- NA
	data.cleaned.wide[(((data.cleaned.wide[,"time.chron"] >= cut.date3) & (data.cleaned.wide[,"time.chron"] < cut.date4)) & (data.cleaned.wide[,lab.Tcwr] > cut.Tcws3) & (!is.na(data.cleaned.wide[,lab.Tcwr])) & (!is.na(data.cleaned.wide[,"time.chron"]))),lab.Tcwr ] <- NA
	data.cleaned.wide[ ((data.cleaned.wide[,"time.chron"] >= cut.date4)                                                   & (data.cleaned.wide[,lab.Tcwr] > cut.Tcws4) & (!is.na(data.cleaned.wide[,lab.Tcwr])) & (!is.na(data.cleaned.wide[,"time.chron"]))),lab.Tcwr ] <- NA
}
cat(paste("\npeaks in Tcws are replaced with \"NA\"","!\n",sep=""),file=FL.LOG,append=TRUE)
# the last cleaning on the wide format [data.cleaned.wide]

# list of variables
lab.var <- names(data.cleaned.wide)[!(names(data.cleaned.wide) %in% c("time.chron","dates","times","month","day","year","hour","minute","second","period"))]# 

# convert the cleaned wide format data into a long format
#
 dat.tmp      <- data.cleaned.wide[,lab.var]	# only the variables
  no.tmp      <- dim(dat.tmp)[2]		# number of the variables
 dat.tmp.long <- stack(dat.tmp)			# stack the pre data variable one on the top of another

data.cleaned.long <- data.frame(dates  = rep(data.cleaned.wide[,"dates"], no.tmp),
                                times  = rep(data.cleaned.wide[,"times"], no.tmp),
                                month  = rep(data.cleaned.wide[,"month"], no.tmp),
                                day    = rep(data.cleaned.wide[,"day"],   no.tmp),
                                year   = rep(data.cleaned.wide[,"year"],  no.tmp),
                                hour   = rep(data.cleaned.wide[,"hour"],  no.tmp),
                                minute = rep(data.cleaned.wide[,"minute"],no.tmp),
                                second = rep(data.cleaned.wide[,"second"],no.tmp),
           	                value  = dat.tmp.long[,1],
			        vari   = dat.tmp.long[,2],
			        period = rep(data.cleaned.wide[,"period"],no.tmp))
rm(dat.tmp,dat.tmp.long)

# -------------------------------------------------------------------------------------------------
# create a chron data frame for time series plotting
# -------------------------------------------------------------------------------------------------
data.cleaned.long <- data.frame(time.chron = chron(dates  = as.character(data.cleaned.long[,"dates"]),
		                                   times  = as.character(data.cleaned.long[,"times"]),
		                                   format = c('m/d/y','h:m:s')),
		                data.cleaned.long)
		            
		            
# ---------------------------------------------------------
# plot cleaned Tcws
# ---------------------------------------------------------
FL.PDF <- paste(Path.out,paste("timeseries_Tcws.pdf",sep=""),sep="/")	# OUTPUT Time Series 
if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,paper="a4r", width=0, height=0)

	plot.obj1 <- xyplot(data.cleaned.wide[,"Tcwsa(C)"]     ~ time.chron,data=data.cleaned.wide,type="l",col="red",main = "pre-installation Tcwsa(C)",     xlab="Date/Time",ylab="Tcwsa(C)",    scales=list(x=list(limits=xlim4plot_pre,tick.number = 5,at = xat4plot_pre),y="free"),panel = function(...){panel.xyplot(...);panel.abline(h=c(cut.Tcws1,cut.Tcws2,cut.Tcws3,cut.Tcws4));panel.abline(v=c(cut.date1,cut.date2,cut.date3,cut.date4))})
	plot.obj2 <- xyplot(data.cleaned.wide[,"Tcwsb(C)"]     ~ time.chron,data=data.cleaned.wide,type="l",col="red",main = "pre-installation Tcwsb(C)",     xlab="Date/Time",ylab="Tcwsb(C)",    scales=list(x=list(limits=xlim4plot_pre,tick.number = 5,at = xat4plot_pre),y="free"),panel = function(...){panel.xyplot(...);panel.abline(h=c(cut.Tcws1,cut.Tcws2,cut.Tcws3,cut.Tcws4));panel.abline(v=c(cut.date1,cut.date2,cut.date3,cut.date4))})
	plot.obj3 <- xyplot(data.cleaned.wide[,"Tcwsc(C)"]     ~ time.chron,data=data.cleaned.wide,type="l",col="red",main = "pre-installation Tcwsc(C)",     xlab="Date/Time",ylab="Tcwsc(C)",    scales=list(x=list(limits=xlim4plot_pre,tick.number = 5,at = xat4plot_pre),y="free"),panel = function(...){panel.xyplot(...);panel.abline(h=c(cut.Tcws1,cut.Tcws2,cut.Tcws3,cut.Tcws4));panel.abline(v=c(cut.date1,cut.date2,cut.date3,cut.date4))})
	plot.obj4 <- xyplot(data.cleaned.wide[,"Tcws_b316(C)"] ~ time.chron,data=data.cleaned.wide,type="l",col="red",main = "pre-installation Tcws_b315(C)", xlab="Date/Time",ylab="Tcws_b316(C)",scales=list(x=list(limits=xlim4plot_pre,tick.number = 5,at = xat4plot_pre),y="free"),panel = function(...){panel.xyplot(...);panel.abline(h=c(cut.Tcws1,cut.Tcws2,cut.Tcws3,cut.Tcws4));panel.abline(v=c(cut.date1,cut.date2,cut.date3,cut.date4))})

	plot.obj5 <- xyplot(data.cleaned.wide[,"Tcwsa(C)"]     ~ time.chron,data=data.cleaned.wide,type="l",col="red",main = "post-installation Tcwsa(C)",     xlab="Date/Time",ylab="Tcwsa(C)",    scales=list(x=list(limits=xlim4plot_post,tick.number = 5,at = xat4plot_post),y="free"),panel = function(...){panel.xyplot(...);panel.abline(h=c(cut.Tcws1,cut.Tcws2,cut.Tcws3,cut.Tcws4));panel.abline(v=c(cut.date1,cut.date2,cut.date3,cut.date4))})
	plot.obj6 <- xyplot(data.cleaned.wide[,"Tcwsb(C)"]     ~ time.chron,data=data.cleaned.wide,type="l",col="red",main = "post-installation Tcwsb(C)",     xlab="Date/Time",ylab="Tcwsb(C)",    scales=list(x=list(limits=xlim4plot_post,tick.number = 5,at = xat4plot_post),y="free"),panel = function(...){panel.xyplot(...);panel.abline(h=c(cut.Tcws1,cut.Tcws2,cut.Tcws3,cut.Tcws4));panel.abline(v=c(cut.date1,cut.date2,cut.date3,cut.date4))})
	plot.obj7 <- xyplot(data.cleaned.wide[,"Tcwsc(C)"]     ~ time.chron,data=data.cleaned.wide,type="l",col="red",main = "post-installation Tcwsc(C)",     xlab="Date/Time",ylab="Tcwsc(C)",    scales=list(x=list(limits=xlim4plot_post,tick.number = 5,at = xat4plot_post),y="free"),panel = function(...){panel.xyplot(...);panel.abline(h=c(cut.Tcws1,cut.Tcws2,cut.Tcws3,cut.Tcws4));panel.abline(v=c(cut.date1,cut.date2,cut.date3,cut.date4))})
	plot.obj8 <- xyplot(data.cleaned.wide[,"Tcws_b316(C)"] ~ time.chron,data=data.cleaned.wide,type="l",col="red",main = "post-installation Tcws_b315(C)", xlab="Date/Time",ylab="Tcws_b316(C)",scales=list(x=list(limits=xlim4plot_post,tick.number = 5,at = xat4plot_post),y="free"),panel = function(...){panel.xyplot(...);panel.abline(h=c(cut.Tcws1,cut.Tcws2,cut.Tcws3,cut.Tcws4));panel.abline(v=c(cut.date1,cut.date2,cut.date3,cut.date4))})
 
	plot(plot.obj1)
	plot(plot.obj2)
	plot(plot.obj3)
	plot(plot.obj4)
	plot(plot.obj5)
	plot(plot.obj6)
	plot(plot.obj7)
	plot(plot.obj8)
dev.off()

		            

# ---------------------------------------------------------
# plot Flow rate of "hot water supply"
# ---------------------------------------------------------
FL.PDF <- paste(Path.out,paste("timeseries_flowrate.pdf",sep=""),sep="/")	# OUTPUT Time Series 
if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,paper="a4r", width=0, height=0)
	var.Fhws    <- grep("Fhws",levels(as.factor(data.cleaned.long[,"vari"])),value=TRUE)
	data.subset <- data.cleaned.long[data.cleaned.long[,"vari"] %in% var.Fhws,,drop=TRUE]

	key.dailyCurve <- list(title="Fhws",columns=length(var.Fhws),space="top",cex=0.75,
			       text=list(paste(var.Fhws,sep=" ")),
			       lines=list(lty=rep("solid",length(var.Fhws)),
			       col=c("red","blue","green","cyan","black")[1:length(var.Fhws)]))

	plot.Fhws <- xyplot(value~time.chron | period,data=data.subset,
			    groups = vari,
			    layout = c(1,2),
			    key = key.dailyCurve,
			    as.table = TRUE,
			    type=c("l","l","l","l","l"),lty=c(1,1,1,1,1),col=c("red","blue","green","cyan","black"),xlab="Date/time",ylab="gpm",main="Fhws")
	plot(plot.Fhws)

	# ---------------------------------------------------------
	# plot Flow rate of hot water supply at the three wings ONLY
	# ---------------------------------------------------------
	var.Fhws    <- grep("Fhws_",levels(as.factor(data.cleaned.long[,"vari"])),value=TRUE)
	data.subset <- data.cleaned.long[data.cleaned.long[,"vari"] %in% var.Fhws,]
	data.subset[data.subset[,"value"] < -10 & !(is.na(data.subset[,"value"])),"value"]   <- NA


	key.dailyCurve <- list(title="Fhws",columns=nlevels(as.factor(data.subset$vari)),space="top",cex=0.75,
			       text=list(paste(levels(as.factor(data.subset$vari)),sep=" ")),
			       lines=list(lty=rep("solid",nlevels(as.factor(data.subset$vari))),
			       col=c("red","blue","green","cyan","black")[1:length(var.Fhws)]))

	plot.Fhws <- xyplot(value~time.chron | period,data=data.subset,
			    groups = vari,
			    layout = c(1,2),
			    key = key.dailyCurve,
			    type=c("l","l","l","l","l"),lty=c(1,1,1,1,1),col=c("red","blue","green","cyan","black"),xlab="Date/time",ylab="gpm",main="Fhws")
	plot(plot.Fhws)




	# ---------------------------------------------------------
	# plot Flow rate of chilled water supply
	# ---------------------------------------------------------
	var.Fcwr    <- grep("Fcwr",levels(as.factor(data.cleaned.long[,"vari"])),value=TRUE)
	data.subset <- data.cleaned.long[data.cleaned.long[,"vari"] %in% var.Fcwr,]
	data.subset[data.subset[,"value"] < -10 & !(is.na(data.subset[,"value"])),"value"]   <- NA


	key.dailyCurve <- list(title="Fcwr",columns=nlevels(as.factor(data.subset$vari)),space="top",cex=0.75,
			       text=list(paste(levels(as.factor(data.subset$vari)),sep=" ")),
			       lines=list(lty=rep("solid",nlevels(as.factor(data.subset$vari))),
			       col=c("red","blue","green","cyan","black")[1:length(var.Fhws)]))

	plot.Fcwr <- xyplot(value~time.chron | period,data=data.subset,
			    groups = vari,
			    layout = c(1,2),
			    key = key.dailyCurve,
			    type=c("l","l","l","l","l"),lty=c(1,1,1,1,1),col=c("red","blue","green","cyan","black"),xlab="Date/time",ylab="gpm",main="Fcwr")
	plot(plot.Fcwr)

	# ---------------------------------------------------------
	# plot Flow rate of chilled water supply at three wings ONLY
	# ---------------------------------------------------------
	var.Fcwr    <- c("Fcwr_a(gpm)","Fcwr_b(gpm)","Fcwr_c(gpm)" )
	data.subset <- data.cleaned.long[data.cleaned.long[,"vari"] %in% var.Fcwr,]
	data.subset[data.subset[,"value"] < -10 & !(is.na(data.subset[,"value"])),"value"]   <- NA


	key.dailyCurve <- list(title="Fcwr",columns=nlevels(as.factor(data.subset$vari)),space="top",cex=0.75,
			       text=list(paste(levels(as.factor(data.subset$vari)),sep=" ")),
			       lines=list(lty=rep("solid",nlevels(as.factor(data.subset$vari))),
			       col=c("red","blue","green","cyan","black")[1:length(var.Fhws)]))

	plot.Fcwr <- xyplot(value~time.chron | period,data=data.subset,
			    groups = vari,
			    layout = c(1,2),
			    key = key.dailyCurve,
			    type=c("l","l"),lty=c(1,1,1,1,1),col=c("red","blue","green","cyan","black"),xlab="Date/time",ylab="gpm",main="Fcwr")
	plot(plot.Fcwr)
dev.off()




#
# ---------------------------------------------------------------------------------------------------
# split the cleaned long format data into two
# ---------------------------------------------------------------------------------------------------
data.pre.long  <- subset(data.cleaned.long,period == "pre")
data.post.long <- subset(data.cleaned.long,period == "post")
data.pre.wide  <- subset(data.cleaned.wide,period == "pre")
data.post.wide <- subset(data.cleaned.wide,period == "post")
lab.pre        <- lab.var
lab.post       <- lab.var


# ---------------------------------------------------------
# output cleaned raw data
# ---------------------------------------------------------
cat(paste("\nraw data have been cleaned\n"))
cat(paste("\nraw data have been cleaned\n"),file=FL.LOG,append=TRUE)


#
# save the cleaned data
#
## save(data.cleaned.wide,data.cleaned.long,data.pre.wide,data.pre.long,data.post.wide,data.post.long,lab.pre,lab.post,file = FL.OBJ.cleaned)
save(data.cleaned.wide,data.cleaned.long,lab.pre,lab.post,file = FL.OBJ.cleaned)
cat(paste("\n cleaned data are saved in R object file!",sep=""))
cat(paste("\n cleaned data are saved in R object file!",sep=""),file=FL.LOG,append=TRUE)

#
# save in csv
#
cat(paste("cleaned,",sep=""),file=FL.CSV.cleaned,append=TRUE)
write.table(data.cleaned.wide,file = FL.CSV.cleaned,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	
cat(paste("\n cleaned data are saved in csv format!\n",sep=""))
cat(paste("\n cleaned data are saved in csv format!\n",sep=""),file=FL.LOG,append=TRUE)


# ---------------------------------------------------------------------------------------------------
# time series of pre-installation
# ---------------------------------------------------------------------------------------------------
str.title = "raw cleaned"

FL.PDF <- paste(Path.out,paste("timeseries_pre.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- paste("timeseries_pre.pdf",sep="")
if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)
plot.obj <- xyplot(value ~ time.chron | as.factor(vari),data=data.pre.long,
		   xlab="date/time",ylab="",
		   type="l",lty=1,cex=0.5,col="red",
		   main=paste("pre-installation:"," (",str.title,")",sep=""),
		   scales=list(x=list(limits=xlim4plot_pre,tick.number = 5,at = xat4plot_pre),y="free"),
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
if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)
plot.obj <- xyplot(value ~ time.chron | as.factor(vari),data=data.post.long,
		   xlab="date/time",ylab="",
		   type="l",lty=1,cex=0.5,col="red",
		   main=paste("post-installation:"," (",str.title,")",sep=""),
		   scales=list(x=list(limits=xlim4plot_post,tick.number = 5,at = xat4plot_post),y="free"),
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
FL.PDF <- paste(Path.out,paste("timeseries_pre_post_inC.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- paste("timeseries_pre_post_inC.pdf",sep="")
if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)

for (lab in lab.post)
{
	if (length(grep("^T",lab)))
	{
		if (length(grep("^Tcw",lab)))
		{
			h.line <- c(4.44,10.0)
		}
		if (length(grep("^Thw",lab)))
		{
			h.line <- c(82.22,93.33)
		}
		plot.obj.pre <- xyplot(data.pre.wide[,lab] ~ data.pre.wide[,"time.chron"],
				       xlab="date/time",ylab=lab,
				       type="l",lty=1,cex=0.5,col="red",
				       main=paste("pre-installation: ",lab," (",str.title,")",sep=""),
				       scales=list(x=list(limits=xlim4plot_pre,tick.number = 5,at = xat4plot_pre),y="free"),

				       panel = function(x,y,...) {
					       panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="red")
					       panel.abline(h=0,lty=2,col="black")
					       panel.abline(v=c(period1.start,period1.end),lty=2,col="red")
					       panel.abline(v=c(period2.start,period2.end),lty=2,col="blue")
					       panel.abline(v=c(period3.start,period3.end),lty=2,col="green")
					       panel.abline(h=h.line,lty=2,col="green")
					       }
					)

		plot.obj.post<- xyplot(data.post.wide[,lab] ~ data.post.wide[,"time.chron"],
				       xlab="date/time",ylab=lab,
				       type="l",lty=1,cex=0.5,col="blue",
				       main=paste("post-installation: ",lab," (",str.title,")",sep=""),
				       scales=list(x=list(limits=xlim4plot_post,tick.number = 5,at = xat4plot_post),y="free"),

				       panel = function(x,y,...) {
					       panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="blue")
					       panel.abline(h=0,lty=2,col="black")
					       panel.abline(v=c(period1.start,period1.end),lty=2,col="red")
					       panel.abline(v=c(period2.start,period2.end),lty=2,col="blue")
					       panel.abline(v=c(period3.start,period3.end),lty=2,col="green")
					       panel.abline(h=h.line,lty=2,col="green")
					       }
					)
	}else{					

		plot.obj.pre <- xyplot(data.pre.wide[,lab] ~ data.pre.wide[,"time.chron"],
				       xlab="date/time",ylab=lab,
				       type="l",lty=1,cex=0.5,col="red",
				       main=paste("pre-installation: ",lab," (",str.title,")",sep=""),
				       scales=list(x=list(limits=xlim4plot_pre,tick.number = 5,at = xat4plot_pre),y="free"),

				       panel = function(x,y,...) {
					       panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="red")
					       panel.abline(h=0,lty=2,col="black")
					       panel.abline(v=c(period1.start,period1.end),lty=2,col="red")
					       panel.abline(v=c(period2.start,period2.end),lty=2,col="blue")
					       panel.abline(v=c(period3.start,period3.end),lty=2,col="green")
					       }
					)

		plot.obj.post<- xyplot(data.post.wide[,lab] ~ data.post.wide[,"time.chron"],
				       xlab="date/time",ylab=lab,
				       type="l",lty=1,cex=0.5,col="blue",
				       main=paste("post-installation: ",lab," (",str.title,")",sep=""),
				       scales=list(x=list(limits=xlim4plot_post,tick.number = 5,at = xat4plot_post),y="free"),

				       panel = function(x,y,...) {
					       panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="blue")
					       panel.abline(h=0,lty=2,col="black")
					       panel.abline(v=c(period1.start,period1.end),lty=2,col="red")
					       panel.abline(v=c(period2.start,period2.end),lty=2,col="blue")
					       panel.abline(v=c(period3.start,period3.end),lty=2,col="green")
					       }
					)
	}					

	plot(plot.obj.pre, split=c(1,1,1,2))
	plot(plot.obj.post,split=c(1,2,1,2),newpage=FALSE)
}	
dev.off()



# ---------------------------------------------------------------------------------------------------
# time series of pre- and post-installation
# ---------------------------------------------------------------------------------------------------	
FL.PDF <- paste(Path.out,paste("timeseries_pre_post_inF.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- paste("timeseries_pre_post_inF.pdf",sep="")
if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)

for (lab in lab.post)
{
	if (length(grep("^T",lab)))
	{
		if (length(grep("^Tcw",lab)))
		{
			h.line <- c(40,50)
		}
		if (length(grep("^Thw",lab)))
		{
			h.line <- c(180,200)
		}
		plot.obj.pre <- xyplot((data.pre.wide[,lab]*9/5+32) ~ data.pre.wide[,"time.chron"],
				       xlab="date/time",ylab=sub("\\(C\\)","(F)",lab),
				       type="l",lty=1,cex=0.5,col="red",
				       main=paste("pre-installation: ",sub("\\(C\\)","(F)",lab)," (",str.title,")",sep=""),
				       scales=list(x=list(limits=xlim4plot_pre,at = xat4plot_pre),y="free"),

				       panel = function(x,y,...) {
					       panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="red")
					       panel.abline(h=0,lty=2,col="black")
					       panel.abline(v=c(period1.start,period1.end),lty=2,col="red")
					       panel.abline(v=c(period2.start,period2.end),lty=2,col="blue")
					       panel.abline(v=c(period3.start,period3.end),lty=2,col="green")
					       panel.abline(h=h.line,lty=2,col="green")
					       }
					)


		plot.obj.post<- xyplot((data.post.wide[,lab]*9/5+32) ~ data.post.wide[,"time.chron"],
				       xlab="date/time",ylab=sub("\\(C\\)","(F)",lab),
				       type="l",lty=1,cex=0.5,col="blue",
				       main=paste("post-installation: ",sub("\\(C\\)","(F)",lab)," (",str.title,")",sep=""),
				       scales=list(x=list(limits=xlim4plot_post,at = xat4plot_post),y="free"),

				       panel = function(x,y,...) {
					       panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="blue")
					       panel.abline(h=0,lty=2,col="black")
					       panel.abline(v=c(period1.start,period1.end),lty=2,col="red")
					       panel.abline(v=c(period2.start,period2.end),lty=2,col="blue")
					       panel.abline(v=c(period3.start,period3.end),lty=2,col="green")
					       panel.abline(h=h.line,lty=2,col="green")
					       }
					)
	}else{
		plot.obj.pre <- xyplot(data.pre.wide[,lab] ~ data.pre.wide[,"time.chron"],
				       xlab="date/time",ylab=lab,
				       type="l",lty=1,cex=0.5,col="red",
				       main=paste("pre-installation: ",lab," (",str.title,")",sep=""),
				       scales=list(x=list(limits=xlim4plot_pre,at = xat4plot_pre),y="free"),

				       panel = function(x,y,...) {
					       panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="red")
					       panel.abline(h=0,lty=2,col="black")
					       panel.abline(v=c(period1.start,period1.end),lty=2,col="red")
					       panel.abline(v=c(period2.start,period2.end),lty=2,col="blue")
					       panel.abline(v=c(period3.start,period3.end),lty=2,col="green")
					       }
					)

		plot.obj.post<- xyplot(data.post.wide[,lab] ~ data.post.wide[,"time.chron"],
				       xlab="date/time",ylab=lab,
				       type="l",lty=1,cex=0.5,col="blue",
				       main=paste("post-installation: ",lab," (",str.title,")",sep=""),
				       scales=list(x=list(limits=xlim4plot_post,at = xat4plot_post),y="free"),

				       panel = function(x,y,...) {
					       panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="blue")
					       panel.abline(h=0,lty=2,col="black")
					       panel.abline(v=c(period1.start,period1.end),lty=2,col="red")
					       panel.abline(v=c(period2.start,period2.end),lty=2,col="blue")
					       panel.abline(v=c(period3.start,period3.end),lty=2,col="green")
					       }
					)	
	}
	
	plot(plot.obj.pre, split=c(1,1,1,2))
	plot(plot.obj.post,split=c(1,2,1,2),newpage=FALSE)
}	
dev.off()


# **************************************************************************************************************************************
# more plot for checking the Flow rate and Temperature data
# the concenring is those data with higher thannormal temperatures
# generally higher than normal T_cwr(C) corresponding to close to 0 flow rate, but the plots below do not fully support this!!!
# **************************************************************************************************************************************
###### FL.PDF <- paste(Path.out,paste("timeseries_Fcwr_Tcwr.pdf",sep=""),sep="/")	# OUTPUT Time Series 
###### TI.PDF <- paste("timeseries_Fcwr_Tcwr.pdf",sep="")
###### if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
###### pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)
###### 
###### 
###### 	# --------------------------------------
###### 	# Pre-installation
###### 	# --------------------------------------
###### 	# Fcwr_a and Tcwr_a
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsa(C)","Tcwra(C)","Fcwr_a(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwra(C)"],   lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_a(C)",  xlim=xlim4plot_pre,main="Tcwr_a Fcwr_a (pre)")
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="pre",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwsa(C)"],   lty=1,type="l",col="black",xlab="date/time",ylab="Tcws_a(C)",  xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                          "Fcwr_a(gpm)"],lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_a(gpm)",xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwra(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwra(C)"]>18,"Tcwra(C)"],   lty=1,type="h",col="blue", xlab="date/time",ylab="Tcwr_a(C)",  xlim=xlim4plot_pre)
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwsa(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwsa(C)"]>18,"Tcwsa(C)"],   lty=1,type="h",col="black",xlab="date/time",ylab="Tcws_a(C)",  xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwra(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwra(C)"]>18,"Fcwr_a(gpm)"],lty=1,type="h",col="red",  xlab="date/time",ylab="Fcwr_a(gpm)",xlim=xlim4plot_pre)
###### 
###### 
###### 	# Fcwr_b and Tcwr_b
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsb(C)","Tcwrb(C)","Fcwr_b(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwrb(C)"],   lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_b(C)",  xlim=xlim4plot_pre,main="Tcwr_b Fcwr_b (pre)")
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="pre",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwsb(C)"],   lty=1,type="l",col="black",xlab="date/time",ylab="Tcws_b(C)",  xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                          "Fcwr_b(gpm)"],lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_b(gpm)",xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwrb(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwrb(C)"]>18,"Tcwrb(C)"],   lty=1,type="h",col="blue", xlab="date/time",ylab="Tcwr_b(C)",  xlim=xlim4plot_pre)
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwsb(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwsb(C)"]>18,"Tcwsb(C)"],   lty=1,type="h",col="black",xlab="date/time",ylab="Tcws_b(C)",  xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwrb(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwrb(C)"]>18,"Fcwr_b(gpm)"],lty=1,type="h",col="red",  xlab="date/time",ylab="Fcwr_b(gpm)",xlim=xlim4plot_pre)
###### 
###### 
###### 
###### 	# Fcwr_c and Tcwr_c
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsc(C)","Tcwrc(C)","Fcwr_c(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwrc(C)"],   lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_c(C)",  xlim=xlim4plot_pre,main="Tcwr_c Fcwr_c (pre)")
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="pre",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwsc(C)"],   lty=1,type="l",col="black",xlab="date/time",ylab="Tcws_c(C)",  xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                          "Fcwr_c(gpm)"],lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_c(gpm)",xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwrc(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwrc(C)"]>18,"Tcwrc(C)"],   lty=1,type="h",col="blue", xlab="date/time",ylab="Tcwr_c(C)",  xlim=xlim4plot_pre)
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwsc(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwsc(C)"]>18,"Tcwsc(C)"],   lty=1,type="h",col="black",xlab="date/time",ylab="Tcws_c(C)",  xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwrc(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwrc(C)"]>18,"Fcwr_c(gpm)"],lty=1,type="h",col="red",  xlab="date/time",ylab="Fcwr_c(gpm)",xlim=xlim4plot_pre)
###### 
###### 
###### 	# Fcwr_b316 and Tcwr_b316
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcws_b316(C)","Tcwr_b316(C)","Fcwr_b316(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre",                              "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                              "Tcwr_b316(C)"],  lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_b316(C)",  xlim=xlim4plot_pre,main="Tcwr_b316 Fcwr_b316 (pre)")
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="pre",                              "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                              "Tcws_b316(C)"],  lty=1,type="l",col="black",xlab="date/time",ylab="Tcws_b316(C)",  xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre",                              "time.chron"],dat.tmp[dat.tmp[,"period"]=="pre",                              "Fcwr_b316(gpm)"],lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_b316(gpm)",xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwr_b316(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwr_b316(C)"]>18,"Tcwr_b316(C)"],  lty=1,type="h",col="blue", xlab="date/time",ylab="Tcwr_b316(C)",  xlim=xlim4plot_pre)
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcws_b316(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcws_b316(C)"]>18,"Tcws_b316(C)"],  lty=1,type="h",col="black",xlab="date/time",ylab="Tcws_b316(C)",  xlim=xlim4plot_pre)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwr_b316(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="pre" & dat.tmp[,"Tcwr_b316(C)"]>18,"Fcwr_b316(gpm)"],lty=1,type="h",col="red",  xlab="date/time",ylab="Fcwr_b316(gpm)",xlim=xlim4plot_pre)
###### 
###### 
###### 
###### 	# --------------------------------------
###### 	# post-installation
###### 	# --------------------------------------
###### 	# Fcwr_a and Tcwr_a
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsa(C)","Tcwra(C)","Fcwr_a(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwra(C)"],   lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_a(C)",  xlim=xlim4plot_post,main="Tcwr_a Fcwr_a (post)")
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="post",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwsa(C)"],   lty=1,type="l",col="black",xlab="date/time",ylab="Tcws_a(C)",  xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                          "Fcwr_a(gpm)"],lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_a(gpm)",xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwra(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwra(C)"]>18,"Tcwra(C)"],   lty=1,type="h",col="blue", xlab="date/time",ylab="Tcwr_a(C)",  xlim=xlim4plot_post)
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwsa(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwsa(C)"]>18,"Tcwsa(C)"],   lty=1,type="h",col="black",xlab="date/time",ylab="Tcws_a(C)",  xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwra(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwra(C)"]>18,"Fcwr_a(gpm)"],lty=1,type="h",col="red",  xlab="date/time",ylab="Fcwr_a(gpm)",xlim=xlim4plot_post)
###### 
###### 
###### 	# Fcwr_b and Tcwr_b
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsb(C)","Tcwrb(C)","Fcwr_b(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwrb(C)"],   lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_b(C)",  xlim=xlim4plot_post,main="Tcwr_b Fcwr_b (post)")
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="post",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwsb(C)"],   lty=1,type="l",col="black",xlab="date/time",ylab="Tcws_b(C)",  xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                          "Fcwr_b(gpm)"],lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_b(gpm)",xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwrb(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwrb(C)"]>18,"Tcwrb(C)"],   lty=1,type="h",col="blue", xlab="date/time",ylab="Tcwr_b(C)",  xlim=xlim4plot_post)
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwsb(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwsb(C)"]>18,"Tcwsb(C)"],   lty=1,type="h",col="black",xlab="date/time",ylab="Tcws_b(C)",  xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwrb(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwrb(C)"]>18,"Fcwr_b(gpm)"],lty=1,type="h",col="red",  xlab="date/time",ylab="Fcwr_b(gpm)",xlim=xlim4plot_post)
###### 
###### 
###### 
###### 	# Fcwr_c and Tcwr_c
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsc(C)","Tcwrc(C)","Fcwr_c(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwrc(C)"],   lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_c(C)",  xlim=xlim4plot_post,main="Tcwr_c Fcwr_c (post)")
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="post",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwsc(C)"],   lty=1,type="l",col="black",xlab="date/time",ylab="Tcws_c(C)",  xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post",                          "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                          "Fcwr_c(gpm)"],lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_c(gpm)",xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwrc(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwrc(C)"]>18,"Tcwrc(C)"],   lty=1,type="h",col="blue", xlab="date/time",ylab="Tcwr_c(C)",  xlim=xlim4plot_post)
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwsc(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwsc(C)"]>18,"Tcwsc(C)"],   lty=1,type="h",col="black",xlab="date/time",ylab="Tcws_c(C)",  xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwrc(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwrc(C)"]>18,"Fcwr_c(gpm)"],lty=1,type="h",col="red",  xlab="date/time",ylab="Fcwr_c(gpm)",xlim=xlim4plot_post)
###### 
###### 
###### 	# Fcwr_b316 and Tcwr_b316
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcws_b316(C)","Tcwr_b316(C)","Fcwr_b316(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post",                              "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                              "Tcwr_b316(C)"],  lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_b316(C)",  xlim=xlim4plot_post,main="Tcwr_b316 Fcwr_b316 (post)")
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="post",                              "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                              "Tcws_b316(C)"],  lty=1,type="l",col="black",xlab="date/time",ylab="Tcws_b316(C)",  xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post",                              "time.chron"],dat.tmp[dat.tmp[,"period"]=="post",                              "Fcwr_b316(gpm)"],lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_b316(gpm)",xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwr_b316(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwr_b316(C)"]>18,"Tcwr_b316(C)"],  lty=1,type="h",col="blue", xlab="date/time",ylab="Tcwr_b316(C)",  xlim=xlim4plot_post)
###### 	#lines(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcws_b316(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcws_b316(C)"]>18,"Tcws_b316(C)"],  lty=1,type="h",col="black",xlab="date/time",ylab="Tcws_b316(C)",  xlim=xlim4plot_post)
###### 	  plot(dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwr_b316(C)"]>18,"time.chron"],dat.tmp[dat.tmp[,"period"]=="post" & dat.tmp[,"Tcwr_b316(C)"]>18,"Fcwr_b316(gpm)"],lty=1,type="h",col="red",  xlab="date/time",ylab="Fcwr_b316(gpm)",xlim=xlim4plot_post)
###### 	
###### 	dev.off()
###### 	
###### 
###### 
###### 
###### # **************************************************************************************************************************************
###### # more plot for checking the Flow rate and Temperature data
###### # the concenring is those data with higher thannormal temperatures
###### # if the difference of the return and supply temperatures are close to 0 for those higher temperature point, that still be OK to not deleted all those data with higher than normal temperatures
###### # the plots below do not support it
###### # **************************************************************************************************************************************
###### FL.PDF <- paste(Path.out,paste("timeseries_Tcwr_Tcws.pdf",sep=""),sep="/")	# OUTPUT Time Series 
###### TI.PDF <- paste("timeseries_Fcwr_Tcwr.pdf",sep="")
###### if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
###### pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)
###### 
###### 	# --------------------------------------
###### 	# Pre-installation
###### 	# --------------------------------------
###### 	# Fcwr_a and Tcwr_a
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsa(C)","Tcwra(C)","Fcwr_a(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwra(C)"],                          lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_a(C)",  xlim=xlim4plot_pre,main="Tcwr_a Fcwr_a (pre)")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwsa(C)"],                          lty=1,type="l",col="pink", xlab="date/time",ylab="Tcws_a(C)",  xlim=xlim4plot_pre)
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"],(dat.tmp[dat.tmp[,"period"]=="pre","Tcwra(C)"] - dat.tmp[dat.tmp[,"period"]=="pre","Tcwsa(C)"]),   lty=1,type="l",col="black",xlab="date/time",ylab="deltaT_a(C)",xlim=xlim4plot_pre)
###### 	abline(h=0,col="red")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Fcwr_a(gpm)"],                       lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_a(gpm)",xlim=xlim4plot_pre)
###### 
###### 	# Fcwr_b and Tcwr_b
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsb(C)","Tcwrb(C)","Fcwr_b(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwrb(C)"],                          lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_b(C)",  xlim=xlim4plot_pre,main="Tcwr_b Fcwr_b (pre)")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwsb(C)"],                          lty=1,type="l",col="pink", xlab="date/time",ylab="Tcws_b(C)",  xlim=xlim4plot_pre)
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"],(dat.tmp[dat.tmp[,"period"]=="pre","Tcwrb(C)"] - dat.tmp[dat.tmp[,"period"]=="pre","Tcwsb(C)"]),   lty=1,type="l",col="black",xlab="date/time",ylab="deltaT_b(C)",xlim=xlim4plot_pre)
###### 	abline(h=0,col="red")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Fcwr_b(gpm)"],                       lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_b(gpm)",xlim=xlim4plot_pre)
###### 
###### 
###### 	# Fcwr_c and Tcwr_c
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsc(C)","Tcwrc(C)","Fcwr_c(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwrc(C)"],                          lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_c(C)",  xlim=xlim4plot_pre,main="Tcwr_c Fcwr_c (pre)")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwsc(C)"],                          lty=1,type="l",col="pink", xlab="date/time",ylab="Tcws_c(C)",  xlim=xlim4plot_pre)
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"],(dat.tmp[dat.tmp[,"period"]=="pre","Tcwrc(C)"] - dat.tmp[dat.tmp[,"period"]=="pre","Tcwsc(C)"]),   lty=1,type="l",col="black",xlab="date/time",ylab="deltaT_c(C)",xlim=xlim4plot_pre)
###### 	abline(h=0,col="red")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Fcwr_c(gpm)"],                       lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_c(gpm)",xlim=xlim4plot_pre)
###### 
###### 	# Fcwr_b316 and Tcwr_b316
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcws_b316(C)","Tcwr_b316(C)","Fcwr_b316(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcwr_b316(C)"],                          lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_b316(C)",  xlim=xlim4plot_pre,main="Tcwr_b316 Fcwr_b316 (pre)")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Tcws_b316(C)"],                          lty=1,type="l",col="pink", xlab="date/time",ylab="Tcws_b316(C)",  xlim=xlim4plot_pre)
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"],(dat.tmp[dat.tmp[,"period"]=="pre","Tcwr_b316(C)"] - dat.tmp[dat.tmp[,"period"]=="pre","Tcws_b316(C)"]),   lty=1,type="l",col="black",xlab="date/time",ylab="deltaT_b316(C)",xlim=xlim4plot_pre)
###### 	abline(h=0,col="red")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="pre","time.chron"], dat.tmp[dat.tmp[,"period"]=="pre",                          "Fcwr_b316(gpm)"],                       lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_b316(gpm)",xlim=xlim4plot_pre)
###### 
###### 
###### 	# --------------------------------------
###### 	# post-installation
###### 	# --------------------------------------
###### 	# Fcwr_a and Tcwr_a
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsa(C)","Tcwra(C)","Fcwr_a(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwra(C)"],                          lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_a(C)",  xlim=xlim4plot_post,main="Tcwr_a Fcwr_a (post)")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwsa(C)"],                          lty=1,type="l",col="pink", xlab="date/time",ylab="Tcws_a(C)",  xlim=xlim4plot_post)
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"],(dat.tmp[dat.tmp[,"period"]=="post","Tcwra(C)"] - dat.tmp[dat.tmp[,"period"]=="post","Tcwsa(C)"]),   lty=1,type="l",col="black",xlab="date/time",ylab="deltaT_a(C)",xlim=xlim4plot_post)
###### 	abline(h=0,col="red")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Fcwr_a(gpm)"],                       lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_a(gpm)",xlim=xlim4plot_post)
###### 
###### 	# Fcwr_b and Tcwr_b
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsb(C)","Tcwrb(C)","Fcwr_b(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwrb(C)"],                          lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_b(C)",  xlim=xlim4plot_post,main="Tcwr_b Fcwr_b (post)")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwsb(C)"],                          lty=1,type="l",col="pink", xlab="date/time",ylab="Tcws_b(C)",  xlim=xlim4plot_post)
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"],(dat.tmp[dat.tmp[,"period"]=="post","Tcwrb(C)"] - dat.tmp[dat.tmp[,"period"]=="post","Tcwsb(C)"]),   lty=1,type="l",col="black",xlab="date/time",ylab="deltaT_b(C)",xlim=xlim4plot_post)
###### 	abline(h=0,col="red")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Fcwr_b(gpm)"],                       lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_b(gpm)",xlim=xlim4plot_post)
###### 
###### 
###### 	# Fcwr_c and Tcwr_c
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcwsc(C)","Tcwrc(C)","Fcwr_c(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwrc(C)"],                          lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_c(C)",  xlim=xlim4plot_post,main="Tcwr_c Fcwr_c (post)")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwsc(C)"],                          lty=1,type="l",col="pink", xlab="date/time",ylab="Tcws_c(C)",  xlim=xlim4plot_post)
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"],(dat.tmp[dat.tmp[,"period"]=="post","Tcwrc(C)"] - dat.tmp[dat.tmp[,"period"]=="post","Tcwsc(C)"]),   lty=1,type="l",col="black",xlab="date/time",ylab="deltaT_c(C)",xlim=xlim4plot_post)
###### 	abline(h=0,col="red")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Fcwr_c(gpm)"],                       lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_c(gpm)",xlim=xlim4plot_post)
###### 
###### 	# Fcwr_b316 and Tcwr_b316
###### 	dat.tmp <- data.cleaned.wide[,c("time.chron","period","Tcws_b316(C)","Tcwr_b316(C)","Fcwr_b316(gpm)")]
###### 	par(mfrow = c(4, 1))
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Tcwr_b316(C)"],                          lty=1,type="l",col="blue", xlab="date/time",ylab="Tcwr_b316(C)",  xlim=xlim4plot_post,main="Tcwr_b316 Fcwr_b316 (post)")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Tcws_b316(C)"],                          lty=1,type="l",col="pink", xlab="date/time",ylab="Tcws_b316(C)",  xlim=xlim4plot_post)
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"],(dat.tmp[dat.tmp[,"period"]=="post","Tcwr_b316(C)"] - dat.tmp[dat.tmp[,"period"]=="post","Tcws_b316(C)"]),   lty=1,type="l",col="black",xlab="date/time",ylab="deltaT_b316(C)",xlim=xlim4plot_post)
###### 	abline(h=0,col="red")
###### 	plot(dat.tmp[dat.tmp[,"period"]=="post","time.chron"], dat.tmp[dat.tmp[,"period"]=="post",                          "Fcwr_b316(gpm)"],                       lty=1,type="l",col="red",  xlab="date/time",ylab="Fcwr_b316(gpm)",xlim=xlim4plot_post)
###### 
###### dev.off()

# ---------------------------------------------------------------------------------------------------
# 40. time used for completing this script
# ---------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n3_clean_monitoringData.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n3_clean_monitoringData.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)


cat(paste("Processing time for [3_clean_monitoringData.R] is",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)

