#
# 9a_gasDataProcess.R (CST time which is CDT + 1)  (or CDT = CST - 1)
#
# Note: the summary calculated here includes the data from October.  So
# new summary was calculated by "31_O3Analysis"
# June 15, 2009
#
#
# INPUT:
# "../../DataReceived/dataReceived_0915_2008_A"				
# "../../DataReceived/dataReceived_0915_2008_A/5mindata/5mindata"				Aldine and Bayland
# "../../DataReceived/dataReceived_0923_2008_DeerPark/deer_parkc18_c35/deer_parkc18_c35"	DeerPark
#
# OUTPUT:
# "../9a_gasDataProcess"
# "DataGaseous.Rdata"
# "DataGaseous_wide.CSV"
# "DataGaseous_long.CSV"
#
# Time System: LST which is the same CST (Shelly Thomas Sep 23, 2008)
# 
# NOTE: CO in ppm and the other in ppb
#       so convert CO from ppm to ppb by multiplying 1000
# This change is made on Oct 28, 2008 
#
# Processing the DAT and ERR data
# Create a Rdata file when the empty sample and empty mz removed data
# 
# CAMS station ID:
# 8:  Aldine
# 53: Bayland
# 18: Deer Park
#
# Created  on September 18 (for the first batch of data received for ALdine and Bayland)
# Modified on September 23, 2008 (after received data at Deer Park from Shelly and CO data from C408 & C411 as surogate for Bayland)
# Note:
# Data received on September 18 has one day more data (24 * 12=288)
#
# Date/Time system for gaseous data is:LST which is the same CST (Shelly Thomas Sep 23, 2008)
# UTC = CST + 6
# UTC = CDT + 5 in Summer
# UTC = CDT + 6 in winter
# So our data (in Summer) CST = CDT-1 or CDT = CST + 1)
# Checked on Sep 25, 2008
# Revised on Oct 12, 2008
# Oct 13, 2008: (1) add CST in the time axis; (2) change summary file name; (3) make corr matrix upper triangle
# Revised on Oct 17 to (a) output PDF instead of PS
#                      (b) add prefix to all output file like "PTRMS", "autoGS","Gaseous" to distinct the data source
# Revised on Nov 4, 2008
# 
# November 13, 2008
# Re-organize the file structure:
# up to this date are based on data with negative values retained.
# all analyses up to this date are saved in the directory of "\Analysis_withNegativeRetained\"
#
# A new folder "\Analysis_withNegativeRemoved\" is created on this date to accomodate the analysis when negative values are deleted!!
# -------------------------------------------------------------------------
#

# 
# 0. eliminate all stuff
# 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# 0. correlation coeeficient cutoff
corr.cut <- 0.7

# 
# 1. load libraries
# 
library(lattice)
library(chron)	

# date/time information
time.info <- "LST which is CST: according to Shelly Sep 23, 2008 email.  UTC=CST+6, UTC=CDT+5 in Summer, UTC=CDT+6 in Winter"


# 
# 2. setup plotting limits for x axis
# 
xlim4plot <- c(chron(dates="9/1/2006", times="0:0:0",format=c('m/d/y','h:m:s')),chron(dates="10/1/2006",times="23:55:0",format=c('m/d/y','h:m:s')))
sites     <- c("Aldine","Bayland","DeerPark")
gases     <- c("CO","NOx","NOy","O3")

# 
# 3. change to the script directory
# 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_withNegativeRemoved/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_withNegativeRemoved/0_scripts"
}
setwd(Path.Current)


# ---------------------------------------------------------------------------------------------------
# 4. define a summary function
# ---------------------------------------------------------------------------------------------------
source(paste(Path.Current,"my_functions.R",sep="/"))

# --------------------------------------------------------------------------------------------------- 
# 5. setup output and log directory
# --------------------------------------------------------------------------------------------------- 
Path.out   <- "../9a_gasDataProcess"	# OUTPUT processed result directory
Path.log   <- "../0_log"		# OUTPUT log  directory

if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# 
# 6. create a LOG file and a TIME Recording file
# 
FL.TIME    <- paste(Path.log,"time.log",sep="/")		# OUTPUT Time Log file for all scripts
FL.LOG     <- paste(Path.log,"9a_gasDataProcess.log",sep="/")	# OUTPUT Log file

if (file.exists(FL.LOG)) {print(paste(FL.LOG, " exist. Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for processing gaseous data at Aldine (AD), Bayland (BL) and DeerPark (DP)!\n",sep=""),file=FL.LOG, append=TRUE)

# --------------------------------------------------------------------------------------------------- 
# 7.define OUTPUT files
# ---------------------------------------------------------------------------------------------------
# create a R object for output
FL.OBJ      <- paste(Path.out,paste("DataGaseous.Rdata",sep=""),sep="/")	# OUTPUT Rdata
FL.CSV.wide <- paste(Path.out,paste("DataGaseous_wide.CSV",sep=""),sep="/")	# OUTPUT CSV
FL.CSV.long <- paste(Path.out,paste("DataGaseous_long.CSV",sep=""),sep="/")	# OUTPUT CSV
FL.SUM      <- paste(Path.out,paste("Gaseous_Summary.csv",sep=""),sep="/")	# OUTPUT Summary 
FL.CORR     <- paste(Path.out,paste("Gaseous_CorrCoef.csv",sep=""),sep="/")	# OUTPUT Time Series 
FL.CORR.pdf <- paste(Path.out,paste("Gaseous_CorrCoef.pdf",sep=""),sep="/")	# OUTPUT Scatter Plot in PS file
TI.CORR.pdf <- "Gaseous_CorrCoef.pdf"						# OUTPUT Scatter Plot in PS file

if (file.exists(FL.OBJ)){print(paste(FL.OBJ," exist. Delete it!"));file.remove(FL.OBJ)}			# remove existing OUTPUT files
if (file.exists(FL.CSV.wide)){print(paste(FL.CSV.wide," exist. Delete it!"));file.remove(FL.CSV.wide)}	# remove existing OUTPUT files
if (file.exists(FL.CSV.long)){print(paste(FL.CSV.long," exist. Delete it!"));file.remove(FL.CSV.long)}	# remove existing OUTPUT files
if (file.exists(FL.SUM)){print(paste(FL.SUM," exist. Delete it!"));file.remove(FL.SUM)}			# remove existing OUTPUT files
if (file.exists(FL.CORR)){print(paste(FL.CORR," exist. Delete it!"));file.remove(FL.CORR)}			# remove existing OUTPUT files
if (file.exists(FL.CORR.pdf)){print(paste(FL.CORR.pdf," exist. Delete it!"));file.remove(FL.CORR.pdf)}			# remove existing OUTPUT files
cat(paste("\nDefined an output R object file!\n",sep=""),file=FL.LOG, append=TRUE)


# ---------------------------------------------------------------------------------------------------
# 8. Path and file of ERROR CODE in data files
# ---------------------------------------------------------------------------------------------------
Path.code  <- "../../DataReceived/dataReceived_0915_2008_A"
FL.ErrCode <- paste(Path.code,"CAMS_BAD_VALUES.csv",sep="/")
Error.Code <- read.table(FL.ErrCode,sep=",", header=TRUE,blank.lines.skip = FALSE)		# 

badConc.code <- Error.Code[,"TITLE"]
badConc.code <- as.character(badConc.code[-1])	# remove the 1st which is NA already
cat(paste("\nHave read in the error conc code file:",FL.ErrCode,"\n",sep=""),file=FL.LOG, append=TRUE)



# ---------------------------------------------------------------------------------------------------
# 11. Path and files of gaseous data at Aldine (AD) and Bayland (BL) received on September 15, 2008
# ---------------------------------------------------------------------------------------------------
Path.in.AD_BL  <- "../../DataReceived/dataReceived_0915_2008_A/5mindata/5mindata"				# INPUT  Folder of Bayland Park
if (!file.exists(Path.in.AD_BL)){stop(paste(" gaseous data folder at Aldine and Bayland does NOT existing\n",sep=""))}
FL.CO.AD     <- paste(Path.in.AD_BL, "CO_ylx.csv", sep="/")		# INPUT DAT file 
FL.NOx.AD_BL <- paste(Path.in.AD_BL, "NOx_ylx.csv",sep="/")		# INPUT DAT file 
FL.NOy.AD_BL <- paste(Path.in.AD_BL, "NOy_ylx.csv",sep="/")		# INPUT DAT file 
FL.O3.AD_BL  <- paste(Path.in.AD_BL, "O3_ylx.csv", sep="/")		# INPUT DAT file 
cat(paste("\nDefined the INPUT data files at Aldine (AD), Bayland (BL)!\n",sep=""),file=FL.LOG, append=TRUE)



# --------------------------------------------------------------------------------------------------- 
# 12a. read gaseous data from data files and add names at Aldine (AD) and Bayland (BL)
# --------------------------------------------------------------------------------------------------- 
data.CO.AD     <- read.table(FL.CO.AD,    sep=",",header=TRUE,blank.lines.skip = TRUE,na.strings=badConc.code)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files

# convert CO from ppm to ppb
data.CO.AD[,"C8_CO"] <- data.CO.AD[,"C8_CO"] * 1000

# the other gases are in the units of ppb already
data.NOx.AD_BL <- read.table(FL.NOx.AD_BL,sep=",",header=TRUE,blank.lines.skip = TRUE,na.strings=badConc.code)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files
data.NOy.AD_BL <- read.table(FL.NOy.AD_BL,sep=",",header=TRUE,blank.lines.skip = TRUE,na.strings=badConc.code)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files
data.O3.AD_BL  <- read.table(FL.O3.AD_BL, sep=",",header=TRUE,blank.lines.skip = TRUE,na.strings=badConc.code)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files

# -------------------------------------------------------------------------------------------------
# delete negative values in the numeric fields (Nov 13, 2008)
#
for (idx.field in names(data.CO.AD))
{
	if(is.numeric(data.CO.AD[,idx.field]))
	{
		data.CO.AD[!is.na(data.CO.AD[,idx.field]) & data.CO.AD[,idx.field]<0 ,idx.field] <- NA
	}
}

#
# delete negative values in the numeric fields (Nov 13, 2008)
#
for (idx.field in names(data.NOx.AD_BL))
{
	if(is.numeric(data.NOx.AD_BL[,idx.field]))
	{
		data.NOx.AD_BL[!is.na(data.NOx.AD_BL[,idx.field]) & data.NOx.AD_BL[,idx.field]<0 ,idx.field] <- NA
	}
}

#
# delete negative values in the numeric fields (Nov 13, 2008)
#
for (idx.field in names(data.NOy.AD_BL))
{
	if(is.numeric(data.NOy.AD_BL[,idx.field]))
	{
		data.NOy.AD_BL[!is.na(data.NOy.AD_BL[,idx.field]) & data.NOy.AD_BL[,idx.field]<0 ,idx.field] <- NA
	}
}

#
# delete negative values in the numeric fields (Nov 13, 2008)
#
for (idx.field in names(data.O3.AD_BL))
{
	if(is.numeric(data.O3.AD_BL[,idx.field]))
	{
		data.O3.AD_BL[!is.na(data.O3.AD_BL[,idx.field]) & data.O3.AD_BL[,idx.field]<0 ,idx.field] <- NA
	}
}
# -------------------------------------------------------------------------------------------------


cat(paste("\nHave read in the following gaseous data files at Aldine(AD) and Bayland(BL) (note: error codes are replaced by NA):
                  \n\t",FL.CO.AD,
                 "\n\t",FL.NOx.AD_BL,
                 "\n\t",FL.NOy.AD_BL,
                 "\n\t",FL.O3.AD_BL,"\n",sep=""),file=FL.LOG, append=TRUE)


# 12b. add field names according to the input file
names(data.CO.AD)     <- c("dates","times","CO.Aldine")			# consist CO  at C8 (Aldine)
names(data.NOx.AD_BL) <- c("dates","times","NOx.Aldine","NOx.Bayland")	# consist NOx at C8 (Aldine) and C53 (Bayland)
names(data.NOy.AD_BL) <- c("dates","times","NOy.Aldine","NOy.Bayland")	# consist NOy at C8 (Aldine) and C53 (Bayland)
names(data.O3.AD_BL)  <- c("dates","times","O3.Aldine", "O3.Bayland")	# consist  O3 at C8 (Aldine) and C53 (Bayland)
cat(paste("\nHave added proper names to the columns of the data read in from Aldine(AD) and Bayland(BL).\n",sep=""),file=FL.LOG, append=TRUE)

# 12c. check if the date/time of these 4 files are the same within this set of data files
if(!setequal(data.CO.AD[,    "dates"],data.NOx.AD_BL[,"dates"])){stop(paste(" date stamps among the Aldine/Bayland files differ\n",sep=""))}
if(!setequal(data.NOx.AD_BL[,"dates"],data.NOy.AD_BL[,"dates"])){stop(paste(" date stamps among the Aldine/Bayland files differ\n",sep=""))}
if(!setequal(data.NOy.AD_BL[,"dates"],data.O3.AD_BL[, "dates"])){stop(paste(" date stamps among the Aldine/Bayland files differ\n",sep=""))}
if(!setequal(data.CO.AD[,    "times"],data.NOx.AD_BL[,"times"])){stop(paste(" time stamps among the Aldine/Bayland files differ\n",sep=""))}	
if(!setequal(data.NOx.AD_BL[,"times"],data.NOy.AD_BL[,"times"])){stop(paste(" time stamps among the Aldine/Bayland files differ\n",sep=""))}
if(!setequal(data.NOy.AD_BL[,"times"],data.O3.AD_BL[, "times"])){stop(paste(" time stamps among the Aldine/Bayland files differ\n",sep=""))}
cat(paste("\nThe consistency of the date/time stamps of the gaseous files of the batch of files (Aldine & Bayland) have been checked.\n",sep=""),file=FL.LOG, append=TRUE)

# 12d. split the date into year, month and day
year      <- as.numeric(substr(data.CO.AD[,"dates"],1,4))
month     <- as.numeric(substr(data.CO.AD[,"dates"],5,6))
day       <- as.numeric(substr(data.CO.AD[,"dates"],7,8))
dates     <- paste(month,day,year,sep="/")

# 12e. split the time as hour and minute (note: there is no seconds)
TS.HMS    <- unlist(strsplit(as.character(data.CO.AD[,"times"]),":"))	# time is separated by ":"
hour      <- as.numeric(TS.HMS[seq(from=1,to=length(TS.HMS),by=2)])
minute    <- as.numeric(TS.HMS[seq(from=2,to=length(TS.HMS),by=2)])
times     <- paste(hour,minute,(rep(0,length(minute))),sep=":")			# add 0:00 as seconds
hourInDay <- hour+minute/60							# for diurnal pattern plotting
hour_0901 <- ifelse(month==9,(day-1)*24+hourInDay,(day+29)*24+hourInDay)	# for those sep days, the hours to the 0:00 of sep 01 is: {(day-1)+hours/24}.  Starting from the second day, add 24 hour each day
cat(paste("\nThe dates/times field have been splitted and processsed.\n",sep=""),file=FL.LOG, append=TRUE)


# ---------------------------------------------------------------------------------------------------
# 13. Path of gaseous data at DeerPark 
# ---------------------------------------------------------------------------------------------------
Path.in.DP  <- "../../DataReceived/dataReceived_0923_2008_DeerPark/deer_parkc18_c35/deer_parkc18_c35"				# INPUT  Folder of Bayland Park
if (!file.exists(Path.in.DP)){stop(paste(" gaseous data folder at DeerPark does NOT existing\n",sep=""))}
FL.CO.DP  <- paste(Path.in.DP, "C35_CO_ylx.csv",       sep="/")		# INPUT DAT file 
FL.NOx.DP <- paste(Path.in.DP, "C35_NOx_ylx.csv",      sep="/")		# INPUT DAT file 
FL.NOy.DP <- paste(Path.in.DP, "C35_NOy_ylx.csv",      sep="/")		# INPUT DAT file 
FL.O3.DP  <- paste(Path.in.DP, "C35_O3_ylx.csv",       sep="/")		# INPUT DAT file 
FL.CO.BL  <- paste(Path.in.DP, "C408_C411_CO_ylx.csv", sep="/")		# INPUT DAT file 
cat(paste("\nDefined the INPUT data files at DeerPark(DP)!\n",sep=""),file=FL.LOG, append=TRUE)



# --------------------------------------------------------------------------------------------------- 
# 14a. read gaseous data from data files and add names at DeerPark (AD) and Bayland (BL)
# ---------------------------------------------------------------------------------------------------
data.CO.DP  <- read.table(FL.CO.DP, sep=",",header=TRUE,blank.lines.skip = TRUE,na.strings=badConc.code)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files
data.CO.BL  <- read.table(FL.CO.BL, sep=",",header=TRUE,blank.lines.skip = TRUE,na.strings=badConc.code)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files

# convert CO from ppm to ppb
data.CO.DP[,"C35_CO"]  <- data.CO.DP[,"C35_CO"]  * 1000
data.CO.BL[,"C408_CO"] <- data.CO.BL[,"C408_CO"] * 1000
data.CO.BL[,"C411_CO"] <- data.CO.BL[,"C411_CO"] * 1000

# the other gases are in the units of ppb already
data.NOx.DP <- read.table(FL.NOx.DP,sep=",",header=TRUE,blank.lines.skip = TRUE,na.strings=badConc.code)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files
data.NOy.DP <- read.table(FL.NOy.DP,sep=",",header=TRUE,blank.lines.skip = TRUE,na.strings=badConc.code)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files
data.O3.DP  <- read.table(FL.O3.DP, sep=",",header=TRUE,blank.lines.skip = TRUE,na.strings=badConc.code)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files

# -------------------------------------------------------------------------------------------------
# delete negative values in the numeric fields (Nov 13, 2008)
#
for (idx.field in names(data.CO.DP))
{
	if(is.numeric(data.CO.DP[,idx.field]))
	{
		data.CO.DP[!is.na(data.CO.DP[,idx.field]) & data.CO.DP[,idx.field]<0 ,idx.field] <- NA
	}
}

#
# delete negative values in the numeric fields (Nov 13, 2008)
#
for (idx.field in names(data.CO.BL))
{
	if(is.numeric(data.CO.BL[,idx.field]))
	{
		data.CO.BL[!is.na(data.CO.BL[,idx.field]) & data.CO.BL[,idx.field]<0 ,idx.field] <- NA
	}
}

#
# delete negative values in the numeric fields (Nov 13, 2008)
#
for (idx.field in names(data.NOx.DP))
{
	if(is.numeric(data.NOx.DP[,idx.field]))
	{
		data.NOx.DP[!is.na(data.NOx.DP[,idx.field]) & data.NOx.DP[,idx.field]<0 ,idx.field] <- NA
	}
}

#
# delete negative values in the numeric fields (Nov 13, 2008)
#
for (idx.field in names(data.NOy.DP))
{
	if(is.numeric(data.NOy.DP[,idx.field]))
	{
		data.NOy.DP[!is.na(data.NOy.DP[,idx.field]) & data.NOy.DP[,idx.field]<0 ,idx.field] <- NA
	}
}

#
# delete negative values in the numeric fields (Nov 13, 2008)
#
for (idx.field in names(data.O3.DP))
{
	if(is.numeric(data.O3.DP[,idx.field]))
	{
		data.O3.DP[!is.na(data.O3.DP[,idx.field]) & data.O3.DP[,idx.field]<0 ,idx.field] <- NA
	}
}
# -------------------------------------------------------------------------------------------------


cat(paste("\nHave read in the following gaseous data files at DeerPark (DP) (note: error codes are replaced by NA):
                  \n\t",FL.CO.DP,
                 "\n\t",FL.CO.BL,
                 "\n\t",FL.NOx.DP,
                 "\n\t",FL.NOy.DP,
                 "\n\t",FL.O3.DP,"\n",sep=""),file=FL.LOG, append=TRUE)


# 14b. add field names according to the input file
names(data.CO.DP)  <- c("dates","times","CO.DeerPark")		# consist CO  at C35 (DeerPark)
names(data.CO.BL)  <- c("dates","times","CO.Lang","CO.Texas")	# consist CO  at C408 (Lang) and C411 (Texas)
names(data.NOx.DP) <- c("dates","times","NOx.DeerPark")		# consist NOx at C35 (DeerPark)
names(data.NOy.DP) <- c("dates","times","NOy.DeerPark")		# consist NOy at C35 (DeerPark)
names(data.O3.DP)  <- c("dates","times","O3.DeerPark")		# consist O3  at C35 (DeerPark)
cat(paste("\nHave added proper names to the columns of the data read in from DeerPark (DP).\n",sep=""),file=FL.LOG, append=TRUE)



#
# 14c. append the DeerPark data with one more day (288 5 minute data) to be consistent with Aldnine and Bayland
# NOTE: Hard Coded the 288 5 minutes date/time for Oct 1    ----------                   
#                                           
data.CO.DP  <- merge(data.CO.DP, data.frame(dates=data.CO.AD[8641:8928,"dates"],times=data.CO.AD[8641:8928,"times"],"CO.DeerPark"  = as.numeric(rep(NA,288))),all=TRUE) 
data.CO.BL  <- merge(data.CO.BL, data.frame(dates=data.CO.AD[8641:8928,"dates"],times=data.CO.AD[8641:8928,"times"],"CO.Lang"      = rep(NA,288),"CO.Texas" = rep(NA,288)),all=TRUE) 
data.NOx.DP <- merge(data.NOx.DP,data.frame(dates=data.CO.AD[8641:8928,"dates"],times=data.CO.AD[8641:8928,"times"],"NOx.DeerPark" = rep(NA,288)),all=TRUE) 
data.NOy.DP <- merge(data.NOy.DP,data.frame(dates=data.CO.AD[8641:8928,"dates"],times=data.CO.AD[8641:8928,"times"],"NOy.DeerPark" = rep(NA,288)),all=TRUE) 
data.O3.DP  <- merge(data.O3.DP, data.frame(dates=data.CO.AD[8641:8928,"dates"],times=data.CO.AD[8641:8928,"times"],"O3.DeerPark"  = rep(NA,288)),all=TRUE)                                           




# 14d. check if the date/time of these 4 files are the same within this set of data files
if(!setequal(data.CO.DP[, "dates"],data.NOx.DP[,"dates"])){stop(paste(" date stamps among the DeerPark files differ\n",sep=""))}
if(!setequal(data.NOx.DP[,"dates"],data.NOy.DP[,"dates"])){stop(paste(" date stamps among the DeerPark files differ\n",sep=""))}
if(!setequal(data.NOy.DP[,"dates"],data.O3.DP[, "dates"])){stop(paste(" date stamps among the DeerPark files differ\n",sep=""))}
if(!setequal(data.O3.DP[, "dates"],data.CO.BL[, "dates"])){stop(paste(" date stamps among the DeerPark files differ\n",sep=""))}

if(!setequal(data.CO.DP[, "times"],data.NOx.DP[,"times"])){stop(paste(" time stamps among the DeerPark files differ\n",sep=""))}	
if(!setequal(data.NOx.DP[,"times"],data.NOy.DP[,"times"])){stop(paste(" time stamps among the DeerPark files differ\n",sep=""))}
if(!setequal(data.NOy.DP[,"times"],data.O3.DP[, "times"])){stop(paste(" time stamps among the DeerPark files differ\n",sep=""))}
if(!setequal(data.O3.DP[, "times"],data.CO.BL[, "times"])){stop(paste(" date stamps among the DeerPark files differ\n",sep=""))}
cat(paste("\nThe consistency of the date/time stamps of the gaseous files of the batch of files (DeerPark) have been checked.\n",sep=""),file=FL.LOG, append=TRUE)

# This is why we need to append 288 data (one more day) for DeerPark in order to compare with Bayland and Aldine
# compare the dates/times fileds of the two batches of data files (one on sep 15 for Aldine/Bayland) and the othert on Set 23 for DeerPark
if(!setequal(data.O3.DP[, "dates"],data.O3.AD_BL[, "dates"])){stop(paste(" date stamps between the two batches (AD/BL vs DP) of files differ\n",sep=""))}
if(!setequal(data.O3.DP[, "times"],data.O3.AD_BL[, "times"])){stop(paste(" date stamps between the two batches (AD/BL vs DP) of files differ\n",sep=""))}
cat(paste("\nThe consistency of the date/time stamps of the gaseous files of between the (Aldine/Bayland) batch and the (DeerPark) batch have been checked.\n",sep=""),file=FL.LOG, append=TRUE)



# --------------------------------------------------------------------------------------------------- 
# 15a. combine all gaseous species into a single data frame
# --------------------------------------------------------------------------------------------------- 
data.gas.wide <- data.frame(dates        = dates,
			    times        = times,
			    year         = year,
			    month        = month,
			    day          = day,
			    hour         = hour,
			    minute       = minute,
			    second       = rep(0,length(minute)),
			    hourInDay    = hourInDay,	# hour in the day for diurnal pattern plotting
			    hour_0901    = hour_0901,	# hour from the beginning of September 2006
			     CO.Aldine   = data.CO.AD[,     "CO.Aldine"],
			    NOx.Aldine   = data.NOx.AD_BL[,"NOx.Aldine"],
			    NOy.Aldine   = data.NOy.AD_BL[,"NOy.Aldine"],
			     O3.Aldine   = data.O3.AD_BL[,  "O3.Aldine"],
			    
			     CO.Bayland1 = data.CO.BL[,     "CO.Lang"],			   
			     CO.Bayland2 = data.CO.BL[,     "CO.Texas"],			   
			    NOx.Bayland  = data.NOx.AD_BL[,"NOx.Bayland"],
			    NOy.Bayland  = data.NOy.AD_BL[,"NOy.Bayland"],
			     O3.Bayland  = data.O3.AD_BL[,  "O3.Bayland"],
			    
			     CO.DeerPark = data.CO.DP[,     "CO.DeerPark"],
			    NOx.DeerPark = data.NOx.DP[,   "NOx.DeerPark"],
			    NOy.DeerPark = data.NOy.DP[,   "NOy.DeerPark"],
			     O3.DeerPark = data.O3.DP[,     "O3.DeerPark"]			     
			    )
lab.allGas        <- c("CO.Aldine","CO.Bayland1","CO.Bayland2","CO.DeerPark","NOx.Aldine","NOx.Bayland","NOx.DeerPark","NOy.Aldine","NOy.Bayland","NOy.DeerPark","O3.Aldine","O3.Bayland","O3.DeerPark")
lab.allGas.short  <- c("CO(AD)",   "CO(BL1))",   "CO(BL2)",    "CO(DP)",     "NOx(AD)",   "NOx(BL)",    "NOx(DP)",     "NOy(AD)",   "NOy(BL)",    "NOy(DP)",     "O3(AD)",   "O3(BL)",    "O3(DP)")    

cat(paste("\nAll gaseous species of all sites are combined into a single data frame.\n",sep=""),file=FL.LOG, append=TRUE)
			    
# 15b. create a chron data frame for time series plotting
data.gas.wide <- data.frame(Date.chron = chron(dates  = as.character(data.gas.wide[,"dates"]),
		                               times  = as.character(data.gas.wide[,"times"]),
		                               format = c('m/d/y','h:m:s')),
		            data.gas.wide)

# sort according to date/time
o <- order(data.gas.wide[,"Date.chron"])
data.gas.wide <- data.gas.wide[o,]
		            
		         
cat(paste("\nA corresponded chron dates/time data frame has been created for time series plotting purpose.\n",sep=""),file=FL.LOG, append=TRUE)



# ---------------------------------------------------------------------------------------------------
# 16. get the summary of all chemicals
# ---------------------------------------------------------------------------------------------------
gas.Sum <- data.frame(t(apply(data.gas.wide[,lab.allGas],2,sumfn)))
names(gas.Sum) <- c("no","no.NA","no.NEG","min","p10","p25","median","p75","p90","max","mean","stdev","sum","NA%","NA%.cls","NEG%","NEG%.cls")

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



# -------------------------------------------------------------------------------------------------
# 17a. convert to long format for panel diurnal pattern plotting
# -------------------------------------------------------------------------------------------------
data.Chem      <- data.gas.wide[,lab.allGas]
  no.Chem      <- dim(data.Chem)[2]
data.Chem.long <- STACK(data.Chem)	# STACK the gaseous chemical one on the tp of another

data.gas.long <- data.frame(dates     = rep(data.gas.wide[,"dates"],    no.Chem),
                            times     = rep(data.gas.wide[,"times"],    no.Chem),
                            hourInDay = rep(data.gas.wide[,"hourInDay"],no.Chem),
                            hour_0901 = rep(data.gas.wide[,"hour_0901"],no.Chem),
                            hour      = rep(data.gas.wide[,"hour"],no.Chem),
			    value     = data.Chem.long[,1],
			    chem      = data.Chem.long[,2])


# 15b. create a chron data frame for time series plotting
data.gas.long <- data.frame(Date.chron = chron(dates  = as.character(data.gas.long[,"dates"]),
		                               times  = as.character(data.gas.long[,"times"]),
		                               format = c('m/d/y','h:m:s')),
		            data.gas.long)

# sort according to date/time
o <- order(data.gas.long[,"Date.chron"])
data.gas.long <- data.gas.long[o,]
		            
		          

cat(paste("\nConverted to a long format for panel diurnal pattern plotting.\n",sep=""),file=FL.LOG, append=TRUE)

# -------------------------------------------------------------------------------------------------
# 17b. split the "chem" into "gas" and "site"
# -------------------------------------------------------------------------------------------------
tmp <- as.character(data.gas.long[,"chem"])				# which is ##.$$$ where ## denote gaseous species and $$$ denote the site
gas.name  <- sub("(\\.\\D+\\d?$)", "",tmp,perl=TRUE)	# remove the site name the dot.
site.name <- sub("\\D+\\d?\\.","",tmp,perl=TRUE)	# remove the gas name and the dot.

data.gas.long <- data.frame(data.gas.long,
                             gas.name  = gas.name,
                             site.name = site.name)



# -------------------------------------------------------------------------------------------------
# 18a. correlation among the gaseous species
# -------------------------------------------------------------------------------------------------
corr.coef.pearson  <- cor(data.Chem,method="pearson",use="pairwise.complete.obs")
corr.coef.spearman <- cor(data.Chem,method="spearman",use="pairwise.complete.obs")

diag(corr.coef.pearson)  <- NA
diag(corr.coef.spearman) <- NA


# retain only triangle portion
library("matrixcalc") # turn to upper triangle
corr.coef.pearson  <- upper.triangle(corr.coef.pearson)
corr.coef.spearman <- upper.triangle(corr.coef.spearman)
detach(package:matrixcalc)
cat(paste("\nonly upper triangle of Pearson/Spearman Correlatoin coefficients among gases\n",sep=""),file=FL.LOG,append=TRUE)


#
# add the calculation of spearman correlation on Nov 4, 2008
#


# convert the correlation coefficient table into a list of pairs
corr.list.pearson  <- STACK(data.frame(corr.coef.pearson))
corr.list.spearman <- STACK(data.frame(corr.coef.spearman))

corr.coef.list <- data.frame(cor.pearson  = corr.list.pearson[,"values"],
			     cor.spearman = corr.list.spearman[,"values"],
                             name1.gas    = rep(row.names(corr.coef.pearson),dim(corr.coef.pearson)[1]),
                             name2.gas    = corr.list.pearson[,"ind"])
corr.coef.list <- corr.coef.list[!is.na(corr.coef.list[,"cor.pearson"]),] 
row.names(corr.coef.list) <- seq(1:dim(corr.coef.list)[1])
               

# subsetting the corr pair list by site
corr.bySite <- NULL
for (site in sites)
{
	idx.site <- rep(FALSE,dim(corr.coef.list)[1])
	idx      <- intersect(grep(site,corr.coef.list[,"name1.gas"]),grep(site,corr.coef.list[,"name2.gas"]))
	idx.site[idx] <- TRUE
	tmp.site <- subset(corr.coef.list,idx.site)
	tmp.site <- tmp.site[order(tmp.site[,"name1.gas"]),]	# sort the array according to alphabet of the name

	corr.bySite <- rbind(corr.bySite,tmp.site)
}


# subsetting the corr pair list by gaseous species
corr.byGas  <- NULL
for (gase in gases)
{
	idx.gase <- rep(FALSE,dim(corr.coef.list)[1])
	idx      <- intersect(grep(gase,corr.coef.list[,"name1.gas"]),grep(gase,corr.coef.list[,"name2.gas"]))
	idx.gase[idx] <- TRUE
	tmp.gase <- subset(corr.coef.list,idx.gase)
	tmp.gase <- tmp.gase[order(tmp.gase[,"name1.gas"]),]	# sort the array according to alphabet of the name

	corr.byGas <- rbind(corr.byGas,tmp.gase)
}


# only keep those pair with corr.coef > cut.corr
corr.bySite <- corr.bySite[abs(corr.bySite[,"cor.pearson"])>corr.cut,]
corr.byGas  <- corr.byGas[abs(corr.byGas[,"cor.pearson"])>corr.cut,]
row.names(corr.bySite) <- seq(1:dim(corr.bySite)[1])
row.names(corr.byGas)  <- seq(1:dim(corr.byGas)[1])

# output the correlation coefficients
cat(paste("\nPearson,",sep=""),file=FL.CORR, append=TRUE)
write.table(corr.coef.pearson,file = FL.CORR, sep = ",", col.names = TRUE, row.names=TRUE,append=TRUE)  	                
cat(paste("\nPearson Correlatoin coefficients among gases have been computed.\n",sep=""),file=FL.LOG, append=TRUE)

cat(paste("\nSpearman,",sep=""),file=FL.CORR, append=TRUE)
write.table(corr.coef.spearman,file = FL.CORR, sep = ",", col.names = TRUE, row.names=TRUE,append=TRUE)  	                
cat(paste("\nspearman Correlatoin coefficients among gases have been computed.\n",sep=""),file=FL.LOG, append=TRUE)

cat(paste("\ncorrBySite,",sep=""),file=FL.CORR, append=TRUE)
write.table(corr.bySite,file = FL.CORR, sep = ",", col.names = TRUE, row.names=TRUE,append=TRUE)  	                
cat(paste("\nCorrelatoin coefficients grouped by site.\n",sep=""),file=FL.LOG, append=TRUE)

cat(paste("\ncorrByGas,",sep=""),file=FL.CORR, append=TRUE)
write.table(corr.byGas,file = FL.CORR, sep = ",", col.names = TRUE, row.names=TRUE,append=TRUE)  	                
cat(paste("\nCorrelatoin coefficients grouped by gas.\n",sep=""),file=FL.LOG, append=TRUE)


#
# the truncated correlation coefficients matrix is based on the pearson correlatoin:  
# only retain those pairs with high correlation coefficients
#
corr.coef.trun <- corr.coef.pearson
corr.coef.trun[abs(corr.coef.trun)<corr.cut] <- NA
print(paste(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>corr.cut]))," pairs with r>",corr.cut,";  Only plot those pairs with corr.coef.pearson larger than 0.99\n"))		


cat(paste("\ntrunacted the corr coef matrix to retain less than 50 pairs for DeerPark\n",sep=""),file=FL.LOG,append=TRUE)


          corr.coef.trun  <- data.frame(corr.coef.trun)
    names(corr.coef.trun) <-      names(corr.coef.trun)
row.names(corr.coef.trun) <-      names(corr.coef.trun)



# retrieve those pairs with highest correlation
index   <- which(!is.na(corr.coef.trun))
idx.col <- ceiling(index / no.Chem)
idx.row <- index - (idx.col-1)*no.Chem
cat(paste("\n(gaseous) retained pairsd are retrieved\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 18b. scatter plot of highly correlated pairs
# -------------------------------------------------------------------------------------------------
# postscript(file = FL.CORR.pdf,horizontal=TRUE)
pdf(file = FL.CORR.pdf,title=TI.CORR.pdf,paper="a4r", width=0, height=0)

cat(paste("\n(gaseous)  scatter plots are generated for the following pairs gaseous species\n",sep=""),file=FL.LOG,append=TRUE)	
PAIR_NAME1 <- NULL
PAIR_NAME2 <- NULL
PAIR_CORR  <- NULL
for (idx in seq(along=idx.col))
{		
	# time series of the two gass
	cor.pearson  <- as.character(corr.coef.trun[idx.row[idx],idx.col[idx]])
	cor.spearman <- as.character(corr.coef.spearman[idx.row[idx],idx.col[idx]])

	# only keep several decimals
	if (!is.na(cor.pearson) & (cor.pearson != "1"))	# the reason for this condition is that: when there is only one pair of data, R will be "NA"
	{
		ltrs <- substring(cor.pearson,1:nchar(cor.pearson),1:nchar(cor.pearson))
		cor.pearson <- substr(cor.pearson,1,which(ltrs==".")+3)	
		
		ltrs <- substring(cor.spearman,1:nchar(cor.spearman),1:nchar(cor.spearman))
		cor.spearman <- substr(cor.spearman,1,which(ltrs==".")+3)			
	}

	name1 <- lab.allGas[idx.row[idx]]
	name2 <- lab.allGas[idx.col[idx]]
	
	par(mar=c(5, 4, 4, 5)) 
	layout(rbind(c(1,1,1),
		     c(0,2,0)),
		     respect=rbind(FALSE,TRUE))	

	plot(data.gas.wide[,"Date.chron"],data.gas.wide[,name1],
	     type="l",lty=1,lwd=0.15,col="red",col.lab="red",
	     xlab="date/time (CST)",
	     ylab=paste("Conc(ppbv) ",name1,sep=""),
	     main=paste("(",site,")  Corr coef of ",name1," and ",name2," is ",cor.pearson,"|",cor.spearman,sep=""))
	par(new=TRUE)
	plot(data.gas.wide[,"Date.chron"],data.gas.wide[,name2],
	     type="l",lty=1,lwd=0.15,col="blue",col.lab="blue",
	     axes=FALSE,bty="n",
	     xlab="",ylab="")
	axis(side=4,at=pretty(range(data.gas.wide[,name2],na.rm=TRUE)))
	mtext(col="blue",paste("Conc(ppbv) ",name2,sep=""),side=4,line=3)

	plot(data.gas.wide[,name1],data.gas.wide[,name2],
	     type="p",pch=".",col="red",cex=4,
	     pty="s",
	     xlab=name1,ylab=name2,
	     main=paste("(",site,")  scatter plot of ",name1," vs ",name2,sep=""))


	##### key.4plot <- list(space = "top", 
	##### 		  columns=2,
	##### 		  text  = list(c(name1,name2)),
	##### 		 # points= list(pch=1:6,col=c("black","red","blue","green","magenta","cyan","pink")),
	##### 		 lines = list(lty=1:2,col=c("red","blue")))		
	##### plot.obj1 <- xyplot(data.gas.wide[,name1] + data.gas.wide[,name2] ~ data.gas.wide[,"Date.chron"],
	#####                     data = data.gas.wide,
	##### 		    col=c("red","blue"),
	##### 		    type=c("l","l"),
	##### 		    lty=c(1,2),
	##### 		    xlab="date/time (CST)",ylab="Conc(ppb)",
	##### 		    key = key.4plot,
	##### 		    main=paste("Time series of ",name1," and ",name2," (r= ",cor.pearson,"|",cor.spearman,")",sep=""))
	##### plot.obj2 <- xyplot(as.numeric(data.gas.wide[,name2]) ~ as.numeric(data.gas.wide[,name1]),
	#####                     pch=".",col="cyan",cex=2,
	##### 		    xlab=name1,ylab=name2,
	##### 		    aspect=1,
	##### 		    main=paste("scatter plot of ",name1," and ",name2,sep=""))
	##### plot(plot.obj1,split=c(1,1,1,2))
	##### plot(plot.obj2,split=c(1,2,1,2),newpage=FALSE)

	# write this pair out to the CORRELATION file
	# cat(paste(paste(paste(name1,"(",name1,")",sep=""),paste(name2,"(",name2,")",sep=""),cor.pearson,"|",cor.spearman,sep=","),"\n",sep=""),file=FL.VOC.cor,append=TRUE)	
	PAIR_NAME1 <- c(PAIR_NAME1,paste(name1,sep=""))
	PAIR_NAME2 <- c(PAIR_NAME2,paste(name2,sep=""))
	PAIR_CORR  <- rbind(PAIR_CORR,c(cor.pearson,cor.spearman))
}

PAIR_OUT <- data.frame(PAIR_CORR,name1=PAIR_NAME1,name2=PAIR_NAME2)
names(PAIR_OUT) <- c("corr.Pearson","corr.Spearman","name1","name2")
cat(paste("\n",length(idx.col)," highCorrPair,",sep=""),file=FL.CORR, append=TRUE)
write.table(PAIR_OUT,file = FL.CORR, sep = ",", col.names = TRUE, row.names=TRUE,append=TRUE)  	                
cat(paste("\nhighly correlated pairs.\n",sep=""),file=FL.LOG, append=TRUE)

dev.off() 
 


# ------------------------------------------------------------------------------------------------- 
# 19. time series and scatter plot of all possible pairs
# -------------------------------------------------------------------------------------------------
FL.PDF <- paste(Path.out,paste("Gaseous_ScatterPlot.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- "Gaseous_ScatterPlot.pdf"
if (file.exists(FL.PDF)){print(paste(FL.PDF," exist. Delete it!"));file.remove(FL.PDF)}			# remove existing OUTPUT files

pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)

# the list of all gas-site item
lab.4plot <- union(
             union(
             union(grep("CO", names(data.gas.wide),value=TRUE,perl=TRUE),
                   grep("NOx",names(data.gas.wide),value=TRUE,perl=TRUE)),
                   grep("NOy",names(data.gas.wide),value=TRUE,perl=TRUE)),
                   grep("O3", names(data.gas.wide),value=TRUE,perl=TRUE))

for (idx in seq(from=1,to=length(lab.4plot)-1))
{
	for (idy in seq(from=idx+1,to=length(lab.4plot)))
	{
		name1 <- lab.4plot[idx]
		name2 <- lab.4plot[idy]


		cor.pearson  <- cor(data.gas.wide[,name1],data.gas.wide[,name2],method="pearson",use="pairwise.complete.obs")
		cor.spearman <- cor(data.gas.wide[,name1],data.gas.wide[,name2],method="spearman",use="pairwise.complete.obs")
		
		
		# only keep several decimals
		if (!is.na(cor.pearson) & (cor.pearson != "1"))	# the reason for this condition is that: when there is only one pair of data, R will be "NA"
		{
			ltrs <- substring(cor.pearson,1:nchar(cor.pearson),1:nchar(cor.pearson))
			cor.pearson <- substr(cor.pearson,1,which(ltrs==".")+3)	
			
			ltrs <- substring(cor.spearman,1:nchar(cor.spearman),1:nchar(cor.spearman))
			cor.spearman <- substr(cor.spearman,1,which(ltrs==".")+3)				
		}


		
		key.4plot <- list(space = "top", 
				  columns=2,
				  text  = list(c(name1,name2)),
				 # points= list(pch=1:6,col=c("black","red","blue","green","magenta","cyan","pink")),
				 lines = list(lty=1:2,col=c("red","blue")))		
		plot.obj1 <- xyplot(data.gas.wide[,name1] + data.gas.wide[,name2] ~ data.gas.wide[,"Date.chron"],
				    col=c("red","blue"),
				    type=c("l","l"),
				    lty=c(1,2),
				    xlab="date/time (CST)",ylab="Conc(ppb)",
				    key = key.4plot,
				    main=paste("Time series of ",name1," and ",name2," (r= ",cor.pearson,"|",cor.spearman,")",sep=""))
		plot.obj2 <- xyplot(data.gas.wide[,name2] ~ data.gas.wide[,name1],
				    pch=".",col="cyan",cex=2,
				    xlab=name1,ylab=name2,
				    aspect=1,
				    main=paste("scatter plot of ",name1," and ",name2,sep=""))
		plot(plot.obj1,split=c(1,1,1,2))
		plot(plot.obj2,split=c(1,2,1,2),newpage=FALSE)
	}
}
dev.off()



# ------------------------------------------------------------------------------------------------- 
# 20. plot time series one plot for each species at all sites (not grouped together)
# -------------------------------------------------------------------------------------------------
FL.PDF <- paste(Path.out,paste("Gaseous_TimeSeries.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- "Gaseous_TimeSeries.pdf"
if (file.exists(FL.PDF)){print(paste(FL.PDF," exist. Delete it!"));file.remove(FL.PDF)}			# remove existing OUTPUT files


pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)
for (gas in gases)
{

	idx.row <- as.character(data.gas.long[,"gas.name"]) == gas
	n.row <- ifelse(gas == "CO",4,3)

	plot.obj <- xyplot(value ~ Date.chron | site.name,
		           data = data.gas.long[idx.row,],
		           main=paste("Time Series of (",gas,")",sep=""),
		           type="l",lty=1,lwd=0.5,
		           xlab="date/time (CST)",ylab="Conc(ppbv)", 
		           scales=list(y="free"),
		           layout=c(1,n.row),between=list(y=0.5),
		           as.table=TRUE)
	plot(plot.obj)
}
dev.off()
		    

# -------------------------------------------------------------------------------------------------	
# 21. dot plots
# -------------------------------------------------------------------------------------------------
FL.PDF <- paste(Path.out,paste("Gaseous_dotplot.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- "Gaseous_dotplot.pdf"
if (file.exists(FL.PDF)){print(paste(FL.PDF," exist. Delete it!"));file.remove(FL.PDF)}			# remove existing OUTPUT files

# postscript(file = FL.PDF,horizontal=TRUE)
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)
# -------------------------------------------------------------
# in RAW concentration UNIT
# -------------------------------------------------------------

	plot.obj <- xyplot(value ~ hourInDay | factor(chem),data=data.gas.long,
		   prepanel = function(x, y) prepanel.loess(x, y, span = 0.025),
		   xlab="Hour(CST)",ylab="Conc(ppb)",main="Diurnal Pattern of all gaseous chemicals",
		   cex=0.5,pch=".",col="red",
		   layout = c(4,4),
		   skip = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE),
		   
		   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
					labels = c(0,4,8,12,16,20,24),
					limits = c(-1,25)
					),y="free"
			      ),
		   panel = function(x, y) {
			   panel.grid(h=-1, v= 4)
			   panel.xyplot(x, y,pch=".",col="blue")
			   panel.loess(x,y, span=.025,type="l",lty=1,col="red")
		       }	                      
		   )
	plot(plot.obj) 	 
	
	#
	# diurnal pattern of (site) in panel
	#
	for (site in sites)
	{
		n.row <- ifelse(site == "Bayland",3,2)	# Bayland has 2 CO leads to 5 Species, otherwise only 4 and 2 rows are enough

		# get index of the subset for each site
		idx <- grep(site,data.gas.long[,"chem"])
		plot.obj <- xyplot(value ~ hourInDay | factor(chem),data=data.gas.long[idx,],
			   prepanel = function(x, y) prepanel.loess(x, y, span = 0.025),
			   xlab="Hour(CST)",ylab="Conc(ppb)",
			   main=paste("Diurnal Pattern of all gaseous chemicals at (",site,")",sep=""),
			   cex=0.5,pch=".",col="red",
		   	   between = list(x=c(0.5,0.5),y=c(0.5)),
   			   layout = c(2,n.row),
			   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
						labels = c(0,4,8,12,16,20,24),
						limits = c(-1,25)
						),y="free"
				      ),
			   panel = function(x, y) {
				   panel.grid(h=-1, v= 4)
				   panel.xyplot(x, y,pch=".",col="blue")
				   panel.loess(x,y, span=.025,type="l",lty=1,col="red")
			       }	                      
			   )
		plot(plot.obj) 	 
	}
	
	#
	# diurnal pattern of (gas) in panel
	#
	for (gas in gases)
	{
		# get index of the subset for each gas
		idx <- grep(gas,data.gas.long[,"chem"])
		plot.obj <- xyplot(value ~ hourInDay | factor(chem),data=data.gas.long[idx,],
			   prepanel = function(x, y) prepanel.loess(x, y, span = 0.025),
			   xlab="Hour(CST)",ylab="Conc(ppb)",
			   main=paste("Diurnal Pattern of (",gas,") at all sites",sep=""),
			   cex=0.5,pch=".",col="red",
		   	   between = list(x=c(0.5,0.5),y=c(0.5)),
   			   layout = c(2,n.row),			   
			   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
						labels = c(0,4,8,12,16,20,24),
						limits = c(-1,25)
						),y="free"
				      ),
			   panel = function(x, y) {
				   panel.grid(h=-1, v= 4)
				   panel.xyplot(x, y,pch=".",col="blue")
				   panel.loess(x,y, span=.025,type="l",lty=1,col="red")
			       }	                      
			   )
		plot(plot.obj) 	 
	}	
	
# -------------------------------------------------------------
# in LOGARITHM concentration SCALE
# -------------------------------------------------------------

	plot.obj <- xyplot(value ~ hourInDay | factor(chem),data=data.gas.long,
		   prepanel = function(x, y) prepanel.loess(x, y, span = 0.025),
		   xlab="Hour(CST)",ylab="Conc(ppb)",main="Diurnal Pattern of all gaseous chemicals",
		   cex=0.5,pch=".",col="red",
		   layout = c(4,4),
		   skip = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE),
		   
		   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
					labels = c(0,4,8,12,16,20,24),
					limits = c(-1,25)
					),y=list(log=TRUE,relation="free")
			      ),
		   panel = function(x, y) {
			   panel.grid(h=-1, v= 4)
			   panel.xyplot(x, y,pch=".",col="blue")
			   panel.loess(x,y, span=.025,type="l",lty=1,col="red")
		       }	                      
		   )
	plot(plot.obj) 	 
	
	#
	# diurnal pattern of (site) in panel
	#
	for (site in sites)
	{
		n.row <- ifelse(site == "Bayland",3,2)	# Bayland has 2 CO leads to 5 Species, otherwise only 4 and 2 rows are enough

		# get index of the subset for each site
		idx <- grep(site,data.gas.long[,"chem"])
		plot.obj <- xyplot(value ~ hourInDay | factor(chem),data=data.gas.long[idx,],
			   prepanel = function(x, y) prepanel.loess(x, y, span = 0.025),
			   xlab="Hour(CST)",ylab="Conc(ppb)",
			   main=paste("Diurnal Pattern of all gaseous chemicals at (",site,")",sep=""),
			   cex=0.5,pch=".",col="red",
		   	   between = list(x=c(0.5,0.5),y=c(0.5)),
   			   layout = c(2,n.row),
			   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
						labels = c(0,4,8,12,16,20,24),
						limits = c(-1,25)
						),y=list(log=TRUE,relation="free")
				      ),
			   panel = function(x, y) {
				   panel.grid(h=-1, v= 4)
				   panel.xyplot(x, y,pch=".",col="blue")
				   panel.loess(x,y, span=.025,type="l",lty=1,col="red")
			       }	                      
			   )
		plot(plot.obj) 	 
	}
	
	#
	# diurnal pattern of (gas) in panel
	#
	for (gas in gases)
	{
		# get index of the subset for each gas
		idx <- grep(gas,data.gas.long[,"chem"])
		plot.obj <- xyplot(value ~ hourInDay | factor(chem),data=data.gas.long[idx,],
			   prepanel = function(x, y) prepanel.loess(x, y, span = 0.025),
			   xlab="Hour(CST)",ylab="Conc(ppb)",
			   main=paste("Diurnal Pattern of (",gas,") at all sites",sep=""),
			   cex=0.5,pch=".",col="red",
		   	   between = list(x=c(0.5,0.5),y=c(0.5)),
   			   layout = c(2,n.row),			   
			   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
						labels = c(0,4,8,12,16,20,24),
						limits = c(-1,25)
						),y=list(log=TRUE,relation="free")
				      ),
			   panel = function(x, y) {
				   panel.grid(h=-1, v= 4)
				   panel.xyplot(x, y,pch=".",col="blue")
				   panel.loess(x,y, span=.025,type="l",lty=1,col="red")
			       }	                      
			   )
		plot(plot.obj) 	 
	}
dev.off()
cat(paste("Diurnal patterns have been plotted in dots and loess smoothed.\n",sep=""),file=FL.LOG, append=TRUE)




# -------------------------------------------------------------------------------------------------	
# 22. boxplots
# -------------------------------------------------------------------------------------------------
FL.PDF <- paste(Path.out,paste("Gaseous_boxplot.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- "Gaseous_boxplot.pdf"
if (file.exists(FL.PDF)){print(paste(FL.PDF," exist. Delete it!"));file.remove(FL.PDF)}			# remove existing OUTPUT files
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)

# -------------------------------------------------------------
# in RAW concentration UNIT
# -------------------------------------------------------------
	plot.obj <- xyplot(data.gas.long[,"value"] ~ data.gas.long[,"hour"] | factor(data.gas.long[,"chem"]),
	           panel = panel.bwplot,horizontal=FALSE,
		   xlab="Hour(CST)",ylab="Conc(ppb)",main="Diurnal Pattern of all gaseous chemicals",
		   cex=0.5,pch=19,col="red",
		   between = list(x=c(0.5,0.5,0.5,0.5),y=c(0.5,0.5,0.5)),
		   layout = c(4,4),
		   skip = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE),
		   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
					labels = c(0,4,8,12,16,20,24),
					limits = c(-1,25)
					),y="free"
			      )	                      
		   )
	plot(plot.obj) 	 
	
	#
	# diurnal pattern of (site) in panel
	#
	for (site in sites)
	{
		n.row <- ifelse(site == "Bayland",3,2)	# Bayland has 2 CO leads to 5 Species, otherwise only 4 and 2 rows are enough
		
		# get index of the subset for each site
		idx <- grep(site,data.gas.long[,"chem"])
		plot.obj <- xyplot(data.gas.long[idx,"value"] ~ data.gas.long[idx,"hour"] | factor(data.gas.long[idx,"chem"]),
		            panel = panel.bwplot,horizontal=FALSE,
			   xlab="Hour(CST)",ylab="Conc(ppb)",
			   main=paste("Diurnal Pattern of all gaseous chemicals at (",site,")",sep=""),
			   cex=0.5,pch=19,col="red",
		   	   between = list(x=c(0.5,0.5),y=c(0.5)),
   			   layout = c(2,n.row),
			   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
						labels = c(0,4,8,12,16,20,24),
						limits = c(-1,25)
						),y="free"
				      )
	                      
			   )
		plot(plot.obj) 	 
	}
	
	#
	# diurnal pattern of (gas) in panel
	#
	for (gas in gases)
	{
		# get index of the subset for each gas
		idx <- grep(gas,data.gas.long[,"chem"])
		plot.obj <- xyplot(data.gas.long[idx,"value"] ~ data.gas.long[idx,"hour"] | factor(data.gas.long[idx,"chem"]),
			   panel = panel.bwplot,horizontal=FALSE,
			   xlab="Hour(CST)",ylab="Conc(ppb)",
			   main=paste("Diurnal Pattern of (",gas,") at all sites",sep=""),
			   cex=0.5,pch=19,col="red",
		   	   between = list(x=c(0.5),y=c(0.5)),
   			   layout = c(2,2),
   			   notch=TRUE,
			   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
						labels = c(0,4,8,12,16,20,24),
						limits = c(-1,25)
						),y="free"
				      )	                      
			   )
		plot(plot.obj) 	 
	}	
	
# -------------------------------------------------------------
# in LOGARITHM concentration SCALE
# -------------------------------------------------------------
	plot.obj <- xyplot(data.gas.long[,"value"] ~ data.gas.long[,"hour"] | factor(data.gas.long[,"chem"]),
	           panel = panel.bwplot,horizontal=FALSE,
		   xlab="Hour(CST)",ylab="Conc(ppb)",main="Diurnal Pattern of all gaseous chemicals",
		   cex=0.5,pch=19,col="red",
		   between = list(x=c(0.5,0.5,0.5,0.5),y=c(0.5,0.5,0.5)),
		   layout = c(4,4),
		   skip = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE),
		   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
					labels = c(0,4,8,12,16,20,24),
					limits = c(-1,25)
					),y=list(log=TRUE,relation="free")
			      )	                      
		   )
	plot(plot.obj) 	 
	
	#
	# diurnal pattern of (site) in panel
	#
	for (site in sites)
	{
		n.row <- ifelse(site == "Bayland",3,2)	# Bayland has 2 CO leads to 5 Species, otherwise only 4 and 2 rows are enough
		
		# get index of the subset for each site
		idx <- grep(site,data.gas.long[,"chem"])
		plot.obj <- xyplot(data.gas.long[idx,"value"] ~ data.gas.long[idx,"hour"] | factor(data.gas.long[idx,"chem"]),
		            panel = panel.bwplot,horizontal=FALSE,
			   xlab="Hour(CST)",ylab="Conc(ppb)",
			   main=paste("Diurnal Pattern of all gaseous chemicals at (",site,")",sep=""),
			   cex=0.5,pch=19,col="red",
		   	   between = list(x=c(0.5,0.5),y=c(0.5)),
   			   layout = c(2,n.row),
			   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
						labels = c(0,4,8,12,16,20,24),
						limits = c(-1,25)
						),y=list(log=TRUE,relation="free")
				      )
	                      
			   )
		plot(plot.obj) 	 
	}
	
	#
	# diurnal pattern of (gas) in panel
	#
	for (gas in gases)
	{
		# get index of the subset for each gas
		idx <- grep(gas,data.gas.long[,"chem"])
		plot.obj <- xyplot(data.gas.long[idx,"value"] ~ data.gas.long[idx,"hour"] | factor(data.gas.long[idx,"chem"]),
			   panel = panel.bwplot,horizontal=FALSE,
			   xlab="Hour(CST)",ylab="Conc(ppb)",
			   main=paste("Diurnal Pattern of (",gas,") at all sites",sep=""),
			   cex=0.5,pch=19,col="red",
		   	   between = list(x=c(0.5),y=c(0.5)),
   			   layout = c(2,2),
   			   notch=TRUE,
			   scales=list(x = list(at     = c(0,4,8,12,16,20,24),
						labels = c(0,4,8,12,16,20,24),
						limits = c(-1,25)
						),y=list(log=TRUE,relation="free")
				      )	                      
			   )
		plot(plot.obj) 	 
	}
dev.off()
cat(paste("Diurnal patterns have been plotted in boxplots.\n",sep=""),file=FL.LOG, append=TRUE)



# ---------------------------------------------------------------------------------------------------
# 30. write all data objects into a R data object file
# ---------------------------------------------------------------------------------------------------	
save(data.gas.wide,data.gas.long,corr.coef.pearson,corr.coef.spearman,corr.bySite,corr.byGas,time.info,file = FL.OBJ)
cat(paste("all important data objects are saved in ",FL.OBJ,"\n",sep=""),file=FL.LOG,append=TRUE)


cat(paste("(merged-wide),",sep=""),file=FL.CSV.wide,append=TRUE)
write.table(data.frame(data.gas.wide),file = FL.CSV.wide,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	    

cat(paste("(merged-long),",sep=""),file=FL.CSV.long,append=TRUE)
write.table(data.frame(data.gas.long),file = FL.CSV.long,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	    



#
# 35. List of data objects saved in the R object file
#
list.object <- c("data.gas.wide","data.gas.long","corr.coef.pearson","corr.coef.spearman","corr.bySite","corr.byGas","time.info")
cat(paste("\n The following data objects are saved in the R object file (",FL.OBJ,")\n\n\n\n\n",sep=""))
cat(paste("\n The following data objects are saved in the R object file (",FL.OBJ,")\n\n\n\n\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste(list.object,collapse="\n"))
cat(paste(list.object,collapse="\n"),file=FL.LOG,append=TRUE)


# ---------------------------------------------------------------------------------------------------
# 40. time used for completing this script
# ---------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("9a_gasDataProcess.R is finished successfully!\n",sep=" "))
cat(paste("9a_gasDataProcess.R is finished successfully!\n",sep=" "),file=FL.LOG,append=TRUE)


cat(paste("Processing time for 9a_gasDataProcess.R is",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)

