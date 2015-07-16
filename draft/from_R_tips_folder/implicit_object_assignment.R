#
# 2011_HanfordReach_Evaluate_2012NewCriteria.R
#
# March 27, 2012: called Nino to get knowing the detail of the criteria implemented for this year.
# 	If unit’s top width delta is more than 32.4 ft  (where the rtw delta is the max of history and minimum of future)
#		Check if there are any entrapments in that (2-quadrant) unit
#			If none, don’t add the unit to the list and increment the unit loop counter
#			Else (If there’s one or more)
#				Find out the start of the sampling window T_start (the first point in future data which has an at least 32.4 ft of rtw drop from the rtw maximum in the history)
#				If the start of the sampling window is 9:00 pm or beyond, throw the unit away.
#				Check if the sampling window is at least 2 hours long (starting from the start of the sampling window, at least two consecutive hours with more than 32.4 ft rtw drop)
#				Get the WSEL for the point of time when the first top-width-delta-32.4ft+ starts (i.e., T_start)
#				Check each entrapment location’s ELEVATION to see if it is +-15 cm difference from the WSEL acquired in the previous step (This might need to be confirmed.  Should we only check if the entrapment elevation is at least 15 cm above WSE?)
#					If none meets those criteria,  don’t add the unit to the list and increment the unit loop counter
#					Else (there’s some that meet the criteria)
#						Add the unit to the list 
#					If there’s 10 units in the list
#						Exit the loop
#					Else (if there’s still less than 10 units and there’s still available units)
#						Increment the unit loop counter
#	Else (if there’s still available units)
#		Increment the unit loop counter
#
#
# March 26, 2012: 
# Implement what have been discussed with Chris on Wednesday March 21, 2012 and the following changes we are going to make:
# -------------------------------------------------------------------------------------------------------------------------
# (1) In the evaulation process, for each field event we will only extract entrapments from the immediate transect NOT from the apriing transect.
# (2) Will keep the starting time down to the tnes of minute instead of round to hours
# (3) The WSEL time series will be interpolated into tens of minute instead of hourly in order to utilize the field event starting time in tens of minutes.
# (4) Instead of find the maximum WSEL and the difference between the maximum WSEL and the entrapment elevation, search backward to see if there is a time point where the WSEL exceed the entrapment elevation by 5, 10, 15 ... cm.  If yes, when is the most recent time point satifying that.
# (5) record the time of the most recent satisification to access the age of the entrapments.
# 
# (6) Evaluate the new criter received from Nino Zuljevic on March 22, 2012 (which is based on the transect pairs.
# -------------------------------------------------------------------------------------------------------------------------
# created on March 16, 2012
# three pieces of data are needed
# 1. MASS1 water elevation for each of the transects: 
# 	C:\YuLong_Projects\FY2012_SalmonStrandingFieldSurvey\Data_Received\DataReceived_Jan10_2012_MASS1\
#
# 2. historic entrapments with entrapment elevations (assuming it is the elevation of the bottom of the entrapment
#	C:\YuLong_Projects\FY2012_SalmonStrandingFieldSurvey\0_data\entrapment_historyWithElev.csv
#
# 3. the places visited during 2011 field survey: 
#	C:\YuLong_Projects\FY2012_SalmonStrandingFieldSurvey\0_data\entrapment_visits2011.csv
#
# 
# -------------------------------------------------------------------------------------------------
#
# section	segment	lower boundary	upper boundary	site	tot sites
# upper		1	620		635		15
# upper		2	605		620		15
# ---------------------------------------------------------	30
# middle 	3 	595		605		10	
# middle 	4 	588		595		7	
# middle 	5 	581		588		7	
# middle 	6 	575		581		6	
# ---------------------------------------------------------	30
# lower		7 	558		575		17	
# lower 	8 	545		558		13	
# ---------------------------------------------------------	30
#
# This script is used to turn the data into the following format:
# a 360 by 10 matrix
# rows:    360 transect
# columns: 9   two-week period and one "section" field
#
# eliminate all stuff
rm(list = ls(all = TRUE))
library("chron")

# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# close all devices which have been opened
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

# metter to feet conversion
meter2foot <- 3.280839895
cm2foot    <- 0.03280839895
foot2cm    <- 30.48

# critera of river top surface drop: 32.4 ft
rtw.criteria <- 32.4


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
	Path.Current <- "C:/YuLong_Projects/FY2012_SalmonStrandingFieldSurvey/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2012_SalmonStrandingFieldSurvey/0_scripts"
}
setwd(Path.Current)


# -------------------------------------------------------------------------------------------------
#	setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.ENT.IN   <- "../0_data"							# INPUT EWntrapment data directory
Path.MASS1.IN <- "../Data_Received/DataReceived_Jan10_2012_MASS1"		# INPUT MASS1 data directory

Path.out      <- "../2011_HanfordReach_Evaluate_2012NewCriteria"		# OUTPUT directory
Path.log      <- "../0_log"							# OUTPUT log  directory
if (!file.exists(Path.ENT.IN))  {stop(paste(" INPUT data folder ",Path.ENT.IN,  " does NOT exist!\n",sep=""))}
if (!file.exists(Path.MASS1.IN)){stop(paste(" INPUT data folder ",Path.MASS1.IN," does NOT exist!\n",sep=""))}
if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}


# ------------------------------------------------------------------------------------------------- 
# 	create output files
# ------------------------------------------------------------------------------------------------- 
FL.EntHist.IN  <- paste(Path.ENT.IN,paste("entrapment_historyWithElev.csv",sep=""),sep="/")
FL.EntVisit.IN <- paste(Path.ENT.IN,paste("entrapment_visits2011.csv",     sep=""),sep="/")
if (!file.exists(FL.EntHist.IN)) {stop(paste(" INPUT data file [",FL.EntHist.IN,"]  does NOT exist!\n",sep=""))}
if (!file.exists(FL.EntVisit.IN)){stop(paste(" INPUT data file [",FL.EntVisit.IN,"] does NOT exist!\n",sep=""))}



# ------------------------------------------------------------------------------------------------- 
# 	create output files
# ------------------------------------------------------------------------------------------------- 
FL.LOG            <- paste(Path.log,"2011_HanfordReach_Evaluate_2012NewCriteria.log",  sep="/")	
FL.rawHist.csv    <- paste(Path.out,"EntHist_data.csv",  sep="/")	
FL.rawVisit.csv   <- paste(Path.out,"EntVisit_data.csv", sep="/")	
FL.results.detail <- paste(Path.out,"results_detail.csv",sep="/")	
FL.results.mass1  <- paste(Path.out,"results_mass1.csv", sep="/")	
FL.results.sum    <- paste(Path.out,"results_sum.csv",   sep="/")	
FL.results.obj    <- paste(Path.out,"results.Rdata",     sep="/")	
FL.results.pdf    <- paste(Path.out,"results.pdf",       sep="/")	
if (file.exists(FL.LOG))           {print(paste(FL.LOG,           "exist.Delete it!"));file.remove(FL.LOG)}
if (file.exists(FL.rawHist.csv))   {print(paste(FL.rawHist.csv,   "exist.Delete it!"));file.remove(FL.rawHist.csv)}
if (file.exists(FL.rawVisit.csv))  {print(paste(FL.rawVisit.csv,  "exist.Delete it!"));file.remove(FL.rawVisit.csv)}
if (file.exists(FL.results.mass1)) {print(paste(FL.results.mass1, "exist.Delete it!"));file.remove(FL.results.mass1)}
if (file.exists(FL.results.sum))   {print(paste(FL.results.sum,   "exist.Delete it!"));file.remove(FL.results.sum)}
if (file.exists(FL.results.detail)){print(paste(FL.results.detail,"exist.Delete it!"));file.remove(FL.results.detail)}
if (file.exists(FL.results.obj))   {print(paste(FL.results.obj,   "exist.Delete it!"));file.remove(FL.results.obj)}
if (file.exists(FL.results.pdf))   {print(paste(FL.results.pdf,   "exist.Delete it!"));file.remove(FL.results.pdf)}

pdf(file = FL.results.pdf,paper="a4r", width=0, height=0)


# ------------------------------------------------------------------------------------------------- 
# 	load libraries
# -------------------------------------------------------------------------------------------------
library("reshape")
# library("lattice")
# library("chron")
# library("cwhmisc")	# used for remove duplicate rows in a data frame
# library(RODBC)
# library(graphics)
# library(gplots)
# library("locfit")
# library("boot")
cat(paste("loaded necessary libraries","\n",sep=""))
cat(paste("loaded necessary libraries","\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# read entrapment history data into [myData.EntHist]
# -------------------------------------------------------------------------------------------------
field.EntHist  <- c("id","transect","elevation(m)","easting(m)","northing(m)")
      myData.EntHist  <- read.table(file=FL.EntHist.IN,sep=",",stringsAsFactors=FALSE,header=TRUE)
names(myData.EntHist) <- field.EntHist
cat(paste("Entrapment History [",FL.EntHist.IN,"] has been loaded!\n",sep=""))
cat(paste("Entrapment History [",FL.EntHist.IN,"] has been loaded!\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# read entrapment history data into [myData.EntVisit]
# -------------------------------------------------------------------------------------------------
field.EntVisit <- c("date","transect","Entrapments","id","time")
      myData.EntVisit  <- read.table(file=FL.EntVisit.IN,sep=",",stringsAsFactors=FALSE,header=TRUE)
names(myData.EntVisit) <- field.EntVisit
cat(paste("Entrapment Visit events [",FL.EntVisit.IN,"] has been loaded!\n",sep=""))
cat(paste("Entrapment Visit events [",FL.EntVisit.IN,"] has been loaded!\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# read MASS1 data for each of the transect into objects renamed in [MASS1.obj] variable which has the format [results###], we will have 360 [results] objects
# -------------------------------------------------------------------------------------------------
field.MASS1 <- c("date","transect","riverMile(mi)","wse(ft)","discharge(ft3/s)","riverTopWidth(ft)")
MASS1.obj.array <- c()	# array to record MASS1 objects
for (transect in seq(from=1,to=360))
{
	if (transect < 10)
	{
		MASS1.obj <- paste("results.00",transect,sep="")
	}else if (transect < 100){
		MASS1.obj <- paste("results.0",transect,sep="")
	}else{
		MASS1.obj <- paste("results.",transect,sep="")
	}
	MASS1.obj.array <- c(MASS1.obj.array,MASS1.obj)
	
	FL.MASS1 <- paste(Path.MASS1.IN,paste(MASS1.obj,".csv",sep=""),sep="/")
	
	command.string <- paste("tmp.MASS1 <- read.table(file=\"",FL.MASS1, "\",header=TRUE,colClasses = c(\"character\",\"numeric\",\"numeric\",\"numeric\",\"numeric\",\"numeric\"),sep=\",\")",sep="")
	eval(parse(text=command.string))
	names(tmp.MASS1) <- field.MASS1
	
	# add a chorn date field
	tmp.mdy.hms <- unlist(strsplit(tmp.MASS1[,"date"],"\\s+",perl=TRUE))
	
	tmp.mdy.part <- tmp.mdy.hms[seq(from=1,to=length(tmp.mdy.hms),by=2)]
	tmp.hms.part <- tmp.mdy.hms[seq(from=2,to=length(tmp.mdy.hms),by=2)]
	
	tmp.mdy   <- unlist(strsplit(tmp.mdy.part,"-",perl=TRUE))
	tmp.month <- as.numeric(tmp.mdy[seq(from=1,to=length(tmp.mdy),by=3)])	
	tmp.day   <- as.numeric(tmp.mdy[seq(from=2,to=length(tmp.mdy),by=3)])	
	tmp.year  <- as.numeric(tmp.mdy[seq(from=3,to=length(tmp.mdy),by=3)])

	tmp.hms   <- unlist(strsplit(tmp.hms.part,":",perl=TRUE))
	tmp.hour  <- as.numeric(tmp.hms[seq(from=1,to=length(tmp.hms),by=3)])
	tmp.minute<- as.numeric(tmp.hms[seq(from=2,to=length(tmp.hms),by=3)])
	tmp.second<- as.numeric(tmp.hms[seq(from=3,to=length(tmp.hms),by=3)])

	tmp.chron <- chron(dates = paste(tmp.month,tmp.day,   tmp.year,  sep="/"),
		           times = paste(tmp.hour, tmp.minute,tmp.second,sep=":"))

	# add more field into [tmp.MASS1]
	tmp.MASS1 <- cbind(tmp.MASS1,
                           chron.date = tmp.chron,
                           year       = tmp.year,
                           month      = tmp.month,
                           day        = tmp.day,
                           hour       = tmp.hour)	
	                   
	
	# assing [tmp.MASS1] to a more meaningful name
	command.string <- paste("\"",MASS1.obj,"\" <- tmp.MASS1",sep="")
	eval(parse(text=command.string))	
}
cat(paste("MASS1 water surface elevation of all ",transect," have been loaded!\n",sep=""))
cat(paste("MASS1 water surface elevation of all ",transect," have been loaded!\n",sep=""),file=FL.LOG,append=TRUE)

# use the transect number as the index of the MASS1 obj array
# the MASS1 data will be in objects like: [results.001], [results.002], ..., [results360]
names(MASS1.obj.array) <- as.numeric(sub("results.","",MASS1.obj.array))


# ------------------------------------------------------------------------------------------------- 
# output the history data for verification purpose
# -------------------------------------------------------------------------------------------------
cat(paste("raw History,",sep=""),file=FL.rawHist.csv,append=TRUE)
write.table(myData.EntHist,file=FL.rawHist.csv,sep=",",quote = FALSE,row.names=TRUE,col.names=TRUE,append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# output the survey event data for verification purpose
# -------------------------------------------------------------------------------------------------
cat(paste("raw Visit Event,",sep=""),file=FL.rawVisit.csv,append=TRUE)
write.table(myData.EntVisit,file=FL.rawVisit.csv,sep=",",quote = FALSE,row.names=TRUE,col.names=TRUE,append=TRUE)


# -------------------------------------------------------------------------------------------------
# [myData.EntHist]: add an entrapment elevation field in unit of feet (historic data has elevation in unit of meter.  2011 data has both meter and feet unit. Only kept meet unit.
# -------------------------------------------------------------------------------------------------
myData.EntHist[,"elevation(ft)"] <- myData.EntHist[,"elevation(m)"] * meter2foot
cat(paste("added elevation(ft) field in [myData.EntHist]!\n",sep=""))
cat(paste("added elevation(ft) field in [myData.EntHist]!\n",sep=""),file=FL.LOG,append=TRUE)
        

# -------------------------------------------------------------------------------------------------
# [myData.EntVisit]: get the month, year and day, hour
# -------------------------------------------------------------------------------------------------
tmp.mdy   <- unlist(strsplit(myData.EntVisit[,"date"],"/",perl=TRUE))
tmp.month <- as.numeric(tmp.mdy[seq(from=1,to=length(tmp.mdy),by=3)])	
tmp.day   <- as.numeric(tmp.mdy[seq(from=2,to=length(tmp.mdy),by=3)])	
tmp.year  <- as.numeric(tmp.mdy[seq(from=3,to=length(tmp.mdy),by=3)])

tmp.hms   <- unlist(strsplit(myData.EntVisit[,"time"],":",perl=TRUE))
tmp.hour  <- as.numeric(tmp.hms[seq(from=1,to=length(tmp.hms),by=2)])
tmp.minute<- as.numeric(tmp.hms[seq(from=2,to=length(tmp.hms),by=2)])
tmp.minute<- floor(tmp.minute/10+.5)*10				# round the minute to the nearest tens of minutes


# -------------------------------------------------------------------------------------------------
# keep the starting time to the tens of minutes
# the rounding of minutes to the nearest tens of minute may results rounded minute == 60, which means we need to increase the hour by 1 and reset the minute to 0
# since the hour range is between 5 and 20, we do not need to worry about the hour change at the turn of the day and the need to change the date, and then the month and even the year if it it occur at the end of the month and year!!
# -------------------------------------------------------------------------------------------------
tmp.hour.adj   <- tmp.hour
tmp.minute.adj <- tmp.minute

tmp.hour.adj[tmp.minute.adj >= 60]   <- tmp.hour.adj[tmp.minute.adj >= 60] + 1	# when minute rounded to 60, the hour should be increase by 1 and reset the minute to 0
tmp.minute.adj[tmp.minute.adj >= 60] <- 0					# reset the minute to 0 after resetting the hour by an increment of 1


tmp.chron <- chron(dates = paste(tmp.month,tmp.day,tmp.year,sep="/"),
		   times = paste(tmp.hour.adj, tmp.minute.adj,rep("0",length(tmp.hour)),sep=":"))


myData.EntVisit <- cbind(myData.EntVisit,
                         start.time = tmp.chron,
                         year       = tmp.year,
                         month      = tmp.month,
                         day        = tmp.day,
                         hour       = tmp.hour,
                         hour.adj   = tmp.hour.adj,
                         minute     = tmp.minute,
                         minute.adj = tmp.minute.adj)
cat(paste("added split date/time and chron date fields in [myData.EntVisit]!\n",sep=""))
cat(paste("added split date/time and chron date fields in [myData.EntVisit]!\n",sep=""),file=FL.LOG,append=TRUE)
                         

 
# -------------------------------------------------------------------------------------------------
# sort the entrapment history and visit event based on transect and then tim
# -------------------------------------------------------------------------------------------------
myData.EntHist  <- myData.EntHist[order(myData.EntHist[,"transect"]),]
myData.EntVisit <- myData.EntVisit[order(myData.EntVisit[,"transect"],myData.EntVisit[,"start.time"]),]
cat(paste("sorted [myData.EntVisit] and [myData.EntHist] according to transect!\n",sep=""))
cat(paste("sorted [myData.EntVisit] and [myData.EntHist] according to transect!\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# output the sorted [myData.EntHist] data for verification purpose
# -------------------------------------------------------------------------------------------------
cat(paste("\n\nsorted History,",sep=""),file=FL.rawHist.csv,append=TRUE)
write.table(myData.EntHist,file=FL.rawHist.csv,sep=",",quote = FALSE,row.names=TRUE,col.names=TRUE,append=TRUE)

# -------------------------------------------------------------------------------------------------
# output the sorted [myData.EntVisit] data for verification purpose
# -------------------------------------------------------------------------------------------------
cat(paste("\n\nsorted Visit Event,",sep=""),file=FL.rawVisit.csv,append=TRUE)
write.table(myData.EntVisit,file=FL.rawVisit.csv,sep=",",quote = FALSE,row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("write the sorted [myData.EntVisit] and [myData.EntHist] out!\n",sep=""))
cat(paste("write the sorted [myData.EntVisit] and [myData.EntHist] out!\n",sep=""),file=FL.LOG,append=TRUE)


# *************************************************************************************************
# *************************************************************************************************
# *************************************************************************************************
# start the evaluation for each of the entrapment survey event
# *************************************************************************************************
# *************************************************************************************************
# *************************************************************************************************
cat(paste("visit.idx,date,time,transect,flag,start.time,no.ent.primTransect,no.ent.pairTransect,no.ent.foundThis,criteria1,criteria2, criteria3,criteria4,ent.elev,flag.entSuit.5cm,flag.entSuit.10cm,flag.entSuit.15cm,flag.entForm.20cm,flag.entSuit.25cm,flag.entSuit.30cm,T.start,T.start.hr,wse.T.start,rtw.T.start,change.rtw(ft),elev.drop(cm),MASS1.fileName,time0,time1\n"),file=FL.results.detail,append=TRUE)
cat(paste("visit.idx,date,time,transect,flag,start.time,no.ent.primTransect,no.ent.pairTransect,no.ent.foundThis,FLAG.entForm.5cm,FLAG.entForm.10cm,FLAG.entForm.15cm,FLAG.entForm.20cm,FLAG.entForm.25cm,FLAG.entForm.30cm,no.entForm.5cm,no.entForm.10cm,no.entForm.15cm,no.entForm.20cm,no.entForm.25cm,no.entForm.30cm\n"),file=FL.results.sum,append=TRUE)
cat(paste("visit.idx,date,time,transect,flag,start.time,no.ent.primTransect,no.ent.pairTransect,no.ent.foundThis,transect,rtw.change(ft),FLAG.entForm.5cm,FLAG.entForm.10cm,FLAG.entForm.15cm,FLAG.entForm.20cm,FLAG.entForm.25cm,FLAG.entForm.30cm,no.entForm.5cm,no.entForm.10cm,no.entForm.15cm,no.entForm.20cm,no.entForm.25m,no.entForm.30cm\n"),file=FL.results.mass1,append=TRUE)

# loopping through all 2600 entrapment field visit in 2011 (537 entrapmenets found)
no.EntVisit <- dim(myData.EntVisit)[1]		 	# the total number of survey events (both Y and N) in 2011
for (thisVisit in seq(from=1,to=no.EntVisit))
{	
	# note: there are 2663 events, for each event I'd like to have the wse time series being plotted 
	if (thisVisit < 10)
	{
		visit.string <- paste("visit000",thisVisit,sep="")
	}else if (thisVisit < 100)
	{
		visit.string <- paste("visit00",thisVisit,sep="")
	}else if (thisVisit < 1000)
	{
		visit.string <- paste("visit0",thisVisit,sep="")
	}else
	{
		visit.string <- paste("visit",thisVisit,sep="")
	}
	fl.string.wse <- paste(visit.string,"_wse.pdf",sep="")
	fl.string.rtw <- paste(visit.string,"_rtw.pdf",sep="")
	
	cat(paste("Processing ",visit.string,"......\n",sep=""))
	cat(paste("Processing ",visit.string,"......\n",sep=""),file=FL.LOG,append=TRUE)
	
	# creat a pdf file for each of the visit to show the water surface elevation time series of all entrapment found from the database in previous 24 hours
	FL.wse.pdf  <- paste(Path.out,fl.string.wse,sep="/")	
	FL.rtw.pdf  <- paste(Path.out,fl.string.rtw,sep="/")	
	if (file.exists(FL.wse.pdf)){print(paste(FL.wse.pdf, "exist.Delete it!"));file.remove(FL.wse.pdf)}
	if (file.exists(FL.rtw.pdf)){print(paste(FL.rtw.pdf, "exist.Delete it!"));file.remove(FL.rtw.pdf)}

	# infomation of this visit
	thisVisit.date     <- myData.EntVisit[thisVisit,"date"]
	thisVisit.time     <- myData.EntVisit[thisVisit,"time"]
	thisVisit.transect <- myData.EntVisit[thisVisit,"transect"]
	thisVisit.year     <- myData.EntVisit[thisVisit,"year"]
	thisVisit.month    <- myData.EntVisit[thisVisit,"month"]
	thisVisit.day      <- myData.EntVisit[thisVisit,"day"]
	thisVisit.hour     <- myData.EntVisit[thisVisit,"hour.adj"]	# note use adjusted hour   where minutes have been rounded to nearest tens of minutes
	thisVisit.minute   <- myData.EntVisit[thisVisit,"minute.adj"]	# note use adjusted minute where minutes have been rounded to nearest tens of minutes
	thisVisit.startT   <- myData.EntVisit[thisVisit,"start.time"]
	thisVisit.flag     <- myData.EntVisit[thisVisit,"Entrapments"]
	
	# entrapments associated with this transect which ware found from the entrapment data base
	   ent.primTransect <- myData.EntHist[myData.EntHist[,"transect"] == thisVisit.transect,]
	no.ent.primTransect <- dim(ent.primTransect)[1]
	
	# survey conducted in transect pair: paring is like {1,2}, {49,50}...
	if ((thisVisit.transect %% 2) == 0)
	{
		pairing.transect <- thisVisit.transect - 1	# odd transect always pairing with a transect after
	}else
	{
		pairing.transect <- thisVisit.transect + 1	# even transect always pairing with a transect before
	}
	cat(paste("The transect pair corresponds to visit ",thisVisit," consists of transect ",thisVisit.transect, " and transect ", pairing.transect,"\n",sep=""))
	cat(paste("The transect pair corresponds to visit ",thisVisit," consists of transect ",thisVisit.transect, " and transect ", pairing.transect,"\n",sep=""),file=FL.LOG,append=TRUE)
	
	# entrapments associated with the pairing transect which ware found from the entrapment data base
	   ent.pairTransect <- myData.EntHist[myData.EntHist[,"transect"] == pairing.transect,]
	no.ent.pairTransect <- dim(ent.pairTransect)[1]
	
	#
	# Keep both options because I need to evaluate the field visits based on the primary transect only and to evaluate the 
	# all entrapments found from the database for the pair of transects of ths field visit for this survey event
	# *********************************************************************************
	# put both [no.ent.foundThis] and [no.ent.pairFound] entrapments fround from the database into a list called [ent.foundThis]
	# *********************************************************************************
	
	# no.ent.foundThis <- no.ent.primTransect
	no.ent.foundThis <-    no.ent.primTransect + no.ent.pairTransect
	   ent.foundThis <- rbind(ent.primTransect,ent.pairTransect)
	
	# output information out to the summary file
	cat(paste(paste(thisVisit,thisVisit.date,thisVisit.time,thisVisit.transect,thisVisit.flag,thisVisit.startT,no.ent.primTransect,no.ent.pairTransect,no.ent.foundThis,sep=","),",",sep=""),file=FL.results.sum,   append=TRUE)
	cat(paste(paste(thisVisit,thisVisit.date,thisVisit.time,thisVisit.transect,thisVisit.flag,thisVisit.startT,no.ent.primTransect,no.ent.pairTransect,no.ent.foundThis,sep=","),",",sep=""),file=FL.results.detail,append=TRUE)
	# note: the output does not finiss yet.  The evaulation results will be followed

	# *****************************************************************************************
	# *****************************************************************************************
	# *****************************************************************************************
	# evaluate the creteria based on water surface elevation drops 
	# *****************************************************************************************
	# *****************************************************************************************
	# *****************************************************************************************	
	# check each of the entrapment to see if there is ann entrapment did form in the 24 hour prior to the stating time of the survey
	if (no.ent.foundThis > 0)
	{
		cat(paste(no.ent.foundThis," entrapments are found for Visit",thisVisit," with ",no.ent.primTransect," from primary and ",no.ent.pairTransect," from paired transects!\n",sep=""))
		cat(paste(no.ent.foundThis," entrapments are found for Visit",thisVisit," with ",no.ent.primTransect," from primary and ",no.ent.pairTransect," from paired transects!\n",sep=""),file=FL.LOG,append=TRUE)
		
		# create a pdf ONLY when there are data to evaluate
		pdf(file = FL.wse.pdf,paper="a4r", width=0, height=0)


		# initialize the entrapment formation-flags for the [no.ent.foundThis] entrapments found from the database associated with the pair of transect of current survey event
		flag.entSuit.5cm.array  <- rep(FALSE,no.ent.foundThis)
		flag.entSuit.10cm.array <- rep(FALSE,no.ent.foundThis)
		flag.entSuit.15cm.array <- rep(FALSE,no.ent.foundThis)
		flag.entSuit.20cm.array <- rep(FALSE,no.ent.foundThis)
		flag.entSuit.25cm.array <- rep(FALSE,no.ent.foundThis)
		flag.entSuit.30cm.array <- rep(FALSE,no.ent.foundThis)
				
		
		# an array to record the maximum water surface width changes of the the entrapments associated with this survey event
		change.rtw.array <- rep(-999,no.ent.foundThis)	# initialize with -999
		
		# there are at most two transects associated with this survey event (due to pairing)
		unique.transect      <- rep(-999,2)
		rtw.change.thisVisit <- rep(-999,2)	# the maximum change of river top width during the 24 hours prior to thew start of current survey event
		
		# looping through the entrapments int the list
		for (idx.ent in seq(from = 1,to = dim(ent.foundThis)[1]))
		{
		
			cat(paste("Visit",thisVisit,"-entrapment ",idx.ent," out of total ",no.ent.foundThis,"(",no.ent.primTransect,"|",no.ent.pairTransect,") entrapments found for visit ",thisVisit,"!\n",sep=""))
			cat(paste("Visit",thisVisit,"-entrapment ",idx.ent," out of total ",no.ent.foundThis,"(",no.ent.primTransect,"|",no.ent.pairTransect,") entrapments found for visit ",thisVisit,"!\n",sep=""),file=FL.LOG,append=TRUE)

			# initialize the flags for each of the criteria (determine if there was an entrapment formed immediately prior to the survey starting time)
			flag.entSuit.5cm  <- FALSE
			flag.entSuit.10cm <- FALSE
			flag.entSuit.15cm <- FALSE
			flag.entSuit.20cm <- FALSE
			flag.entSuit.25cm <- FALSE
			flag.entSuit.30cm <- FALSE
			


			# get the transect index and entrapment elevation of current entrapment in the list 
			current.transect <- ent.foundThis[idx.ent,"transect"]
			current.entElev  <- ent.foundThis[idx.ent,"elevation(ft)"]
			
			# cut off line to satiafy the +/- 15 cm difference between wse elev at the start of the sampling window and the entrapment elevation
			wse.cutoffP <- current.entElev + 15 * cm2foot 	# if rtw of future is below this line, it satify the criteria 1
			wse.cutoffM <- current.entElev - 15 * cm2foot 	# the wse levation at the start of the sampling window should be within this limits
			
			
			# get the MASS1 wse data object name [current.MASS1.obj] of this transect
			if (current.transect < 10)
			{
				current.MASS1.obj <- paste("results.00",current.transect,sep="")
			}else if (current.transect < 100){
				current.MASS1.obj <- paste("results.0",current.transect,sep="")
			}else{
				current.MASS1.obj <- paste("results.",current.transect,sep="")
			}

			# -------------------------------------------------------------------------
			# put the MASS1 wse data object [current.MASS1.obj] (which is actually [results###]) into a working data object [myData.MASS1]
			# -------------------------------------------------------------------------
			command.string <- paste("myData.MASS1  <- ",current.MASS1.obj,sep="")
			eval(parse(text=command.string))
			
			# *********************************************************************************
			# define the limits of the 24 hours segment prior to the starting time of the survey
			# get 24 hours history and 12 hour future data
			# *********************************************************************************
			time0 <- thisVisit.startT - 1   - 1/48	# -1 get to the previous day.  To avoid extraplolation, extend additional half hour by 1/48.			
			time1 <- thisVisit.startT + 1/2 + 1/48	# 1/2 get to 12 hours later.   Extend another half hour
			myData.hourly <- myData.MASS1[(myData.MASS1[,"chron.date"] >= time0) & (myData.MASS1[,"chron.date"] <= time1),]

			# get the hourly wse and rtw for the past 24 hours
		       time.hourly  <- myData.hourly[,"chron.date"]
			wse.hourly  <- myData.hourly[,"wse(ft)"]
			rtw.hourly  <- myData.hourly[,"riverTopWidth(ft)"]
			dis.hourly  <- myData.hourly[,"discharge(ft3/s)"]
			
			# interpolate the 24 hour wse and rtw time series into tens of minutes interval
			# this returns a list of x and y
			time.10min <- seq(from = time0, to = time1, by = 1/(24*6))	# note the increment of chron is 1 day, so to get to the tens of minutes, need to divide 24 to reach hour level and then divide by 6 to reach tens of minute level
			tmp.wse <- approx(time.hourly,wse.hourly,time.10min,method="linear",rule=2,ties=mean)
			tmp.rtw <- approx(time.hourly,rtw.hourly,time.10min,method="linear",rule=2,ties=mean)
			tmp.dis <- approx(time.hourly,dis.hourly,time.10min,method="linear",rule=2,ties=mean)
			wse.10min <- tmp.wse[[2]]	# list 2 is the interploated values
			rtw.10min <- tmp.rtw[[2]]	# list 2 is the interploated values
			dis.10min <- tmp.dis[[2]]	# list 2 is the interploated values

			# assembled the interpolated data into a new data frame 
			myData <- data.frame(time.10min = time.10min,
					      wse.10min  = wse.10min,
					      rtw.10min  = rtw.10min,
					      dis.10min  = dis.10min)
		  names(myData)<- c("chron.date","wse(ft)","riverTopWidth(ft)","discharge(ft3/s)")			                          
			no.points <- dim(myData)[1]


			
			# add a flag to distinct "history" and "future"
			myData[,"type"] <- rep("history",dim(myData)[1])			# intitialize "type" with "history"
			myData[myData[,"chron.date"] >= thisVisit.startT,"type"] <- "future"	# change "history" to "future" for future data including the data at "thisVisit.startT"
			
			# find the maximum rtw in history and the minimum rtw in the future
			rtw.hist.max <-   max(myData[myData[,"type"] == "history","riverTopWidth(ft)"],na.rm=TRUE)
			idx.hist.max <- which(myData[myData[,"type"] == "history","riverTopWidth(ft)"] == rtw.hist.max)
			  T.hist.max <-       myData[myData[,"type"] == "history",][idx.hist.max,"chron.date"]		# the time point of the maximum river top width in history data
		      wse.T.hist.max <-       myData[myData[,"type"] == "history",][idx.hist.max,"wse(ft)"]		# the wse(ft) at the start of the sampling window
			
			rtw.futu.min <- max(myData[myData[,"type"] == "future", "riverTopWidth(ft)"],na.rm=TRUE)
			myData[,"rtw.hist.max"] <- rep(rtw.hist.max,dim(myData)[1])
			myData[,"rtw.futu.min"] <- rep(rtw.futu.min,dim(myData)[1])
			
			# calculate the rtw.cutoff value for plotting purpose
			rtw.cutoff <- rtw.hist.max - rtw.criteria	# if rtw of future is below this line, it satify the criteria 1
			
			# the maximum change of rtw from history to future
			change.rtw <- rtw.hist.max - rtw.futu.min
			change.rtw.array[idx.ent] <- change.rtw	
			myData[,"rtw.diff"] <- rtw.hist.max - myData[,"riverTopWidth(ft)"]	# this is used to check against "rtw.criteria" for the futur data to find the start of the sampling window when "change.rtw" > "rtw.criteria"
			
			# maximum wse in history 
			wse.max  <- max(myData[myData[,"type"] == "history","wse(ft)"],na.rm=TRUE)
			wse.drop <- wse.max - current.entElev	# the minimum wse drop in previous 24 hours to the entrapment elevation
		
			# -------------------------------------------------------------------------
			# some quantities can only be calculated once a sampling window can be identified. So they need to be initialized
			# define wse.diff as the difference between wse at the start of the sampling window from the enttrapment elevation
			# -------------------------------------------------------------------------
			    T.start    <- NA
			    T.start.hr <- NA
			wse.T.start    <- NA
			rtw.T.start    <- NA
			elev.diff      <- -999
			
			
			# -------------------------------------------------------------------------
			# there will be at most two transects (due to pairing), so there are at most two wse.drop(ft) and rtw(ft) values associated with current survey event
			# -------------------------------------------------------------------------
			if (idx.ent == 1)
			{
				no.uni.transect <- 1
				unique.transect[no.uni.transect]      <- current.transect
				rtw.change.thisVisit[no.uni.transect] <- change.rtw
			}else{
				if (current.transect != unique.transect[no.uni.transect])	# from different transect
				{

					no.uni.transect <- no.uni.transect + 1
					if (no.uni.transect <= 2)
					{
						unique.transect[no.uni.transect]      <- current.transect
						rtw.change.thisVisit[no.uni.transect] <- change.rtw				
					}else{
						cat(paste(paste("Visit",thisVisit,"-ent:",idx.ent,sep=""),"\tYou cannot have more than 2 transects surveyed for each field visit!\n",sep=""))
						cat(paste(paste("Visit",thisVisit,"-ent:",idx.ent,sep=""),"\tYou cannot have more than 2 transects surveyed for each field visit!\n",sep=""),file=FL.LOG,append=TRUE)
						die;
					}
				}
			}

			
			# *************************************************************************
			# first criteria: determine if there is enough river top surface change
			# *************************************************************************
			criteria1.satisfy <- FALSE	# rtw drop from history max to future min is larger than or equal to 32.4 ft
			criteria2.satisfy <- FALSE	# there is a longer than 2 hours sampling window associated with the sufficient rwt drop
			criteria3.satisfy <- FALSE	# the start time of such a window is earlier than 21:00 PM
			criteria4.satisfy <- FALSE	# at the start of sampling window, the difference between the wse and the ent elev is within a specified threshold say +/-15 cm.
			
		
			# -------------------------------------------------------------------------
			# criteria 1: sufficient rtw change
			# -------------------------------------------------------------------------
			if (change.rtw >= rtw.criteria)	# satify the river top width drop criteria
			{
				criteria1.satisfy <- TRUE
				
				cat(paste("\tcriteria 1(Y): rtw change criteria satisfied\n",sep=""))
				cat(paste("\tcriteria 1(Y): rtw change criteria satisfied\n",sep=""),file=FL.LOG,append=TRUE)
				
				# determinef the existence of a larger than 2 hours window with at least 32.4 ft rtw.change.  If yes, find the starting time of this window: T.start									
				myData[,"rtw.big"]  <- rep("N",dim(myData)[1]) 				# initialize: "Y" stands for the rtw.diff is larger than or equal to 32.4 feet
				myData[myData[,"rtw.diff"] >= rtw.criteria,"rtw.big"] <- "Y"		# flag that hour with more than 32.4 ft of rtw change
				
				# need to check if there are two consecutive hours in the "future" satifying this criteria
				myData.future <- myData[myData[,"type"] == "future",]
				
				# check the runs of "rtw.big"
				run.rtw <- rle(myData.future[,"rtw.big"])	# this generate run results in a list of 2 componenets: lengths and values
				
				# convert the "rle" list into a data frame to easily locate the row index of the start of a sampling window bigger than 2 hours
				df.rtw <- data.frame(index   = seq(from = 1, to =length(run.rtw$lengths)),	# the index in the runs
				                     lengths = run.rtw$lengths,					# the lengths of each run segment
				                     values  = run.rtw$values)					# the value   of each run segment
				                     
				# isolate the run results for "Y" run segment longer than 12 ten minutes (i.e., 2 hours)
				df.rtw.long <- df.rtw[df.rtw[,"lengths"]>=12 & df.rtw[,"values"]=="Y",]		# the elements in the runs results data frame which satify more than 2 hours of exceeding 32.4 ft rtw changes
				
				# -----------------------------------------------------------------
				# test the existence of longer than 2 hour "Y" run segment for "rtw.diff"
				# criteria 2: the sufficient rtw change does lead to a longer than 2 hour sampling window
				# -----------------------------------------------------------------
				if (dim(df.rtw.long) > 0)							# means at least there is such a run with a length longer than 2 for "Y" values
				{		
					criteria2.satisfy <- TRUE
					
					# get the index in the run for the first window identified
					idx.in.runs <- df.rtw.long[1,"index"]					# this is the index in the runs results which satifies the criteria
					idx.T.start <- sum(df.rtw[seq(from=1,to=idx.in.runs),"lengths"]) - df.rtw[idx.in.runs,"lengths"] + 1	# this is the start point of the first "Y" run longer than 2 hours

					    # this is the start of the sampling window
					    T.start <- myData.future[idx.T.start,"chron.date"]			# the time of the start of the sampling window.
					    
					    # aa quick test: T.start has to be a single time point in the future time series.  
					    if(any(is.na(myData.future[idx.T.start,"chron.date"]))){stop(cat(paste("T.start is not right!\n")))}

					cat(paste("\tcriteria 2(Y): a larger than 2 hours sampling window with sufficient rtw change has been identified at ",T.start,"\n",sep=""))
					cat(paste("\tcriteria 2(Y): a larger than 2 hours sampling window with sufficient rtw change has been identified at ",T.start,"\n",sep=""),file=FL.LOG,append=TRUE)

					
					# judge if the start time is beyond 9:00 pm in the afternoon
					tmp.split1 <- unlist(strsplit(sub("\\)","",sub("\\(","",as.character(T.start))),"\\s+",perl=TRUE))
					tmp.split2 <- tmp.split1[seq(from=2,to=length(tmp.split1),by=2)]
					tmp.split3 <- unlist(strsplit(tmp.split2,":",perl=TRUE))
					T.start.hr <- as.numeric(tmp.split3[seq(from=1,to=length(tmp.split3),by=3)])	# this is the hour of the start of the sampling window
					
					# -----------------------------------------------------------------
					# criteria 3: the starting sampling time is earlier than 9:00 pm
					# -----------------------------------------------------------------
					if (T.start.hr <= 21)	# 9:00 pm in the afternoon
					{
						
						criteria3.satisfy <- TRUE
						
						cat(paste("\tcriteria 3(Y): the starting hour of the sampling window is earlier than 9:00 pm!\n",sep=""))
						cat(paste("\tcriteria 3(Y): the starting hour of the sampling window is earlier than 9:00 pm!\n",sep=""),file=FL.LOG,append=TRUE)
					
	
						# get the WSE at the start of the sampling window
						wse.T.start <- myData[myData[,"chron.date"] == T.start,"wse(ft)"]	# the wse(ft) at the start of the sampling window
						rtw.T.start <- myData[myData[,"chron.date"] == T.start,"riverTopWidth(ft)"]

						# -------------------------------------------------
						# criteria 4: the difference between wse at the identified sampling starting point from the entrapment elevation is within a given threshold (e.g., it is the +/-15 cm)
						# -------------------------------------------------
						cat(paste("\tcriteria 4(Y/N): evaluate criteria 4: the wse and entrapment elevation is matched within a limits!\n",sep=""))
						cat(paste("\tcriteria 4(Y/N): evaluate criteria 4: the wse and entrapment elevation is matched within a limits!\n",sep=""),file=FL.LOG,append=TRUE)

						


						# calculate the difference of wse and entrapment elevation
						elev.diff <- abs(current.entElev - wse.T.start)


						# initialize the flags for each of the criteria (determine if there was an entrapment formed immediately prior to the survey starting time)
						flag.entSuit.5cm  <- ifelse(elev.diff * foot2cm <= 5, TRUE,FALSE)
						flag.entSuit.10cm <- ifelse(elev.diff * foot2cm <= 10,TRUE,FALSE)
						flag.entSuit.15cm <- ifelse(elev.diff * foot2cm <= 15,TRUE,FALSE)
						flag.entSuit.20cm <- ifelse(elev.diff * foot2cm <= 20,TRUE,FALSE)
						flag.entSuit.25cm <- ifelse(elev.diff * foot2cm <= 25,TRUE,FALSE)
						flag.entSuit.30cm <- ifelse(elev.diff * foot2cm <= 30,TRUE,FALSE)
						
						# this threshol dis the one currently implemented
						criteria4.satisfy <- ifelse(elev.diff * foot2cm <= 15,TRUE,FALSE)
						

						# record the flag determined for current entrapment 
						flag.entSuit.5cm.array[idx.ent]  <- flag.entSuit.5cm
						flag.entSuit.10cm.array[idx.ent] <- flag.entSuit.10cm
						flag.entSuit.15cm.array[idx.ent] <- flag.entSuit.15cm
						flag.entSuit.20cm.array[idx.ent] <- flag.entSuit.20cm
						flag.entSuit.25cm.array[idx.ent] <- flag.entSuit.25cm
						flag.entSuit.30cm.array[idx.ent] <- flag.entSuit.30cm


						# -------------------------------------------------------------------------
						# write out the finding for current entrapment into the "detail" file
						# -------------------------------------------------------------------------
						if (idx.ent == 1)
						{
							cat(paste(paste(              paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,sep=","),current.entElev,flag.entSuit.5cm,flag.entSuit.10cm,flag.entSuit.15cm,flag.entSuit.20cm,flag.entSuit.25cm,flag.entSuit.30cm,T.start,T.start.hr,wse.T.start,rtw.T.start,change.rtw,elev.diff*foot2cm,current.MASS1.obj,time0,time1,sep=","),"\n",sep=""),file=FL.results.detail,append=TRUE)
						}else{
							cat(paste(paste(c(",,,,,,,,"),paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,sep=","),current.entElev,flag.entSuit.5cm,flag.entSuit.10cm,flag.entSuit.15cm,flag.entSuit.20cm,flag.entSuit.25cm,flag.entSuit.30cm,T.start,T.start.hr,wse.T.start,rtw.T.start,change.rtw,elev.diff*foot2cm,current.MASS1.obj,time0,time1,sep=","),"\n",sep=""),file=FL.results.detail,append=TRUE)
						}

					}else{
						criteria3.satisfy <- FALSE
						if (idx.ent == 1)
						{
							cat(paste(paste(                 paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,current.entElev,sep=","),"\n",sep=""),sep=""),file=FL.results.detail,append=TRUE)
						}else{
							cat(paste(paste(c(",,,,,,,,,"),paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,current.entElev,sep=","),"\n",sep=""),sep=""),file=FL.results.detail,append=TRUE)
						}
						
						
						cat(paste("\tcriteria 3(N): the starting hour of the sampling window is LATER than 9:00 pm!\n",sep=""))
						cat(paste("\tcriteria 3(N): the starting hour of the sampling window is LATER than 9:00 pm!\n",sep=""),file=FL.LOG,append=TRUE)
					}
					
				}else{
					criteria2.satisfy <- FALSE
					T.start <- NA
					if (idx.ent == 1)
					{
						cat(paste(paste(                 paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,current.entElev,sep=","),"\n",sep=""),sep=""),file=FL.results.detail,append=TRUE)
					}else{
						cat(paste(paste(c(",,,,,,,,,"),paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,current.entElev,sep=","),"\n",sep=""),sep=""),file=FL.results.detail,append=TRUE)
					}					
					# cannot find a larger than 2 hour window for "Y" values where "Y" means exceeding 32.4 ft rtw change.
					cat(paste("\tcriteria 2(N): a larger than 2 hours sampling window with sufficient rtw change has NOT been identified at ",T.start,"\n",sep=""))
					cat(paste("\tcriteria 2(N): a larger than 2 hours sampling window with sufficient rtw change has NOT been identified at ",T.start,"\n",sep=""),file=FL.LOG,append=TRUE)
					
				}					
			}else{
				# the rtw change is smaller than 32.4 feet
				criteria1.satisfy <- FALSE			
				T.start <- NA
				if (idx.ent == 1)
				{
					cat(paste(paste(                 paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,current.entElev,sep=","),"\n",sep=""),sep=""),file=FL.results.detail,append=TRUE)
				}else{
					cat(paste(paste(c(",,,,,,,,,"),paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,current.entElev,sep=","),"\n",sep=""),sep=""),file=FL.results.detail,append=TRUE)
				}
				cat(paste("criteria 1(N): rtw change criteria NOT satisfied\n",sep=""))
				cat(paste("criteria 1(N): rtw change criteria NOT satisfied\n",sep=""),file=FL.LOG,append=TRUE)				
			}
			
			
			cat(paste("starting plotting -----------------!\n",sep=""))
			cat(paste("starting plotting -----------------!\n",sep=""),file=FL.LOG,append=TRUE)
			#
			# -------------------------------------------------------------------------
			# plot the time series for current entrapment found from the database for current event
			# -------------------------------------------------------------------------
			# plot to the pdf file for all events
			# -------------------------------------------------------------------------
			# dev.set(2)
		#	par(mfrow=c(2,2))
		# 	plot(myData[,"chron.date"],myData[,"wse(ft)"],
		# 	     type = "b",
		# 	     lty  = 1,
		# 	     pch  = 16,
		# 	     col  = "red",
		# 	     xlab = "time",
		#	     ylab = "Water Surface Elevation (ft)",
		# 	     ylim = c(min(min(myData[,"wse(ft)"]),current.entElev),max(max(myData[,"wse(ft)"]),current.entElev)),
		# 	     main = paste(paste(paste("Event",thisVisit,sep=""),"-transect",current.transect,paste("-ent",idx.ent,sep=""),": ent elev (",round(current.entElev,digits=0),"ft) and drop: ",round((elev.diff * foot2cm),digits=2),"(cm)",sep=""),paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,sep=","),sep="\n"))
		# 	lines(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","wse(ft)"],type = "b",lty=1,pch=16,col="blue")
		#        points(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","wse(ft)"],type = "b",lty=1,pch=16,col="blue")
		# 	abline(h = current.entElev,col="cyan",lty=1)
		# 	abline(h = c(wse.cutoffP,wse.cutoffM),col=c("grey","grey"),lty=c(2,2))	# the limits where the wse at the start of sampling window should fall in			
		#	abline(h = c(wse.T.hist.max,wse.T.start),col=c("red","green"),lty=c(1,1))
		# 	abline(v = c(thisVisit.startT,T.hist.max,T.start),col=c("black","red","green"),lty=c(1,1,1),lwd=c(1,1,1))
		# 
		# 	text(myData[no.points/2,"chron.date"],current.entElev,paste("ent elev:",round(current.entElev,digits=2),"(ft): ",flag.entSuit.5cm,",",flag.entSuit.10cm,",",flag.entSuit.15cm,",",flag.entSuit.20cm,",",flag.entSuit.25cm,",",flag.entSuit.30cm,sep=""),offset=0.5)
		# 	text(myData[no.points/2,"chron.date"],wse.T.start,paste("wse.T.start:",round(wse.T.start,digits=2),"(ft)",sep=""),offset=0.5)
		# 
		# 
		#	plot(myData[,"chron.date"],myData[,"riverTopWidth(ft)"],
		# 	     type = "b",
		# 	     lty  = 1,
		# 	     pch  = 16,
		# 	     col  = "red",
		# 	     xlab = "time (of prior 24 hours)",
		# 	     ylab = "River Top Width (ft)",
		#	     main = paste(paste("(Event",thisVisit,sep=""),"-transect",current.transect,") RTW(ft): ent elev (",round(current.entElev,digits=0),"ft)",sep=""))
		# 	lines(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","riverTopWidth(ft)"],type = "b",lty=1,pch=16,col="blue")
		#        points(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","riverTopWidth(ft)"],type = "b",lty=1,pch=16,col="blue")			     
		# 	     abline(h = c(rtw.hist.max,rtw.T.start,rtw.cutoff),col=c("red","cyan","black"),lty=c(1,1,2))
		# 	     abline(v = c(thisVisit.startT,T.hist.max,T.start),col=c("black","red","green"),lty=c(1,1,1),lwd=c(1,1,1))			     
		# 		
		# 	text(myData[no.points/2,"chron.date"],rtw.hist.max,paste("rtw.hist.max:",round(rtw.hist.max,digits=2),"(ft)",sep=""),offset=0.5)
		#	text(myData[no.points/2,"chron.date"],rtw.T.start, paste("rtw.T.start:", round(rtw.T.start, digits=2),"(ft)",sep=""),offset=0.5)
		# 
		# 	plot(myData[,"chron.date"],myData[,"discharge(ft3/s)"],
		# 	     type = "b",
		# 	     lty  = 1,
		# 	     pch  = 16,
		# 	     col  = "red",
		#	     xlab = "time (of prior 24 hours)",
		# 	     ylab = "discharge(ft3/s)",
		# 	     main = paste(paste("(Event",thisVisit,sep=""),"-transect",current.transect,") discharge(ft3/s): ent elev (",round(current.entElev,digits=0),"ft)",sep=""))
		# 	lines(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","discharge(ft3/s)"],type = "b",lty=1,pch=16,col="blue")
		#        points(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","discharge(ft3/s)"],type = "b",lty=1,pch=16,col="blue")			     			     
		# 	     abline(v  =c(thisVisit.startT,T.hist.max,T.start),col=c("black","red","green"),lty=c(1,1,1),lwd=c(1,1,1))
		# 
		#	plot(myData[,"wse(ft)"],myData[,"riverTopWidth(ft)"],
		# 	     type = "p",
		# 	     lty  = 1,
		# 	     pch  = 16,
		# 	     col  = "cyan",
		# 	     xlab = "Water Surface Elevation (ft)",
		# 	     ylab = "River Top Width (ft)",
		#	     main = paste(paste("(Event",thisVisit,sep=""),"-transect",current.transect,")WSE vs RTW :ent elev (",round(current.entElev,digits=0),"ft)",sep=""))
		#	     abline(v = c(thisVisit.startT,T.hist.max,T.start),col=c("black","red","green"),lty=c(1,1,1),lwd=c(1,1,1)) 	

			# -------------------------------------------------------------------------
			# plot to the pdf file for current even
			# -------------------------------------------------------------------------
			dev.set(3)
			par(mfrow=c(2,2))
			plot(myData[,"chron.date"],myData[,"wse(ft)"],
			     type = "b",
			     lty  = 1,
			     pch  = 16,
			     col  = "red",
			     xlab = "time",
			     ylab = "Water Surface Elevation (ft)",
			     ylim = c(min(min(myData[,"wse(ft)"]),current.entElev),max(max(myData[,"wse(ft)"]),current.entElev)),
			     main = paste(paste(paste("Event",thisVisit,sep=""),"-transect",current.transect,paste("-ent",idx.ent,sep=""),": ent elev (",round(current.entElev,digits=0),"ft) and drop: ",round((elev.diff * foot2cm),digits=2),"(cm)",sep=""),paste(criteria1.satisfy,criteria2.satisfy,criteria3.satisfy,criteria4.satisfy,sep=","),sep="\n"))
			lines(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","wse(ft)"],type = "b",lty=1,pch=16,col="blue")
		       points(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","wse(ft)"],type = "b",lty=1,pch=16,col="blue")
			abline(h = current.entElev,col="cyan",lty=1)
			abline(h = c(wse.cutoffP,wse.cutoffM),col=c("grey","grey"),lty=c(2,2))	# the limits where the wse at the start of sampling window should fall in			
			abline(h = c(wse.T.hist.max,wse.T.start),col=c("red","green"),lty=c(1,1))
			abline(v = c(thisVisit.startT,T.hist.max,T.start),col=c("black","red","green"),lty=c(1,1,1),lwd=c(1,1,1))

			text(myData[no.points/2,"chron.date"],current.entElev,paste("ent elev:",round(current.entElev,digits=2),"(ft): ",flag.entSuit.5cm,",",flag.entSuit.10cm,",",flag.entSuit.15cm,",",flag.entSuit.20cm,",",flag.entSuit.25cm,",",flag.entSuit.30cm,sep=""),offset=0.5)
			text(myData[no.points/2,"chron.date"],wse.T.start,paste("wse.T.start:",round(wse.T.start,digits=2),"(ft)",sep=""),offset=0.5)


			plot(myData[,"chron.date"],myData[,"riverTopWidth(ft)"],
			     type = "b",
			     lty  = 1,
			     pch  = 16,
			     col  = "red",
			     xlab = "time (of prior 24 hours)",
			     ylab = "River Top Width (ft)",
			     main = paste(paste("(Event",thisVisit,sep=""),"-transect",current.transect,") RTW(ft): ent elev (",round(current.entElev,digits=0),"ft)",sep=""))
			lines(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","riverTopWidth(ft)"],type = "b",lty=1,pch=16,col="blue")
		       points(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","riverTopWidth(ft)"],type = "b",lty=1,pch=16,col="blue")			     
			     abline(h = c(rtw.hist.max,rtw.T.start,rtw.cutoff),col=c("red","cyan","black"),lty=c(1,1,2))
			     abline(v = c(thisVisit.startT,T.hist.max,T.start),col=c("black","red","green"),lty=c(1,1,1),lwd=c(1,1,1))			     
				
			text(myData[no.points/2,"chron.date"],rtw.hist.max,paste("rtw.hist.max:",round(rtw.hist.max,digits=2),"(ft)",sep=""),offset=0.5)
			text(myData[no.points/2,"chron.date"],rtw.T.start, paste("rtw.T.start:", round(rtw.T.start, digits=2),"(ft)",sep=""),offset=0.5)

			plot(myData[,"chron.date"],myData[,"discharge(ft3/s)"],
			     type = "b",
			     lty  = 1,
			     pch  = 16,
			     col  = "red",
			     xlab = "time (of prior 24 hours)",
			     ylab = "discharge(ft3/s)",
			     main = paste(paste("(Event",thisVisit,sep=""),"-transect",current.transect,") discharge(ft3/s): ent elev (",round(current.entElev,digits=0),"ft)",sep=""))
			lines(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","discharge(ft3/s)"],type = "b",lty=1,pch=16,col="blue")
		       points(myData[myData[,"type"]=="future","chron.date"],myData[myData[,"type"]=="future","discharge(ft3/s)"],type = "b",lty=1,pch=16,col="blue")			     			     
			     abline(v  =c(thisVisit.startT,T.hist.max,T.start),col=c("black","red","green"),lty=c(1,1,1),lwd=c(1,1,1))

			plot(myData[,"wse(ft)"],myData[,"riverTopWidth(ft)"],
			     type = "p",
			     lty  = 1,
			     pch  = 16,
			     col  = "cyan",
			     xlab = "Water Surface Elevation (ft)",
			     ylab = "River Top Width (ft)",
			     main = paste(paste("(Event",thisVisit,sep=""),"-transect",current.transect,")WSE vs RTW :ent elev (",round(current.entElev,digits=0),"ft)",sep=""))
			     abline(v = c(thisVisit.startT,T.hist.max,T.start),col=c("black","red","green"),lty=c(1,1,1),lwd=c(1,1,1)) 	
			
		}
		
		
		# ---------------------------------------------------------------------------------
		# for current survey event, we have evaluated all [no.ent.foundThis] entrapment found from the database to have determined if there are any entrapments formed at their locations during the 24 hours prior to the survey starting time
		# if there are at least 5 entrapments formed, we flag it as "Y" otherwise "N"
		# ---------------------------------------------------------------------------------
		FLAG.entForm.5cm  <- ifelse(sum(flag.entSuit.5cm.array) >=1,TRUE,FALSE)
		FLAG.entForm.10cm <- ifelse(sum(flag.entSuit.10cm.array)>=1,TRUE,FALSE)
		FLAG.entForm.15cm <- ifelse(sum(flag.entSuit.15cm.array)>=1,TRUE,FALSE)
		FLAG.entForm.20cm <- ifelse(sum(flag.entSuit.20cm.array)>=1,TRUE,FALSE)
		FLAG.entForm.25cm <- ifelse(sum(flag.entSuit.25cm.array)>=1,TRUE,FALSE)
		FLAG.entForm.30cm <- ifelse(sum(flag.entSuit.30cm.array)>=1,TRUE,FALSE)
		
		# the actual number of entrapment formed during the past 24 hours
		no.entForm.5cm  <- sum(flag.entSuit.5cm.array) 
		no.entForm.10cm <- sum(flag.entSuit.10cm.array)
		no.entForm.15cm <- sum(flag.entSuit.15cm.array)
		no.entForm.20cm <- sum(flag.entSuit.20cm.array)
		no.entForm.25cm <- sum(flag.entSuit.25cm.array)
		no.entForm.30cm <- sum(flag.entSuit.30cm.array)
		
		# the statistics of the river top width change 
		change.rtw.min <- min(change.rtw.array)
		change.rtw.max <- max(change.rtw.array)
		change.rtw.050 <- median(change.rtw.array)
		
		# ---------------------------------------------------------------------------------
		# write the wse.drop and rtw.change of the current event out
		# ---------------------------------------------------------------------------------
		for (index in seq(from=1,to=no.uni.transect))
		{
			cat(paste(paste(thisVisit,thisVisit.date,thisVisit.time,thisVisit.transect,thisVisit.flag,thisVisit.startT,no.ent.primTransect,no.ent.pairTransect,no.ent.foundThis,unique.transect[index],rtw.change.thisVisit[index],FLAG.entForm.5cm,FLAG.entForm.10cm,FLAG.entForm.15cm,FLAG.entForm.20cm,FLAG.entForm.25cm,FLAG.entForm.30cm,no.entForm.5cm,no.entForm.10cm,no.entForm.15cm,no.entForm.20cm,no.entForm.25cm,no.entForm.30cm,sep=","),"\n",sep=""),file=FL.results.mass1,append=TRUE)
		}
		
		# ---------------------------------------------------------------------------------
		# write out the finding
		# ---------------------------------------------------------------------------------
		cat(paste(paste(FLAG.entForm.5cm,FLAG.entForm.10cm,FLAG.entForm.15cm,FLAG.entForm.20cm,FLAG.entForm.25cm,FLAG.entForm.30cm,no.entForm.5cm,no.entForm.10cm,no.entForm.15cm,no.entForm.20cm,no.entForm.25cm,no.entForm.30cm,sep=","),"\n", sep=""),file=FL.results.sum,append=TRUE)
		
		# close the pdf file
		dev.off(3)
	}else{
		cat(paste(paste(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,sep=","),"\n", sep=""),file=FL.results.sum,append=TRUE)
		cat(paste(paste(c(",,,,,,,,,,,"),"\n",sep=""),sep=""),file=FL.results.detail,append=TRUE)
		
	}	
}
dev.off(2)


cat(paste("complete the loop of visit and entrapmenet\n",sep=""))
cat(paste("complete the loop of visit and entrapmenet\n",sep=""),file=FL.LOG,append=TRUE)

# post-processing
FL.results.tab    <- paste(Path.out,"results_table.csv",sep="/")	
if (file.exists(FL.results.tab))   {print(paste(FL.results.tab,   "exist.Delete it!"));file.remove(FL.results.tab)}

FL.results_more.pdf    <- paste(Path.out,"results_more.pdf",       sep="/")	
if (file.exists(FL.results_more.pdf))   {print(paste(FL.results_more.pdf,   "exist.Delete it!"));file.remove(FL.results_more.pdf)}
pdf(file = FL.results_more.pdf,paper="a4r", width=0, height=0)


FL.results.mass1  <- paste(Path.out,"results_mass1.csv", sep="/")	
FL.results.sum    <- paste(Path.out,"results_sum.csv",   sep="/")	
if (!file.exists(FL.results.mass1)) {stop(paste(" INPUT data file [",FL.results.mass1,"]  does NOT exist!\n",sep=""))}
if (!file.exists(FL.results.sum))   {stop(paste(" INPUT data file [",FL.results.sum,"]  does NOT exist!\n",sep=""))}


myData.rtw <- read.table(file=FL.results.mass1,sep=",",header=TRUE,stringsAsFactors = TRUE)
myData.wse <- read.table(file=FL.results.sum,sep=",",header=TRUE,stringsAsFactors = TRUE)

# only keep the field needed
myData.rtw   <- myData.rtw[,c("flag","rtw.change.ft.")]
myData.wse5  <- myData.wse[,c("flag","FLAG.entForm.5cm")]
myData.wse10 <- myData.wse[,c("flag","FLAG.entForm.10cm")]
myData.wse15 <- myData.wse[,c("flag","FLAG.entForm.15cm")]
myData.wse20 <- myData.wse[,c("flag","FLAG.entForm.20cm")]
myData.wse25 <- myData.wse[,c("flag","FLAG.entForm.25cm")]
myData.wse30 <- myData.wse[,c("flag","FLAG.entForm.30cm")]

names(myData.wse5)  <- c("flag","wse")
names(myData.wse10) <- c("flag","wse")
names(myData.wse15) <- c("flag","wse")
names(myData.wse20) <- c("flag","wse")
names(myData.wse25) <- c("flag","wse")
names(myData.wse30) <- c("flag","wse")

names(myData.rtw) <- c("flag","rtw.big")

table.wse5cm  <- data.frame(table(myData.wse5))
table.wse10cm <- data.frame(table(myData.wse10))
table.wse15cm <- data.frame(table(myData.wse15))
table.wse20cm <- data.frame(table(myData.wse20))
table.wse25cm <- data.frame(table(myData.wse25))
table.wse30cm <- data.frame(table(myData.wse30))


cat(paste("5cm threshold,",sep=""),file=FL.results.tab,append=TRUE)
write.table(table.wse5cm,file=FL.results.tab,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("10cm threshold,",sep=""),file=FL.results.tab,append=TRUE)
write.table(table.wse10cm,file=FL.results.tab,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("15cm threshold,",sep=""),file=FL.results.tab,append=TRUE)
write.table(table.wse15cm,file=FL.results.tab,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("20cm threshold,",sep=""),file=FL.results.tab,append=TRUE)
write.table(table.wse20cm,file=FL.results.tab,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("25cm threshold,",sep=""),file=FL.results.tab,append=TRUE)
write.table(table.wse25cm,file=FL.results.tab,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("30cm threshold,",sep=""),file=FL.results.tab,append=TRUE)
write.table(table.wse30cm,file=FL.results.tab,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)


# rtw threshold
plot.obj <- histogram(~rtw.big | factor(flag),data=myData.rtw)
plot(plot.obj)
plot.obj <- densityplot(~rtw.big, data=myData.rtw, groups=flag,plot.points=FALSE,auto.key=list(space="top",title="Y/N"))
plot(plot.obj)
plot.obj <- bwplot(rtw.big~flag, data=myData.rtw, var.width = TRUE, ylab = "rtw(ft)")
plot(plot.obj)
dev.off()
# 



#
# save the data in R object
#
save(list = ls(all=TRUE),file=FL.results.obj)


# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n2011_HanfordReach_Evaluate_2012NewCriteria.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n2011_HanfordReach_Evaluate_2012NewCriteria.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [2011_HanfordReach_Evaluate_2012NewCriteria.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [2011_HanfordReach_Evaluate_2012NewCriteria.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

#
# put run related information into the log file
#
cat(paste("This run was conducted in ",.Platform$OS.type,"\n",sep=""));
cat(paste("This run was conducted in ",.Platform$OS.type,"\n",sep=""),file=FL.LOG,append=TRUE);

# get the version of R used for this computation and the latest version released
current.Rversion <- R.Version()$version.string
tmp = readLines("http://cran.r-project.org/sources.html")
rls = tmp[grep("latest release", tmp) + 1L]			# the version number is in the next line of 'The latest release'
latest.Rversion  <- gsub("(.*R-|\\.tar\\.gz.*)", "", rls)	# "The latest release: R-2.13.0.tar.gz"
if (latest.Rversion != current.Rversion)
{
	cat(paste("\n\nyou may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""));
	cat(paste("\n\nyou may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""),file=FL.LOG,append=TRUE);
}else{
	cat(paste("\n\nThe R version you are using is the latest version released so far!\n",sep=""))
	cat(paste("\n\nThe R version you are using is the latest version released so far!\n",sep=""),file=FL.LOG,append=TRUE)
}

# get the version information of the attached libraries
cat(paste("\n\nThe information of the packages you used for this calculation:\n"))
cat(paste("\n\nThe information of the packages you used for this calculation:\n"),file=FL.LOG,append=TRUE)
tmp <- sessionInfo()
pkg.loaded <- tmp$otherPkgs
no.pkg.loaded <- length(pkg.loaded)
for (i in seq(1,no.pkg.loaded))
{
	cat(paste(pkg.loaded[[i]]$Package,":",pkg.loaded[[i]]$Version," ",pkg.loaded[[i]]$Date,"\n",sep=" "))
	cat(paste(pkg.loaded[[i]]$Package,":",pkg.loaded[[i]]$Version," ",pkg.loaded[[i]]$Date,"\n",sep=" "),file=FL.LOG,append=TRUE)
}




# -------------------------------------------------------------------------------------------------
# Format of MASS1 data file
# These files contain simulation results from MASS1 for February 15,
# 2011 to August 1, 2011.  A separate file for each WDFW study quadrant,
# results.###.csv, is present, where ### is the quadrant number.  These
# files have the following fields:
# 
#     * date/time string, e.g., "02-15-2011 00:00:00", times are Pacific
#       Standard Time (PST)
# 
#     * quadrant number, 1-360
# 
#     * Columbia river mile near the center of the quadrant
# 
#     * simulated water surface elevation, feet
#  
#     * simulated discharge, cubic feet per second
# 
#     * simulated river top width, feet
# 
# Plots were made of each field:
# 
#     * q###.png are plots of discharge
# 
#     * e###.png water surface elevation
# 
#     * tw###.png top width
# 
# 
# Bill Perkins
# Research Engineer
# Hydrology Group
# 
# Pacific Northwest National Laboratory
# 902 Battelle Boulevard
# P.O. Box 999, MSIN K9-36
# Richland, WA  99352 USA
# Tel:  509-372-6131
# Fax: 509-372-6089
# william.perkins@pnl.gov
# www.pnl.gov
# -------------------------------------------------------------------------------------------------















