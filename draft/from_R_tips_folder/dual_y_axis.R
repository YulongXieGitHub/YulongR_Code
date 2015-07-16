#
# 16_RH_Temperature.R 
# 
#
# June 7, 2009
# Based on Meeting with Bing and Steve on June 5, 2009, we need to generate a plot of Relative Humidity in a room suring a ,onth (say August) for pre- and post-installation 
# HOBO data are used.
# 
#
# Input files:
# 
# Path.in  <- "C:\YuLong_Projects\FY2009_Techval\DataReceived\DataReceived_0606_2009\cleaned_[HOBO_Code(Room_Label)]_[Pre|Post].csv"	
# Prefix "Cleaned" menas some extra text in the file like "logged" and empty rows (at the bottom) are deleted.
#
#
# Output data frames:
# Path.out.pre  <- "C:\YuLong_Projects\FY2009_Techval\16_RH_Temperature"
# [data.pre.wide],   [data.pre.long]	saved in [rawData.Rdata],[rawData.csv]
# [data.post.wide],  [data.post.long]	saved in [rawData.Rdata],[rawData.csv]
# [data.merged.wide],[data.merged.long]	saved in [rawData.Rdata],[rawData.csv]
#
# [summary.pre],[summary.pot]		save in  [rawData_summary.csv]
# -------------------------------------------------------------------------
#
# 	eliminate all stuff
# 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number


month.label <- c( 1,    2,    3,    4,    5,    6,    7,    8,    9,    10,   11,   12)
month.names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

day.label <- c( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
day.names <- c( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)





# ----------------------------------------------- 
# 	two input files for each room, one for pre- and one for post-installation
# -----------------------------------------------
rooms <- c("904009(C210)","904002(C208)","904001(A312)","903996(B114)","903988(B310)","903980(A119TVLounge)","903978(A314)","903977(A208)","903976(A320)","897202(B205)","897201(C211)")

# ----------------------------------------------- 
# 	load libraries
# -----------------------------------------------
library(lattice)
library(chron)	
library(gplots)

# ----------------------------------------------- 
# 	setup plotting limits for x axis
# note: the pre-installation  period is from 11/8/2006 21:00 -> 10/10/2007 8;30
#       the post-installation period is from 11/1/2007 0:45  -> 11/13/2008 7:00
# ----------------------------------------------- 
xlim4plot_pre  <- c(chron(dates="10/30/2006", times="0:0:0",format=c('m/d/y','h:m:s')),chron(dates="10/15/2007",times="23:59:59",format=c('m/d/y','h:m:s')))
xlim4plot_post <- c(chron(dates="10/30/2007", times="0:0:0",format=c('m/d/y','h:m:s')),chron(dates="11/15/2008",times="23:59:59",format=c('m/d/y','h:m:s')))

# ----------------------------------------------- 
# 	change to the script directory
# ----------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2009_Techval/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2009_Techval/0_scripts"
}
setwd(Path.Current)

# -----------------------------------------------
# 	define a summary function
# -----------------------------------------------
source(paste(Path.Current,"my_functions.R",sep="/"))

# -----------------------------------------------
#	setup output and log directory
# -----------------------------------------------
Path.out   <- "../16_RH_Temperature"	# OUTPUT processed result directory
Path.log   <- "../0_log"		# OUTPUT log  directory

if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ----------------------------------------------- 
# 	create a LOG file and a TIME Recording file
# ----------------------------------------------- 
FL.TIME     <- paste(Path.log,"time.log",sep="/")		# OUTPUT Time Log file for all scripts
FL.LOG      <- paste(Path.log,"16_RH_Temperature.log",sep="/")	# OUTPUT Log file

if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for data processing script [16_RH_Temperature.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
          "*                            [16_RH_Temperature.R]                            *",
          "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)


# -----------------------------------------------
# 	define OUTPUT files
# -----------------------------------------------
FL.OBJ      <- paste(Path.out,paste("rawData.Rdata",sep=""),sep="/")					# OUTPUT Rdata
FL.SUM      <- paste(Path.out,paste("rawData_summary.csv",sep=""),sep="/")		 		# OUTPUT SUMMARY general
FL.CSV      <- paste(Path.out,paste("rawData.csv",sep=""),sep="/")					# OUTPUT pre-installation data in CSV for checking purpose
if (file.exists(FL.OBJ)){print(paste(FL.OBJ," exist. Delete it!"));file.remove(FL.OBJ)}			# remove existing OUTPUT files
if (file.exists(FL.SUM)){print(paste(FL.SUM," exist. Delete it!"));file.remove(FL.SUM)}			# remove existing OUTPUT files
if (file.exists(FL.CSV)){print(paste(FL.CSV," exist. Delete it!"));file.remove(FL.CSV)}			# remove existing OUTPUT files
cat(paste("\nDefined an output R object file!\n",sep=""),file=FL.LOG, append=TRUE)


# -----------------------------------------------
# 	input path names
# -----------------------------------------------
Path.in  <- "../DataReceived/DataReceived_0606_2009"	# INPUT  
Path.out <- "../16_RH_Temperature"			# OUTPUT 
if (!file.exists(Path.in)){stop(paste("input data folder does NOT existing\n",sep=""))}
if (!file.exists(Path.out)){stop(paste("output data folder does NOT existing\n",sep=""))}
cat(paste("\nDefined the INPUT/OUTPUT data paths!\n",sep=""),file=FL.LOG, append=TRUE)


# ----------------------------------------------- 
# 	loop throu the rooms
# ----------------------------------------------- 
for (room in rooms)
{
	# -----------------------------------------------
	#    input file names
	# -----------------------------------------------
	FL.pre.in   <- paste(Path.in,paste("Cleaned",room,"Pre.csv", sep="_"),sep="/")		# INPUT  pre-process data
	FL.post.in  <- paste(Path.in,paste("Cleaned",room,"Post.csv",sep="_"),sep="/")		# OUTPUT post-process data
	cat(paste("\nDefined the INPUT pre- and post-installation data files!\n",sep=""),file=FL.LOG, append=TRUE)

	# (A1). read pre-installation data
	      data.pre  <- read.table(FL.pre.in, sep=",",skip=1,stringsAsFactors=FALSE,header=FALSE,blank.lines.skip = TRUE)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files
	      data.pre <- data.pre[,c(1,2,3,4)]
	names(data.pre) <- c("idx","date.time","T.F","RH");

	# (A2). read post-installation data
	      data.post <- read.table(FL.post.in, sep=",",skip=1,stringsAsFactors=FALSE,header=FALSE,blank.lines.skip = TRUE)	# blank.lines.skip = TRUE since there is no empty lines in the middle but in the end of the files
	      data.post <- data.post[,c(1,2,3,4)]
	names(data.post)<- c("idx","date.time","T.F","RH");

	# add date/time, month/day/year field
	# (B1) split date/time into dates and times
	tmp <- unlist(strsplit(data.pre[,"date.time"],"\\s+"))	# split the date/time filed into date and time
	tmp.dates <- tmp[seq(from=1,to=length(tmp),by=2)]
	tmp.times <- tmp[seq(from=2,to=length(tmp),by=2)]	

	#  (B2) split dates into year, month and day
	tmp <- unlist(strsplit(tmp.dates,"/"))			# split the date/time filed into date and time
	tmp.month <- tmp[seq(from=1,to=length(tmp),by=3)]
	tmp.day   <- tmp[seq(from=2,to=length(tmp),by=3)]
	tmp.year  <- tmp[seq(from=3,to=length(tmp),by=3)]

	#  (B3) append the month, day, year to the data
	data.pre <- cbind(data.pre,month=tmp.month,day=tmp.day,year=tmp.year)
	data.pre[,"month"] <- factor(data.pre[,"month"],levels=month.label,labels=month.names)
	data.pre[,"day"]   <- factor(data.pre[,"day"],levels=day.label,labels=day.names)



	# (C1) split date/time into dates and times
	tmp <- unlist(strsplit(data.post[,"date.time"],"\\s+"))	# split the date/time filed into date and time
	tmp.dates <- tmp[seq(from=1,to=length(tmp),by=2)]
	tmp.times <- tmp[seq(from=2,to=length(tmp),by=2)]	

	#  (C2) split dates into year, month and day
	tmp <- unlist(strsplit(tmp.dates,"/"))			# split the date/time filed into date and time
	tmp.month <- tmp[seq(from=1,to=length(tmp),by=3)]
	tmp.day   <- tmp[seq(from=2,to=length(tmp),by=3)]
	tmp.year  <- tmp[seq(from=3,to=length(tmp),by=3)]

	#  (C3) append the month, day, year to the data
	data.post <- cbind(data.post,month=tmp.month,day=tmp.day,year=tmp.year)
	data.post[,"month"] <- factor(data.post[,"month"],levels=month.label,labels=month.names)
	data.post[,"day"]   <- factor(data.post[,"day"],levels=day.label,labels=day.names)

	for (month.idx in month.names)
	{
		data.pre.month  <- subset(data.pre,month==month.idx)
		data.post.month <- subset(data.post,month==month.idx)
		
		# -----------------------------------------------
		#    get the monthly range of Temperature and RH
		# -----------------------------------------------
		RH.pre.min    <- tapply(data.pre.month[,"RH"],data.pre.month[,"day"],min,   na.rm=TRUE)
		RH.pre.max    <- tapply(data.pre.month[,"RH"],data.pre.month[,"day"],max,   na.rm=TRUE)
		RH.pre.median <- tapply(data.pre.month[,"RH"],data.pre.month[,"day"],median,na.rm=TRUE)
		RH.pre.mean   <- tapply(data.pre.month[,"RH"],data.pre.month[,"day"],mean,  na.rm=TRUE)
		T.pre.min     <- tapply(data.pre.month[,"T.F"], data.pre.month[,"day"],min,   na.rm=TRUE)
		T.pre.max     <- tapply(data.pre.month[,"T.F"], data.pre.month[,"day"],max,   na.rm=TRUE)
		T.pre.median  <- tapply(data.pre.month[,"T.F"], data.pre.month[,"day"],median,na.rm=TRUE)
		T.pre.mean    <- tapply(data.pre.month[,"T.F"], data.pre.month[,"day"],mean,  na.rm=TRUE)

		RH.post.min    <- tapply(data.post.month[,"RH"],data.post.month[,"day"],min,   na.rm=TRUE)
		RH.post.max    <- tapply(data.post.month[,"RH"],data.post.month[,"day"],max,   na.rm=TRUE)
		RH.post.median <- tapply(data.post.month[,"RH"],data.post.month[,"day"],median,na.rm=TRUE)
		RH.post.mean   <- tapply(data.post.month[,"RH"],data.post.month[,"day"],mean,  na.rm=TRUE)
		T.post.min     <- tapply(data.post.month[,"T.F"], data.post.month[,"day"],min,   na.rm=TRUE)
		T.post.max     <- tapply(data.post.month[,"T.F"], data.post.month[,"day"],max,   na.rm=TRUE)
		T.post.median  <- tapply(data.post.month[,"T.F"], data.post.month[,"day"],median,na.rm=TRUE)
		T.post.mean    <- tapply(data.post.month[,"T.F"], data.post.month[,"day"],mean,  na.rm=TRUE)

		# put all summary data of pre-installation in the arrays
		RH.pre <- cbind(RH.pre.min,RH.pre.max,RH.pre.mean,RH.pre.median)
		T.pre  <- cbind(T.pre.min,T.pre.max,T.pre.mean,T.pre.median)

		# put all summary data of post-installation in the arrays
		RH.post <- cbind(RH.post.min,RH.post.max,RH.post.mean,RH.post.median)
		T.post  <- cbind(T.post.min,T.post.max,T.post.mean,T.post.median)

		#
		# put in a single array
		#
		data.summary <- cbind(RH.pre,RH.post,T.pre,T.post)
		RH.limit <- range(range(RH.pre),range(RH.post))
		T.limit  <- range(range(T.pre),range(T.post))
		
		#
		# make plot
		#
		par(mar=c(5, 4, 4, 5)) 
		x.shift.left  <- day.label - 0.25
		x.shift.right <- day.label + 0.25
		
		# ---------------------------------------------------------------------------------
		# pre-installation plotted in one color
		# ---------------------------------------------------------------------------------
		plot(x.shift.left,data.summary[,"RH.pre.min"],
		     type="p",lty=1,pch=16,lwd=0.15,col="red",col.lab="red",
		     xlab="hour",
		     ylab="RH(%) ",
		     main=paste("(",room,")  on ",month.idx,sep=""),ylim=RH.limit)
                points(x.shift.left,data.summary[,"RH.pre.max"],   pch=16,col="red")	
                points(x.shift.left,data.summary[,"RH.pre.median"],pch=20,col="red")	
                for (day.idx in day.label)
                {
                	x.shift.left <- day.idx - 0.25
                	lines(c(x.shift.left,x.shift.left),c(data.summary[day.idx,"RH.pre.min"],data.summary[day.idx,"RH.pre.max"]),col="red")
                }
 
 		# ---------------------------------------------------------------------------------
 		# post-installation plotted in another color
 		# ---------------------------------------------------------------------------------
 		points(x.shift.right,data.summary[,"RH.post.min"],   pch=16,col="blue")
              	points(x.shift.right,data.summary[,"RH.post.max"],   pch=16,col="blue")	
             	points(x.shift.right,data.summary[,"RH.post.median"],pch=20,col="blue")	
            	for (day.idx in day.label)
            	{
            		x.shift.right <- day.idx + 0.25
                 	lines(c(x.shift.right,x.shift.right),c(data.summary[day.idx,"RH.post.min"],data.summary[day.idx,"RH.post.max"]),col="blue")
                }
                
                # ---------------------------------------------------------------------------------
                # plotting the separating lines for days
            	# ---------------------------------------------------------------------------------
            	for (day.idx in c(0,day.label))
            	{
            		x.separator <- day.idx + 0.5
                 	lines(c(x.separator,x.separator),RH.limit,type="l",lty=2,col="grey")
                }
                
                
                
                points(row.names(data.summary),data.summary[,"RH.pre.max"],col="cyan")		     
		par(new=TRUE)
		plot(row.names(data.summary),data.summary[,"T.pre.min"],
		     type="l",lty=1,lwd=0.15,col="blue",col.lab="blue",
		     axes=FALSE,bty="n",
		     xlab="",ylab="")
		axis(side=4,at=pretty(range(data.summary[,"T.pre.min"],na.rm=TRUE)))
		mtext(col="blue","T (F)",side=4,line=3)				
	}
}


# -----------------------------------------------
#    split date/time into dates and times
# -----------------------------------------------
tmp <- unlist(strsplit(data.pre[,"date.time"],"\\s+"))	# split the date/time filed into date and time
tmp.dates <- tmp[seq(from=1,to=length(tmp),by=2)]
tmp.times <- tmp[seq(from=2,to=length(tmp),by=2)]	

# -----------------------------------------------
#    split dates into year, month and day
# -----------------------------------------------
tmp <- unlist(strsplit(tmp.dates,"/"))			# split the date/time filed into date and time
tmp.month <- tmp[seq(from=1,to=length(tmp),by=3)]
tmp.day   <- tmp[seq(from=2,to=length(tmp),by=3)]
tmp.year  <- tmp[seq(from=3,to=length(tmp),by=3)]


time.pre  <- data.frame(dates=tmp[seq(from=1,to=length(tmp),by=2)],times=paste(tmp[seq(from=2,to=length(tmp),by=2)],"00",sep=":"))	# odd is date, even is time, but time need to add second
rm(tmp)




# The following line is for checking the name above on Jan 29, 2009
#                     TIMESTAMP   Tcws_Avg  Tcwr_Avg  Tcwsa_Avg  Tcwra_Avg  Tcwsb_Avg	Tcwrb_Avg  Tcwsc_Avg  Tcwrc_Avg	 Thws_Avg  Thwr_Avg  Thwsa_Avg	Thwra_Avg  Thwsb_Avg  Thwrb_Avg	 Thwsc_Avg  Thwrc_Avg  kWa3_Avg	  kWb1_Avg   kWb2_Avg	kWb3_Avg   kWc1_Avg   kWc2_Avg	 Fcwr_Avg    Fhws_Avg	 Fcwr_a_Avg    Fhws_a_Avg    Fcwr_b_Avg	   Fhws_b_Avg	 Fcwr_c_Avg    Fhws_c_Avg    Fdcw_Avg	 kWc3_Avg   kWchiller_Avg   kWa1_Avg   kWa2_Avg	  Toa_Avg  RHoa_Avg  Tef_Avg  RHef_Avg	Fcwr_b316_Avg    Tcws_b316_Avg	Tcwr_b316_Avg  SolRad_Avg	       CHW Flow	       Cooling Load	      Cooling Load
		
		
# record the changes of field name to the LOG file
cat(paste(paste("\nNote the following changes on the field names of the pre-installation data file",FL.pre.in,":",sep=""),
           "\t(a) there is an empty column after \"SolRad(kWm-2)\"",
           "\t(b) field names \"F_cwr_b316(%)\" changes to \"F_cwr_b316(gpm)\"",
           "\t(c)             \"Tcws_b316\"     changes to \"Tcws_b316(C)\"",
           "\t(d)             \"Tcwr_b316\"     changes to \"Tcwr_b316(C)\"",
           "\n",
           sep="\n"),file=FL.LOG,append=TRUE)
cat(paste("\nread in pre-installation data!\n",sep=""),file=FL.LOG, append=TRUE)



# remve the "empty" column in the pre data
data.pre <- subset(data.pre,select=-empty)
lab.pre  <- names(data.pre)[2:dim(data.pre)[2]]	# label of variables except the timestamp 

# -------------------------------------------------------------------------------------------------
# make pre-installation data ready: delete the last three ROWS since there are only values for the last three fields which are not shared by the post-installation data
# -------------------------------------------------------------------------------------------------
data.pre <- data.pre[1:(dim(data.pre)[1] - 3),]

# record the deletion of the last three rows of pre-installation data in the LOG file
cat(paste(paste("\nThere are three more fields in [",FL.pre.in,"]\n\t(i.e. \"CHW Flow(gpm)\",\"Cooling Load(Btuh-1)\",\"Cooling Load(tons)\")",sep=""),
           "\tthey have three more rows.",
           "\tthese 3 more rows are deleted since all fields common to post-installation are missing.",
           "\n",
           sep="\n"),file=FL.LOG,append=TRUE)
cat(paste("\nDeleted the last 3 rows of the pre-installation data!\n",sep=""),file=FL.LOG, append=TRUE)



# (b) get date and time of the pre-data
tmp       <- unlist(strsplit(as.character(data.pre[,"TIMESTAMP"]),"\\s+"))		# split the date/time filed into two
time.pre  <- data.frame(dates=tmp[seq(from=1,to=length(tmp),by=2)],times=paste(tmp[seq(from=2,to=length(tmp),by=2)],"00",sep=":"))	# odd is date, even is time, but time need to add second
rm(tmp)

# (c). make date/time ready for pre-installation data: 
tmp.date   <- unlist(strsplit(as.character(time.pre[,"dates"]),"/"))	
tmp.time   <- unlist(strsplit(as.character(time.pre[,"times"]),":"))
time.pre   <- data.frame(time.pre,
                         month = tmp.date[seq(from=1,to=length(tmp.date),by=3)],
			 day   = tmp.date[seq(from=2,to=length(tmp.date),by=3)],
			 year  = tmp.date[seq(from=3,to=length(tmp.date),by=3)],
			 hour  = tmp.time[seq(from=1,to=length(tmp.time),by=3)],
			 minute= tmp.time[seq(from=2,to=length(tmp.time),by=3)],
			 second= tmp.time[seq(from=3,to=length(tmp.time),by=3)])
rm(tmp.date,tmp.time)			 
			     
# convert the factor field into character field (do we have a better way for such conversion?? {as.character} does not work)
for (i in seq(1:dim(time.pre)[2]))
{
	if(is.factor(time.pre[,i]))
	{
		time.pre[,i] <- as.character(time.pre[,i]) 
	}
}
cat(paste("\nSplit \"TIMESTAMP\" into more date/time fields for pre-installation data!\n",sep=""),file=FL.LOG, append=TRUE)


# (d). create a chron date object for plotting purpose			     
time.pre.chron <- chron(dates  = as.character(time.pre[,"dates"]),
	     	        times  = as.character(time.pre[,"times"]),
		        format = c('m/d/y','h:m:s'))
cat(paste("\nAdded a chron date/time object for pre-installation data!\n",sep=""),file=FL.LOG, append=TRUE)

		       
# -------------------------------------------------------------------------------------------------
# (e). organize the pre-data in wide format by including data, date/time and chron date/time
# -------------------------------------------------------------------------------------------------
data.pre.wide <- data.frame(time.chron = time.pre.chron,
                            time.pre,
                            subset(data.pre,select=-TIMESTAMP,drop=FALSE))	# note: lab.pre does not include "TIMESTAMP" field
names(data.pre.wide) <- c("time.chron",names(time.pre),lab.pre)                           
rm(data.pre)
cat(paste("\nWide format: Put the data, date/time and chron date/time together for pre-installation data!\n",sep=""),file=FL.LOG, append=TRUE)

# -------------------------------------------------------------------------------------------------		    
# (f). sort according to chron date/time
# -------------------------------------------------------------------------------------------------
o <- order(data.pre.wide[,"time.chron"])
data.pre.wide <- data.pre.wide[o,]
row.names(data.pre.wide) <- seq(1:dim(data.pre.wide)[1])
cat(paste("\nSorted the wide format of the pre-installation data according to date/time!\n",sep=""),file=FL.LOG, append=TRUE)




# ----------------------------------------------- 
# 	convert pre-installation data to long format 
# ----------------------------------------------- 
 dat.tmp      <- data.pre.wide[,lab.pre]	# only the variables
  no.tmp      <- dim(dat.tmp)[2]		# number of the variables
 dat.tmp.long <- stack(dat.tmp)			# stack the pre data variable one on the top of another

data.pre.long <- data.frame(dates  = rep(data.pre.wide[,"dates"], no.tmp),
                            times  = rep(data.pre.wide[,"times"], no.tmp),
                            month  = rep(data.pre.wide[,"month"], no.tmp),
                            day    = rep(data.pre.wide[,"day"],   no.tmp),
                            year   = rep(data.pre.wide[,"year"],  no.tmp),
                            hour   = rep(data.pre.wide[,"hour"],  no.tmp),
                            minute = rep(data.pre.wide[,"minute"],no.tmp),
                            second = rep(data.pre.wide[,"second"],no.tmp),
			    value  = dat.tmp.long[,1],
			    vari   = dat.tmp.long[,2])
rm(dat.tmp,dat.tmp.long)			    
			    

# -------------------------------------------------------------------------------------------------
# create a chron data frame for time series plotting
# -------------------------------------------------------------------------------------------------
data.pre.long <- data.frame(time.chron = chron(dates  = as.character(data.pre.long[,"dates"]),
		                               times  = as.character(data.pre.long[,"times"]),
		                               format = c('m/d/y','h:m:s')),
		            data.pre.long)

# sort according to date/time
o <- order(data.pre.long[,"time.chron"])
data.pre.long <-data.pre.long[o,]
row.names(data.pre.long) <- seq(1:dim(data.pre.long)[1])
		            
# convert the factor fields into character fields
for (i in seq(1:dim(data.pre.long)[2]))
{
	if(is.factor(data.pre.long[,i]))
	{
		data.pre.long[,i] <- as.character(data.pre.long[,i]) 
	}
}

# make sure the "value" field is a numeric field
if(is.character(data.pre.long[,"value"])){data.pre.long[,"value"] <- as.numeric(data.pre.long[,"value"])}








# *************************************************************************************************
# -----------------------------------------------
#     processing post-processed data 
#     note: (a) field names "F_cwr_b316(%)" changes to "F_cwr_b316(gpm)"
#           (b)             "Tcws_b316"     changes to "Tcws_b316(C)"
#           (c)             "Tcwr_b316"     changes to "Tcwr_b316(C)"
# ----------------------------------------------- 
# (a). read post-installation data
      data.post  <- read.table(FL.post.in, sep=",",stringsAsFactors=FALSE,header=TRUE,blank.lines.skip = TRUE)	
names(data.post) <- c("TIMESTAMP","Tcws(C)","Tcwr(C)","Tcwsa(C)","Tcwra(C)","Tcwsb(C)","Tcwrb(C)","Tcwsc(C)","Tcwrc(C)","Thws(C)","Thwr(C)","Thwsa(C)","Thwra(C)","Thwsb(C)","Thwrb(C)","Thwsc(C)","Thwrc(C)","kWa3(kW)","kWb1(kW)","kWb2(kW)","kWb3(kW)","kWc1(kW)","kWc2(kW)","Fcwr(gpm)","Fhws(gpm)","Fcwr_a(gpm)","Fhws_a(gpm)","Fcwr_b(gpm)","Fhws_b(gpm)","Fcwr_c(gpm)","Fhws_c(gpm)","Fdcw(gpm)","kWc3(kW)","kWchiller(kW)","kWa1(kW)","kWa2(kW)","Toa(C)","RHoa(%)","Tef(C)","RHef(%)","Fcwr_b316(gpm)","Tcws_b316(C)","Tcwr_b316(C)","SolRad(kWm-2)");
       lab.post  <- names(data.post)[2:dim(data.post)[2]]	# label of variables except the timestamp
# The following line is for checking the name above on Jan 29, 2009
#		       TIMESTAMP   Tcws_Avg  Tcwr_Avg	Tcwsa_Avg  Tcwra_Avg  Tcwsb_Avg	 Tcwrb_Avg  Tcwsc_Avg  Tcwrc_Avg  Thws_Avg  Thwr_Avg  Thwsa_Avg	 Thwra_Avg  Thwsb_Avg  Thwrb_Avg  Thwsc_Avg  Thwrc_Avg	kWa3_Avg   kWb1_Avg   kWb2_Avg   kWb3_Avg   kWc1_Avg   kWc2_Avg	  Fcwr_Avg    Fhws_Avg	  Fcwr_a_Avg	Fhws_a_Avg    Fcwr_b_Avg    Fhws_b_Avg	  Fcwr_c_Avg    Fhws_c_Avg    Fdcw_Avg	  kWc3_Avg   kWchiller_Avg   kWa1_Avg	kWa2_Avg   Toa_Avg  RHoa_Avg  Tef_Avg  RHef_Avg	 Fcwr_b316_Avg	  Tcws_b316_Avg	 Tcwr_b316_Avg	SolRad_Avg

# record the changes of field name to the LOG file
cat(paste(paste("\nNote the following changes on the field names of the post-installation data file",FL.post.in,":",sep=""),
           "\t(a) field names \"F_cwr_b316(%)\" changes to \"F_cwr_b316(gpm)\"",
           "\t(b)             \"Tcws_b316\"     changes to \"Tcws_b316(C)\"",
           "\t(c)             \"Tcwr_b316\"     changes to \"Tcwr_b316(C)\"",
           "\n",
           sep="\n"),file=FL.LOG,append=TRUE)
cat(paste("\nread in post-installation data!\n",sep=""),file=FL.LOG, append=TRUE)



# (b) get date and time of the post-data
tmp       <- unlist(strsplit(as.character(data.post[,"TIMESTAMP"]),"\\s+"))		# split the date/time filed into two
time.post <- data.frame(dates=tmp[seq(from=1,to=length(tmp),by=2)],times=paste(tmp[seq(from=2,to=length(tmp),by=2)],"00",sep=":"))	# odd is date, even is time, but time need to add second
rm(tmp)

# (c). make date/time ready for post-installation data: 
tmp.date   <- unlist(strsplit(as.character(time.post[,"dates"]),"/"))	
tmp.time   <- unlist(strsplit(as.character(time.post[,"times"]),":"))
time.post  <- data.frame(time.post,
                         month = tmp.date[seq(from=1,to=length(tmp.date),by=3)],
			 day   = tmp.date[seq(from=2,to=length(tmp.date),by=3)],
			 year  = tmp.date[seq(from=3,to=length(tmp.date),by=3)],
			 hour  = tmp.time[seq(from=1,to=length(tmp.time),by=3)],
			 minute= tmp.time[seq(from=2,to=length(tmp.time),by=3)],
			 second= tmp.time[seq(from=3,to=length(tmp.time),by=3)])
rm(tmp.date,tmp.time)			 
			     
# convert the factor field into character field (do we have a better way for such conversion?? {as.character} does not work)
for (i in seq(1:dim(time.post)[2]))
{
	if(is.factor(time.post[,i]))
	{
		time.post[,i] <- as.character(time.post[,i]) 
	}
}
cat(paste("\nSplit \"TIMESTAMP\" into more date/time fields for post-installation data!\n",sep=""),file=FL.LOG, append=TRUE)


# (d). create a chron date object for plotting purpose			     
time.post.chron<- chron(dates  = as.character(time.post[,"dates"]),
	     	        times  = as.character(time.post[,"times"]),
		        format = c('m/d/y','h:m:s'))
cat(paste("\nAdded a chron date/time object for post-installation data!\n",sep=""),file=FL.LOG, append=TRUE)


# -------------------------------------------------------------------------------------------------		        
# (e). organize the post-data in wide format by including data, date/time and chron date/time
# -------------------------------------------------------------------------------------------------
data.post.wide<- data.frame(time.chron = time.post.chron,
                            time.post,
                            subset(data.post,select=-TIMESTAMP,drop=FALSE))	# note: lab.post does not include "TIMESTAMP" field
names(data.post.wide) <- c("time.chron",names(time.post),lab.post)                           
rm(data.post)
cat(paste("\nWide format: Put the data, date/time and chron date/time together for post-installation data!\n",sep=""),file=FL.LOG, append=TRUE)

		    
# (f). sort according to chron date/time
o <- order(data.post.wide[,"time.chron"])
data.post.wide <- data.post.wide[o,]
row.names(data.post.wide) <- seq(1:dim(data.post.wide)[1])
cat(paste("\nSorted the wide format of the post-installation data according to date/time!\n",sep=""),file=FL.LOG, append=TRUE)




# ----------------------------------------------- 
# 	convert post-installation data to long format 
# ----------------------------------------------- 
 dat.tmp      <- data.post.wide[,lab.post]	# only the variables
  no.tmp      <- dim(dat.tmp)[2]		# number of the variables
 dat.tmp.long <- stack(dat.tmp)			# stack the post data variable one on the top of another

data.post.long<- data.frame(dates  = rep(data.post.wide[,"dates"], no.tmp),
                            times  = rep(data.post.wide[,"times"], no.tmp),
                            month  = rep(data.post.wide[,"month"], no.tmp),
                            day    = rep(data.post.wide[,"day"],   no.tmp),
                            year   = rep(data.post.wide[,"year"],  no.tmp),
                            hour   = rep(data.post.wide[,"hour"],  no.tmp),
                            minute = rep(data.post.wide[,"minute"],no.tmp),
                            second = rep(data.post.wide[,"second"],no.tmp),
			    value  = dat.tmp.long[,1],
			    vari   = dat.tmp.long[,2])
rm(dat.tmp,dat.tmp.long)			    

# -------------------------------------------------------------------------------------------------
# create a chron data frame for time series plotting
# -------------------------------------------------------------------------------------------------
data.post.long<- data.frame(time.chron = chron(dates  = as.character(data.post.long[,"dates"]),
		                               times  = as.character(data.post.long[,"times"]),
		                               format = c('m/d/y','h:m:s')),
		            data.post.long)

# sort according to date/time
o <- order(data.post.long[,"time.chron"])
data.post.long <-data.post.long[o,]
row.names(data.post.long) <- seq(1:dim(data.post.long)[1])
		            
# convert the factor fields into character fields
for (i in seq(1:dim(data.post.long)[2]))
{
	if(is.factor(data.post.long[,i]))
	{
		data.post.long[,i] <- as.character(data.post.long[,i]) 
	}
}

# make sure the "value" field is a numeric field
if(is.character(data.post.long[,"value"])){data.post.long[,"value"] <- as.numeric(data.post.long[,"value"])}








# ----------------------------------------------- 
# put "pre" and "post" data into a single data frame: note, "pre" has 3 more fields than "post" which are not kept for the merged data frame
# ----------------------------------------------- 
data.merged.wide <- rbind(cbind(data.pre.wide[1:(dim(data.pre.wide)[2]-3)],period=rep("pre",dim(data.pre.wide)[1])),cbind(data.post.wide,period=rep("post",dim(data.post.wide)[1])))
data.merged.long <- rbind(cbind(data.pre.long,period=rep("pre",dim(data.pre.long)[1])),cbind(data.post.long,period=rep("post",dim(data.post.long)[1])))


# *************************************************************************************************
# ---------------------------------------------------------------------------------------------------
# write all data objects into a R data object file
# ---------------------------------------------------------------------------------------------------	
save(data.merged.wide,data.merged.long,data.pre.wide,data.pre.long,data.post.wide,data.post.long,lab.pre,lab.post,file = FL.OBJ)
# save(data.merged.wide,file = FL.OBJ)
cat(paste("all important data objects are saved in ",FL.OBJ,"\n",sep=""))
cat(paste("all important data objects are saved in ",FL.OBJ,"\n",sep=""),file=FL.LOG,append=TRUE)


#
# List of data objects saved in the R object file
#
list.object <- c("data.merged.wide","data.merged.long","data.pre.wide","data.pre.long","data.post.wide","data.post.long","lab.pre","lab.post")
cat(paste("\n The following data objects are saved in the R object file (",FL.OBJ,")\n\n\n\n\n",sep=""))
cat(paste("\n The following data objects are saved in the R object file (",FL.OBJ,")\n\n\n\n\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste(list.object,collapse="\n"))
cat(paste(list.object,collapse="\n"),file=FL.LOG,append=TRUE)


# ---------------------------------------------------------------------------------------------------
# write merged pre- and post installation out in csv format for quality control (checking against with the input files)
# ---------------------------------------------------------------------------------------------------	
cat(paste("pre-post(raw),",sep=""),file=FL.CSV,append=TRUE)
write.table(data.merged.wide,file = FL.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	


# ---------------------------------------------------------------------------------------------------
# summarize the values of data in each sample
# ---------------------------------------------------------------------------------------------------
    dat.tmp <- data.pre.wide[,lab.pre]
summary.pre <- t(apply(dat.tmp,2,sumfn))
rm(dat.tmp)

    dat.tmp <- data.post.wide[,lab.post]
summary.post<- t(apply(dat.tmp,2,sumfn))
rm(dat.tmp)

	 summary.pre  <- data.frame(summary.pre)
   names(summary.pre) <- c("no","no.NA","no.NEG","min","p10","p25","median","p75","p90","max","mean","stdev","sum","NA%","NA%.cls","NEG%","NEG%.cls")

	 summary.post  <- data.frame(summary.post)
   names(summary.post) <- c("no","no.NA","no.NEG","min","p10","p25","median","p75","p90","max","mean","stdev","sum","NA%","NA%.cls","NEG%","NEG%.cls")


cat(paste("\npre-installation,",sep=""),	
	    file=FL.SUM,
	    append=TRUE)
write.table(summary.pre, 
	    file = FL.SUM,      
	    sep = ",", 
	    col.names = TRUE, 
	    row.names = TRUE,
	    append = TRUE)

cat(paste("\npost-installation,",sep=""),	
	    file=FL.SUM,
	    append=TRUE)
write.table(summary.post, 
	    file = FL.SUM,      
	    sep = ",", 
	    col.names = TRUE, 
	    row.names = TRUE,
	    append = TRUE)
cat(paste("\nsummary statistics are computed and outputted to",FL.SUM,sep=""))
cat(paste("\nsummary statistics are computed and outputted to",FL.SUM,sep=""),file=FL.LOG,append=TRUE)

		    
# ---------------------------------------------------------------------------------------------------
# time used for completing this script
# ---------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n16_RH_Temperature.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n16_RH_Temperature.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [16_RH_Temperature.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)

