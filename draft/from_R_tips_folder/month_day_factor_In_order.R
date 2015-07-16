#
# 16a_RH_Temperature.R 
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
# Path.out.pre  <- "C:\YuLong_Projects\FY2009_Techval\16a_RH_Temperature"
# [data.pre],   [data.pre.month]	
# [data.post],  [data.post.month]	
# [data.range]	
#
# Modified on June 8 and 9, 2009:
# to plot the raw outdoor air temperature and relative humidity 
# Originally the aggregated (hr or day) noaa RH and T.db.F were plotted against the monitored data but in the aggregated format (e.g.,mean_h etc) see "14_summary.R for Figure 2AZ and 2B for detail)
# here the noaa data are plotted in the raw format.
# So a plot overlaid with the previous aggregated data are generated for verification purpose
# -------------------------------------------------------------------------
#
# 	eliminate all stuff
# 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

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

chron.pre.start  <- chron(dates="10/30/2006",times="0:0:0",   format=c('m/d/y','h:m:s'))
chron.pre.end    <- chron(dates="10/15/2007",times="23:59:59",format=c('m/d/y','h:m:s'))
chron.post.start <- chron(dates="10/30/2007",times="0:0:0",   format=c('m/d/y','h:m:s'))
chron.post.end   <- chron(dates="11/15/2008",times="23:59:59",format=c('m/d/y','h:m:s'))

	       	   
# periods where data are deleted
period1.start <- chron(dates="6/5/2007", times="11:15:00",format=c('m/d/y','h:m:s'))	# All EMCO flow meter dived down to -500 gpm between 6/5/2007 - 6/11/2007
period1.end   <- chron(dates="6/11/2007",times="11:00:00",format=c('m/d/y','h:m:s'))
period2.start <- chron(dates="4/8/2008", times="10:30:00",format=c('m/d/y','h:m:s'))	# Failure of EMCO flow meters and T_hws between 4/8/2008 and 4/28/2008
period2.end   <- chron(dates="4/28/2008",times="5:30:00", format=c('m/d/y','h:m:s'))
period3.start <- chron(dates="5/9/2008", times="20:15:00",format=c('m/d/y','h:m:s'))	# Due to chiller shut off
period3.end   <- chron(dates="5/27/2008",times="8:15:00", format=c('m/d/y','h:m:s'))


#
# define arrays
#
month.label <- c( 1,    2,    3,    4,    5,    6,    7,    8,    9,    10,   11,   12)
month.names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month.fullNames <- c("January","February","March","April","May","June","July","August","September","October","November","December")
names(month.fullNames) <- month.names

day.label <- c( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
day.names <- c( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)

# ----------------------------------------------- 
# 	two input files for each room, one for pre- and one for post-installation
# -----------------------------------------------
rooms      <- c("904009(C210)","904002(C208)","904001(A312)","903996(B114)","903988(B310)","903980(A119TVLounge)","903978(A314)","903977(A208)","903976(A320)","897202(B205)","897201(C211)")
rooms      <- c("904009(C210)","904002(C208)",               "903996(B114)",               "903980(A119TVLounge)","903978(A314)","903977(A208)","903976(A320)",               "897201(C211)")
rooms.name <- c(       "C210",        "C208",                       "B114",                       "A119TVLounge",        "A314",        "A208",        "A320",                       "C211")
names(rooms.name) <- rooms



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
Path.out   <- "../16a_RH_Temperature"	# OUTPUT processed result directory
Path.log   <- "../0_log"		# OUTPUT log  directory

if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ----------------------------------------------- 
# 	create a LOG file and a TIME Recording file
# ----------------------------------------------- 
FL.TIME     <- paste(Path.log,"time.log",sep="/")		# OUTPUT Time Log file for all scripts
FL.LOG      <- paste(Path.log,"16a_RH_Temperature.log",sep="/")	# OUTPUT Log file

if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for data processing script [16a_RH_Temperature.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
          "*                            [16a_RH_Temperature.R]                            *",
          "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)




# -----------------------------------------------
# 	input path names
# -----------------------------------------------
Path.in  <- "../DataReceived/DataReceived_0606_2009"	# INPUT  
Path.out <- "../16a_RH_Temperature"			# OUTPUT 
if (!file.exists(Path.in)){stop(paste("input data folder does NOT existing\n",sep=""))}
if (!file.exists(Path.out)){stop(paste("output data folder does NOT existing\n",sep=""))}
cat(paste("\nDefined the INPUT/OUTPUT data paths!\n",sep=""),file=FL.LOG, append=TRUE)


# ----------------------------------------------- 
# 	loop throu the rooms
# ----------------------------------------------- 
for (room in rooms)
{
	cat(paste("\n",room,"\n",sep=""))
	data.range <- NA
	
	# -----------------------------------------------
	#    output pdf file names
	# -----------------------------------------------
	FL.pdf   <- paste(Path.out,paste(room,".pdf", sep=""),sep="/")		# OUTPUT  
	FL.csv   <- paste(Path.out,paste(room,".csv", sep=""),sep="/")		# OUTPUT  

	if (file.exists(FL.pdf)){print(paste(FL.pdf," exist. Delete it!"));file.remove(FL.pdf)}			# remove existing OUTPUT files
	if (file.exists(FL.csv)){print(paste(FL.csv," exist. Delete it!"));file.remove(FL.csv)}			# remove existing OUTPUT files
	cat(paste("\nDefined an output R object file!\n",sep=""),file=FL.LOG, append=TRUE)

	#
	# open the pdf file
	#
	pdf(file = FL.pdf,paper="a4r", width=0, height=0)



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
	# (B1) split date/time into dates and times for pre data
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



	# (C1) split date/time into dates and times for post data
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


		
	# get the limits for plotting
	RH.limit <- range(range(data.pre[,"RH"],na.rm=TRUE), range(data.post[,"RH"],na.rm=TRUE))				
	T.limit  <- range(range(data.pre[,"T.F"],na.rm=TRUE),range(data.post[,"T.F"],na.rm=TRUE))

	# round to tenth place		
	RH.limit <- c(floor(floor(min(RH.limit))/10)*10,ceiling(ceiling(max(RH.limit))/10)*10)
	T.limit  <- c(floor(floor(min(T.limit))/10)*10,ceiling(ceiling(max(T.limit))/10)*10)
	cat(paste("RH.limit for ",room," is:",RH.limit,"\n\n",sep=""))
	cat(paste("T.limit for ",room," is:",T.limit,"\n\n",sep=""))


	#
	# loop for month
	#
	for (month.idx in month.names)
	{
		cat(paste("\n",room," (",month.idx,")\n",sep=""))

		# -----------------------------------------------
		# get the subset of data for current month
		# -----------------------------------------------
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

		# put all range data of pre-installation in the arrays
		RH.pre <- cbind(RH.pre.min,RH.pre.max,RH.pre.mean,RH.pre.median)
		T.pre  <- cbind(T.pre.min,T.pre.max,T.pre.mean,T.pre.median)

		# put all range data of post-installation in the arrays
		RH.post <- cbind(RH.post.min,RH.post.max,RH.post.mean,RH.post.median)
		T.post  <- cbind(T.post.min,T.post.max,T.post.mean,T.post.median)

		# ---------------------------------------------------------------------------------
		# range data of current month
		# ---------------------------------------------------------------------------------
		data.range.thisMonth <- cbind(RH.pre,RH.post,T.pre,T.post)
		
		# ---------------------------------------------------------------------------------
		# put range data of all months together in a single array
		# ---------------------------------------------------------------------------------		
		data.range <- cbind(data.range,data.frame(month = rep(month.idx,31),data.range.thisMonth))
		
		cat(paste("range data for ",room," (",month.idx,") is ready\n",sep=""))

	###	# *******************************************************************************************
	###	# *******************************************************************************************
	###	# *******************************************************************************************
	###	# 1: range plots
	###	# *******************************************************************************************
	###	# *******************************************************************************************
	###	# *******************************************************************************************
	###	par(mar=c(5, 4, 4, 5)) 
	###	layout(rbind(c(1,1,1),
	###		     c(2,2,2)),
	###		     respect=rbind(FALSE,TRUE))	
	###		     
	###	# **********************************
	###	# plot for RH(%)
	###	# **********************************
	###	x.shift.left  <- day.label - 0.30
	###	x.shift.right <- day.label + 0.30		
	###	y.limit <- RH.limit
	###	x.limit <- c(0,day.label)
	###	
	###	# ---------------------------------------------------------------------------------
	###	# pre-installation of RH% plotted in one color
	###	# ---------------------------------------------------------------------------------
	###	plot(x.shift.left,data.range.thisMonth[,"RH.pre.min"],
	###	     type="p",lty=1,pch=24,lwd=0.15,col="red",col.lab="red",
	###	     xlab="day",
	###	     ylab="RH(%) ",
	###	     main=paste("Relative Humidity(%) in ",room," (",month.fullNames[month.idx],")",sep=""),ylim=y.limit)
        ###        points(x.shift.left,data.range.thisMonth[,"RH.pre.max"],   pch=25,col="red")	
        ###        points(x.shift.left,data.range.thisMonth[,"RH.pre.median"],pch=3,col="red")	
        ###        for (day.idx in day.label)
        ###        {
        ###        	x.shift.left <- day.idx - 0.30
        ###        	lines(c(x.shift.left,x.shift.left),c(data.range.thisMonth[day.idx,"RH.pre.min"],data.range.thisMonth[day.idx,"RH.pre.max"]),col="red")
        ###        }
 	###
 	###	# add legend
 	###	legend(min(x.limit),max(y.limit),legend=c("pre","post"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue")) 	
 	###
 	###	# ---------------------------------------------------------------------------------
 	###	# post-installation RH% plotted in another color
 	###	# ---------------------------------------------------------------------------------
 	###	points(x.shift.right,data.range.thisMonth[,"RH.post.min"],   pch=24,col="blue")
        ###      	points(x.shift.right,data.range.thisMonth[,"RH.post.max"],   pch=25,col="blue")	
        ###     	points(x.shift.right,data.range.thisMonth[,"RH.post.median"],pch=3, col="blue")	
        ###    	for (day.idx in day.label)
        ###    	{
        ###    		x.shift.right <- day.idx + 0.30
        ###         	lines(c(x.shift.right,x.shift.right),c(data.range.thisMonth[day.idx,"RH.post.min"],data.range.thisMonth[day.idx,"RH.post.max"]),col="blue")
        ###        }
        ###        
        ###        # ---------------------------------------------------------------------------------
        ###        # plotting the separating lines for days for RH%
        ###    	# ---------------------------------------------------------------------------------
        ###    	for (day.idx in c(0,day.label))
        ###    	{
        ###    		x.separator <- day.idx + 0.5
        ###         	lines(c(x.separator,x.separator),y.limit,type="l",lty=2,col="grey")
        ###        }
        ###        
        ###        
	###	# **********************************
	###	# plot for T(F)
	###	# **********************************  
	###	x.shift.left  <- day.label - 0.15
	###	x.shift.right <- day.label + 0.15		
	###	y.limit <- T.limit
	###	
	###	#### par(new=TRUE)
	###	# ---------------------------------------------------------------------------------
	###	# pre-installation of Temperature plotted in one color
	###	# ---------------------------------------------------------------------------------
	###	plot(x.shift.left,data.range.thisMonth[,"T.pre.min"],
	###	     type="p",lty=1,pch=24,lwd=0.15,col="red",col.lab="red",
	###	     xlab="day",
	###	     ylab="T (F)",
	###	     main=paste("Temperature (F) in ",room," (",month.fullNames[month.idx],")",sep=""),ylim=y.limit)
	###	     
        ###        #### # add the secondary axis
	###	#### axis(side=4,at=pretty(range(data.range.thisMonth[,"T.pre.min"],na.rm=TRUE)))
	###	#### mtext(col="blue","T (F)",side=4,line=3)	
	###
	###
        ###        points(x.shift.left,data.range.thisMonth[,"T.pre.max"],   pch=25,col="red")	
        ###        points(x.shift.left,data.range.thisMonth[,"T.pre.median"],pch=3, col="red")	
        ###        for (day.idx in day.label)
        ###        {
        ###        	x.shift.left <- day.idx - 0.15
        ###        	lines(c(x.shift.left,x.shift.left),c(data.range.thisMonth[day.idx,"T.pre.min"],data.range.thisMonth[day.idx,"T.pre.max"]),col="red")
        ###        }
        ###        
        ###        
 	###	# add legend
 	###	legend(min(x.limit),max(y.limit),legend=c("pre","post"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue")) 	
	###	
 	###	# ---------------------------------------------------------------------------------
 	###	# post-installation Temperature plotted in another color
 	###	# ---------------------------------------------------------------------------------
 	###	points(x.shift.right,data.range.thisMonth[,"T.post.min"],   pch=24,col="blue")
        ###      	points(x.shift.right,data.range.thisMonth[,"T.post.max"],   pch=25,col="blue")	
        ###     	points(x.shift.right,data.range.thisMonth[,"T.post.median"],pch=3, col="blue")	
        ###    	for (day.idx in day.label)
        ###    	{
        ###    		x.shift.right <- day.idx + 0.15
        ###         	lines(c(x.shift.right,x.shift.right),c(data.range.thisMonth[day.idx,"T.post.min"],data.range.thisMonth[day.idx,"T.post.max"]),col="blue")
        ###        }	
        ###        
        ###       # ---------------------------------------------------------------------------------
        ###        # plotting the separating lines for days for Temperature
        ###    	# ---------------------------------------------------------------------------------
        ###    	for (day.idx in c(0,day.label))
        ###    	{
        ###    		x.separator <- day.idx + 0.5
        ###         	lines(c(x.separator,x.separator),y.limit,type="l",lty=2,col="grey")
        ###        }    
        ###        
        ###        
	 	# *******************************************************************************************
	 	# *******************************************************************************************
	 	# *******************************************************************************************
		# 2. boxplots RH and T.F in two separate plots
		# *******************************************************************************************
		# *******************************************************************************************
		# *******************************************************************************************
		par(mar=c(5, 4, 4, 5)) 
		layout(rbind(c(1,1,1),
			     c(2,2,2)),
			     respect=rbind(FALSE,TRUE))	
			     
		# **********************************
		# plot for RH(%)
		# **********************************
		y.limit <- RH.limit
		
		# ---------------------------------------------------------------------------------
		# pre-installation of RH% plotted in one color
		# ---------------------------------------------------------------------------------
		boxplot(data.pre.month[,"RH"] ~ data.pre.month[,"day"],
			boxwex = 0.15, notch=FALSE, at = 1:31 - 0.25,cex=1.0,
			xaxt="n",	# if Byalnd is non-empty, no axis is plotted for Aldine 
			main=paste("Relative Humidity(%) in room \"",rooms.name[room],"\" (",month.fullNames[month.idx],")",sep=""),
			ylim=y.limit,
			xlab="day",
			ylab="RH(%) ",
			outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="red")

		boxplot(data.post.month[,"RH"] ~ data.post.month[,"day"], add = TRUE,
			boxwex = 0.15, notch=FALSE, at = 1:31 + 0.00,cex=1.0,
			main="",
			ylim=y.limit,
			xlab="",
			ylab="",
			outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="blue")

		# legends (from package gplots)
		smartlegend(x="right",y="top", inset = 0.05,
			    c("pre","post"),
			    col=c("red","blue"),
			    fill = c("red","blue"))

                # ---------------------------------------------------------------------------------
                # plotting the separating lines for days for RH%
            	# ---------------------------------------------------------------------------------
            	for (day.idx in c(0,day.label))
            	{
            		x.separator <- day.idx + 0.5
                 	lines(c(x.separator,x.separator),y.limit,type="l",lty=2,col="grey")
                }
                
                
		# **********************************
		# plot for T(F)
		# **********************************  
		y.limit <- T.limit
		
		# ---------------------------------------------------------------------------------
		# pre-installation of T.F% plotted in one color
		# ---------------------------------------------------------------------------------
		boxplot(data.pre.month[,"T.F"] ~ data.pre.month[,"day"],
			boxwex = 0.15, notch=FALSE, at = 1:31 - 0.25,cex=1.0,
			xaxt="n",	# if Byalnd is non-empty, no axis is plotted for Aldine 
			main=paste("T (F) in room \"",rooms.name[room],"\" (",month.fullNames[month.idx],")",sep=""),
			ylim=y.limit,
			xlab="day",
			ylab="T (F)",
			outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="red")

		boxplot(data.post.month[,"T.F"] ~ data.post.month[,"day"], add = TRUE,
			boxwex = 0.15, notch=FALSE, at = 1:31 + 0.00,cex=1.0,
			main="",
			ylim=y.limit,
			xlab="",
			ylab="",
			outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="blue")

		# legends (from package gplots)
		smartlegend(x="right",y="top", inset = 0.05,
			    c("pre","post"),
			    col=c("red","blue"),
			    fill = c("red","blue"))

                # ---------------------------------------------------------------------------------
                # plotting the separating lines for days for T.F
            	# ---------------------------------------------------------------------------------
            	for (day.idx in c(0,day.label))
            	{
            		x.separator <- day.idx + 0.5
                 	lines(c(x.separator,x.separator),y.limit,type="l",lty=2,col="grey")
                }      
                
	###	# *******************************************************************************************
	###	# *******************************************************************************************
	###	# *******************************************************************************************
	###	# 3. boxplots RH and T.F in a single plot
	###	# *******************************************************************************************
	###	# *******************************************************************************************
	###	# *******************************************************************************************
	###	par(mar=c(5, 4, 4, 5)) 
	###		     
	###	# **********************************
	###	# plot for RH(%)
	###	# **********************************
	###	y.limit <- range(RH.limit,T.limit)
	###	
	###	# ---------------------------------------------------------------------------------
	###	# pre-installation of RH% plotted in one color
	###	# ---------------------------------------------------------------------------------
	###	boxplot(data.pre.month[,"RH"] ~ data.pre.month[,"day"],
	###		boxwex = 0.15, notch=FALSE, at = 1:31 - 0.35,cex=1.0,
	###		xaxt="n",	# if Byalnd is non-empty, no axis is plotted for Aldine 
	###		main=paste("RH(%) | T(F) in ",room," (",month.fullNames[month.idx],")",sep=""),
	###		ylim=y.limit,
	###		xlab="day",
	###		ylab="RH(%) | T(F)",
	###		outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="red")
	###
	###	boxplot(data.post.month[,"RH"] ~ data.post.month[,"day"], add = TRUE,
	###		boxwex = 0.15, notch=FALSE, at = 1:31 - 0.15,cex=1.0,
	###		main="",
	###		ylim=y.limit,
	###		xlab="",
	###		ylab="",
	###		outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="blue")
        ###        
	###	# **********************************
	###	# plot for T(F)
	###	# **********************************  
	###	# ---------------------------------------------------------------------------------
	###	# pre-installation of T.F% plotted in one color
	###	# ---------------------------------------------------------------------------------
	###	boxplot(data.pre.month[,"T.F"] ~ data.pre.month[,"day"],add = TRUE,
	###		boxwex = 0.15, notch=FALSE, at = 1:31 + 0.15,cex=1.0,
	###		xaxt="n",	# if Byalnd is non-empty, no axis is plotted for Aldine 
	###	 	main="",
	###		ylim=y.limit,
	###		xlab="",
	###		ylab="",
	###		outcex=0.7,outcol="magenta",staplecol="magenta",whiskcol="magenta",boxcol="magenta",boxfill="white",medlwd=2.0,medcol="magenta")
	###
	###	boxplot(data.post.month[,"T.F"] ~ data.post.month[,"day"], add = TRUE,
	###		boxwex = 0.15, notch=FALSE, at = 1:31 + 0.35,cex=1.0,
	###		xaxt="n",
	###		main="",
	###		ylim=y.limit,
	###		xlab="",
	###		ylab="",
	###		outcex=0.7,outcol="cyan",staplecol="cyan",whiskcol="cyan",boxcol="cyan",boxfill="white",medlwd=2.0,medcol="cyan")
	###
	###	# legends (from package gplots)
	###	smartlegend(x="right",y="bottom", inset = 0.05,
	###		    c("RH:pre","RH:post","T:pre","T:Post"),
	###		    col=c("red","blue","magenta","cyan"),
	###		    fill = c("red","blue","magenta","cyan"))
	###
        ###        # ---------------------------------------------------------------------------------
        ###        # plotting the separating lines for days for T.F%
        ###    	# ---------------------------------------------------------------------------------
        ###    	for (day.idx in c(0,day.label))
        ###    	{
        ###    		x.separator <- day.idx + 0.5
        ###         	lines(c(x.separator,x.separator),y.limit,type="l",lty=2,col="grey")
        ###        }                     
	}	# end of day loop
	dev.off()
	
	#
	# output the range data
	#
	cat(paste("\n(",room,")",sep=""),file=FL.csv,append=TRUE)	# this is to put something in the first entry of the headline (note, no newline)	
	write.table(data.range, 
		    file = FL.csv,      
		    sep = ",", 
		    col.names = TRUE, 
		    row.names = TRUE,
		    append = TRUE)
	cat(paste("\n(",room,") data summary are outputted ",FL.csv,"\n\n",sep=""))	
	
}

# *********************************************************************************
# PLOT OUTDOOR RH(%) and T(F) based on NOAA weather station data (raw data)
# *********************************************************************************
# ----------------------------------------------------------
# Generate a plot for outdoor RH and T (data from NOAA site)
# ----------------------------------------------------------
chron.pre.start  <- chron(dates="10/30/2006",times="0:0:0",   format=c('m/d/y','h:m:s'))
chron.pre.end    <- chron(dates="10/15/2007",times="23:59:59",format=c('m/d/y','h:m:s'))
chron.post.start <- chron(dates="10/30/2007",times="0:0:0",   format=c('m/d/y','h:m:s'))
chron.post.end   <- chron(dates="11/15/2008",times="23:59:59",format=c('m/d/y','h:m:s'))

Path.noaa.in <- "../1_data_OceanaNAS_noaa"	# INPUT processed result directory
FL.OBJ.noaa  <- paste(Path.noaa.in,paste("weather_noaa.Rdata",sep=""),sep="/")	# INPUT Rdata
load(FL.OBJ.noaa)
lab.var <- c("date.PST","time.PST","year.PST","month.PST","day.PST","hour.PST","minute.PST","chron.PST","week.lab.PST","month.lab.PST","T.db.F","RH");

data.noaa.pre  <- data.merged.noaa[((data.merged.noaa[,"chron.PST"] >= chron.pre.start)  & (data.merged.noaa[,"chron.PST"] <= chron.pre.end)),lab.var]
data.noaa.post <- data.merged.noaa[((data.merged.noaa[,"chron.PST"] >= chron.post.start) & (data.merged.noaa[,"chron.PST"] <= chron.post.end)),lab.var]


# ------------------------------------------------
# convert the month, day fields to factor
# ------------------------------------------------
data.noaa.pre[,"month.PST"] <- factor(data.noaa.pre[,"month.PST"],levels=month.label,labels=month.names)
data.noaa.pre[,"day.PST"]   <- factor(data.noaa.pre[,"day.PST"],  levels=day.label,  labels=day.names)

data.noaa.post[,"month.PST"] <- factor(data.noaa.post[,"month.PST"],levels=month.label,labels=month.names)
data.noaa.post[,"day.PST"]   <- factor(data.noaa.post[,"day.PST"],  levels=day.label,  labels=day.names)

		
# get the limits for plotting
RH.limit.outdoor <- range(range(data.noaa.pre[,"RH"],na.rm=TRUE), range(data.noaa.post[,"RH"],na.rm=TRUE))				
T.limit.outdoor  <- range(range(data.noaa.pre[,"T.db.F"],na.rm=TRUE),range(data.noaa.post[,"T.db.F"],na.rm=TRUE))

# round to tenth place		
RH.limit.outdoor <- c(floor(floor(min(RH.limit.outdoor))/10)*10,ceiling(ceiling(max(RH.limit.outdoor))/10)*10)
T.limit.outdoor  <- c(floor(floor(min(T.limit.outdoor))/10)*10,ceiling(ceiling(max(T.limit.outdoor))/10)*10)
cat(paste("RH.limit.outdoor for ",room," is:",RH.limit.outdoor,"\n\n",sep=""))
cat(paste("T.limit.outdoor for ",room," is:",T.limit.outdoor,"\n\n",sep=""))

# -----------------------------------------------
#    output pdf file names
# -----------------------------------------------
FL.pdf   <- paste(Path.out,"outdoor_RH_T.pdf",sep="/")		# OUTPUT  
FL.csv   <- paste(Path.out,"outdoor_RH_T.csv",sep="/")		# OUTPUT  

if (file.exists(FL.pdf)){print(paste(FL.pdf," exist. Delete it!"));file.remove(FL.pdf)}			# remove existing OUTPUT files
if (file.exists(FL.csv)){print(paste(FL.csv," exist. Delete it!"));file.remove(FL.csv)}			# remove existing OUTPUT files

#
# open the pdf file
#
pdf(file = FL.pdf,paper="a4r", width=0, height=0)

#
# loop for month
#
for (month.idx in month.names)
{
	cat(paste("\n",month.idx,")\n",sep=""))

	# -----------------------------------------------
	# get the subset of data for current month
	# -----------------------------------------------
	data.noaa.pre_month  <- subset(data.noaa.pre,month.PST==month.idx)
	data.noaa.post_month <- subset(data.noaa.post,month.PST==month.idx)


	# *******************************************************************************************
	# *******************************************************************************************
	# *******************************************************************************************
	# 2. boxplots RH and T.F in two separate plots
	# *******************************************************************************************
	# *******************************************************************************************
	# *******************************************************************************************
	par(mar=c(5, 4, 4, 5)) 
	layout(rbind(c(1,1,1),
		     c(2,2,2)),
		     respect=rbind(FALSE,TRUE))	

	# **********************************
	# plot for RH(%)
	# **********************************
	y.limit <- RH.limit.outdoor

	# ---------------------------------------------------------------------------------
	# pre-installation of RH% plotted in one color
	# ---------------------------------------------------------------------------------
	boxplot(data.noaa.pre_month[,"RH"] ~ data.noaa.pre_month[,"day.PST"],
		boxwex = 0.15, notch=FALSE, at = 1:31 - 0.25,cex=1.0,
		xaxt="n",	# if Byalnd is non-empty, no axis is plotted for Aldine 
		main=paste("Outdoor RH(%) (",month.fullNames[month.idx],")",sep=""),
		ylim=y.limit,
		xlab="day",
		ylab="RH(%) ",
		outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="red")

	boxplot(data.noaa.post_month[,"RH"] ~ data.noaa.post_month[,"day.PST"], add = TRUE,
		boxwex = 0.15, notch=FALSE, at = 1:31 + 0.00,cex=1.0,
		main="",
		ylim=y.limit,
		xlab="",
		ylab="",
		outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="blue")

	# legends (from package gplots)
	smartlegend(x="right",y="top", inset = 0.05,
		    c("pre","post"),
		    col=c("red","blue"),
		    fill = c("red","blue"))

	# ---------------------------------------------------------------------------------
	# plotting the separating lines for days for RH%
	# ---------------------------------------------------------------------------------
	for (day.idx in c(0,day.label))
	{
		x.separator <- day.idx + 0.5
		lines(c(x.separator,x.separator),y.limit,type="l",lty=2,col="grey")
	}


	# **********************************
	# plot for T(F)
	# **********************************  
	y.limit <- T.limit.outdoor

	# ---------------------------------------------------------------------------------
	# pre-installation of T.db.F% plotted in one color
	# ---------------------------------------------------------------------------------
	boxplot(data.noaa.pre_month[,"T.db.F"] ~ data.noaa.pre_month[,"day.PST"],
		boxwex = 0.15, notch=FALSE, at = 1:31 - 0.25,cex=1.0,
		xaxt="n",	# if Byalnd is non-empty, no axis is plotted for Aldine 
		main=paste("Outdoor T (F) (",month.fullNames[month.idx],")",sep=""),
		ylim=y.limit,
		xlab="day",
		ylab="T (F)",
		outcex=0.7,outcol="red",staplecol="red",whiskcol="red",boxcol="red",boxfill="white",medlwd=2.0,medcol="red")

	boxplot(data.noaa.post_month[,"T.db.F"] ~ data.noaa.post_month[,"day.PST"], add = TRUE,
		boxwex = 0.15, notch=FALSE, at = 1:31 + 0.00,cex=1.0,
		main="",
		ylim=y.limit,
		xlab="",
		ylab="",
		outcex=0.7,outcol="blue",staplecol="blue",whiskcol="blue",boxcol="blue",boxfill="white",medlwd=2.0,medcol="blue")

	# legends (from package gplots)
	smartlegend(x="right",y="top", inset = 0.05,
		    c("pre","post"),
		    col=c("red","blue"),
		    fill = c("red","blue"))

	# ---------------------------------------------------------------------------------
	# plotting the separating lines for days for T.db.F
	# ---------------------------------------------------------------------------------
	for (day.idx in c(0,day.label))
	{
		x.separator <- day.idx + 0.5
		lines(c(x.separator,x.separator),y.limit,type="l",lty=2,col="grey")
	}      

}	# end of day loop
dev.off()





# Previously the NOAA data were plotted against the montored data but the aggregated data (hy, day) were used.
# To make sure the NOAA data are used right, the raw NOAA data we used here are plotted against the aggregatred data used in "14_summary.R"
# 
# ---------------------------------------------------------------------------
# Plot raw NOAA Outdoor RH and Temperature on the top of mean hourly aggregated data previous plotted in Figure 2A and 2B for verification: check "14_summary.R" for detail
# ---------------------------------------------------------------------------
# only plot against the hour mean aggregated data
avg.opt <- "mean_hr"

# load aggregated data
Path.in   <- "../10_merge_monitor_noaa"	# INPUT  directory of cleaned 
FL.OBJ.in <- paste(paste(Path.in,"mean_hr",sep="/"),paste("merged_mean_hr.Rdata",sep=""),sep="/")	# INPUT Rdata
load(FL.OBJ.in)
lab.kW      <- grep("^kW",names(data.merged.wide),value=TRUE)

break5  <- c(seq(from=10,to=100,by=5))		# temperature interval 5  minutes
label5  <- c(10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)	# category labels when 5  minutes is used as time interval for averaging
cut5    <- cut(data.merged.wide[,"T.db.F"],breaks=break5,labels=label5,right=FALSE)	# convert the "T.db.F" field into categories
nbins   <- 17
data.merged.wide <- cbind(data.merged.wide,bin5=cut5)

# ******************************************************
# previously aggreagated data is in [data.merged.wide]
# raw data used here is in [data.noaa.pre] and [data.noaa.post]
# ******************************************************

# ------------------------------------------------------------------------------------------------------
# plot of outdoor temperature
# ------------------------------------------------------------------------------------------------------
FL.PDF  <- paste(Path.out, paste("ComapreTo_Figure_2A_",avg.opt,".pdf",sep=""),sep="/")	# OUTPUT FL.OBJ.out
TL.PDF  <- paste("ComapreTo_Figure_2A_",avg.opt,".pdf",sep="")					# OUTPUT FL.OBJ.out
if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,title=TL.PDF,paper="a4r", width=0, height=0)
	par(mfrow=c(2,1),mar=c(5,4,4,4)+0.3)

	# pre-installation data
	# previously hour mean aggregated data
	plot(data.merged.wide[,"time.chron"],data.merged.wide[,"T.db.F"],type="l",lty=1,col="red",cex=2,
	     main = paste("pre-installation: outdoor air dry bulb temperature",sep=""),
	     xlab = "date/time",ylab = "T(F)", xlim=xlim4plot_pre,xaxt = "n")

	axis(1, at = xat4plot_pre, labels = xlab4plot_pre, tick = TRUE)

	# currently used raw data
	lines(data.noaa.pre[,"chron.PST"],data.noaa.pre[,"T.db.F"],    type="l",lty=2,col="blue",  cex=0.5)

	abline(h=0,lty=2,col="black")
	abline(v=c(period1.start,period1.end),lty=2,col="red")
	abline(v=c(period2.start,period2.end),lty=2,col="blue")
	abline(v=c(period3.start,period3.end),lty=2,col="green")


	# legends (from package gplots)
	smartlegend(x="right",y="bottom", inset = 0.05,
		    c("hour aggregated data (in Figure 2A)","raw data"),
		    col=c("red","blue"),
		    fill = c("red","blue"))

	# post-installation data
	# previously used mean hour aggregated data
	plot(data.merged.wide[,"time.chron"],data.merged.wide[,"T.db.F"],type="l",lty=1,col="red",cex=2,
	     main = paste("post-installation: outdoor air dry bulb temperature",sep=""),
	     xlab = "date/time",ylab = "T(F)", xlim=xlim4plot_post,xaxt = "n")

	axis(1, at = xat4plot_post, labels = xlab4plot_post, tick = TRUE)

	# currently used raw noaa data
	lines(data.noaa.post[,"chron.PST"],data.noaa.post[,"T.db.F"],    type="l",lty=2,col="blue",  cex=0.5)


	abline(h=0,lty=2,col="black")
	abline(v=c(period1.start,period1.end),lty=2,col="red")
	abline(v=c(period2.start,period2.end),lty=2,col="blue")
	abline(v=c(period3.start,period3.end),lty=2,col="green")

	# legends (from package gplots)
	smartlegend(x="right",y="bottom", inset = 0.05,
		    c("hour aggregated data (in Figure 2A)","raw data"),
		    col=c("red","blue"),
		    fill = c("red","blue"))
	

dev.off()

# ------------------------------------------------------------------------------------------------------
# plot of outdoor relative humidity
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
FL.PDF  <- paste(Path.out, paste("ComapreTo_Figure_2B_",avg.opt,".pdf",sep=""),sep="/")	# OUTPUT FL.OBJ.out
TL.PDF  <- paste("ComapreTo_Figure_2B_",avg.opt,".pdf",sep="")					# OUTPUT FL.OBJ.out
if (file.exists(FL.PDF)){print(paste(FL.PDF,"exist.Delete it!"));file.remove(FL.PDF)}
pdf(file = FL.PDF,title=TL.PDF,paper="a4r", width=0, height=0)
	par(mfrow=c(2,1),mar=c(5,4,4,4)+0.3)

	# pre-installation data
	# previously used mean hour aggregated data
	plot(data.merged.wide[,"time.chron"],data.merged.wide[,"RH"],type="l",lty=1,col="red",cex=2,
	     main = paste("pre-installation: outdoor air relative humidity",sep=""),
	     xlab = "date/time",ylab = "RH(%)", xlim=xlim4plot_pre,xaxt = "n")

	axis(1, at = xat4plot_pre, labels = xlab4plot_pre, tick = TRUE)

	# currently used noaa raw data
	lines(data.noaa.pre[,"chron.PST"],data.noaa.pre[,"RH"],    type="l",lty=2,col="blue",  cex=0.5)


	abline(h=0,lty=2,col="black")
	abline(v=c(period1.start,period1.end),lty=2,col="red")
	abline(v=c(period2.start,period2.end),lty=2,col="blue")
	abline(v=c(period3.start,period3.end),lty=2,col="green")

	# legends (from package gplots)
	smartlegend(x="right",y="bottom", inset = 0.05,
		    c("hour aggregated data (in Figure 2A)","raw data"),
		    col=c("red","blue"),
		    fill = c("red","blue"))


	# post-installation data
	# previously used mean hour aggregated data
	plot(data.merged.wide[,"time.chron"],data.merged.wide[,"RH"],type="l",lty=1,col="red",cex=2,
	     main = paste("post-installation: outdoor air relative humidity",sep=""),
	     xlab = "date/time",ylab = "RH(%)", xlim=xlim4plot_post,xaxt = "n")

	axis(1, at = xat4plot_post, labels = xlab4plot_post, tick = TRUE)

	# currently used noaa raw data
	lines(data.noaa.post[,"chron.PST"],data.noaa.post[,"RH"],    type="l",lty=2,col="blue",  cex=0.5)


	abline(h=0,lty=2,col="black")
	abline(v=c(period1.start,period1.end),lty=2,col="red")
	abline(v=c(period2.start,period2.end),lty=2,col="blue")
	abline(v=c(period3.start,period3.end),lty=2,col="green")
	
	# legends (from package gplots)
	smartlegend(x="right",y="bottom", inset = 0.05,
		    c("hour aggregated data (in Figure 2A)","raw data"),
		    col=c("red","blue"),
		    fill = c("red","blue"))

dev.off()



		    
# ---------------------------------------------------------------------------------------------------
# time used for completing this script
# ---------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n16a_RH_Temperature.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n16a_RH_Temperature.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [16a_RH_Temperature.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)

