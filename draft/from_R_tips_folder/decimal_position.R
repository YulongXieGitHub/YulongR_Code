#
# Profile_thermalSetpoint.R 
# 
#
# October 28, 2009
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
	Path.Current <- "/phome/d3l143/FY2010_NAP_DataAnalysis/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2010_NAP_DataAnalysis/0_scripts"
}
setwd(Path.Current)


# -------------------------------------------------------------------------------------------------
# 	include needed objects
# -------------------------------------------------------------------------------------------------
source(paste(Path.Current,"0_CrownePlaza_Function.R",sep="/"))



# -------------------------------------------------------------------------------------------------
#	setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.out   <- "../Profile_thermalSetpoint"	# OUTPUT processed result directory
Path.log   <- "../0_log"			# OUTPUT log  directory
Path.Data  <- "../1_CrownePlaza_ReturnAirT"	# INPUT  data folder retrieved from "nac" on "nac.pnl.gov" server

if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Data)){stop(paste(" INPUT  data folder retrieved from \"nac\" on \"nac.pnl.gov\" server does NOT existing\n",sep=""))}

# ------------------------------------------------------------------------------------------------- 
# 	create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
FL.LOG <- paste(Path.log,"Profile_thermalSetpoint.log",sep="/")	# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for data processing script [Profile_thermalSetpoint.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
          "*                            [Profile_thermalSetpoint.R]                            *",
          "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)


# -------------------------------------------------------------------------------------------------
# 	define OUTPUT files
# -------------------------------------------------------------------------------------------------
FL.rawData.OBJ      	<- paste(Path.Data,paste("CrownePlaza_ReturnAirT.Rdata",sep=""),sep="/")			# INPUT  DATA in Rdata Objects
FL.aggData.OBJ      	<- paste(Path.Data,paste("CrownePlaza_ReturnAirT_Sum.Rdata",sep=""),sep="/")			# INPUT  SUM  in Rdata Objects
FL.profile.OBJ          <- paste(Path.out, paste("CrownePlaza_Profile_ThermalSetPoint.Rdata",sep=""),sep="/")		# OUTPUT Profile of Thermal SetPoint
FL.profile.CSV          <- paste(Path.out, paste("CrownePlaza_Profile_ThermalSetPoint.CSV",sep=""),sep="/")		# OUTPUT Profile of Thermal SetPoint
FL.profile.PDF          <- paste(Path.out, paste("CrownePlaza_Profile_ThermalSetPoint.PDF",sep=""),sep="/")		# OUTPUT Profile of Thermal SetPoint
FL.profile.IDF          <- paste(Path.out, paste("CrownePlaza_Profile_ThermalSetPoint.IDF",sep=""),sep="/")		# OUTPUT Profile of Thermal SetPoint

if (!file.exists(FL.rawData.OBJ)){stop(paste(FL.rawData.OBJ," should exist. Find out why it is missing!"));file.remove(FL.rawData.OBJ)}				# remove existing OUTPUT files
if (!file.exists(FL.aggData.OBJ)){stop(paste(FL.aggData.OBJ," should exist. Find out why it is missing!"));file.remove(FL.aggData.OBJ)}				# remove existing OUTPUT files
if (file.exists(FL.profile.OBJ)){print(paste(FL.profile.OBJ," exist. Delete it!"));file.remove(FL.profile.OBJ)}					# remove existing OUTPUT files
if (file.exists(FL.profile.CSV)){print(paste(FL.profile.CSV," exist. Delete it!"));file.remove(FL.profile.CSV)}					# remove existing OUTPUT files
if (file.exists(FL.profile.PDF)){print(paste(FL.profile.PDF," exist. Delete it!"));file.remove(FL.profile.PDF)}					# remove existing OUTPUT files
if (file.exists(FL.profile.IDF)){print(paste(FL.profile.IDF," exist. Delete it!"));file.remove(FL.profile.IDF)}					# remove existing OUTPUT files
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
# calculating the quantitle and output 25%, 50% and 75% to the CSV file
# -------------------------------------------------------------------------------------------------
# 
# calculating the quantitle on the raw data
# 
p25 <- tapply(myData[,"Value"],as.factor(as.character(myData[,"hour"])),quantile,0.25,na.rm = TRUE)
p50 <- tapply(myData[,"Value"],as.factor(as.character(myData[,"hour"])),quantile,0.50,na.rm = TRUE)
p75 <- tapply(myData[,"Value"],as.factor(as.character(myData[,"hour"])),quantile,0.75,na.rm = TRUE)

Profile.raw <- data.frame(p25 = p25, p50 = p50, p75= p75)
Profile.raw <- data.frame(hour = as.numeric(as.character(row.names(Profile.raw))),Profile.raw)
Profile.raw <- Profile.raw[order(Profile.raw[,"hour"]),]

cat(paste("\n\nProfile.raw on the raw data\n"),file=FL.profile.CSV,append=TRUE)
write.table(Profile.raw,sep=",",row.names = FALSE,col.names = TRUE,file=FL.profile.CSV,append=TRUE)


#
# calculating the quantitle on the hourly aggregated data
#
p25 <- tapply(Data.daily.roomWise[,"Value"],as.factor(as.character(Data.daily.roomWise[,"hour"])),quantile,0.25,na.rm = TRUE)
p50 <- tapply(Data.daily.roomWise[,"Value"],as.factor(as.character(Data.daily.roomWise[,"hour"])),quantile,0.50,na.rm = TRUE)
p75 <- tapply(Data.daily.roomWise[,"Value"],as.factor(as.character(Data.daily.roomWise[,"hour"])),quantile,0.75,na.rm = TRUE)

Profile.aggHourly <- data.frame(p25 = p25, p50 = p50, p75= p75)
Profile.aggHourly <- data.frame(hour = as.numeric(as.character(row.names(Profile.aggHourly))),Profile.aggHourly)
Profile.aggHourly <- Profile.aggHourly[order(Profile.aggHourly[,"hour"]),]

cat(paste("\n\nProfile.aggHourly on the hourly aggregated data\n"),file=FL.profile.CSV,append=TRUE)
write.table(Profile.aggHourly,sep=",",row.names = FALSE,col.names = TRUE,file=FL.profile.CSV,append=TRUE)



#
# calculating the quantitle on the daytype-hourly aggregated data
# 
p25 <- tapply(Data.dayType.roomWise[,"Value"],as.factor(as.character(Data.dayType.roomWise[,"hour"])),quantile,0.25,na.rm = TRUE)
p50 <- tapply(Data.dayType.roomWise[,"Value"],as.factor(as.character(Data.dayType.roomWise[,"hour"])),quantile,0.50,na.rm = TRUE)
p75 <- tapply(Data.dayType.roomWise[,"Value"],as.factor(as.character(Data.dayType.roomWise[,"hour"])),quantile,0.75,na.rm = TRUE)

Profile.aggDayType <- data.frame(p25 = p25, p50 = p50, p75= p75)
Profile.aggDayType <- data.frame(hour = as.numeric(as.character(row.names(Profile.aggDayType))),Profile.aggDayType)
Profile.aggDayType <- Profile.aggDayType[order(Profile.aggDayType[,"hour"]),]

cat(paste("\n\nProfile.aggDayType on the daytype-hourly aggregated data\n"),file=FL.profile.CSV,append=TRUE)
write.table(Profile.aggDayType,sep=",",row.names = FALSE,col.names = TRUE,file=FL.profile.CSV,append=TRUE)

# -------------------------------------------------------------------------------------------------
# output Thermal Setpoint shcedule at (P25, P50 & P75) in Eplus idf format 
# -------------------------------------------------------------------------------------------------
# 
# output in Eplus idf format (P25)
# 
cat(paste("Output the 25% Thermal Setpoint Schedule!\n",sep=""))
cat(paste("Output the 25% Thermal Setpoint Schedule!\n",sep=""),file=FL.LOG,append=TRUE)




cat(paste("\n\nSchedule:Compact,",                                               "\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    CLGSETP_SCH_P25,                !- Name",                     "\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    Temperature,                    !- Schedule Type Limits Name","\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    Through: 12/31,                 !- Field 1",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    For: Weekdays SummerDesignDay Saturday WinterDesignDay Sunday Holiday AllOtherDays  !- Field 2",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
for (idx in seq(from=1,to=(dim(Profile.raw)[1]-1),by=1))
{
	idx.field <- 2 + idx
	T      <- Profile.raw[idx,"p25"]
	ltrs   <- substring(T,1:nchar(T),1:nchar(T))
	T.3dec <- substr(T,1,which(ltrs==".")+3)
	
	cat(paste("    Until: ",idx,":00, ",T.3dec,",            !- Field ",idx.field,   "\n",sep=""),file=FL.profile.IDF,append=TRUE)
}
idx.field <- idx.field + 1
T      <- Profile.raw[dim(Profile.raw)[1],"p25"]
ltrs   <- substring(T,1:nchar(T),1:nchar(T))
T.3dec <- substr(T,1,which(ltrs==".")+3)
cat(paste("    Until: ",dim(Profile.raw)[1],":00, ",T.3dec,";      !- Field ",idx.field,"\n",sep=""),file=FL.profile.IDF,append=TRUE)

#
# output in Eplus idf format (P50)
#
cat(paste("Output the 50% Thermal Setpoint Schedule!\n",sep=""))
cat(paste("Output the 50% Thermal Setpoint Schedule!\n",sep=""),file=FL.LOG,append=TRUE)


cat(paste("\n\nSchedule:Compact,",                                               "\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    CLGSETP_SCH_p50,                !- Name",                     "\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    Temperature,                    !- Schedule Type Limits Name","\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    Through: 12/31,                 !- Field 1",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    For: Weekdays SummerDesignDay Saturday WinterDesignDay Sunday Holiday AllOtherDays  !- Field 2",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
for (idx in seq(from=1,to=(dim(Profile.raw)[1]-1),by=1))
{
	idx.field <- 2 + idx
	T      <- Profile.raw[idx,"p50"]
	ltrs   <- substring(T,1:nchar(T),1:nchar(T))
	T.3dec <- substr(T,1,which(ltrs==".")+3)
	
	cat(paste("    Until: ",idx,":00, ",T.3dec,",            !- Field ",idx.field,   "\n",sep=""),file=FL.profile.IDF,append=TRUE)
}
idx.field <- idx.field + 1
T      <- Profile.raw[dim(Profile.raw)[1],"p50"]
ltrs   <- substring(T,1:nchar(T),1:nchar(T))
T.3dec <- substr(T,1,which(ltrs==".")+3)
cat(paste("    Until: ",dim(Profile.raw)[1],":00, ",T.3dec,";      !- Field ",idx.field,"\n",sep=""),file=FL.profile.IDF,append=TRUE)

# 
# output in Eplus idf format (p75)
# 
cat(paste("Output the 75% Thermal Setpoint Schedule!\n",sep=""))
cat(paste("Output the 75% Thermal Setpoint Schedule!\n",sep=""),file=FL.LOG,append=TRUE)



cat(paste("\n\nSchedule:Compact,",                                               "\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    CLGSETP_SCH_p75,                !- Name",                     "\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    Temperature,                    !- Schedule Type Limits Name","\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    Through: 12/31,                 !- Field 1",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
cat(paste("    For: Weekdays SummerDesignDay Saturday WinterDesignDay Sunday Holiday AllOtherDays  !- Field 2",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
for (idx in seq(from=1,to=(dim(Profile.raw)[1]-1),by=1))
{
	idx.field <- 2 + idx
	T      <- Profile.raw[idx,"p75"]
	ltrs   <- substring(T,1:nchar(T),1:nchar(T))
	T.3dec <- substr(T,1,which(ltrs==".")+3)
	
	cat(paste("    Until: ",idx,":00, ",T.3dec,",            !- Field ",idx.field,   "\n",sep=""),file=FL.profile.IDF,append=TRUE)
}
idx.field <- idx.field + 1
T      <- Profile.raw[dim(Profile.raw)[1],"p75"]
ltrs   <- substring(T,1:nchar(T),1:nchar(T))
T.3dec <- substr(T,1,which(ltrs==".")+3)
cat(paste("    Until: ",dim(Profile.raw)[1],":00, ",T.3dec,";      !- Field ",idx.field,"\n",sep=""),file=FL.profile.IDF,append=TRUE)



# 
# output in Eplus idf format for each room
# 
for (room.lab in as.character(levels(Data.allDays.roomWise[,"room.name"])))
{
	cat(paste("Output the Thermal Setpoint Schedule at room ",room.lab,"!\n",sep=""))
	cat(paste("Output the Thermal Setpoint Schedule at room ",room.lab,"!\n",sep=""),file=FL.LOG,append=TRUE)


	cat(paste("\n\nSchedule:Compact,",                                               "\n",sep=""),file=FL.profile.IDF,append=TRUE)
	cat(paste("    ",paste("CLGSETP_SCH_",room.lab,sep=""),"                !- Name",                     "\n",sep=""),file=FL.profile.IDF,append=TRUE)
	cat(paste("    Temperature,                    !- Schedule Type Limits Name","\n",sep=""),file=FL.profile.IDF,append=TRUE)
	cat(paste("    Through: 12/31,                 !- Field 1",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
	cat(paste("    For: Weekdays SummerDesignDay Saturday WinterDesignDay Sunday Holiday AllOtherDays  !- Field 2",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
	
	for (idx.hour in seq(from=0,to=22,by=1))
	{
	
	        idx.field <- 2 + idx.hour
	        
		idx <- seq(from=1,to=dim(Data.allDays.roomWise)[1],by=1)[as.character(Data.allDays.roomWise[,"room.name"]) == room.lab & Data.allDays.roomWise[,"hour"] == idx.hour]
		T      <- Data.allDays.roomWise[idx,"Value"]
		ltrs   <- substring(T,1:nchar(T),1:nchar(T))
		T.3dec <- substr(T,1,which(ltrs==".")+3)

		cat(paste("    Until: ",idx.hour+1,":00, ",T.3dec,",            !- Field ",idx.field,   "\n",sep=""),file=FL.profile.IDF,append=TRUE)
	}
	
	idx.field <- idx.field + 1
	idx <- seq(from=1,to=dim(Data.allDays.roomWise)[1],by=1)[as.character(Data.allDays.roomWise[,"room.name"]) == room.lab & Data.allDays.roomWise[,"hour"] == 23]
	T      <- Data.allDays.roomWise[idx,"Value"]
	ltrs   <- substring(T,1:nchar(T),1:nchar(T))
	T.3dec <- substr(T,1,which(ltrs==".")+3)
	cat(paste("    Until: ",24,":00, ",T.3dec,";            !- Field ",idx.field,"\n",sep=""),file=FL.profile.IDF,append=TRUE)
}














# -------------------------------------------------------------------------------------------------
# plotting
# -------------------------------------------------------------------------------------------------
cat(paste("Plot the profiles!\n",sep=""))
cat(paste("Plot the profiles!\n",sep=""),file=FL.LOG,append=TRUE)

pdf(file = FL.profile.PDF,paper="a4r", width=0, height=0)

# -------------------------------------------------------------------------------------------------
# Variation Across Rooms [myData] 
# -------------------------------------------------------------------------------------------------
cat(paste("\nprofile across all rooms and all days based on raw data!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nprofile across all rooms and all days based on raw data!\n"))
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
	      

 


# -------------------------------------------------------------------------------------------------
# Variation Across Rooms
# -------------------------------------------------------------------------------------------------
cat(paste("\nprofile based on hourly aggregated data at each room across all days!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nprofile based on hourly aggregated data at each room across all days!\n"))
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



# -------------------------------------------------------------------------------------------------
# Variation Across Rooms
# -------------------------------------------------------------------------------------------------
cat(paste("\nprofile based on hourly aggregated data across all weekends or weekdays across all rooms!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nprofile based on hourly aggregated data across all weekends or weekdays across all rooms!\n"))
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
boxplot(Data.dayType.roomWise[idx.row,"Value"] ~ factor(Data.dayType.roomWise[idx.row,"hour"]), add = TRUE, 
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

cat(paste("\nProfile_thermalSetpoint.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\nProfile_thermalSetpoint.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [Profile_thermalSetpoint.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [Profile_thermalSetpoint.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

