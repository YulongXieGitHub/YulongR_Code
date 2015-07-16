#
# 32_O3_trajectories.R
# 
# This script is to visually locate those trajectories with circular features
#
# Installed a package called "SM' to introduce a "Pause" function
#
# INPUT:
# "../9a_gasDataProcess"	# gas data including ozone
# 		[data.gas.wide],[data.gas.long]
#		[corr.coef.pearson],[corr.coef.spearman],[corr.bySite],[corr.byGas],[time.info]
#
# (2) Wind direction and speed data in 1 minute resolution
# 	"/23_InterpolateWindData/DataWinds.Rdata" which consists of 
#       [wind.data.Aldine], [wind.data.Bayland], [wind.DeerPark] with a dimension of 30236 by 2
#
# (3) sinuosity/quadrant measure of wind trajectory data in 1 minute
# 	"/26_measureOfTraj/[site]/sinuosity_traj_[site].Rdata" which consists of 
#       [latitude],              [longitude]		  with a dimension of 181 by 31621
#       [sinuosity.index.6hour], [sinuosity.index.12hour] with a dimension of 31621 by 1
#       [quadrant.index.6hour],  [quadrant.index.12hour]  with a dimension of 31621 by 4
#
#
# OUTPUT:
# ../32_O3_trajectories/
# Revised on May 4, 2009:
# In previous study we used CDT 7:00 - 20:00 for daytime and 21:00 - 6:00 for nighttime
# the CST equivalent is         6:00 - 19:00 for daytime and 20:00 - 5:00 for nighttime
# here we tended to split the time as:
#     (6:00 - 12:00] for period 1	( 6 hours) 
#    (12:00 - 18:00] for period 2	( 6 hours)
#    (18:00 -  6:00] for period 3	(12 hours)
#     (6:00 - 18:00] for period 4 which is the combination of periods 1 & 2
# 
#
# -------------------------------------------------------------------------------------------------
rm(list = ls(all = TRUE))
Start.time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# -------------------------------------------------------------------------------------------------
# 1. load libraries
# -------------------------------------------------------------------------------------------------
library(lattice)
library(chron)	
library(graphics)
library(sm)

# -------------------------------------------------------------------------------------------------
# 2. define a chron object every hour from Sep 1 to Sep 31, 2006 for plotting
# -------------------------------------------------------------------------------------------------
hour4plot <- chron(dates=rep(paste("9",seq(1,30),"2006",sep="/"),each=24),times=rep(paste(seq(from=0,to=23),"0","0",sep=":"),30),  format=c('m/d/y','h:m:s'))

# -------------------------------------------------------------------------------------------------
# 3. define some arrays
# -------------------------------------------------------------------------------------------------
sites <- c("Aldine","Bayland","DeerPark")
no.Sector           <- 36			# define 36 wind sectors
delta.Sector        <- 360/no.Sector		# the interval of the wind sector
threshold.WS        <- 1			# use 1 s/m as wind speed threshold
threshold.4wgt      <- 1;			#
threshold.sinuosity <- 0.95			# threshold of sinuosity measure for retaining samples with relatively straight trajectories
hour.trajectory     <- 6			# hour of back trajectories to be used for calculating the sinuosity measure
no.segRetain        <- hour.trajectory*12+1	# the number of 5 minute wind segments to be used in the trajectories

# define labels for wind sectors
break.Sector  <- c(seq(from=0,to=360,by=delta.Sector))		# 10 degree of wind sector
label.Sector  <- c(seq(from=1,to=360/delta.Sector,by=1))	# index of wind sectors (from 1 to 36)
name.Sector   <- (label.Sector-1)*delta.Sector +delta.Sector/2	# the degree of the middle of the wind sectors is used as the name of the wind sector (e.g., 5, 15, ...,355)

# categorize the wind speed
break.Speed  <- c(0,threshold.WS,1e21)				# time interval 5  minutes
label.Speed  <- c("slow","fast")				# category labels when 5  minutes is used as time interval for averaging


# -------------------------------------------------------------------------------------------------
# 4. setup output directories
# -------------------------------------------------------------------------------------------------
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_withNegativeRemoved/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_withNegativeRemoved/0_scripts"
}
setwd(Path.Current)



# -------------------------------------------------------------------------------------------------
# 5. load the summary function
# -------------------------------------------------------------------------------------------------
source(paste(Path.Current,"my_functions.R",sep="/"))

# -------------------------------------------------------------------------------------------------
# 6. define paths
# -------------------------------------------------------------------------------------------------
Path.IN.Traj  <- "../26_measureOfTraj"					# INPUT  folder of interpolated back trajectories in 1 minute resolution
Path.IN.Wind  <- "../23_InterpolateWindData"				# INPUT  folder of interpolated wind directions and speeds in 1 minute resolution
Path.IN.O3    <- "../31_O3Analysis"					# INPUT  one-hour running average O3 and other gaseous data
Path.OUT      <- "../32_O3_trajectories"				# OUTPUT folder of plotted back trajectories for each O3 sample
Path.LOG      <- "../0_log"						# OUTPUT log  directory

if (!file.exists(Path.IN.Traj)){stop(paste("NOT existing:",Path.IN.Traj," You must have an input folder of interpolated trajectories for this analysis!",sep=""))}
if (!file.exists(Path.IN.Wind)){stop(paste("NOT existing:",Path.IN.Wind," You must have an input folder of interpolated wind direction and speed data for this analysis!",sep=""))}
if (!file.exists(Path.IN.O3)){stop(paste("NOT existing:",Path.IN.O3," You must have an input folder of interpolated O3 direction and speed data for this analysis!",sep=""))}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.LOG)){print(paste("NOT existing:",Path.LOG));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}

# -------------------------------------------------------------------------------------------------
# 7. define file names
# -------------------------------------------------------------------------------------------------
FL.TIME       		<- paste(Path.LOG,"time.log",sep="/")				# OUTPUT Time Log file for all scripts
FL.LOG        		<- paste(Path.LOG,"32_O3_trajectories.log",sep="/")			# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG," exist. Delete it!"));file.remove(FL.LOG)}

# -------------------------------------------------------------------------------------------------
# 8. load the wind direction and speed data: 
# consists of objects [wind.data.Aldine], 
#                     [wind.data.Bayland], 
#                     [wind.DeerPark]    with a dimension of 30236 by 2
# -------------------------------------------------------------------------------------------------
FL.IN.Wind <- paste(Path.IN.Wind,"DataWinds.Rdata",sep="/")		# INPUT  wind directions and speeds in 1 minute resolution
if (!file.exists(FL.IN.Wind))          {stop(paste("NOT existing:",FL.IN.Wind,"           You must have an input file of wind data         for this analysis!",sep=""))}
load(FL.IN.Wind)
cat(paste("\nWind direction/speed data are loaded from ",FL.IN.Wind,"\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("\nWind direction/speed data are loaded from ",FL.IN.Wind,"\n",sep=""))

# -------------------------------------------------------------------------------------------------
# 9. load the one hour average O3 data
# -------------------------------------------------------------------------------------------------
FL.O3 <- paste(Path.IN.O3,"DataGaseous_oneHr_runAvg.Rdata",sep="/")	# INPUT Rdata
if (!file.exists(FL.O3)){stop(paste("NOT existing:",FL.O3," You must have an input folder of ozone data for this analysis!",sep=""))}
load(FL.O3)
	
# -------------------------------------------------------------------------------------------------
# 10. load trajectories and their measures
# -------------------------------------------------------------------------------------------------
for (site in sites)
{
	lab.O3 <- paste("O3",site,sep=".")
	lab.inc.O3 <- paste("inc",lab.O3,sep=".")

	# -------------------------------------------------------------------------------------------
	Path.IN.Site  <- paste(Path.IN.Traj,site,sep="/")					# OUTPUT folder of interpolated back trajectories
	if (!file.exists(Path.IN.Site)){stop(paste("NOT existing:",Path.IN.Site,"You must have an input folder of trajectory data for this analysis!",sep=""))}

	Path.OUT.Site  <- paste(Path.OUT,site,sep="/")						# OUTPUT folder of interpolated back trajectories
	if (!file.exists(Path.OUT.Site)){print(paste("NOT existing:",Path.OUT.Site));dir.create(Path.OUT.Site,showWarnings=TRUE,recursive=TRUE)}

	FL.SINU   <- paste(Path.IN.Site,paste("sinuosity_traj_",site,".Rdata",sep=""),sep="/")	# INPUT sinuosity measure of the trajectories
	if (!file.exists(FL.SINU)){stop(paste("NOT existing:",FL.SINU,"You must have an input file of trajectory data for this analysis!",sep=""))}

	FL.PDF  <- paste(Path.OUT.Site,paste("trajectory_",site,".pdf",sep=""),sep="/")		# OUTPUT in PDF  files of the sinuosity/quadrant on the O3 increment bins
	if (file.exists(FL.PDF)) {print(paste(FL.PDF, " exist. Delete it!"));file.remove(FL.PDF)}

	# ------------------------------------------------------------------------
	# load trajectories and sinuosity measures of the trajectories
	# ------------------------------------------------------------------------
	load(FL.SINU)
	
	#
	# get the range of the longitude and latitude
	#
	min.Lon <- range(longitude,na.rm=TRUE)[1]
	max.Lon <- range(longitude,na.rm=TRUE)[2]

	min.Lat <- range(latitude,na.rm=TRUE)[1]
	max.Lat <- range(latitude,na.rm=TRUE)[2]


	# ------------------------------------------------------------------------
	# get the subset of wind data of current site
	# ------------------------------------------------------------------------
	if (site == "Aldine")
	{
		wind.data <- wind.data.Aldine					# wind direction and speed data 		
	}	
	if (site == "Bayland")
	{
		wind.data <- wind.data.Bayland					# wind direction and speed data 				
	}
	if (site == "DeerPark")
	{
		wind.data <- wind.data.DeerPark					# wind direction and speed data 		
	}	

	# only keep those no-missing wind data (NOTE: Wind data at DeerPark consists of 2521 NAs of "WD" and 2431 NAs of "WS"
	wind.data <- wind.data[!is.na(wind.data[,"WD"]) & !is.na(wind.data[,"WS"]),]

	# ------------------------------------------------------------------------	
	# get the subset of one hour average O3 data of current site
	# ------------------------------------------------------------------------
	data.O3 <- subset(data.1hr.runAvg,select=c(lab.O3,lab.inc.O3))
	
	# remove NaN data
	command.string <- paste("data.O3 <- subset(data.O3,!is.na(",lab.O3,"))",sep="")
	eval(parse(text=command.string))

	# ------------------------------------------------------------------------	
	# further only retain the data right at the hour (to reduce the number of data to be plotted, only plot the sample at right the hour)
	# ------------------------------------------------------------------------
	data.O3.atTheHour <- data.O3[row.names(data.O3) %in%  as.character(hour4plot),]
		
	# ------------------------------------------------------------------------
	# plot each trajectory
	# ------------------------------------------------------------------------
	cat(paste("plotting trajectories for ",site,"\n",sep=""))
	cat(paste("plotting trajectories for ",site,"\n",sep=""),file=FL.LOG,append=TRUE)
	
	pdf(file = FL.PDF,paper="a4r", width=0, height=0)
	for (idx in seq(1:dim(data.O3.atTheHour)[1]))
	{
		# date/time of current trajectory
		sample.idx <- row.names(data.O3.atTheHour)[idx]
		
		# only thos sampled measured at the hour will be plotted (that means the other 11 5 minute measurement will not be plotted)
		# if (sample.idx %in% as.character(hour4plot))
		# {
			cat(paste(sample.idx),"\n",sep="")
			
			# O3 and O3 increment of current sample
			current.O3     <- data.O3.atTheHour[sample.idx,lab.O3]
			current.inc.O3 <- data.O3.atTheHour[sample.idx,lab.inc.O3]

			if(!is.na(current.O3))
			{
				ltrs <- substring(current.O3,1:nchar(current.O3),1:nchar(current.O3))
				loc.decimal <- which(ltrs==".")
				if (length(loc.decimal) != 0)
				{		
					current.O3 <- substr(current.O3,1,which(ltrs==".")+2)	
				}
			}

			if(!is.na(current.inc.O3) & (current.inc.O3!=0))
			{
				ltrs <- substring(current.inc.O3,1:nchar(current.inc.O3),1:nchar(current.inc.O3))
				loc.decimal <- which(ltrs==".")
				if (length(loc.decimal) != 0)
				{
					current.inc.O3 <- substr(current.inc.O3,1,which(ltrs==".")+2)	
				}
			}




			# wind direction and speed of current trajectory when reaching site
			current.WD <- wind.data[sample.idx,1]	# wind direction measured at site
			current.WS <- wind.data[sample.idx,2]	# wind speed     measured at site

			if(!is.na(current.WD))
			{
				ltrs <- substring(current.WD,1:nchar(current.WD),1:nchar(current.WD))
				loc.decimal <- which(ltrs==".")
				if (length(loc.decimal) != 0)
				{			
					current.WD <- substr(current.WD,1,which(ltrs==".")+1)	
				}
			}

			if(!is.na(current.WS))
			{
				ltrs <- substring(current.WS,1:nchar(current.WS),1:nchar(current.WS))
				loc.decimal <- which(ltrs==".")
				if (length(loc.decimal) != 0)
				{			
					current.WS <- substr(current.WS,1,which(ltrs==".")+1)	
				}
			}

			# measure of current trajectory
			current.sinu.6hour  <- sinuosity.index.6hour[sample.idx]
			current.sinu.12hour <- sinuosity.index.12hour[sample.idx]
			current.quad.6hour  <- quadrant.index.6hour[sample.idx,]
			current.quad.12hour <- quadrant.index.12hour[sample.idx,]
			current.Q1.6hour    <- current.quad.6hour[1]
			current.Q2.6hour    <- current.quad.6hour[2]
			current.Q3.6hour    <- current.quad.6hour[3]
			current.Q4.6hour    <- current.quad.6hour[4]

			current.Q1.12hour    <- current.quad.12hour[1]
			current.Q2.12hour    <- current.quad.12hour[2]
			current.Q3.12hour    <- current.quad.12hour[3]
			current.Q4.12hour    <- current.quad.12hour[4]		

			if(!is.na(current.sinu.6hour))
			{	
				ltrs <- substring(current.sinu.6hour,1:nchar(current.sinu.6hour),1:nchar(current.sinu.6hour))
				loc.decimal <- which(ltrs==".")
				if (length(loc.decimal) != 0)
				{			
					current.sinu.6hour <- substr(current.sinu.6hour,1,which(ltrs==".")+2)	
				}
			}

			if(!is.na(current.sinu.12hour))
			{
				ltrs <- substring(current.sinu.12hour,1:nchar(current.sinu.12hour),1:nchar(current.sinu.12hour))
				loc.decimal <- which(ltrs==".")
				if (length(loc.decimal) != 0)
				{			
					current.sinu.12hour <- substr(current.sinu.12hour,1,which(ltrs==".")+2)	
				}
			}		

			plot(longitude[,sample.idx],latitude[,sample.idx],col="red",type="l",lty=1,
			     xlim = c(min.Lon,max.Lon),ylim = c(min.Lat,max.Lat),
			     main = paste(sample.idx," O3:",current.O3," (ppb); inc.O3:",current.inc.O3,sep=""),xlab="Latitude",ylab="Longitude",
			     sub  = paste(paste("(WD|WS: ",paste(current.WD,current.WS,sep="|"),")",sep=""),
					  paste("(sinue.6hr|sinu.12hr: ",paste(current.sinu.6hour,current.sinu.12hour,sep="|"),")",sep=""),
					  paste("(Q1.6hr|Q1.12hr: ",paste(current.Q1.6hour,current.Q1.12hour,sep="|"),")",sep=""),
					  paste("(Q2.6hr|Q2.12hr: ",paste(current.Q2.6hour,current.Q2.12hour,sep="|"),")",sep=""),
					  paste("(Q3.6hr|Q3.12hr: ",paste(current.Q3.6hour,current.Q3.12hour,sep="|"),")",sep=""),
					  paste("(Q4.6hr|Q4.12hr: ",paste(current.Q4.6hour,current.Q4.12hour,sep="|"),")",sep="")))

			points(longitude[1:73,sample.idx],latitude[1:73,sample.idx],type="p",pch=16,col="blue")

			legend(max(longitude,na.rm=TRUE),max(latitude,na.rm=TRUE),legend=c("raw","1hr running avg"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue")) 	
		# }		
		# pause()
	}
	# ## ### dev.off()
}	# end of site loop
	

# -------------------------------------------------------------------------------------------------
# 40. time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("32_O3_trajectories.R start at",Start.time,"end at",End.time,"and is finished successfully!\n",sep=" "))
cat(paste("32_O3_trajectories.R start at",Start.time,"end at",End.time,"and is finished successfully!\n",sep=" "),file=FL.LOG,append=TRUE)
cat(paste("Processing time for 32_O3_trajectories.R is",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)

