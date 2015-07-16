#
# newProfile_LightsCircuitLoad.R 
# 
#
# Created on Nov 18, 2009
# Modified on Dec 3, 2009 based on EB's detailed revised requests (zone name are specified for the groups of light circuits)
# EB request all internal common area lights in one idf and the exterior light in another idf.
# Reviewed on Jan 6, 2010
# Modified on Jan 8, 2010: figure out a strategy to output the same graphic objects intyo multiple graphic devicers (e.g., multiple pdf files)
# Modified on January 29, 2010: start with the hourly average data retrieved from the database.
# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "/phome/nap/CrownePlaza/Data_uploaded_till_Dec2009_simple/CrownePlaza_allData/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2010_NAP_DataAnalysis/CrownePlaza_allData/0_scripts"
}
setwd(Path.Current)

# -------------------------------------------------------------------------
# 	load stuff defined in the "0_CrownePlaza_FunctionR"
# -------------------------------------------------------------------------------------------------
source(paste(Path.Current,"0_CrownePlaza_Function.R",sep="/"))



# -------------------------------------------------------------------------------------------------
#	setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.out  <- "../newProfile_LightsCircuitLoad"			# OUTPUT processed result directory
Path.log  <- "../new0_log"					# OUTPUT log  directory
Path.in   <- "../new22b_CrownePlaza_LightsDataAggregate"	# INPUT  data folder retrieved from "nac" on "nac.pnl.gov" server
Path.idf  <- "../all_idfs"					# OUTPUT processed result directory

if (!file.exists(Path.idf)){print(paste("NOT existing:",Path.idf));dir.create(Path.idf,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.in)){stop(paste(" INPUT  data folder retrieved from \"nac\" on \"nac.pnl.gov\" server does NOT existing\n",sep=""))}

# -------------------------------------------------------------------------------------------------
# put all internal lights in a single idf file
# -------------------------------------------------------------------------------------------------
FL.internalLights.OBJ <- paste(Path.out, paste("CrownePlaza_Profile_all_LightsCommonArea.Rdata",sep=""),sep="/")			# OUTPUT Profile of Thermal SetPoint
FL.internalLights.CSV <- paste(Path.out, paste("CrownePlaza_Profile_all_LightsCommonArea.CSV",sep=""),sep="/")				# OUTPUT Profile of Thermal SetPoint
FL.internalLights.PDF <- paste(Path.out, paste("CrownePlaza_Profile_all_LightsCommonArea.PDF",sep=""),sep="/")				# OUTPUT Profile of Thermal SetPoint
FL.internalLights.IDF <- paste(Path.out, paste("CrownePlaza_Profile_all_LightsCommonArea.IDF",sep=""),sep="/")				# OUTPUT Profile of Thermal SetPoint

if (file.exists(FL.internalLights.OBJ)){print(paste(FL.internalLights.OBJ," exist. Delete it!"));file.remove(FL.internalLights.OBJ)}	# remove existing OUTPUT files
if (file.exists(FL.internalLights.CSV)){print(paste(FL.internalLights.CSV," exist. Delete it!"));file.remove(FL.internalLights.CSV)}	# remove existing OUTPUT files
if (file.exists(FL.internalLights.PDF)){print(paste(FL.internalLights.PDF," exist. Delete it!"));file.remove(FL.internalLights.PDF)}	# remove existing OUTPUT files
if (file.exists(FL.internalLights.IDF)){print(paste(FL.internalLights.IDF," exist. Delete it!"));file.remove(FL.internalLights.IDF)}	# remove existing OUTPUT files

# -------------------------------------------------------------------------------------------------
# put all exterior lights in a single idf file
# -------------------------------------------------------------------------------------------------
FL.exteriorLights.OBJ <- paste(Path.out, paste("CrownePlaza_Profile_all_LightsExterior.Rdata",sep=""),sep="/")				# OUTPUT Profile of Thermal SetPoint
FL.exteriorLights.CSV <- paste(Path.out, paste("CrownePlaza_Profile_all_LightsExterior.CSV",sep=""),sep="/")				# OUTPUT Profile of Thermal SetPoint
FL.exteriorLights.PDF <- paste(Path.out, paste("CrownePlaza_Profile_all_LightsExterior.PDF",sep=""),sep="/")				# OUTPUT Profile of Thermal SetPoint
FL.exteriorLights.IDF <- paste(Path.out, paste("CrownePlaza_Profile_all_LightsExterior.IDF",sep=""),sep="/")				# OUTPUT Profile of Thermal SetPoint

if (file.exists(FL.exteriorLights.OBJ)){print(paste(FL.exteriorLights.OBJ," exist. Delete it!"));file.remove(FL.exteriorLights.OBJ)}	# remove existing OUTPUT files
if (file.exists(FL.exteriorLights.CSV)){print(paste(FL.exteriorLights.CSV," exist. Delete it!"));file.remove(FL.exteriorLights.CSV)}	# remove existing OUTPUT files
if (file.exists(FL.exteriorLights.PDF)){print(paste(FL.exteriorLights.PDF," exist. Delete it!"));file.remove(FL.exteriorLights.PDF)}	# remove existing OUTPUT files
if (file.exists(FL.exteriorLights.IDF)){print(paste(FL.exteriorLights.IDF," exist. Delete it!"));file.remove(FL.exteriorLights.IDF)}	# remove existing OUTPUT files

# ------------------------------------------------------------------------------------------------- 
# 	create a IDF for all idfs
# ------------------------------------------------------------------------------------------------- 
FL.IDF <- paste(Path.idf,"newProfile_LightCircuitLoad.CSV",sep="/")	# OUTPUT Log file
if (file.exists(FL.IDF)){print(paste(FL.IDF," exist. Delete it!"));file.remove(FL.IDF)}		# remove existing OUTPUT files


# -------------------------------------------------------------------------------------------------
# create two graphic windows, one for "FL.internalLights.PDF" and another for "FL.exteriorLights.PDF"
# Note: the index of the graphic windows are used, so do not move arbitrary the order of the pdf() command
# -------------------------------------------------------------------------------------------------
pdf(file = FL.internalLights.PDF,paper="a4r", width=0, height=0)	# this should have a integer index of 2.  Note: the "null device" is always device 1
pdf(file = FL.exteriorLights.PDF,paper="a4r", width=0, height=0)	# this should have a integer index of 3.  Note: the "null device" is always device 1


# ------------------------------------------------------------------------------------------------- 
# 	create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
FL.LOG <- paste(Path.log,"newProfile_LightsCircuitLoad.log",sep="/")	# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for data processing script [newProfile_LightsCircuitLoad.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
            "*                            [newProfile_LightsCircuitLoad.R]             *",
            "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)


# -------------------------------------------------------------------------------------------------
# Loop through lights.meter
# -------------------------------------------------------------------------------------------------
for (lights.meter in lights.meters)
{
	lights.meter.name <- names(lights.meters[lights.meters==lights.meter])		# the name of the end use
	zone.name         <- zones[lights.meter.name]					# get the zone name (added on Dec 3, 2009)
	
	Path.in.lights.meter   <- paste(Path.in,lights.meter.name,sep="/")		# INPUT 
	if (!file.exists(Path.in.lights.meter)){stop(paste("NOT existing:",Path.in.lights.meter," Check why you do not have it!\n",sep=""))}

	cat(paste("\n ------------------------------- Now processing data for [",lights.meter.name,"] ------------------------------- \n",sep=""))
	cat(paste("\n ------------------------------- Now processing data for [",lights.meter.name,"] ------------------------------- \n",sep=""),file=FL.LOG,append=TRUE)


	# -------------------------------------------------------------------------------------------------
	# 	define INPUT files
	# -------------------------------------------------------------------------------------------------
	FL.aggData.OBJ <- paste(Path.in.lights.meter,paste("CrownePlaza_",lights.meter.name,"_aggregate.Rdata",sep=""),sep="/")	# OUTPUT  SUM  in Rdata Objects
	if (!file.exists(FL.aggData.OBJ)){stop(paste(" INPUT  data ",FL.aggData.OBJ," does NOT exist\n",sep=""))}		# remove existing OUTPUT files
	cat(paste("\n1. All paths and files are defined for [",lights.meter.name,"]\n",sep=""),file=FL.LOG,append=TRUE)
	cat(paste("\n1. All paths and files are defined for [",lights.meter.name,"]\n",sep=""))

	# -------------------------------------------------------------------------------------------------
	# 	define OUTPUT files
	# -------------------------------------------------------------------------------------------------
	FL.profile.OBJ <- paste(Path.out, paste("CrownePlaza_Profile_",lights.meter.name,".Rdata",sep=""),sep="/")		# OUTPUT Profile of Thermal SetPoint
	FL.profile.CSV <- paste(Path.out, paste("CrownePlaza_Profile_",lights.meter.name,".CSV",sep=""),sep="/")		# OUTPUT Profile of Thermal SetPoint
	FL.profile.PDF <- paste(Path.out, paste("CrownePlaza_Profile_",lights.meter.name,".PDF",sep=""),sep="/")		# OUTPUT Profile of Thermal SetPoint
	FL.profile.IDF <- paste(Path.out, paste("CrownePlaza_Profile_",lights.meter.name,".IDF",sep=""),sep="/")		# OUTPUT Profile of Thermal SetPoint

	if (file.exists(FL.profile.OBJ)){print(paste(FL.profile.OBJ," exist. Delete it!"));file.remove(FL.profile.OBJ)}		# remove existing OUTPUT files
	if (file.exists(FL.profile.CSV)){print(paste(FL.profile.CSV," exist. Delete it!"));file.remove(FL.profile.CSV)}		# remove existing OUTPUT files
	if (file.exists(FL.profile.PDF)){print(paste(FL.profile.PDF," exist. Delete it!"));file.remove(FL.profile.PDF)}		# remove existing OUTPUT files
	if (file.exists(FL.profile.IDF)){print(paste(FL.profile.IDF," exist. Delete it!"));file.remove(FL.profile.IDF)}		# remove existing OUTPUT files
	cat(paste("\nAll paths and files are defined\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nAll paths and files are defined\n"))


	# -------------------------------------------------------------------------------------------------
	# 	load the data
	# -------------------------------------------------------------------------------------------------
	load(FL.aggData.OBJ)	# aggregated data
	cat(paste("\nLoaded the ON only and aggregated data retrieved from the database!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nLoaded the ON only and aggregated data retrieved from the database!\n"))


	#
	# output to the IDF file
	#
	cat(paste("\n\nnewProfile_LightsCircuit:[",lights.meter.name,"]",sep=""),file=FL.IDF,append=TRUE)
	write.table(Data.allDays.circuitAll,file=FL.IDF,append=TRUE,sep=",")


	# -------------------------------------------------------------------------------------------------
	# add a "month-day" field
	# -------------------------------------------------------------------------------------------------
	myData                 <- cbind(myData,                month.day = paste(myData[,"month"],myData[,"day"],sep="-"))
	Data.daily.circuitWise <- cbind(Data.daily.circuitWise,month.day = paste(Data.daily.circuitWise[,"month"],Data.daily.circuitWise[,"day"],sep="-"))

	
	# -------------------------------------------------------------------------------------------------
	# output the overall assembled profile for this group of meters
	# -------------------------------------------------------------------------------------------------
	load.overall      <- Data.allDays.circuitAll[1,"Value.max"]
	ltrs              <- substring(load.overall,1:nchar(load.overall),1:nchar(load.overall))
	load.overall.3dec <- ifelse(length(grep("\\.",ltrs)),substr(load.overall,1,which(ltrs==".")+3),load.overall)

	# ------------------------------------------------------------------------------------------------- 
	# Exterior Light is a different object from the indoor common area lights
	# -------------------------------------------------------------------------------------------------
	if (lights.meter.name == "Exterior_OutSide")
	{
		# 
		# the Overall peak power (W) of each guest room
		# 
		Eplus.object.name   <- zone.name
		Eplus.schedule.name <- paste("LightSCH_",zone.name,sep="")

		# *********************************************************************************
		# A. Output to an individual file for each zone
		#
		cat(paste("\n\n! --------------------------- ",lights.meter.name," (",zone.name,") --------------------------------------\n",sep=""),file=FL.profile.IDF,append=TRUE)	
		cat(paste("Exterior:Lights,",                                               "\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ",Eplus.object.name,",                 !- Name\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ",Eplus.schedule.name,",        !- Schedule Name\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ",load.overall.3dec,",                         !- Design Level {W}\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    scheduleNameOnly,               !- Control Option\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    General;                        !- End-Use Subcategory\n\n",sep=""),file=FL.profile.IDF,append=TRUE)

		# 
		# the Overall fraction schedule of each guest room
		# 
		cat(paste("\n\nSchedule:Compact,                                               \n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ",Eplus.schedule.name,",                   !- Name\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    Fraction,                                  !- Schedule Type Limits Name","\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    Through: 12/31,                            !- Field 1",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    For: AllDays,                              !- Field 2",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
		# *********************************************************************************
		
		# *********************************************************************************
		# B. repeat to output to a single idf for all zones
		#
		cat(paste("\n\n! --------------------------- ",lights.meter.name," (",zone.name,") --------------------------------------\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)	
		cat(paste("Exterior:Lights,",                                               "\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		cat(paste("    ",Eplus.object.name,",                 !- Name\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		cat(paste("    ",Eplus.schedule.name,",        !- Schedule Name\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		cat(paste("    ",load.overall.3dec,",                         !- Design Level {W}\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		cat(paste("    scheduleNameOnly,               !- Control Option\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		cat(paste("    General;                        !- End-Use Subcategory\n\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)

		# 
		# the Overall fraction schedule of each guest room
		# 
		cat(paste("\n\nSchedule:Compact,                                               \n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		cat(paste("    ",Eplus.schedule.name,",                   !- Name\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		cat(paste("    Fraction,                                  !- Schedule Type Limits Name","\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		cat(paste("    Through: 12/31,                            !- Field 1",                  "\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		cat(paste("    For: AllDays,                              !- Field 2",                  "\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
		# *********************************************************************************
			
		
		
		for (idx.hour in seq(from=0,to=22,by=1))
		{

			idx.field    <- 3 + idx.hour
			on.freq      <- Data.allDays.circuitAll[Data.allDays.circuitAll[,"hour"] == idx.hour,"on.freq"]
			ltrs         <- substring(on.freq,1:nchar(on.freq),1:nchar(on.freq))
			on.freq.3dec <- ifelse(length(grep("\\.",ltrs)),substr(on.freq,1,which(ltrs==".")+3),on.freq)

			if (idx.hour<9)
			{
				cat(paste("    Until: ",idx.hour+1,":00, ",on.freq.3dec,",                        !- Field ",idx.field,   "\n",sep=""),file=FL.profile.IDF,append=TRUE)
				cat(paste("    Until: ",idx.hour+1,":00, ",on.freq.3dec,",                        !- Field ",idx.field,   "\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
			}else{
				cat(paste("    Until: ",idx.hour+1,":00, ",on.freq.3dec,",                       !- Field ",idx.field,   "\n",sep=""),file=FL.profile.IDF,append=TRUE)
				cat(paste("    Until: ",idx.hour+1,":00, ",on.freq.3dec,",                       !- Field ",idx.field,   "\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)
			}
		}

		idx.field    <- idx.field + 1
		on.freq      <- Data.allDays.circuitAll[Data.allDays.circuitAll[,"hour"] == 23,"on.freq"]
		ltrs         <- substring(on.freq,1:nchar(on.freq),1:nchar(on.freq))
		on.freq.3dec <- ifelse(length(grep("\\.",ltrs)),substr(on.freq,1,which(ltrs==".")+3),on.freq)

		cat(paste("    Until: ",24,":00, ",on.freq.3dec,";                       !- Field ",idx.field,"\n",sep=""),file=FL.profile.IDF,append=TRUE)	
		cat(paste("    Until: ",24,":00, ",on.freq.3dec,";                       !- Field ",idx.field,"\n",sep=""),file=FL.exteriorLights.IDF,append=TRUE)	
	}else{
		# 
		# the Overall peak power (W) of each guest room
		# 
		Eplus.object.name   <- paste("Lights_",zone.name,sep="")
		Eplus.zone.name     <- zone.name
		Eplus.schedule.name <- paste("LightSCH_",zone.name,sep="")

		# *********************************************************************************
		# A. Output to an individual file for each zone
		#
		cat(paste("\n\n! --------------------------- ",lights.meter.name," (",zone.name,") --------------------------------------\n",sep=""),file=FL.profile.IDF,append=TRUE)	
		cat(paste("Lights,",                                               "\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ",Eplus.object.name,",                  !- Name\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ",Eplus.zone.name,",                         !- Zone Name\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ",Eplus.schedule.name,",                !- Schedule Name\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    LightingLevel,                                   !- Design Level Calculation Method\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ",load.overall.3dec,",                                          !- Lighting Level {W}\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ,                                                !- Watts per Zone Floor Area {W/m2}\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ,                                                !- Watts per Person {W/person}\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    0,                                               !- Return Air Fraction\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    0.42,                                            !- Fraction Radiant\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    0.18,                                            !- Fraction Visible\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    1;                                               !- Fraction Replaceable\n\n",sep=""),file=FL.profile.IDF,append=TRUE)

		# 
		# the Overall fraction schedule of each guest room
		# 
		cat(paste("\n\nSchedule:Compact,                                               \n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    ",Eplus.schedule.name,",          !- Name\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    Fraction,                                  !- Schedule Type Limits Name","\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    Through: 12/31,                            !- Field 1",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    For: AllDays,                              !- Field 2",                  "\n",sep=""),file=FL.profile.IDF,append=TRUE)
		# *********************************************************************************
		
		# *********************************************************************************
		# B. repeat to output to a single idf for all zones
		#
		cat(paste("\n\n! --------------------------- ",lights.meter.name," (",zone.name,") --------------------------------------\n",sep=""),file=FL.internalLights.IDF,append=TRUE)	
		cat(paste("Lights,",                                               "\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    ",Eplus.object.name,",                  !- Name\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    ",Eplus.zone.name,",                         !- Zone Name\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    ",Eplus.schedule.name,",                !- Schedule Name\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    LightingLevel,                                   !- Design Level Calculation Method\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    ",load.overall.3dec,",                                          !- Lighting Level {W}\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    ,                                                !- Watts per Zone Floor Area {W/m2}\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    ,                                                !- Watts per Person {W/person}\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    0,                                               !- Return Air Fraction\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    0.42,                                            !- Fraction Radiant\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    0.18,                                            !- Fraction Visible\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    1;                                               !- Fraction Replaceable\n\n",sep=""),file=FL.internalLights.IDF,append=TRUE)

		# 
		# the Overall fraction schedule of each guest room
		# 
		cat(paste("\n\nSchedule:Compact,                                               \n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    ",Eplus.schedule.name,",          !- Name\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    Fraction,                                  !- Schedule Type Limits Name","\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    Through: 12/31,                            !- Field 1",                  "\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		cat(paste("    For: AllDays,                              !- Field 2",                  "\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
		# *********************************************************************************
		
		for (idx.hour in seq(from=0,to=22,by=1))
		{

			idx.field    <- 3 + idx.hour
			on.freq      <- Data.allDays.circuitAll[Data.allDays.circuitAll[,"hour"] == idx.hour,"on.freq"]
			ltrs         <- substring(on.freq,1:nchar(on.freq),1:nchar(on.freq))
			on.freq.3dec <- ifelse(length(grep("\\.",ltrs)),substr(on.freq,1,which(ltrs==".")+3),on.freq)

			if (idx.hour<9)
			{
				cat(paste("    Until: ",idx.hour+1,":00, ",on.freq.3dec,",                        !- Field ",idx.field,   "\n",sep=""),file=FL.profile.IDF,append=TRUE)
				cat(paste("    Until: ",idx.hour+1,":00, ",on.freq.3dec,",                        !- Field ",idx.field,   "\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
			}else{
				cat(paste("    Until: ",idx.hour+1,":00, ",on.freq.3dec,",                       !- Field ",idx.field,   "\n",sep=""),file=FL.profile.IDF,append=TRUE)
				cat(paste("    Until: ",idx.hour+1,":00, ",on.freq.3dec,",                       !- Field ",idx.field,   "\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
			}
		}

		idx.field    <- idx.field + 1
		on.freq      <- Data.allDays.circuitAll[Data.allDays.circuitAll[,"hour"] == 23,"on.freq"]
		ltrs         <- substring(on.freq,1:nchar(on.freq),1:nchar(on.freq))
		on.freq.3dec <- ifelse(length(grep("\\.",ltrs)),substr(on.freq,1,which(ltrs==".")+3),on.freq)

		cat(paste("    Until: ",24,":00, ",on.freq.3dec,";                       !- Field ",idx.field,"\n",sep=""),file=FL.profile.IDF,append=TRUE)
		cat(paste("    Until: ",24,":00, ",on.freq.3dec,";                       !- Field ",idx.field,"\n",sep=""),file=FL.internalLights.IDF,append=TRUE)
	}


	# -------------------------------------------------------------------------------------------------
	# A. generate plot the profiles of individual meters within this group of meters
	# -------------------------------------------------------------------------------------------------
	# get the y limit for plotting
	y.limit <- range(Data.allDays.circuitWise[,"Value.max"],na.rm=TRUE)

	# round to tenth place		
	y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)

	key.dailyCurve <- list(title="light circuit",columns=5,space="top",cex=0.75,
			       text=list(paste(levels(as.factor(Data.allDays.circuitWise[,"circuit.name"])),sep=" ")),
			       type = "b",
			       lines=list(lty=lty.array[1: nlevels(as.factor(Data.allDays.circuitWise[,"circuit.name"]))],
					  col=col.array[1: nlevels(as.factor(Data.allDays.circuitWise[,"circuit.name"]))],
					  pch=pch.array[1: nlevels(as.factor(Data.allDays.circuitWise[,"circuit.name"]))]))

	# profile of maximum power (W)				   
	plot.obj1 <- xyplot(Data.allDays.circuitWise[,"Value.max"] ~ Data.allDays.circuitWise[,"hour"]  ,
			   group = Data.allDays.circuitWise[,"circuit.name"],
			   xlab="Hour",ylab = paste("Power (W)",sep=""),
			   main=paste("(",lights.meter.name,") Profile of each light circuit across multiple days",sep=""),
			   cex=0.5,type="b",
			   key = key.dailyCurve,
			   lty=lty.array[1: nlevels(as.factor(Data.allDays.circuitWise[,"circuit.name"]))],		   
			   col=col.array[1: nlevels(as.factor(Data.allDays.circuitWise[,"circuit.name"]))],
			   pch=pch.array[1: nlevels(as.factor(Data.allDays.circuitWise[,"circuit.name"]))],
			   scales=list(x = list(at     = seq(from=0,to=23,by=1),
						labels = seq(from=0,to=23,by=1),
						limits = c(-1,25)),
				       y = y.limit)
			    )	


	# profile of "ON fraction"
	y.limit <- c(0,1)
	plot.obj2 <- xyplot(Data.allDays.circuitWise[,"on.freq"] ~ Data.allDays.circuitWise[,"hour"]  ,
			   group = Data.allDays.circuitWise[,"circuit.name"],
			   xlab="Hour",ylab = "ON Fraction",
			   main=paste("(",lights.meter.name,") Profile of each light circuit across multiple days",sep=""),
			   cex=0.5,type="b",
			   key = key.dailyCurve,
			   lty=lty.array[1: nlevels(as.factor(Data.allDays.circuitWise[,"circuit.name"]))],		   
			   col=col.array[1: nlevels(as.factor(Data.allDays.circuitWise[,"circuit.name"]))],
			   pch=pch.array[1: nlevels(as.factor(Data.allDays.circuitWise[,"circuit.name"]))],
			   scales=list(x = list(at     = seq(from=0,to=23,by=1),
						labels = seq(from=0,to=23,by=1),
						limits = c(-1,25)),
				       y = y.limit)
			    )	


	 
	# -------------------------------------------------------------------------------------------------
	# B. plot the profiles of individual meters within this group of meters into their own pdf file
	# -------------------------------------------------------------------------------------------------
	pdf(file = FL.profile.PDF,paper="a4r", width=0, height=0)	# this should have a integer index of 4.  Note: the "null device" is always device 1
		plot(plot.obj1)  	
		plot(plot.obj2)  
		
		#
		# plot the overall schedule
		#
		 plot(Data.allDays.circuitAll[,"hour"],Data.allDays.circuitAll[,"on.freq"],
		      type="b",lty=1,pch=16, lwd=2,col="red",cex=1.5,ylim=y.limit,
		      xlab="hour",ylab="ON Fraction",
		      lab= c(23,23,7))
		abline(v=seq(from=0,to=23,by=1),col="grey",lty=2)

		legend(1, max(y.limit),legend=c(paste("Overall:",load.overall.3dec,"(W)",sep="")),
					  col=c("red"),
					  lty=c(1),
					  text.col = c("red")) 	
	dev.off()	# this is the current graphic device which should have integer 4.  Note: the "null device" is always device 1
	
	# *****************************************************************************************
	# the next part is to plot the same plots into a different pdf file which is used to store plots for all internal lights in one pdf and all exterior plots into another pdf file
	# *****************************************************************************************
	if (lights.meter.name == "Exterior_OutSide")
	{
		dev.set(3)	# switch to graphic device 3 which if for "exterior lightcircuit"
		plot(plot.obj1)  	
		plot(plot.obj2)  
		
		#
		# plot the overall schedule
		#
		 plot(Data.allDays.circuitAll[,"hour"],Data.allDays.circuitAll[,"on.freq"],
		      type="b",lty=1,pch=16, lwd=2,col="red",cex=1.5,ylim=y.limit,
		      xlab="hour",ylab="ON Fraction",
		      lab= c(23,23,7))
		abline(v=seq(from=0,to=23,by=1),col="grey",lty=2)

		legend(1, max(y.limit),legend=c(paste("Overall:",load.overall.3dec,"(W)",sep="")),
					  col=c("red"),
					  lty=c(1),
					  text.col = c("red")) 
	}else{
		dev.set(4)	# switch to graphic device 4 which if for "interior lightcircuits"
		plot(plot.obj1)  	
		plot(plot.obj2)  
		
		#
		# plot the overall schedule
		#
		 plot(Data.allDays.circuitAll[,"hour"],Data.allDays.circuitAll[,"on.freq"],
		      type="b",lty=1,pch=16, lwd=2,col="red",cex=1.5,ylim=y.limit,
		      xlab="hour",ylab="ON Fraction",
		      lab= c(23,23,7))
		abline(v=seq(from=0,to=23,by=1),col="grey",lty=2)

		legend(1, max(y.limit),legend=c(paste("Overall:",load.overall.3dec,"(W)",sep="")),
					  col=c("red"),
					  lty=c(1),
					  text.col = c("red")) 
			
	}
}
dev.off(2)
dev.off(3)



	    
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\nnewProfile_LightsCircuitLoad.R is finished successfully at ",End.time,"]!\n",sep=" "))
cat(paste("\nnewProfile_LightsCircuitLoad.R is finished successfully at ",End.time,"]!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [newProfile_LightsCircuitLoad.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [newProfile_LightsCircuitLoad.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

