#
# 01_gatheringData_LargeOffice.R 
# 
# Feb 27, 2015: re-visit the scripts
#
# Feb 3, 2015: re-generate this data for Data Mining purpose
#
# Large Office:
# Simulated at Miami for all of the individual EEMs
#
#

# eliminate all stuff
rm(list = ls(all = TRUE))


# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

library("chron")
library("reshape2")

# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number



# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Project <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining"
	Path.Sim     <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining"
	Path.Script  <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/0_scripts"
}else{
	Path.Project <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining"
	Path.Sim     <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining"
	Path.Script  <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/0_scripts"
	
}
setwd(Path.Script)

# E+ output file folder
Path.IN <- paste(Path.Project,"copy.csv.file.gz",sep="/")
if (!file.exists(Path.IN)){print(paste("NOT existing:",Path.IN,".  Check Why!/n",sep=""));die}

# results output folder
Path.OUT <- paste(Path.Sim,"01_gatheringData_LargeOffice",sep="/")
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}

# FL.LOG
FL.LOG <- paste(Path.OUT,"01_gatheringData_LargeOffice.log",sep="/")
if (file.exists(FL.LOG)){print(paste(FL.LOG," exist. Delete it!"));file.remove(FL.LOG)}		
cat(paste("1. defined files/folders.\n",sep=""))
cat(paste("1. defined files/folders.\n",sep=""),file=FL.LOG,append=TRUE)	


# CZ arrays
CZ.arrays <- c("Miami","Houston","Phoenix","Atlanta","LosAngeles","LasVegas","SanFrancisco","Baltimore","Albuquerque","Seattle","Chicago","Denver","Minneapolis","Helena","Duluth","Fairbanks")

# EEMs of all CZs
      EEMs.all.CZs <- c("EEM00Base_OfficeLarge_base_Miami.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Miami.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Miami.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Miami.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Miami.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Miami.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Miami.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Miami.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Miami.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Miami.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Miami.csv",
			"EEM00Base_OfficeLarge_base_Houston.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Houston.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Houston.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Houston.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Houston.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Houston.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Houston.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Houston.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Houston.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Houston.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Houston.csv",
			"EEM00Base_OfficeLarge_base_Phoenix.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Phoenix.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Phoenix.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Phoenix.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Phoenix.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Phoenix.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Phoenix.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Phoenix.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Phoenix.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Phoenix.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Phoenix.csv",
			"EEM00Base_OfficeLarge_base_Atlanta.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Atlanta.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Atlanta.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Atlanta.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Atlanta.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Atlanta.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Atlanta.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Atlanta.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Atlanta.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Atlanta.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Atlanta.csv",
			"EEM00Base_OfficeLarge_base_LosAngeles.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_LosAngeles.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_LosAngeles.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_LosAngeles.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_LosAngeles.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_LosAngeles.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_LosAngeles.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_LosAngeles.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_LosAngeles.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_LosAngeles.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_LosAngeles.csv",
			"EEM00Base_OfficeLarge_base_LasVegas.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_LasVegas.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_LasVegas.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_LasVegas.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_LasVegas.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_LasVegas.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_LasVegas.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_LasVegas.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_LasVegas.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_LasVegas.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_LasVegas.csv",
			"EEM00Base_OfficeLarge_base_SanFrancisco.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_SanFrancisco.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_SanFrancisco.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_SanFrancisco.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_SanFrancisco.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_SanFrancisco.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_SanFrancisco.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_SanFrancisco.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_SanFrancisco.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_SanFrancisco.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_SanFrancisco.csv",
			"EEM00Base_OfficeLarge_base_Baltimore.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Baltimore.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Baltimore.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Baltimore.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Baltimore.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Baltimore.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Baltimore.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Baltimore.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Baltimore.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Baltimore.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Baltimore.csv",
			"EEM00Base_OfficeLarge_base_Albuquerque.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Albuquerque.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Albuquerque.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Albuquerque.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Albuquerque.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Albuquerque.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Albuquerque.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Albuquerque.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Albuquerque.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Albuquerque.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Albuquerque.csv",
			"EEM00Base_OfficeLarge_base_Seattle.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Seattle.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Seattle.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Seattle.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Seattle.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Seattle.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Seattle.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Seattle.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Seattle.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Seattle.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Seattle.csv",
			"EEM00Base_OfficeLarge_base_Chicago.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Chicago.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Chicago.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Chicago.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Chicago.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Chicago.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Chicago.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Chicago.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Chicago.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Chicago.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Chicago.csv",
			"EEM00Base_OfficeLarge_base_Denver.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Denver.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Denver.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Denver.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Denver.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Denver.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Denver.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Denver.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Denver.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Denver.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Denver.csv",
			"EEM00Base_OfficeLarge_base_Minneapolis.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Minneapolis.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Minneapolis.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Minneapolis.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Minneapolis.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Minneapolis.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Minneapolis.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Minneapolis.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Minneapolis.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Minneapolis.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Minneapolis.csv",
			"EEM00Base_OfficeLarge_base_Helena.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Helena.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Helena.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Helena.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Helena.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Helena.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Helena.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Helena.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Helena.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Helena.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Helena.csv",
			"EEM00Base_OfficeLarge_base_Duluth.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Duluth.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Duluth.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Duluth.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Duluth.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Duluth.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Duluth.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Duluth.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Duluth.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Duluth.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Duluth.csv",
			"EEM00Base_OfficeLarge_base_Fairbanks.csv",
			"EEM03Retune_OfficeLarge_LeakFixed_Fairbanks.csv",
			"EEM04Retune_OfficeLarge_HVACfixed_Fairbanks.csv",
			"EEM08Retune_OfficeLarge_ResetVAV_Fairbanks.csv",
			"EEM16Retune_OfficeLarge_LowerAirflow_Fairbanks.csv",
			"EEM17Retune_OfficeLarge_WidenDeadb_Fairbanks.csv",
			"EEM27AdvCtrl_OfficeLarge_ClTowerCtrl_Fairbanks.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStop_Fairbanks.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStart_Fairbanks.csv",
			"EEM33AdvCtrl_OfficeLarge_OptimalStartStop_Fairbanks.csv",
			"EEM44Retrofit_OfficeLarge_BoilerEff_Fairbanks.csv")
cat(paste("2. define [EEMs.all.CZs].\n",sep=""))
cat(paste("2. define [EEMs.all.CZs].\n",sep=""),file=FL.LOG,append=TRUE)	



for (this.CZ in CZ.arrays)
{
	Path.CZ.OUT <- paste(Path.OUT,this.CZ,sep="/")
	if (!file.exists(Path.CZ.OUT)){print(paste("NOT existing:",Path.CZ.OUT));dir.create(Path.CZ.OUT,showWarnings=TRUE,recursive=TRUE)}
	cat(paste("3. [",this.CZ,"]: Output folder has been set.\n",sep=""))
	cat(paste("3. [",this.CZ,"]: Output folder has been set.\n",sep=""),file=FL.LOG,append=TRUE)	


	FL.LargeOffice.OBJ <- paste(Path.CZ.OUT,paste("GatheringData_LargeOffice.Rdata",sep=""),sep="/")
	FL.LargeOffice.CSV <- paste(Path.CZ.OUT,paste("GatheringData_LargeOffice.csv",  sep=""),sep="/")
	FL.LargeOffice.PDF <- paste(Path.CZ.OUT,paste("GatheringData_LargeOffice.pdf",  sep=""),sep="/")
	FL.LargeOffice.LOG <- paste(Path.CZ.OUT,paste("GatheringData_LargeOffice.log",  sep=""),sep="/")

	if (file.exists(FL.LargeOffice.OBJ)){print(paste(FL.LargeOffice.OBJ," exist. Delete it!"));file.remove(FL.LargeOffice.OBJ)}		
	if (file.exists(FL.LargeOffice.CSV)){print(paste(FL.LargeOffice.CSV," exist. Delete it!"));file.remove(FL.LargeOffice.CSV)}		
	if (file.exists(FL.LargeOffice.PDF)){print(paste(FL.LargeOffice.PDF," exist. Delete it!"));file.remove(FL.LargeOffice.PDF)}		
	if (file.exists(FL.LargeOffice.LOG)){print(paste(FL.LargeOffice.LOG," exist. Delete it!"));file.remove(FL.LargeOffice.LOG)}		
	cat(paste("4. [",this.CZ,"]: Output files have been specified.\n",sep=""))
	cat(paste("4. [",this.CZ,"]: Output files have been specified.\n",sep=""),file=FL.LOG,append=TRUE)	


	pdf(file = FL.LargeOffice.PDF,paper="special", width=17, height=11,bg = "transparent")
	cat(paste("5. [",this.CZ,"]: Open pdf file for plotting.\n",sep=""))
	cat(paste("5. [",this.CZ,"]: Open pdf file for plotting.\n",sep=""),file=FL.LOG,append=TRUE)	
	

	week.label     <- c( 1,       2,       3,        4,          5,         6,       7,         8)
	week.names     <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Holiday")
	week.fullNames <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Holiday")
	names(week.fullNames) <- week.names

	dayType.label     <- c( 0,        1,         2,       3)
	dayType.names     <- c("Weekday","Saturday","Sunday","Holiday")
	dayType.fullNames <- c("Weekday","Saturday","Sunday","Holiday")
	names(dayType.fullNames) <- dayType.names

	# the simulation assume the first day is sunday
	first.sunday <- chron(dates="01/01/2010",times="0:0:0",  format=c('m/d/y','h:m:s'))
	cat(paste("6. [",this.CZ,"]: specified week labels, day type and first.sunday.\n",sep=""))
	cat(paste("6. [",this.CZ,"]: specified week labels, day type and first.sunday.\n",sep=""),file=FL.LOG,append=TRUE)	

	# get the EEMs 
	EEMs.names <- grep(this.CZ,EEMs.all.CZs,value=TRUE)
	cat(paste("7A. [",this.CZ,"]: Subsetting the EEMs from [EEMs.all.CZs].\n",sep=""))
	cat(paste("7A. [",this.CZ,"]: Subsetting the EEMs from [EEMs.all.CZs].\n",sep=""),file=FL.LOG,append=TRUE)	

	EEMs.number <- sub("EEM(\\d+)(.*)","\\1",EEMs.names)
	EEMs.label  <- sub("EEM(.*)_(.*)_(.*)_(.*)","\\3",EEMs.names)
	cat(paste("7B. [",this.CZ,"]: Extracted [EEM.number] and [EEMs.label] from [EEMs.names].\n",sep=""))
	cat(paste("7B. [",this.CZ,"]: Extracted [EEM.number] and [EEMs.label] from [EEMs.names].\n",sep=""),file=FL.LOG,append=TRUE)	

	for (this.idx in seq(from=1,to=length(EEMs.names)))
	{
		this.EEM.name   <- EEMs.names[this.idx]
		this.EEM.number <- EEMs.number[this.idx]
		this.EEM.label  <- EEMs.label[this.idx]
		cat(paste("\n\n8. [",this.CZ,"]-[",this.EEM.name,"]: loopping for each EEM.\n",sep=""))
		cat(paste("\n\n8. [",this.CZ,"]-[",this.EEM.name,"]: loopping for each EEM.\n",sep=""),file=FL.LOG,append=TRUE)	



		# E+ output file folder
		this.file <- paste(Path.IN,this.EEM.name,this.EEM.name,sep="/")
		cat(paste("9. [",this.CZ,"]-[",this.EEM.name,"]: specified E+ output file name.\n",sep=""))
		cat(paste("9. [",this.CZ,"]-[",this.EEM.name,"]: specified E+ output file name.\n",sep=""),file=FL.LOG,append=TRUE)	

		myTmp <- read.table(file=this.file,sep=",",header=TRUE)
		names(myTmp) <- c("Date.Time","T.dryBulb","T.WetBulb","Elec(GJ)","Gas(GJ)")

		myTmp.Part1 <- myTmp[49:8808,1:3]
		myTmp.Part2 <- myTmp[49:8808,4:5]/1000000000	# turn J to GJ
		cat(paste("10. [",this.CZ,"]-[",this.EEM.name,"]: loaded the data and convert J to GJ.\n",sep=""))
		cat(paste("10. [",this.CZ,"]-[",this.EEM.name,"]: loaded the data and convert J to GJ.\n",sep=""),file=FL.LOG,append=TRUE)	

		if (this.idx == 1)
		{
			# split the data into two parts
			myBase.Part1 <- myTmp.Part1
			myBase.Part2 <- myTmp.Part2
			myBase       <- cbind(myBase.Part1,myBase.Part2)

			# rename the fields with the EEM name in the fields of [myBase]
			names(myBase) <- sub("Gas\\(GJ\\)",paste("Gas",this.EEM.number,this.EEM.label,sep="_"),sub("Elec\\(GJ\\)",paste("Elec",this.EEM.number,this.EEM.label,sep="_"),names(myBase)))


			# add date, time, chron.date fields
			myBase[,"date"] <- paste(sub("\\s+(.*[^ ])(\\s+)(.*)","\\1",as.character(myBase[,"Date.Time"])),"2010",sep="/")

			# Get month and day
			myBase[,"month"] <- as.numeric(sub("(.*)/(.*)/(.*)","\\1",myBase[,"date"]))
			myBase[,"day"]   <- as.numeric(sub("(.*)/(.*)/(.*)","\\2",myBase[,"date"]))

			# get time
			myBase[,"time"] <- sub("\\s+(.*[^ ])(\\s+)(.*)","\\3",as.character(myBase[,"Date.Time"]))

			# need to change hour from 1-2 to 0-23 in order to use chron.date
			myBase[,"hour"]  <- as.numeric(sub("(.*):(.*):(.*)","\\1",myBase[,"time"]))-1

			myBase[,"time.new"] <- paste(myBase[,"hour"],sub("(.*):(.*):(.*)","\\2",myBase[,"time"]),sub("(.*):(.*):(.*)","\\3",myBase[,"time"]),sep=":")

			myBase[,"chron.date"] <-  chron(dates  = myBase[,"date"],
							times  = myBase[,"time.new"],
							format = c('m/d/y','h:m:s'))

			myBase[,"chron.day"] <-  chron(dates  = myBase[,"date"],
						       times  = "00:00:00",
						       format = c('m/d/y','h:m:s'))

			myBase[,"day.in.year"] <- as.numeric(myBase[,"chron.day"])-as.numeric(myBase[1,"chron.day"]) + 	 1 		                       


			# the start day is sunday: 1: sunday, 2: monday, ..., 6 saturday
			myBase[,"day.week.num"] <- rep((seq(1:365) %% 7),each=24)
			myBase[myBase[,"day.week.num"]==0,"day.week.num"] <- 7

			# day type
			myBase[,"day.type.num"] <- rep(0,dim(myBase)[1])					# Monday - Friday
			myBase[(myBase[,"day.week.num"] == 7),"day.type.num"] <- 1				# saturday
			myBase[(myBase[,"day.week.num"] == 1),"day.type.num"] <- 2				# sunday

			# hour in a week: this has to be before the addition of te holiday type.
			myBase[,"hour.in.week"] <- (myBase[,"day.week.num"]-1) * 24 + myBase[,"hour"]

			
			# hard code the holiday: this need to be after the creation of the "hour.in.week" field.
			myBase[(myBase[,"month"] == 1  & myBase[,"day"] == 1)  ,"day.type.num"] <- 3		# New Year
			myBase[(myBase[,"month"] == 1  & myBase[,"day"] == 16) ,"day.type.num"] <- 3		# MLK Day: 3rd Monday in January
			myBase[(myBase[,"month"] == 2  & myBase[,"day"] == 20) ,"day.type.num"] <- 3		# Presidents Day, 3rd Monday in February
			myBase[(myBase[,"month"] == 5  & myBase[,"day"] == 29) ,"day.type.num"] <- 3		# Memorial Day, Last Monday in May
			myBase[(myBase[,"month"] == 7  & myBase[,"day"] == 4)  ,"day.type.num"] <- 3		# July 4, independent day
			myBase[(myBase[,"month"] == 9  & myBase[,"day"] == 4)  ,"day.type.num"] <- 3		# Labor Day, 1st Monday in September
			myBase[(myBase[,"month"] == 10 & myBase[,"day"] == 9)  ,"day.type.num"] <- 3		# Columbus Day, 2nd Monday in October
			myBase[(myBase[,"month"] == 11 & myBase[,"day"] == 11) ,"day.type.num"] <- 3		# Nov 11, Verterans Days
			myBase[(myBase[,"month"] == 11 & myBase[,"day"] == 23) ,"day.type.num"] <- 3		# Thanksgiving, 4th Thursday in November
			myBase[(myBase[,"month"] == 12 & myBase[,"day"] == 25) ,"day.type.num"] <- 3		# Dec 25, Christmas day
			
			
			myBase[(myBase[,"month"] == 1  & myBase[,"day"] == 1)  ,"day.week.num"] <- 8		# New Year
			myBase[(myBase[,"month"] == 1  & myBase[,"day"] == 16) ,"day.week.num"] <- 8		# MLK Day: 8rd Monday in January
			myBase[(myBase[,"month"] == 2  & myBase[,"day"] == 20) ,"day.week.num"] <- 8		# Presidents Day, 8rd Monday in February
			myBase[(myBase[,"month"] == 5  & myBase[,"day"] == 29) ,"day.week.num"] <- 8		# Memorial Day, Last Monday in May
			myBase[(myBase[,"month"] == 7  & myBase[,"day"] == 4)  ,"day.week.num"] <- 8		# July 4, independent day
			myBase[(myBase[,"month"] == 9  & myBase[,"day"] == 4)  ,"day.week.num"] <- 8		# Labor Day, 1st Monday in September
			myBase[(myBase[,"month"] == 10 & myBase[,"day"] == 9)  ,"day.week.num"] <- 8		# Columbus Day, 2nd Monday in October
			myBase[(myBase[,"month"] == 11 & myBase[,"day"] == 11) ,"day.week.num"] <- 8		# Nov 11, Verterans Days
			myBase[(myBase[,"month"] == 11 & myBase[,"day"] == 23) ,"day.week.num"] <- 8		# Thanksgiving, 4th Thursday in November
			myBase[(myBase[,"month"] == 12 & myBase[,"day"] == 25) ,"day.week.num"] <- 8		# Dec 25, Christmas day
			cat(paste("11. [",this.CZ,"]-[",this.EEM.name,"]: national holidays.\n",sep=""))
			cat(paste("11. [",this.CZ,"]-[",this.EEM.name,"]: national holidays.\n",sep=""),file=FL.LOG,append=TRUE)	
			
			

			# CONVERT TO FACTOR		
			myBase[,"day.week.lab"] <- factor(myBase[,"day.week.num"],levels = week.label,   labels=week.names)		# convert 1-7 to Sun,Mon,...,Sat
			myBase[,"day.type.lab"] <- factor(myBase[,"day.type.num"],levels = dayType.label,labels=dayType.names)		# convert 1 & 7 to "wkend and 2-6 to "wkday"


			myBase    <- data.frame(myBase,
						week.idx = as.numeric(ceiling((myBase[,"chron.date"]    - first.sunday + 1/48)/7)))	# plus 1/48 (half hour).  plus (5/60)/48 which is half of a 5 minute interval is to make sure Nov 16, 2009, 00:00:00 is the start of the second week




			# re-assign to [myData.wide] which will hold all the data
			myData.wide <- myBase
			
			cat(paste("12. [",this.CZ,"]-[",this.EEM.name,"]: [myBase].\n",sep=""))
			cat(paste("12. [",this.CZ,"]-[",this.EEM.name,"]: [myBase].\n",sep=""),file=FL.LOG,append=TRUE)	
			
		}else{
			# split the data into two parts
			myAdvn.Part1 <- myTmp.Part1
			myAdvn.Part2 <- myTmp.Part2
			myAdvn       <- cbind(myAdvn.Part1,myAdvn.Part2)

			# rename the fields with the EEM name in the fields of [myBase]
			names(myAdvn) <- sub("Gas\\(GJ\\)",paste("Gas",this.EEM.number,this.EEM.label,sep="_"),sub("Elec\\(GJ\\)",paste("Elec",this.EEM.number,this.EEM.label,sep="_"),names(myAdvn)))

			# check if the first three columns are the same as those in baseline file
			if (setequal(myBase.Part1,myAdvn.Part1))
			{
				myData.wide <- cbind(myData.wide,myAdvn[,c(4,5)])

				# generate some plots
				old.par <- par(mfrow=c(2, 2))

				# plot 1: time series of the baseline electricity/gas usage in Joule
				plot(myBase[,"chron.date"],myBase.Part2[,1],type="l",lty=1,col="red", main=paste("Electricity/Gas Time Series (GJ) [baseline]",sep=""),xlab = "Date",ylab = "GJ")		# electricity
			       lines(myBase[,"chron.date"],myBase.Part2[,2],type="l",lty=1,col="blue")														# gas

				# plot 2: : time series of the advan electricity/gas usage in Joule
				plot(myBase[,"chron.date"],myAdvn.Part2[,1],type="l",lty=1,col="red", main=paste("Electricity/Gas Time Series (GJ) [",this.EEM.number,"]-[",this.EEM.label,"]",sep=""),xlab = "Date",ylab = "GJ")	# electricity
			       lines(myBase[,"chron.date"],myAdvn.Part2[,2],type="l",lty=1,col="blue")														# gas

				# plot 3: : scatter plot of advn electricity vs baseline electricity 			
				plot(myBase.Part2[,1],myAdvn.Part2[,1],type="p",pch=16,col="red", cex=0.5,main=paste("Electricity (GJ) [",this.EEM.number,"]-[",this.EEM.label,"] vs [baseline]",sep=""),xlab = "baseline",ylab = this.EEM.label)
				abline(a=0,b=1,lty=1,col="black",cex=1.5)

				# plot 4: : scatter plot of advn gas vs baseline electricity 			
				plot(myBase.Part2[,2],myAdvn.Part2[,2],type="p",pch=16,col="blue",cex=0.5,main=paste("Natural Gas (GJ) [",this.EEM.number,"]-[",this.EEM.label,"] vs [baseline]",sep=""),xlab = "baseline",ylab = this.EEM.label)
				abline(a=0,b=1,lty=1,col="black",cex=1.5)

				par(old.par)					
			}else{
				cat(paste("The first three coulmn in [",this.EEM.name," differ from those in baseline.  Check why?\n",sep=""))
			}
			cat(paste("13. [",this.CZ,"]-[",this.EEM.name,"]: all EEM other than baseline.\n",sep=""))
			cat(paste("13. [",this.CZ,"]-[",this.EEM.name,"]: all EEM other than baseline.\n",sep=""),file=FL.LOG,append=TRUE)
		}
	}
	dev.off()
	cat(paste("make long format\n",sep=""))
	cat(paste("14. [",this.CZ,"]: all EEMs has been processed.\n",sep=""))
	cat(paste("14. [",this.CZ,"]: all EEMs has been processed.\n",sep=""),file=FL.LOG,append=TRUE)



	#
	# split [myData.wide] into [myData.Elec.wide] and [myData.Gas.wide]
	#
	var.elec <- grep("Elec_",names(myData.wide),value=TRUE)
	var.gas  <- grep("Gas_",names(myData.wide),value=TRUE)
	var.common <- names(myData.wide)[!(names(myData.wide) %in% c(var.elec,var.gas))]

	myData.Elec.wide <- myData.wide[,c(var.common,var.elec)]
	myData.Gas.wide  <- myData.wide[,c(var.common,var.gas)]

	cat(paste("\n\n15. split [myData.wide] into [myData.Elec.wide] and [myData.Gas.wide].\n",sep=""))
	cat(paste("\n\n15. split [myData.wide] into [myData.Elec.wide] and [myData.Gas.wide].\n",sep=""),file=FL.LOG,append=TRUE)



	# 
	# turn into long format
	#
	myData.long      <- melt(myData.wide,     id.vars = var.common,measure.vars = c(var.elec,var.gas),value.name="value")
	myData.Elec.long <- melt(myData.Elec.wide,id.vars = var.common,measure.vars =   var.elec,         value.name="value")
	myData.Gas.long  <- melt(myData.Gas.wide, id.vars = var.common,measure.vars =   var.gas,          value.name="value")
	cat(paste("16. turn [myData.wide], [myData.Elec.wide], [myData.Gas.wide] into [myData.long], [myData.Elec.long], [myData.Gas.long].\n",sep=""))
	cat(paste("16. turn [myData.wide], [myData.Elec.wide], [myData.Gas.wide] into [myData.long], [myData.Elec.long], [myData.Gas.long].\n",sep=""),file=FL.LOG,append=TRUE)



	# extract identification variables for long format
	myData.long[,"fuel"]           <- sub("(.*)_(.*)_(.*)","\\1",myData.long[,"variable"])
	myData.long[,"EEM.idx"]        <- as.numeric(sub("(.*)_(.*)_(.*)","\\2",myData.long[,"variable"]))
	myData.long[,"EEM.name"]       <- sub("(.*)_(.*)_(.*)","\\3",myData.long[,"variable"])

	myData.Elec.long[,"fuel"]      <- sub("(.*)_(.*)_(.*)","\\1",myData.Elec.long[,"variable"])
	myData.Elec.long[,"EEM.idx"]   <- as.numeric(sub("(.*)_(.*)_(.*)","\\2",myData.Elec.long[,"variable"]))
	myData.Elec.long[,"EEM.name"]  <- sub("(.*)_(.*)_(.*)","\\3",myData.Elec.long[,"variable"])

	myData.Gas.long[,"fuel"]       <- sub("(.*)_(.*)_(.*)","\\1",myData.Gas.long[,"variable"])
	myData.Gas.long[,"EEM.idx"]    <- as.numeric(sub("(.*)_(.*)_(.*)","\\2",myData.Gas.long[,"variable"]))
	myData.Gas.long[,"EEM.name"]   <- sub("(.*)_(.*)_(.*)","\\3",myData.Gas.long[,"variable"])
	cat(paste("17. add identification fields into [myData.long], [myData.Elec.long], [myData.Gas.long].\n",sep=""))
	cat(paste("17. add identification fields into [myData.long], [myData.Elec.long], [myData.Gas.long].\n",sep=""),file=FL.LOG,append=TRUE)


	#
	# output
	#
	save(myData.wide,myData.Elec.wide,myData.Gas.wide,
	     myData.long,myData.Elec.long,myData.Gas.long,file=FL.LargeOffice.OBJ)
	cat(paste("18. save [myData.wide], [myData.Elec.wide], [myData.Gas.wide], [myData.long], [myData.Elec.long], [myData.Gas.long] into [",FL.LargeOffice.OBJ,"].\n",sep=""))
	cat(paste("18. save [myData.wide], [myData.Elec.wide], [myData.Gas.wide], [myData.long], [myData.Elec.long], [myData.Gas.long] into [",FL.LargeOffice.OBJ,"].\n",sep=""),file=FL.LOG,append=TRUE)


	cat("LargeOffice (wide format),",file=FL.LargeOffice.CSV,append=TRUE)
	write.table(myData.wide,file=FL.LargeOffice.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	cat(paste("19. save [myData.wide] are written out to [",FL.LargeOffice.CSV,"].\n",sep=""))
	cat(paste("19. save [myData.wide] are written out to [",FL.LargeOffice.CSV,"].\n",sep=""),file=FL.LOG,append=TRUE)

	# cat("LargeOffice (long format),",file=FL.LargeOffice.CSV,append=TRUE)
	# write.table(myData.long,file=FL.LargeOffice.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	# cat(paste("20. save [myData.long] are written out to [",FL.LargeOffice.CSV,"].\n",sep=""))
	# cat(paste("20. save [myData.long] are written out to [",FL.LargeOffice.CSV,"].\n",sep=""),file=FL.LOG,append=TRUE)


}
cat(paste("21. All CZs of all EEMs has been processed.\n",sep=""))
cat(paste("21. All CZs of all EEMs has been processed.\n",sep=""),file=FL.LOG,append=TRUE)









#
# more p;ots to compare the two time series in terms of weekly plots  
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n01_gatheringData_LargeOffice.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n01_gatheringData_LargeOffice.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LargeOffice.LOG,append=TRUE)

cat(paste("\nProcessing time for [01_gatheringData_LargeOffice.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [01_gatheringData_LargeOffice.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LargeOffice.LOG,append=TRUE)

cat(paste("This run was conducted in ",.Platform$OS.type,"\n",sep=""));
cat(paste("This run was conducted in ",.Platform$OS.type,"\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE);




# get the version of R used for this computation and the latest version released
current.Rversion <- R.Version()$version.string
tmp = readLines("http://cran.r-project.org/sources.html")
rls = tmp[grep("latest release", tmp) + 1L]			# the version number is in the next line of 'The latest release'
latest.Rversion  <- gsub("(.*R-|\\.tar\\.gz.*)", "", rls)	# "The latest release: R-2.13.0.tar.gz"
if (latest.Rversion != current.Rversion)
{
	cat(paste("\n\nyou may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""));
	cat(paste("\n\nyou may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE);
}else{
	cat(paste("\n\nThe R version you are using is the latest version released so far!\n",sep=""))
	cat(paste("\n\nThe R version you are using is the latest version released so far!\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)
}



# get the version information of the attached libraries
cat(paste("\n\nThe information of the packages you used for this calculation:\n"))
cat(paste("\n\nThe information of the packages you used for this calculation:\n"),file=FL.LargeOffice.LOG,append=TRUE)
tmp <- sessionInfo()
pkg.loaded <- tmp$otherPkgs
no.pkg.loaded <- length(pkg.loaded)
for (i in seq(1,no.pkg.loaded))
{
	cat(paste(pkg.loaded[[i]]$Package,":",pkg.loaded[[i]]$Version," ",pkg.loaded[[i]]$Date,"\n",sep=" "))
	cat(paste(pkg.loaded[[i]]$Package,":",pkg.loaded[[i]]$Version," ",pkg.loaded[[i]]$Date,"\n",sep=" "),file=FL.LargeOffice.LOG,append=TRUE)
}

