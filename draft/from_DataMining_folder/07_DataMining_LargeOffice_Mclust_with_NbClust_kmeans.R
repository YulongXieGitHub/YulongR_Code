#
# 07_DataMining_LargeOffice_Mclust_with_NbClust_kmeans.R 
# April 3, 2015
# On April 1, 2015 team meeting, Srinivas proposed to create new data sets based on the simulation data: use three weeks of EEm data and one week of baseline data for each EEM
#                                                     do cluster analysis 
#                                                     check the distribution of independent variables in the clusters
#
# March 19, 2015: tally the composition in term of dates in each cluster.
# 
# March 6, 2015: prepare plots for todays' meeting
#
# Feb 27, 2015: re-visit the scripts
#
# Feb 3, 2015: load the data for each CZ and each EEMs
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

library("RSNNS")
library("chron")
library("reshape2")
library("lattice")
library("ggplot2")
library("mclust")
library("fpc")
library("NbClust")

col.array <- c("red","blue","green","magenta","cyan","purple","brown","black")

# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

this.method.4.cluster <- "Mclust"
this.method.4.NbClust <- "kmean"


# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Project <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining"
	Path.Sim     <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining"
	Path.Script  <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/0_scripts"
	Path.EPW     <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/00_process_epw"
}else{
	Path.Project <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining"
	Path.Sim     <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining"
	Path.Script  <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/0_scripts"
	Path.EPW     <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/00_process_epw"
	
#	Path.Project <- "C:/Yulong_Projects/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining"
#	Path.Sim     <- "C:/Yulong_Projects/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining"
#	Path.Script  <- "C:/Yulong_Projects/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/0_scripts"		
#	Path.EPW     <- "C:/Yulong_Projects/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/00_process_epw"
}
setwd(Path.Script)

Path.IN  <- paste(Path.Sim,"02_Plotting_LargeOffice",         sep="/")
Path.OUT <- paste(Path.Sim,"07_DataMining_LargeOffice_Mclust_with_NbClust_kmeans",sep="/")
if (!file.exists(Path.IN)){print(paste(Path.IN," does not exist. Check why!",sep=""));die}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}

FL.LOG <- paste(Path.OUT,"07_DataMining_LargeOffice_Mclust_with_NbClust_kmeans.log",sep="/")
if (file.exists(FL.LOG)){print(paste(FL.LOG," exist. Delete it!"));file.remove(FL.LOG)}		

FL.EPW <- paste(Path.EPW,"epw.Rdata",sep="/")
if (!(file.exists(FL.EPW))){print(paste(FL.EPW," does not exist.Check why!"));die}		

# fields in epw weather file which have been used in Eplus Simulation
field.used.all   <- c("epw.T.drybulb","epw.T.dewpoint","epw.rel.humidity","epw.atm.pressure","epw.hor.ir.sky","epw.direct.norm.rad","epw.diffuse.hor.rad","epw.wind.direct","epw.wind.speed","epw.pres.weath.obs","epw.pres.weath.codes","epw.snow.depth","epw.liquid.precip.depth")
field.used.short <- c("epw.T.drybulb","epw.T.dewpoint","epw.rel.humidity","epw.atm.pressure","epw.hor.ir.sky","epw.direct.norm.rad","epw.diffuse.hor.rad","epw.wind.direct","epw.wind.speed")


# 1. load multiplot function
source("multipleplot.R")
cat(paste("1. insert a [multipleplot] function for ggplot2.\n",sep=""))
cat(paste("1. insert a [multipleplot] function for ggplot2.\n",sep=""),file=FL.LOG,append=TRUE)	

# 2. CZ arrays
CZ.arrays <- c("Miami","Houston","Phoenix","Atlanta","LosAngeles","LasVegas","SanFrancisco","Baltimore","Albuquerque","Seattle","Chicago","Denver","Minneapolis","Helena","Duluth","Fairbanks")
cat(paste("2. specify inptu/out file/folder.\n",sep=""))
cat(paste("2. specify inptu/out file/folder.\n",sep=""),file=FL.LOG,append=TRUE)	

# 3. Load the weather data
load(FL.EPW)
cat(paste("2b. a binary file with all epw weather data have been loaded.\n",sep=""))
cat(paste("2b. a binary file with all epw weather data have been loaded.\n",sep=""),file=FL.LOG,append=TRUE)	


cat(paste("---------------- Loopping through Climate Zone ----------------------.\n",sep=""))
cat(paste("---------------- Loopping through Climate Zone ----------------------.\n",sep=""),file=FL.LOG,append=TRUE)	

for (this.CZ in CZ.arrays[c(7,9,4,1,2,3,5,6,8,10:16)][c(1,3)])	#for (this.CZ in CZ.arrays)  c("SanFrancisco","Albuquerque")  c(7,9,4,8,3,2,5,6,10,11,12,13,14,15,16,1)
{	
	if (this.CZ == "SanFrancisco")
	{
		EEM.selected <- c(1,6)
	}else if (this.CZ == "Albuquerque")
	{
		EEM.selected <- c(2,8)		
	}else{
		EEM.selected <- c(1,6)
	}
	
	# 3. subfolder for each CZ	
	Path.CZ.IN  <- paste(Path.IN, this.CZ,sep="/")
	Path.CZ.OUT <- paste(Path.OUT,this.CZ,sep="/")
	if (!file.exists(Path.CZ.IN)) {print(paste(Path.CZ.IN," does not exist. Check why!",sep=""));die}
	if (!file.exists(Path.CZ.OUT)){print(paste("NOT existing:",Path.CZ.OUT));dir.create(Path.CZ.OUT,showWarnings=TRUE,recursive=TRUE)}
	cat(paste("3. [",this.CZ,"]: Output folder has been set.\n",sep=""))
	cat(paste("3. [",this.CZ,"]: Output folder has been set.\n",sep=""),file=FL.LOG,append=TRUE)	


	# -----------------------------------------------------------------------------------------
	# 4. extract all Rdata file name in this CZ folder
	# -----------------------------------------------------------------------------------------
	EEMs.arrays <- grep("\\.Rdata",list.files(Path.CZ.IN),value=TRUE)				# all EEM objects
	cat(paste("4. [",this.CZ,"]: extract all R object files into [EEMs.arrays].\n",sep=""))
	cat(paste("4. [",this.CZ,"]: extract all R object files into [EEMs.arrays].\n",sep=""),file=FL.LOG,append=TRUE)	
	
	# 5. delete "GatheringData_OfficeLarge.Rdata)
	EEMs.arrays <- grep("GatheringData",EEMs.arrays,value=TRUE,invert=TRUE)
	cat(paste("5. [",this.CZ,"]: delete [atheringData].\n",sep=""))
	cat(paste("5. [",this.CZ,"]: delete [atheringData].\n",sep=""),file=FL.LOG,append=TRUE)	
	
	# 6. stripping EEM names
	EEMs.name   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\1_\\2_\\3",EEMs.arrays)
	EEMs.fuel   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\1",EEMs.arrays)
	EEMs.num    <- as.numeric(sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\2",EEMs.arrays))
	EEMs.label  <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\3",EEMs.arrays)
	EEMs.saving <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\4",EEMs.arrays)
	cat(paste("6. [",this.CZ,"]: get EEM related information.\n",sep=""))
	cat(paste("6. [",this.CZ,"]: get EEM related information.\n",sep=""),file=FL.LOG,append=TRUE)	
	
	# 7. prepare a data frame consists of the information of all EEMs
	EEMs.df <- data.frame(EEMs.names  = EEMs.name,
	                      EEMs.fuel   = EEMs.fuel,
	                      EEMs.num    = EEMs.num,
	                      EEMs.label  = EEMs.label,
	                      EEMs.saving = EEMs.saving)
	cat(paste("7. [",this.CZ,"]: put all EEMs related into [EEMs.df].\n",sep=""))
	cat(paste("7. [",this.CZ,"]: put all EEMs related into [EEMs.df].\n",sep=""),file=FL.LOG,append=TRUE)	

	# get the EPW weather file at this CZ
	df.epw.thisCZ <- paste("EPW.",this.CZ,sep="")
	command.string.EPW <- paste("myEPW.thisCZ <- ",df.epw.thisCZ,sep="")
	eval(parse(text=command.string.EPW))
	cat(paste("7b. [",this.CZ,"]: the epw weather data has been store into [myEPW.thisCZ].\n",sep=""))
	cat(paste("7b. [",this.CZ,"]: the epw weather data has been store into [myEPW.thisCZ].\n",sep=""),file=FL.LOG,append=TRUE)	
	

	# -----------------------------------------------------------------------------------------
	# -----------------------------------------------------------------------------------------
	# -----------------------------------------------------------------------------------------
	# loopping through the EEMs                     
	# -----------------------------------------------------------------------------------------
	# -----------------------------------------------------------------------------------------
	# -----------------------------------------------------------------------------------------
	  for (this.idx in seq(1,length(EEMs.name))[EEM.selected])
	# for (this.idx in seq(1,length(EEMs.name)))
	{
		this.EEM.name   = EEMs.name[this.idx]
		this.EEM.fuel   = EEMs.fuel[this.idx]
		this.EEM.num    = EEMs.num[this.idx]
		this.EEM.label  = EEMs.label[this.idx]
		this.EEM.saving = EEMs.saving[this.idx]
		this.EEM.Rdata  = EEMs.arrays[this.idx]
		
		# 
		# 11. current EEM being processed.
		#
		this.EEM.pdf       <- sub("\\.Rdata",".pdf",                this.EEM.Rdata)
		this.EEM_dummy.pdf <- sub("\\.Rdata","_dummy.pdf",          this.EEM.Rdata)
		this.EEM.clsLAB    <- sub("\\.Rdata","_cluster_Label.csv",  this.EEM.Rdata)
		this.EEM.clsDAT    <- sub("\\.Rdata","_cluster_Data.csv",   this.EEM.Rdata)
		this.EEM.clsSUM    <- sub("\\.Rdata","_cluster_Summary.csv",this.EEM.Rdata)
		this.EEM.weather   <- sub("\\.Rdata","_weather.csv",        this.EEM.Rdata)		
		this.EEM.Obj       <- sub("\\.Rdata","_Processed.Rdata",    this.EEM.Rdata)
		this.EEM.csv       <- sub("\\.Rdata","_fabriacted.csv",     this.EEM.Rdata)
		cat(paste("\n\n11. [",this.CZ,"]-[",this.EEM.name,"]: processing......................\n",sep=""))
		cat(paste("\n\n11. [",this.CZ,"]-[",this.EEM.name,"]: processing......................\n",sep=""),file=FL.LOG,append=TRUE)	

		#
		# 12. define a character string for the plot title
		#
		thisLearn.string <- paste(this.method.4.cluster,sep="")
		cat(paste("12. [",this.CZ,"]-[",this.EEM.name,"]: character string for plot title.\n",sep=""))
		cat(paste("12. [",this.CZ,"]-[",this.EEM.name,"]: character string for plot title.\n",sep=""),file=FL.LOG,append=TRUE)	



		#
		# 13. generate files for each EEMs
		#
		FL.IN.OBJ        <- paste(Path.CZ.IN, this.EEM.Rdata,    sep="/")
		FL.OUT.OBJ       <- paste(Path.CZ.OUT,this.EEM.Rdata,    sep="/")
		FL.OUT.CSV       <- paste(Path.CZ.OUT,this.EEM.csv,      sep="/")
		FL.OUT.PDF       <- paste(Path.CZ.OUT,this.EEM.pdf,      sep="/")
		FL.OUT_Dummy.PDF <- paste(Path.CZ.OUT,this.EEM_dummy.pdf,sep="/")
		FL.OUT.CLSLAB    <- paste(Path.CZ.OUT,this.EEM.clsLAB,   sep="/")
		FL.OUT.CLSDAT    <- paste(Path.CZ.OUT,this.EEM.clsDAT,   sep="/")
		FL.OUT.CLSSUM    <- paste(Path.CZ.OUT,this.EEM.clsSUM,   sep="/")
		FL.weather       <- paste(Path.CZ.OUT,this.EEM.weather,  sep="/")
		FL.PROCESSED.OBJ <- paste(Path.CZ.OUT,this.EEM.Obj,      sep="/")
		if (!(file.exists(FL.IN.OBJ)))     {print(paste(FL.IN.OBJ,"  does exist. Check Why!"));die}
		if  (file.exists(FL.OUT.OBJ))      {print(paste(FL.OUT.OBJ,      "exist. Delete it!"));file.remove(FL.OUT.OBJ)}		
		if  (file.exists(FL.OUT.CSV))      {print(paste(FL.OUT.CSV,      "exist. Delete it!"));file.remove(FL.OUT.CSV)}
		if  (file.exists(FL.OUT.PDF))      {print(paste(FL.OUT.PDF,      "exist. Delete it!"));file.remove(FL.OUT.PDF)}		
		if  (file.exists(FL.OUT_Dummy.PDF)){print(paste(FL.OUT_Dummy.PDF,"exist. Delete it!"));file.remove(FL.OUT_Dummy.PDF)}		
		if  (file.exists(FL.OUT.CLSLAB))   {print(paste(FL.OUT.CLSLAB,   "exist. Delete it!"));file.remove(FL.OUT.CLSLAB)}			
		if  (file.exists(FL.OUT.CLSDAT))   {print(paste(FL.OUT.CLSDAT,   "exist. Delete it!"));file.remove(FL.OUT.CLSDAT)}	
		if  (file.exists(FL.OUT.CLSSUM))   {print(paste(FL.OUT.CLSSUM,   "exist. Delete it!"));file.remove(FL.OUT.CLSSUM)}	
		if  (file.exists(FL.weather))      {print(paste(FL.weather,      "exist. Delete it!"));file.remove(FL.weather)}
		if  (file.exists(FL.PROCESSED.OBJ)){print(paste(FL.PROCESSED.OBJ,"exist. Delete it!"));file.remove(FL.PROCESSED.OBJ)}
		cat(paste("13. [",this.CZ,"]-[",this.EEM.name,"]: prepare files for output.\n",sep=""))
		cat(paste("13. [",this.CZ,"]-[",this.EEM.name,"]: prepare files for output.\n",sep=""),file=FL.LOG,append=TRUE)	



		# ---------------------------------------------------------------------------------
		# 14. load the data which contains 
		#					[myData.base]: differ from [myData.advn] only on 4 fields (i.e., -c(19,20,22,23) === "EEM","EnergyGJ","EEM.idx","EEM.name")
		#					[myData.advn]
		#					[myData.merged.long]
		#					[myData.4.weeklyLong]
		#					[myData.4.dailyLong]
		# April 4, 2015: since we are going to fabricate a data set by using three weeks of EEM and one week of Baseline based on [myData.base"] and [myDaat.advn]
		#                [myData.merged.long], [myData.4.weeklyLong], [myData.4.dailyLong] will need to be deleted
		# ---------------------------------------------------------------------------------
		load(FL.IN.OBJ)
		cat(paste("14. [",this.CZ,"]-[",this.EEM.name,"]: load data from [",FL.IN.OBJ,"].\n",sep=""))
		cat(paste("14. [",this.CZ,"]-[",this.EEM.name,"]: load data from [",FL.IN.OBJ,"].\n",sep=""),file=FL.LOG,append=TRUE)

		#
		# add "month.lab" to [myData.base] and [myData.advn]
		#
		myData.base[,"month.lab"] <- factor(myData.base[,"month"],levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
		myData.advn[,"month.lab"] <- factor(myData.advn[,"month"],levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
		cat(paste("14A. [",this.CZ,"]-[",this.EEM.name,"]: added [month.lab] to [myData.base] and [myData.advn].\n",sep=""))
		cat(paste("14A. [",this.CZ,"]-[",this.EEM.name,"]: added [month.lab] to [myData.base] and [myData.advn].\n",sep=""),file=FL.LOG,append=TRUE)
		
		#
		# delete the [myData.merged.long], [myData.4.weeklyLong] and [myData.4.dailyLong] which conists of two years of data
		#
		rm(myData.merged.long,myData.4.weeklyLong,myData.4.dailyLong)
		cat(paste("14B. [",this.CZ,"]-[",this.EEM.name,"]: delete [myData.merged.long], [myData.4.weeklyLong], [myData.4.dailyLong].\n",sep=""))
		cat(paste("14B. [",this.CZ,"]-[",this.EEM.name,"]: delete [myData.merged.long], [myData.4.weeklyLong], [myData.4.dailyLong].\n",sep=""),file=FL.LOG,append=TRUE)
		
		
		
		# **************************************************************************************************************************************************************************
		# re-construct a data set by using three weeks of data from EEM and one week of data from baseline (i.e., every four weeks has three weeks of EEM and one week of baseline)
		# week.idx (4,8,12,16,20,24,28,32,36,40,44,48,52) from baseline
		# **************************************************************************************************************************************************************************
		myData.Fake <- myData.advn				# initialize [myData.Fake] with the EEM data
		myData.Fake[myData.Fake[,"week.idx"] %in% c(4,8,12,16,20,24,28,32,36,40,44,48,52),] <- myData.base[myData.base[,"week.idx"] %in% c(4,8,12,16,20,24,28,32,36,40,44,48,52),]
		cat(paste("14C. [",this.CZ,"]-[",this.EEM.name,"]: construct [myData.Fake] by using three week's of EEM data and one week of baseline data consecutively in each month\n",sep=""))
		cat(paste("14C. [",this.CZ,"]-[",this.EEM.name,"]: construct [myData.Fake] by using three week's of EEM data and one week of baseline data consecutively in each month\n.\n",sep=""),file=FL.LOG,append=TRUE)
		
		# ********************************************
		# ********************************************
		# ********************************************
		# ********************************************
		# ********************************************
		# not necessary but did this for minizing scriipt change
		# ********************************************
		# ********************************************
		# ********************************************
		# ********************************************
		myData.merged.long  <- myData.Fake
		myData.4.weeklyLong <- myData.Fake
		myData.4.dailyLong  <- myData.Fake
		

		#
		# 15. output: immediately save out
		#     April 3, 2015: although we only have [myData.Fake] and all other data frames are identical to this one, to minimize the script change, I still duplicate them!!!!
		#
		save(myData.base,myData.advn,myData.Fake,file=FL.OUT.OBJ)
		cat(paste("[",this.CZ,"]-[",this.EEM.name,"]: fabricated data,",sep=""),file=FL.OUT.CSV,append=TRUE)
		write.table(myData.Fake,file=FL.OUT.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
		cat(paste("15. [",this.CZ,"]-[",this.EEM.name,"]: save the fabricated data out into [",FL.OUT.OBJ,"] and [",FL.OUT.CSV,"].\n",sep=""))
		cat(paste("15. [",this.CZ,"]-[",this.EEM.name,"]: save the fabricated data out into [",FL.OUT.OBJ,"] and [",FL.OUT.CSV,"].\n",sep=""),file=FL.LOG,append=TRUE)


		# ----------------------------------------------------------------------------------------
		# 16. open the pdf file
		# ----------------------------------------------------------------------------------------
		pdf(file = FL.OUT.PDF,      paper="special", width=17, height=11,bg = "transparent")			# dev.set(2) goes to what we want
		pdf(file = FL.OUT_Dummy.PDF,paper="special", width=17, height=11,bg = "transparent")		# dev.set(3) goes to dummy
		cat(paste("16. [",this.CZ,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""))
		cat(paste("16. [",this.CZ,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""),file=FL.LOG,append=TRUE)	


		
		# ----------------------------------------------------------------------------------------------
		# 17. compare the weather data between EplusOut and the epw read in
		# ----------------------------------------------------------------------------------------------
		# T dryBulb from E+ output file
		myTdryBulb.eplus <- myData.Fake
		
		# T dryBulb from the epw weather file
		myTdryBulb.epw   <- myTdryBulb.eplus
		myTdryBulb.epw[,"T.dryBulb"] <- myEPW.thisCZ[,"epw.T.drybulb"]	# replace with the reading T dryBulb from epw weather file
		
		# the difference between E+ and EPW
		myTdryBulb.diff  <- myTdryBulb.eplus
		myTdryBulb.diff[,"T.dryBulb"] <- myTdryBulb.eplus[,"T.dryBulb"] - myTdryBulb.epw[,"T.dryBulb"]
		
		# percentage difference between Eplus out and EPW input 
		perc.diff <- 100*(myTdryBulb.diff[,"T.dryBulb"] / myTdryBulb.epw[,"T.dryBulb"])
		
		# add an "ID" to them
		myTdryBulb.eplus[,"ID"] <- "eplus"
		myTdryBulb.epw[,"ID"]   <- "epw"
		myTdryBulb.diff[,"ID"]  <- "diff (eplus-epw)"
		
		myTdryBulb.wide <- cbind(myTdryBulb.eplus,myTdryBulb.eplus[,"T.dryBulb"],myTdryBulb.epw[,"T.dryBulb"],myTdryBulb.diff[,"T.dryBulb"],perc.diff)
          names(myTdryBulb.wide)<- c(names(myTdryBulb.eplus),"T.eplus","T.epw","T.diff","T.diff%")		
		
		# long format for plotting
		myTdryBulb.long <- rbind(myTdryBulb.eplus,myTdryBulb.epw,myTdryBulb.diff)

		# plotting
		plot.TdryBulb <- qplot(data=myTdryBulb.long,x=hour,y=T.dryBulb,group=ID,color=ID,facets=month.lab~day,geom="line")  
		plot.TdryBulb <- plot.TdryBulb + theme(legend.position="top") 
		plot.TdryBulb <- plot.TdryBulb + labs(title=paste(paste("EPW vs EPLUS: in Each Month of (",this.CZ,")",sep="")))
		
		plot.TdryBulb <- plot.TdryBulb + geom_hline(aes(yintercept = 0),linetype=14,colour="red")
	
		# actual plotting
		dev.set(3)
		plot(plot.TdryBulb)
		
		p.wk.TdryBulb <- qplot(data=myTdryBulb.long,x=hour.in.week,y=T.dryBulb,group=ID,color=ID,facets=~week.idx,geom="line") 
		p.wk.TdryBulb <- p.wk.TdryBulb + theme(legend.position="top") 
		p.wk.TdryBulb <- p.wk.TdryBulb + labs(title=paste(paste("EPW vs EPLUS: in Each Month of (",this.CZ,")",sep="")))
		
		p.wk.TdryBulb <- p.wk.TdryBulb + geom_hline(aes(yintercept = 0),linetype=14,colour="red")
		dev.set(3)
		plot(p.wk.TdryBulb)
		cat(paste("17. [",this.CZ,"]-[",this.EEM.name,"]: plot the dry bulb T data.\n",sep=""))
		cat(paste("17. [",this.CZ,"]-[",this.EEM.name,"]: plot the dry bulb T data.\n",sep=""),file=FL.LOG,append=TRUE)	


		# --------------------------------------------------------------------------------
		# 18. make epw weather data ready for cluster results plotting [myWeather.wide]
		# --------------------------------------------------------------------------------
		myWeather.wide <- cbind(myTdryBulb.wide,myEPW.thisCZ[,field.used.short])
		cat(paste("18A. [",this.CZ,"]-[",this.EEM.name,"]: prepare [myWeather.wide] which consists of the weather parameter in the epw file.\n",sep=""))
		cat(paste("18A. [",this.CZ,"]-[",this.EEM.name,"]: prepare [myWeather.wide] which consists of the weather parameter in the epw file.\n",sep=""),file=FL.LOG,append=TRUE)	
		
		
		# output the weather file in csv for further checking
		cat(",",file=FL.weather,append=TRUE)
		write.table(myTdryBulb.wide,file=FL.weather,sep=",",row.names=TRUE,col.names=TRUE)
		cat(paste("18B. [",this.CZ,"]-[",this.EEM.name,"]: Output [myTdryBulb.wide] consists of T drybuld from epw and from E+ out and their differences are checked.\n",sep=""))
		cat(paste("18B. [",this.CZ,"]-[",this.EEM.name,"]: Output [myTdryBulb.wide] consists of T drybuld from epw and from E+ out and their differences are checked.\n",sep=""),file=FL.LOG,append=TRUE)	
	
	
	
	
		# --------------------------------------------------------------
		# 19. manually specify a scaling factor in order to use ggplot2 to plot T as well.
		# --------------------------------------------------------------			
		scaling.factor <- (max(myData.Fake[,c("EnergyGJ")]) / max(myData.Fake[,c("T.dryBulb")])) * 2
		cat(paste("19A. [",this.CZ,"]-[",this.EEM.name,"]: manually specifying a scaling factor in order to plot Energy and Weather T drybulb in the same plots.\n",sep=""))
		cat(paste("19A. [",this.CZ,"]-[",this.EEM.name,"]: manually specifying a scaling factor in order to plot Energy and Weather T drybulb in the same plots.\n",sep=""),file=FL.LOG,append=TRUE)	

		# 
		# the following code may not be used at all
		#
		myTmp1 <- myData.Fake[,c("Date.Time","T.dryBulb","EEM")]			
		myTmp1[,"T.dryBulb"] <- myTmp1[,"T.dryBulb"] * scaling.factor
		myTmp1[,"ID"] <- "T.dryBulb (scaled)"
		names(myTmp1) <- sub("T.dryBulb","value",names(myTmp1))


		myTmp2 <- myData.Fake[,c("Date.Time","EnergyGJ","EEM")]
		myTmp2[,"ID"] <- "EnergyGJ"
		names(myTmp2) <- sub("EnergyGJ","value",names(myTmp2))

		myTmp3 <- rbind(myTmp1,myTmp2)
		myTmp3[,"Variable"] <- paste(myTmp3[,"EEM"],myTmp3[,"ID"],sep="_")
		cat(paste("19B. [",this.CZ,"]-[",this.EEM.name,"]: do not remember what they are for.\n",sep=""))
		cat(paste("19B. [",this.CZ,"]-[",this.EEM.name,"]: do not remember what they are for.\n",sep=""),file=FL.LOG,append=TRUE)	




		# ----------------------------------------------------------------------------------------
		# PLOTTING RAW DATA ..................
		# ----------------------------------------------------------------------------------------
		# --------------------------------------------------------------
		# 20A. plot weekly plot in each month
		# --------------------------------------------------------------
		p.weekly1 <- qplot(data=myData.Fake,x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM,facets=month.lab~week.idx.in.month,geom="line") 
		p.weekly1 <- p.weekly1 + theme(legend.position="top") 
		p.weekly1 <- p.weekly1 + labs(title=paste(paste("Raw Data: ",paste(this.EEM.fuel,this.EEM.num,this.EEM.label,sep="_"),sep="")," (",this.EEM.saving,") Weekly Profile of Weeks in Each Month (",this.CZ,")",sep=""))	
		dev.set(2)
		plot(p.weekly1)
		cat(paste("20A. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly profile for each month.\n",sep=""))
		cat(paste("20A. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly profile for each month.\n",sep=""),file=FL.LOG,append=TRUE)	


		# --------------------------------------------------------------
		# 20B. plot weekly plot in conseccutive weeks
		# --------------------------------------------------------------
		p.weekly2 <- qplot(data=myData.Fake,x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM,facets=~week.idx,geom="line") 
		p.weekly2 <- p.weekly2 + theme(legend.position="top") 
		p.weekly2 <- p.weekly2 + labs(title=paste(paste("Raw Data: ",paste(this.EEM.fuel,this.EEM.num,this.EEM.label,sep="_"),sep="")," (",this.EEM.saving,") Weekly Profile of All Weeks in the Year (",this.CZ,")",sep=""))		
		dev.set(2)
		plot(p.weekly2)
		cat(paste("20B. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly plot in conseccutive weeks.\n",sep=""))
		cat(paste("20B. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly plot in conseccutive weeks.\n",sep=""),file=FL.LOG,append=TRUE)	


		# --------------------------------------------------------------
		# 20C. plot daily plot in each month
		# --------------------------------------------------------------
		p.daily1 <- qplot(data=myData.Fake,x=hour,y=EnergyGJ,group=EEM,color=EEM,facets=month.lab~day,geom="line")  
		p.daily1 <- p.daily1 + theme(legend.position="top") 
		p.daily1 <- p.daily1 + labs(title=paste(paste("Raw Data: ",paste(this.EEM.fuel,this.EEM.num,this.EEM.label,sep="_"),sep="")," (",this.EEM.saving,") Daily Profile of All Days in Each Month (",this.CZ,")",sep=""))	
		dev.set(2)
		plot(p.daily1)
		cat(paste("20C. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month\n",sep=""))
		cat(paste("20C. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month.\n",sep=""),file=FL.LOG,append=TRUE)	



		# ----------------------------------------------------------------------------------------
		# DATA MINING ..................
		# ----------------------------------------------------------------------------------------

 		# ---------------------------------------------------------------------------------
 		# 21. to avoid worngly taking existing data frame,
  		# ---------------------------------------------------------------------------------
 		df_name <- "class.days";	if (exists(df_name) && is.data.frame(get(df_name))){rm("class.days")}
 		df_name <- "myData.Work.long";	if (exists(df_name) && is.data.frame(get(df_name))){rm("myData.Work.long")}
 		df_name <- "myData.Work.wide";	if (exists(df_name) && is.data.frame(get(df_name))){rm("myData.Work.wide")}
		cat(paste("21. [",this.CZ,"]-[",this.EEM.name,"]: to avoid worngly taking existing data frame, delete them first.\n",sep=""))
		cat(paste("21. [",this.CZ,"]-[",this.EEM.name,"]: to avoid worngly taking existing data frame, delete them first..\n",sep=""),file=FL.LOG,append=TRUE)	
 
 		
 		# for (this.subset in c("artificial1","artificial2","artificial3","July","July-August","WeekDay","All Data","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Holiday"))
 		# for (this.subset in c("artificial1","artificial2","artificial3","July","July-August","July-Weekday","July-August-Weekday","WeekDay","Sunday","Saturday","Holiday"))
 		  count.subset <- 0
 		  for (this.subset in c("all","WeekDay","July","July-August"))
 		{
			idx.name          <- "all"
			idx.name.selected <- c("Hartigan","Duda","PseudoT2","Beale","TraceW")
			if (this.subset == "all")
			{
				myData.Work.long <- subset(myData.Fake,   								select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))
			
				min.nc <- 2
				max.nc <- 20
			}else if (this.subset == "July")
			{
				myData.Work.long <- subset(myData.Fake,subset = month %in% c(7),            				select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))	

				min.nc <- 2
				max.nc <- 20
				if (this.CZ == "SanFrancisco"){max.nc <- 4}
				if (this.CZ == "Atlanta"){max.nc <- 7}
				if (this.CZ == "Phoenix"){max.nc <- 10}
				if (this.CZ == "Seattle"){max.nc <- 5}
				if (this.CZ == "Fairbanks"){max.nc <- 7}
				if (this.CZ == "Miami"){max.nc <- 10;idx.name <- "hartigan";idx.name.selected <- "Hartigan"}
			}else if (this.subset == "July-August")
			{
				myData.Work.long <- subset(myData.Fake,subset = month %in% c(7,8),          				select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))

				min.nc <- 2
				max.nc <- 20			
			}else if (this.subset == "WeekDay")
			{
				myData.Work.long <- subset(myData.Fake,subset=(day.type.lab == "Weekday"),				select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))			

				min.nc <- 2
				max.nc <- 20
			}else if (this.subset %in% c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Holiday"))
			{
				myData.Work.long <- subset(myData.Fake,subset=(day.week.lab == this.subset),				select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))			
				min.nc <- 2
				max.nc <- 20
			}else if (this.subset == "All Data")
			{
				myData.Work.long <- subset(myData.Fake,                                     				select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))	
				min.nc <- 2
				max.nc <- 20
			}
			cat(paste("\n\n22. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: [myData.Work.long] is a subset of [myData.Fake] used for data mining.\n",sep=""))
			cat(paste("\n\n22. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: [myData.Work.long] is a subset of [myData.Fake] used for data mining.\n",sep=""),file=FL.LOG,append=TRUE)	


			# seems like the following are not used.  turn the long format to wide format
			var.intact   <- c("date","hour")		# fields will not be changed
			var.expand   <- c("EEM")			# fields will be used to expand
			var.EnergyGJ <- c("EnergyGJ.norm")		# fields of the measurement
			var.TdryBulb <- c("T.dryBulb.norm")		# fields of the measurement

			# -----------------------------------------------------------------
			# 25. normalize the energy consumption data in [myData.Work.long]
			# -----------------------------------------------------------------
			myData.Work.long[,"EnergyGJ.norm"]  <- (myData.Work.long[,"EnergyGJ"]  - min(myData.Work.long[,"EnergyGJ"]))  / (max(myData.Work.long[,"EnergyGJ"])  - min(myData.Work.long[,"EnergyGJ"]))
			myData.Work.long[,"T.dryBulb.norm"] <- (myData.Work.long[,"T.dryBulb"] - min(myData.Work.long[,"T.dryBulb"])) / (max(myData.Work.long[,"T.dryBulb"]) - min(myData.Work.long[,"T.dryBulb"]))
			cat(paste("25. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: normalize the data in [myData.Work.long].\n",sep=""))
			cat(paste("25. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: normalize the data in [myData.Work.long]..\n",sep=""),file=FL.LOG,append=TRUE)	

			# -----------------------------------------------------------------
			# 26. [myData.Work.wide] for data mining the daily profiles
			# -----------------------------------------------------------------
			myData.Work.wide.EnergyGJ <- dcast(myData.Work.long,date + EEM ~ hour,value.var = var.EnergyGJ)
			myData.Work.wide.TdryBulb <- dcast(myData.Work.long,date + EEM ~ hour,value.var = var.TdryBulb)
			cat(paste("26. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: turn the long format to a wide format having 24 hour a day.\n",sep=""))
			cat(paste("26. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: turn the long format to a wide format having 24 hour a day.\n",sep=""),file=FL.LOG,append=TRUE)	


			# 27. use [date] as row names
			row.names(myData.Work.wide.EnergyGJ) <- as.Date(myData.Work.wide.EnergyGJ[,"date"],"%m/%d/%y")
			row.names(myData.Work.wide.TdryBulb) <- as.Date(myData.Work.wide.TdryBulb[,"date"],"%m/%d/%y")
			cat(paste("27. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use date as row names.\n",sep=""))
			cat(paste("27. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use date as row names.\n",sep=""),file=FL.LOG,append=TRUE)	

			# ---------------------------------------------------------------------------------
			# 28. use "GJ.h" for fields name in [myData.Work.wide.EnergyGJ]
			# ---------------------------------------------------------------------------------
			field1 <- c("date","EEM")
			field2 <- names(myData.Work.wide.EnergyGJ)[!(names(myData.Work.wide.EnergyGJ) %in% field1)]
			myData.GJ.part1  <- myData.Work.wide.EnergyGJ[,field1]
			myData.GJ.part2  <- myData.Work.wide.EnergyGJ[,field2]
			myData.Work.wide.EnergyGJ  <- cbind(myData.GJ.part1,myData.GJ.part2)
		  names(myData.Work.wide.EnergyGJ) <- c(field1,paste("GJ.h",field2,sep=""))
			cat(paste("28. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: a re-organization on the GJ data.\n",sep=""))
			cat(paste("28. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: a re-organization.\n",sep=""),file=FL.LOG,append=TRUE)	

			# ---------------------------------------------------------------------------------
			# 29. use "T.h" for fields name in [myData.Work.wide.TdryBulb]
			# ---------------------------------------------------------------------------------
			field1 <- c("date","EEM")
			field2 <- names(myData.Work.wide.TdryBulb)[!(names(myData.Work.wide.TdryBulb) %in% field1)]
			myData.T.part1  <- myData.Work.wide.TdryBulb[,field1]
			myData.T.part2  <- myData.Work.wide.TdryBulb[,field2]
			myData.Work.wide.TdryBulb  <- cbind(myData.T.part1,myData.T.part2)
		  names(myData.Work.wide.TdryBulb) <- c(field1,paste("T.h",field2,sep=""))
			cat(paste("29. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: a re-organization.\n",sep=""))
			cat(paste("29. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: a re-organization.\n",sep=""),file=FL.LOG,append=TRUE)	
		  

			# ---------------------------------------------------------------------------------
			# 30. decide if we want to merge GJ and T of the 24 hours OR
			#                       just use GJ data
			# ---------------------------------------------------------------------------------
			# myData.Work.wide <- merge(myData.Work.wide.EnergyGJ,myData.Work.wide.TdryBulb)
			  myData.Work.wide <- myData.Work.wide.EnergyGJ
			cat(paste("30. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use the date as row name for the data frame.\n",sep=""))
			cat(paste("30. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use the date as row name for the data frame.\n",sep=""),file=FL.LOG,append=TRUE)	

			# -----------------------------------------------------------------
			# 31. [myData.Work.long] and [myData.Work.wide] are ready
			# -----------------------------------------------------------------
			myData.Work.long[,"date"] <- as.Date(myData.Work.long[,"date"],"%m/%d/%y")
			myData.Work.wide[,"date"] <- as.Date(myData.Work.wide[,"date"],"%m/%d/%y")
			cat(paste("31. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: have the [date] field in [myData.Work.long] and [myData.Work.wide].\n",sep=""))
			cat(paste("31. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: have the [date] field in [myData.Work.long] and [myData.Work.wide].\n",sep=""),file=FL.LOG,append=TRUE)	


			# 32. create a mapping table between [date] and [day.type.lab] and [day.weel.lab]
			myData.dayMapping  <- myData.Work.long[myData.Work.long[,"hour"] == 0,c("EEM","date","day.week.lab","day.type.lab","week.idx","date")]		
	      row.names(myData.dayMapping) <- myData.dayMapping[,"date"]
			cat(paste("32. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use the date as row name for the data frame.\n",sep=""))
			cat(paste("32. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use the date as row name for the data frame.\n",sep=""),file=FL.LOG,append=TRUE)	


			# -----------------------------------------------------------------
			# 33. [myData.Work] is the one used for data mining, consist of only variables 
			# -----------------------------------------------------------------			
			myDate      <- myData.Work.wide[,c(1,2)]
			myData.Work <- myData.Work.wide[,-c(1,2)]
			names(myData.Work) <- paste("hour",names(myData.Work),sep="")
			cat(paste("33. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: only keep the data for Data Mining.\n",sep=""))
			cat(paste("33. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: only keep the data for Data Mining.\n",sep=""),file=FL.LOG,append=TRUE)	


			# -----------------------------------------------------------------
			# 41. MClust 
			# -----------------------------------------------------------------
			no.object   <- dim(myData.Work)[1]
			no.variable <- dim(myData.Work)[2]
			if (no.object >= no.variable)
			{ 
				# Use NbClust to determine the best number of clusters
				# the distance measure to be used to compute the dissimilarity matrix. This must be one of: "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski" or "NULL". By default, distance="euclidean".
				# the cluster analysis method to be used the cluster analysis method to be used. This should be one of: "ward.D","ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid","kmeans".
				dev.set(3)	# sent the plot to screen no to the file
				NbClust.model <- NbClust(myData.Work, diss=NULL,distance="euclidean",min.nc = min.nc,max.nc = max.nc,method=this.method.4.NbClust,index=idx.name)
				cat(paste("34A. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: Using NbClust to determine the numbe rof clusters.\n",sep=""))
				cat(paste("34A. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: Using NbClust to determine the numbe rof clusters.\n",sep=""),file=FL.LOG,append=TRUE)	

				if (length(grep("artificial",this.subset)))
				{
					no.cls.best <- which.max(table(NbClust.model$Best.nc["Number_clusters",]))
				}else{
					count.subset <- count.subset + 1
					arrays.clusters     <- data.frame(NbClust.model$Best.nc)
					if(length(idx.name.selected) == 1)
					{
						arrays.clusters.sub <- arrays.clusters
					}else{
						arrays.clusters.sub <- arrays.clusters[,idx.name.selected]
					}
					
				      # no.cls.best <- which.max(table(NbClust.model$Best.nc["Number_clusters",]))
					no.cls.best <- max(arrays.clusters.sub["Number_clusters",],na.rm=TRUE)					
					
					# put the cluster number information into an data frame
					tmp.df <- data.frame(t(arrays.clusters))
					names(tmp.df) <- c(paste("Number_Clusters(",this.subset,")",sep=""),paste("Value_Index(",this.subset,")",sep=""))
					tmp.df[,"Index"] <- row.names(tmp.df)
					if (count.subset == 1)
					{
						myCluster.summary <- tmp.df

					}else{
						myCluster.summary <- merge(myCluster.summary,tmp.df)
					}					
				}
				# if(this.CZ == "SanFrancisco" & this.subset == "WeekDay" & this.idx == 1){no.cls.best <- 4}
				# if(this.CZ == "Atlanta"      & this.subset == "all"     & this.idx == 1){no.cls.best <- 13}
				cat(paste("34B. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: Using NbClust to determine the numbe rof clusters.\n",sep=""))
				cat(paste("34B. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: Using NbClust to determine the numbe rof clusters.\n",sep=""),file=FL.LOG,append=TRUE)	

				#
				# Mclust Modeling
				#
				d_clust <- Mclust(as.matrix(myData.Work), G=no.cls.best)
				cls.Work <- d_clust$classification
				cat(paste("34C. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: PAM modeling using the number of cluster selected by NbClust.\n",sep=""))
				cat(paste("34C. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: PAM modeling using the number of cluster selected by NbClust.\n",sep=""),file=FL.LOG,append=TRUE)	

				myResults.Work <- cbind(myDate,myData.Work)
				cat(paste("36. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: combine clustering results with data.\n",sep=""))
				cat(paste("36. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: combine clustering results with data.\n",sep=""),file=FL.LOG,append=TRUE)	

				# assign cluster label
				myResults.Work[,"cls.Work"] <- cls.Work
				cat(paste("37. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: combine clustering results with data.\n",sep=""))
				cat(paste("37. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: combine clustering results with data.\n",sep=""),file=FL.LOG,append=TRUE)	

				#
				# no.cls.best <- max(myResults.Work[,"cls.Work"])
				thisLearn.string <- paste(this.method.4.cluster,": ",no.cls.best," clusters",sep="")
				cat(paste("41. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: add [cls.Work].\n",sep=""))
				cat(paste("41. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: add [cls.Work].\n",sep=""),file=FL.LOG,append=TRUE)	


				# ---------------------------------------------------------------------------------
				# 42. create [class.days] data frame to record the date, type of the day,EEM and class label
				# ---------------------------------------------------------------------------------
				class.days <- myResults.Work[,c("date","cls.Work")]
				row.names(class.days) <- class.days[,"date"]

				# add "day.type" and "day.week" into [days.class]
				class.days[,"day.type.lab"] <- NA
				class.days[,"day.week.lab"] <- NA
				class.days[,"EEM"]          <- NA

				class.days[,"day.type.lab"] <- myData.dayMapping[as.character(class.days[,"date"]),"day.type.lab"]
				class.days[,"day.week.lab"] <- myData.dayMapping[as.character(class.days[,"date"]),"day.week.lab"]
				class.days[,"EEM"]          <- myData.dayMapping[as.character(class.days[,"date"]),"EEM"]
				cls.levels                  <- paste("cls",sort(unique(class.days[,"cls.Work"])),sep="-")
				class.days[,"cluster"]      <- factor(paste("cls",class.days[,"cls.Work"],sep="-"),levels = cls.levels,labels = cls.levels,ordered = TRUE)
				cat(paste("clustering results on [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"],",sep=""),file=FL.OUT.CLSLAB,append=TRUE)
				write.table(class.days,file=FL.OUT.CLSLAB,sep=",",row.names=TRUE,col.names=TRUE,appen=TRUE)
				cat(paste("42. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: associate dates and class label.\n",sep=""))
				cat(paste("42. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: associate dates and class label.\n",sep=""),file=FL.LOG,append=TRUE)	


				# ---------------------------------------------------------------------------------
				# 43. add a class label to [myData.Work.wide] and [myData.Work.long]
				# ---------------------------------------------------------------------------------
				myData.Work.long[,"cls.Work"] <- -999
				myData.Work.long[,"cls.Work"] <- class.days[as.character(myData.Work.long[,"date"]),"cls.Work"]

				myData.Work.wide[,"cls.Work"] <- -999
				myData.Work.wide[,"cls.Work"] <- class.days[as.character(myData.Work.wide[,"date"]),"cls.Work"]	

				cls.levels <- paste("cls",sort(unique(myData.Work.long[,"cls.Work"])),sep="-")
				myData.Work.long[,"cluster"]      <- factor(paste("cls",myData.Work.long[,"cls.Work"],sep="-"),levels = cls.levels,labels = cls.levels,ordered = TRUE)
				myData.Work.wide[,"cluster"]      <- factor(paste("cls",myData.Work.wide[,"cls.Work"],sep="-"),levels = cls.levels,labels = cls.levels,ordered = TRUE)

				myData.Work.long[,"month"]        <- as.numeric(sub("(.*)-(.*)-(.*)","\\2",myData.Work.long[,"date"]))
				myData.Work.long[,"day"]          <- as.numeric(sub("(.*)-(.*)-(.*)","\\3",myData.Work.long[,"date"]))
				myData.Work.long[,"month.lab"]    <- factor(myData.Work.long[,"month"],levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)

				cat(paste("\n\nclustering results on [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"],",sep=""),file=FL.OUT.CLSDAT,append=TRUE)
				write.table(myData.Work.long,file=FL.OUT.CLSDAT,sep=",",row.names=TRUE,col.names=TRUE,appen=TRUE)				

				cat(paste("43. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: add [cls.Work] to [myData.Work.wide] and [myData.Work.long].\n",sep=""))
				cat(paste("43. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: add [cls.Work] to [myData.Work.wide] and [myData.Work.long].\n",sep=""),file=FL.LOG,append=TRUE)	





				#
				# tally the distribution of the type of days in each cluster
				#
				myData.4.tally <- myData.Work.long
				myData.4.tally[,"year"] <- sub("(.*)/(.*)/(.*)","\\3",myData.4.tally[,"date"])
				myTally1 <- dcast(as.data.frame(table(myData.4.tally[,c("day.week.lab","cluster","year")])/24),cluster ~ year + day.week.lab)
				myTally2 <- dcast(as.data.frame(table(myData.4.tally[,c("day.type.lab","cluster","year")])/24),cluster ~ year + day.type.lab)				
				cat(paste(no.cls.best,"clusters for [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"],",sep=""),file = FL.OUT.CLSSUM,append=TRUE)
				write.table(cbind(myTally1,myTally2),sep=",",row.names=TRUE,col.names=TRUE,file = FL.OUT.CLSSUM,append=TRUE)
				cat("\n\n",file = FL.OUT.CLSSUM,append=TRUE)

				cat(paste("44. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: tally the day types.\n",sep=""))
				cat(paste("44. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: tally the day types.\n",sep=""),file=FL.LOG,append=TRUE)	
				



				# -----------------------------------------------------------------
				# 50. PLOTTING: profile of the classes
				# -----------------------------------------------------------------

				# ---------------------------------------------------------------------------------------
				# 51. plot1: (p.cluster): plot the profile of the clusters
				# ---------------------------------------------------------------------------------------
				if(length(grep("artificial",this.subset)))
				{
					p.cluster <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date,color=EEM,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}else{

					p.cluster.all <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date,color=cluster,shape=EEM,geom=c("line","point"))                 + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
					p.cluster     <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date,color=EEM,              geom=c("line"),       facets=~cluster)  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}
				cat(paste("51. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters.\n",sep=""))
				cat(paste("51. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters.\n",sep=""),file=FL.LOG,append=TRUE)	

				# ---------------------------------------------------------------------------------------
				# 52. plot2: (p.cluster.dayType): [Cluster] vs [day.type.lab] vs [base|EEM]
				# ---------------------------------------------------------------------------------------
				if(length(grep("artificial",this.subset)))
				{				
					p.cluster.dayType <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date,color=EEM,facets=~day.type.lab,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}else{
					p.cluster.dayType <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date,color=EEM,facets=day.type.lab~cluster,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}
				cat(paste("52. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters in terms of day type.\n",sep=""))
				cat(paste("52. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters in terms of day type.\n",sep=""),file=FL.LOG,append=TRUE)

				# ---------------------------------------------------------------------------------------
				# 53. plot 3: (p.cluster.dayinWeek): [Cluster] vs [day.week.lab] vs [base|EEM]
				# ---------------------------------------------------------------------------------------
				# plot cluster results of all dates in both "base" and "EEM" in terms of [day.week.lab]
				if(length(grep("artificial",this.subset)))
				{				
					p.cluster.dayinWeek <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date,color=EEM,facets=~day.week.lab,       geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}else{
					p.cluster.dayinWeek <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date,color=EEM,facets=day.week.lab~cluster,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}
				cat(paste("53. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters in terms of day in the week.\n",sep=""))
				cat(paste("53. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters in terms of day in the week.\n",sep=""),file=FL.LOG,append=TRUE)


				# ---------------------------------------------------------------------------------------
				# 54. plot 4: plot the daily profiles coloring in clusters
				# ---------------------------------------------------------------------------------------

				p.daily.cls <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=cluster,color=cluster,facets=month.lab~day,geom="line")  
				p.daily.cls <- p.daily.cls + theme(legend.position="top") 
				p.daily.cls <- p.daily.cls + labs(title=paste(paste("Raw Data: ",paste(this.EEM.fuel,this.EEM.num,this.EEM.label,sep="_"),sep="")," (",this.EEM.saving,") Daily Profile of All Days in Each Month (",this.CZ,")",sep=""))	
				cat(paste("54. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile coloring with clusters.\n",sep=""))
				cat(paste("53. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile coloring with clusters.\n",sep=""),file=FL.LOG,append=TRUE)


				# ---------------------------------------------------------------------------------------
				# 55. actual plotting
				# ---------------------------------------------------------------------------------------
				dev.set(2)
				if(length(grep("artificial",this.subset,invert = TRUE)))	# for all non-artificial subset, plot this (all clusters and both baseline and EEM together
				{
					plot(p.cluster.all)
				}
				plot(p.cluster)
								
				if(length(grep("July",this.subset)) | length(grep("July-August",this.subset)) | length(grep("All Data",this.subset)))
				{	
					multiplot(p.cluster.dayType)	# data from complete month or year can be plotted in terms of day in the week or day type
					multiplot(p.cluster.dayinWeek)	# data from complete month or year can be plotted in terms of day in the week or day type
				}else if (length(grep("WeekDay",this.subset)))
				{

					multiplot(p.cluster.dayinWeek)	# data from a day type will only be plotted in terms of day in the week
				}
				
				plot(p.daily.cls)
				cat(paste("55. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: actually generate the plots.\n",sep=""))
				cat(paste("55. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: actually generate the plots.\n",sep=""),file=FL.LOG,append=TRUE)
				
				# ----------------------------------------------------------------------------------------------------
				# ----------------------------------------------------------------------------------------------------
				# ----------------------------------------------------------------------------------------------------
				# plot the distribution of the environmental variables among clusters
				# ----------------------------------------------------------------------------------------------------
				# ----------------------------------------------------------------------------------------------------
				# ----------------------------------------------------------------------------------------------------
				# 1. merging [myWeather.wide] with [myData.Work.long]
				if (length(grep("artificial",this.subset,invert = TRUE)))	# for all non-artifical subsets
				{
					# add "month","day" in [myData.Work.long] which will be used to merge the weather data in [myWeather.wide] which has "month","day","hour"
					myData.Work.long[,"month"] <- as.numeric(sub("(.*)-(.*)-(.*)","\\2",myData.Work.long[,"date"]))
					myData.Work.long[,"day"]   <- as.numeric(sub("(.*)-(.*)-(.*)","\\3",myData.Work.long[,"date"]))
					
					var.cls   <- c("month","day","hour","cluster","EEM")
					var.epw   <- c("month","day","hour",grep("epw.",names(myWeather.wide),value=TRUE))
					
					tmp1 <- myData.Work.long[,var.cls]
					tmp2 <- myWeather.wide[,var.epw]
					
					tmp1[,"ID"] <- paste(tmp1[,"month"],tmp1[,"day"],tmp1[,"hour"],sep="-")
					tmp2[,"ID"] <- paste(tmp2[,"month"],tmp2[,"day"],tmp2[,"hour"],sep="-")	# this "ID" is unique for each row in [myWeather.wide] i.e., in [tmp2]
		              row.names(tmp2) <- tmp2[,"ID"]
		              
		              		# the corresponding part in [tmp2] which matchs the "month","day","hour" in [tmp1]
		              		# those rows in [tmp2] which has the values of (tmp1[,"ID"])
		              		tmp3 <- tmp2[tmp1[,"ID"],]		# the part of the [myWeather.wide] which matches the "month","day","hour" in [myData.Work.long]
		              		
		              		# merge weather in [tmp3] and the class label in [tmp1]				
					myEpw.Cls <- cbind(tmp1,tmp3)
					
					# put in a long format to plot all environmental variables
					var.epw <- grep("epw.",names(myEpw.Cls),value=TRUE)
					var.non.epw <- names(myEpw.Cls)[!(names(myEpw.Cls) %in% var.epw)]
					myEpw.Cls.long <- melt(myEpw.Cls,id.vars = var.non.epw,measure.vars = var.epw,value.name="value")
					
					
					# plot
					count.epw <- 0
					for (this.variable in unique(myEpw.Cls.long[,"variable"]))
					{
						count.epw <- count.epw + 1
						myData.4.plot <- myEpw.Cls.long[myEpw.Cls.long[,"variable"] == this.variable,]
					 	p.weather <- qplot(data=myData.4.plot,x=cluster,y=value,color=cluster,geom="boxplot") + theme(legend.position="top") + labs(title=paste("[(",this.variable,"): [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	 
						
						command.string.epw <- paste(paste("p.epw",count.epw,sep="")," <- p.weather",sep="")
						eval(parse(text=command.string.epw))
					}
					
					if (count.epw == 1){plot(p.epw1)}
					if (count.epw == 2){multiplot(p.epw1,p.epw2,cols=2)}
					if (count.epw == 3){multiplot(p.epw1,p.epw2,p.epw3,cols=3)}
					if (count.epw == 4){multiplot(p.epw1,p.epw2,p.epw3,p.epw4,cols=2)}
					if (count.epw == 5){multiplot(p.epw1,p.epw2,p.epw3,p.epw4,p.epw5,cols=3)}
					if (count.epw == 6){multiplot(p.epw1,p.epw2,p.epw3,p.epw4,p.epw5,p.epw6,cols=3)}					
					if (count.epw == 7){multiplot(p.epw1,p.epw2,p.epw3,p.epw4,p.epw5,p.epw6,p.epw7,cols=3)}
					if (count.epw == 8){multiplot(p.epw1,p.epw2,p.epw3,p.epw4,p.epw5,p.epw6,p.epw7,p.epw8,cols=3)}
					if (count.epw == 9){multiplot(p.epw1,p.epw2,p.epw3,p.epw4,p.epw5,p.epw6,p.epw7,p.epw8,p.epw9,cols=3)}	
				}
				# ----------------------------------------------------------------------------------------------------
				# ----------------------------------------------------------------------------------------------------
				
			}	# end of decision if number of objects is larger than the number of variables
		}		# end of subset of current EEM
		

		cat(paste("all index: [",this.CZ,"]-[",this.EEM.name,"],",sep=""),file=FL.OUT.CLSSUM,append=TRUE)
		write.table(myCluster.summary,sep=",",row.names=TRUE,col.name=TRUE,file=FL.OUT.CLSSUM,append=TRUE)
		cat(paste("60. [",this.CZ,"]-[",this.EEM.name,"]: output the cluster determination information.\n",sep=""))
		cat(paste("60. [",this.CZ,"]-[",this.EEM.name,"]: output the cluster determination information.\n",sep=""),file=FL.LOG,append=TRUE)	


	
		dev.off(2)
		dev.off(3)
	}			# end of current EEM
	cat(paste("80. [",this.CZ,"]: finished the processing of all EEMs.\n\n\n",sep=""))
	cat(paste("80. [",this.CZ,"]: finished the processing of all EEMs.\n\n\n",sep=""),file=FL.LOG,append=TRUE)	
}				# end of current CZ

cat(paste("\n\n90. Completed the processing the data of all CZs.\n",sep=""))
cat(paste("\n\n90. Completed the processing the data of all CZs.\n",sep=""),file=FL.LOG,append=TRUE)	


# -------------------------------------------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n07_DataMining_LargeOffice_Mclust_with_NbClust_kmeans.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n07_DataMining_LargeOffice_Mclust_with_NbClust_kmeans.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)


cat(paste("\nProcessing time for [07_DataMining_LargeOffice_Mclust_with_NbClust_kmeans.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [07_DataMining_LargeOffice_Mclust_with_NbClust_kmeans.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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
