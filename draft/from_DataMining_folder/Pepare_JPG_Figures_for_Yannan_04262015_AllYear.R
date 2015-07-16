#
# Pepare_JPG_Figures_for_Yannan_04262015_AllYear.R 
#
# April 27, 2015: revise this for plotting Yannan's data.  taking the day label from Yannan and plot the time series: only for SanFrancisco Electricity
#
# April 21, 2015: add a "Janitor" label which has "A', "B" two values corresponding to "baseline" and "EEM"
# April 20, 2015: Team meeting requested (1) create a boxplot on the Janitor Shifting (2) Plots of the traninbg data results from classification.
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
library("rpart")				# Classification and Regression Tree Packge
library("e1071")				# SVM package
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(randomForest)				# Random Forest
# library(party)				# Alternative decision tree algorithm
# library(partykit)				# Convert rpart object to BinaryTree


col.array <- c("red","blue","green","magenta","cyan","purple","brown","black")

# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

this.method.4.cluster <- "PAM"
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

Path.IN  <- paste(Path.Sim,"02_Plotting_LargeOffice",                          sep="/")
Path.OUT <- paste(Path.Sim,"Pepare_JPG_Figures_for_Yannan_04262015_AllYear",sep="/")
if (!file.exists(Path.IN)){print(paste(Path.IN," does not exist. Check why!",sep=""));die}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}

FL.LOG <- paste(Path.OUT,"Pepare_JPG_Figures_for_Yannan_04262015_AllYear.log",sep="/")
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

for (this.random in c("random1","random2")[1])
{
	Path.Random <- paste(Path.OUT,this.random,sep="/")
	if (!file.exists(Path.Random)){print(paste("NOT existing:",Path.Random));dir.create(Path.Random,showWarnings=TRUE,recursive=TRUE)}

	
	
	for (this.CZ in CZ.arrays[c(7)])	#for (this.CZ in CZ.arrays)  c("SanFrancisco","Albuquerque")  c(7,9,4,8,3,2,5,6,10,11,12,13,14,15,16,1)
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
		Path.CZ.OUT <- paste(Path.Random,this.CZ,sep="/")
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
		  for (this.idx in seq(1,length(EEMs.name))[EEM.selected][1])
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
			this.EEM.string     <- sub("\\.Rdata","",                    this.EEM.Rdata)
			this.EEM.pdf        <- sub("\\.Rdata",".pdf",                this.EEM.Rdata)
			this.EEM_dummy.pdf  <- sub("\\.Rdata","_dummy.pdf",          this.EEM.Rdata)
			this.EEM.clsLAB     <- sub("\\.Rdata","_cluster_Label.csv",  this.EEM.Rdata)
			this.EEM.clsDAT     <- sub("\\.Rdata","_cluster_Data.csv",   this.EEM.Rdata)
			this.EEM.clsSUM     <- sub("\\.Rdata","_cluster_Summary.csv",this.EEM.Rdata)
			this.EEM.weather    <- sub("\\.Rdata","_weather.csv",        this.EEM.Rdata)		
			this.EEM.Obj        <- sub("\\.Rdata","_Processed.Rdata",    this.EEM.Rdata)
			this.EEM.csv        <- sub("\\.Rdata","_fabriacted.csv",     this.EEM.Rdata)
			this.EEM.classifier <- sub("\\.Rdata","_classifier.csv",     this.EEM.Rdata)

			this.EEM.string.rev <- paste("EEM",this.EEM.num,"-",this.EEM.fuel,"(GJ)",sep="")
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
			FL.IN.Yannan      <- paste(Path.Script,"allyear_0426_2015.csv",sep="/")
			myLabel.Yannan    <- read.table(file=FL.IN.Yannan,header=FALSE)
			names(myLabel.Yannan) <- "Label"

			FL.IN.OBJ         <- paste(Path.CZ.IN, this.EEM.Rdata,     sep="/")
			FL.OUT.OBJ        <- paste(Path.CZ.OUT,this.EEM.Rdata,     sep="/")
			FL.OUT.CSV        <- paste(Path.CZ.OUT,this.EEM.csv,       sep="/")
			FL.OUT.PDF        <- paste(Path.CZ.OUT,this.EEM.pdf,       sep="/")
			FL.OUT_Dummy.PDF  <- paste(Path.CZ.OUT,this.EEM_dummy.pdf, sep="/")
			FL.OUT.CLSLAB     <- paste(Path.CZ.OUT,this.EEM.clsLAB,    sep="/")
			FL.OUT.CLSDAT     <- paste(Path.CZ.OUT,this.EEM.clsDAT,    sep="/")
			FL.OUT.CLSSUM     <- paste(Path.CZ.OUT,this.EEM.clsSUM,    sep="/")
			FL.weather        <- paste(Path.CZ.OUT,this.EEM.weather,   sep="/")
			FL.PROCESSED.OBJ  <- paste(Path.CZ.OUT,this.EEM.Obj,       sep="/")
			FL.classifier.CSV <- paste(Path.CZ.OUT,this.EEM.classifier,sep="/")


			if (!(file.exists(FL.IN.OBJ)))      {print(paste(FL.IN.OBJ,"   does exist. Check Why!"));die}
			if  (file.exists(FL.OUT.OBJ))       {print(paste(FL.OUT.OBJ,       "exist. Delete it!"));file.remove(FL.OUT.OBJ)}		
			if  (file.exists(FL.OUT.CSV))       {print(paste(FL.OUT.CSV,       "exist. Delete it!"));file.remove(FL.OUT.CSV)}
			if  (file.exists(FL.OUT.PDF))       {print(paste(FL.OUT.PDF,       "exist. Delete it!"));file.remove(FL.OUT.PDF)}		
			if  (file.exists(FL.OUT_Dummy.PDF)) {print(paste(FL.OUT_Dummy.PDF, "exist. Delete it!"));file.remove(FL.OUT_Dummy.PDF)}		
			if  (file.exists(FL.OUT.CLSLAB))    {print(paste(FL.OUT.CLSLAB,    "exist. Delete it!"));file.remove(FL.OUT.CLSLAB)}			
			if  (file.exists(FL.OUT.CLSDAT))    {print(paste(FL.OUT.CLSDAT,    "exist. Delete it!"));file.remove(FL.OUT.CLSDAT)}	
			if  (file.exists(FL.OUT.CLSSUM))    {print(paste(FL.OUT.CLSSUM,    "exist. Delete it!"));file.remove(FL.OUT.CLSSUM)}	
			if  (file.exists(FL.weather))       {print(paste(FL.weather,       "exist. Delete it!"));file.remove(FL.weather)}
			if  (file.exists(FL.PROCESSED.OBJ)) {print(paste(FL.PROCESSED.OBJ, "exist. Delete it!"));file.remove(FL.PROCESSED.OBJ)}
			if  (file.exists(FL.classifier.CSV)){print(paste(FL.classifier.CSV,"exist. Delete it!"));file.remove(FL.classifier.CSV)}
			cat(paste("13. [",this.CZ,"]-[",this.EEM.name,"]: prepare files for output.\n",sep=""))
			cat(paste("13. [",this.CZ,"]-[",this.EEM.name,"]: prepare files for output.\n",sep=""),file=FL.LOG,append=TRUE)	


			# ----------------------------------------------------------------------------------------
			# 16. open the pdf file
			# ----------------------------------------------------------------------------------------
			pdf(file = FL.OUT.PDF,      paper="special", width=17, height=11,bg = "transparent")			# dev.set(2) goes to what we want
			pdf(file = FL.OUT_Dummy.PDF,paper="special", width=17, height=11,bg = "transparent")			# dev.set(3) goes to dummy
			cat(paste("16. [",this.CZ,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""))
			cat(paste("16. [",this.CZ,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""),file=FL.LOG,append=TRUE)	



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
			# generate a plot with two years of data
			#
			myTwoYears <- rbind(myData.base,myData.advn)
			myTwoYears[,"week.idx.in.Year"] <- factor(paste("Week",myTwoYears[,"week.idx"],sep=""),levels=paste("Week",seq(1:53),sep=""),labels=paste("Week",seq(1:53),sep=""),ordered = TRUE)				
			myTwoYears[,"week.idx.in.Month"] <- as.factor(paste("Week",myTwoYears[,"week.idx.in.month"],sep=""))

			myTwoYears[,"Janitor"] <- myTwoYears[,"EEM"]
			myTwoYears[,"Janitor"] <- sub("Elec_00_base","B",sub(this.EEM.name,"A",myTwoYears[,"Janitor"]))


			# 10a. plotting the classification results
			# --------------------------------------------------------------
			p.weekly.2years.op1 <- qplot(data=myTwoYears,x=hour.in.week,y=EnergyGJ,group=Janitor,color=Janitor,facets=month.lab~week.idx.in.Month,geom="line") 
			p.weekly.2years.op1 <- p.weekly.2years.op1 + theme(axis.text.x = element_text(angle=0,color="black"),axis.text.y = element_text(color="black"),legend.position="top") 
			p.weekly.2years.op1 <- p.weekly.2years.op1 + labs(x="Hour in the Week",y="Energy (GJ)",title=paste("Eplus simulated electricity consumption of a Large Office building model\n",sep=""))		
			p.weekly.2years.op1 <- p.weekly.2years.op1 + scale_x_discrete(name="Hour in the week",limits=c(48,96,144))
			# --------------------------------------------------------------
			p.daily.2years <- qplot(data=myTwoYears,x=hour,y=EnergyGJ,group=Janitor,color=Janitor,facets=month.lab~day,geom="line")  
			p.daily.2years <- p.daily.2years + theme(axis.text.x = element_text(angle=90,color="black"),axis.text.y = element_text(color="black"),legend.position="top") 
			p.daily.2years <- p.daily.2years + labs(x="Hour in the Day",y="Energy (GJ)",title=paste("Eplus simulated electricity consumption of a Large Office building model\n",sep=""))			
			p.daily.2years <- p.daily.2years + scale_x_discrete(name="Hour in the day",limits=c(12,24))
			# --------------------------------------------------------------
			p.weekly.2years.op2 <- qplot(data=myTwoYears,x=hour.in.week,y=EnergyGJ,group=Janitor,color=Janitor,facets=~week.idx.in.Year,geom="line") 
			p.weekly.2years.op2 <- p.weekly.2years.op2 + theme(axis.text.x = element_text(angle=0,color="black"),axis.text.y = element_text(color="black"),legend.position="top") 
			p.weekly.2years.op2 <- p.weekly.2years.op2 + labs(x="Hour in the Week",y="Energy (GJ)",title=paste("Eplus simulated electricity consumption of a Large Office building model\n",sep=""))		
			p.weekly.2years.op2 <- p.weekly.2years.op2 + scale_x_discrete(name="Hour in the week",limits=c(48,96,144))
			# --------------------------------------------------------------

			dev.set(2)
				plot(p.weekly.2years.op1)
				plot(p.weekly.2years.op2)
				plot(p.daily.2years)
		
			FL.Fig00A.JPG <- paste(Path.CZ.OUT,paste(this.EEM.string,"Fig00A_TwoYearPlots_001.jpg",sep="_"),sep="/")
			if  (file.exists(FL.Fig00A.JPG)){print(paste(FL.Fig00A.JPG,"exist. Delete it!"));file.remove(FL.Fig00A.JPG)}		
			jpeg(file = FL.Fig00A.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			dev.set(4)
				plot(p.weekly.2years.op1)
			dev.off(4)	
			
			FL.Fig00B.JPG <- paste(Path.CZ.OUT,paste(this.EEM.string,"Fig00B.JPG",sep="_"),sep="/")
			if  (file.exists(FL.Fig00B.JPG)){print(paste(FL.Fig00B.JPG,"exist. Delete it!"));file.remove(FL.Fig00B.JPG)}		
			jpeg(file = FL.Fig00B.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			dev.set(4)
				plot(p.weekly.2years.op2)
			dev.off(4)
			
			FL.Fig00C.JPG <- paste(Path.CZ.OUT,paste(this.EEM.string,"Fig00C.JPG",sep="_"),sep="/")
			if  (file.exists(FL.Fig00C.JPG)){print(paste(FL.Fig00C.JPG,"exist. Delete it!"));file.remove(FL.Fig00C.JPG)}		
			jpeg(file = FL.Fig00C.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			dev.set(4)
				plot(p.daily.2years)
			dev.off(4)			
			
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
			myData.Fake <- myData.advn												# initialize [myData.Fake] with the EEM data

			idx.weekday <- unique(myData.Fake[myData.Fake[,"day.type.lab"]=="Weekday","day.in.year"])				# day index of all weekdays

			random.52  <- sample(idx.weekday,52,replace=FALSE)									# randomly draw 52 day to represent one weekday in a week on average
			remain.200 <- idx.weekday[!(idx.weekday %in% random.52)]									# the remianing 200 weekdays from the baseline can be used to test the classification 

			myData.Fake[myData.Fake[,"day.in.year"] %in% random.52,] <- myData.base[myData.base[,"day.in.year"] %in% random.52,]	# replace those selected day inthe advn time series with those from baseline series
			myData.Remain <- myData.base[myData.base[,"day.in.year"] %in% remain.200,]						# use the remaining 200 weekday for testing the classifiers

			myData.Fake[,"week.idx.in.Month"]   <- factor(paste("Week",myData.Fake[,"week.idx.in.month"],sep=""))			# add "week.idx.in.Month" for plotting facet label purpose
			myData.Remain[,"week.idx.in.Month"] <- factor(paste("Week",myData.Remain[,"week.idx.in.month"],sep=""))			# add "week.idx.in.Month" for plotting facet label purpose

			myData.Fake[,"week.idx.in.Year"]   <- factor(paste("Week",myData.Fake[,"week.idx"],sep=""),levels=paste("Week",seq(1:53),sep=""),labels=paste("Week",seq(1:53),sep=""),ordered = TRUE)					# add "week.idx.in.Year" for plotting facet label purpose
			myData.Remain[,"week.idx.in.Year"] <- factor(paste("Week",myData.Remain[,"week.idx"],sep=""),levels=paste("Week",seq(1:53),sep=""),labels=paste("Week",seq(1:53),sep=""),ordered = TRUE)				# add "week.idx.in.Year" for plotting facet label purpose


			myData.Fake[,"Janitor"] <- myData.Fake[,"EEM"]
			myData.Fake[,"Janitor"] <- sub("Elec_00_base","B",sub(this.EEM.name,"A",myData.Fake[,"Janitor"]))
			
			myData.Remain[,"Janitor"] <- myData.Remain[,"EEM"]
			myData.Remain[,"Janitor"] <- sub("Elec_00_base","B",sub(this.EEM.name,"A",myData.Remain[,"Janitor"]))
						

			cat(paste("14C. [",this.CZ,"]-[",this.EEM.name,"]: construct [myData.Fake] by using data from EEM by randomly replaced 52 days from the baseline weekday data and put remaining 200 weekdays of baseline into [myData.Remain].\n\n",sep=""))
			cat(paste("14C. [",this.CZ,"]-[",this.EEM.name,"]: construct [myData.Fake] by using data from EEM by randomly replaced 52 days from the baseline weekday data and put remaining 200 weekdays of baseline into [myData.Remain].\n\n",sep=""),file=FL.LOG,append=TRUE)


			# ********************************************
			# ********************************************
			# ********************************************
			# ********************************************
			# ********************************************
			# [myData.Fake] has been created!!!!!
			# ********************************************
			# ********************************************
			# ********************************************
			# ********************************************
			# ********************************************


			#
			# 15. output: immediately save out
			#     April 3, 2015: although we only have [myData.Fake] and all other data frames are identical to this one, to minimize the script change, I still duplicate them!!!!
			#
			save(myData.base,myData.advn,myData.Fake,file=FL.OUT.OBJ)
			cat(paste("[",this.CZ,"]-[",this.EEM.name,"]: fabricated data,",sep=""),file=FL.OUT.CSV,append=TRUE)
			write.table(myData.Fake,file=FL.OUT.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat("\n\n",file=FL.OUT.CSV,append=TRUE)

			cat(paste("[",this.CZ,"]-[",this.EEM.name,"]: remaining 200 weekdays from baseline for verification of classifiers,",sep=""),file=FL.OUT.CSV,append=TRUE)
			write.table(myData.Remain,file=FL.OUT.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat("\n\n",file=FL.OUT.CSV,append=TRUE)

			cat(paste("15. [",this.CZ,"]-[",this.EEM.name,"]: save the fabricated data [myData.Fake] and remaining 200 weekdays from baseline in [myData.Remain] out into [",FL.OUT.OBJ,"] and [",FL.OUT.CSV,"].\n",sep=""))
			cat(paste("15. [",this.CZ,"]-[",this.EEM.name,"]: save the fabricated data [myData.Fake] and remaining 200 weekdays from baseline in [myData.Remain] out into [",FL.OUT.OBJ,"] and [",FL.OUT.CSV,"].\n",sep=""),file=FL.LOG,append=TRUE)



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



			# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
			# add Yannan's label to the data
			# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
			myLabel.Yannan.rev <- rep(myLabel.Yannan[,"Label"],each=24)
			myData.Fake[,"Label"] <- myLabel.Yannan.rev
			myData.Fake[,"label"] <- myLabel.Yannan[,"Label"]
			myData.Fake[myData.Fake[,"Label"]==  0,"label"] <- "training"
			myData.Fake[myData.Fake[,"Label"]==  1,"label"] <- "normal"
			myData.Fake[myData.Fake[,"Label"]== -1,"label"] <- "abnormal"
			myData.Fake[,"label"] <- factor(myData.Fake[,"label"],levels = c("abnormal","normal"),labels = c("abnormal","normal"),ordered = TRUE)

			# colored differently for EEM and baseline
			p.daily.Yannan <- qplot(data=myData.Fake,x=hour,y=EnergyGJ,color=label,shape=Janitor,facets=month.lab~day,geom=c("line","point")) + scale_shape_manual(values=c("o",""))   
			p.daily.Yannan <- p.daily.Yannan + theme(axis.text.x = element_text(angle=90,color="black"),axis.text.y = element_text(color="black"),legend.position="top") 
			p.daily.Yannan <- p.daily.Yannan + labs(x="Hour in the Day",y="Energy (GJ)",title=paste(paste("Raw Data: ",this.EEM.string.rev,sep="")," (",this.EEM.saving,"%). Profiles of Days in Each Month (",this.CZ,")",sep=""))			
			dev.set(2)
			plot(p.daily.Yannan)
			cat(paste("20C. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month\n",sep=""))
			cat(paste("20C. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month.\n",sep=""),file=FL.LOG,append=TRUE)	









			### # ----------------------------------------------------------------------------------------
			### # PLOTTING RAW DATA ..................
			### # ----------------------------------------------------------------------------------------
			### # --------------------------------------------------------------
			### # 20A. plot weekly plot in each month
			### # --------------------------------------------------------------
			### p.weekly.all.op1 <- qplot(data=myData.Fake,x=hour.in.week,y=EnergyGJ,group=Janitor,color=Janitor,facets=month.lab~week.idx.in.Month,geom="line") 
			### p.weekly.all.op1 <- p.weekly.all.op1 + theme(axis.text.x = element_text(angle=0,color="black"),axis.text.y = element_text(color="black"),legend.position="none") 
			### p.weekly.all.op1 <- p.weekly.all.op1 + labs(x="Hour in the Week",y="Energy (GJ)",title=paste("Raw Data: Synthetized uses Eplus annual simulation data of a baseline and an EEM model\n",sep=""))	
			### p.weekly.all.op1 <- p.weekly.all.op1 + scale_x_discrete(name="Hour in the week",limits=c(48,96,144))
			### dev.set(2)
			### plot(p.weekly.all.op1)
			### cat(paste("20A. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly profile for each month.\n",sep=""))
			### cat(paste("20A. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly profile for each month.\n",sep=""),file=FL.LOG,append=TRUE)	
			### 
			### # --------------------------------------------------------------
			### # 20AA. plot weekly plot in each month (showing only weekdays)
			### # --------------------------------------------------------------
			### myData.Fake.Weekday <- myData.Fake
			### myData.Fake.Weekday[!(myData.Fake.Weekday[,"day.type.lab"]=="Weekday"),"EnergyGJ"] <- NaN
			### p.weekly.wkday.op1 <- qplot(data=myData.Fake.Weekday,x=hour.in.week,y=EnergyGJ,group=Janitor,color=Janitor,facets=month.lab~week.idx.in.Month,geom="line") 
			### p.weekly.wkday.op1 <- p.weekly.wkday.op1 + theme(axis.text.x = element_text(angle=0,color="black"),axis.text.y = element_text(color="black"),legend.position="none") 
			### p.weekly.wkday.op1 <- p.weekly.wkday.op1 + labs(x="Hour in the Week",y="Energy (GJ)",title=paste("Raw Data: Synthetized uses Eplus annual simulation data of a baseline and an EEM model\nWeekday Only",sep=""))	
			### p.weekly.wkday.op1 <- p.weekly.wkday.op1 + scale_x_discrete(name="Hour in the week",limits=c(48,96,144))
			### dev.set(2)
			### plot(p.weekly.wkday.op1)
			### cat(paste("20AA. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly profile for each month.\n",sep=""))
			### cat(paste("20AA. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly profile for each month.\n",sep=""),file=FL.LOG,append=TRUE)	
			### 
			### # --------------------------------------------------------------
			### # 20B. plot weekly plot in conseccutive weeks
			### # --------------------------------------------------------------
			### p.weekly.all.op2 <- qplot(data=myData.Fake,x=hour.in.week,y=EnergyGJ,group=Janitor,color=Janitor,facets=~week.idx.in.Year,geom="line") 
			### p.weekly.all.op2 <- p.weekly.all.op2 + theme(axis.text.x = element_text(angle=0,color="black"),axis.text.y = element_text(color="black"),legend.position="none") 
			### p.weekly.all.op2 <- p.weekly.all.op2 + labs(x="Hour in the Week",y="Energy (GJ)",title=paste("Raw Data: Synthetized uses Eplus annual simulation data of a baseline and an EEM model\n",sep=""))		
			### p.weekly.all.op2 <- p.weekly.all.op2 + scale_x_discrete(name="Hour in the week",limits=c(48,96,144))
			### dev.set(2)
			### plot(p.weekly.all.op2)
			### cat(paste("20B. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly plot in conseccutive weeks.\n",sep=""))
			### cat(paste("20B. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly plot in conseccutive weeks.\n",sep=""),file=FL.LOG,append=TRUE)	
			### 
			### 
			### 
			### # --------------------------------------------------------------
			### # 20BB. plot weekly plot in conseccutive weeks
			### # --------------------------------------------------------------
			### p.weekly.wkday.op2 <- qplot(data=myData.Fake.Weekday,x=hour.in.week,y=EnergyGJ,group=Janitor,color=Janitor,facets=~week.idx.in.Year,geom="line") 
			### p.weekly.wkday.op2 <- p.weekly.wkday.op2 + theme(axis.text.x = element_text(angle=0,color="black"),axis.text.y = element_text(color="black"),legend.position="none") 
			### p.weekly.wkday.op2 <- p.weekly.wkday.op2 + labs(x="Hour in the Week",y="Energy (GJ)",title=paste("Raw Data: Synthetized uses Eplus annual simulation data of a baseline and an EEM model\nWeekday Only",sep=""))		
			### p.weekly.wkday.op2 <- p.weekly.wkday.op2 + scale_x_discrete(name="Hour in the week",limits=c(48,96,144))
			### dev.set(2)
			### plot(p.weekly.wkday.op2)
			### cat(paste("20BB. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly plot in conseccutive weeks.\n",sep=""))
			### cat(paste("20BB. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly plot in conseccutive weeks.\n",sep=""),file=FL.LOG,append=TRUE)	
			### 
			### 
			### 
			### # --------------------------------------------------------------
			### # 20C. plot daily plot in each month
			### # --------------------------------------------------------------
			### # did not color differently
			### p.daily.all.op1 <- qplot(data=myData.Fake,x=hour,y=EnergyGJ,color="black",facets=month.lab~day,geom="line") 
			### p.daily.all.op1 <- p.daily.all.op1 + theme(axis.text.x = element_text(angle=90,color="black"),axis.text.y = element_text(color="black"),legend.position="none") 
			### p.daily.all.op1 <- p.daily.all.op1 + labs(x="Hour in the Day",y="Energy (GJ)",title=paste("Raw Data: Synthetized uses Eplus annual simulation data of a baseline and an EEM model\n",sep=""))			
			### p.daily.all.op1 <- p.daily.all.op1 + scale_x_discrete(name="Hour in the day",limits=c(12,24))
			### dev.set(2)
			### plot(p.daily.all.op1)
			### cat(paste("20C. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month\n",sep=""))
			### cat(paste("20C. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month.\n",sep=""),file=FL.LOG,append=TRUE)	
			### 
			### # colored differently for EEM and baseline
			### p.daily.all.op2 <- qplot(data=myData.Fake,x=hour,y=EnergyGJ,group=Janitor,color=Janitor,facets=month.lab~day,geom="line")  
			### p.daily.all.op2 <- p.daily.all.op2 + theme(axis.text.x = element_text(angle=90,color="black"),axis.text.y = element_text(color="black"),legend.position="none") 
			### p.daily.all.op2 <- p.daily.all.op2 + labs(x="Hour in the Day",y="Energy (GJ)",title=paste("Raw Data: Synthetized uses Eplus annual simulation data of a baseline and an EEM model\n",sep=""))			
			### p.daily.all.op2 <- p.daily.all.op2 + scale_x_discrete(name="Hour in the day",limits=c(12,24))
			### dev.set(2)
			### plot(p.daily.all.op2)
			### cat(paste("20C. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month\n",sep=""))
			### cat(paste("20C. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month.\n",sep=""),file=FL.LOG,append=TRUE)	
			### 
			### 
			### 
			### # --------------------------------------------------------------
			### # 20CC. plot daily plot in each month
			### # --------------------------------------------------------------
			### # did not color differently
			### p.daily.wkday.op1 <- qplot(data=myData.Fake.Weekday,x=hour,y=EnergyGJ,group=EEM,color="black",facets=month.lab~day,geom="line")  
			### p.daily.wkday.op1 <- p.daily.wkday.op1 + theme(axis.text.x = element_text(angle=90,color="black"),axis.text.y = element_text(color="black"),legend.position="none") 
			### p.daily.wkday.op1 <- p.daily.wkday.op1 + labs(x="Hour in the Day",y="Energy (GJ)",title=paste("Raw Data: Synthetized uses Eplus annual simulation data of a baseline and an EEM model\nWeekday Only",sep=""))			
			### p.daily.wkday.op1 <- p.daily.wkday.op1 + scale_x_discrete(name="Hour in the day",limits=c(12,24))
			### dev.set(2)
			### plot(p.daily.wkday.op1)
			### cat(paste("20CC. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month\n",sep=""))
			### cat(paste("20CC. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month.\n",sep=""),file=FL.LOG,append=TRUE)	
			### 
			### # colored differently for EEM and baseline
			### p.daily.wkday.op2 <- qplot(data=myData.Fake.Weekday,x=hour,y=EnergyGJ,group=Janitor,color=Janitor,facets=month.lab~day,geom="line")  
			### p.daily.wkday.op2 <- p.daily.wkday.op2 + theme(axis.text.x = element_text(angle=90,color="black"),axis.text.y = element_text(color="black"),legend.position="none") 
			### p.daily.wkday.op2 <- p.daily.wkday.op2 + labs(x="Hour in the Day",y="Energy (GJ)",title=paste("Raw Data: Synthetized uses Eplus annual simulation data of a baseline and an EEM model\nWeekday Only",sep=""))			
			### p.daily.wkday.op2 <- p.daily.wkday.op2 + scale_x_discrete(name="Hour in the day",limits=c(12,24))
			### dev.set(2)
			### plot(p.daily.wkday.op2)
			### cat(paste("20CCC. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month\n",sep=""))
			### cat(paste("20CCC. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month.\n",sep=""),file=FL.LOG,append=TRUE)	
			### 
			### 
			### 
			### 
			### 
			### 
			### # ********************************************************************************************************
			### # ********************************************************************************************************
			### # ********************************************************************************************************
			### # ********************************************************************************************************
			### #                     Making JPEG PLOTS
			### # April 5, 2015: plot [p.weekly.all.op1], [p.weekly.wkday.op1], [p.weekly.all.op2], [p.weekly.wkday.op2],	[p.daily.all.op1],	[p.daily.all.op2],	[p.daily.wkday.op1],	[p.daily.wkday.op1] as JPEG plots into
			### #                     [FL.Fig01A.JPG],    [FL.Fig01B.JPG],      [FL.Fig02A.JPG],    [FL.Fig02B.JPG],		[FL.Fig03A.JPG],	[FL.Fig03B.JPG], 	[FL.Fig03C.JPG], 	[FL.Fig03D.JPG]
			### # ********************************************************************************************************
			### # ********************************************************************************************************
			### # ********************************************************************************************************
			### # ********************************************************************************************************	
			### FL.Fig01A.JPG <- paste(Path.CZ.OUT,paste("Fig01A_FakedOneYear_Week_Month_002.jpg",sep="_"),sep="/")
			### if  (file.exists(FL.Fig01A.JPG)){print(paste(FL.Fig01A.JPG,"exist. Delete it!"));file.remove(FL.Fig01A.JPG)}		
			### jpeg(file = FL.Fig01A.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			### dev.set(4)
			### 	plot(p.weekly.all.op1)
			### dev.off(4)
			### 
			### FL.Fig01B.JPG <- paste(Path.CZ.OUT,paste("Fig01B.jpg",sep="_"),sep="/")
			### if  (file.exists(FL.Fig01B.JPG)){print(paste(FL.Fig01B.JPG,"exist. Delete it!"));file.remove(FL.Fig01B.JPG)}		
			### jpeg(file = FL.Fig01B.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			### dev.set(4)
			### 	plot(p.weekly.wkday.op1)
			### dev.off(4)		
			### 
			### FL.Fig02A.JPG <- paste(Path.CZ.OUT,paste("Fig02A.jpg",sep="_"),sep="/")
			### if  (file.exists(FL.Fig02A.JPG)){print(paste(FL.Fig02A.JPG,"exist. Delete it!"));file.remove(FL.Fig02A.JPG)}		
			### jpeg(file = FL.Fig02A.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			### dev.set(4)
			### 	plot(p.weekly.all.op2)
			### dev.off(4)
			### 
			### FL.Fig02B.JPG <- paste(Path.CZ.OUT,paste("Fig02B.jpg",sep="_"),sep="/")
			### if  (file.exists(FL.Fig02B.JPG)){print(paste(FL.Fig02B.JPG,"exist. Delete it!"));file.remove(FL.Fig02B.JPG)}		
			### jpeg(file = FL.Fig02B.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			### dev.set(4)
			### 	plot(p.weekly.wkday.op2)
			### dev.off(4)		
			### 
			### 
			### # Color the same for EEM and baseline
			### FL.Fig03A.JPG <- paste(Path.CZ.OUT,paste("Fig03A_FakedOneYear_Day_Month_OneColor_004.jpg",sep="_"),sep="/")
			### if  (file.exists(FL.Fig03A.JPG)){print(paste(FL.Fig03A.JPG,"exist. Delete it!"));file.remove(FL.Fig03A.JPG)}		
			### jpeg(file = FL.Fig03A.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			### dev.set(4)
			### 	plot(p.daily.all.op1)
			### dev.off(4)
			### 
			### 
			### FL.Fig03AA.JPG <- paste(Path.CZ.OUT,paste("Fig03AA_FakedOneYear_Day_Month_OneColor_015.jpg",sep="_"),sep="/")
			### if  (file.exists(FL.Fig03AA.JPG)){print(paste(FL.Fig03AA.JPG,"exist. Delete it!"));file.remove(FL.Fig03AA.JPG)}		
			### jpeg(file = FL.Fig03A.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			### dev.set(4)
			### 	plot(p.daily.all.op1)
			### dev.off(4)
			### 
			###
			### # ********************************************************************************************************	
			### # color differently for EEM and Baseline
			### FL.Fig03B.JPG <- paste(Path.CZ.OUT,paste("Fig03B_FakedOneYear_Day_Month_003.jpg",sep="_"),sep="/")
			### if  (file.exists(FL.Fig03B.JPG)){print(paste(FL.Fig03B.JPG,"exist. Delete it!"));file.remove(FL.Fig03B.JPG)}		
			### jpeg(file = FL.Fig03B.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			### dev.set(4)
			### 	plot(p.daily.all.op2)
			### dev.off(4)
			### 
			### 
			### # Color the same for EEM and baseline
			### FL.Fig03C.JPG <- paste(Path.CZ.OUT,paste("Fig03C_FakedOneYear_WeekdayOnly_OneColor_005.jpg",sep="_"),sep="/")
			### if  (file.exists(FL.Fig03C.JPG)){print(paste(FL.Fig03C.JPG,"exist. Delete it!"));file.remove(FL.Fig03C.JPG)}		
			### jpeg(file = FL.Fig03C.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			### dev.set(4)
			### 	plot(p.daily.wkday.op1)
			### dev.off(4)
			### 
			### # ********************************************************************************************************	
			### # color differently for EEM and Baseline
			### FL.Fig03D.JPG <- paste(Path.CZ.OUT,paste("Fig03D.jpg",sep="_"),sep="/")
			### if  (file.exists(FL.Fig03D.JPG)){print(paste(FL.Fig03D.JPG,"exist. Delete it!"));file.remove(FL.Fig03D.JPG)}		
			### jpeg(file = FL.Fig03D.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			### dev.set(4)
			### 	plot(p.daily.wkday.op2)
			### dev.off(4)
			### cat(paste("20DD. [",this.CZ,"]-[",this.EEM.name,"]: the plots are re-plotted to JPEG files\n",sep=""))
			### cat(paste("20CC. [",this.CZ,"]-[",this.EEM.name,"]: the plots are re-plotted to JPEG files\n",sep=""),file=FL.LOG,append=TRUE)	




			# ********************************************************************************************************	
			# color differently for EEM and Baseline
			FL.Fig_Yannan.JPG <- paste(Path.CZ.OUT,paste(this.EEM.string,"Fig_Yannan.jpg",sep="_"),sep="/")
			if  (file.exists(FL.Fig_Yannan.JPG)){print(paste(FL.Fig_Yannan.JPG,"exist. Delete it!"));file.remove(FL.Fig_Yannan.JPG)}		
			jpeg(file = FL.Fig_Yannan.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")					# device 4
			dev.set(4)
				plot(p.daily.Yannan)
			dev.off(4)


			dev.off(2)
			dev.off(3)
		}			# end of current EEM
		cat(paste("80. [",this.CZ,"]: finished the processing of all EEMs.\n\n\n",sep=""))
		cat(paste("80. [",this.CZ,"]: finished the processing of all EEMs.\n\n\n",sep=""),file=FL.LOG,append=TRUE)	
	}				# end of current CZ

	cat(paste("\n\n90. Completed the processing the data of all CZs.\n",sep=""))
	cat(paste("\n\n90. Completed the processing the data of all CZs.\n",sep=""),file=FL.LOG,append=TRUE)	
}	# end of random loop

# -------------------------------------------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\nPepare_JPG_Figures_for_Yannan_04262015_AllYear.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\nPepare_JPG_Figures_for_Yannan_04262015_AllYear.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)


cat(paste("\nProcessing time for [Pepare_JPG_Figures_for_Yannan_04262015_AllYear.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [Pepare_JPG_Figures_for_Yannan_04262015_AllYear.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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
