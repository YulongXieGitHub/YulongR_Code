#
# 00_process_epw_files.R 
# 
# March 11, 2015
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
	Path.Out     <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/00_process_epw"
	Path.EPW     <- "/phome/comstd/CtrlBenefit/TMY3_epw"
}else{
	Path.Project <- "C:/Yulong_Projects/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining"
	Path.Sim     <- "C:/Yulong_Projects/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining"
	Path.Script  <- "C:/Yulong_Projects/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/0_scripts"
	Path.Out     <- "C:/Yulong_Projects/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/00_process_epw"
	Path.EPW     <- "C:/Yulong_Projects/CtrlBenefit/TMY3_epw"	

	Path.Project <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining"
	Path.Sim     <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining"
	Path.Script  <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/0_scripts"
	Path.Out     <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/00_process_epw"
	Path.EPW     <- "X:/CtrlBenefit/TMY3_epw"
}
setwd(Path.Script)

if (!file.exists(Path.Out)){print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}

FL.OBJ <- paste(Path.Out,"epw.Rdata",sep="/")
FL.LOG <- paste(Path.Out,"00_process_epw.log",sep="/")
if (file.exists(FL.OBJ)){print(paste(FL.OBJ," exist. Delete it!"));file.remove(FL.OBJ)}		
if (file.exists(FL.LOG)){print(paste(FL.LOG," exist. Delete it!"));file.remove(FL.LOG)}		
cat(paste("1. specify inptu/out file/folder.\n",sep=""))
cat(paste("1. specify inptu/out file/folder.\n",sep=""),file=FL.LOG,append=TRUE)	


# arrays of the CZ
CZ.arrays.city <- c("Miami","Houston","Phoenix","Atlanta","LosAngeles","LasVegas","SanFrancisco","Baltimore","Albuquerque","Seattle","Chicago","Denver","Minneapolis","Helena","Duluth","Fairbanks")


CZ.arrays.epw <- c("USA_FL_Miami.Intl.AP.722020_TMY3.epw",
		   "USA_TX_Houston-Bush.Intercontinental.AP.722430_TMY3.epw",
		   "USA_AZ_Phoenix-Sky.Harbor.Intl.AP.722780_TMY3.epw",
		   "USA_GA_Atlanta-Hartsfield-Jackson.Intl.AP.722190_TMY3.epw",		
		   "USA_CA_Los.Angeles.Intl.AP.722950_TMY3.epw",
		   "USA_NV_Las.Vegas-McCarran.Intl.AP.723860_TMY3.epw",
		   "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw",
		   "USA_MD_Baltimore-Washington.Intl.AP.724060_TMY3.epw",
		   "USA_NM_Albuquerque.Intl.AP.723650_TMY3.epw",
		   "USA_WA_Seattle-Tacoma.Intl.AP.727930_TMY3.epw",		
		   "USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw",
		   "USA_CO_Denver.Intl.AP.725650_TMY3.epw",
		   "USA_MN_Minneapolis-St.Paul.Intl.AP.726580_TMY3.epw",
		   "USA_MT_Helena.Rgnl.AP.727720_TMY3.epw",
		   "USA_MN_Duluth.Intl.AP.727450_TMY3.epw",		
		   "USA_AK_Fairbanks.Intl.AP.702610_TMY3.epw")
		   
CZ.Station.ID <- c("722020",
		   "722430",
		   "722780",
		   "722190",		
		   "722950",
		   "723860",
		   "724940",
		   "724060",
		   "723650",
		   "727930",		
		   "725300",
		   "725650",
		   "726580",
		   "727720",
		   "727450",		
		   "702610") 		   	
fields <- c("epw.year",
	    "epw.month",
	    "epw.day",
	    "epw.hour",
	    "epw.minute",
	    "epw.data.source",
	    "epw.T.drybulb",
	    "epw.T.dewpoint",
	    "epw.rel.humidity",
	    "epw.atm.pressure",
	    "epw.ext.hor.rad",
	    "epw.ext.direct.norm.rad",
	    "epw.hor.ir.sky",
	    "epw.global.hor.rad",
	    "epw.direct.norm.rad",
	    "epw.diffuse.hor.rad",
	    "epw.global.hor.illu",
	    "epw.direct.norm.illu",
	    "epw.diffuse.hor.illu",
	    "epw.zenith.lum",
	    "epw.wind.direct",
	    "epw.wind.speed",
	    "epw.total.sky.cover",
	    "epw.opaque.sky.cover",
	    "epw.visibility",
	    "epw.ceiling.hgt",
	    "epw.pres.weath.obs",
	    "epw.pres.weath.codes",
	    "epw.precip.water",
	    "epw.aerosol.opt.depth",
	    "epw.snow.depth",
	    "epw.days.last.snow",
	    "epw.albedo",
	    "epw.liquid.precip.depth",
	    "epw.liquid.precip.rate")	
names(CZ.arrays.epw) <- CZ.arrays.city
names(CZ.Station.ID) <- CZ.arrays.city
cat(paste("2. defined the weather files, names of the CZ cities.\n\n",sep=""))
cat(paste("2. defined the weather files, names of the CZ cities.\n\n",sep=""),file=FL.LOG,append=TRUE)	



# read in the epw weather file for the 16 stations
command.string.all <- "save("
command.string.rm  <- "rm("
count <- 0
for (this.city in CZ.arrays.city)
{
	count <- count + 1
	
	this.station <- CZ.Station.ID[this.city]
	this.epw     <- CZ.arrays.epw[this.city]
	
	# define the weather file
	FL.EPW <- paste(Path.EPW,this.epw,sep="/")
	
	# read in the epw weather file
	myEPW <- read.table(file=FL.EPW,skip=8,header=FALSE,sep=",")
	names(myEPW) <- fields
	cat(paste("3. ",this.city,": read in the epw file from [",FL.EPW,"].\n",sep=""))
	cat(paste("3. ",this.city,": read in the epw file from [",FL.EPW,"].\n",sep=""),file=FL.LOG,append=TRUE)	


	
	# use the cz city name as the data frame name inathe format of "EPW.Miami"	
	df.epw.thisCZ <- paste("EPW.",this.city,sep="")
	command.string <- paste(df.epw.thisCZ," <- myEPW",sep="")
	eval(parse(text=command.string))
	cat(paste("4. ",this.city,": use the city name as the data frame name to store the weather data read in.\n",sep=""))
	cat(paste("4. ",this.city,": use the city name as the data frame name to store the weather data read in.\n",sep=""),file=FL.LOG,append=TRUE)	
	
	#
	if (count == 1)
	{
		command.string.all <- paste(command.string.all,df.epw.thisCZ,",",sep="")
		command.string.rm  <- paste(command.string.rm,df.epw.thisCZ,sep="")
	}else{
		command.string.all <- paste(command.string.all,df.epw.thisCZ,",",sep="")
		command.string.rm  <- paste(command.string.rm,",",df.epw.thisCZ,sep="")
	}
	cat(paste("5. ",this.city,": put the name of the data frame of this city into an array which will be used for saving in Rdata or to delete.\n\n",sep=""))
	cat(paste("5. ",this.city,": put the name of the data frame of this city into an array which will be used for saving in Rdata or to delete.\n\n",sep=""),file=FL.LOG,append=TRUE)	
	
}
cat(paste("6. weather data of all cities have been read in.\n",sep=""))
cat(paste("6. weather data of all cities have been read in.\n",sep=""),file=FL.LOG,append=TRUE)	



# save all the weather data frames into a binary file
command.string.all <- paste(command.string.all,"file=FL.OBJ)",sep="")
eval(parse(text=command.string.all))
cat(paste("7. all data frames of the weather data from all cities have been saved into [",FL.OBJ,"].\n",sep=""))
cat(paste("7. all data frames of the weather data from all cities have been saved into [",FL.OBJ,"].\n",sep=""),file=FL.LOG,append=TRUE)	

# delete all the data frames of the weather data
command.string.rm  <- paste(command.string.rm,")",sep="")
eval(parse(text=command.string.rm))
cat(paste("8. deleted all data frames of the weather data from all cities.\n",sep=""))
cat(paste("8. deleted all data frames of the weather data from all cities.\n",sep=""),file=FL.LOG,append=TRUE)	

# test a re-loading from the saved binary data file
load(FL.OBJ)
count <- 0
for (this.city in CZ.arrays.city)
{
	count <- count + 1
	df.epw.this.city <- paste("EPW.",this.city,sep="")
	command.string <- paste(paste("myEPW",count,sep="")," <- ",df.epw.thisCZ,sep="")
	eval(parse(text=command.string))
}
cat(paste("9. to this point we should see the weather files in the name of both city name like [EPW.Miami] and in myEPW## like [myEPW1].\n",sep=""))
cat(paste("9. to this point we should see the weather files in the name of both city name like [EPW.Miami] and in myEPW## like [myEPW1].\n",sep=""),file=FL.LOG,append=TRUE)	


#
# more p;ots to compare the two time series in terms of weekly plots  
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n00_process_epw_files.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n00_process_epw_files.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [00_process_epw_files.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [00_process_epw_files.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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

