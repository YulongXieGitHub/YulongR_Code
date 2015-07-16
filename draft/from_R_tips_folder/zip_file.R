#
# "gather_files_for_webPosting.R" 
#
# This script is used to gather files for reporting the ASHRAE30pct Final PI results to DOE web site.
#
# Based on the meeting with Rose and Shannon, Bing and Heejin on April 11, 2011
# We will make a zip file for each prototype at each of the three standard
# That means we will have 3 (standards) by 16 (prototypes) = 48 zipped files
# Each zipped file will consist of 34 files, i.e., 17 idf files and 17 html files
#
# So the original file gatherring script has been re-written for this purpose
#
# We will have 3 subfolders for holding these files, i.e., 2004, 2007 and 2010
#
# each of these three folder will have 16 zipped files one for each prototype
#
# 
# (1) idf file:   from all input nobackup folder
# (2) html files: from all output nobackup folder
# 
# April 14, 2011
#
# revised the TMY2 epw file directory (/phome/weather/EnergyPlus/zip) on April 19, 2011
#
# April 20, 2011: drastic changes based on the simulation team meeting on April 19, 2011
# 
# 0. eliminate all stuff in the workspace

rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number


# close all devices which have been opened
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

# -------------------------------------------------------------------------------------------------
# prototype folders are under "/phome/comstd/[projectName]/"
#
prototypes <- c("ApartmentHighRise",
                "ApartmentMidRise",                
                "Hospital",
                "HotelLarge",
                "HotelSmall",
                "OfficeLarge",
                "OfficeMedium",
                "OfficeSmall",
                "OutPatientHealthCare",                
                "RestaurantFastFood",
                "RestaurantSitDown",
                "RetailStandalone",
                "RetailStripmall",
                "SchoolPrimary",
                "SchoolSecondary",
                "Warehouse")
                
                
climate.zones <- c("Miami",
                   "Riyadh",
                   "Houston",
                   "Phoenix",
                   "Memphis",
                   "El_Paso",
                   "San_Francisco",
                   "Baltimore",
                   "Albuquerque",
                   "Salem",
                   "Chicago",
                   "Boise",
                   "Vancouver",
                   "Burlington",
                   "Helena",
                   "Duluth",
                   "Fairbanks")
                                
#
# file name convention
#
        cases     <- c("ASHRAE30pct","ASHRAE30pct","ASHRAE30pct.PI.Final11")
       standards  <- c("STD2004",    "STD2007",    "STD2010")

 names(cases)     <- standards
 names(standards) <- standards

      
      scorecards  <- c("Apartment_Highrise","Apartment_Midrise","Hospital","Hotel_Large","Hotel_Small","LargeOffice","Office_Medium","Office_Small","OutpatientHealthCare","Restaurant_FastFood","Restaurant_Sit_Down","Retail_Standalone","Retail_StripMall","School_Primary","School_Secondary","Warehouse")
names(scorecards) <- prototypes


# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "/phome/comstd/ASHRAE30pct_for_reporting"
}else{
	Path.Current <- "../not_existing"
}

setwd(Path.Current)


# define data paths
Path.Report     <- paste(Path.Current,"_file_for_webposting",sep="/")
Path.LOG        <- Path.Report
Path.scorecard  <- paste(Path.Current,"_file_for_webposting","PNNL-Scorecards",sep="/")		# the location of the scorecards
Path.weather    <- "/phome/weather/EnergyPlus/zip"						# there are multiple locations with different TMY2 files and this folder contains whatr we actually used
Path.idf        <- paste(Path.Report,"idf",sep="/")
Path.htm        <- paste(Path.Report,"htm",sep="/")
Path.epw        <- paste(Path.Report,"epw",sep="/")
Path.sc         <- paste(Path.Report,"sc",sep="/")
FL.LOG          <- paste(Path.LOG,"gather_files_for_webPosting.log",sep="/")
Path.target     <- paste(Path.Report,"zip",sep="/")

if (!file.exists(Path.Report)){print(paste(Path.Report," does not exist. Create it!"));dir.create(Path.Report)}
if (!file.exists(Path.LOG)){print(paste(Path.LOG," does not exist. Create it!"));dir.create(Path.LOG)}
if (!file.exists(Path.scorecard)){print(paste(Path.scorecard," does not exist. check it why!"));die}
if (!file.exists(Path.weather)){print(paste(Path.weather," does not exist. check it why!"));die}
if (!file.exists(Path.Report)){print(paste(Path.Report," does not exist. Create it!"));dir.create(Path.Report)}
if (!file.exists(Path.idf)){print(paste(Path.idf," does not exist. Create it!"));dir.create(Path.idf)}
if (!file.exists(Path.htm)){print(paste(Path.htm," does not exist. Create it!"));dir.create(Path.htm)}
if (!file.exists(Path.epw)){print(paste(Path.epw," does not exist. Create it!"));dir.create(Path.epw)}
if (!file.exists(Path.sc)){print(paste(Path.sc," does not exist. Create it!"));dir.create(Path.sc)}
if (!file.exists(Path.target)){print(paste(Path.target," does not exist. Create it!"));dir.create(Path.target)}
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}



# ---------------------------------------------------------------------------------------------------------
# 1. zip scorecards
# ---------------------------------------------------------------------------------------------------------
cat("\n\n")
cat("\n\n",file=FL.LOG,append=TRUE)
cat(paste("------------------------------------------------\n",sep=""))
cat(paste("------------------------------------------------\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("zip scorecards for all 16 prototypes............\n",sep=""))
cat(paste("zip scorecards for all 16 prototypes............\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("------------------------------------------------\n",sep=""))
cat(paste("------------------------------------------------\n",sep=""),file=FL.LOG,append=TRUE)
zip.scorecard <- paste(Path.target,paste("scorecards.zip",sep=""),sep="/")
if (file.exists(zip.scorecard)){print(paste(zip.scorecard,"exist.Delete it!"));file.remove(zip.scorecard)}
cat(paste("\n\ndefined a zip file name [",zip.scorecard,"] for scorecard files......\n",sep=""))
cat(paste("\n\ndefined a zip file name [",zip.scorecard,"] for scorecard files......\n",sep=""),file=FL.LOG,append=TRUE)

file.scorecards <- paste("PNNL_Scorecard_90.1Prototypes_",scorecards,".xlsx",sep="")		# gathering all scorecards
source.scorecard.name <- paste(Path.scorecard,file.scorecards,sep="/")				# add source path in front of the scorecards
system(paste("cp -p ",paste(shQuote(source.scorecard.name),collapse=" "),shQuote(Path.target)))	# copy the scorecards over to the target directory
file.all <- file.scorecards									# for zipping all files in a giant zip file
cat(paste("\n\nscorecard file has been copied oveer to [",Path.target,"]......\n",sep=""))
cat(paste("\n\nscorecard file has been copied oveer to [",Path.target,"]......\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------
# switch to the target directory before zipping to avoid paths
# -------------------------------------------------------------------------				
setwd(Path.target)
		
system(paste("zip ",zip.scorecard,paste(shQuote(file.scorecards),collapse=" ")))
cat(paste("the scorecard files have been zipped to [",zip.scorecard,"]\n",sep=""))
cat(paste("the scorecard files have been zipped to [",zip.scorecard,"]\n",sep=""),file=FL.LOG,append=TRUE)

setwd(Path.Current)





# ---------------------------------------------------------------------------------------------------------
# 2. zip weather files
# ---------------------------------------------------------------------------------------------------------
cat("\n\n")
cat("\n\n",file=FL.LOG,append=TRUE)
cat(paste("------------------------------------------------\n",sep=""))
cat(paste("------------------------------------------------\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("zip weather files for 17 climate locations......\n",sep=""))
cat(paste("zip weather files for 17 climate locations......\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("------------------------------------------------\n",sep=""))
cat(paste("------------------------------------------------\n",sep=""),file=FL.LOG,append=TRUE)

zip.epw <- paste(Path.target,paste("epw.zip",sep=""),sep="/")
if (file.exists(zip.epw)){print(paste(zip.epw,"exist.Delete it!"));file.remove(zip.epw)}
cat(paste("\n\ndefined a zip file name [",zip.epw,"] for weather files......\n",sep=""))
cat(paste("\n\ndefined a zip file name [",zip.epw,"] for weather files......\n",sep=""),file=FL.LOG,append=TRUE)

# get the weather file name in any of the parm csv file
FL.parm.csv <- paste(Path.Current,"Warehouse","diffOA","ASHRAE30pct_Warehouse_parm.csv",sep="/")	# pick any parm csv file to get the list of climate locations
     myData <- read.table(file=FL.parm.csv,stringsAsFactors=FALSE,header=TRUE,sep=",")
file.epw    <- unique(myData[,"weatherfile"])								# gathering all weather files
source.epw.name <- paste(Path.weather,file.epw,sep="/")							# add source path in front of the weather files
file.all <- c(file.all,file.epw)									# for zipping all files in a giant zip file

system(paste("cp -p ",paste(shQuote(source.epw.name),collapse=" "),shQuote(Path.target)))		# copy the scorecards over to the target directory
cat(paste("\n\nweather files have been copied oveer to [",Path.target,"]......\n",sep=""))
cat(paste("\n\nweather files have been copied oveer to [",Path.target,"]......\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------
# switch to the target directory before zipping to avoid paths
# -------------------------------------------------------------------------				
setwd(Path.target)

system(paste("zip ",zip.epw,paste(shQuote(file.epw),collapse=" ")))
cat(paste("the weather files have been zipped to [",zip.epw,"]\n",sep=""))
cat(paste("the weather files have been zipped to [",zip.epw,"]\n",sep=""),file=FL.LOG,append=TRUE)

setwd(Path.Current)







# ---------------------------------------------------------------------------------------------------------
# 3. Prototype-wise zip files: each consists of 3 (standards) * 34 (idf & htm) + 1 (scorecard) = 103
# one zip file for each prototype which constsis of the idf (17 by 3), html (17 by 3) files and the score card (1): total 103 file per zip file
# ---------------------------------------------------------------------------------------------------------
cat(paste("zip file for each prototype of each standard individually as well as each prototype of all 3 standards\n",sep=""))
cat(paste("zip file for each prototype of each standard individually as well as each prototype of all 3 standards\n",sep=""),file=FL.LOG,append=TRUE)

Path.target <- paste(Path.Current,"_file_for_webposting","zip",sep="/")
if (!file.exists(Path.target)){print(paste(Path.target," does not exist. Create it!"));dir.create(Path.target)}

for (prototype in prototypes)
{
	cat("--------------------------------------\n",file=FL.LOG,append=TRUE)
	cat("              ",prototype,"           \n",file=FL.LOG,append=TRUE)
	cat("--------------------------------------\n",file=FL.LOG,append=TRUE)
	# the scroe card file name of current prototype
	scorecard <- scorecards[prototype]
	file.scorecard <- paste("PNNL_Scorecard_90.1Prototypes_",scorecard,".xlsx",sep="") 

	# zipped file for current prototype
	zip.thisBldg <- paste(Path.target,paste(prototype,".zip",sep=""),sep="/")
	if (file.exists(zip.thisBldg)){print(paste(zip.thisBldg,"exist.Delete it!"));file.remove(zip.thisBldg)}
	cat(paste("\n\ndefined a zip file name [",zip.thisBldg,"] for (idf) and (html) files of all standards of [",prototype,"]\n",sep=""))
	cat(paste("\n\ndefined a zip file name [",zip.thisBldg,"] for (idf) and (html) files of all standards.of [",prototype,"]\n",sep=""),file=FL.LOG,append=TRUE)	
	

	# -----------------------------------------------------------------------------------------
	# define the source directory of the idf and html file folders
	# -----------------------------------------------------------------------------------------
	Path.idf.source <- paste(Path.Current, prototype,"diffOA","input.nobackup", sep="/")	# taken files from "diffOA"
	Path.htm.source <- paste(Path.Current, prototype,"diffOA","output.nobackup",sep="/")	# taken files from "diffOA"

	#
	# also a zip file for each prototype at each given standard which consists of 34 files
	idx.standard <- 0
	for (standard in standards)
	{
		idx.standard <- idx.standard + 1

		# -------------------------------------------------------------------------------------------------
		# case name is different for each of the standard
		# -------------------------------------------------------------------------------------------------
		case <- cases[standard]	# part 3 in the case name for each standard	
		cat(paste("gathering files for [",case,"] [",standard,"]\n"))
		cat(paste("gathering files for [",case,"] [",standard,"]\n"),file=FL.LOG,append=TRUE)
		
		cat(standard,"\n",file=FL.LOG,append=TRUE)
		# zipped file for current prototype
		zip.thisBldg.thisStand <- paste(Path.target,paste(prototype,"_",standard,".zip",sep=""),sep="/")
		if (file.exists(zip.thisBldg.thisStand)){print(paste(zip.thisBldg.thisStand,"exist.Delete it!"));file.remove(zip.thisBldg.thisStand)}
		cat(paste("\n\ndefined a zip file name [",zip.thisBldg.thisStand,"] for (idf) and (html) files of [",standard,"] of [",prototype,"]\n",sep=""))
		cat(paste("\n\ndefined a zip file name [",zip.thisBldg.thisStand,"] for (idf) and (html) files of [",standard,"] of [",prototype,"]\n",sep=""),file=FL.LOG,append=TRUE)	
		
		#
		# a quick check to see the existence of the directory
		#
		if ((file.exists(Path.idf.source)) & (file.exists(Path.htm.source)))		
		{
			# 4 part in a case name
			caseNm.part1 <- case		# case is corresponding to standard
			caseNm.part2 <- prototype	# current prototype
			caseNm.part3 <- standard	# current standard
			caseNm.part4 <- climate.zones	# for all climate zones

			# all the idf and html file name
			idf.thisBldg.thisStand  <- paste(caseNm.part1,"_",caseNm.part2,"_",caseNm.part3,"_",caseNm.part4,".idf",sep="")
			htm.thisBldg.thisStand  <- paste(caseNm.part1,"_",caseNm.part2,"_",caseNm.part3,"_",caseNm.part4,".table.htm",sep="")
			file.thisBldg.thisStand <- c(idf.thisBldg.thisStand,htm.thisBldg.thisStand)						# all the files (idf, html) need to be zipped for this prototype at this standard
			
			# to zip on the prototype level we incluse (a) idf, (b) html in a single zip file
			if (idx.standard == 1)
			{
				files.thisBldg <- c(file.thisBldg.thisStand)
			}else{
				files.thisBldg <- c(files.thisBldg,file.thisBldg.thisStand)							# accumulated idf and htm file for all three standards for a given prototype 
			}
			cat(paste("gathered file names of all idf and htm files of [",standard,"] for [",prototype,"]\n",sep=""))
			cat(paste("gathered file names of all idf and htm files of [",standard,"] for [",prototype,"]\n",sep=""),file=FL.LOG,append=TRUE)


			# add source directory path in fron to the idf and html file names
			source.idf.thisBldg.thisStand   <- paste(Path.idf.source,idf.thisBldg.thisStand,sep="/")
			source.htm.thisBldg.thisStand   <- paste(Path.htm.source,htm.thisBldg.thisStand,sep="/")
			source.allFL.name <- c(source.idf.thisBldg.thisStand,source.htm.thisBldg.thisStand)					# the joint set of the (idf) and (htnl) files

			# to avoid paths appears in the zipped file, we copy the file to the target directory first

		# -p retaining time stamps,   the list of the files to be copied,            the target directory
			system(paste("cp -p ",paste(shQuote(source.allFL.name),collapse=" "),shQuote(Path.target)))				# pay attention to the sequency of paste(collapse) and shQuote)
				
			system(paste("cp -p ",paste(shQuote(source.idf.thisBldg.thisStand),collapse=" "),shQuote(Path.idf)))			# pay attention to the sequency of paste(collapse) and shQuote)
			system(paste("cp -p ",paste(shQuote(source.htm.thisBldg.thisStand),collapse=" "),shQuote(Path.htm)))			# pay attention to the sequency of paste(collapse) and shQuote)


			# -------------------------------------------------------------------------
			# switch to the target directory before zipping to avoid paths
			# -------------------------------------------------------------------------				
			setwd(Path.target)

			# zip the files for current standard of current prototype
			
			system(paste("zip ",zip.thisBldg.thisStand,paste(shQuote(file.thisBldg.thisStand),collapse=" ")))
			cat(paste("the idf and html files of [",prototype,"] of [",standard,"] have been zipped to [",zip.thisBldg.thisStand,"]\n",sep=""))
			cat(paste("the idf and html files of [",prototype,"] of [",standard,"] have been zipped to [",zip.thisBldg.thisStand,"]\n",sep=""),file=FL.LOG,append=TRUE)
			
			setwd(Path.Current)
		}
	}	
	
	#
	# make a zip file for each prototype which includes (a) idf, (b) htm files from all 3 standards and (c) the excel scorecard
	#
	file.all <- c(file.all,files.thisBldg)									# for zipping all files in a giant zip file
	files.thisBldg <- c(files.thisBldg,file.scorecard)							# include scorecard file for this building type
	
	# -------------------------------------------------------------------------
	# switch to the target directory before zipping to avoid paths
	# -------------------------------------------------------------------------				
	setwd(Path.target)

	# zip the files for current standard of current prototype		
	system(paste("zip ",zip.thisBldg,paste(shQuote(files.thisBldg),collapse=" ")))
	cat(paste("the idf and html files and scorecard of [",prototype,"] have been zipped to [",zip.thisBldg,"]\n",sep=""))
	cat(paste("the idf and html files and scorecard of [",prototype,"] have been zipped to [",zip.thisBldg,"]\n",sep=""),file=FL.LOG,append=TRUE)

	setwd(Path.Current)
}


# ---------------------------------------------------------------------------------------------------------
# 4. zip all idf, htm, scorecard and epw files in a single giant zip files
# ---------------------------------------------------------------------------------------------------------
cat("\n\n")
cat("\n\n",file=FL.LOG,append=TRUE)
cat(paste("------------------------------------------------\n",sep=""))
cat(paste("------------------------------------------------\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("zip all idf, html, scorecards and epw files into a single giant file......\n",sep=""))
cat(paste("zip all idf, html, scorecards and epw files into a single giant file......\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("------------------------------------------------\n",sep=""))
cat(paste("------------------------------------------------\n",sep=""),file=FL.LOG,append=TRUE)

zip.all <- paste(Path.target,paste("all.zip",sep=""),sep="/")
if (file.exists(zip.all)){print(paste(zip.all,"exist.Delete it!"));file.remove(zip.all)}
cat(paste("\n\ndefined a zip file name [",zip.all,"] for weather files......\n",sep=""))
cat(paste("\n\ndefined a zip file name [",zip.all,"] for weather files......\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------
# switch to the target directory before zipping to avoid paths
# -------------------------------------------------------------------------				
setwd(Path.target)

# zip the files for current standard of current prototype		
system(paste("zip ",zip.all,paste(shQuote(file.all),collapse=" ")))
cat(paste("all idf, html, scorecard and epw files of all prototypes across all srtandards have been zipped to [",zip.all,"]\n",sep=""))
cat(paste("all idf, html, scorecard and epw files of all prototypes across all srtandards have been zipped to [",zip.all,"]\n",sep=""),file=FL.LOG,append=TRUE)

setwd(Path.Current)



# ---------------------------------------------------------------------------------------------------------
# 5. standard-wise zip file (each consists of 16 by 34 = 544 files
# Loop of standards (create zip file in terms of standards, i.e., for each standard there is a zip file for each prototype, make it total 3 by 16 = 48 zipped files)
# ---------------------------------------------------------------------------------------------------------
cat("\n\n")
cat("\n\n",file=FL.LOG,append=TRUE)

cat(paste("zip file for each each standard across all 16 prototypes\n",sep=""))
cat(paste("zip file for each each standard across all 16 prototypes\n",sep=""),file=FL.LOG,append=TRUE)


for (standard in standards)
{
	cat("--------------------------------------\n",file=FL.LOG,append=TRUE)
	cat("              ",standard,"            \n",file=FL.LOG,append=TRUE)
	cat("--------------------------------------\n",file=FL.LOG,append=TRUE)


	# zipped file for current prototype
	zip.thisStand <- paste(Path.target,paste(standard,".zip",sep=""),sep="/")
	if (file.exists(zip.thisStand)){print(paste(zip.thisStand,"exist.Delete it!"));file.remove(zip.thisStand)}
	cat(paste("\n\ndefined a zip file name [",zip.thisStand,"] for (idf) and (html) files of [",standard,"] across all prototypes\n",sep=""))
	cat(paste("\n\ndefined a zip file name [",zip.thisStand,"] for (idf) and (html) files of [",standard,"] across all prototypes\n",sep=""),file=FL.LOG,append=TRUE)	

	
	# -------------------------------------------------------------------------------------------------
	# case name is different for each of the standard
	# -------------------------------------------------------------------------------------------------
	case <- cases[standard]	# part 3 in the case name for each standard	
	cat(paste("gathering files for [",case,"] [",standard,"]\n"))
	cat(paste("gathering files for [",case,"] [",standard,"]\n"),file=FL.LOG,append=TRUE)

	
	# -------------------------------------------------------------------------------------------------
	# archive files under each prototype of current project
	# -------------------------------------------------------------------------------------------------
	idx.prototype <- 0
	for (prototype in prototypes)	# prototype loop
	{
		idx.prototype <- idx.prototype + 1
		
		# -----------------------------------------------------------------------------------------
		# define the source directory of the idf and html file folders
		# -----------------------------------------------------------------------------------------
		Path.idf.source <- paste(Path.Current, prototype,"diffOA","input.nobackup", sep="/")	# taken files from "diffOA"
		Path.htm.source <- paste(Path.Current, prototype,"diffOA","output.nobackup",sep="/")	# taken files from "diffOA"
		

		#
		# a quick check to see the existence of the directory
		#
		if ((file.exists(Path.idf.source)) & (file.exists(Path.htm.source)))		
		{
			# 4 part in a case name
			caseNm.part1 <- case		# case is corresponding to standard
			caseNm.part2 <- prototype	# current prototype
			caseNm.part3 <- standard	# current standard
			caseNm.part4 <- climate.zones	# for all climate zones

			# all the idf and html file name
			idf.thisBldg.thisStand  <- paste(caseNm.part1,"_",caseNm.part2,"_",caseNm.part3,"_",caseNm.part4,".idf",sep="")
			htm.thisBldg.thisStand  <- paste(caseNm.part1,"_",caseNm.part2,"_",caseNm.part3,"_",caseNm.part4,".table.htm",sep="")
			file.thisBldg.thisStand <- c(idf.thisBldg.thisStand,htm.thisBldg.thisStand)	# all the files need to be zipped
			cat(paste("define file name of all idf files and htm files\n",sep=""))
			cat(paste("define file name of all idf files and htm files\n",sep=""),file=FL.LOG,append=TRUE)

			#
			# gather files from all prototypes for each standard
			#
			if (idx.prototype == 1)
			{
				file.thisStand <- file.thisBldg.thisStand
			}else{
				file.thisStand <- c(file.thisStand,file.thisBldg.thisStand)				# accumulated idf and htm file for all three standards for a given prototype 
			}
		}
	}
	
	# -------------------------------------------------------------------------
	# switch to the target directory before zipping to avoid paths
	# -------------------------------------------------------------------------				
	setwd(Path.target)

	# zip the files for current standard 			
	system(paste("zip ",zip.thisStand,paste(shQuote(file.thisStand),collapse=" ")))
	cat(paste("the idf and html files of all prototypes of [",standard,"] have been zipped to [",zip.thisStand,"]\n",sep=""))
	cat(paste("the idf and html files of all prototypes of [",standard,"] have been zipped to [",zip.thisStand,"]\n",sep=""),file=FL.LOG,append=TRUE)
	
	setwd(Path.Current)
	
}
cat(paste("\n\nidf and html files have been zipped and stored in [",Path.Report,"]\n\n",sep=""))
cat(paste("\n\nidf and html files have been zipped and stored in [",Path.Report,"]\n\n",sep=""),file=FL.LOG,append=TRUE)





#
# 6. move the files to the separate folders
#
setwd(Path.target)
system(paste("mv *.idf", Path.idf,sep=""))
system(paste("mv *.htm", Path.htm,sep=""))
system(paste("mv *.xlsx",Path.sc,sep=""))
system(paste("mv *.epw", Path.epw,sep=""))
cat(paste("\n\nfiles are moved to the folders for raw files\n\n",sep=""))
cat(paste("\n\nfiles are moved to the folders for raw files\n\n",sep=""),file=FL.LOG,append=TRUE)

setwd(Path.Current)


# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\ngather_files_for_webPosting.R is finished successfully at ",End.time,"]!\n",sep=" "))
cat(paste("\ngather_files_for_webPosting.R is finished successfully at ",End.time,"]!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [gather_files_for_webPosting.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [gather_files_for_webPosting.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)
