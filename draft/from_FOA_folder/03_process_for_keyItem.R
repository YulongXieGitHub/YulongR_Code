#
# 03_process_for_keyItem.R 
#
# In Windows: need to invoke R i386 not X64
#
# A note in preparing Mark's mapping between code item and short description into a csv file
# The existing special character in this file prohibits the csv file directly saved from excel being read correctly in R.
# Therefore, a double qutation marks are added for the cell before save from excel.
# The workable way to do this is:
#
#    1. High­light all columns in the files you want to add the quotes.
#    2. Right click and select: "Format Cells...." from the drop-down list go to Tab "Number" and then select "Custom" down at the bottom of the drop-down list.
#    3. Paste the following into the Type field: "''"@"''" (which is double quote, single quote, single quote, double quote, @ symbol, double quote, single quote, single quote, double quote.)
#    4. Click “okay”
#
#    map state to climate city:
#    i) PA --> 		         | ii) MD --> 		   | iii) NC -->              | iv) AL --> 2A Houston | v) GA --> 2A Houston   | vi) AR --> 	          | vii)TX --> 2A Houston     | vii) KY --> 		          | (3) 
#				 | 			   | 			      | 		      | 	               |  		          |            2B Phoenix     | 				  | (1)
#		  	   	 | 	   		   | 	         3A Memphis   | 	   3A Memphis | 	  3A Memphis   | 	    3A Memphis	  |            3A Memphis     | 				  | (5)
#				 | 			   | 			      | 		      | 		       |  			  |            3B El Paso     | 				  | (1)
#		  4A Baltimore	 | 	      4A Baltimore | 		 4A Baltimore | 		      | 	  4A Baltimore | 	    4A Baltimore  |                           | 		    4A Baltimore  | (6)
#		          	 | 			   | 			      | 		      | 		       | 			  | 	       4B Albuquerque | 				  | (1)
#		  5A Chicago	 | 	      5A Chicago   | 		 5A Chicago   | 		      | 		       | 			  | 			      | 				  | (3)
#		  6A Burlington	 | 			   | 			      | 		      | 		       | 			  | 			      | 				  | (1)
#
# 
# April 27, 2015: add (1) the short desription and 
#                     (2) the range of the data together with the code item
#                     (3) IECC 2015 requirement for NC
#
# Created on April 13, 2015
#
# QA will be more state-specific.  Should generate plots for state specific
#
# Query the FOA RCD database "checkweb_foa_04072015"
#
# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
#
# 	eliminate all stuff
# 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# close all devices which have been opened
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}


# today's month, day and year
month <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
day   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
year  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]

hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]

# lis of states in the FOA
array.states <- c("MD","AllData","AL","AR","GA","KY","NC","PA","TX")
# array.states <- c("NC")

#
# define the directory
#
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "/phome/resstd/FOA/Phase1_Analysis/RCD_Analysis/0_scripts"
	Path.Project <- "/phome/resstd/FOA/Phase1_Analysis/RCD_Analysis"
}else{
	Path.Current <- "Y:/FOA/Phase1_Analysis/RCD_Analysis/0_scripts"
	Path.Project <- "Y:/FOA/Phase1_Analysis/RCD_Analysis"
}

setwd(Path.Current)

# load packages
library("RODBC")
library(ggplot2)
library(reshape2)

# -------------------------------------------------------------------------------------------------
# Select data from corresponding downloading
# *********************************************************************************************************
timeStamp.string <- "2015Jul06"		# this needs to be manually inputted based on the retrieval with "01_extraction_for_QA.R".

# -------------------------------------------------------------------------------------------------
# 1. setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.Data        <- paste(Path.Project,"01_extraction_for_QA",timeStamp.string,sep="/")
Path.Tmp         <- paste(Path.Project,"03_process_for_keyItem",               sep="/")			
Path.Out         <- paste(Path.Tmp,timeStamp.string,                           sep="/")			
Path.log         <- paste(Path.Project,"00_log",                               sep="/")			
Path.auxi        <- paste(Path.Project,"00_auxi_files",                        sep="/")	
if (!file.exists(Path.Data)){print(paste("NOT existing:",Path.Data," Check Why!",sep=""));die}
if (!file.exists(Path.Tmp)) {print(paste("NOT existing:",Path.Tmp,sep=""));dir.create(Path.Tmp,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out)) {print(paste("NOT existing:",Path.Out,sep=""));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)) {print(paste("NOT existing:",Path.log,sep=""));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.auxi)){print(paste("NOT existing:",Path.auxi," Check Why!",sep=""));die}

FL.LOG <- paste(Path.log,  paste("03_process_for_keyItem_",timeStamp.string,".log",sep=""),sep="/")	
FL.MAP <- paste(Path.auxi,"MapTable.csv",sep="/")								# a map between code item and its short description as well as the range if it is a numeric item
if(file.exists(FL.LOG)){print(paste(FL.LOG," exist. Delete it!"));file.remove(FL.LOG)}
if(!(file.exists(FL.MAP))){print(paste(FL.MAP," does not exist. Check why!"));die}
cat(paste("1. Setup paths and files.\n",sep=""))
cat(paste("1. Setup paths and files.\n",sep=""),file=FL.LOG, append=TRUE)								



cat(paste("Log file for data processing script [03_process_for_keyItem.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
            "*                    [03_process_for_keyItem.R]                                *",
            "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)

# list of key code items
list.keyItem <- c("FI1","FI17","FI4","FI4b","FI6","FO1","FO4a","FO4b","FO7a","FO7b","FR10a","FR10b","FR2","FR3","IN1a","IN1b","IN3a","IN3b")

        list.keyItem  <- c("FI1",     "FI17", "FI4",          "FI4b",        "FI6",         "FO1",      "FO4a",             "FO4b",           "FO7a",              "FO7b",            "FR10a",            "FR10b",          "FR2",    "FR3",       "IN1a",          "IN1b",        "IN3a",              "IN3b",            "IQ1",           "IQ2",            "IQ3",           "IQ4",               "MIQ1",               "CSIQ1",                   "KW5",         "KW1",              "KW2",            "BG17")
        name.keyItem  <- c("CeilingR","ACH50","DuctTightness","DuctLeageExt","HighEffLamps","SlabEdgeR","BsmtWallR(Cavity)","BsmtWallR(Cont)","CrawlWallR(Cavity)","CrawlWallR(Cont)","MassWallR(Cavity)","MassWallR(Cont)","WindowU","WindowSHGC","FloorR(Cavity)","FloorR(Cont)","FrameWallR(Cavity)","FrameWallR(Cont)","IQ(RoofCavity)","IQ(FloorCavity)","IQ(WallCavity)","IQ(BsmtWallCavity)","IQ(MassWallCavity)", "IQ(CrawlSpaceWallCavity)","IQ(KneeWall)","KneeWallR(Cavity)","KneeWallR(Cont)","PredominantFoundation")
  names(name.keyItem) <- list.keyItem



# =================================================================================================
# input the map between code item and its short description
# =================================================================================================
myMap <- read.table(file=FL.MAP,header=TRUE,sep=",",stringsAsFactors=FALSE)
row.names(myMap) <- myMap[,"ID"]
cat(paste("2A. a map between code item and its short description as well as rnage for numeric item is loaded.\n",sep=""))
cat(paste("2A. a map between code item and its short description as well as rnage for numeric item is loaded.\n",sep=""),file=FL.LOG, append=TRUE)								


# loopping through the state and all state data
cat(paste("2B. loopping through states.\n",sep=""))
cat(paste("2B. loopping through states.\n",sep=""),file=FL.LOG, append=TRUE)								

for (this.state in array.states)
{
	#
	# A. specify the RDS file name for this state
	#
	if (this.state == "AllData")
	{
		string.thisState <- paste("[",this.state,"]",sep="")
		FL.IN.RDS         <- paste(Path.Data,paste(paste("AllData",timeStamp.string,sep="_"),".rds",             sep=""),sep="/")
		FL.OUT.RDS        <- paste(Path.Out ,paste(paste("AllData",timeStamp.string,sep="_"),".rds",             sep=""),sep="/")
		FL.OUT.CSV        <- paste(Path.Out ,paste(paste("AllData",timeStamp.string,sep="_"),".csv",             sep=""),sep="/")
		FL.SUM.ALL.CSV    <- paste(Path.Out ,paste(paste("AllData",timeStamp.string,sep="_"),"_sum_allItem.csv", sep=""),sep="/")
		FL.SUM.KEY.CSV    <- paste(Path.Out ,paste(paste("AllData",timeStamp.string,sep="_"),"_sum_keyitems.csv",sep=""),sep="/")
		FL.ValueField.PDF <- paste(Path.Out ,paste(paste("AllData",timeStamp.string,sep="_"),"_ValueField.pdf",  sep=""),sep="/")
		FL.OtherField.PDF <- paste(Path.Out ,paste(paste("AllData",timeStamp.string,sep="_"),"_OtherField.pdf",  sep=""),sep="/")
	}else{
		string.thisState <- paste("State [",this.state,"]",sep="")
		FL.IN.RDS         <- paste(Path.Data,paste(paste("State",this.state,timeStamp.string,sep="_"),".rds",             sep=""),sep="/")
		FL.OUT.RDS        <- paste(Path.Out ,paste(paste("State",this.state,timeStamp.string,sep="_"),".rds",             sep=""),sep="/")
		FL.OUT.CSV        <- paste(Path.Out ,paste(paste("State",this.state,timeStamp.string,sep="_"),".csv",             sep=""),sep="/")
		FL.SUM.ALL.CSV    <- paste(Path.Out ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_sum_allItem.csv", sep=""),sep="/")
		FL.SUM.KEY.CSV    <- paste(Path.Out ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_sum_keyitems.csv",sep=""),sep="/")
		FL.ValueField.PDF <- paste(Path.Out ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_ValueField.pdf",  sep=""),sep="/")
		FL.OtherField.PDF <- paste(Path.Out ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_OtherField.pdf",  sep=""),sep="/")	
	}
	if (!file.exists(FL.IN.RDS))        {print(paste("NOT existing:",FL.IN.RDS," Check Why!",sep=""));die}	
	if  (file.exists(FL.OUT.RDS))       {print(paste(FL.OUT.RDS, " exist. Delete it!"));file.remove(FL.OUT.RDS)}
	if  (file.exists(FL.OUT.CSV))       {print(paste(FL.OUT.CSV, " exist. Delete it!"));file.remove(FL.OUT.CSV)}
	if  (file.exists(FL.SUM.ALL.CSV))   {print(paste(FL.SUM.ALL.CSV, " exist. Delete it!"));file.remove(FL.SUM.ALL.CSV)}
	if  (file.exists(FL.SUM.KEY.CSV))   {print(paste(FL.SUM.KEY.CSV, " exist. Delete it!"));file.remove(FL.SUM.KEY.CSV)}
	if  (file.exists(FL.ValueField.PDF)){print(paste(FL.ValueField.PDF, " exist. Delete it!"));file.remove(FL.ValueField.PDF)}
	if  (file.exists(FL.OtherField.PDF)){print(paste(FL.OtherField.PDF, " exist. Delete it!"));file.remove(FL.OtherField.PDF)}
	cat(paste("2. ",string.thisState,": Specify the file name for this state.\n",sep=""))
	cat(paste("2. ",string.thisState,": Specify the file name for this state.\n",sep=""),file=FL.LOG, append=TRUE)								

	#
	# open pdf file
	#
	pdf(file = FL.ValueField.PDF,paper="special", width=17, height=11,bg = "transparent")	# device 2
	pdf(file = FL.OtherField.PDF,paper="special", width=17, height=11,bg = "transparent")	# device 3
	cat(paste("3. ",string.thisState,": open [",FL.ValueField.PDF,"] and [",FL.OtherField.PDF,"] for plots.\n",sep=""))
	cat(paste("3. ",string.thisState,": open [",FL.ValueField.PDF,"] and [",FL.OtherField.PDF,"] for plots.\n",sep=""),file=FL.LOG, append=TRUE)		

	# =================================================================================================
	# B. load the RDS data for this state
	# =================================================================================================
	myData.thisState <- readRDS(file=FL.IN.RDS)
	cat(paste("4. ",string.thisState,": data has been read from [",FL.IN.RDS,"] into [myData.thisState].\n",sep=""))
	cat(paste("4. ",string.thisState,": data has been read from [",FL.IN.RDS,"] into [myData.thisState].\n",sep=""),file=FL.LOG, append=TRUE)								

	# summarize data
	cat(paste("Data Overview at ",string.thisState,"\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
	cat(paste("",dim(myData.thisState)[1],", total records in the database retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
	
	cat(paste("Data Overview at ",string.thisState,"\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
	cat(paste("",dim(myData.thisState)[1],", total records in the database retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
	mySum.tmp1 <- summary(myData.thisState)
	cat(paste(",",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
	write.table(mySum.tmp1,,file=FL.SUM.ALL.CSV,sep=",",append=TRUE)
	cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
	cat(paste("4A. ",string.thisState,": summary of [myData.thisState].\n",sep=""))
	cat(paste("4A. ",string.thisState,": summary of [myData.thisState].\n",sep=""),file=FL.LOG, append=TRUE)								
	
	
	if (dim(myData.thisState)[1] > 0)
	{
		# add customezed cleaning
		myData.thisState[grep("Heat Pum\\[\\\\\\p",myData.thisState[,"PV_codeitem_value"]),"PV_codeitem_value"] <- "Heat Pump"

		# May 8, 2015: also clean some newly found problems!!!!!!!!!!!!!!!
		myData.thisState[,"CIA_codeitem_comments"] <- gsub("\""," inch",myData.thisState[,"CIA_codeitem_comments"])	# double quotation sign replaced with inch
		myData.thisState[,"CIA_codeitem_comments"] <- gsub("'", " ",    myData.thisState[,"CIA_codeitem_comments"])	# single quotation sign replaced with a space
		myData.thisState[,"CIA_codeitem_comments"] <- gsub("@", " at",  myData.thisState[,"CIA_codeitem_comments"])	# @                sign replaced with at
		myData.thisState[,"CIA_codeitem_comments"] <- gsub("\\\\", " ", myData.thisState[,"CIA_codeitem_comments"])	# backslash        sign replaced with a space (note: if there is a single backslash in the database, the retrieved data will have double backslash to escape it.  Therefore we need to replace two backslashes.
		cat(paste("4B. ",string.thisState,": done some cleaning on [myData.thisState].\n",sep=""))
		cat(paste("4B. ",string.thisState,": done some cleaning on [myData.thisState].\n",sep=""),file=FL.LOG, append=TRUE)		

		#
		# convert "1" to "I" for the insulation quality field: eh., there is a "11" in "KW5" ID 8954
		#	
		myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "IQ1",  "PV_codeitem_value"] <- gsub("1","I",gsub("2","II",gsub("3","III",myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "IQ1",  "PV_codeitem_value"])))
		myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "IQ2",  "PV_codeitem_value"] <- gsub("1","I",gsub("2","II",gsub("3","III",myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "IQ2",  "PV_codeitem_value"])))
		myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "IQ3",  "PV_codeitem_value"] <- gsub("1","I",gsub("2","II",gsub("3","III",myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "IQ3",  "PV_codeitem_value"])))
		myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "IQ4",  "PV_codeitem_value"] <- gsub("1","I",gsub("2","II",gsub("3","III",myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "IQ4",  "PV_codeitem_value"])))
		myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "CSIQ1","PV_codeitem_value"] <- gsub("1","I",gsub("2","II",gsub("3","III",myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "CSIQ1","PV_codeitem_value"])))
		myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "MIQ1", "PV_codeitem_value"] <- gsub("1","I",gsub("2","II",gsub("3","III",myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "MIQ1", "PV_codeitem_value"])))
		myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "KW5",  "PV_codeitem_value"] <- gsub("1","I",gsub("2","II",gsub("3","III",myData.thisState[myData.thisState[,"CIA_codeitem_code"] == "KW5",  "PV_codeitem_value"])))
		cat(paste("4C. ",string.thisState,": replace number with Roman character for the insulation quality code items.\n",sep=""))
		cat(paste("4C. ",string.thisState,": replace number with Roman character for the insulation quality code items.\n",sep=""),file=FL.LOG, append=TRUE)	

		#
		# May 13, 2015: there are leading and tailing space in the "county name" field.
		#
		myData.thisState[,"SPL_county"]        <- gsub("\\s*$","",gsub("^\\s*","",myData.thisState[,"SPL_county"]))		# removing the leading and tailing space
		myData.thisState[,"SPL_bldg_name"]     <- gsub("\\s*$","",gsub("^\\s*","",myData.thisState[,"SPL_bldg_name"]))		# removing the leading and tailing space
		myData.thisState[,"PV_codeitem_value"] <- gsub("\\s*$","",gsub("^\\s*","",myData.thisState[,"PV_codeitem_value"]))	# removing the leading and tailing space (like [AL] - [IQ1] show two "II" and "II"
		cat(paste("4D. ",string.thisState,": remove the leading and tailing space in the [County] and [Bldg] field.\n",sep=""))
		cat(paste("4D. ",string.thisState,": remove the leading and tailing space in the [County] and [Bldg] field.\n",sep=""),file=FL.LOG, append=TRUE)	

		
		


		# -------------------------------------------------------------------------------------------------
		# clean the data (C) keep only non-NA data in the "PV_codeitem_value" field
		#                (D) standardize the CZ
		#                (E) Change the lower case to upper case
		# -------------------------------------------------------------------------------------------------	
		#
		# C. Only keep the entries with non-NA in the "PV_codeitem_value" field
		#
		myData.nonNA <- myData.thisState[!(is.na(myData.thisState[,"PV_codeitem_value"])),]			# keep only non NA entries in the code item value field
		myData.nonNA <- myData.nonNA[myData.nonNA[,"PV_codeitem_value"] != "",]					# remove "" entries		
		myData.nonNA <- myData.nonNA[myData.nonNA[,"PV_codeitem_value"] != "NA",]				# remove "" entries
		myData.nonNA <- myData.nonNA[grep("^\\s$",myData.nonNA[,"PV_codeitem_value"],perl=TRUE,invert=TRUE),]	# remove " " entries
		cat(paste("5. ",string.thisState,": only entries with non-NA values in the [PV_codeitem_value] field are kept in [myData.nonNA].\n",sep=""))
		cat(paste("5. ",string.thisState,": only entries with non-NA values in the [PV_codeitem_value] field are kept in [myData.nonNA].\n",sep=""),file=FL.LOG, append=TRUE)		





		if (this.state == "AllData")
		{
			myData.clean <- subset(myData.nonNA,subset = (SPL_state != "HI"))
			cat(paste("5B. remove Hawaii data then assign [myData.nonNA] to [myData.clean].\n",sep=""))
			cat(paste("5B. remove Hawaii data then assign [myData.nonNA] to [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)	
		}else{
			myData.clean <- myData.nonNA	
			cat(paste("5C. assign [myData.nonNA] to [myData.clean].\n",sep=""))
			cat(paste("5C. assign [myData.nonNA] to [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)			
		}
		cat(paste("6. ",string.thisState,": Assign [myData.nonNA] to [myData.clean].\n",sep=""))
		cat(paste("6. ",string.thisState,": Assign [myData.nonNA] to [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)		

		# summarize data
		cat(paste("",dim(myData.clean)[1],", non-NA records of the [value] field in the [PRESCRIPTIVE_VALUES] TABLE retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
		
		cat(paste("",dim(myData.clean)[1],", non-NA records of the [value] field in the [PRESCRIPTIVE_VALUES] TABLE retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		mySum.tmp2 <- summary(myData.clean)
		cat(paste(",",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		write.table(mySum.tmp2,,file=FL.SUM.ALL.CSV,sep=",",append=TRUE)
		cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		cat(paste("6A. ",string.thisState,": summary of [myData.clean].\n",sep=""))
		cat(paste("6A. ",string.thisState,": summary of [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)								



		#
		# D. change all CZ 2 to 2A, 3 to 3A, 
		#
		myData.clean[,"SPL_CZ"] <- sub("^2$","2A",sub("^3$","3A",sub("^4$","4A",sub("^5$","5A",myData.clean[,"SPL_CZ"]))))
		cat(paste("7. ",string.thisState,": standardize the climate zone term.\n",sep=""))
		cat(paste("7. ",string.thisState,": standardize the climate zone term.\n",sep=""),file=FL.LOG, append=TRUE)		

		#
		# E. standardize the CZ field
		#
		myData.clean[,"ClimateZone"] <- paste("CZ",toupper(myData.clean[,"SPL_CZ"]),sep="")
		cat(paste("8A. ",string.thisState,": Add CZ infront of CZ.\n",sep=""))
		cat(paste("8A. ",string.thisState,": Add CZ infront of CZ.\n",sep=""),file=FL.LOG, append=TRUE)	



		# ===========================================================================================================================
		# F. keep only the key code items
		# ===========================================================================================================================
		myData.keyCode <- myData.clean[seq(1:dim(myData.clean)[1])[myData.clean[,"CIA_codeitem_code"] %in% list.keyItem],]
		cat(paste("8B. ",string.thisState,": [myData.keyCode] keep only those entries of the key code items in [myData.clean].\n",sep=""))
		cat(paste("8B. ",string.thisState,": [myData.keyCode] keep only those entries of the key code items in [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)	

		# summarize data
		cat(paste("",dim(myData.keyCode)[1],", non-NA records of the [Key Code Items] in the [value] field in the [PRESCRIPTIVE_VALUES] retrieved on [",timeStamp.string,"] for ",string.thisState,"\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
		
		cat(paste("",dim(myData.keyCode)[1],", non-NA records of the [Key Code Items] in the [value] field in the [PRESCRIPTIVE_VALUES] retrieved on [",timeStamp.string,"] for ",string.thisState,"\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		mySum.tmp3 <- summary(myData.keyCode)
		cat(paste(",",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		write.table(mySum.tmp3,,file=FL.SUM.ALL.CSV,sep=",",append=TRUE)
		cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		cat(paste("8C. ",string.thisState,": summary of [myData.keyCode].\n",sep=""))
		cat(paste("8C. ",string.thisState,": summary of [myData.keyCode].\n",sep=""),file=FL.LOG, append=TRUE)								


		cat(paste("\n\n------------- summary based on subset of data of Key Code Items which show non-NA value collected-------------\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)


		# =======================================================================
		# G. save the cleaned data for this state
		# =======================================================================
		cat(paste("Cleaned Data of State (",this.state,"),",sep=""),file=FL.OUT.CSV,append=TRUE)
		write.table(myData.keyCode,file=FL.OUT.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
		saveRDS(myData.keyCode,file=FL.OUT.RDS)
		cat(paste("9. ",string.thisState,": cleaned data have been saved out to [",FL.OUT.CSV,"] and [",FL.OUT.RDS,"].\n",sep=""))
		cat(paste("9. ",string.thisState,": cleaned data have been saved out to [",FL.OUT.CSV,"] and [",FL.OUT.RDS,"].\n",sep=""),file=FL.LOG, append=TRUE)		



		#
		# condition of non-zero data point
		#
		if (dim(myData.keyCode)[1] > 0)
		{
			#
			# loopping through the fields
			#
			index.sum <- 0
			cat(paste("10. ",string.thisState,": loopping through the fields..........\n\n\n",sep=""))
			cat(paste("10. ",string.thisState,": loopping through the fields..........\n\n\n",sep=""),file=FL.LOG, append=TRUE)		
			for (this.field in c("CIA_codeitem_complies","CIA_codeitem_code"))
			{
				# defining string.field
				string.thisField <- paste(string.thisState,"-(",this.field,")",sep="")		
				cat(paste("11. ",string.thisField,": processing data in this field.\n",sep=""))
				cat(paste("11. ",string.thisField,": processing data in this field.\n",sep=""),file=FL.LOG, append=TRUE)		

				# **********************************************************************
				if (this.field == "CIA_codeitem_code")
				{
					dev.set(2)
					cat(paste("12. ",string.thisField,": loopping through each unqiue code item.\n",sep=""))
					cat(paste("12. ",string.thisField,": loopping through each unqiue code item.\n",sep=""),file=FL.LOG, append=TRUE)		

					# ----------------------------------------------------------------------------
					# processing the "CIA_codeitem_code" itself
					# ----------------------------------------------------------------------------
					myData.keyCode.lvldropped <- droplevels(myData.keyCode)
					cat(paste("12A. ",string.thisField,": dropped the unused levels.\n",sep=""))
					cat(paste("12A. ",string.thisField,": dropped the unused levels.\n",sep=""),file=FL.LOG, append=TRUE)		


					# create summary table
					# ---------------------------------------------------------------------------------------------------------------------
					if(exists("sum.by.State")){rm(sum.by.State)}
					if(exists("sum.by.State")){rm(sum.by.State)}
					if(exists("sum.by.County")){rm(sum.by.County)}
					if(exists("sum.by.Bldg")){rm(sum.by.Bldg)}
					if(exists("sum.by.Complie")){rm(sum.by.Complie)}
					if(exists("sum.by.Comment")){rm(sum.by.Comment)}
					cat(paste("12B. ",string.thisField,": remove the summary data frames if they exist from previous calculation.\n",sep=""))
					cat(paste("12B. ",string.thisField,": remove the summary data frames if they exist from previous calculation.\n",sep=""),file=FL.LOG, append=TRUE)									

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"SPL_state"]),length)
					names(myTmp)   <- c("thisFieldItem","State","Count")
					if (dim(myTmp)[1]>0){
						sum.by.State   <- dcast(myTmp,State~thisFieldItem,value.var = "Count")
					}
					cat(paste("13A. ",string.thisField,": plotting.\n",sep=""))
					cat(paste("13A. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"ClimateZone"]),length)
					names(myTmp)   <- c("thisFieldItem","CZ","Count")
					if (dim(myTmp)[1]>0){
						sum.by.CZ      <- dcast(myTmp,CZ~thisFieldItem,value.var = "Count")
					}
					cat(paste("13B. ",string.thisField,": plotting.\n",sep=""))
					cat(paste("13B. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"SPL_county"]),length)
					names(myTmp)   <- c("thisFieldItem","County","Count")
					if (dim(myTmp)[1]>0){
						sum.by.County  <- dcast(myTmp,County~thisFieldItem,value.var = "Count")
					}
					cat(paste("13C. ",string.thisField,": plotting.\n",sep=""))
					cat(paste("13C. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"SPL_bldg_name"]),length)
					names(myTmp)   <- c("thisFieldItem","BldgName","Count")
					if (dim(myTmp)[1]>0){
						sum.by.Bldg    <- dcast(myTmp,BldgName~thisFieldItem,value.var = "Count")
					}
					cat(paste("13D. ",string.thisField,": plotting.\n",sep=""))
					cat(paste("13D. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"CIA_codeitem_complies"]),length)
					names(myTmp)   <- c("thisFieldItem","CodeComplies","Count")
					if (dim(myTmp)[1]>0){
						sum.by.Complie <- dcast(myTmp,CodeComplies~thisFieldItem,value.var = "Count")
					}
					cat(paste("13E. ",string.thisField,": plotting.\n",sep=""))
					cat(paste("13E. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"CIA_codeitem_comments"]),length)
					names(myTmp)   <- c("thisFieldItem","CodeComments","Count")
					if (dim(myTmp)[1]>0){
						sum.by.Comment <- dcast(myTmp,CodeComments~thisFieldItem,value.var = "Count")
					}
					cat(paste("13F. ",string.thisField,": plotting.\n",sep=""))
					cat(paste("13F. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

					# sum.by.State   <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_state"],            summary))
					# sum.by.CZ      <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"ClimateZone"],          summary))
					# sum.by.County  <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_county"],           summary))
					# sum.by.Bldg    <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_bldg_name"],        summary))
					# sum.by.Complie <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"CIA_codeitem_complies"],summary,na.rm=TRUE))
					cat(paste("13. ",string.thisField,": summarize.\n",sep=""))
					cat(paste("13. ",string.thisField,": summarize.\n",sep=""),file=FL.LOG, append=TRUE)									

					index.sum <- index.sum + 1
					cat(paste("\n (code item index:",index.sum,"):",string.thisField,"\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.State")){
						cat(paste("By State,",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
						write.table(sum.by.State,file=FL.SUM.KEY.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
						
						cat(paste("By State,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
						write.table(sum.by.State,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)						
					}
					cat(paste("\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.CZ")){
						cat(paste("By CZ,",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
						write.table(sum.by.CZ,file=FL.SUM.KEY.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
						
						cat(paste("By CZ,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
						write.table(sum.by.CZ,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					}
					cat(paste("\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.County")){
						cat(paste("By County,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
						write.table(sum.by.County,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					}
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.Bldg")){
						cat(paste("By Bldg,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
						write.table(sum.by.Bldg,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					}
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.Complie")){
						cat(paste("By Complie,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)						
						write.table(sum.by.Complie,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)	
					}
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.Comment")){
						cat(paste("By Comment,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)						
						write.table(sum.by.Comment,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)										
					}
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
					cat(paste("14. ",string.thisField,": summary has been outputted into [",FL.SUM.ALL.CSV,".\n",sep=""))
					cat(paste("14. ",string.thisField,": summary has been outputted into [",FL.SUM.ALL.CSV,".\n",sep=""),file=FL.LOG, append=TRUE)									

					cat(paste("***************************************************************************************************\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
					# ---------------------------------------------------------------------------------------------------------------------


					### # create summary table
					### sum.by.State   <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_state"],            summary))
					### sum.by.CZ      <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_CZ"],               summary))
					### sum.by.County  <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_county"],           summary))
					### sum.by.Bldg    <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_bldg_name"],        summary))
					### sum.by.Complie <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"CIA_codeitem_complies"],summary))
					### cat(paste("12B. ",string.thisField,": summarize.\n",sep=""))
					### cat(paste("12B. ",string.thisField,": summarize.\n",sep=""),file=FL.LOG, append=TRUE)									
					### 
					### cat(paste("\n** ",string.thisField," **\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
					### 
					### cat(paste("By State,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
					### write.table(sum.by.State,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					### 
					### cat(paste("By CZ,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
					### write.table(sum.by.CZ,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					### 
					### cat(paste("By County,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
					### write.table(sum.by.County,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					### 
					### cat(paste("By Bldg,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
					### write.table(sum.by.Bldg,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					### 
					### cat(paste("By Complie,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)						
					### write.table(sum.by.Complie,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					### cat(paste("*******************************************\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
					### cat(paste("12C. ",string.thisField,": output the summary.\n",sep=""))
					### cat(paste("12C. ",string.thisField,": output the summary.\n",sep=""),file=FL.LOG, append=TRUE)									



					# ----------------------------------------------------------------------------
					# ----------------------------------------------------------------------------
					# ----------------------------------------------------------------------------
					# Loopping through each individual code item in "CIA_codeitem_code" field
					# ----------------------------------------------------------------------------
					# ----------------------------------------------------------------------------
					# ----------------------------------------------------------------------------
					list.codeItem <- sort(unique(myData.keyCode[,"CIA_codeitem_code"]))
					count <- 0

					for (this.codeItem in list.codeItem)	
					{
						count    <- count + 1

						# find the corresponding short description and range if it is a numeric code item
						idx.matched <- match(this.codeItem,myMap[,"ID"])
						this.codeItem.desp <- myMap[idx.matched,"Short.Name"]
						this.codeItem.UL   <- myMap[idx.matched,"Upper.Bound"]
						this.codeItem.LL   <- myMap[idx.matched,"Lower.Bound"]
						if (is.na(this.codeItem.UL) | is.na(this.codeItem.LL))
						{
							string.thisCodeitem <- paste(string.thisField,"-CodeItem-",count,": ",this.codeItem,":(",this.codeItem.desp,")",sep="")
							string.thisCodeitem <- paste("[",this.state,"] CodeItem: (",this.codeItem,")-(",this.codeItem.desp,")",sep="")
						}else{
							string.thisCodeitem <- paste(string.thisField,"-CodeItem-",count,": ",this.codeItem,":(",this.codeItem.desp,")",sep="")
							string.thisCodeitem <- paste("[",this.state,"] CodeItem: (",this.codeItem,")-(",this.codeItem.desp,")",sep="")
						}
						cat(paste("15. ",string.thisCodeitem,": get the description of this code item and its range if it is a numeric.\n",sep=""))
						cat(paste("15. ",string.thisCodeitem,": get the description of this code item and its range if it is a numeric.\n",sep=""),file=FL.LOG, append=TRUE)								


						# subsetting data
						mySubset  <- subset(myData.keyCode,subset = (CIA_codeitem_code == this.codeItem))
						no.data   <- dim(mySubset)[1] 
					      # no.Subset <- dim(mySubset)[1]
						no.Subset <- sum(!(is.na(mySubset[,"PV_codeitem_value"])))
								
						mySubset[,"UL"] <- this.codeItem.UL
						mySubset[,"LL"] <- this.codeItem.LL
						cat(paste("16. ",string.thisCodeitem,": subsetting data for this code item from [myData.clean].\n",sep=""))
						cat(paste("16. ",string.thisCodeitem,": subsetting data for this code item from [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)								


						# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
						# for plot title purpose, duplicate some field
						# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
						mySubset[,"State"]         <- mySubset[,"SPL_state"]
						mySubset[,"Building"]      <- mySubset[,"SPL_bldg_name"]
						mySubset[,"County"]        <- mySubset[,"SPL_county"]
						mySubset[,"CodeItemValue"] <- mySubset[,"PV_codeitem_value"]
						cat(paste("16B. ",string.thisCodeitem,": duplicate some field in t[mySubset\ for better plot legend title purpose.\n",sep=""))
						cat(paste("16B. ",string.thisCodeitem,": duplicate some field in t[mySubset\ for better plot legend title purpose.\n",sep=""),file=FL.LOG, append=TRUE)								
						
						
						

						# drop the unused levels
						mySubset <- droplevels(mySubset)
						cat(paste("17. ",string.thisCodeitem,": drop the unexisting levels in [mySubset].\n",sep=""))
						cat(paste("17. ",string.thisCodeitem,": drop the unexisting levels in [mySubset].\n",sep=""),file=FL.LOG, append=TRUE)								



						if (no.data >= 1)
						{
							# note there are something like "1.1.15" which is not a numeric field although there are only dot and numbers.  The second part i sused to match this
							#
							# Categorical Field
							#
							if ((length(grep("[^0-9\\.]",mySubset[,"CodeItemValue"]))) | (length(grep("(\\d*\\.+)(\\d*\\.+)(.*)$",mySubset[,"CodeItemValue"]))))	# if any non-numeric value has been found in the CodeItemValue field, treat it as a character field otherwise a numeric field
							{
								# convert all lower case to upper case for this character field
								mySubset[,"CodeItemValue"] <- toupper(mySubset[,"CodeItemValue"])
								cat(paste("18. ",string.thisCodeitem,": change lower case to upper case.\n",sep=""))
								cat(paste("18. ",string.thisCodeitem,": change lower case to upper case.\n",sep=""),file=FL.LOG, append=TRUE)									

								# summarize data
								cat(paste("",dim(mySubset)[1],", ",string.thisCodeitem," with non-NA records in the [value] field in the [PRESCRIPTIVE_VALUES] retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)

								cat(paste("",dim(mySubset)[1],", ",string.thisCodeitem," with non-NA records in the [value] field in the [PRESCRIPTIVE_VALUES] retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
								mySum.tmp4 <- summary(mySubset)
								cat(paste(",",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
								write.table(mySum.tmp4,,file=FL.SUM.ALL.CSV,sep=",",append=TRUE)
								cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
								cat(paste("18B. ",string.thisState,": summary of [mySubset].\n",sep=""))
								cat(paste("18B. ",string.thisState,": summary of [mySubset].\n",sep=""),file=FL.LOG, append=TRUE)								




								# ---------------------------------------------------------------------------------------------------------------------
								# create summary table
								if(exists("sum.by.State")){rm(sum.by.State)}
								if(exists("sum.by.State")){rm(sum.by.State)}
								if(exists("sum.by.County")){rm(sum.by.County)}
								if(exists("sum.by.Bldg")){rm(sum.by.Bldg)}
								if(exists("sum.by.Complie")){rm(sum.by.Complie)}
								if(exists("sum.by.Comment")){rm(sum.by.Comment)}
								cat(paste("19. ",string.thisCodeitem,": remove the summary data frames if they exist from previous calculation.\n",sep=""))
								cat(paste("19. ",string.thisCodeitem,": remove the summary data frames if they exist from previous calculation.\n",sep=""),file=FL.LOG, append=TRUE)									


								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"State"]),length)
								names(myTmp)   <- c("thisCodeItem","State","Count")
								if (dim(myTmp)[1]>0){
									sum.by.State   <- dcast(myTmp,State~thisCodeItem,value.var = "Count")
								}
								cat(paste("19A. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("19A. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)									

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"ClimateZone"]),length)
								names(myTmp)   <- c("thisCodeItem","CZ","Count")
								if (dim(myTmp)[1]>0){
									sum.by.CZ      <- dcast(myTmp,CZ~thisCodeItem,value.var = "Count")
								}
								cat(paste("19B. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("19B. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)									

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"County"]),length)
								names(myTmp)   <- c("thisCodeItem","County","Count")
								if (dim(myTmp)[1]>0){
									sum.by.County  <- dcast(myTmp,County~thisCodeItem,value.var = "Count")
								}
								cat(paste("19C. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("19C. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)									

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"Building"]),length)
								names(myTmp)   <- c("thisCodeItem","BldgName","Count")
								if (dim(myTmp)[1]>0){
									sum.by.Bldg    <- dcast(myTmp,BldgName~thisCodeItem,value.var = "Count")
								}
								cat(paste("19D. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("19D. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)									

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"CIA_codeitem_complies"]),length)
								names(myTmp)   <- c("thisCodeItem","CodeComplies","Count")
								if (dim(myTmp)[1]>0){
									sum.by.Complie <- dcast(myTmp,CodeComplies~thisCodeItem,value.var = "Count")
								}
								cat(paste("19E. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("19E. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)									

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"CIA_codeitem_comments"]),length)
								names(myTmp)   <- c("thisCodeItem","CodeComments","Count")
								if (dim(myTmp)[1]>0){
									sum.by.Comment <- dcast(myTmp,CodeComments~thisCodeItem,value.var = "Count")
								}
								cat(paste("19F. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("19F. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)									

								# sum.by.State   <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"State"],            summary))
								# sum.by.CZ      <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"ClimateZone"],          summary))
								# sum.by.County  <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"County"],           summary))
								# sum.by.Bldg    <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"Building"],        summary))
								# sum.by.Complie <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"CIA_codeitem_complies"],summary,na.rm=TRUE))
								cat(paste("19G. ",string.thisCodeitem,": summarize.\n",sep=""))
								cat(paste("19G. ",string.thisCodeitem,": summarize.\n",sep=""),file=FL.LOG, append=TRUE)									

								index.sum <- index.sum + 1
								cat(paste("\n (",index.sum,"):",string.thisCodeitem,"\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.State")){
									cat(paste("By State,",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
									write.table(sum.by.State,file=FL.SUM.KEY.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
									
									cat(paste("By State,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
									write.table(sum.by.State,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
								}
								cat(paste("\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.CZ")){
									cat(paste("By CZ,",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
									write.table(sum.by.CZ,file=FL.SUM.KEY.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
									
									cat(paste("By CZ,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
									write.table(sum.by.CZ,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
								}
								cat(paste("\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.County")){
									cat(paste("By County,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
									write.table(sum.by.County,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
								}
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.Bldg")){
									cat(paste("By Bldg,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
									write.table(sum.by.Bldg,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
								}
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.Complie")){
									cat(paste("By Complie,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)						
									write.table(sum.by.Complie,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)	
								}
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.Comment")){
									cat(paste("By Comment,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)						
									write.table(sum.by.Comment,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)										
								}
								cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
								# ---------------------------------------------------------------------------------------------------------------------
								cat(paste("19H. ",string.thisCodeitem,": output the summary.\n",sep=""))
								cat(paste("19H. ",string.thisCodeitem,": output the summary.\n",sep=""),file=FL.LOG, append=TRUE)									

								no.unique.lvl <- length(unique(mySubset[,"CodeItemValue"])) 
								if (no.unique.lvl >= 10){xtick.size = 5}else{xtick.size = 10}
								cat(paste("19I. ",string.thisCodeitem,": get the unique levels.\n",sep=""))
								cat(paste("19I. ",string.thisCodeitem,": get the unique levels.\n",sep=""),file=FL.LOG, append=TRUE)									
									
									
									
									





								if (this.state == "AllData")
								{
									# bar chart in term of state
									p.plot1 <- ggplot(mySubset, aes(x=factor(1), fill=factor(CodeItemValue))) + geom_bar(width=1) + coord_polar(theta="y") + facet_grid(State ~ .,scales="fixed",space="fixed",as.table=TRUE)
									p.plot1 <- p.plot1 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot1 <- p.plot1 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [State]",sep=""))
									plot(p.plot1)
									cat(paste("19J. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("19J. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

									# bar chart in term of both [state] and [County]			
									p.plot2 <- qplot(data=mySubset,CodeItemValue,facets=State~.,colour=County,fill=County,geom="bar") 
									p.plot2 <- p.plot2 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot2 <- p.plot2 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [County]",sep=""))
							              # p.plot2 <- p.plot2 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")
									plot(p.plot2)
									cat(paste("19K. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("19K. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

									# bar chart in term of both [state] and [climate zone]			
									p.plot3 <- qplot(data=mySubset,CodeItemValue,facets=State~.,colour=ClimateZone,fill=ClimateZone,geom="bar") 
									p.plot3 <- p.plot3 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot3 <- p.plot3 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [State] and [Climate Zone]",sep=""))
							              # p.plot3 <- p.plot3 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")
									plot(p.plot3)
									cat(paste("19L. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("19L. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

									# bar chart in term of both [state] and [Building]			
									p.plot4 <- qplot(data=mySubset,CodeItemValue,facets=State~.,colour=factor(Building),fill=factor(Building),geom="bar") 
									p.plot4 <- p.plot4 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right",legend.text=element_text(colour="black",size=5)) 
									p.plot4 <- p.plot4 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [State] and [Unique Bldg]",sep=""))
							              # p.plot4 <- p.plot4 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")
									plot(p.plot4)
									cat(paste("19M. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("19M. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

								}else{
									# bar chart in term of [county]
									p.plot1 <- ggplot(mySubset, aes(x=factor(1), fill=factor(CodeItemValue))) + geom_bar(width=1) + coord_polar(theta="y")
									p.plot1 <- p.plot1 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot1 <- p.plot1 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations",sep=""))
									plot(p.plot1)
									cat(paste("19N. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("19N. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									


									# bar chart in term of [county]
									p.plot2 <- ggplot(mySubset, aes(x=factor(1), fill=factor(CodeItemValue))) + geom_bar(width=1) + coord_polar(theta="y") + facet_grid(County ~ .,scales="fixed",space="fixed",as.table=TRUE)
									p.plot2 <- p.plot2 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot2 <- p.plot2 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [County]",sep=""))
									plot(p.plot2)
									cat(paste("19O. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("19O. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

									# bar chart in term of [County]			
									p.plot3 <- qplot(data=mySubset,CodeItemValue,facet=County~.,colour=County,fill=County,geom="bar") 
									p.plot3 <- p.plot3 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot3 <- p.plot3 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [County]",sep=""))
							              # p.plot3 <- p.plot3 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")
									plot(p.plot3)
									cat(paste("19P. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("19P. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

									# bar chart in term of [climate zone]			
									p.plot4 <- qplot(data=mySubset,CodeItemValue,colour=ClimateZone,fill=ClimateZone,geom="bar") 
									p.plot4 <- p.plot4 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot4 <- p.plot4 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [Climate Zone]",sep=""))
							               # p.plot4 <- p.plot4 + theme(legend.title = element_text(colour="chocolate",size=14,face="bold")) + scale_color_discrete(name="This Color")
									plot(p.plot4)
									cat(paste("19Q. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("19Q. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

									# bar chart in term of both [state] and [Building]			
									p.plot5 <- qplot(data=mySubset,CodeItemValue,colour=factor(Building),fill=factor(Building),geom="bar") 
									p.plot5 <- p.plot5 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right",legend.text=element_text(colour="black",size=5)) 
									p.plot5 <- p.plot5 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [Unique Bldg]",sep=""))
							              # p.plot5 <- p.plot5 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")
									plot(p.plot5)						
									cat(paste("19R. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("19R. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)									

								}
								cat(paste("19D. ",string.thisCodeitem,": is a character field and has ",dim(mySubset)[1]," data points and has been plotted.\n",sep=""))
								cat(paste("19D. ",string.thisCodeitem,": is a character field and has ",dim(mySubset)[1]," data points and has been plotted.\n",sep=""),file=FL.LOG, append=TRUE)									
							#
							# Numeric Field
							#								
							}else{
								# convert to numeric for this numeric field		
								mySubset[,"CodeItemValue"] <- as.numeric(as.character(mySubset[,"CodeItemValue"]))
								cat(paste("19S. ",string.thisCodeitem,": change lower case to upper case.\n",sep=""))
								cat(paste("19S. ",string.thisCodeitem,": change lower case to upper case.\n",sep=""),file=FL.LOG, append=TRUE)


								# summarize data
								cat(paste("",dim(mySubset)[1],", ",string.thisCodeitem," with non-NA records in the [value] field in the [PRESCRIPTIVE_VALUES] retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)

								cat(paste("",dim(mySubset)[1],", ",string.thisCodeitem," with non-NA records in the [value] field in the [PRESCRIPTIVE_VALUES] retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
								mySum.tmp5 <- summary(mySubset)
								cat(paste(",",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
								write.table(mySum.tmp5,,file=FL.SUM.ALL.CSV,sep=",",append=TRUE)
								cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
								cat(paste("19T. ",string.thisState,": summary of [mySubset].\n",sep=""))
								cat(paste("19T. ",string.thisState,": summary of [mySubset].\n",sep=""),file=FL.LOG, append=TRUE)								




								# ---------------------------------------------------------------------------------------------------------------------
								# create summary table
								if(exists("sum.by.State")){rm(sum.by.State)}
								if(exists("sum.by.State")){rm(sum.by.State)}
								if(exists("sum.by.County")){rm(sum.by.County)}
								if(exists("sum.by.Bldg")){rm(sum.by.Bldg)}
								if(exists("sum.by.Complie")){rm(sum.by.Complie)}
								if(exists("sum.by.Comment")){rm(sum.by.Comment)}
								cat(paste("20A. ",string.thisCodeitem,": remove the summary data frames if they exist from previous calculation.\n",sep=""))
								cat(paste("20A. ",string.thisCodeitem,": remove the summary data frames if they exist from previous calculation.\n",sep=""),file=FL.LOG, append=TRUE)									



								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"State"]),length)
								names(myTmp)   <- c("thisCodeItem","State","Count")
								if (dim(myTmp)[1]>0){
									sum.by.State   <- dcast(myTmp,State~thisCodeItem,value.var = "Count")
								}
								cat(paste("20B. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("20B. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)	

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"ClimateZone"]),length)
								names(myTmp)   <- c("thisCodeItem","CZ","Count")
								if (dim(myTmp)[1]>0){
									sum.by.CZ      <- dcast(myTmp,CZ~thisCodeItem,value.var = "Count")
								}
								cat(paste("20C. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("20C. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)	

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"County"]),length)
								names(myTmp)   <- c("thisCodeItem","County","Count")
								if (dim(myTmp)[1]>0){
									sum.by.County  <- dcast(myTmp,County~thisCodeItem,value.var = "Count")
								}
								cat(paste("20D. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("20D. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)	

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"Building"]),length)
								names(myTmp)   <- c("thisCodeItem","BldgName","Count")
								if (dim(myTmp)[1]>0){
									sum.by.Bldg    <- dcast(myTmp,BldgName~thisCodeItem,value.var = "Count")
								}
								cat(paste("20E. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("20E. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)	

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"CIA_codeitem_complies"]),length)
								names(myTmp)   <- c("thisCodeItem","CodeComplies","Count")
								if (dim(myTmp)[1]>0){
									sum.by.Complie <- dcast(myTmp,CodeComplies~thisCodeItem,value.var = "Count")
								}
								cat(paste("20F. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("20F. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)	

								      myTmp    <- aggregate(mySubset[,"CodeItemValue"],list(mySubset[,"CodeItemValue"],mySubset[,"CIA_codeitem_comments"]),length)
								names(myTmp)   <- c("thisCodeItem","CodeComments","Count")
								if (dim(myTmp)[1]>0){
									sum.by.Comment <- dcast(myTmp,CodeComments~thisCodeItem,value.var = "Count")
								}
								cat(paste("20G. ",string.thisField,": summarizing.\n",sep=""))
								cat(paste("20G. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)	

								# sum.by.State   <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"State"],            summary))
								# sum.by.CZ      <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"ClimateZone"],          summary))
								# sum.by.County  <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"County"],           summary))
								# sum.by.Bldg    <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"Building"],        summary))
								# sum.by.Complie <- do.call(rbind , by(mySubset[,"CodeItemValue"], mySubset[,"CIA_codeitem_complies"],summary,na.rm=TRUE))
								cat(paste("20H. ",string.thisCodeitem,": summarize.\n",sep=""))
								cat(paste("20H. ",string.thisCodeitem,": summarize.\n",sep=""),file=FL.LOG, append=TRUE)									

								index.sum <- index.sum + 1
								cat(paste("\n (",index.sum,"):",string.thisCodeitem,"\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.State")){
									cat(paste("By State,",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
									write.table(sum.by.State,file=FL.SUM.KEY.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
									
									cat(paste("By State,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
									write.table(sum.by.State,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
								}
								cat(paste("\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.CZ")){
									cat(paste("By CZ,",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
									write.table(sum.by.CZ,file=FL.SUM.KEY.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
									
									cat(paste("By CZ,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
									write.table(sum.by.CZ,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
								}
								cat(paste("\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.County")){
									cat(paste("By County,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
									write.table(sum.by.County,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
								}
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.Bldg")){
									cat(paste("By Bldg,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
									write.table(sum.by.Bldg,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
								}
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.Complie")){
									cat(paste("By Complie,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)						
									write.table(sum.by.Complie,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)	
								}
								cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

								if(exists("sum.by.Comment")){
									cat(paste("By Comment,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)						
									write.table(sum.by.Comment,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)										
								}
								cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
								# ---------------------------------------------------------------------------------------------------------------------
								cat(paste("21. ",string.thisCodeitem,": output the summary.\n",sep=""))
								cat(paste("21. ",string.thisCodeitem,": output the summary.\n",sep=""),file=FL.LOG, append=TRUE)									

								no.unique.lvl <- length(unique(mySubset[,"CodeItemValue"])) 
								if (no.unique.lvl >= 10){xtick.size = 5}else{xtick.size = 10}
								cat(paste("22. ",string.thisCodeitem,": get the unique levels.\n",sep=""))
								cat(paste("22. ",string.thisCodeitem,": get the unique levels.\n",sep=""),file=FL.LOG, append=TRUE)									


								if (this.state == "AllData")
								{
									# histogram in term of state
									p.plot1 <- qplot(data=mySubset,CodeItemValue,facets=State~.,colour=State,fill=State,geom="histogram") 
									p.plot1 <- p.plot1 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot1 <- p.plot1 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": Distribution of Code Item Values in term of State and Climate Zone\nThis is a numeric field (blue dash line indicates the plausible range)",sep=""))
									p.plot1 <- p.plot1 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")
									plot(p.plot1)	
									cat(paste("23A. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("23A. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)

									# histogram in term of both state and climate zone
									p.plot2 <- qplot(data=mySubset,CodeItemValue,facets=State~ClimateZone,colour=State,fill=State,geom="histogram") 
									p.plot2 <- p.plot2 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot2 <- p.plot2 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": Distribution of Code Item Values in term of State and Climate Zone\nThis is a numeric field (blue dash line indicates the plausible range)",sep=""))
									p.plot2 <- p.plot2 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")									
									plot(p.plot2)	
									cat(paste("23B. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("23B. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)

								}else{
									# histogram in term of [Climate Zone]
									p.plot1 <- qplot(data=mySubset,CodeItemValue,colour=ClimateZone,fill=ClimateZone,geom="histogram") 
									p.plot1 <- p.plot1 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot1 <- p.plot1 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [Climate Zone]\nBlue dash line indicates the plausible range",sep=""))
									p.plot1 <- p.plot1 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")
									plot(p.plot1)	
									cat(paste("23C. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("23C. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)


									# histogram in term of [county]
									p.plot2 <- qplot(data=mySubset,CodeItemValue,colour=County,fill=County,geom="histogram") 
									p.plot2 <- p.plot2 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot2 <- p.plot2 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [County]\nBlue dash line indicates the plausible range",sep=""))
									p.plot2 <- p.plot2 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")									
									plot(p.plot2)	
									cat(paste("23D. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("23D. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)

									# histogram in term of [unique bldg]
									p.plot3 <- qplot(data=mySubset,CodeItemValue,colour=Building,fill=Building,geom="histogram") 
									p.plot3 <- p.plot3 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
									p.plot3 <- p.plot3 + labs(x=paste(this.codeItem,":(",this.codeItem.desp,")",sep=""),y="count",title=paste(string.thisCodeitem,": ",no.Subset," observations by [Unique Bldg]\nBlue dash line indicates the plausible range",sep=""))
									p.plot3 <- p.plot3 + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")
									plot(p.plot3)	
									cat(paste("23E. ",string.thisField,": plotting.\n",sep=""))
									cat(paste("23E. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)

								}
								cat(paste("36. ",string.thisCodeitem,": is a numeric field and has ",dim(mySubset)[1]," data points and has been plotted.\n",sep=""))
								cat(paste("36. ",string.thisCodeitem,": is a numeric field and has ",dim(mySubset)[1]," data points and has been plotted.\n",sep=""),file=FL.LOG, append=TRUE)					
							}

						}
					}
					cat(paste("37. ",string.thisField,": done.\n\n",sep=""))
					cat(paste("37. ",string.thisField,": done.\n\n",sep=""),file=FL.LOG, append=TRUE)			
				}else if (this.field == "CIA_codeitem_complies")
				{
					dev.set(3)
					cat(paste("41. ",string.thisField,": check the dustribution.\n",sep=""))
					cat(paste("41. ",string.thisField,": check the dustribution.\n",sep=""),file=FL.LOG, append=TRUE)		

					myData.keyCode.lvldropped <- droplevels(myData.keyCode)
					cat(paste("42. ",string.thisField,": dropped the unused levels.\n",sep=""))
					cat(paste("42. ",string.thisField,": dropped the unused levels.\n",sep=""),file=FL.LOG, append=TRUE)		

					# create summary table
					# ---------------------------------------------------------------------------------------------------------------------
					if(exists("sum.by.SPL_state")){rm(sum.by.SPL_state)}
					if(exists("sum.by.SPL_state")){rm(sum.by.SPL_state)}
					if(exists("sum.by.SPL_county")){rm(sum.by.SPL_county)}
					if(exists("sum.by.Bldg")){rm(sum.by.Bldg)}
					if(exists("sum.by.Complie")){rm(sum.by.Complie)}
					if(exists("sum.by.Comment")){rm(sum.by.Comment)}
					cat(paste("43. ",string.thisField,": remove the summary data frames if they exist from previous calculation.\n",sep=""))
					cat(paste("43. ",string.thisField,": remove the summary data frames if they exist from previous calculation.\n",sep=""),file=FL.LOG, append=TRUE)									

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"SPL_state"]),length)
					names(myTmp)   <- c("thisFieldItem","SPL_state","Count")
					if (dim(myTmp)[1]>0){
						sum.by.SPL_state   <- dcast(myTmp,SPL_state~thisFieldItem,value.var = "Count")
					}
					cat(paste("44A. ",string.thisField,": summarizing.\n",sep=""))
					cat(paste("44A. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"ClimateZone"]),length)
					names(myTmp)   <- c("thisFieldItem","CZ","Count")
					if (dim(myTmp)[1]>0){
						sum.by.CZ      <- dcast(myTmp,CZ~thisFieldItem,value.var = "Count")
					}
					cat(paste("44B. ",string.thisField,": summarizing.\n",sep=""))
					cat(paste("44B. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"SPL_county"]),length)
					names(myTmp)   <- c("thisFieldItem","SPL_county","Count")
					if (dim(myTmp)[1]>0){
						sum.by.SPL_county  <- dcast(myTmp,SPL_county~thisFieldItem,value.var = "Count")
					}
					cat(paste("44C. ",string.thisField,": summarizing.\n",sep=""))
					cat(paste("44C. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"SPL_bldg_name"]),length)
					names(myTmp)   <- c("thisFieldItem","BldgName","Count")
					if (dim(myTmp)[1]>0){
						sum.by.Bldg    <- dcast(myTmp,BldgName~thisFieldItem,value.var = "Count")
					}
					cat(paste("44D. ",string.thisField,": summarizing.\n",sep=""))
					cat(paste("44D. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"CIA_codeitem_complies"]),length)
					names(myTmp)   <- c("thisFieldItem","CodeComplies","Count")
					if (dim(myTmp)[1]>0){
						sum.by.Complie <- dcast(myTmp,CodeComplies~thisFieldItem,value.var = "Count")
					}
					cat(paste("44E. ",string.thisField,": summarizing.\n",sep=""))
					cat(paste("44E. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)

					      myTmp    <- aggregate(myData.keyCode.lvldropped[,this.field],list(myData.keyCode.lvldropped[,this.field],myData.keyCode.lvldropped[,"CIA_codeitem_comments"]),length)
					names(myTmp)   <- c("thisFieldItem","CodeComments","Count")
					if (dim(myTmp)[1]>0){
						sum.by.Comment <- dcast(myTmp,CodeComments~thisFieldItem,value.var = "Count")
					}
					cat(paste("44F. ",string.thisField,": summarizing.\n",sep=""))
					cat(paste("44F. ",string.thisField,": summarizing.\n",sep=""),file=FL.LOG, append=TRUE)

					# sum.by.SPL_state   <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_state"],            summary))
					# sum.by.CZ      <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"ClimateZone"],          summary))
					# sum.by.SPL_county  <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_county"],           summary))
					# sum.by.Bldg    <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"SPL_bldg_name"],        summary))
					# sum.by.Complie <- do.call(rbind , by(myData.keyCode.lvldropped[,this.field], myData.keyCode.lvldropped[,"CIA_codeitem_complies"],summary,na.rm=TRUE))
					cat(paste("44G. ",string.thisField,": summarize.\n",sep=""))
					cat(paste("44G. ",string.thisField,": summarize.\n",sep=""),file=FL.LOG, append=TRUE)									

					index.sum <- index.sum + 1
					cat(paste("\n (",index.sum,"):",string.thisField,"\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.SPL_state")){
						cat(paste("By SPL_state,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
						write.table(sum.by.SPL_state,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					}
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.CZ")){
						cat(paste("By CZ,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
						write.table(sum.by.CZ,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					}
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.SPL_county")){
						cat(paste("By SPL_county,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
						write.table(sum.by.SPL_county,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					}
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.Bldg")){
						cat(paste("By Bldg,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
						write.table(sum.by.Bldg,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					}
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.Complie")){
						cat(paste("By Complie,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)						
						write.table(sum.by.Complie,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)	
					}
					cat(paste("\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)

					if(exists("sum.by.Comment")){
						cat(paste("By Comment,",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)						
						write.table(sum.by.Comment,file=FL.SUM.ALL.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)										
					}			
					cat(paste("####################################################################\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
					# ---------------------------------------------------------------------------------------------------------------------
					cat(paste("45. ",string.thisField,": output the summary.\n",sep=""))
					cat(paste("45. ",string.thisField,": output the summary.\n",sep=""),file=FL.LOG, append=TRUE)									






					no.unique.lvl <- length(unique(myData.keyCode[,this.field])) 
					if (no.unique.lvl >= 10){xtick.size = 5}else{xtick.size = 10}	
					cat(paste("46. ",string.thisField,": get the unique levels.\n",sep=""))
					cat(paste("46. ",string.thisField,": get the unique levels.\n",sep=""),file=FL.LOG, append=TRUE)									

					# ------------------------------------------------------------------
					# check the values in "complies" field
					# ------------------------------------------------------------------
					if (this.state == "AllData")
					{			
						# complies response in term of [climate zone] and [state]
						p.plot1 <- ggplot(myData.keyCode, aes(x=factor(1), fill=factor(CIA_codeitem_complies))) + geom_bar(width=1) + coord_polar(theta="y") + facet_grid(ClimateZone ~ SPL_state,scales="fixed",space="fixed",as.table=TRUE)
						p.plot1 <- p.plot1 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot1 <- p.plot1 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone] and [SPL_state]\n",sep=""))
						plot(p.plot1)
						cat(paste("47A. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47A. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)


						# pie chart in term of both state and climate zone			
						 p.plot4 <- qplot(data=myData.keyCode,CIA_codeitem_complies,facets=SPL_state~ClimateZone,colour=SPL_state,fill=SPL_state,geom="bar") 
						 p.plot4 <- p.plot4 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						 p.plot4 <- p.plot4 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone] and [SPL_state]\n",sep=""))
						 plot(p.plot4)
						cat(paste("47B. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47B. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)					 

						# bar chart in terms of [CZ] and [counties]
						p.plot5 <- qplot(data=myData.keyCode,CIA_codeitem_complies,colour=SPL_county,fill=SPL_county,geom="bar",facets=ClimateZone~SPL_state) 
						p.plot5 <- p.plot5 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot5 <- p.plot5 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone] and [SPL_county] and [SPL_state]\n",sep=""))
						plot(p.plot5)
						cat(paste("47C. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47C. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)

						# bar chart in terms of [CZ] and [SPL_bldg_name]
						p.plot6 <- qplot(data=myData.keyCode,CIA_codeitem_complies,colour=SPL_bldg_name,fill=SPL_bldg_name,geom="bar",facets=ClimateZone~.) 
						p.plot6 <- p.plot6 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot6 <- p.plot6 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone] and [Unique Bldg]\n",sep=""))
						plot(p.plot6)
						cat(paste("47D. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47D. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)

						# bar chart in terms of [CZ] and [CIA_codeitem_code]
						p.plot7 <- qplot(data=myData.keyCode,CIA_codeitem_complies,colour=CIA_codeitem_code,fill=CIA_codeitem_code,geom="bar",facets=ClimateZone~.) 
						p.plot7 <- p.plot7 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot7 <- p.plot7 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone] and [Code Item]\n",sep=""))
						plot(p.plot7)
						cat(paste("47E. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47E. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)					
					}else{
						# complies response in term of [climate zone] and [state]
						p.plot1 <- ggplot(myData.keyCode, aes(x=factor(1), fill=factor(CIA_codeitem_complies))) + geom_bar(width=1) + coord_polar(theta="y") + facet_grid(ClimateZone ~ .,scales="fixed",space="fixed",as.table=TRUE)
						p.plot1 <- p.plot1 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot1 <- p.plot1 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone]\n",sep=""))
						plot(p.plot1)
						cat(paste("47F. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47F. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)					


						# pie chart in term of both state and climate zone			
						 p.plot4 <- qplot(data=myData.keyCode,CIA_codeitem_complies,colour=ClimateZone,fill=ClimateZone,geom="bar") 
						 p.plot4 <- p.plot4 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						 p.plot4 <- p.plot4 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone]\n",sep=""))
						 plot(p.plot4)
						cat(paste("47G. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47G. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)					

						# bar chart in terms of [CZ] and [counties]
						p.plot5 <- qplot(data=myData.keyCode,CIA_codeitem_complies,colour=SPL_county,fill=SPL_county,geom="bar",facets=ClimateZone~.) 
						p.plot5 <- p.plot5 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot5 <- p.plot5 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone] and [SPL_county]\n",sep=""))
						plot(p.plot5)
						cat(paste("47H. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47H. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)					


						# bar chart in terms of [CZ] and [SPL_bldg_name]
						p.plot6 <- qplot(data=myData.keyCode,CIA_codeitem_complies,colour=SPL_bldg_name,fill=SPL_bldg_name,geom="bar",facets=ClimateZone~.) 
						p.plot6 <- p.plot6 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot6 <- p.plot6 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone] and [Unique Bldg]\n",sep=""))
						plot(p.plot6)
						cat(paste("47I. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47I. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)					

						# bar chart in terms of [CZ] and [CIA_codeitem_code]
						p.plot7 <- qplot(data=myData.keyCode,CIA_codeitem_complies,colour=CIA_codeitem_code,fill=CIA_codeitem_code,geom="bar",facets=ClimateZone~.) 
						p.plot7 <- p.plot7 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot7 <- p.plot7 + labs(x=paste("Levels of (",this.field,")",sep=""),y="Count",title=paste(string.thisField,": Responses to (complies) in term of [Climate Zone] and [Code Item]\n",sep=""))
						plot(p.plot7)	
						cat(paste("47J. ",string.thisField,": plotting.\n",sep=""))
						cat(paste("47J. ",string.thisField,": plotting.\n",sep=""),file=FL.LOG, append=TRUE)					

					}

					cat(paste("47K. ",string.thisField,": done.\n\n",sep=""))
					cat(paste("47K. ",string.thisField,": done.\n\n",sep=""),file=FL.LOG, append=TRUE)		

				}	# end of field if condition
			}		# end of field loop
			dev.off(2)
			dev.off(3)	
		}else{		# end of non-zero [myData.keyCode]   data condition
			cat(paste("48. there is no keyitem data retrieved for ",this.state," at ",timeStamp.string,".\n\n",sep=""))
			cat(paste("48. there is no keyitem data retriveed for ",this.state," at ",timeStamp.string,".\n\n",sep=""),file=FL.LOG, append=TRUE)						
		}
	}else{			# end of non-zero [myData.thisState] data condition
		cat(paste("49. there is no data retrieved for ",this.state," at ",timeStamp.string,".\n\n",sep=""))
		cat(paste("49. there is no data retriveed for ",this.state," at ",timeStamp.string,".\n\n",sep=""),file=FL.LOG, append=TRUE)				
	}
}				# end of state loop
	
	    
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n03_process_for_keyItem.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n03_process_for_keyItem.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [03_process_for_keyItem.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [03_process_for_keyItem.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

