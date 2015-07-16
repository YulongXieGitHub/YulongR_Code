#
# 01_extraction_for_QA.R 
#
# In Windows: need to invoke R i386 not X64
#
# June  29, 2015: Jared helped to revise the script to make it running faster.  Jared's example script is located in 	    "\\poncho\resstd\FOA\Phase1_Analysis\RCD_Analysis\00_manual_diagnosis\MySQL_Revised_Script_Testing\Jared_revised_original.sql"
#                 A minor modification is made ensuring the new data has the exact same fields as we had before for easy comparison and the file is "\\poncho\resstd\FOA\Phase1_Analysis\RCD_Analysis\00_manual_diagnosis\MySQL_Revised_Script_Testing\Jared_revised_YLX.sql"
#
# April 19, 2015: add one more output on the database tables
#
# 
# Created on April 13, 2015
# Use saveRDS to save single R object one per state per J's suggestion.
# Major differneces between save and saveRDS:
# (1) saveRDS() doesn’t save the both the object and its name it just saves a representation of the object.  As a result, the saved object can be loaded into a named object within R that is different from the name it had when originally serialized.
# (2) saveRDS() can only save on eobject.
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

# *********************************************************************************************************
# make a time stamp string to be used in the folder and file names
# *********************************************************************************************************
timeStamp.string <- paste(year,month,day,sep="")

# lis of states in the FOA
array.states <- c("AL","AR","GA","KY","MD","NC","PA","TX")


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



# -------------------------------------------------------------------------------------------------
#	setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.TMP   <- paste(Path.Project,"01_extraction_for_QA",  sep="/")				# OUTPUT processed result directory
Path.LOG   <- paste(Path.Project,"00_log",                sep="/")				# OUTPUT log  directory
Path.OUT   <- paste(Path.TMP,paste(year,month,day,sep=""),sep="/")				# OUTPUT processed result directory

if (!file.exists(Path.TMP)){print(paste("NOT existing:",Path.TMP));dir.create(Path.TMP,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.LOG)){print(paste("NOT existing:",Path.LOG));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}

# ------------------------------------------------------------------------------------------------- 
# 	create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
FL.LOG          <- paste(Path.LOG,paste("01_extraction_for_QA_",timeStamp.string,".LOG",    sep=""),sep="/")		
FL.allMySQL.CSV <- paste(Path.OUT,paste("MySQL_Script_",        timeStamp.string,".CSV",    sep=""),sep="/")	
FL.allData.OBJ  <- paste(Path.OUT,paste("AllData_",             timeStamp.string,".Rdata",  sep=""),sep="/")
FL.allData.CHK  <- paste(Path.OUT,paste("AllData_",             timeStamp.string,"_CHK.CSV",sep=""),sep="/")
FL.allData.SUM  <- paste(Path.OUT,paste("AllData_",             timeStamp.string,"_SUM.CSV",sep=""),sep="/")
if (file.exists(FL.LOG))         {print(paste(FL.LOG,         " exist. Delete it!"));file.remove(FL.LOG)}
if (file.exists(FL.allMySQL.CSV)){print(paste(FL.allMySQL.CSV," exist. Delete it!"));file.remove(FL.allMySQL.CSV)}			
if (file.exists(FL.allData.OBJ)) {print(paste(FL.allData.OBJ, " exist. Delete it!"));file.remove(FL.allData.OBJ)}			
if (file.exists(FL.allData.CHK)) {print(paste(FL.allData.CHK, " exist. Delete it!"));file.remove(FL.allData.CHK)}			
if (file.exists(FL.allData.SUM)) {print(paste(FL.allData.SUM, " exist. Delete it!"));file.remove(FL.allData.SUM)}			
cat(paste("Log file for data processing script [01_extraction_for_QA.R]!\n",sep=""))
cat(paste("Log file for data processing script [01_extraction_for_QA.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
            "*                    [01_extraction_for_QA.R]                       *",
            "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)

cat(paste("Data Retrieved on ",timeStamp.string,".\n\n",sep=""),file=FL.allData.SUM,append=TRUE)
cat(paste("1. output todays date!\n",sep=""))
cat(paste("1. output todays date!\n",sep=""),file=FL.allData.SUM, append=TRUE)

# -------------------------------------------------------------------------------------------------
#  1. prepare MySQL connection
# -------------------------------------------------------------------------------------------------
myDatabase <- "FOA_read_only"
myUser     <- "foa"
myPassword <- ""
ch         <- odbcConnect(myDatabase,uid=myUser,pwd=myPassword)
cat(paste("1A. data base has been connected!\n",sep=""))
cat(paste("1A. data base has been connected!\n",sep=""),file=FL.LOG, append=TRUE)


#
# a command string to put data frame into a save command
#
command.saving <- "save(";
cat(paste("1B. put all created data in a save command!\n",sep=""))
cat(paste("1B. put all created data in a save command!\n",sep=""),file=FL.LOG, append=TRUE)

# -------------------------------------------------------------------------------------------------
# 2. get all content in each of the six tables
# -------------------------------------------------------------------------------------------------
array.tables <- c("ACCOUNT_GROUP","CHECKLIST_ITEM_ANSWER","JURISDICTION","PRESCRIPTIVE_VALUES","SAMPLE","USER_PROFILE")
for (this.table in array.tables)
{
	# ============================================================
	# a. output the queried data into a csv file
	#    define files for each TABLE in the database
	# ============================================================
	FL.Table.CSV <- paste(Path.OUT,paste("Table_",this.table,"_",timeStamp.string,".CSV",sep=""),sep="/")	
	FL.Table.RDS <- paste(Path.OUT,paste("Table_",this.table,"_",timeStamp.string,".RDS",sep=""),sep="/")	
	if (file.exists(FL.Table.CSV)){print(paste(FL.Table.CSV, " exist. Delete it!"));file.remove(FL.Table.CSV)}			
	if (file.exists(FL.Table.RDS)){print(paste(FL.Table.RDS, " exist. Delete it!"));file.remove(FL.Table.RDS)}			
	cat(paste("2a. [",this.table,"]: specify a csv file for outputting the retrieved data.\n",sep=""))
	cat(paste("2a. [",this.table,"]: specify a csv file for outputting the retrieved data.\n",sep=""),file=FL.LOG, append=TRUE)
	
	# b. have the MySQL command to retrieve evverythig in each of the table
	command.string1 <- paste("myQuery.command  <- ",paste("\"SELECT *",
		                                              "FROM   ",
		                                               this.table,
		                                               ";\"",
		                                              sep=" "),
		                                              sep="")
	cat(paste("2b. [",this.table,"]: prepare a command line for query.\n",sep=""))
	cat(paste("2b. [",this.table,"]: prepare a command line for query.\n",sep=""),file=FL.LOG, append=TRUE)
	
	# c. have the command line ready	
	eval(parse(text=command.string1))
	cat(paste("2c. [",this.table,"]: have a command line in [myQuery.command] for query ready.\n",sep=""))
	cat(paste("2c. [",this.table,"]: have a command line in [myQuery.command] for query ready.\n",sep=""),file=FL.LOG, append=TRUE)

	# d. execute the query of the current data table
	#    delete the already existed data frame [myQuery.data] to avoid accident.  Note the actual name of the data frame instead of a variable is used her.
	if(exists("myQuery.data") && is.data.frame(get("myQuery.data"))){rm(myQuery.data)}
	myQuery.data <- sqlQuery(ch,myQuery.command,errors=TRUE)
	cat(paste("2d. [",this.table,"]: query everything in the table and put it in [myQuery.data].\n",sep=""))
	cat(paste("2d. [",this.table,"]: query everything in the table and put it in [myQuery.data].\n",sep=""),file=FL.LOG, append=TRUE)

	# -----------------------------------------------------------------------------
	# e. output the retrieved data of each individual TABLE of the database out to a csv file
	#    output [myQuery.data] to [FL.Table.CSV] 
	#    note: since the existence of some un-usual text in the comments field of CHECKLIST_ITEM_ANSWER table, the csv table outputted at this point without handling them will show break line, shift position etc.
	# -----------------------------------------------------------------------------
	cat(paste(this.table,",",sep=""),file=FL.Table.CSV,append=TRUE)
	write.table(myQuery.data,file=FL.Table.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)	
	saveRDS(myQuery.data,file=FL.Table.RDS)
	cat(paste("2e. [",this.table,"]: ",dim(myQuery.data)[1]," by ",dim(myQuery.data)[2]," data has been queried and outputted to [",FL.Table.CSV,"] and [",FL.Table.RDS,"].\n",sep=""))
	cat(paste("2e. [",this.table,"]: ",dim(myQuery.data)[1]," by ",dim(myQuery.data)[2]," data has been queried and outputted to [",FL.Table.CSV,"] and [",FL.Table.RDS,"].\n",sep=""),file=FL.LOG, append=TRUE)	
	
	# -----------------------------------------------------------------------------
	# f. record the Query for raw data in a csv file
	#    output the query commands
	# -----------------------------------------------------------------------------
	cat(paste("# Query for retrieving raw data in [",this.table,"]!\n"),file=FL.allMySQL.CSV,append=TRUE)
	cat(myQuery.command,file=FL.allMySQL.CSV,append=TRUE)
	cat("\n\n",file=FL.allMySQL.CSV,append=TRUE)
	cat(paste("2f. [",this.table,"]: query for this retrieval has been outputted to [",FL.allMySQL.CSV,"].\n",sep=""))
	cat(paste("2f. [",this.table,"]: query for this retrieval has been outputted to [",FL.allMySQL.CSV,"].\n",sep=""),file=FL.LOG, append=TRUE)	
	
	# g. assign the data in table into a new data frame named after this table in order to put all data frames of all retrieved data into a single save command.
	df.new <- paste("myTable_",this.table,sep="")
	command.string2 <- paste(df.new," <- myQuery.data",sep="")
	eval(parse(text=command.string2))
	cat(paste("2g. [",this.table,"]: assign [myQuery.data] into [",df.new,"],.\n",sep=""))
	cat(paste("2g. [",this.table,"]: assign [myQuery.data] into [",df.new,"],.\n",sep=""),file=FL.LOG, append=TRUE)		
	
	# h. append the saving command
	command.saving <- paste(command.saving,df.new,",",sep="");
	cat(paste("2h. [",this.table,"]: append [command.saving],.\n",sep=""))
	cat(paste("2h. [",this.table,"]: append [command.saving],.\n",sep=""),file=FL.LOG, append=TRUE)		
	
	# i. put the number of records of the tables into a summary file
	cat(paste("Table [",this.table,"] has [",dim(myQuery.data)[1],"] by [",dim(myQuery.data)[2],"] data\n",sep=""))
	cat(paste("Table [",this.table,"] has [",dim(myQuery.data)[1],"] by [",dim(myQuery.data)[2],"] data\n",sep=""),file=FL.allData.SUM, append=TRUE)	
	cat(paste("Table [",this.table,"] has [",dim(myQuery.data)[1],"] by [",dim(myQuery.data)[2],"] data\n",sep=""),file=FL.LOG,         append=TRUE)	
	
	cat(paste("2i. [",this.table,"]: output retrieved records output to [",FL.allData.SUM,"].\n",sep=""))
	cat(paste("2i. [",this.table,"]: output retrieved records output to [",FL.allData.SUM,"].\n",sep=""),file=FL.LOG, append=TRUE)		
	
}
cat(paste("\n",sep=""),file=FL.allData.SUM, append=TRUE)
cat(paste("2j. Data in all tables have been queried and outputted.\n",sep=""))
cat(paste("2j. Data in all tables have been queried and outputted.\n",sep=""),file=FL.LOG, append=TRUE)	

# -------------------------------------------------------------------------------------------------
# 3. get all content with the six tables combined
# -------------------------------------------------------------------------------------------------
# ============================================================
# a. output the queried data into a csv file
#    define files for data retriebved from the merged TABLES in the database
# ============================================================
FL.Table.CSV <- paste(Path.OUT,paste("AllData_",timeStamp.string,".CSV",sep=""),sep="/")	
FL.Table.RDS <- paste(Path.OUT,paste("AllData_",timeStamp.string,".RDS",sep=""),sep="/")	
if (file.exists(FL.Table.CSV)){print(paste(FL.Table.CSV, " exist. Delete it!"));file.remove(FL.Table.CSV)}			
if (file.exists(FL.Table.RDS)){print(paste(FL.Table.RDS, " exist. Delete it!"));file.remove(FL.Table.RDS)}			
cat(paste("3a. [all data]: specify a csv file for outputting the retrieved data.\n",sep=""))
cat(paste("3a. [all data]: specify a csv file for outputting the retrieved data.\n",sep=""),file=FL.LOG, append=TRUE)

# b. have the MySQL command to retrieve evverythig in each of the table
command.string1 <- paste("myQuery.command  <- ",	                                              

	paste("\"SELECT  USER_PROFILE.mail                                       as UP_mail,                   \n",
			"SAMPLE.id_code                                          as SPL_bldg_name,             \n",
			"SAMPLE.state                                            as SPL_state,                 \n",
			"SAMPLE.county                                           as SPL_county,                \n",
			"SAMPLE.climate_zone                                     as SPL_CZ,                    \n",
			"CHECKLIST_ITEM_ANSWER.code                              as CIA_codeitem_code,         \n", 
			"CHECKLIST_ITEM_ANSWER.complies                          as CIA_codeitem_complies,     \n",
			"CHECKLIST_ITEM_ANSWER.comments                          as CIA_codeitem_comments,     \n",        
			"CHECKLIST_ITEM_ANSWER.ok                                as CIA_codeitem_completeness, \n",
			"CHECKLIST_ITEM_ANSWER.last_modified                     as CIA_codeitem_timestamp,    \n",
			"PRESCRIPTIVE_VALUES.value                               as PV_codeitem_value,         \n",
			"PRESCRIPTIVE_VALUES.hers                                as PV_codeitem_hers,          \n",
			"PRESCRIPTIVE_VALUES.last_modified                       as PV_codeitem_timestamp,     \n",
			"SAMPLE.user_id                                          as SPL_user_id,               \n",
			"USER_PROFILE.id                                         as UP_user_id,                \n",
			"SAMPLE.id                                               as SPL_bldg_ID,               \n",
			"CHECKLIST_ITEM_ANSWER.sample                            as CIA_bldg_ID,               \n",      
			"CHECKLIST_ITEM_ANSWER.id                                as CIA_codeitem_ID,           \n", 
			"PRESCRIPTIVE_VALUES.checklist_item_answer               as PV_codeitem_ID             \n",
			"	                                                                               \n",
		  paste("FROM  CHECKLIST_ITEM_ANSWER LEFT JOIN PRESCRIPTIVE_VALUES ON CHECKLIST_ITEM_ANSWER.id  = PRESCRIPTIVE_VALUES.checklist_item_answer   \n",sep=""),
			"                            LEFT JOIN SAMPLE              ON SAMPLE.id = CHECKLIST_ITEM_ANSWER.sample                                \n",
			"                            LEFT JOIN USER_PROFILE        ON SAMPLE.user_id = USER_PROFILE.id                                     ;\"\n",
			sep=" "),
			sep="")

cat(paste("3b. [all data]: prepare a command line for query.\n",sep=""))
cat(paste("3b. [all data]: prepare a command line for query.\n",sep=""),file=FL.LOG, append=TRUE)

# c. have the command line ready	
eval(parse(text=command.string1))
cat(paste("3c. [all data]: have a command line in [myQuery.command] for query ready.\n",sep=""))
cat(paste("3c. [all data]: have a command line in [myQuery.command] for query ready.\n",sep=""),file=FL.LOG, append=TRUE)

# d. execute the query of the current data table	
if(exists("myQuery.data") && is.data.frame(get("myQuery.data"))){rm(myQuery.data)}
myQuery.data <- sqlQuery(ch,myQuery.command,errors=TRUE)
cat(paste("3d. [all data]: query everything in the table and put it in [myQuery.data].\n",sep=""))
cat(paste("3d. [all data]: query everything in the table and put it in [myQuery.data].\n",sep=""),file=FL.LOG, append=TRUE)


# -----------------------------------------------------------------------------
# e. output the retrieved data of the records by merging the TABLES of the database out to a csv file
#    output [myQuery.data] to [FL.Table.CSV] 
#    note: since the existence of some un-usual text in the comments field of CHECKLIST_ITEM_ANSWER table, the csv table outputted at this point without handling them will show break line, shift position etc.
# -----------------------------------------------------------------------------
cat(paste("All Data,",sep=""),file=FL.Table.CSV,append=TRUE)
write.table(myQuery.data,file=FL.Table.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
saveRDS(myQuery.data,file=FL.Table.RDS)
cat(paste("3e. [all data]: ",dim(myQuery.data)[1]," by ",dim(myQuery.data)[2]," data has been queried and outputted to [",FL.Table.CSV,"] and [",FL.Table.RDS,"].\n",sep=""))
cat(paste("3e. [all data]: ",dim(myQuery.data)[1]," by ",dim(myQuery.data)[2]," data has been queried and outputted to [",FL.Table.CSV,"] and [",FL.Table.RDS,"].\n",sep=""),file=FL.LOG, append=TRUE)	

# -----------------------------------------------------------------------------
# f. record the Query for raw data in a csv file
#    output the query commands
# -----------------------------------------------------------------------------
cat(paste("# Query for retrieving raw data in [all data]!\n"),file=FL.allMySQL.CSV,append=TRUE)
cat(myQuery.command,file=FL.allMySQL.CSV,append=TRUE)
cat("\n\n",file=FL.allMySQL.CSV,append=TRUE)
cat(paste("3f. [all data]: query for this retrieval has been outputted to [",FL.allMySQL.CSV,"].\n",sep=""))
cat(paste("3f. [all data]: query for this retrieval has been outputted to [",FL.allMySQL.CSV,"].\n",sep=""),file=FL.LOG, append=TRUE)	

# g. assign the data in table into a new data frame named after this table
df.new <- "myTable_AllData"
command.string2 <- paste(df.new," <- myQuery.data",sep="")
eval(parse(text=command.string2))
cat(paste("3g. [all data]: assign [myQuery.data] into [",df.new,"],.\n",sep=""))
cat(paste("3g. [all data]: assign [myQuery.data] into [",df.new,"],.\n",sep=""),file=FL.LOG, append=TRUE)	

# h. append the saving command
command.saving <- paste(command.saving,df.new,",",sep="");
cat(paste("3h. [all data]: append [command.saving].\n",sep=""))
cat(paste("3h. [all data]: append [command.saving].\n",sep=""),file=FL.LOG, append=TRUE)		


# i. put the number of records of the tables into a summary file
cat(paste("Data from all tables has [",dim(myQuery.data)[1],"] by [",dim(myQuery.data)[2],"] data\n",sep=""))
cat(paste("Data from all tables has [",dim(myQuery.data)[1],"] by [",dim(myQuery.data)[2],"] data\n",sep=""),file=FL.allData.SUM, append=TRUE)	
cat(paste("Data from all tables has [",dim(myQuery.data)[1],"] by [",dim(myQuery.data)[2],"] data\n",sep=""),file=FL.LOG,         append=TRUE)	

cat(paste("\n",sep=""),file=FL.allData.SUM, append=TRUE)
cat(paste("3i. [all data]: output retrieved records output to [",FL.allData.SUM,"].\n",sep=""))
cat(paste("3i. [all data]: output retrieved records output to [",FL.allData.SUM,"].\n",sep=""),file=FL.LOG, append=TRUE)		



# -------------------------------------------------------------------------------------------------
# 4. get all content for each of the state
# -------------------------------------------------------------------------------------------------
for (this.state in array.states)
{
	# ============================================================
	# a. output the queried data into a csv file
	#    define files for data retriebved from the merged TABLES in the database
	# ============================================================
	FL.Table.CSV <- paste(Path.OUT,paste("State_",this.state,"_",timeStamp.string,".CSV",sep=""),sep="/")	
	FL.Table.RDS <- paste(Path.OUT,paste("State_",this.state,"_",timeStamp.string,".RDS",sep=""),sep="/")	
	if (file.exists(FL.Table.CSV)){print(paste(FL.Table.CSV, " exist. Delete it!"));file.remove(FL.Table.CSV)}			
	if (file.exists(FL.Table.RDS)){print(paste(FL.Table.RDS, " exist. Delete it!"));file.remove(FL.Table.RDS)}			
	cat(paste("4a. [",this.state,"]: specify a csv file for outputting the retrieved data.\n",sep=""))
	cat(paste("4a. [",this.state,"]: specify a csv file for outputting the retrieved data.\n",sep=""),file=FL.LOG, append=TRUE)
	
	# b. have the MySQL command to retrieve evverythig in each of the table                                              
	command.string1 <- paste("myQuery.command  <- ",	                                              
		                                              		                                              
		paste("\"SELECT  CIA.UP_mail                                                               as UP_mail,                                \n",
                        "        CIA.SPL_bldg_name                                                         as SPL_bldg_name,                          \n",
                        "        CIA.SPL_state                                                             as SPL_state,                              \n",
                        "        CIA.SPL_county                                                            as SPL_county,                             \n",
                        "        CIA.SPL_CZ                                                                as SPL_CZ,                                 \n",
                        "        CIA.CIA_codeitem_code                                                     as CIA_codeitem_code,                      \n", 
                        "        CIA.CIA_codeitem_complies                                                 as CIA_codeitem_complies,                  \n",
                        "        CIA.CIA_codeitem_comments                                                 as CIA_codeitem_comments,                  \n",        
                        "        CIA.CIA_codeitem_completeness                                             as CIA_codeitem_completeness,              \n",
                        "        CIA.CIA_codeitem_timestamp                                                as CIA_codeitem_timestamp,                 \n",
                        "        PV.PV_codeitem_value                                                      as PV_codeitem_value,                      \n",
                        "        PV.PV_codeitem_hers                                                       as PV_codeitem_hers,                       \n",
                        "        PV.PV_codeitem_timestamp                                                  as PV_codeitem_timestamp,                  \n",
                        "        CIA.SPL_user_id                                                           as SPL_user_id,                            \n",
                        "        CIA.UP_user_id                                                            as UP_user_id,                             \n",
                        "        CIA.SPL_bldg_ID                                                           as SPL_bldg_ID,                            \n",
                        "        CIA.CIA_bldg_ID                                                           as CIA_bldg_ID,                            \n",      
                        "        CIA.CIA_codeitem_ID                                                       as CIA_codeitem_ID,                        \n", 
                        "        PV.PV_codeitem_ID                                                         as PV_codeitem_ID 		              \n",
                        "	                                                                                                                      \n",
                        "FROM (SELECT USER_PROFILE.mail                                                    as UP_mail,                                \n",
                        "             SPL.id_code                                                          as SPL_bldg_name,                          \n",
                        "             SPL.state                                                            as SPL_state,                              \n",
                        "             SPL.county                                                           as SPL_county,                             \n",
                        "             SPL.climate_zone                                                     as SPL_CZ,                                 \n",
                        "             CHECKLIST_ITEM_ANSWER.code                                           as CIA_codeitem_code,                      \n",
                        "             CHECKLIST_ITEM_ANSWER.complies                                       as CIA_codeitem_complies,                  \n",
                        "             CHECKLIST_ITEM_ANSWER.comments                                       as CIA_codeitem_comments,                  \n",
                        "             CHECKLIST_ITEM_ANSWER.ok                                             as CIA_codeitem_completeness,              \n",
                        "             CHECKLIST_ITEM_ANSWER.last_modified                                  as CIA_codeitem_timestamp,                 \n",
                        "             SPL.user_id                                                          as SPL_user_id,                            \n",
                        "             USER_PROFILE.id                                                      as UP_user_id,                             \n",
                        "             SPL.id                                                               as SPL_bldg_ID,                            \n",
                        "             CHECKLIST_ITEM_ANSWER.sample                                         as CIA_bldg_ID,                            \n",
                        "             CHECKLIST_ITEM_ANSWER.id                                             as CIA_codeitem_ID                         \n", 
			"                                                                                                                             \n",
                        "      FROM   CHECKLIST_ITEM_ANSWER, SAMPLE as SPL, USER_PROFILE                                                              \n",
                  paste("      WHERE  SPL.state = '",this.state,"'                                                                                    \n",sep=""),
                        "        AND  SPL.id = CHECKLIST_ITEM_ANSWER.sample                                                                           \n",
                        "        AND  SPL.user_id = USER_PROFILE.id)                                       AS CIA                                     \n",
                        "	                                                                                                                      \n",
                        "LEFT JOIN                                                                                                                    \n",
                        "     (SELECT                                                                                                                 \n",
                        "             PRESCRIPTIVE_VALUES.value                                            as PV_codeitem_value,                      \n",
                        "             PRESCRIPTIVE_VALUES.hers                                             as PV_codeitem_hers,                       \n",
                        "             PRESCRIPTIVE_VALUES.last_modified                                    as PV_codeitem_timestamp,                  \n",
                        "             PRESCRIPTIVE_VALUES.checklist_item_answer                            as PV_codeitem_ID                          \n", 
                  paste("      FROM (SELECT id FROM SAMPLE where state='",this.state,"') SPL, CHECKLIST_ITEM_ANSWER, PRESCRIPTIVE_VALUES              \n",sep=""),
                        "      WHERE SPL.id = CHECKLIST_ITEM_ANSWER.sample                                                                            \n",
                        "        AND PRESCRIPTIVE_VALUES.checklist_item_answer = CHECKLIST_ITEM_ANSWER.id) AS PV                                      \n",
                        "                                                                                                                             \n",
                        "ON CIA.CIA_codeitem_ID = PV.PV_codeitem_ID                                                                                ;\"\n",sep=" "),sep="") 


	                                              
	cat(paste("4b. [",this.state,"]: prepare a command line for query.\n",sep=""))
	cat(paste("4b. [",this.state,"]: prepare a command line for query.\n",sep=""),file=FL.LOG, append=TRUE)
	
	# c. have the command line ready	
	eval(parse(text=command.string1))
	cat(paste("4c. [",this.state,"]: have a command line in [myQuery.command] for query ready.\n",sep=""))
	cat(paste("4c. [",this.state,"]: have a command line in [myQuery.command] for query ready.\n",sep=""),file=FL.LOG, append=TRUE)

	# d. execute the query of the current data table	
	if(exists("myQuery.data") && is.data.frame(get("myQuery.data"))){rm(myQuery.data)}
	myQuery.data <- sqlQuery(ch,myQuery.command,errors=TRUE)
	cat(paste("4d. [",this.state,"]: query everything in the table and put it in [myQuery.data].\n",sep=""))
	cat(paste("4d. [",this.state,"]: query everything in the table and put it in [myQuery.data].\n",sep=""),file=FL.LOG, append=TRUE)

	# -----------------------------------------------------------------------------
	# e. output the retrieved data of the records by merging the TABLES of the database out to a csv file
	#    output [myQuery.data] to [FL.Table.CSV] 
	#    note: since the existence of some un-usual text in the comments field of CHECKLIST_ITEM_ANSWER table, the csv table outputted at this point without handling them will show break line, shift position etc.
	# -----------------------------------------------------------------------------
	cat(paste(this.state,",",sep=""),file=FL.Table.CSV,append=TRUE)
	write.table(myQuery.data,file=FL.Table.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	saveRDS(myQuery.data,file=FL.Table.RDS)
	cat(paste("4e. [",this.state,"]: ",dim(myQuery.data)[1]," by ",dim(myQuery.data)[2]," data has been queried and outputted to [",FL.Table.CSV,"] and [",FL.Table.RDS,"].\n",sep=""))
	cat(paste("4e. [",this.state,"]: ",dim(myQuery.data)[1]," by ",dim(myQuery.data)[2]," data has been queried and outputted to [",FL.Table.CSV,"] and [",FL.Table.RDS,"].\n",sep=""),file=FL.LOG, append=TRUE)	
	
	# -----------------------------------------------------------------------------
	# f. record the Query for raw data in a csv file
	#    output the query commands
	# -----------------------------------------------------------------------------
	cat(paste("# Query for retrieving raw data in [",this.state,"]!\n"),file=FL.allMySQL.CSV,append=TRUE)
	cat(myQuery.command,file=FL.allMySQL.CSV,append=TRUE)
	cat("\n\n",file=FL.allMySQL.CSV,append=TRUE)
	cat(paste("4f. [",this.state,"]: query for this retrieval has been outputted to [",FL.allMySQL.CSV,"].\n",sep=""))
	cat(paste("4f. [",this.state,"]: query for this retrieval has been outputted to [",FL.allMySQL.CSV,"].\n",sep=""),file=FL.LOG, append=TRUE)	

	
	# g. assign the data in table into a new data frame named after this table
	df.new <- paste("myState_",this.state,sep="")
	command.string2 <- paste(df.new," <- myQuery.data",sep="")
	eval(parse(text=command.string2))
	cat(paste("4g. [",this.state,"]: assign [myQuery.data] into [",df.new,"],.\n",sep=""))
	cat(paste("4g. [",this.state,"]: assign [myQuery.data] into [",df.new,"],.\n",sep=""),file=FL.LOG, append=TRUE)		

	# h. append the saving command
	command.saving <- paste(command.saving,df.new,",",sep="");
	cat(paste("4h. [",this.state,"]: append [command.saving],.\n",sep=""))
	cat(paste("4h. [",this.state,"]: append [command.saving],.\n",sep=""),file=FL.LOG, append=TRUE)	
	



	# i. put the number of records of the tables into a summary file
	cat(paste("Data combined from all Tables for [",this.state,"] has [",dim(myQuery.data)[1],"] by [",dim(myQuery.data)[2],"] data\n",sep=""))
	cat(paste("Data combined from all Tables for [",this.state,"] has [",dim(myQuery.data)[1],"] by [",dim(myQuery.data)[2],"] data\n",sep=""),file=FL.allData.SUM, append=TRUE)	
	cat(paste("Data combined from all Tables for [",this.state,"] has [",dim(myQuery.data)[1],"] by [",dim(myQuery.data)[2],"] data\n",sep=""),file=FL.LOG,         append=TRUE)	
	cat(paste("4i. [",this.state,"]: output retrieved records output to [",FL.allData.SUM,"].\n",sep=""))
	cat(paste("4i. [",this.state,"]: output retrieved records output to [",FL.allData.SUM,"].\n",sep=""),file=FL.LOG, append=TRUE)		
}
cat(paste("\n",sep=""),file=FL.allData.SUM, append=TRUE)
	
	

cat(paste("4j. Data in all states have been queried and outputted.\n",sep=""))
cat(paste("4j. Data in all states have been queried and outputted.\n",sep=""),file=FL.LOG, append=TRUE)	



# -------------------------------------------------------------------------------------------------
# 5. save the data frames
# -------------------------------------------------------------------------------------------------
command.saving <- paste(command.saving,"file = \"",FL.allData.OBJ,"\")",sep="");
eval(parse(text = command.saving))
cat(paste("5. all data frame are saved to [",FL.allData.OBJ,"].\n",sep=""))
cat(paste("5. all data frame are saved to [",FL.allData.OBJ,"].\n",sep=""),file=FL.LOG, append=TRUE)		


# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# 6. Customized individually cleaning for entries which break the data parsing below wneh output into a csv file
#
#    April 28, 2015: find one entry "Heat Pum[\p" in "PV_CodeItem_Value" field in AL
#    May 8, 2015:    find 32 fields in "PV_codeItem_value" field has "\" in it
#                          1 field  in "PV_codeItem_name" field has "\" in it
#    
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# April 28, 2015: a record having "Heat Pum[\\p" in the codeItem_value field
myTable_AllData[grep("Heat Pum\\[\\\\\\p",myTable_AllData[,"PV_codeitem_value"]),"PV_codeitem_value"] <- "Heat Pump"

# May 8, 2015: also clean some newly found problems!!!!!!!!!!!!!!!
myTable_AllData[,"CIA_codeitem_comments"] <- gsub("\""," inch",myTable_AllData[,"CIA_codeitem_comments"])	# double quotation sign replaced with inch
myTable_AllData[,"CIA_codeitem_comments"] <- gsub("'", " ",    myTable_AllData[,"CIA_codeitem_comments"])	# single quotation sign replaced with a space
myTable_AllData[,"CIA_codeitem_comments"] <- gsub("@", " at",  myTable_AllData[,"CIA_codeitem_comments"])	# @                sign replaced with at
myTable_AllData[,"CIA_codeitem_comments"] <- gsub("\\\\", " ", myTable_AllData[,"CIA_codeitem_comments"])	# backslash        sign replaced with a space (note: if there is a single backslash in the database, the retrieved data will have double backslash to escape it.  Therefore we need to replace two backslashes.
cat(paste("6. do some customized cleaning on [myTable_AllData].\n",sep=""))
cat(paste("6. do some customized cleaning on [myTable_AllData].\n",sep=""),file=FL.LOG, append=TRUE)		




# -------------------------------------------------------------------------------------------------
# 7. checking the data field
# -------------------------------------------------------------------------------------------------
# check the levels of each field
for (this.field in names(myTable_AllData)[-c(8,10,13,18,19)])	# do not do it on comments, timestamps, and ID columns!!!
{
	command.string <- paste("unique.lvls <- data.frame(\"",paste(sort(unique(myTable_AllData[,this.field])),collapse="\",\""),"\")",sep="")
	eval(parse(text=command.string))
	
	          myTmp  <- data.frame(t(unique.lvls))
	    names(myTmp) <- this.field
	row.names(myTmp) <- myTmp[,this.field]	
	
	cat(paste(this.field,",",sep=""),file=FL.allData.CHK,append=TRUE)
	write.table(myTmp,file=FL.allData.CHK,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	cat("\n\n",file=FL.allData.CHK,append=TRUE)
}
cat(paste("7. unique levels of the fields in the data (note: some fields cannot be sorted and do not know why).\n",sep=""))
cat(paste("7. unique levels of the fields in the data (note: some fields cannot be sorted and do not know why).\n",sep=""),file=FL.LOG, append=TRUE)		

# -------------------------------------------------------------------------------------------------
# 8. checking the data with non-NA of values
# -------------------------------------------------------------------------------------------------	
FL.nonNA.RDS  <- paste(Path.OUT,paste("nonNA_",             timeStamp.string,".RDS",  sep=""),sep="/")
FL.nonNA.CSV  <- paste(Path.OUT,paste("nonNA_",             timeStamp.string,".CSV",sep=""),sep="/")
if (file.exists(FL.nonNA.RDS)) {print(paste(FL.nonNA.RDS, " exist. Delete it!"));file.remove(FL.nonNA.RDS)}			
if (file.exists(FL.nonNA.CSV)) {print(paste(FL.nonNA.CSV, " exist. Delete it!"));file.remove(FL.nonNA.CSV)}			



myData.nonNA <- myTable_AllData[!(is.na(myTable_AllData[,"PV_codeitem_value"])),]	# keep only non NA entries in the code item value field
myData.nonNA <- myData.nonNA[myData.nonNA[,"PV_codeitem_value"] != "",]			# remove "" entries

      mySum  <- aggregate(myData.nonNA[,"PV_codeitem_value"],list(myData.nonNA[,"CIA_codeitem_code"],myData.nonNA[,"PV_codeitem_value"]),length)
names(mySum) <- c("Code_Item_Name","Code_Item_Value","Count")

      mySum  <- mySum[order(mySum[,"Code_Item_Name"]),]


cat(paste("data with non-NA value,",sep=""),file=FL.nonNA.CSV,append=TRUE)
saveRDS(myData.nonNA,file=FL.nonNA.RDS)
write.table(myData.nonNA,file=FL.nonNA.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("\n\n",file=FL.nonNA.CSV,append=TRUE)

cat(paste("code item and thier values,",sep=""),file=FL.allData.CHK,append=TRUE)
write.table(mySum,file=FL.allData.CHK,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("\n\n",file=FL.allData.CHK,append=TRUE)

cat(paste("8. keep only the non-NA entries in the code item value field.\n",sep=""))
cat(paste("8. keep only the non-NA entries in the code item value field.\n",sep=""),file=FL.LOG, append=TRUE)		





# -------------------------------------------------------------------------------------------------
# 9. close the command.saving string
# -------------------------------------------------------------------------------------------------
odbcClose(ch)
cat(paste("9. the connection to database ",myDatabase," is closed!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("9. the connection to database ",myDatabase," is closed!\n",sep=""))
		    
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n01_extraction_for_QA.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n01_extraction_for_QA.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [01_extraction_for_QA.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [01_extraction_for_QA.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

