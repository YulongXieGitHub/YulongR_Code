#
# Test_Brugs01.R
# Chris Vernon prepared a revised data sheet on May 31,   2013 after having deleted some segment 7 data.
# Chris Vernon prepared a revised data sheet on May 27,   2013 with (1) relative discharge    and (2) new field names
#
# Data difference between May 27, 2013 and May 14, 2013

# The data has been pre-processed in excel in the following way before saved as the csv format to be used by this script.
# (1) replace all 1,380,446 NULL  with empty field
# (2) replace all       142 NaN   with empty field
# (3) replace all         0 -9999 with empty field
# (4) replace 8     "#DIV/0!"     with empty field in the CPUE field
# (5) replace 8 "DIV/0!" for "CPUE" and 1 "DIV/0!" for "CPUA" and the same number for "CPUE2" and "CPUA2".
#
# eliminate all stuff
rm(list = ls(all = TRUE))


# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}


# today's month, day and year in the format of "Thu Jun 16 08:48:36 2011", 5 fields separated by space
today.month  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
today.day    <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
today.year   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]
today.hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
today.minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
today.second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]


# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_scripts"
}
setwd(Path.Current)

# # Data Folder
# Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/_data_received/sturg_spatial/data/Habitat_Metrics_Output"
# Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
# Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/Test_Brugs01"
# if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
# if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
# if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}
# 
# # data file
# FL.Data.IN   <- paste(Path.Data.IN, "MoRiver_Variables_2013May14_YLX.csv",sep="/")
# FL.Data.OUT  <- paste(Path.Out,"TLC_data.csv",sep="/")
# 
# FL.LOG       <- paste(Path.log,"Test_Brugs01.log",sep="/")	
# FL.SUM.cat   <- paste(Path.Out,"Test_Brugs01_categoricalSum.csv",sep="/")
# FL.SUM.num   <- paste(Path.Out,"Test_Brugs01_numericSum.csv",sep="/")
# FL.Summary   <- paste(Path.Out,"Test_Brugs01_summary.csv",sep="/")
# FL.missing   <- paste(Path.Out,"Test_Brugs01_missing.csv",sep="/")
# if (!file.exists(FL.Data.IN))  {stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
# if  (file.exists(FL.Data.OUT)) {print(paste(FL.Data.OUT, "exist.Delete it!")); file.remove(FL.Data.OUT)}
# if  (file.exists(FL.LOG))      {print(paste(FL.LOG,      "exist.Delete it!")); file.remove(FL.LOG)}
# if  (file.exists(FL.SUM.cat))  {print(paste(FL.SUM.cat,  "exist.Delete it!")); file.remove(FL.SUM.cat)}
# if  (file.exists(FL.SUM.num))  {print(paste(FL.SUM.num,  "exist.Delete it!")); file.remove(FL.SUM.num)}
# if  (file.exists(FL.Summary))  {print(paste(FL.Summary,  "exist.Delete it!")); file.remove(FL.Summary)}
# if  (file.exists(FL.missing))  {print(paste(FL.missing,  "exist.Delete it!")); file.remove(FL.missing)}


library("lattice")
library("reshape")


# -------------------------------------------------------------------------------------------------
# Data Folder and files
# -------------------------------------------------------------------------------------------------
Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/_data_received/sturg_spatial/data/Habitat_Metrics_Output"
Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/Test_Brugs01"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}


FL.Data.IN  <- paste(Path.Data.IN, "MoRiver_Variables_2013May31_YLX.csv",sep="/")
FL.Data.OUT <- paste(Path.Out,"TLC_data.csv",sep="/")
FL.Data.OBJ <- paste(Path.Out,"TLC_data.Rdata",sep="/")
FL.RESL.OUT <- paste(Path.Out,"Test_Brugs01.csv",sep="/")
FL.LOG      <- paste(Path.log,"Test_Brugs01.log",sep="/")	
FL.PDF      <- paste(Path.Out,"Test_Brugs01.pdf",sep="/")	
FL.SUM.cat  <- paste(Path.Out,"Test_Brugs01_cat.sum",sep="/")
FL.SUM.num  <- paste(Path.Out,"Test_Brugs01_num.sum",sep="/")

FL.package  <- paste(Path.Current,"package_loading.R",sep="/")


if (!file.exists(FL.Data.IN))  {stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.Data.OUT)) {print(paste(FL.Data.OUT,"exist.Delete it!")); file.remove(FL.Data.OUT)}
if  (file.exists(FL.Data.OBJ)) {print(paste(FL.Data.OBJ,"exist.Delete it!")); file.remove(FL.Data.OBJ)}
if  (file.exists(FL.RESL.OUT)) {print(paste(FL.RESL.OUT,"exist.Delete it!")); file.remove(FL.RESL.OUT)}

if  (file.exists(FL.LOG))      {print(paste(FL.LOG,     "exist.Delete it!")); file.remove(FL.LOG)}
if  (file.exists(FL.PDF))      {print(paste(FL.PDF,     "exist.Delete it!")); file.remove(FL.PDF)}
if  (file.exists(FL.SUM.cat))  {print(paste(FL.SUM.cat, "exist.Delete it!")); file.remove(FL.SUM.cat)}
if  (file.exists(FL.SUM.num))  {print(paste(FL.SUM.num, "exist.Delete it!")); file.remove(FL.SUM.num)}




    
# open pdf file for outputting plots
pdf(file = FL.PDF,         paper="a4r",width=0,height=0)	



# -------------------------------------------------------------------------------------------------
# load two functions which is needed
# -------------------------------------------------------------------------------------------------
clog <- function(x) log(x + 0.5)

cfac <- function(x, breaks = NULL) 
{
	if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
	x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
	levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
	c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
	sep = "")
	return(x)
}
argument <- "glm"
source(FL.package)
# read April 24, 2013 data 
row.titles  <- c("ID",     "SY",     "FieldOffice","Project","UniqueID","Gear",  "Season","Bend",   "BendRN", "BendRiverMile","Near_NHD2rm_FID","Near_NHD2RM_dist_m","NHD2_RM","X1960BendID","RM1960_RM","UpDamDist","UpDamNm","DnDamDist","DnDamNm",                                 "Ch_W_Full","Ch_W_NoIsl","Braiding","UpTrib",   "UpTribDist","DnTrib",   "DTribDist",                            "Reach",  "Mean_z", "Max_z",  "Min_z",  "Grade10RM","GradeBend","Rocktype1","Rocktype2","Lithology","NFHAP_Scr","MedFlow",  "MeanFlow",  "taxorder","taxpartsiz", "comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Segment","nPage",  "TotalPages","SetDate",  "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turbidity","Conductivity","DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "DecimalTimeDifference","StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3",              "Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","VelocityBot2","VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Total_Fish_Count","Pal_cnt","CPUE",   "CPU_Area","Alt_Pal_cnt","Alt_CPUE","Alt_CPU_Area","S_U10_D2_Near","S_U10_D2_Cumu","S_U20_D4_Near","S_U20_D4_Cumu","S_U30_D6_Near","S_U30_D6_Cumu","S_U50_D10_Near","S_U50_D10_Cumu","S_U100_D20_Near","S_U100_D20_Cumu",                            "gear.type1","gear.type2","macro.type")
colClasses  <- c("integer","integer","factor",     "integer","integer", "factor","factor","integer","factor", "numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",  "numeric",  "factor", "numeric",  "factor",                                  "numeric",  "numeric",   "integer", "factor",   "numeric",   "factor",   "numeric",                              "factor", "numeric","numeric","numeric","numeric",  "numeric",  "factor",   "factor",   "factor",   "factor",   "numeric",  "numeric",   "factor",  "factor",     "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "integer","integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric",             "numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",                                    "factor",    "factor",    "factor")
used.4.plot <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     TRUE,    TRUE,    TRUE,     FALSE,   TRUE,            FALSE,            FALSE,               FALSE,    FALSE,        TRUE,       TRUE,       FALSE,    FALSE,      FALSE,                                     TRUE,       TRUE,        TRUE,      FALSE,      TRUE,        FALSE,      TRUE,                                   TRUE,     TRUE,     TRUE,     TRUE,     TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,        TRUE,      TRUE,         TRUE,       TRUE,         TRUE,         TRUE,         TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     FALSE,      FALSE,         FALSE,    TRUE,      FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    TRUE,    FALSE,   TRUE,    TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          TRUE,     TRUE,     TRUE,                  FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,                                         TRUE,        TRUE,        TRUE)
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     FALSE,   TRUE,    FALSE,    FALSE,    FALSE,          FALSE,            FALSE,               FALSE,    FALSE,        FALSE,      TRUE,       FALSE,    FALSE,      FALSE,                                     TRUE,       FALSE,       TRUE,      FALSE,      TRUE,        FALSE,      TRUE,                                   TRUE,     FALSE,    FALSE,    FALSE,    TRUE,       FALSE,      TRUE,       FALSE,      FALSE,      TRUE,       FALSE,      FALSE,       FALSE,     TRUE,         TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    FALSE,   FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,                 FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,                                         TRUE,        TRUE,        TRUE)

row.titles  <- c("Dep_ID", "SY",     "FO",         "Proj",   "Un_ID",   "Gear",  "Season","Bend",   "Bend_RN","Bend_RM",      "Near_NHD2rm_FID","Near_NHD2RM_dist_m","GIS_RM", "Bend_ID_60", "Bend_RM_60","D_dist_up","D_name_up","D_dist_dn","D_name_dn","D_name_near","D_dist_near","Chan_wid", "Wet_wid",   "Braid"   ,"T_name_up","T_dist_up", "T_name_dn","T_dist_dn","T_name_near","T_dist_near","MA",     "Mean_z", "Max_z",  "Min_z",  "Grade_10_RM",          "Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Disch_med","Disch_mean","Tax_ord", "Tax_part_sz","Comp_per", "Sand_per",   "Clay_per",   "Frag_per",   "VF_wid",  "Seg",    "nPage",  "TotalPages","Set_date", "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turb",     "Cond",        "DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "Set_dur",              "StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3", "Depth_Mean","Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","Btm_vel",     "VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Alt_Pal_Cnt",          "CPUE_2",  "CPUA_2",      "Stk_12_dist",  "Stk_12_cnt",   "Stk_24_dist",  "Stk_24_cnt",   "Stk_36_dist",  "Stk_36_cnt",   "Stk_60_dist",   "Stk_60_cnt",    "Stk_120_dist",   "Stk_120_cnt",    "Rel_dsch_AY","Rel_dsch_WY","gear.type1","gear.type2","macro.type")
colClasses  <- c("integer","integer","factor",     "integer","integer", "factor","factor","integer","factor" ,"numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",   "numeric",  "factor",   "numeric",  "factor",   "factor",     "numeric",    "numeric",  "numeric",   "integer", "factor",   "numeric",   "factor",   "numeric",  "factor"     ,"numeric",    "factor", "numeric","numeric","numeric","numeric",              "factor",   "factor",   "factor",   "factor",   "numeric",  "numeric",   "factor",  "factor",     "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "integer","integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric","numeric",   "numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",        "numeric",    "numeric",    "factor",    "factor",    "factor")
used.4.plot <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     TRUE,    TRUE,    TRUE,     FALSE,    TRUE,           FALSE,            FALSE,               FALSE,    FALSE,        TRUE,        TRUE,       FALSE,      TRUE,       FALSE,      TRUE,         TRUE,         TRUE,       TRUE,        TRUE,      FALSE,      TRUE,        FALSE,      TRUE,       TRUE,         TRUE,         TRUE,     TRUE,     TRUE,     TRUE,     TRUE,                   TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,        TRUE,      TRUE,         TRUE,       TRUE,         TRUE,         TRUE,         TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     FALSE,      FALSE,         FALSE,    TRUE,      FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    TRUE,    FALSE,   TRUE,    TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          TRUE,     TRUE,     TRUE,     TRUE,        FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,         TRUE,         TRUE,        TRUE,        TRUE)
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     FALSE,   TRUE,    FALSE,    FALSE,    FALSE,          FALSE,            FALSE,               FALSE,    FALSE,        FALSE,       TRUE,       FALSE,      TRUE,       FALSE,      TRUE,         TRUE,         TRUE,       FALSE,       TRUE,      FALSE,      TRUE,        FALSE,      TRUE,       TRUE,         TRUE,         TRUE,     FALSE,    FALSE,    FALSE,    TRUE,                   TRUE,       FALSE,      FALSE,      TRUE,       FALSE,      FALSE,       FALSE,     TRUE,         TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    FALSE,   FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,    TRUE,        FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,         TRUE,         TRUE,        TRUE,        TRUE)


# the raw data including all variables
      myData.raw  <- read.csv(file=FL.Data.IN,sep=",",header=TRUE,stringsAsFactors=TRUE)
names(myData.raw) <- row.titles

#
# remove data from segment 5 and 6
#
myData.raw <- myData.raw[(myData.raw[,"Seg"] != 5) & (myData.raw[,"Seg"] != 6),]

# **** added a [binary] field **** 
myData.raw <- cbind(myData.raw,binary = rep("no",dim(myData.raw)[1]),stringsAsFactors=FALSE)
myData.raw[myData.raw[,"Pal_cnt"] > 0,"binary"] <- "Yes"

#
#
var.retained <- c("D_dist_up","Chan_wid","T_dist_up","Grade_10_RM","VF_wid","Temp","Depth_Mean","Pal_cnt","Stk_12_cnt","Stk_24_cnt","Stk_36_cnt","Stk_60_cnt","Stk_120_cnt")
var.retained <- c("Pal_cnt","D_dist_up","Chan_wid","T_dist_up","Grade_10_RM","VF_wid","Temp","Depth_Mean")
myData.reduced <- myData.raw[myData.raw[,"MA"]=="Lower" & myData.raw[,"gear.type1"]=="TLC",var.retained]

# standardized the variables
myData.X <- myData.reduced[,grep("[^Pal_cnt]",names(myData.reduced),value=TRUE,perl=TRUE),drop=FALSE]
myData.Y <- myData.reduced[,grep("Pal_cnt",names(myData.reduced),value=TRUE,perl=TRUE),drop=FALSE]
var.mean <- as.data.frame(matrix(rep(apply(myData.X,2,mean,na.rm=TRUE),dim(myData.X)[1]),nrow=dim(myData.X)[1],byrow=TRUE))
var.sd   <- as.data.frame(matrix(rep(apply(myData.X,2,sd,na.rm=TRUE),  dim(myData.X)[1]),nrow=dim(myData.X)[1],byrow=TRUE))


myData.XX <- (myData.X - var.mean)/var.sd

myData.reduced <- cbind(myData.Y,myData.XX)

save(myData.reduced,file=FL.Data.OBJ )


# -------------------------------------------------------------------------------------------------
# output the other format used by WinBUGS or BUGS
# -------------------------------------------------------------------------------------------------
cat("DATA(LIST)\n",FL.Data.OUT,append=TRUE)
cat("list(n=",dim(myData.reduced)[1],",\n",file=FL.Data.OUT,append=TRUE)
idx <- 0
for (col.name in names(myData.reduced))
{
	idx <- idx + 1
	if (idx == dim(myData.reduced)[2])
	{
		A <- paste(col.name,paste(" = c(",paste(myData.reduced[,col.name],collapse=","),")",sep=""),")\n",sep="")
	}else{
		A <- paste(col.name,paste(" = c(",paste(myData.reduced[,col.name],collapse=","),")",sep=""),",\n",sep="")
	}
	cat(A,file=FL.Data.OUT,append=TRUE)
}
cat("data presented in WinBUGS/BUGS format has been outputted!\n")
cat("data presented in WinBUGS/BUGS format has been outputted!\n",file=FL.LOG,append=TRUE)




die


# # -------------------------------------------------------------------------------------------------
# # (2) added a "Lith_1" filed: re-assign categories for "Rocktype1"
# # -------------------------------------------------------------------------------------------------
# myData.raw[,"Lith_1"] <- rep("NA",dim(myData.raw)[1])
# 
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"Rocktype1"]=="alluvium",   "Lith_1"] <- "alluvium"
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"Rocktype1"]=="clay or mud","Lith_1"] <- "clay_or_mud"
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"Rocktype1"]=="silt",       "Lith_1"] <- "silt"
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"Lith_1"]   =="NA",         "Lith_1"] <- "other"
# 
# 
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"Rocktype1"]=="limestone",  "Lith_1"] <- "limestone"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"Rocktype1"]=="clay or mud","Lith_1"] <- "clay_or_mud"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"Rocktype1"]=="shale",      "Lith_1"] <- "shale"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"Lith_1"]   =="NA",         "Lith_1"] <- "sandstone"
# if (!(is.factor(myData.raw[,"Lith_1"]))){myData.raw[,"Lith_1"] <- as.factor(myData.raw[,"Lith_1"])}
# 
# # -------------------------------------------------------------------------------------------------
# (3) added a "NFHAP_Scr" filed: re-assign categories for "NFHAP_Scr"
# -------------------------------------------------------------------------------------------------
# myData.raw[,"NFHAP_new"] <- rep("NA",dim(myData.raw)[1])
# 
# myData.raw[myData.raw[,"NFHAP_Scr"]=="Low",      "NFHAP_new"]                              <- "Low"
# myData.raw[myData.raw[,"NFHAP_Scr"]=="High",     "NFHAP_new"]                              <- "High"
# myData.raw[myData.raw[,"NFHAP_Scr"]=="Very High","NFHAP_new"]                              <- "High"
# myData.raw[myData.raw[,"NFHAP_Scr"]=="Moderate", "NFHAP_new"]                              <- "Moderate"
# myData.raw[myData.raw[,"NFHAP_Scr"]=="Not Scored / Unavailable at this scale","NFHAP_new"] <- "Low"
# 
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"NFHAP_Scr"]=="clay or mud","NFHAP_new"] <- "clay_or_mud"
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"NFHAP_Scr"]=="silt",       "NFHAP_new"] <- "silt"
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"NFHAP_new"]   =="NA",    "NFHAP_new"] <- "other"
# if (!(is.factor(myData.raw[,"NFHAP_new"]))){myData.raw[,"NFHAP_new"] <- as.factor(myData.raw[,"NFHAP_new"])}
# 
# 
# # -------------------------------------------------------------------------------------------------
# # (4) added a "Tax_part_new" field: re-assign categories for "Tax_part_sz"
# # -------------------------------------------------------------------------------------------------
# myData.raw[,"Tax_part_new"] <- rep("NA",dim(myData.raw)[1])
# 
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"taxpartsiz"]=="loamy",     "Tax_part_new"] <- "loamy"
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"taxpartsiz"]=="fine-loamy","Tax_part_new"] <- "fine-loamy"
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"taxpartsiz"]=="fine-silty","Tax_part_new"] <- "fine-loamy"
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"taxpartsiz"]=="fine",      "Tax_part_new"] <- "fine"
# myData.raw[myData.raw[,"Reach"]=="Upper" & myData.raw[,"taxpartsiz"]=="clayey",    "Tax_part_new"] <- "loamy"
# 
# 
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"taxpartsiz"]=="fine",                    "Tax_part_new"] <- "fine"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"taxpartsiz"]=="fine-loamy",              "Tax_part_new"] <- "fine"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"taxpartsiz"]=="fine-silty",              "Tax_part_new"] <- "fine"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"taxpartsiz"]=="coarse-silty",            "Tax_part_new"] <- "coarse-silty"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"taxpartsiz"]=="coarse-silty over clayey","Tax_part_new"] <- "coarse-silty"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"taxpartsiz"]=="clayey over loamy",       "Tax_part_new"] <- "clayey over loamy"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"taxpartsiz"]=="clayey",                  "Tax_part_new"] <- "clayey over loamy"
# myData.raw[myData.raw[,"Reach"]=="Lower" & myData.raw[,"taxpartsiz"]=="not used",                "Tax_part_new"] <- "clayey over loamy"
# 
# if (!(is.factor(myData.raw[,"Tax_part_new"]))){myData.raw[,"Tax_part_new"] <- as.factor(myData.raw[,"Tax_part_new"])}
# 
# # -------------------------------------------------------------------------------------------------
# # (5) added a "mean depth" field": re-assign categories for "Tax_part_sz"
# # -------------------------------------------------------------------------------------------------
# myData.raw[,"Depth.mean"] <- apply(myData.raw[,c("Depth1","Depth2","Depth3")],1,mean,na.rm=TRUE)
# 
# 
# # 
# # change some categprical variables to factors
# #
# if (!(is.factor(myData.raw[,"MicroClass"]))){myData.raw[,"MicroClass"] <- as.factor(myData.raw[,"MicroClass"])}
# if (!(is.factor(myData.raw[,"Segment"]))){myData.raw[,"Segment"] <- as.factor(myData.raw[,"Segment"])}
# if (!(is.factor(myData.raw[,"SY"]))){myData.raw[,"SY"] <- as.factor(myData.raw[,"SY"])}
# 
# 
# # read April 24, 2013 data 
# row.titles  <- c("ID",     "SY",     "FO",    "Proj",   "Un_ID",   "Gear",  "Season","Bend",   "BendRN","BendRiverMile","Near_NHD2rm_FID","Near_NHD2RM_dist_m","NHD2_RM","X1960BendID","RM1960_RM","UpDamDist","UpDamNm","DnDamDist","DnDamNm","Ch_W_Full","Ch_W_NoIsl","Braiding","UpTrib","UpTribDist","DnTrib","DTribDist","Reach", "Mean_z", "Max_z",  "Min_z",  "Grade10RM","GradeBend","Rocktype1","Rocktype2","Lithology","NFHAP_Scr","MedFlow","MeanFlow","taxorder","taxpartsiz","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Segment","nPage",  "TotalPages","SetDate",  "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turbidity","Conductivity","DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "DecimalTimeDifference","StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3", "Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","VelocityBot2","VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Total_Fish_Count","Pal_cnt","CPUE",   "CPU_Area","Alt_Pal_cnt","Alt_CPUE","Alt_CPU_Area","S_U10_D2_Near","S_U10_D2_Cumu","S_U20_D4_Near","S_U20_D4_Cumu","S_U30_D6_Near","S_U30_D6_Cumu","S_U50_D10_Near","S_U50_D10_Cumu","S_U100_D20_Near","S_U100_D20_Cumu","gear.type1","gear.type2","macro.type","binary", "Tax_part_new","NFHAP_new","Lith_1","Depth.mean")
# colClasses  <- c("integer","integer","factor","integer","integer", "factor","factor","integer","factor","numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",  "numeric",  "factor", "numeric",  "factor", "numeric",  "numeric",   "integer", "factor","numeric",   "factor","numeric",  "factor","numeric","numeric","numeric","numeric",  "numeric",  "factor",   "factor",   "factor",   "factor",   "numeric","numeric", "factor",  "factor",    "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "integer","integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric","numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",        "factor",    "factor",    "factor",    "integer","numeric",     "factor",   "factor","factor")
# used.4.plot <- c( FALSE,    TRUE,     FALSE,   FALSE,    FALSE,     TRUE,    TRUE,    TRUE,     FALSE,   TRUE,           FALSE,            FALSE,               FALSE,    FALSE,        TRUE,       TRUE,       FALSE,    FALSE,      FALSE,    TRUE,       TRUE,        TRUE,      FALSE,   TRUE,        FALSE,   TRUE,       TRUE,    TRUE,     TRUE,     TRUE,     TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,     TRUE,      TRUE,      TRUE,        TRUE,       TRUE,         TRUE,         TRUE,         TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     FALSE,      FALSE,         FALSE,    TRUE,      FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    TRUE,    FALSE,   TRUE,    TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          TRUE,     TRUE,     TRUE,     FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,        TRUE,        TRUE,        TRUE,     TRUE,          TRUE,       TRUE,    TRUE)
# used.4.anal <- c( FALSE,    TRUE,     FALSE,   FALSE,    FALSE,     FALSE,   TRUE,    FALSE,    FALSE,   FALSE,          FALSE,            FALSE,               FALSE,    FALSE,        FALSE,      TRUE,       FALSE,    TRUE,       FALSE,    TRUE,       FALSE,       FALSE,     FALSE,   TRUE,        FALSE,   TRUE,       FALSE,   FALSE,    FALSE,    FALSE,    TRUE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,    FALSE,     FALSE,     FALSE,       TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     FALSE,      FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,   FALSE,    FALSE,   FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,    FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       FALSE,             TRUE,               FALSE,    FALSE,     FALSE,                  FALSE,     FALSE,         FALSE,          TRUE,           FALSE,          TRUE,           FALSE,          TRUE,           FALSE,           TRUE,            FALSE,            TRUE,             FALSE,       FALSE,       TRUE,        FALSE,    TRUE,          TRUE,       TRUE,    TRUE)
# 
# 
# 
# #
# # extrcat TLC data only
# #
  subset.idx <- myData.raw[,"Reach"] == "Lower" & myData.raw[,"gear.type1"] == "TLC"
  myData.TLC.lower <- myData.raw[subset.idx,used.4.anal]
  
  
  die;
  
#  
#   subset.idx <- myData.raw[,"Reach"] == "Upper" & myData.raw[,"gear.type1"] == "TLC"
#   myData.TLC.upper <- myData.raw[subset.idx,used.4.anal]
#  
#  
 myData.reduced <-  myData.TLC.lower
  
 myData.reduced <- myData.reduced[complete.cases(myData.reduced),]
#  
# 
# *************************************************************************************************
# 1. Poisson Model
# *************************************************************************************************
cat(paste("Poisson Model\n",sep=""))
cat(paste("Poisson Model\n",sep=""),file=FL.LOG,append=TRUE)
fm_pois <- glm(Pal_cnt ~ ., data = myData.reduced, family = poisson)

# estimated lambda
est.pred   <- predict(fm_pois)		# predict is the linear predictor, eta.  In Poisson the linear predictor is linked with a log function: log(mu) = eta, so the lambda in Poisson distribution is: lambda = mu = exp(eta)
est.mu     <- exp(est.pred)		# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda <- fm_pois$fitted		# lambda is mu which is the expected value: lamnda estimated using the fitted values from modeling, which should be exact the same as "est.mu"
est.zero.p <- dpois(0,est.lambda)	# the lambda defines the Posisson distribution and we can estimate the probability of zeros of each observation
est.pois   <- cbind(Pal_cnt        = myData.reduced[,"Pal_cnt"],
                    eta        = est.pred,
                    mu         = est.mu,
                    lambda     = est.lambda,
                    est.zero.p = est.zero.p)
                    
estimation.Poisson <- rbind(sum1 = apply(est.pois,2,sum),
                            sum0 = apply(est.pois,2,function(x){sum(x==0)}),
                            est.pois)
                    

par(mfrow = c(2,2))
 hist(estimation.Poisson[3:1811,"Pal_cnt"],nclass=100,xlab="Pal_cnt",ylab="frequency",main="[fm_pois]: distribution of observed count")
 hist(estimation.Poisson[3:1811,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_pois]: distribution of expected count")

 plot(estimation.Poisson[3:1811,"Pal_cnt"],estimation.Poisson[3:1811,"mu"],type="p",pch=16,cex=0.5,xlab="Pal_cnt count",ylab="expected",main="[fm_Poisson]: model fitting")
 abline(a=0,b=1,col="red")
                    

summary(fm_pois)	# note the Pr is from Wald test which might be too optimistic due to a misspecificaTION OF THE LIKELIHOOD.
      df.coef  <- data.frame(summary(fm_pois)$coefficients)
names(df.coef) <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(aic = summary(fm_pois)$aic,deviance.null = summary(fm_pois)$null.deviance,df.null = summary(fm_pois)$df.null,deviance.residual = summary(fm_pois)$deviance,df.residual=summary(fm_pois)$df.residual)

cat(paste("\nPoisson Model\n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Poisson Model,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Poisson Model,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

# TO RE-COMPUTE THE wALD TEST USING SANDWICH STANDARD ERROS
tmp <- coeftest(fm_pois, vcov = sandwich)

      df.wald  <- data.frame(tmp[1:8,])
names(df.wald) <- c("Estimate","StdError","zValue","Pr(>|z|)")

cat(paste("Poisson Model(re-calc Wald test),",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.wald,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Poisson Model is finished! ------------------------\n")



# *************************************************************************************************
# 2. Quasi-Poisson Model
# *************************************************************************************************
rm(df.coef,df.sum)
fm_qpois <- glm(Pal_cnt ~ ., data = myData.reduced, family = quasipoisson)

# estimated lambda
est.pred    <- predict(fm_qpois)	# predict is the linear predictor, eta.  In Poisson the linear predictor is linked with a log function: log(mu) = eta, so the lambda in Poisson distribution is: lambda = mu = exp(eta)
est.mu      <- exp(est.pred)		# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda  <- fm_qpois$fitted		# lambda is mu which is the expected value: lamnda estimated using the fitted values from modeling, which should be exact the same as "est.mu"
est.zero.p  <- dpois(0,est.lambda)	# the lambda defines the Posisson distribution and we can estimate the probability of zeros of each observation
est.qpois   <- cbind(Pal_cnt        = myData.reduced[,"Pal_cnt"],
                     eta        = est.pred,
                     mu         = est.mu,
                     lambda     = est.lambda,
                     est.zero.p = est.zero.p)
                    
estimation.QuasiPoisson <- rbind(sum1 = apply(est.qpois,2,sum),
                                 sum0 = apply(est.qpois,2,function(x){sum(x==0)}),
                                 est.qpois)
                    


par(mfrow = c(2,2))
 hist(estimation.QuasiPoisson[3:1811,"Pal_cnt"],nclass=100,xlab="Pal_cnt",ylab="frequency",main="[fm_qpois]: distribution of observed count")
 hist(estimation.QuasiPoisson[3:1811,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_qpois]: distribution of expected count")

 plot(estimation.QuasiPoisson[3:1811,"Pal_cnt"],estimation.QuasiPoisson[3:1811,"mu"],type="p",pch=16,cex=0.5,xlab="Pal_cnt count",ylab="expected",main="[fm_QuasiPoisson]: model fitting")
 abline(a=0,b=1,col="red")
 
                    
                    
summary(fm_qpois)							# Coefficients are exact the same but se are different so the Pr


coeftest(fm_qpois, vcov = sandwich)					# why "coeftest(fm_qpois, vcov = sandwich)" provide exact the same results as "coeftest(fm_pois, vcov = sandwich)"  


      df.coef  <- data.frame(summary(fm_qpois)$coefficients)
names(df.coef) <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(aic = summary(fm_qpois)$aic,deviance.null = summary(fm_qpois)$null.deviance,df.null = summary(fm_qpois)$df.null,deviance.residual = summary(fm_qpois)$deviance,df.residual=summary(fm_qpois)$df.residual)

cat(paste("\nQuasi-Poisson\n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Quasi-Poisson,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Quasi-Poisson,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

tmp <- coeftest(fm_qpois, vcov = sandwich)
      df.wald  <- data.frame(tmp[1:8,])
names(df.wald) <- c("Estimate","StdError","zValue","Pr(>|z|)")

cat(paste("Quasi-Poisson(re-calc Wald test),",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.wald,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Quasi-Poisson Model is finished! ------------------------\n")

# *************************************************************************************************
# 3. Negative Binomial Model
# dnbinom(x, size, prob, mu, log = FALSE)
# x:	vector of (non-negative integer) quantiles.
# q:	vector of quantiles.
# p:	vector of probabilities.
# n:	number of observations. If length(n) > 1, the length is taken to be the number required.
# size:	target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution). Must be strictly positive, need not be integer.
# prob: probability of success in each trial. 0 < prob <= 1.
# mu:	alternative parametrization via mean: see ‘Details’.
# *************************************************************************************************
rm(df.coef,df.sum)
fm_nbin <- glm.nb(Pal_cnt ~ ., data = myData.reduced)

# estimated lambda
est.pred    <- predict(fm_nbin)					# predict is the linear predictor, eta.  In Poisson the linear predictor is linked with a log function: log(mu) = eta, so the lambda in Poisson distribution is: lambda = mu = exp(eta)
est.mu      <- exp(est.pred)					# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda  <- fm_nbin$fitted					# lambda is mu which is the expected value: lamnda estimated using the fitted values from modeling, which should be exact the same as "est.mu"
est.zero.p  <- dnbinom(0,mu=est.lambda,size=fm_nbin$theta)	# the lambda defines the Posisson distribution and we can estimate the probability of zeros of each observation
est.nbin    <- cbind(Pal_cnt        = myData.reduced[,"Pal_cnt"],
                     eta        = est.pred,
                     mu         = est.mu,
                     lambda     = est.lambda,
                     est.zero.p = est.zero.p)
                    
estimation.nbin <- rbind(sum1 = apply(est.nbin,2,sum),
                         sum0 = apply(est.nbin,2,function(x){sum(x==0)}),
                         est.nbin)
                    
                    

par(mfrow = c(2,2))
 hist(estimation.nbin[3:1811,"Pal_cnt"],nclass=100,xlab="Pal_cnt",ylab="frequency",main="[fm_nbin]: distribution of observed count")
 hist(estimation.nbin[3:1811,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_nbin]: distribution of expected count")

 plot(estimation.nbin[3:1811,"Pal_cnt"],estimation.nbin[3:1811,"mu"],type="p",pch=16,cex=0.5,xlab="Pal_cnt count",ylab="expected",main="[fm_nbin]: model fitting")
 abline(a=0,b=1,col="red")
 
                    
                    
                    
                    
                    
summary(fm_nbin)
coeftest(fm_nbin, vcov = sandwich)					# so the sandwich adjustment should not be used in nb and qpois models??????


      df.coef  <- data.frame(summary(fm_nbin)$coefficients)
names(df.coef) <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(aic = summary(fm_nbin)$aic,deviance.null = summary(fm_nbin)$null.deviance,df.null = summary(fm_nbin)$df.null,deviance.residual = summary(fm_nbin)$deviance,df.residual=summary(fm_nbin)$df.residual)

cat(paste("\nNegative Binomial Model\n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Negative Binomial Model,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Negative Binomial Model,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

tmp <- coeftest(fm_nbin, vcov = sandwich)
      df.wald  <- data.frame(tmp[1:8,])
names(df.wald) <- c("Estimate","StdError","zValue","Pr(>|z|)")

cat(paste("Negative Binomial Model(re-calc Wald test),",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.wald,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Negative Binomial Model is finished! ------------------------\n")


# *************************************************************************************************
# 6. Zero-inflated negative binomial regression
#
# How to get the two estimates for each observations, i.e., the expected count and the inflated zero probability?
#
# The predict function in zeroinfl is different from the glm. (Both the fitted and predict methods can compute fitted responses in zeroinfl)
# In GLM, "predict" gives the values of the linear predictor "eta" and "fitted" gives the expected values "mu" which is "eta" after applied the mean function on the linear predictore, in Poisson, mu = exp(eta)
# In zeroinfl, "predict" gives the same thing of "fitted"
#              also, "predict" without argument gives the estimated expected values (counts) Default is "response"
#                    "predict" has 4 types, which are "response","prob","count","zero"
#                              "response": the fitted count
#                              "prob": probability to have count from 0 to the maximum observed count in the data
#                              "count":  the predicted mean from the count component (without zero inflation) 
#                              "zero":   and the predicted probability for the zero component
#                   
#                    "predict" with argument "type="prob"" gives a matrix of probability with a dimensionof N by K where N is the number of observations and K is the maximum count observed in the data.
# *************************************************************************************************
rm(df.coef.count,df.coef.zero,df.sum)
fm_zinb0 <- zeroinfl(Pal_cnt ~ ., data = myData.reduced, dist = "negbin")

# estimated lambda
est.mu          <- predict(fm_zinb0)				# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda      <- fm_zinb0$fitted				# the expected count from the model
est.zero.p      <- predict(fm_zinb0, type = "prob")[,1]		# the probability to have zero count
est.zero.infl   <- predict(fm_zinb0, type = "zero")		# the probability to have inflated zeros
est.count       <- predict(fm_zinb0, type = "count")		# the expected count without the zero inflation

est.zinb0   	<- cbind(Pal_cnt           = myData.reduced[,"Pal_cnt"],
                         mu            = est.mu,
                         lambda        = est.lambda,
                         est.zero.p    = est.zero.p,
                         est.zero.infl = est.zero.infl,
                         est.count     = est.count)
                    
estimation.zinb0 <- rbind(sum1 = apply(est.zinb0,2,sum),
                          sum0 = apply(est.zinb0,2,function(x){sum(x==0)}),
                          est.zinb0)
                    
par(mfrow = c(2,2))
 hist(estimation.zinb0[3:1811,"Pal_cnt"],nclass=100,xlab="Pal_cnt",ylab="frequency",main="[fm_zinb0]: distribution of observed count")
 hist(estimation.zinb0[3:1811,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_zinb0]: distribution of expected count")
                    
 hist(estimation.zinb0[3:1811,"est.zero.infl"],nclass=100,xlab="p.0infl",ylab="frequency",main="[fm_zinb0]:distribution of probability of inflated zero")
 plot(estimation.zinb0[3:1811,"Pal_cnt"],estimation.zinb0[3:1811,"mu"],type="p",pch=16,cex=0.5,xlab="Pal_cnt count",ylab="expected",main="[fm_zinb0]:model fitting (Zero-Inflated)")
 abline(a=0,b=1,col="red")
                    

                    
summary(fm_zinb0)



      df.coef.count  <- data.frame(summary(fm_zinb0)$coefficients$count)
      df.coef.zero   <- data.frame(summary(fm_zinb0)$coefficients$zero)
names(df.coef.count) <- c("Estimate","StdError","zValue","Pr(>|z|)")
names(df.coef.zero)  <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(df.null = fm_zinb0$df.null,df.residual=fm_zinb0$df.residual,theta = fm_zinb0$theta,loglik = fm_zinb0$loglik)

cat(paste("\nZero-inflated negative binomial \n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Zero-inflated negative binomial (count) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.count,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Zero-inflated negative binomial (zero) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.zero, file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Zero-inflated negative binomial ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Zero-inflated negative binomial regression Model is finished! ------------------------\n")



# list.2.keep <- c("SY","Season","UpDamDist","DnDamDist","Ch_W_Full","UpTribDist","DTribDist","Grade10RM","comppct_r","sandtotal_r","claytotal_r","VF_width", "Segment",  "Temp",  "MicroClass","WaterVel","Pal_cnt","S_U10_D2_Cumu","S_U20_D4_Cumu","S_U30_D6_Cumu","S_U50_D10_Cumu","S_U100_D20_Cumu","macro.type","Lith_1","NFHAP_new","Tax_part_new", "Depth.mean")
# list.2.keep <- c("Pal_cnt","D_dist_up","Chan_wid","T_dist_up");
# ,"Grade_10_RM","VF_wid","Temp","Depth_Mean","WaterVel","Pal_cnt","Stk_12_cnt","Stk_24_cnt","Stk_36_cnt","Stk_60_cnt","Stk_120_cnt")

# myData.reduced <- myData.reduced[,list.2.keep]



# *************************************************************************************************
# 8. Zero-inflated Poisson regression
# *************************************************************************************************
rm(df.coef.count,df.coef.zero,df.sum)
fm_zip0 <- zeroinfl(Pal_cnt ~ ., data = myData.reduced, dist = "poisson")

# estimated lambda
est.mu          <- predict(fm_zip0)				# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda      <- fm_zip0$fitted				# the expected count from the model
est.zero.p      <- predict(fm_zip0, type = "prob")[,1]		# the probability to have zero count
est.zero.infl   <- predict(fm_zip0, type = "zero")		# the probability to have inflated zeros
est.count       <- predict(fm_zip0, type = "count")		# the expected count without the zero inflation

est.zip0   	<- cbind(Pal_cnt           = myData.reduced[,"Pal_cnt"],
                         mu            = est.mu,
                         lambda        = est.lambda,
                         est.zero.p    = est.zero.p,
                         est.zero.infl = est.zero.infl,
                         est.count     = est.count)
                    
estimation.zip0 <- rbind(sum1 = apply(est.zip0,2,sum),
                         sum0 = apply(est.zip0,2,function(x){sum(x==0)}),
                         est.zip0)

par(mfrow = c(2,2))
 hist(estimation.zip0[3:1811,"Pal_cnt"],nclass=100,xlab="Pal_cnt",ylab="frequency",main="[fm_zinp0]: distribution of observed count")
 hist(estimation.zip0[3:1811,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_zinp0]: distribution of expected count")
                    
 hist(estimation.zip0[3:1811,"est.zero.infl"],nclass=100,xlab="p.0infl",ylab="frequency",main="[fm_zinp0]:distribution of probability of inflated zero")
 plot(estimation.zip0[3:1811,"Pal_cnt"],estimation.zip0[3:1811,"mu"],type="p",pch=16,cex=0.5,xlab="Pal_cnt count",ylab="expected",main="[fm_zinp0]:model fitting (Zero-Inflated)")
 abline(a=0,b=1,col="red")
                                                   
                    
                    
summary(fm_zip0)


      df.coef.count  <- data.frame(summary(fm_zip0)$coefficients$count)
      df.coef.zero   <- data.frame(summary(fm_zip0)$coefficients$zero)
names(df.coef.count) <- c("Estimate","StdError","zValue","Pr(>|z|)")
names(df.coef.zero)  <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(df.null = fm_zip0$df.null,df.residual=fm_zip0$df.residual,loglik = fm_zip0$loglik)

cat(paste("\nZero-inflated Poisson \n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Zero-inflated Poisson (count) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.count,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Zero-inflated Poisson (zero) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.zero, file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Zero-inflated Poisson ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Zero-inflated Poisson regression Model is finished! ------------------------\n")


































 
#
# reduce the number of categories
#
#
# convert the integer to factor
#
 if(!(is.factor( myData.TLC.lower[,"SY"]))){myData.TLC.lower[,"SY"] = as.factor(myData.TLC.lower[,"SY"])}
 if(!(is.factor( myData.TLC.lower[,"Segment"]))){myData.TLC.lower[,"Segment"] = as.factor(myData.TLC.lower[,"Segment"])}
 if(!(is.factor( myData.TLC.lower[,"MicroClass"]))){myData.TLC.lower[,"MicroClass"] = as.factor(myData.TLC.lower[,"MicroClass"])}
 


# -------------------------------------------------------------------------------------------------
# I. Check the 10 recently added variables: distance to the stocking stations
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
FL.stock.PDF <- paste(Path.Out,"Test_Brugs01_stock_dis.pdf",sep="/")	
if  (file.exists(FL.stock.PDF)){print(paste(FL.stock.PDF,"exist.Delete it!")); file.remove(FL.stock.PDF)}
pdf(file = FL.stock.PDF,paper="a4r",width=0,height=0)	



var.new      <-             grep("S_U",names(myData.raw),value=TRUE)
var.new.near <- grep("Near",grep("S_U",names(myData.raw),value=TRUE),value=TRUE)
var.new.cumu <- grep("Cumu",grep("S_U",names(myData.raw),value=TRUE),value=TRUE)


# # 1a. histogram of near distance
# plot.near1 <- histogram(myData.raw[,"S_U10_D2_Near"],  nint=100,col="red")
# plot.near2 <- histogram(myData.raw[,"S_U20_D4_Near"],  nint=100,col="red")
# plot.near3 <- histogram(myData.raw[,"S_U30_D6_Near"],  nint=100,col="red")
# plot.near4 <- histogram(myData.raw[,"S_U50_D10_Near"], nint=100,col="red")
# plot.near5 <- histogram(myData.raw[,"S_U100_D20_Near"],nint=100,col="red")
# 
# # 1b. histogram of cumu distance
# plot.cumu1 <- histogram(myData.raw[,"S_U10_D2_Cumu"],  nint=100,col="blue")
# plot.cumu2 <- histogram(myData.raw[,"S_U20_D4_Cumu"],  nint=100,col="blue")
# plot.cumu3 <- histogram(myData.raw[,"S_U30_D6_Cumu"],  nint=100,col="blue")
# plot.cumu4 <- histogram(myData.raw[,"S_U50_D10_Cumu"], nint=100,col="blue")
# plot.cumu5 <- histogram(myData.raw[,"S_U100_D20_Cumu"],nint=100,col="blue")
# 
# plot(plot.near1,split=c(1,1,5,2))
# plot(plot.near2,split=c(2,1,5,2),newpage=FALSE)	
# plot(plot.near3,split=c(3,1,5,2),newpage=FALSE)	
# plot(plot.near4,split=c(4,1,5,2),newpage=FALSE)	
# plot(plot.near5,split=c(5,1,5,2),newpage=FALSE)	
# 
# plot(plot.cumu1,split=c(1,2,5,2),newpage=FALSE)	
# plot(plot.cumu2,split=c(2,2,5,2),newpage=FALSE)	
# plot(plot.cumu3,split=c(3,2,5,2),newpage=FALSE)	
# plot(plot.cumu4,split=c(4,2,5,2),newpage=FALSE)	
# plot(plot.cumu5,split=c(5,2,5,2),newpage=FALSE)	
# 
#
# 1. histogram of the distance variables
par(mfrow = c(5,2))
hist(myData.raw[,"S_U10_D2_Near"],  xlab="",ylab = paste("S_U10_D2_Near",  sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U10_D2_Cumu"],  xlab="",ylab = paste("S_U10_D2_Cumu",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	

hist(myData.raw[,"S_U20_D4_Near"],  xlab="",ylab = paste("S_U20_D4_Near",  sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U20_D4_Cumu"],  xlab="",ylab = paste("S_U20_D4_Cumu",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	

hist(myData.raw[,"S_U30_D6_Near"],  xlab="",ylab = paste("S_U30_D6_Near",  sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U30_D6_Cumu"],  xlab="",ylab = paste("S_U30_D6_Cumu",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	

hist(myData.raw[,"S_U50_D10_Near"], xlab="",ylab = paste("S_U50_D10_Near", sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U50_D10_Cumu"], xlab="",ylab = paste("S_U50_D10_Cumu", sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	

hist(myData.raw[,"S_U100_D20_Near"],xlab="",ylab = paste("S_U100_D20_Near",sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U100_D20_Cumu"],xlab="",ylab = paste("S_U100_D20_Cumu",sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	




# 2a. scatter plot among different radiu of near distance
par(mfrow = c(2,1))
  plot(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U50_D10_Near"],col="black",main="black: U50, cyan:U30, magenta: U20, green: U10");abline(a=0,b=1,col="red")
points(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U30_D6_Near"], col="cyan")
points(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U20_D4_Near"], col="magenta")
points(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U10_D2_Near"], col="green")

# 2b. scatter plot among different radiu of cumudistance
  plot(myData.raw[,"S_U100_D20_Cumu"],myData.raw[,"S_U50_D10_Cumu"],col="black",main="black: U50, cyan:U30, magenta: U20, green: U10");abline(a=0,b=1,col="red")
points(myData.raw[,"S_U100_D20_Cumu"],myData.raw[,"S_U30_D6_Cumu"], col="cyan")
points(myData.raw[,"S_U100_D20_Cumu"],myData.raw[,"S_U20_D4_Cumu"], col="magenta")
points(myData.raw[,"S_U100_D20_Cumu"],myData.raw[,"S_U10_D2_Cumu"], col="green")

# 3. scatter plots between near and cumu distance
par(mfrow = c(2,3))
  plot(myData.raw[,"S_U10_D2_Near"],  myData.raw[,"S_U10_D2_Cumu"],  xlab = "near",ylab="cumu",main = "U10_D2")
  plot(myData.raw[,"S_U20_D4_Near"],  myData.raw[,"S_U20_D4_Cumu"],  xlab = "near",ylab="cumu",main = "U20_D4")
  plot(myData.raw[,"S_U30_D6_Near"],  myData.raw[,"S_U30_D6_Cumu"],  xlab = "near",ylab="cumu",main = "U30_D6")
  plot(myData.raw[,"S_U50_D10_Near"], myData.raw[,"S_U50_D10_Cumu"], xlab = "near",ylab="cumu",main = "U50_D10")
  plot(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U100_D20_Cumu"],xlab = "near",ylab="cumu",main = "U100_D20")
  
  
# 4. scatter plot between stocking station distance and the sturgeon catch number
par(mfrow = c(2,3))
boxplot(S_U10_D2_Near   ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U10_D2_Near  ")
boxplot(S_U20_D4_Near   ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U20_D4_Near  ")
boxplot(S_U30_D6_Near   ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U30_D6_Near  ")
boxplot(S_U50_D10_Near  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U50_D10_Near ")
boxplot(S_U100_D20_Near ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U100_D20_Near")

par(mfrow = c(2,3))
boxplot(S_U10_D2_Cumu   ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U10_D2_Cumu  ")
boxplot(S_U20_D4_Cumu   ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U20_D4_Cumu  ")
boxplot(S_U30_D6_Cumu   ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U30_D6_Cumu  ")
boxplot(S_U50_D10_Cumu  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U50_D10_Cumu ")
boxplot(S_U100_D20_Cumu ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U100_D20_Cumu")




# check the binary variable of the stocking station distance against the sturgeon counting
# turn the NULL in the stocking variables into one category and the non-zero distance into another category
myData.raw[,"stocking"] <- rep("yes",dim(myData.raw)[1])
myData.raw[!(is.na(myData.raw[,"S_U100_D20_Near"])),"stocking"] <- "no"
# -------------------------------------------------------------------------------------------------

dev.off();



# -------------------------------------------------------------------------------------------------
# II. Check the newly assigned micro habitat variable
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
FL.microHabitat <- paste(Path.Out,"Test_Brugs01_microHabitat.pdf",sep="/")	
if  (file.exists(FL.microHabitat)){print(paste(FL.microHabitat,"exist.Delete it!")); file.remove(FL.microHabitat)}
pdf(file = FL.microHabitat,paper="a4r",width=0,height=0)	




myData.Lower.micro <- myData.raw[myData.raw[,"Reach"]=="Lower",c("macro.type","gear.type1","gear.type2","MicroClass","Pal_cnt")]
myData.Lower.micro <- cbind(myData.Lower.micro,binary = rep("no",dim(myData.Lower.micro)[1]),stringsAsFactors=FALSE)
myData.Lower.micro[myData.Lower.micro[,"Pal_cnt"] > 0,"binary"] <- "Yes"

# get the frequency
table(myData.Lower.micro[,c("binary",           "MicroClass")])
table(myData.Lower.micro[,c("Pal_cnt","MicroClass")])

dev.off()

# -------------------------------------------------------------------------------------------------
# III. Check the correlation of the three percentage to avoid closure
# -------------------------------------------------------------------------------------------------
FL.sandClay <- paste(Path.Out,"Test_Brugs01_sandClay.pdf",sep="/")	
pdf(file = FL.sandClay,paper="a4r",width=0,height=0)	


if  (file.exists(FL.sandClay)){print(paste(FL.sandClay,"exist.Delete it!")); file.remove(FL.sandClay)}

plot.obj1 <- xyplot(sandtotal_r~claytotal_r,data=myData.raw,type="p",pch=16,col="red",panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.obj2 <- xyplot(sandtotal_r~claytotal_r | Reach,data=myData.raw,type="p",pch=16,col="red",panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")},as.table=TRUE)

plot(plot.obj1)
plot(plot.obj2)

dev.off()





# since the missing entries (-9999, NaN, NULL) in the excel spread sheet has been set to empty, we need to reset the empty entries of the factor variable to NA in order to count the missing numbers
for (idx in seq(from=1,to=dim(myData.raw)[2]))
{
	if (is.factor(myData.raw[,idx]))
	{
		myData.raw[myData.raw[,idx]=="",idx] <- NA
	}
}
cat(paste("empty entries in the factor variables have been re-assigned to [NA]\n",sep=""))



# -------------------------------------------------------------------------------------------------
# 1a. check the total data point
# -------------------------------------------------------------------------------------------------
df.data    <- NULL
number.allData <- apply(myData.raw[,1,drop=FALSE],2,length)
number.Reach   <- tapply(myData.raw[,1],list(myData.raw[,"Reach"]),  length)
number.Segment <- tapply(myData.raw[,1],list(myData.raw[,"Segment"]),length)

names(number.Segment) <- paste("segment",names(number.Segment),sep="")
names(number.allData) <- "all"
names(number.Reach)   <- sub("L","l",names(number.Reach))
names(number.Reach)   <- sub("U","u",names(number.Reach))

df.data <- data.frame(t(c(number.allData,number.Reach,number.Segment)))
row.names(df.data) <- "no.data"
cat(paste("total number of data has been counted\n",sep=""))
cat(paste("total number of data has been counted\n",sep=""),file=FL.LOG,append=TRUE)

                         
# -------------------------------------------------------------------------------------------------
# 1b. check and re-assign missing data
# -------------------------------------------------------------------------------------------------
df.missing <- NULL
number.missing  <- function(x){sum(is.na(x))}
missing.allData <- apply(myData.raw,2,number.missing)
missing.Upper   <- apply(myData.raw[myData.raw[,"Reach"]=="Upper",],2,number.missing)
missing.Lower   <- apply(myData.raw[myData.raw[,"Reach"]=="Lower",],2,number.missing)


df.missing <- data.frame(all = missing.allData,                         
                         lower = missing.Lower,
                         upper = missing.Upper)

# missing value in each segment                         
for (idx.segment in sort(unique(myData.raw[,"Segment"])))
{
	missing.data <- apply(myData.raw[myData.raw[,"Segment"]==idx.segment,],2,number.missing)
	command.string <- paste("df.missing <- cbind(df.missing,",paste("segment",idx.segment,sep="")," = missing.data)",sep="")
	eval(parse(text=command.string))
}	
cat(paste("missing data has been counted\n",sep=""))

# output the number of data/missing data
cat(paste("no. of  data,",sep=""),file=FL.missing,append=TRUE)
write.table(df.data,file=FL.missing,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Number of data have been accounted\n",sep=""))
cat(paste("Number of data have been accounted\n",sep=""),file=FL.LOG,append=TRUE)


# output the number of data/missing data
cat(paste("no. of missing data,",sep=""),file=FL.missing,append=TRUE)
write.table(df.missing,file=FL.missing,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Missing values have been accounted\n",sep=""))
cat(paste("Missing values have been accounted\n",sep=""),file=FL.LOG,append=TRUE)


# 
# check the missing data in the CPU_Area field  in term of passive and active gear type
#
myData.tmp0 <- myData.raw[,c("gear.type2","CPU_Area")]
missing.tmp0 <- tapply(myData.tmp0[,"CPU_Area"],list(myData.tmp0[,"gear.type2"]),number.missing)





# -------------------------------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------------------------------
myData <- myData.raw[,used.4.plot]
cat(paste("Only some of the variables are retained for plotting!\n",sep=""))
cat(paste("Only some of the variables are retained for plotting!\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 2a. check each individual variable: [Test_Brugs01_indVar.pdf] and [Test_Brugs01_num.sum]
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
FL.indVar.PDF <- paste(Path.Out,"Test_Brugs01_indVar.pdf",sep="/")	
if  (file.exists(FL.indVar.PDF)){print(paste(FL.indVar.PDF,"exist.Delete it!")); file.remove(FL.indVar.PDF)}
pdf(file = FL.indVar.PDF,         paper="a4r",width=0,height=0)	


mySummary <- NULL
idx <- 1
for (var in names(myData))
{
	# Categorical Varible
	if (is.factor(myData[,var]))
	{	
		no.NA <- sum(is.na(myData[,var]))
		no.NA.upper <- sum(is.na(myData[myData[,"Reach"] == "Upper",var]))
		no.NA.lower <- sum(is.na(myData[myData[,"Reach"] == "Lower",var]))
		
		myData.tmp <- myData[!(is.na(myData[,var])),var]
		myData.tmp.upper <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Upper",var]
		myData.tmp.lower <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Lower",var]
		
		# plot.all <- barplot(table(myData.tmp),xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		# layout(matrix(c(1,2),nrow=2,ncol=1),height = c(1,1))	
		# layout(rbind(c(1,1),c(0,2)),respect=rbind(FALSE,TRUE))		             
	             
	             
		plot.all   <- barchart(myData.tmp,      xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		plot.upper <- barchart(myData.tmp.upper,xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA.upper," missing values)\nUpper",sep=""))
		plot.lower <- barchart(myData.tmp.lower,xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA.lower," missing values)\nLower",sep=""))
		
		plot(plot.all,  split=c(1,1,1,2))
		plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
		plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)
		
		# output to the summary file
		cat(paste(var,",\n",sep=""),file=FL.SUM.cat,append=TRUE)
		write.table(as.data.frame(table(myData[,var])),file=FL.SUM.cat,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)		
	}else{
		no.NA <- sum(is.na(myData[,var]))
		no.NA.upper <- sum(is.na(myData[myData[,"Reach"] == "Upper",var]))
		no.NA.lower <- sum(is.na(myData[myData[,"Reach"] == "Lower",var]))
		
		myData.tmp <- myData[!(is.na(myData[,var])),var]
		myData.tmp.upper <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Upper",var]
		myData.tmp.lower <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Lower",var]

		
		# hist(myData.tmp,freq=FALSE,nclass=100,xlab=var,ylab="Density",main=paste("Distribution of ",var,"(with ",no.NA," missing values)",sep=""))
		plot.all   <- histogram(myData.tmp,      freq=FALSE,nint=100, xlab=var,ylab="Density",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		plot.upper <- histogram(myData.tmp.upper,freq=FALSE,nint=100, xlab=var,ylab="Density",main=paste("Distribution of ",var,"(with ",no.NA.upper," missing values)\nUpper",sep=""))
		plot.lower <- histogram(myData.tmp.lower,freq=FALSE,nint=100, xlab=var,ylab="Density",main=paste("Distribution of ",var,"(with ",no.NA.lower," missing values)\nLower",sep=""))
		
		plot(plot.all,  split=c(1,1,1,2))
		plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
		plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)		
		
		#
		# get summary of the data
		mySummary <- cbind(
			     data.frame(min.all = apply(myData[,var,drop=FALSE],2,min,na.rm=TRUE),
					med.all = apply(myData[,var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
					max.all = apply(myData[,var,drop=FALSE],2,max,na.rm=TRUE),
					sd.all  = apply(myData[,var,drop=FALSE],2,sd,na.rm=TRUE),
					no.all  = apply(myData[,var,drop=FALSE],2,function(x){length(x)}),
					NA.all  = apply(myData[,var,drop=FALSE],2,function(x){sum(is.na(x))})),
			     data.frame(min.upper = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,min,na.rm=TRUE),
					med.upper = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
					max.upper = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,max,na.rm=TRUE),
					sd.upper  = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,sd,na.rm=TRUE),
					no.upper  = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,function(x){length(x)}),
					NA.upper  = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,function(x){sum(is.na(x))})),
			     data.frame(min.lower = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,min,na.rm=TRUE),
					med.lower = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
					max.lower = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,max,na.rm=TRUE),
					sd.lower  = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,sd,na.rm=TRUE),
					no.lower  = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,function(x){length(x)}),
					NA.lower  = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,function(x){sum(is.na(x))})))                        
		
		if (idx == 1)
		{
			cat(",",file=FL.SUM.num,append=TRUE)
			write.table(mySummary,file=FL.SUM.num,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
		}else{
			write.table(mySummary,file=FL.SUM.num,sep=",",col.names=FALSE,row.names=TRUE,append=TRUE)
		}
		
	}
	
	# make a table for missing data
	idx <- idx + 1
}
cat(paste("each retained individual variable has been checked!\n",sep=""))
cat(paste("each retained individual variable has been checked!\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 2b. check the correlation between small group of variables: [Test_Brugs01_indVar.pdf]
# -------------------------------------------------------------------------------------------------
# 1. Ch_W_Full and CH_W_Nolsl
myData.subset <- myData[,c("Ch_W_Full","Ch_W_NoIsl","Reach")]
cor.cef.all   <- cor(myData.subset[,c("Ch_W_Full","Ch_W_NoIsl")],use="pairwise.complete.obs")[1,2]
cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Ch_W_Full","Ch_W_NoIsl")],use="pairwise.complete.obs")[1,2]
cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Ch_W_Full","Ch_W_NoIsl")],use="pairwise.complete.obs")[1,2]

	plot.all   <- xyplot(myData.subset[,"Ch_W_Full"]                                   ~ myData.subset[,"Ch_W_NoIsl"],                                   type = "p", pch=16,col="black",xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Correlation between the two Channel Width variables (corcoef=",round(cor.cef.all,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_NoIsl"], type = "p", pch=16,col="red",  xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_NoIsl"], type = "p", pch=16,col="red",  xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot(plot.all,  split=c(1,1,1,2))
	plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
	plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)

	# 2. Grad10RM and GradBend
	myData.subset <- myData[,c("Grade10RM","GradeBend","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("Grade10RM","GradeBend")],use="pairwise.complete.obs")[1,2]
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Grade10RM","GradeBend")],use="pairwise.complete.obs")[1,2]
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Grade10RM","GradeBend")],use="pairwise.complete.obs")[1,2]

	plot.all   <- xyplot(myData.subset[,"Grade10RM"]                                   ~ myData.subset[,"GradeBend"],                                   type = "p", pch=16,col="black",xlab="GradeBend",ylab="Grade10RM",main=paste("Correlation between the two River Garde Variables (corcoef=",round(cor.cef.all,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Grade10RM"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","GradeBend"], type = "p", pch=16,col="red",  xlab="GradeBend",ylab="Grade10RM",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Grade10RM"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","GradeBend"], type = "p", pch=16,col="red",  xlab="GradeBend",ylab="Grade10RM",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot(plot.all,  split=c(1,1,1,2))
	plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
	plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)

	# 3. "MedFlow","MeanFlow",
	myData.subset <- myData[,c("MedFlow","MeanFlow","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("MedFlow","MeanFlow")],use="pairwise.complete.obs")[1,2]
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("MedFlow","MeanFlow")],use="pairwise.complete.obs")[1,2]
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("MedFlow","MeanFlow")],use="pairwise.complete.obs")[1,2]

	plot.all   <- xyplot(myData.subset[,"MedFlow"]                                   ~ myData.subset[,"MeanFlow"],                                   type = "p", pch=16,col="black",xlab="MeanFlow",ylab="MedFlow",main=paste("Correlation between the two River Flows (corcoef=",round(cor.cef.all,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","MedFlow"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","MeanFlow"], type = "p", pch=16,col="red",  xlab="MeanFlow",ylab="MedFlow",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","MedFlow"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","MeanFlow"], type = "p", pch=16,col="red",  xlab="MeanFlow",ylab="MedFlow",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot(plot.all,  split=c(1,1,1,2))
	plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
	plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)


	# 4. Mean_z, min_z, max_z
	myData.subset <- myData[,c("Mean_z","Min_z","Max_z","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("Mean_z","Min_z","Max_z")],use="pairwise.complete.obs")
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Mean_z","Min_z","Max_z")],use="pairwise.complete.obs")
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Mean_z","Min_z","Max_z")],use="pairwise.complete.obs")

	cor.string.all   <- paste("cor among elevation vars are ",round(cor.cef.all[1,2],digits=2),  " and ",round(cor.cef.all[1,3],digits=2),  " and ",round(cor.cef.all[2,3],digits=2),  " (all data)",sep="")
	cor.string.upper <- paste("cor among elevation vars are ",round(cor.cef.upper[1,2],digits=2)," and ",round(cor.cef.upper[1,3],digits=2)," and ",round(cor.cef.upper[2,3],digits=2)," (Upper Stream)",sep="")
	cor.string.lower <- paste("cor among elevation vars are ",round(cor.cef.lower[1,2],digits=2)," and ",round(cor.cef.lower[1,3],digits=2)," and ",round(cor.cef.lower[2,3],digits=2)," (Lower Stream)", sep="")

	par(mfrow = c(3,1))
	plot(myData.subset[,c("Mean_z")],col=c("red"),type="l",lty=1,ylab="mean/min/max elevation (all data)",main = cor.string.all)
	lines(myData.subset[,c("Min_z")],col=c("blue"),type="l",lty=1)
	lines(myData.subset[,c("Max_z")],col=c("green"),type="l",lty=1)

	# for upper stream
	plot(myData.subset[myData.subset[,"Reach"] == "Upper",c("Mean_z")],col=c("red"),type="l",lty=1,ylab="mean/min/max elevation (Upper Stream)",main = cor.string.upper)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Min_z")],col=c("blue"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Max_z")],col=c("green"),type="l",lty=1)

	# for lower stream
	plot(myData.subset[myData.subset[,"Reach"] == "Lower",c("Mean_z")],col=c("magenta"),type="l",lty=1,ylab="mean/min/max elevation (Lower Stream)",main = cor.string.lower)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Min_z")],col=c("cyan"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Max_z")],col=c("light green"),type="l",lty=1)


	# 5. Depth1, Depth2, Depth3
	myData.subset <- myData[,c("Depth1","Depth2","Depth3","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("Depth1","Depth2","Depth3")],use="pairwise.complete.obs")
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Depth1","Depth2","Depth3")],use="pairwise.complete.obs")
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Depth1","Depth2","Depth3")],use="pairwise.complete.obs")

	cor.string.all   <- paste("cor among three Depths vars are ",round(cor.cef.all[1,2],digits=2),  " and ",round(cor.cef.all[1,3],digits=2),  " and ",round(cor.cef.all[2,3],digits=2),  " (all data)",sep="")
	cor.string.upper <- paste("cor among three Depths vars are ",round(cor.cef.upper[1,2],digits=2)," and ",round(cor.cef.upper[1,3],digits=2)," and ",round(cor.cef.upper[2,3],digits=2)," (Upper Stream)",sep="")
	cor.string.lower <- paste("cor among three Depths vars are ",round(cor.cef.lower[1,2],digits=2)," and ",round(cor.cef.lower[1,3],digits=2)," and ",round(cor.cef.lower[2,3],digits=2)," (Lower Stream)", sep="")



	par(mfrow = c(3,1))
	 plot(myData.subset[,c("Depth1")],col=c("red"),type="l",lty=1,ylab="three Depths (all data)",main = cor.string.all)
	lines(myData.subset[,c("Depth2")],col=c("blue"),type="l",lty=1)
	lines(myData.subset[,c("Depth3")],col=c("green"),type="l",lty=1)

	# for upper stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Upper",c("Depth1")],col=c("red"),type="l",lty=1,ylab="three Depths (Upper Stream)",main = cor.string.upper)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Depth2")],col=c("blue"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Depth3")],col=c("green"),type="l",lty=1)

	# for lower stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Lower",c("Depth1")],col=c("magenta"),type="l",lty=1,ylab="three Depths (Lower Stream)",main = cor.string.lower)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Depth2")],col=c("cyan"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Depth3")],col=c("light green"),type="l",lty=1)


	# 6a. May 6, 2013: check the correlation of the newly added distance to the stock stations
	myData.subset <- myData[,c("S_U10_D2_Near","S_U20_D4_Near","S_U30_D6_Near","S_U50_D10_Near","S_U100_D20_Near","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("S_U10_D2_Near","S_U20_D4_Near","S_U30_D6_Near","S_U50_D10_Near","S_U100_D20_Near")],use="pairwise.complete.obs")
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U10_D2_Near","S_U20_D4_Near","S_U30_D6_Near","S_U50_D10_Near","S_U100_D20_Near")],use="pairwise.complete.obs")
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U10_D2_Near","S_U20_D4_Near","S_U30_D6_Near","S_U50_D10_Near","S_U100_D20_Near")],use="pairwise.complete.obs")

	cor.string.all   <- paste("cor among three stock station distnace vars are ",round(cor.cef.all[1,2],digits=2),  " and ",round(cor.cef.all[1,3],digits=2),  " and ",round(cor.cef.all[2,3],digits=2),  " (all data)",sep="")
	cor.string.upper <- paste("cor among three stock station distnace vars are ",round(cor.cef.upper[1,2],digits=2)," and ",round(cor.cef.upper[1,3],digits=2)," and ",round(cor.cef.upper[2,3],digits=2)," (Upper Stream)",sep="")
	cor.string.lower <- paste("cor among three stock station distnace vars are ",round(cor.cef.lower[1,2],digits=2)," and ",round(cor.cef.lower[1,3],digits=2)," and ",round(cor.cef.lower[2,3],digits=2)," (Lower Stream)", sep="")


	par(mfrow = c(3,1))
	 plot(myData.subset[,c("S_U10_D2_Near")],  col=c("red"),    type="l",lty=1,ylab="five near stock dists (all data)",main = cor.string.all)
	lines(myData.subset[,c("S_U20_D4_Near")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[,c("S_U30_D6_Near")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[,c("S_U50_D10_Near")], col=c("magenta"),type="l",lty=1)
	lines(myData.subset[,c("S_U100_D20_Near")],col=c("cyan"),   type="l",lty=1)

	# for upper stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U10_D2_Near")],  col=c("red"),    type="l",lty=1,ylab="five near stock dists (Upper Stream)",main = cor.string.upper)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U20_D4_Near")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U30_D6_Near")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U50_D10_Near")], col=c("magenta"),type="l",lty=1)	
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U100_D20_Near")],col=c("cyan"),   type="l",lty=1)	

	# for lower stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U10_D2_Near")],  col=c("red"),    type="l",lty=1,ylab="five near stock dists (Lower Stream)",main = cor.string.lower)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U20_D4_Near")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U30_D6_Near")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U50_D10_Near")], col=c("magenta"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U100_D20_Near")],col=c("cyan"),   type="l",lty=1)


	
	# 6b. May 6, 2013: check the correlation of the newly added distance to the stock stations
	myData.subset <- myData[,c("S_U10_D2_Cumu","S_U20_D4_Cumu","S_U30_D6_Cumu","S_U50_D10_Cumu","S_U100_D20_Cumu","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("S_U10_D2_Cumu","S_U20_D4_Cumu","S_U30_D6_Cumu","S_U50_D10_Cumu","S_U100_D20_Cumu")],use="pairwise.complete.obs")
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U10_D2_Cumu","S_U20_D4_Cumu","S_U30_D6_Cumu","S_U50_D10_Cumu","S_U100_D20_Cumu")],use="pairwise.complete.obs")
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U10_D2_Cumu","S_U20_D4_Cumu","S_U30_D6_Cumu","S_U50_D10_Cumu","S_U100_D20_Cumu")],use="pairwise.complete.obs")

	cor.string.all   <- paste("cor among three stock station distnace vars are ",round(cor.cef.all[1,2],digits=2),  " and ",round(cor.cef.all[1,3],digits=2),  " and ",round(cor.cef.all[2,3],digits=2),  " (all data)",sep="")
	cor.string.upper <- paste("cor among three stock station distnace vars are ",round(cor.cef.upper[1,2],digits=2)," and ",round(cor.cef.upper[1,3],digits=2)," and ",round(cor.cef.upper[2,3],digits=2)," (Upper Stream)",sep="")
	cor.string.lower <- paste("cor among three stock station distnace vars are ",round(cor.cef.lower[1,2],digits=2)," and ",round(cor.cef.lower[1,3],digits=2)," and ",round(cor.cef.lower[2,3],digits=2)," (Lower Stream)", sep="")
	
	par(mfrow = c(3,1))
	 plot(myData.subset[,c("S_U10_D2_Cumu")],  col=c("red"),    type="l",lty=1,ylab="five cum stock dists (all data)",main = cor.string.all)
	lines(myData.subset[,c("S_U20_D4_Cumu")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[,c("S_U30_D6_Cumu")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[,c("S_U50_D10_Cumu")], col=c("magenta"),type="l",lty=1)
	lines(myData.subset[,c("S_U100_D20_Cumu")],col=c("cyan"),   type="l",lty=1)

	# for upper stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U10_D2_Cumu")],  col=c("red"),    type="l",lty=1,ylab="five cum stock dists (Upper Stream)",main = cor.string.upper)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U20_D4_Cumu")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U30_D6_Cumu")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U50_D10_Cumu")], col=c("magenta"),type="l",lty=1)	
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U100_D20_Cumu")],col=c("cyan"),   type="l",lty=1)	

	# for lower stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U10_D2_Cumu")],  col=c("red"),    type="l",lty=1,ylab="five cum stock dists (Lower Stream)",main = cor.string.lower)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U20_D4_Cumu")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U30_D6_Cumu")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U50_D10_Cumu")], col=c("magenta"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U100_D20_Cumu")],col=c("cyan"),   type="l",lty=1)


		



	
	

dev.off()	# close (FL.indVar.PDF)
cat(paste("some groups of variables have been analyzed!\n",sep=""))
cat(paste("some groups of variables have been analyzed!\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 3. check correlation between numeric covariates: [Test_Brugs01_Correlation.pdf]
# -------------------------------------------------------------------------------------------------
FL.Correlation.PDF  <- paste(Path.Out,"Test_Brugs01_Correlation.pdf",sep="/")	
if  (file.exists(FL.Correlation.PDF)) {print(paste(FL.Correlation.PDF, "exist.Delete it!")); file.remove(FL.Correlation.PDF)}
pdf(file = FL.Correlation.PDF,         paper="a4r",width=0,height=0)	
# y.vars <- c("RM1960_RM","UpDamDist","Ch_W_Full","UpTribDist","DTribDist","Mean_z", "Grade10RM","MedFlow","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Temp",   "Distance","Depth1", "Depth2", "Depth3","S_U10_D2_Near","S_U10_D2_Cumu","S_U20_D4_Near","S_U20_D4_Cumu","S_U30_D6_Near","S_U30_D6_Cumu","S_U50_D10_Near","S_U50_D10_Cumu","S_U100_D20_Near","S_U100_D20_Cumu")
  y.vars <- c("RM1960_RM","UpDamDist","Ch_W_Full","UpTribDist","DTribDist","Mean_z", "Grade10RM","MedFlow","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Temp",   "Distance","Depth1", "Depth2", "Depth3")
x.var  <- c("BendRiverMile")
	# 1. scatter plot against BendRiverMiles
	for (y.var in y.vars)
	{
		myData.reduced <- myData.raw[,c(x.var,y.var,"Reach")]
		command.string <- paste("plot.obj1 <- xyplot(",y.var," ~ ",x.var,", data = myData.reduced,xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(All) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
		eval(parse(text=command.string))

		command.string <- paste("plot.obj2 <- xyplot(",y.var," ~ ",x.var,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Upper\",],xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(Upper) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
		eval(parse(text=command.string))			

		command.string <- paste("plot.obj3 <- xyplot(",y.var," ~ ",x.var,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Lower\",],xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(Lower) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
		eval(parse(text=command.string))

		plot(plot.obj1,split=c(1,1,2,2))
		plot(plot.obj2,split=c(2,1,2,2),newpage=FALSE)	
		plot(plot.obj3,split=c(1,2,2,2),newpage=FALSE)	
	}
	
	# 2. scatter plot of each other
	for (i in seq(from=1,to=(length(y.vars)-1)))
	{
		for (j in seq(from = (i+1),to = length(y.vars)))
		{
			x.var <- y.vars[i]
			y.var <- y.vars[j]
			
			myData.reduced <- myData.raw[,c(x.var,y.var,"Reach")]
			command.string <- paste("plot.obj1 <- xyplot(",y.var," ~ ",x.var,", data = myData.reduced,xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(All) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
			eval(parse(text=command.string))

			command.string <- paste("plot.obj2 <- xyplot(",y.var," ~ ",x.var,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Upper\",],xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(Upper) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
			eval(parse(text=command.string))			

			command.string <- paste("plot.obj3 <- xyplot(",y.var," ~ ",x.var,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Lower\",],xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(Lower) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
			eval(parse(text=command.string))

			plot(plot.obj1,split=c(1,1,2,2))
			plot(plot.obj2,split=c(2,1,2,2),newpage=FALSE)	
			plot(plot.obj3,split=c(1,2,2,2),newpage=FALSE)	
		}
	}	
	
dev.off()
cat(paste("correlation between covariates!\n",sep=""))
cat(paste("correlation between covariates!\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 4. count summary: FL.Summary == [Test_Brugs01_summary.csv]
# -------------------------------------------------------------------------------------------------
myData.sub <- melt(myData,id.var=c("SY","Reach","Season","Bend","Macro","Meso","Segment","Gear","gear.type1","gear.type2","macro.type","Rocktype1","taxpartsiz","MicroClass"),measure.var="Pal_cnt")

sum.SY.Count      <- as.data.frame(cast(myData.sub,SY         ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across sample year   [SY]
sum.Reach.Count   <- as.data.frame(cast(myData.sub,Reach      ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Reach         [Reach]
sum.Season.Count  <- as.data.frame(cast(myData.sub,Season     ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Season        [Season]
sum.Macro1.Count  <- as.data.frame(cast(myData.sub,Macro      ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Macro         [Macro]
sum.Macro2.Count  <- as.data.frame(cast(myData.sub,macro.type ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Macro         [macro.type]
sum.Segment.Count <- as.data.frame(cast(myData.sub,Segment    ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Segment       [Segment]
sum.Gear.Count    <- as.data.frame(cast(myData.sub,Gear       ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Gear          [Gear]
sum.Gear1.Count   <- as.data.frame(cast(myData.sub,gear.type1 ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Gear.type1    [gear.type1]
sum.Gear2.Count   <- as.data.frame(cast(myData.sub,gear.type2 ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Gear.type2    [gear.type2]




#
# single variable distribution
#
cat("Count distribution in Year\n",             file=FL.Summary,append=TRUE)
write.table(sum.SY.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.SY.Count] out

cat("\n\nCount distribution in Reach\n",        file=FL.Summary,append=TRUE)
write.table(sum.Reach.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Reach.Count] out

cat("\n\nCount distribution in Season\n",       file=FL.Summary,append=TRUE)
write.table(sum.Season.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)					# write [sum.Season.Count] out

cat("\n\nCount distribution in Macro1\n",       file=FL.Summary,append=TRUE)
write.table(sum.Macro1.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)					# write [sum.Macro1.Count] out

cat("\n\nCount distribution in Macro2\n",       file=FL.Summary,append=TRUE)
write.table(sum.Macro2.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)					# write [sum.Macro2.Count] out

cat("\n\nCount distribution in Segment\n",      file=FL.Summary,append=TRUE)
write.table(sum.Segment.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)					# write [sum.Segment.Count] out

cat("\n\nCount distribution in Gear\n",         file=FL.Summary,append=TRUE)
write.table(sum.Gear.Count, file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear.Count] out

cat("\n\nCount distribution in Gear Type 1\n"  ,file=FL.Summary,append=TRUE)
write.table(sum.Gear1.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear1.Count] out

cat("\n\nCount distribution in Gear Type 2\n",  file=FL.Summary,append=TRUE)
write.table(sum.Gear2.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear2.Count] out


#
# two variable distribution
# ........ [Year] vs [Gear.Type1] ......................
cat("\n\nCount distribution in [Year] and [Gear.Type1]\n")
cat("\n\nCount distribution in [Year] and [Gear.Type1]\n",file=FL.Summary,append=TRUE)
sum.SY.Gear1.Count <- cast(myData.sub,SY ~ value | gear.type1,fun=length)								# count distribution across sample year   [SY] and [gear.type1]
for (idx in names(sum.SY.Gear1.Count))
{
	cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.SY.Gear1.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.SY.Gear1.Count] out
}

# ........ [Year] vs [Gear.Type2] ......................
cat("\n\nCount distribution in [Year] and [Gear.Type2]\n")
cat("\n\nCount distribution in [Year] and [Gear.Type2]\n",file=FL.Summary,append=TRUE)
sum.SY.Gear2.Count <- cast(myData.sub,SY ~ value | gear.type2,fun=length)								# count distribution across sample year   [SY] and [gear.type2]
for (idx in names(sum.SY.Gear2.Count))
{
	cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.SY.Gear2.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.SY.Gear2.Count] out
}

# ........ [Reach] vs [Gear.Type1] ......................
cat("\n\nCount distribution in [Reach] and [Gear.Type1]\n")
cat("\n\nCount distribution in [Reach] and [Gear.Type1]\n",file=FL.Summary,append=TRUE)
sum.Reach.Gear1.Count <- cast(myData.sub,Reach ~ value | gear.type1,fun=length)								# count distribution across Reach [Reach] and [gear.type1]
for (idx in names(sum.Reach.Gear1.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.Gear1.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.Gear1.Count] out
}

# ........ [Reach] vs [Gear.Type2] ......................
cat("\n\nCount distribution in [Reach] and [Gear.Type2]\n")
cat("\n\nCount distribution in [Reach] and [Gear.Type2]\n",file=FL.Summary,append=TRUE)
sum.Reach.Gear2.Count <- cast(myData.sub,Reach ~ value | gear.type2,fun=length)								# count distribution across Reach [Reach] and [gear.type2]
for (idx in names(sum.SY.Gear2.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.Gear2.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.Gear2.Count] out
}

# ........ [Macro2] vs [Meso] ......................
cat("\n\nCount distribution in [Macro2] and [Meso]\n")
cat("\n\nCount distribution in [Macro2] and [Meso]\n",file=FL.Summary,append=TRUE)
sum.Macro2.Meso.Count <- cast(myData.sub,macro.type ~ value | Meso,fun=length)								# count distribution across Macro2 [macro.type] and Meso [Meso]
for (idx in names(sum.Macro2.Meso.Count))
{
	cat(paste("Count distribution in [macro.type] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Macro2.Meso.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Macro2.Meso.Count] out
}

# ........ [Reach] vs [Macro] ......................
cat("\n\nCount distribution in [Reach] and [Macro]\n")
cat("\n\nCount distribution in [Reach] and [Macro]\n",file=FL.Summary,append=TRUE)
sum.Reach.Macro.Count <- cast(myData.sub,Reach ~ value | Macro,fun=length)								# count distribution across Reach [Reach] and Macro [Macro]
for (idx in names(sum.Reach.Macro.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.Macro.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.Macro.Count] out
}

# ........ [Gear.Type1] vs [Macro] ......................
cat("\n\nCount distribution in [Gear.type1] and [Macro]\n")
cat("\n\nCount distribution in [Gear.type1] and [Macro]\n",file=FL.Summary,append=TRUE)
sum.GearType1.Macro.Count <- cast(myData.sub,gear.type1 ~ value | Macro,fun=length)							# count distribution across Gear Type1 [gear.type1] and Macro [Macro]
for (idx in names(sum.GearType1.Macro.Count))
{
	cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType1.Macro.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.Macro.Count] out
}

# ........ [Gear.Type2] vs [Macro] ......................
cat("\n\nCount distribution in [Gear.type2] and [Macro]\n")
cat("\n\nCount distribution in [Gear.type2] and [Macro]\n",file=FL.Summary,append=TRUE)
sum.GearType2.Macro.Count <- cast(myData.sub,gear.type1 ~ value | Macro,fun=length)							# count distribution across Gear Type1 [gear.type2] and Macro [Macro]
for (idx in names(sum.GearType2.Macro.Count))
{
	cat(paste("Count distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType2.Macro.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.Macro.Count] out
}




# ........ [Reach] vs [macro.type] ......................
cat("\n\nCount distribution in [Reach] and [macro.type]\n")
cat("\n\nCount distribution in [Reach] and [macro.type]\n",file=FL.Summary,append=TRUE)
sum.Reach.macro.type.Count <- cast(myData.sub,Reach ~ value | macro.type,fun=length)								# count distribution across Reach [Reach] and macro.type [macro.type]
for (idx in names(sum.Reach.macro.type.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.macro.type.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.macro.type.Count] out
}

# ........ [Gear.Type1] vs [macro.type] ......................
cat("\n\nCount distribution in [Gear.type1] and [macro.type]\n")
cat("\n\nCount distribution in [Gear.type1] and [macro.type]\n",file=FL.Summary,append=TRUE)
sum.GearType1.macro.type.Count <- cast(myData.sub,gear.type1 ~ value | macro.type,fun=length)							# count distribution across Gear Type1 [gear.type1] and macro.type [macro.type]
for (idx in names(sum.GearType1.macro.type.Count))
{
	cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType1.macro.type.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.macro.type.Count] out
}

# ........ [Gear.Type2] vs [macro.type] ......................
cat("\n\nCount distribution in [Gear.type2] and [macro.type]\n")
cat("\n\nCount distribution in [Gear.type2] and [macro.type]\n",file=FL.Summary,append=TRUE)
sum.GearType2.macro.type.Count <- cast(myData.sub,gear.type1 ~ value | macro.type,fun=length)							# count distribution across Gear Type1 [gear.type2] and macro.type [macro.type]
for (idx in names(sum.GearType2.macro.type.Count))
{
	cat(paste("Count distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType2.macro.type.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.macro.type.Count] out
}





# ........ [Reach] vs [MicroClass] ......................
cat("\n\nCount distribution in [Reach] and [MicroClass]\n")
cat("\n\nCount distribution in [Reach] and [MicroClass]\n",file=FL.Summary,append=TRUE)
sum.Reach.MicroClass.Count <- cast(myData.sub,Reach ~ value | MicroClass,fun=length)								# count distribution across Reach [Reach] and MicroClass [MicroClass]
for (idx in names(sum.Reach.MicroClass.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.MicroClass.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.MicroClass.Count] out
}

# ........ [Gear.Type1] vs [MicroClass] ......................
cat("\n\nCount distribution in [Gear.type1] and [MicroClass]\n")
cat("\n\nCount distribution in [Gear.type1] and [MicroClass]\n",file=FL.Summary,append=TRUE)
sum.GearType1.MicroClass.Count <- cast(myData.sub,gear.type1 ~ value | MicroClass,fun=length)							# count distribution across Gear Type1 [gear.type1] and MicroClass [MicroClass]
for (idx in names(sum.GearType1.MicroClass.Count))
{
	cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType1.MicroClass.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.MicroClass.Count] out
}

# ........ [Gear.Type2] vs [MicroClass] ......................
cat("\n\nCount distribution in [Gear.type2] and [MicroClass]\n")
cat("\n\nCount distribution in [Gear.type2] and [MicroClass]\n",file=FL.Summary,append=TRUE)
sum.GearType2.MicroClass.Count <- cast(myData.sub,gear.type1 ~ value | MicroClass,fun=length)							# count distribution across Gear Type1 [gear.type2] and MicroClass [MicroClass]
for (idx in names(sum.GearType2.MicroClass.Count))
{
	cat(paste("Count distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType2.MicroClass.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.MicroClass.Count] out
}








# ........ [Gear.Type1] vs [Segment] ......................
cat("\n\nCount distribution in [Gear.type1] and [Segment]\n")
cat("\n\nCount distribution in [Gear.type1] and [Segment]\n",file=FL.Summary,append=TRUE)
sum.GearType1.Segment.Count <- cast(myData.sub,gear.type1 ~ value | Segment,fun=length)							# count distribution across Gear Type1 [gear.type1] and Segment [Segment]
for (idx in names(sum.GearType1.Segment.Count))
{
	cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType1.Segment.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.Segment.Count] out
}

# ........ [Gear.Type2] vs [Segment] ......................
cat("\n\nCount distribution in [Gear.Type2] and [Segment]\n")
cat("\n\nCount distribution in [Gear.Type2] and [Segment]\n",file=FL.Summary,append=TRUE)
sum.GearType2.Segment.Count <- cast(myData.sub,gear.type2 ~ value | Segment,fun=length)							# count distribution across Gear Type1 [gear.type2] and Segment [Segment]
for (idx in names(sum.GearType2.Segment.Count))
{
	cat(paste("Count distribution in [Gear.Type2] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType2.Segment.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.Segment.Count] out
}

# ........ [Year] vs [Segment] ......................
cat("\n\nCount distribution in [Year] and [Segment]\n")
cat("\n\nCount distribution in [Year] and [Segment]\n",file=FL.Summary,append=TRUE)
sum.Year.Segment.Count <- cast(myData.sub,SY ~ value | Segment,fun=length)								# count distribution across Year [Year] and Segment [Segment]
for (idx in names(sum.Year.Segment.Count))
{
	cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Year.Segment.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Year.Segment.Count] out
}
cat(paste("\n\nCount summary has been done for some single variables and some two variable combinations!\n",sep=""))
cat(paste("\n\nCount summary has been done for some single variables and some two variable combinations!\n",sep=""),file=FL.LOG,append=TRUE)


# ........ [Reach] vs [Rocktype1] ......................
cat("\n\nCount distribution in [Reach] and [Rocktype1]\n")
cat("\n\nCount distribution in [Reach] and [Rocktype1]\n",file=FL.Summary,append=TRUE)
sum.Reach.Rocktype1.Count <- cast(myData.sub,Reach ~ value | Rocktype1,fun=length)								# count distribution across Reach [Reach] and Rocktype1 [Rocktype1]
for (idx in names(sum.Reach.Rocktype1.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.Rocktype1.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.Rocktype1.Count] out
}


# ........ [Reach] vs [taxpartsiz] ......................
cat("\n\nCount distribution in [Reach] and [taxpartsiz]\n")
cat("\n\nCount distribution in [Reach] and [taxpartsiz]\n",file=FL.Summary,append=TRUE)
sum.Reach.taxpartsiz.Count <- cast(myData.sub,Reach ~ value | taxpartsiz,fun=length)								# count distribution across Reach [Reach] and taxpartsiz [taxpartsiz]
for (idx in names(sum.Reach.taxpartsiz.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.taxpartsiz.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.taxpartsiz.Count] out
}




# -------------------------------------------------------------------------------------------------
# 5. add a presence/absence variable for Pallid Sturgeon Count
# -------------------------------------------------------------------------------------------------
myData.sup <- cbind(myData,binary = rep("no",dim(myData)[1]),stringsAsFactors=FALSE)
myData.sup[myData.sup[,"Pal_cnt"] > 0,"binary"] <- "Yes"

y.vars.num <- "Pal_cnt"
# x.vars.num <- c("UpDamDist","Ch_W_Full","Ch_W_NoIsl","Braiding","UpTribDist","DTribDist","Mean_z", "Max_z",  "Min_z",  "Grade10RM","GradeBend","MedFlow","MeanFlow","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Segment","Temp",   "Distance","Depth1", "Depth2", "Depth3", "WaterVel","Depth.mean","S_U20_D4_Near","S_U20_D4_Cumu","S_U30_D6_Near","S_U30_D6_Cumu","S_U50_D10_Near","S_U50_D10_Cumu","S_U100_D20_Near","S_U100_D20_Cumu")
  x.vars.num <- c("UpDamDist","Ch_W_Full","Ch_W_NoIsl","Braiding","UpTribDist","DTribDist","Mean_z", "Max_z",  "Min_z",  "Grade10RM","GradeBend","MedFlow","MeanFlow","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Segment","Temp",   "Distance","Depth1", "Depth2", "Depth3", "WaterVel","Depth.mean")

y.vars.cat <- "binary"
x.vars.cat <- c("Gear",  "Season","Bend",  "Reach", "Rocktype1","Rocktype2","Lithology","NFHAP_Scr","taxorder","taxpartsiz","Segment","gear.type1","gear.type2","macro.type","MicroClass")

myData.reduced <- myData.sup[,c(x.vars.num,x.vars.cat,y.vars.num,y.vars.cat)]
cat(paste("\n\nAdded a pbinary variable for Pallid Sturgeon (presence/absence)!\n",sep=""))
cat(paste("\n\nAdded a pbinary variable for Pallid Sturgeon (presence/absence)!\n",sep=""),file=FL.LOG,append=TRUE)





# -------------------------------------------------------------------------------------------------
# 6. check correlation of continusous variables vs the [count] of Pallid Sturgeon: [Test_Brugs01_Count_numVar.pdf]
# -------------------------------------------------------------------------------------------------
FL.Count_num.PDF  <- paste(Path.Out,"Test_Brugs01_Count_numVar.pdf",sep="/")	
if  (file.exists(FL.Count_num.PDF)) {print(paste(FL.Count_num.PDF, "exist.Delete it!")); file.remove(FL.Count_num.PDF)}
pdf(file = FL.Count_num.PDF,         paper="a4r",width=0,height=0)	

cat(paste("check correlation of count with some variables!\n",sep=""))
cat(paste("\nCheck the distribution of continuous variables in terms of the count of Pallid Sturgeon!\n",sep=""),file=FL.Summary,append=TRUE)
cat(paste("\nCheck the distribution of continuous variables in terms of the count of Pallid Sturgeon!\n",sep=""),file=FL.LOG,    append=TRUE)
for (var.num in x.vars.num)
{
	# all data
	command.string <- paste("plot.obj1 <- xyplot(",y.vars.num," ~ ",var.num,", data = myData.reduced,xlab=\"",var.num,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
	eval(parse(text=command.string))

	command.string <- paste("plot.obj2 <- bwplot(",var.num," ~ as.factor(",y.vars.num,"), data = myData.reduced,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"red\")",sep="")
	eval(parse(text=command.string))

	# upper stream
	command.string <- paste("plot.obj3 <- xyplot(",y.vars.num," ~ ",var.num,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Upper\",],xlab=\"",var.num,"\",ylab=\"",y.vars.num,"\",main=paste(\"(Upper) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
	eval(parse(text=command.string))

	command.string <- paste("plot.obj4 <- bwplot(",var.num," ~ as.factor(",y.vars.num,"), data = myData.reduced[myData.reduced[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(Upper) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\")",sep="")
	eval(parse(text=command.string))
	
	# lower stream
	command.string <- paste("plot.obj5 <- xyplot(",y.vars.num," ~ ",var.num,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Lower\",],xlab=\"",var.num,"\",ylab=\"",y.vars.num,"\",main=paste(\"(Lower) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
	eval(parse(text=command.string))

	command.string <- paste("plot.obj6 <- bwplot(",var.num," ~ as.factor(",y.vars.num,"), data = myData.reduced[myData.reduced[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(Lower) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"green\")",sep="")
	eval(parse(text=command.string))
	
	plot(plot.obj1,split=c(1,1,2,3))
	plot(plot.obj2,split=c(2,1,2,3),newpage=FALSE)	
	plot(plot.obj3,split=c(1,2,2,3),newpage=FALSE)	
	plot(plot.obj4,split=c(2,2,2,3),newpage=FALSE)	
	plot(plot.obj5,split=c(1,3,2,3),newpage=FALSE)	
	plot(plot.obj6,split=c(2,3,2,3),newpage=FALSE)	
	
	# Segment-wise: scatter plots
	command.string <- paste("plot.obj <- xyplot(",y.vars.num," ~ ",var.num," | Segment, data = myData.reduced,xlab=\"",var.num,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)

	# Segment-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.num,") | Segment, data = myData.reduced,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"red\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	# Macro-wise: scatter plots
	command.string <- paste("plot.obj <- xyplot(",y.vars.num," ~ ",var.num," | macro.type, data = myData.reduced,xlab=\"",var.num,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)

	# Macro-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.num,") | macro.type, data = myData.reduced,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
		
}
dev.off()
cat(paste("\n\nchecked correlation of continusous variables vs the [count] of Pallid Sturgeon!\n",sep=""))
cat(paste("\n\nchecked correlation of continusous variables vs the [count] of Pallid Sturgeon!\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 7. check the correlation of discrete variables vs the [count] of Pallid Sturgeon: FL.Summary == [Test_Brugs01_summary.csv]
# -------------------------------------------------------------------------------------------------
# FL.Count_cat.PDF  <- paste(Path.Out,"Test_Brugs01_Count_catVar.pdf",sep="/")	
# if  (file.exists(FL.Count_cat.PDF)) {print(paste(FL.Count_cat.PDF, "exist.Delete it!")); file.remove(FL.Count_cat.PDF)}
# pdf(file = FL.Count_cat.PDF,         paper="a4r",width=0,height=0)	
# 
cat(paste("Check the distribution of variables in terms of Presence/Absence of Pallid Sturgeon!\n",sep=""),file=FL.Summary,append=TRUE)
cat(paste("Check the distribution of variables in terms of Presence/Absence of Pallid Sturgeon!\n",sep=""),file=FL.LOG,    append=TRUE)
for (var.cat in x.vars.cat)
{
# 	# all data
# 	command.string <- paste("plot.obj1 <- xyplot(",y.vars.num," ~ ",var.cat,", data = myData.reduced,xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj2 <- bwplot(",var.cat," ~ as.factor(",y.vars.num,"), data = myData.reduced,xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	# upper stream
# 	command.string <- paste("plot.obj3 <- xyplot(",y.vars.num," ~ ",var.cat,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Upper\",],xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(Upper) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj4 <- bwplot(",var.cat," ~ as.factor(",y.vars.num,"), data = myData.reduced[myData.reduced[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(Upper) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	
# 	# lower stream
# 	command.string <- paste("plot.obj5 <- xyplot(",y.vars.num," ~ ",var.cat,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Lower\",],xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(Lower) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj6 <- bwplot(",var.cat," ~ as.factor(",y.vars.num,"), data = myData.reduced[myData.reduced[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(Lower) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"green\")",sep="")
# 	eval(parse(text=command.string))
# 	
# 	plot(plot.obj1,split=c(1,1,2,3))
# 	plot(plot.obj2,split=c(2,1,2,3),newpage=FALSE)	
# 	plot(plot.obj3,split=c(1,2,2,3),newpage=FALSE)	
# 	plot(plot.obj4,split=c(2,2,2,3),newpage=FALSE)	
# 	plot(plot.obj5,split=c(1,3,2,3),newpage=FALSE)	
# 	plot(plot.obj6,split=c(2,3,2,3),newpage=FALSE)	
# 	
# 	# Segment-wise: scatter plots
# 	command.string <- paste("plot.obj <- xyplot(",y.vars.num," ~ ",var.cat," | Segment, data = myData.reduced,xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 
# 	# Segment-wise: boxplot plots
# 	command.string <- paste("plot.obj <- bwplot(",var.cat," ~ as.factor(",y.vars.num,") | Segment, data = myData.reduced,xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 	
# 	# Macro-wise: scatter plots
# 	command.string <- paste("plot.obj <- xyplot(",y.vars.num," ~ ",var.cat," | macro.type, data = myData.reduced,xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 
# 	# Macro-wise: boxplot plots
# 	command.string <- paste("plot.obj <- bwplot(",var.cat," ~ as.factor(",y.vars.num,") | macro.type, data = myData.reduced,xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 	
 	#
 	# make cross table
 	#
 	command.string <- paste("myData.tmp <- myData.reduced[,c(\"",var.cat,"\",\"",y.vars.cat,"\")]",sep="")
 	eval(parse(text=command.string))
 
 	tmp.table <- data.frame(table(myData.tmp))
 	command.string <- paste("tmp.df <- cast(tmp.table,",var.cat," ~ ",y.vars.cat,")",sep="")
 	eval(parse(text=command.string))
 
 	# write the frequency out
 	cat(paste(var.cat," vs ",y.vars.cat,",",sep=""),file=FL.Summary,append=TRUE)
 	write.table(tmp.df,file=FL.Summary,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
 	cat("\n\n",file=FL.Summary,append=TRUE)
 }
# dev.off()
 cat(paste("\n\nchecked correlation of discrete variables vs the [count] of Pallid Sturgeon!\n",sep=""))
 cat(paste("\n\nchecked correlation of discrete variables vs the [count] of Pallid Sturgeon!\n",sep=""),file=FL.LOG,append=TRUE)
 



# -------------------------------------------------------------------------------------------------
# 8. check correlation of continusous variables vs the [count] of Pallid Sturgeon: Test_Brugs01_Binary_numVar.pdf; 	FL.Summary == [Test_Brugs01_summary.csv]
# -------------------------------------------------------------------------------------------------
FL.binary_num.PDF  <- paste(Path.Out,"Test_Brugs01_Binary_numVar.pdf",sep="/")	
if  (file.exists(FL.binary_num.PDF)) {print(paste(FL.binary_num.PDF, "exist.Delete it!")); file.remove(FL.binary_num.PDF)}
pdf(file = FL.binary_num.PDF,         paper="a4r",width=0,height=0)	

cat(paste("check correlation of count with some variables!\n",sep=""))
cat(paste("\nCheck the distribution of continuous variables in terms of the count of Pallid Sturgeon!\n",sep=""),file=FL.Summary,append=TRUE)
cat(paste("\nCheck the distribution of continuous variables in terms of the count of Pallid Sturgeon!\n",sep=""),file=FL.LOG,    append=TRUE)
for (var.num in x.vars.num)
{
	# all data
	command.string <- paste("plot.obj1 <- bwplot(",var.num," ~ as.factor(",y.vars.cat,"), data = myData.reduced,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"red\",scale=list(y=\"free\"))",sep="")
	eval(parse(text=command.string))

	# upper stream
	command.string <- paste("plot.obj2 <- bwplot(",var.num," ~ as.factor(",y.vars.cat,"), data = myData.reduced[myData.reduced[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(Upper) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\",scale=list(y=\"free\"))",sep="")
	eval(parse(text=command.string))
	
	# lower stream
	command.string <- paste("plot.obj3 <- bwplot(",var.num," ~ as.factor(",y.vars.cat,"), data = myData.reduced[myData.reduced[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(Lower) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"green\",scale=list(y=\"free\"))",sep="")
	eval(parse(text=command.string))
	
	plot(plot.obj1,split=c(1,1,1,3))
	plot(plot.obj2,split=c(1,2,1,3),newpage=FALSE)	
	plot(plot.obj3,split=c(1,3,1,3),newpage=FALSE)	
	
	# Segment-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.cat,") | Segment, data = myData.reduced,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"red\",scale=list(y=\"free\"))",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	# Macro-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.cat,") | macro.type, data = myData.reduced,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\",scale=list(y=\"free\"))",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	
}
dev.off()
cat(paste("\n\nchecked correlation of continusous variables vs the [count] of Pallid Sturgeon!\n",sep=""))
cat(paste("\n\nchecked correlation of continusous variables vs the [count] of Pallid Sturgeon!\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 9. check the correlation of discrete variables vs the [count] of Pallid Sturgeon
# -------------------------------------------------------------------------------------------------
# FL.binary_cat.PDF  <- paste(Path.Out,"Test_Brugs01_Binary_catVar.pdf",sep="/")	
# if  (file.exists(FL.binary_cat.PDF)) {print(paste(FL.binary_cat.PDF, "exist.Delete it!")); file.remove(FL.binary_cat.PDF)}
# pdf(file = FL.binary_cat.PDF,         paper="a4r",width=0,height=0)	
# 
 cat(paste("Check the distribution of variables in terms of Presence/Absence of Pallid Sturgeon!\n",sep=""),file=FL.Summary,append=TRUE)
 cat(paste("Check the distribution of variables in terms of Presence/Absence of Pallid Sturgeon!\n",sep=""),file=FL.LOG,    append=TRUE)
 for (var.cat in x.vars.cat)
 {
# 	# all data
# 	command.string <- paste("plot.obj1 <- xyplot(",y.vars.cat," ~ ",var.cat,", data = myData.reduced,xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj2 <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,"), data = myData.reduced,xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	# upper stream
# 	command.string <- paste("plot.obj3 <- xyplot(",y.vars.cat," ~ ",var.cat,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Upper\",],xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(Upper) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj4 <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,"), data = myData.reduced[myData.reduced[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(Upper) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	
# 	# lower stream
# 	command.string <- paste("plot.obj5 <- xyplot(",y.vars.cat," ~ ",var.cat,", data = myData.reduced[myData.reduced[,\"Reach\"]==\"Lower\",],xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(Lower) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj6 <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,"), data = myData.reduced[myData.reduced[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(Lower) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"green\")",sep="")
# 	eval(parse(text=command.string))
# 	
# 	plot(plot.obj1,split=c(1,1,2,3))
# 	plot(plot.obj2,split=c(2,1,2,3),newpage=FALSE)	
# 	plot(plot.obj3,split=c(1,2,2,3),newpage=FALSE)	
# 	plot(plot.obj4,split=c(2,2,2,3),newpage=FALSE)	
# 	plot(plot.obj5,split=c(1,3,2,3),newpage=FALSE)	
# 	plot(plot.obj6,split=c(2,3,2,3),newpage=FALSE)	
# 	
# 	# Segment-wise: scatter plots
# 	command.string <- paste("plot.obj <- xyplot(",y.vars.cat," ~ ",var.cat," | Segment, data = myData.reduced,xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 
# 	# Segment-wise: boxplot plots
# 	command.string <- paste("plot.obj <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,") | Segment, data = myData.reduced,xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 	
# 	# Macro-wise: scatter plots
# 	command.string <- paste("plot.obj <- xyplot(",y.vars.cat," ~ ",var.cat," | macro.type, data = myData.reduced,xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 
# 	# Macro-wise: boxplot plots
# 	command.string <- paste("plot.obj <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,") | macro.type, data = myData.reduced,xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 	
 	#
 	# make cross table
 	#
 	command.string <- paste("myData.tmp <- myData.reduced[,c(\"",var.cat,"\",\"",y.vars.cat,"\")]",sep="")
 	eval(parse(text=command.string))
 
 	tmp.table <- data.frame(table(myData.tmp))
 	command.string <- paste("tmp.df <- cast(tmp.table,",var.cat," ~ ",y.vars.cat,")",sep="")
 	eval(parse(text=command.string))
 
 	# write the frequency out
 	cat(paste(var.cat," vs ",y.vars.cat,",",sep=""),file=FL.Summary,append=TRUE)
 	write.table(tmp.df,file=FL.Summary,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
 	cat("\n\n",file=FL.Summary,append=TRUE)
 }
# dev.off()
 cat(paste("\n\nchecked correlation of discrete variables vs the [count] of Pallid Sturgeon!\n",sep=""))
 cat(paste("\n\nchecked correlation of discrete variables vs the [count] of Pallid Sturgeon!\n",sep=""),file=FL.LOG,append=TRUE)



# #
# # to have consistent format for the summary (i.e., have Pallid Count 0 to 11 as the column header,
# # 1.
#       summary.count  <- aggregate(myData[,"Pal_cnt"],list(myData[,"Pal_cnt"]),length)
# names(summary.count) <- c("Count","value")
# cat(paste("Count distribution\n"),file=FL.Summary,append=TRUE)
# write.table(t(summary.count),file=FL.Summary,sep=",",row.names=TRUE,col.names=FALSE,append=TRUE)	# write [sum.GearType2.Macro.Count] out
# 
# 
# # 2. 
#       summary.year.count  <- aggregate(myData[,"Pal_cnt"],list(myData[,"SY"],myData[,"Pal_cnt"]),length)
# names(summary.year.count) <- c("Year","Count","value")
# tmp <- cast(summary.year.count,Year~Count,fun=sum,margins=c("grand_row","grand_col"))
# write.table(as.data.frame(tmp),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
# 
# 3. 
#       summary.year.gearType1.count  <- aggregate(myData[,"Pal_cnt"],list(myData[,"SY"],myData[,"gear.type1"],myData[,"Pal_cnt"]),length)
# names(summary.year.gearType1.count) <- c("Year","gear.type1","Count","value")
# tmp <- cast(summary.year.gearType1.count,Year~Count | gear.type1,fun=sum,rm.na=FALSE)
# write.table(as.data.frame(tmp),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
# 
# no.rows <- length(unique(myData[,"SY"]))
# no.cols <- length(unique(myData[,"Pal_cnt"]))
#  data.default <- data.frame(matrix(rep(0,no.rows * no.cols),nrow=no.rows))
#      names(data.default) <- sort(unique(myData[,"Pal_cnt"]))
#  row.names(data.default) <- sort(unique(myData[,"SY"]))
#  data.default <- cbind(Year = sort(unique(myData[,"SY"])),data.default)
 
 
 
 


# get summary of the data



# 1   objectID					should ignore.  not sure what it is.  It is unique.
# 2   SY					should keep.	Sample year (2006-2011)
# 3   FieldOffice				should ignore.  One of the 7 field office (CF, MO, NE, SD, GP, MR, MT)
# 4   Project					should ignore.  always take 1.  one of the four (01: PSPAP, 02: Habitat Assessment, 03: Chute Study-Mitigation), 04: Spring Rise Evaluation)
# 5   UniqueIdentifier				should ignore.  Four-digits (1-9999) (Required). Restarts to 1 at the beginning of every sampling year. NOTE: it is very critical that the UID’s are never the same for each Segment.
# 6   Gear					should keep.	five digit code of gear type (9 values: GN14S, GN18S, GN41S, GN48S, OT16S, TLC1S, TLC2S, TNS, TN25S) where "S" stands for "standard", "W" for "Wild Gear" and "E" for "Evaluation".  Should turn into 4 gear types, GN, OT, TLC, TN)
# 7   Season					should keep.	either ST: Sturgeon or FC: Fish Community seasons
# 8   Bend					should keep.	bend index from 1 to 87
# 9   BendRN					should ignore.	R: random selected bend and N: non-randomly selected bend.  Here we only retain the randomly selected bend.
# 10  BendRiverMile				Is it useful?	Record the upper river mile identifying the bend being sampled in the Missouri River or the river mile of the tributary being sampled. All tributaries being sampled have segment numbers.
# 11  Near_NHD2rm_FID				should ignore.  the near GIS feature to the plotted PAP catches
# 12  Near_NHD2RM_dist_m			should ignore.  the near GIS feature distance to the plotted PAP catches
# 13  NHD2_RM					should ignore.  NHD2_RM is the new river mile delineation that was created from the NHDPlus data to replace/fix the old 1960 Missouri River Miles data
# 14  1960BendID		399   -9999	GIS fields.	
# 15  RM1960_RM					GIS fields.
# 16  UpDamDist					GIS fields.
# 17  UpDamNm					GIS fields.
# 18  DnDamDist			23542 -9999	GIS fields.
# 19  DnDamNm			23542 NaN.	GIS fields.
# 20  Ch_W_Full					GIS fields.
# 21  Ch_W_NoIsl				GIS fields.
# 22  Braiding					GIS fields.
# 23  UpTrib			38    NaN.	GIS fields.
# 24  UpTribDist		38    -9999.	GIS fields.
# 25  DnTrib			104   NaN	GIS fields.
# 26  DTribDist			104   -9999	GIS fields.
# 27  Reach					GIS fields.
# 28  Mean_z					GIS fields.
# 29  Max_z					GIS fields.
# 30  Min_z					GIS fields.
# 31  Grade10RM					GIS fields.
# 32  GradeBend					GIS fields.
# 33  Rocktype1					GIS fields.
# 34  Rocktype2					GIS fields.
# 35  Lithology					GIS fields.
# 36  NFHAP_Scr					GIS fields.
# 37  MedFlow					GIS fields.
# 38  MeanFlow					GIS fields.
# 39  taxorder					GIS fields.
# 40  taxpartsiz				GIS fields.
# 41  comppct_r					GIS fields.
# 42  sandtotal_r				GIS fields.
# 43  claytotal_r				GIS fields.
# 44  frag3to10_r				GIS fields.
# 45  VF_width			12    -9999.	GIS fields.
# 46  Segment					segment 2-14.  See page 172 of PAP_Database_SOP_V15_June2010.pdf for name of the segment
# 47  nPage					should ignore.  which page of the total pages for a particular gear deployment.  Is it the page number in the log book?
# 48  TotalPages				should ignore.  the total number of pages used for a particular gear deployment.  Is it the page number in the log book?
# 49  SetDate					should ignore.  not sure what it is.  Always has a "00:00.0" value. 
# 50  Replicate			35033 NULL.	should ignore.  not sure what it is.  Always has a "NULL" value. 
# 51  ReplicateRN		35033 NULL.	should ignore.  not sure what it is.  Always has a "NULL" value.  
# 52  SubSample					should ignore.  we have value from 1 to 20:	Each gear deployment is a sub-sample. Sub-sample numbering will be consistent with the design of the Project 1 where gear deployment is guided by the habitats available within the bend. Sub-samples will be numbered 1-X for each gear by Macrohabitat/Mesohabitat combination. For example, if you were sampling in bend and made 5 trammel net drifts in the ISB and 3 in the CHXO, you would number your sub-samples as 1 through 5 for those drifts in the ISB and 1-3 for those drifted in the CHXO to achieve the minimum of 8 sub-samples for that gear.
# 53  SubSamplePass				should ignore.  We always have 1 which mean the first sub-sample in an area.  Did we deliberately delete the additional samples whenever a pallid sturgeon was caught because whenever a pallid sturgeon is collected with an active gear, it is a requirement to do two additional samples in this exact location even if the minimum distance for the gear is not achieved?????
# 54  SubSampleN		35033 NULL.	should ignore.  always "NULL".  This might be because all additional pas need to be recorded as "N": non-random and they are not included in this data compilation.
# 55  Biologist			35033 NULL.	should ignore.  not sure what it is. Always "NULL"
# 56  Recorder					should ignore.  The initials of the individual that recorded the data. The first initial of the individual’s first, middle, and last name (If no middle name, use "X" for middle initial.).
# 57  CheckBy			4     NULL.	should ignore.  The initials of the individual that checks the data sheet prior to submitting this for data entry. This must be somebody other than the recorder! The first initial of the individual’s first, middle, and last name (If no middle name, use “X” for middle initial.). The individual that is checking the data sheets should clarify in the comments section all situations where the protocols were not followed. For example, for a large collection of shovelnose sturgeon that weight data was not collected, this should be explained in the comments section referencing the unique ID and the circumstances (shovelnose sturgeon weight data was not collected…..too many fish, scale malfunction, etc.).
# 58  Temp			66    NULL.	Three digit temperature. The recorder is required to record to the nearest degree in Celsius, but has the option of record up to a single decimal place. Temperature is recorded on the day the gear is set.
# 59  Turbidity			21458 NULL.	Four digits Nephelometric Turbidity (NTU).  Record to the nearest Nephelometric Turbidity Unit (NTU). Recorded following gear retrieval.
# 60  Conductivity		35029 NULL.	Four-digits Conductivity in "microhos/cm3).  Record to the nearest microhos/cm3 . Recorded following gear retrieval.
# 61  DO			35022 NULL.	Dissolved Oxygen in mg/L: Three-digits (Not required for Project 1).  Record to one decimal place (units are parts per million (ppm) and milligrams/liter (mg/L). Recorded following gear retrieval.
# 62  Distance			6428  NULL.	Length of sample for trawling, drifting trammel, seining).  0-661 with "NULL".  Three-digits (Required for trawling, drifting trammel nets, seining when using the rectangular method and mini-fyke netting).  Record in meters, indicating length of sample. Mini-fyke netting distance is measured perpendicular to the bank line to the midline of the first cab. This measurement will be recorded in centimeters for mini-fyke nets. When retrieving the trotline, the number of hooks that fished is recorded in the DISTANCE box on the front of the data sheet.
# 63  Width			35031 NULL.	The width of the seine hauls.  Two values: 7.5 and 15.9 with "NULL".  Three-digits (Only required for Seining).  Record width in tenths of a meter for the width of the seine hauls (not the length of the seine).
# 64  NetRiverMile		12948 NULL.	0.1 to 1759.4 miles.  Record the river mile location identifying where the sample was collected in the mainstem Missouri or the river mile of the tributary if sampling a tributary. Should be recorded to the nearest .1 of a mile based on the quality of the maps available.
# 65  StructureNumber		25559 NULL.	9 digits code.  Character string with "NULL". Record the Corps dikes or other structures number (based on 1890 river mileage). An extra box was added after the decimal point for the number structures involving additional letters or numbers.
# 66  USGS			24714 NULL.	Eight digit code with "NULL".  USGS Gauge Station Code - The nearest USGS gauge to the sampling location should be used. If sampling near a tributary, use the nearest gauging station that represents the discharge at the location you are sampling. For example, if sampling a bend above a major trib, there's a possibility that the "nearest" gauge will have tributary influence thus a higher discharge and stage. The gauge should be read in conjunction with the time that the net was set for passive gears
# 67  RiverStage		28089 NULL.	three digits. 2.2 to 19.4 with "NULL".  Daily gauge height in feet
# 68  Discharge			25347 NULL.	six digit.  daily discharge in cubic feet per second (CFS)
# 69  U1			34477 NULL.	Utility box 1: was being used in conjunction with the PIT Tagging/Scute Marking effort in the Fort Peck Reach.
# 70  U2			31748 NULL.	Utility box 2: initial hook number for trotline
# 71  U3			34742 NULL.	Utility box 3: Push Trawl/Minifyke proximity evaluation
# 72  U4			34535 NULL.	Utility box 4: was being used in conjunction with the PIT Tagging/Scute Marking effort in the Fort Peck Reach.
# 73  U5			34573 NULL.	Utility box 5: was being used in conjunction with the PIT Tagging/Scute Marking effort in the Fort Peck Reach.
# 74  U6			35017 NULL.	Utility box 6: If a net does not fish properly, but catches fish, MNCF will be recorded in this utility box and the actual fish species codes will be recorded on the back of the standard data sheet.
# 75  U7			34767 NULL.	Utility box 7: For targeted pallid sturgeon broodstock collection efforts, “BS” (Broodstock) will be recorded for all gear deployments
# 76  Macro					four digits code: 14 options
# 77  MacroSW			35033 NULL.	should ignore.  NULL
# 78  Meso					four digits code: 6 options
# 79  MesoSW			35033 NULL.	should ignore.  NULL
# 80  Micro			16232 NULL.	six digits code: (required for segment 8-14). 
# 81  StartTime			600   NULL.	should ignore.  Four-digits (Required)
# 82  StopTime			17764 NULL.	should ignore.  Four-digits (Required for passive gears only)
# 83  DecimalTimeDifference	278   NULL.	not sure what it is.
# 84  StartLatitude				should ignore.  
# 85  StopLatitude		8966  NULL.	should ignore.  
# 86  StartLongitude				should ignore.  
# 87  StopLongitude		8970  NULL.	should ignore.  
# 88  Depth1			28    NULL.	Why we have three depth variables
# 89  Depth2			41    NULL.	Why we have three depth variables
# 90  Depth3			62    NULL.	Why we have three depth variables
# 91  Velocity02or06_1		35033 NULL.	Why we have 9 velocityh variables
# 92  Velocity02or06_2		23245 NULL.	Why we have 9 velocityh variables
# 93  Velocity02or06_3		35031 NULL.	Why we have 9 velocityh variables
# 94  Velocity08_1		35033 NULL.	Why we have 9 velocityh variables
# 95  Velocity08_2		23572 NULL.	Why we have 9 velocityh variables
# 96  Velocity08_3		35033 NULL.	Why we have 9 velocityh variables
# 97  VelocityBot1		35032 NULL.	Why we have 9 velocityh variables
# 98  VelocityBot2		23269 NULL.	Why we have 9 velocityh variables
# 99  VelocityBot3		35032 NULL.	Why we have 9 velocityh variables
# 100 WaterVel			3755  NULL.	Visual estimation of water velocity.  Single digit code 9from 0 to 5 ([0]: no reliable visual est, [1]: Eddy, [2]: 0.0-0.3 m/s, [3]: 0.3-0.6 m/s, [4]: 0.6-0.9m/s, [5]: => 0.9 m/s).
# 101 HabitatRN			21343 NULL.	Habitat R: random and N: non-random.
# 102 Cobble			31690 NULL.	Substrate: [0]: absent, [1]: incidental, [2]: dominant, [3]: ubiquitous
# 103 silt			33195 NULL.	Substrate: sum of [silt], [sand] and [gravel} equal to 100%.
# 104 Sand			31645 NULL.	Substrate: sum of [silt], [sand] and [gravel} equal to 100%.
# 105 Gravel			32842 NULL.	Substrate: sum of [silt], [sand] and [gravel} equal to 100%.
# 106 Organic			32489 NULL.	Substrate: [0]: absent, [1]: incidental, [2]: dominant, [3]: ubiquitous
# 107 QC			35020 NULL.	X or NULL
# 108 Comments			29412 NULL.	should ignore.  
# 109 MappingBox		35033 NULL.	should ignore.  Always "NULL".
# 110 Total_Fish_Count				0 to 1843
# 111 Pal_cnt				0 to 11
# 112 CPUE			56   NULL, 8 #DIV/0!. 	see formula
# 113 CPU_Area			8985 NULL, 1 #DIV/0!.	see formula
# 114 Alt_Pal_cnt			should ignore.  alternative way: use 0.01 to represent count of 0
# 115 Alt_CPUE			56   NULL, 8 #DIV/0!. 	should ignore.  alternative way: use 0.01 to represent count of 0
# 116 Alt_CPU_Area		8985 NULL, 1 #DIV/0!.	should ignore.  alternative way: use 0.01 to represent count of 0	


# An example of "recast" which is a combined steps of "melt" and "cast"

# original data frame is "french_fries"
# str(french_fries)
# ----------------------------------------------------------------------------
# 'data.frame':   696 obs. of  9 variables:
#  $ time     : Factor w/ 10 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ treatment: Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
#  $ subject  : Factor w/ 12 levels "3","10","15",..: 1 1 2 2 3 3 4 4 5 5 ...
#  $ rep      : num  1 2 1 2 1 2 1 2 1 2 ...
#  $ potato   : num  2.9 14 11 9.9 1.2 8.8 9 8.2 7 13 ...
#  $ buttery  : num  0 0 6.4 5.9 0.1 3 2.6 4.4 3.2 0 ...
#  $ grassy   : num  0 0 0 2.9 0 3.6 0.4 0.3 0 3.1 ...
#  $ rancid   : num  0 1.1 0 2.2 1.1 1.5 0.1 1.4 4.9 4.3 ...
#  $ painty   : num  5.5 0 0 0 5.1 2.3 0.2 4 3.2 10.3 ...
# ----------------------------------------------------------------------------
# where "time", "treatment", "subject", "rep" are id vars
#       "potato","buttery", "grassy", "rancid", and "painty" are measure vars
# 
# First: "melt", need to specify id.vars and measure.vars.  By default, all factor will be treated as id.vars and all non-id vars will be treated as measure vars.
# Usage:  melt.data.frame(data, id.vars, measure.vars, variable_name = "variable", na.rm = !preserve.na, preserve.na = TRUE, ...)
#
# A <- melt(french_fries,id.var = c("time", "treatment", "subject", "rep"),variable_name="measurement")
# str(A)
# ----------------------------------------------------------------------------
# 'data.frame':   3480 obs. of  6 variables:
#  $ time        : Factor w/ 10 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ treatment   : Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
#  $ subject     : Factor w/ 12 levels "3","10","15",..: 1 1 2 2 3 3 4 4 5 5 ...
#  $ rep         : num  1 2 1 2 1 2 1 2 1 2 ...
#  $ measurement : Factor w/ 5 levels "potato","buttery",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ value       : num  2.9 14 11 9.9 1.2 8.8 9 8.2 7 13 ...
# ----------------------------------------------------------------------------
#
# Then: "cast":  Cast a molten data frame into the reshaped or aggregated form you want
# Usage: cast(data, formula = ... ~ variable, fun.aggregate=NULL, ..., margins=FALSE, subset=TRUE, df=FALSE, fill=NULL, add.missing=FALSE, value = guess_value(data))
#  The cast formula has the following format: x_variable + x_2 ~ y_variable + y_2 ~ z_variable ~ ... 
#  The order of the variables makes a difference. The first varies slowest, and the last fastest. 
#  There are a couple of special variables: "..." represents all other variables not used in the formula and "." represents no variable, so you can do formula = var1 ~ ..
#
#
#
# B <- cast(A,time~measurement,fun=length)
# str(B)
# ----------------------------------------------------------------------------
# List of 6
#  $ time   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10
#  $ potato : int [1:10] 72 72 72 72 72 72 72 72 60 60
#  $ buttery: int [1:10] 72 72 72 72 72 72 72 72 60 60
#  $ grassy : int [1:10] 72 72 72 72 72 72 72 72 60 60
#  $ rancid : int [1:10] 72 72 72 72 72 72 72 72 60 60
#  $ painty : int [1:10] 72 72 72 72 72 72 72 72 60 60
#  - attr(*, "row.names")= int [1:10] 1 2 3 4 5 6 7 8 9 10
#  - attr(*, "idvars")= chr "time"
#  - attr(*, "rdimnames")=List of 2
#   ..$ :'data.frame':    10 obs. of  1 variable:
#   .. ..$ time: Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10
#   ..$ :'data.frame':    5 obs. of  1 variable:
#   .. ..$ measurement: Factor w/ 5 levels "potato","buttery",..: 1 2 3 4 5
# ----------------------------------------------------------------------------
#  B
#    time potato buttery grassy rancid painty
# 1     1     72      72     72     72     72
# 2     2     72      72     72     72     72
# 3     3     72      72     72     72     72
# 4     4     72      72     72     72     72
# 5     5     72      72     72     72     72
# 6     6     72      72     72     72     72
# 7     7     72      72     72     72     72
# 8     8     72      72     72     72     72
# 9     9     60      60     60     60     60
# 10   10     60      60     60     60     60
# ----------------------------------------------------------------------------
#
# To use "recast", we achieve the same thing in one step
#  C <- recast(french_fries,time~variable,fun=length,id.var = c("time", "treatment", "subject", "rep"))
#  C
#    time potato buttery grassy rancid painty
# 1     1     72      72     72     72     72
# 2     2     72      72     72     72     72
# 3     3     72      72     72     72     72
# 4     4     72      72     72     72     72
# 5     5     72      72     72     72     72
# 6     6     72      72     72     72     72
# 7     7     72      72     72     72     72
# 8     8     72      72     72     72     72
# 9     9     60      60     60     60     60
# 10   10     60      60     60     60     60
# ----------------------------------------------------------------------------