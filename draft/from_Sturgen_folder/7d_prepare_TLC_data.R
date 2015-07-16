#
# 7d_prepare_TLC_data.R
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
# Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/7d_prepare_TLC_data"
# if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
# if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
# if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}
# 
# # data file
# FL.Data.IN   <- paste(Path.Data.IN, "MoRiver_Variables_2013May14_YLX.csv",sep="/")
# FL.Data.OUT  <- paste(Path.Out,"TLC_data.csv",sep="/")
# 
# FL.LOG       <- paste(Path.log,"7d_prepare_TLC_data.log",sep="/")	
# FL.SUM.cat   <- paste(Path.Out,"7d_prepare_TLC_data_categoricalSum.csv",sep="/")
# FL.SUM.num   <- paste(Path.Out,"7d_prepare_TLC_data_numericSum.csv",sep="/")
# FL.Summary   <- paste(Path.Out,"7d_prepare_TLC_data_summary.csv",sep="/")
# FL.missing   <- paste(Path.Out,"7d_prepare_TLC_data_missing.csv",sep="/")
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
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/7d_prepare_TLC_data"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}


FL.Data.IN  <- paste(Path.Data.IN, "MoRiver_Variables_2013May31_YLX.csv",sep="/")
FL.Data.OUT <- paste(Path.Out,"TLC_data.csv",sep="/")
FL.Data.OBJ <- paste(Path.Out,"TLC_data.Rdata",sep="/")
FL.RESL.OUT <- paste(Path.Out,"7d_prepare_TLC_data.csv",sep="/")
FL.LOG      <- paste(Path.log,"7d_prepare_TLC_data.log",sep="/")	
FL.PDF      <- paste(Path.Out,"7d_prepare_TLC_data.pdf",sep="/")	
FL.SUM.cat  <- paste(Path.Out,"7d_prepare_TLC_data_cat.sum",sep="/")
FL.SUM.num  <- paste(Path.Out,"7d_prepare_TLC_data_num.sum",sep="/")

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
# var.retained <- c("D_dist_up","Chan_wid","T_dist_up","Grade_10_RM","VF_wid","Temp","Depth_Mean","Pal_cnt","Stk_12_cnt","Stk_24_cnt","Stk_36_cnt","Stk_60_cnt","Stk_120_cnt")
# myData.X <- myData.TLC.lower[,grep("[^Pal_cnt]",names(myData.TLC.lower),value=TRUE,perl=TRUE),drop=FALSE]
# myData.Y <- myData.TLC.lower[,grep("Pal_cnt",names(myData.TLC.lower),value=TRUE,perl=TRUE),drop=FALSE]
Lower.TLC <- myData.raw[,"MA"]=="Lower" & myData.raw[,"gear.type1"]=="TLC"
var.X.num <- c("D_dist_up","Chan_wid","T_dist_up","Grade_10_RM","Comp_per","VF_wid","Temp","Depth_Mean","Stk_12_cnt","Stk_24_cnt","Stk_36_cnt","Stk_60_cnt","Stk_120_cnt")
var.X.cat <- c("macro.type")
var.Y     <- c("Pal_cnt")

myData.X.num <- myData.raw[Lower.TLC,var.X.num,drop=FALSE]
myData.X.cat <- myData.raw[Lower.TLC,var.X.cat,drop=FALSE] 
myData.Y     <- myData.raw[Lower.TLC,var.Y,drop=FALSE] 

# make sure the categorical are factors
for (i in names(myData.X.cat))
{
	if (!(is.factor(myData.X.cat[,i])))
	{
		myData.X.cat[,i] <- as.factor(myData.X.cat[,i])
	}
}

# standardized the numerical variables
var.mean <- as.data.frame(matrix(rep(apply(myData.X.num,2,mean,na.rm=TRUE),dim(myData.X.num)[1]),nrow=dim(myData.X.num)[1],byrow=TRUE))
var.sd   <- as.data.frame(matrix(rep(apply(myData.X.num,2,sd,na.rm=TRUE),  dim(myData.X.num)[1]),nrow=dim(myData.X.num)[1],byrow=TRUE))
myData.X.num.std <- (myData.X.num - var.mean)/var.sd


myData.TLC.lower <- cbind(myData.Y,myData.X.num.std,myData.X.cat)


save(myData.TLC.lower,file=FL.Data.OBJ )


# -------------------------------------------------------------------------------------------------
# output the other format used by WinBUGS or BUGS
# -------------------------------------------------------------------------------------------------
cat("DATA(LIST)\n",FL.Data.OUT,append=TRUE)
cat("list(n=",dim(myData.TLC.lower)[1],",\n",file=FL.Data.OUT,append=TRUE)
idx <- 0
for (col.name in names(myData.TLC.lower))
{
	idx <- idx + 1
	if (idx == dim(myData.TLC.lower)[2])
	{
		A <- paste(col.name,paste(" = c(",paste(myData.TLC.lower[,col.name],collapse=","),")",sep=""),")\n",sep="")
	}else{
		A <- paste(col.name,paste(" = c(",paste(myData.TLC.lower[,col.name],collapse=","),")",sep=""),",\n",sep="")
	}
	cat(A,file=FL.Data.OUT,append=TRUE)
}
cat("data presented in WinBUGS/BUGS format has been outputted!\n")
cat("data presented in WinBUGS/BUGS format has been outputted!\n",file=FL.LOG,append=TRUE)







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
#  subset.idx <- myData.raw[,"Reach"] == "Lower" & myData.raw[,"gear.type1"] == "TLC"
#  myData.TLC.lower <- myData.raw[subset.idx,used.4.anal]
#  
#   subset.idx <- myData.raw[,"Reach"] == "Upper" & myData.raw[,"gear.type1"] == "TLC"
#   myData.TLC.upper <- myData.raw[subset.idx,used.4.anal]
#  
#  
# myData.TLC.lower <-  myData.TLC.lower
#  
# myData.TLC.lower <- myData.TLC.lower[complete.cases(myData.TLC.lower),]
#  
# 
# *************************************************************************************************
# 1. Poisson Model
# *************************************************************************************************
cat(paste("Poisson Model\n",sep=""))
cat(paste("Poisson Model\n",sep=""),file=FL.LOG,append=TRUE)
fm_pois <- glm(Pal_cnt ~ ., data = myData.TLC.lower, family = poisson)

# estimated lambda
est.pred   <- predict(fm_pois)		# predict is the linear predictor, eta.  In Poisson the linear predictor is linked with a log function: log(mu) = eta, so the lambda in Poisson distribution is: lambda = mu = exp(eta)
est.mu     <- exp(est.pred)		# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda <- fm_pois$fitted		# lambda is mu which is the expected value: lamnda estimated using the fitted values from modeling, which should be exact the same as "est.mu"
est.zero.p <- dpois(0,est.lambda)	# the lambda defines the Posisson distribution and we can estimate the probability of zeros of each observation
est.pois   <- cbind(Pal_cnt        = myData.TLC.lower[,"Pal_cnt"],
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
fm_qpois <- glm(Pal_cnt ~ ., data = myData.TLC.lower, family = quasipoisson)

# estimated lambda
est.pred    <- predict(fm_qpois)	# predict is the linear predictor, eta.  In Poisson the linear predictor is linked with a log function: log(mu) = eta, so the lambda in Poisson distribution is: lambda = mu = exp(eta)
est.mu      <- exp(est.pred)		# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda  <- fm_qpois$fitted		# lambda is mu which is the expected value: lamnda estimated using the fitted values from modeling, which should be exact the same as "est.mu"
est.zero.p  <- dpois(0,est.lambda)	# the lambda defines the Posisson distribution and we can estimate the probability of zeros of each observation
est.qpois   <- cbind(Pal_cnt        = myData.TLC.lower[,"Pal_cnt"],
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
fm_nbin <- glm.nb(Pal_cnt ~ ., data = myData.TLC.lower)

# estimated lambda
est.pred    <- predict(fm_nbin)					# predict is the linear predictor, eta.  In Poisson the linear predictor is linked with a log function: log(mu) = eta, so the lambda in Poisson distribution is: lambda = mu = exp(eta)
est.mu      <- exp(est.pred)					# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda  <- fm_nbin$fitted					# lambda is mu which is the expected value: lamnda estimated using the fitted values from modeling, which should be exact the same as "est.mu"
est.zero.p  <- dnbinom(0,mu=est.lambda,size=fm_nbin$theta)	# the lambda defines the Posisson distribution and we can estimate the probability of zeros of each observation
est.nbin    <- cbind(Pal_cnt        = myData.TLC.lower[,"Pal_cnt"],
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
fm_zinb0 <- zeroinfl(Pal_cnt ~ ., data = myData.TLC.lower, dist = "negbin")

# estimated lambda
est.mu          <- predict(fm_zinb0)				# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda      <- fm_zinb0$fitted				# the expected count from the model
est.zero.p      <- predict(fm_zinb0, type = "prob")[,1]		# the probability to have zero count
est.zero.infl   <- predict(fm_zinb0, type = "zero")		# the probability to have inflated zeros
est.count       <- predict(fm_zinb0, type = "count")		# the expected count without the zero inflation

est.zinb0   	<- cbind(Pal_cnt           = myData.TLC.lower[,"Pal_cnt"],
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

# myData.TLC.lower <- myData.TLC.lower[,list.2.keep]



# *************************************************************************************************
# 8. Zero-inflated Poisson regression
# *************************************************************************************************
rm(df.coef.count,df.coef.zero,df.sum)
fm_zip0 <- zeroinfl(Pal_cnt ~ ., data = myData.TLC.lower, dist = "poisson")

# estimated lambda
est.mu          <- predict(fm_zip0)				# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda      <- fm_zip0$fitted				# the expected count from the model
est.zero.p      <- predict(fm_zip0, type = "prob")[,1]		# the probability to have zero count
est.zero.infl   <- predict(fm_zip0, type = "zero")		# the probability to have inflated zeros
est.count       <- predict(fm_zip0, type = "count")		# the expected count without the zero inflation

est.zip0   	<- cbind(Pal_cnt           = myData.TLC.lower[,"Pal_cnt"],
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











# put all models in a list
# In summary, the models are not too dierent with respect to their tted mean functions. The
fm <- list("ML-Pois" = fm_pois, "Quasi-Pois" = fm_qpois, "NB" = fm_nbin,"ZINB" = fm_zinb,"ZIP" = fm_zip)
sapply(fm, function(x) coef(x)[1:8])

#                ML-Pois  Quasi-Pois          NB   Hurdle-NB        ZINB
# (Intercept)      1.02887420  1.02887420  0.92925658  1.19769892  1.19371555
# hosp             0.16479739  0.16479739  0.21777223  0.21189820  0.20147683
# healthpoor       0.24830697  0.24830697  0.30501303  0.31595757  0.28513277
# healthexcellent -0.36199320 -0.36199320 -0.34180660 -0.33186113 -0.31933918
# numchron         0.14663928  0.14663928  0.17491552  0.12642059  0.12899916
# gendermale      -0.11231992 -0.11231992 -0.12648813 -0.06831702 -0.08027732
# school           0.02614299  0.02614299  0.02681508  0.02069321  0.02142327
# privinsyes       0.20168688  0.20168688  0.22440187  0.10017164  0.12586475

# the associated estimated standard errors are very similar as well. The only exception are the model-based standard errors for the Poisson model, when treated
# as a fully specied model, which is obviously not appropriate for this data set.
std.error <- data.frame(cbind("ML-Pois" = sqrt(diag(vcov(fm_pois))),"Adj-Pois" = sqrt(diag(sandwich(fm_pois))),	# add pois and sandwich adjusted poisson
                                    sapply(fm[-1], function(x) sqrt(diag(vcov(x)))[1:8])))			# remove the pois SE which is the first one in the list.  Also note that we are extracting the first 8 coefficients


cat(paste("\nStandard Error of Various Models: ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(std.error,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Standard Error of Models! ------------------------\n")



# > std.error
#                     ML-Pois    Adj-Pois  Quasi-Pois          NB  Hurdle-NB        ZINB         ZIP
# (Intercept)     0.023784601 0.064529808 0.061593641 0.054591271 0.05897349 0.056660841 0.024178655
# hosp            0.005997367 0.021945186 0.015531043 0.020176492 0.02139606 0.020359727 0.006060009
# healthpoor      0.017844531 0.054021990 0.046210977 0.048510797 0.04805566 0.045092639 0.017705506
# healthexcellent 0.030303905 0.077448586 0.078476316 0.060923623 0.06609306 0.060404889 0.031264705
# numchron        0.004579677 0.012907865 0.011859732 0.012091749 0.01245231 0.011930513 0.004721027
# gendermale      0.012945146 0.035343487 0.033523316 0.031215523 0.03241561 0.031024027 0.013056404
# school          0.001843329 0.005084002 0.004773565 0.004393971 0.00453483 0.004357569 0.001873485
# privinsyes      0.016859826 0.043128006 0.043660942 0.039463744 0.04261858 0.041587616 0.017147025










                                
# The dierences become obvious if not only the mean but the full likelihood is considered: 
# note: The quasi-Poisson model and the sandwich-adjusted Poisson model are not associated with a fitted likelihood.
tmp <- data.frame(rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),					# useful usage of sapply
      Df     = sapply(fm, function(x) attr(logLik(x), "df")),
      AIC    = sapply(fm, function(x) round(AIC(x), digits = 0)),
      BIC    = sapply(fm, function(x) round(BIC(x), digits = 0))))
      
cat(paste("\nOther Diagnosis of Models: ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(tmp,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Other Diagnosis of Models! ------------------------\n")
      
#        ML-Pois Quasi-Pois     NB Hurdle-NB   ZINB    ZIP
# logLik  -17972         NA -12171    -12090 -12091 -16135
# Df           8          8      9        15     15     14
# AIC      35959         NA  24359     24210  24211  32298
# BIC      36010         NA  24417        NA     NA     NA
# 
# 




# notice that:
logLik(fm_pois)			# to get the model's log likelihood					# useful usage to get the logLik from a model
attr(logLik(fm_pois),"df")	# to get the df of the model




# Additionally, it is of interest how the zero counts are captured by the various models.
# Therefore, the observed zero counts are compared to the expected number of zero counts for
# the likelihood-based models:
# fitted(model) gives the fitted mean function such as lambda in the poisson distribution.  For each data point, one of such lambda will be estimated from the model
# dpois: density function, taking values between the valid range of the function say -infinity o infinity of the normal distribution
# ppois: cumulative distribution function, taking values between the valid range of the function say -infinity o infinity of the normal distribution
# qpois: quantile of a distribution, taking values between 0 and 1
number.zero  <- round(c("Obs" 		= sum(dt$ofp < 1),
			"ML-Pois" 	= sum(dpois(0, fitted(fm_pois))),				# the probability to get 0 with the lambda estimated from the model
			"NB" 		= sum(dnbinom(0, mu = fitted(fm_nbin), size = fm_nbin$theta)), 	# the probability to get 0 with the lambda and theta estimated from the model
			"NB-Hurdle" 	= sum(predict(fm_hurdle, type = "prob")[,1]),			# the probability to get 0 predicted 
			"ZINB" 		= sum(predict(fm_zinb, type = "prob")[,1]),			# the probability to get 0 predicted 
			"ZIP" 		= sum(predict(fm_zip, type = "prob")[,1])))			# the probability to get 0 predicted 
# 			
# number.zero
#      Obs   ML-Pois        NB NB-Hurdle      ZINB       ZIP 
#      683        47       608       683       709       682 INB 
	
# By construction, the expected number of zero counts in the hurdle model matches the observed number.

# predicted count
count.predict <- cbind("Obs" 		= dt$ofp,
			"ML-Pois" 	= predict(fm_hurdle,type = "prob")[,1],		# the probability to get 0 with the lambda estimated from the model
			"NB" 		= predict(fm_hurdle,type = "prob")[,1], 	# the probability to get 0 with the lambda and theta estimated from the model
			"NB-Hurdle" 	= predict(fm_hurdle,type = "prob")[,1],		# the probability to get 0 predicted 
			"ZINB" 		= predict(fm_zinb,  type = "prob")[,1],		# the probability to get 0 predicted 
			"ZIP" 		= predict(fm_zip,   type = "prob")[,1])		# the probability to get 0 predicted 


# summarize the coefficients for the count components
coef.count     <- rbind("ML-Pois" 	= fm_pois$coefficients,				# the probability to get 0 with the lambda estimated from the model
		  	"NB" 		= fm_nbin$coefficients, 			# the probability to get 0 with the lambda and theta estimated from the model
			"NB-Hurdle" 	= fm_hurdle$coefficients$count,			# the probability to get 0 predicted 
			"ZINB" 		= fm_zinb$coefficients$count,			# the probability to get 0 predicted 
			"ZIP" 		= fm_zip$coefficients$count)			# the probability to get 0 predicted 

#           (Intercept)      hosp healthpoor healthexcellent  numchron  gendermale     school privinsyes
# ML-Pois     1.0288742 0.1647974  0.2483070      -0.3619932 0.1466393 -0.11231992 0.02614299  0.2016869
# NB          0.9292566 0.2177722  0.3050130      -0.3418066 0.1749155 -0.12648813 0.02681508  0.2244019
# NB-Hurdle   1.1976989 0.2118982  0.3159576      -0.3318611 0.1264206 -0.06831702 0.02069321  0.1001716
# ZINB        1.1937156 0.2014768  0.2851328      -0.3193392 0.1289992 -0.08027732 0.02142327  0.1258647
# ZIP         1.4056000 0.1590135  0.2534164      -0.3073657 0.1018459 -0.06235219 0.01916943 0.08053267

# summarize the coefficients for the count components
coef.zero     <- rbind(	"NB-Hurdle" 	= fm_hurdle$coefficients$zero,			# the probability to get 0 predicted 
			"ZINB" 		= fm_zinb$coefficients$zero,			# the probability to get 0 predicted 
			"ZIP" 		= fm_zip$coefficients$zero)			# the probability to get 0 predicted 


#           (Intercept)       hosp   numchron privinsyes      school gendermale
# NB-Hurdle  0.01594017  0.3184345  0.5478325   0.745720  0.05707282 -0.4191478
# ZINB      -0.04683873 -0.8004650 -1.2478971  -1.175584 -0.08377747  0.6476602
# ZIP       -0.05936931 -0.3066871 -0.5397165 -0.7537324 -0.05559510  0.4180648
#  
# 

#
# the sum of the zero probability
#
p.zero <- data.frame(data.0  = sum(dt[,"ofp"]==0),
                     Poisson = estimation.Poisson[1,"est.zero.p"],
                     NBIN    = estimation.nbin[1,"est.zero.p"],
                     ZINB    = estimation.zinb[1,"est.zero.p"],
                     ZIP     = estimation.zip[1,"est.zero.p"])


cat(paste("\nZeros Estimated in prob: ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(p.zero,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)


#
# save the data in R data format
#
save(dt2,dt,estimation.nbin,estimation.Poisson,estimation.QuasiPoisson,estimation.zinb,estimation.zinb0,estimation.zip,estimation.zip0,file=FL.Data.OBJ )



# How to get the esitmated values???????
dev.off()


# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n0_classical_GLM_revised.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n0_classical_GLM_revised.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [0_classical_GLM_revised.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [0_classical_GLM_revised.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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



























 
#
# reduce the number of categories
#
#
# convert the integer to factor
#
# if(!(is.factor( myData.TLC.lower[,"SY"]))){myData.TLC.lower[,"SY"] = as.factor(myData.TLC.lower[,"SY"])}
# if(!(is.factor( myData.TLC.lower[,"Segment"]))){myData.TLC.lower[,"Segment"] = as.factor(myData.TLC.lower[,"Segment"])}
# if(!(is.factor( myData.TLC.lower[,"MicroClass"]))){myData.TLC.lower[,"MicroClass"] = as.factor(myData.TLC.lower[,"MicroClass"])}
 


