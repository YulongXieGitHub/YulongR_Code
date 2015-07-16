# ---------------------------------------------------------------------------------------------------
# extract_HVAC_revised.R
# 
# remember to verify the results
#
# March 4, 2013: Two changes in Brian's updated list
#                (1) OfficeLarge (for addendum ak): although three controls were specified in page 5.49 of the TSD, only the first one (constant) and third one (VFD) are used.
#                    therefore we only need to retrieve the "PumpCurve_flag" from the parm csv file, if "yes" it is "VFD" and if it is no, it is constant ride curve. 
#                (2) Hotel Small: there is no optimum start
#
# March 1, 2013: To implement the updated HVAC extract request. Brian updated the HVAC info to be extracted on Feb 25, 2013 with significant changes to the original list.  (See Brian Feb 25, 2013 9:00 AM)
# Feb 14, 2013: Original OA extraction was not right for the prototype subjected to mutz calculation.  Revise it.
# Feb 1,  2013: finalize the 43 state city list of 22 states
#               columbia district need to be handled explicitly.
# Jan 29, 2013: modify to allow the calculation the regular analysis such as "ASH90.1-2007 vs ASHRAE90.1-2010" and "IECC_2009" vs "IECC_2012"
# Jan 17, 2013:
# checked the possiblity to have cross standards
# finalizing using Gopal's state weights
# 
# Jan 9, 2013: 
# This script is for extracting the mechanical sizing data for the FY2013 state cost effectiveness analysis.
# It originates from Heejin's unfinished R script "summary_cost_eff.R".
# change name from "summary_cost_eff.R" to "extract_HVAC_revised.R"
#
# The following perl scripts are recently coded to provide summary information needed for this R script.
# 1. proc_idf_get_flag.pl			get (a) economizer type of systems and (b) single zone VAV decision from the annual idf files after sizing script processing.
# 2. proc_eio_get_pump_flow_sizing.pl		
# 3. proc_eio_get_datacenter_sizing.pl
#
# Like the other processing script under \_p.bin\, these perl scripts extract information from a variety of EPlus output files or idf files to prepare some summary tables.
#
# Significant changes (re-write) was done:
# (i)   it was supposed to deal with state analysis cases but started on the regular PI cases.  
# 		but it did not use the case name convention for state analysis in subsetting and aggregating.
# (ii)  it was supposed to provide a single HVAC information file for each state consisting of 
#       	all 6 prototypes (one stacks on the top of the other), 
#       	all climate zones of the state (side by side)
#       	and two standards (two immediate adjacent columns for each climate zone for either two IECCs or two ASHRAEs)
#       but it was structured in a way to work on individual prototype and provide one file for each prototype-standard combination 
#               which did not consider there are multiple (25) states in the state cases and each state has various number of climate zones and 
#               possibly diffeent standards (either ASHREA90.1-2007/2010 or IECC 2009/IECCplus-2012) and
#               hard coded ASHRAE90.1-2007 and ASHRAE90.1-2010 as the two standards involved.
# Actions taken:
# (i)   restructure the script to be used as a standalone script instead of a script in the make process which will lead to one or more files for each prototype.
# (ii)  add three function with is used to add auxiliary fields for each table read in in order to subset for each state
# (iii) add state loop 
#
# Syntax to run this script:
#
# No arguments is needed because the paths have been specified in the script.
#
# Note:
# ***********************************************************************************************************
# this script is set to run two different batch
# When set "this.analysis=="regular"", it assumes a regular 17 CZ cases for ASHRAE90.1-2007 and ASHRAE90.1-2010. Thisis used to verify the results since Heejin's original code was based on that assumption.
# When set "this.analysis=="state"",   it runs state analysis assuming varying number of CZ in each of the states.  The number of the states is not pre-set in the code BUT determined based on the state cases.
# ***********************************************************************************************************
# Yulong Xie
# ---------------------------------------------------------------------------------------------------
# Author: Heejin Cho
# Date: 12/10/2012 Original
#
# Summary:
# To perform the state-level cost analysis, an automated process to extract mechanical sizing data 
# from simulation results is necessary.
# The type of data varies by prototype because of variations in HVAC system type and quantity. 
# Therefore, the data extraction process for each prototype needs to be highly customized.   
#
# Implementaion Steps:
#   - Identify locations of mechanical sizing information.
#   - Write a script to extract the mechanical sizing information for each prototype.
#   - Create a summary table (.csv) for each prototype using the script.
#   - Import the data from the summary table into the mechanical cost analysis spreadsheets. 
#
# ---------------------------------------------------------------------------------------------------

# eliminate all stuff
rm(list = ls(all = TRUE))


# ***********************************************************************************************************
#
# FUNCTIONS
#
# ***********************************************************************************************************
# -------------------------------------------------------------------------------------------------
# function: mapOAscenario
#
# this is a function to map a standard to an OA scenario.  Note: we have mapped different standard to different OA scenario for mutz calc
# input:  a DXCoil data frame or its subset which consists of the standat name and year
# output: an OA scenario name corresponds to the standard name-year
# -------------------------------------------------------------------------------------------------
mapOAscenario <- function(dataFrame.IN,standard.name)
{
	# the standard year
	standard.year <- sub("STD","",dataFrame.IN[1,"standard"])

	# construct the OA scenario corresponding to the current standard
	if (length(grep("IECC",standard.name)))		# if the standard has "IECC" in it
	{
		if (length(grep("plus",standard.name)))	# if the standard has "plus" in it
		{
			OA.scenario <- paste("state_IECCplus_STD",standard.year,"_IMC",standard.year,sep="")
		}else{
			OA.scenario <- paste("state_IECC_STD",standard.year,"_IMC",standard.year,sep="")
		}
	}else{
		OA.scenario <- paste("state_ASHRAE901_STD",standard.year,"_OA2004",sep="")
	}

	return(OA.scenario)
}		

# -------------------------------------------------------------------------------------------------
# function: expandSummary
#
# this is a function to use and/or split some of the fields of a data frame reading from a summary file of EPlus and append it with some auxiliary feilds
# input:  a data frame reading from a summary file of EPlus which always has some common fields such as "case", "project", "prototype", "standard", "location"
# output: the same data frame with additional fields
# -------------------------------------------------------------------------------------------------
expandSummary <- function(dataFrame.IN)
{
	CZ.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,            10,      11,       12,      13,         14,         15,        16,     17,         18,      19,          20,          21,      22,      23,      24,        25,          26,          27,          28,       29,         30,        31,        32,                 33,           34,             35,           36,          37,             38,         39,       40,      41,       42,        43,        44,          45,        46,               47,         48,           49,        50,      51,           52,      53,          54,               55,            56,         57,          58,       59,           60,      61,              62)
	CZ.state <- c("FL",    "XX",    "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK",       "AK",    "CA",        "CA",        "TX",    "TX",    "AL",    "GA",      "TX",        "AL",        "AR",        "GA",     "NC",       "OK",      "SC",      "TX",               "UT",         "AR",           "DC",         "DE",        "GA",           "KY",       "NC",     "NJ",    "NY",     "VA",      "CO",      "OK",        "TX",      "CT",             "IA",       "MA",         "NC",      "NE",    "NJ",         "NY",    "RI",        "CO",             "UT",          "IA",       "NY",        "WI",     "CO",         "UT",    "CO",            "WI")
	CZ.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks","Juneau","LosAngeles","Sacramento","Dallas","Austin","Mobile","Savannah","SanAntonio","Birmingham","LittleRock","Atlanta","Charlotte","Oklahoma","Columbia","FortWorthAlliance","SaintGeorge","SpringfieldMO","BaltimoreMD","Wilmington","ChattanoogaTN","Lexington","Raleigh","Newark","NewYork","Richmond","Trinidad","AmarilloTX","Amarillo","HartfordBradley","DesMoines","BostonLogan","ElkinsWV","Omaha", "AllentownPA","Albany","Providence","ColoradoSprings","SaltLakeCity","MasonCity","Binghamton","Madison","EagleCounty","Vernal","GunnisonCounty","DuluthMN")
	CZ.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B","Zone3C",      "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8",    "Zone8", "Zone3C",    "Zone3C",    "Zone2A","Zone2A","Zone2A","Zone2A",  "Zone2B",    "Zone3A",    "Zone3A",    "Zone3A", "Zone3A",   "Zone3A",  "Zone3A",  "Zone3A",           "Zone3B",     "Zone4A",       "Zone4A",     "Zone4A",    "Zone4A",       "Zone4A",   "Zone4A", "Zone4A","Zone4A", "Zone4A",  "Zone4B",  "Zone4B",    "Zone4B",  "Zone5A",         "Zone5A",   "Zone5A",     "Zone5A",  "Zone5A","Zone5A",     "Zone5A","Zone5A",    "Zone5B",         "Zone5B",      "Zone6A",   "Zone6A",    "Zone6A", "Zone6B",     "Zone6B","Zone7",         "Zone7")
	CZ.array <- data.frame(city = CZ.city,zone = CZ.label,state.abb=CZ.state)
	CZ.array[,"identifier"] <- paste(CZ.array[,"zone"],CZ.array[,"state.abb"],sep="_")

	# -----------------------------------------
	CZ.all.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,           10,       11,       12,      13,         14,          15,      16,      17)
	CZ.all.state <- c("FL",     "XX",   "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK")
	CZ.all.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks")
	CZ.all.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B", "Zone3C",     "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8")
	CZ.all.array <- data.frame(city = CZ.all.city,zone = CZ.all.label,state.abb=CZ.all.state)
	CZ.all.array[,"identifier"] <- paste(CZ.all.array[,"zone"],CZ.all.array[,"state.abb"],sep="_")


	myState <- data.frame(state.abb = CZ.state,climate.city = CZ.city,climate.zone=CZ.label)


	# add auxilary fields for identification
	# A. determine if it is for state analysis based on case name
	auxi.type <- rep("regular",dim(dataFrame.IN)[1])
	auxi.type[grep("\\.state",dataFrame.IN[,"project"])] <- "state"

	# B. remove "_" in climate city
	dataFrame.IN[,"location"] <- sub("San_Francisco","SanFrancisco",dataFrame.IN[,"location"])
	dataFrame.IN[,"location"] <- sub("El_Paso",      "ElPaso",      dataFrame.IN[,"location"])
	dataFrame.IN[,"location"] <- sub("Los_Angeles",  "LosAngeles",  dataFrame.IN[,"location"])

	# C. get climate zone label for climate city
	auxi.cz   <- CZ.array[match(dataFrame.IN[,"location"],CZ.array[,"city"]),"zone"]

	# D. find index in [myState] which matching [parm.city] and [myState[,"climate.city"]]
	idx.in.myState  <- match(dataFrame.IN[,"location"],myState[,"climate.city"])
	auxi.state.abb <- myState[idx.in.myState,"state.abb"]

	# E. add a combined string with name and year of the code
	auxi.STD <- paste(dataFrame.IN[,"project"],dataFrame.IN[,"standard"],sep="_")

	dataFrame.OUT <- cbind(dataFrame.IN,
			       A.type       = auxi.type,						# add "regular" and "state" identification
			       A.CZ.zone    = auxi.cz,							# add climate zone identification
			       A.CZ.city    = dataFrame.IN[,"location"],				# use "CZ.city" to replace "location"
			       A.CZ.climate = paste(auxi.cz,"(",dataFrame.IN[,"location"],")",sep=""),
			       A.stateAbb   = auxi.state.abb,						# add state abbreviation
			       A.standard   = auxi.STD)


	return(dataFrame.OUT)
}

# -------------------------------------------------------------------------------------------------
# function: expandOA
#
# this is a function to use and/or split some of the fields of a data frame reading from the OA csv file from mutz calc and append it with some auxiliary feilds
# input:  a data frame reading from the OA csv file from mutz calc 
# output: the same data frame with additional fields
# -------------------------------------------------------------------------------------------------
expandOA <- function(dataFrame.IN)
{
	CZ.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,            10,      11,       12,      13,         14,         15,        16,     17,         18,      19,          20,          21,      22,      23,      24,        25,          26,          27,          28,       29,         30,        31,        32,                 33,           34,             35,           36,          37,             38,         39,       40,      41,       42,        43,        44,          45,        46,               47,         48,           49,        50,      51,           52,      53,          54,               55,            56,         57,          58,       59,           60,      61,              62)
	CZ.state <- c("FL",    "XX",    "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK",       "AK",    "CA",        "CA",        "TX",    "TX",    "AL",    "GA",      "TX",        "AL",        "AR",        "GA",     "NC",       "OK",      "SC",      "TX",               "UT",         "AR",           "DC",         "DE",        "GA",           "KY",       "NC",     "NJ",    "NY",     "VA",      "CO",      "OK",        "TX",      "CT",             "IA",       "MA",         "NC",      "NE",    "NJ",         "NY",    "RI",        "CO",             "UT",          "IA",       "NY",        "WI",     "CO",         "UT",    "CO",            "WI")
	CZ.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks","Juneau","LosAngeles","Sacramento","Dallas","Austin","Mobile","Savannah","SanAntonio","Birmingham","LittleRock","Atlanta","Charlotte","Oklahoma","Columbia","FortWorthAlliance","SaintGeorge","SpringfieldMO","BaltimoreMD","Wilmington","ChattanoogaTN","Lexington","Raleigh","Newark","NewYork","Richmond","Trinidad","AmarilloTX","Amarillo","HartfordBradley","DesMoines","BostonLogan","ElkinsWV","Omaha", "AllentownPA","Albany","Providence","ColoradoSprings","SaltLakeCity","MasonCity","Binghamton","Madison","EagleCounty","Vernal","GunnisonCounty","DuluthMN")
	CZ.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B","Zone3C",      "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8",    "Zone8", "Zone3C",    "Zone3C",    "Zone2A","Zone2A","Zone2A","Zone2A",  "Zone2B",    "Zone3A",    "Zone3A",    "Zone3A", "Zone3A",   "Zone3A",  "Zone3A",  "Zone3A",           "Zone3B",     "Zone4A",       "Zone4A",     "Zone4A",    "Zone4A",       "Zone4A",   "Zone4A", "Zone4A","Zone4A", "Zone4A",  "Zone4B",  "Zone4B",    "Zone4B",  "Zone5A",         "Zone5A",   "Zone5A",     "Zone5A",  "Zone5A","Zone5A",     "Zone5A","Zone5A",    "Zone5B",         "Zone5B",      "Zone6A",   "Zone6A",    "Zone6A", "Zone6B",     "Zone6B","Zone7",         "Zone7")
	CZ.array <- data.frame(city = CZ.city,zone = CZ.label,state.abb=CZ.state)
	CZ.array[,"identifier"] <- paste(CZ.array[,"zone"],CZ.array[,"state.abb"],sep="_")


	# -----------------------------------------
	CZ.all.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,           10,       11,       12,      13,         14,          15,      16,      17)
	CZ.all.state <- c("FL",     "XX",   "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK")
	CZ.all.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks")
	CZ.all.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B", "Zone3C",     "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8")
	CZ.all.array <- data.frame(city = CZ.all.city,zone = CZ.all.label,state.abb=CZ.all.state)
	CZ.all.array[,"identifier"] <- paste(CZ.all.array[,"zone"],CZ.all.array[,"state.abb"],sep="_")


	myState <- data.frame(state.abb = CZ.state,climate.city = CZ.city,climate.zone=CZ.label)



	# add auxilary fields for identification
	# A. determine if it is for state analysis based on case name
	auxi.type <- rep("regular",dim(dataFrame.IN)[1])
	auxi.type[grep("^state_",dataFrame.IN[,"OA_scenario"])] <- "state"

	# B. remove "_" in climate city
	dataFrame.IN[,"climatezone"] <- sub("San_Francisco","SanFrancisco",dataFrame.IN[,"climatezone"])
	dataFrame.IN[,"climatezone"] <- sub("El_Paso",      "ElPaso",      dataFrame.IN[,"climatezone"])
	dataFrame.IN[,"climatezone"] <- sub("Los_Angeles",  "LosAngeles",  dataFrame.IN[,"climatezone"])

	# C. get climate zone label for climate city
	auxi.cz   <- CZ.array[match(dataFrame.IN[,"climatezone"],CZ.array[,"city"]),"zone"]

	# D. find index in [myState] which matching [parm.city] and [myState[,"climate.city"]]
	idx.in.myState  <- match(dataFrame.IN[,"climatezone"],myState[,"climate.city"])
	auxi.state.abb <- myState[idx.in.myState,"state.abb"]

	dataFrame.OUT <- cbind(dataFrame.IN,
			       A.type       = auxi.type,							# add "regular" and "state" identification
			       A.CZ.zone    = auxi.cz,								# add climate zone identification
			       A.CZ.city    = dataFrame.IN[,"climatezone"],					# use "CZ.city" to replace "location"
			       A.CZ.climate = paste(auxi.cz,"(",dataFrame.IN[,"climatezone"],")",sep=""),
			       A.stateAbb   = auxi.state.abb							# add state abbreviation
			       )


	return(dataFrame.OUT)
}

# -------------------------------------------------------------------------------------------------
# function: expandParm
#
# this is a function to split the "Case" field of the parm csv file and append it with some auxiliary feilds
# input:  a data frame of the parm csv file 
# output: the same data frame with additional fields
# -------------------------------------------------------------------------------------------------
expandParm <- function(dataFrame.IN)
{
	CZ.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,            10,      11,       12,      13,         14,         15,        16,     17,         18,      19,          20,          21,      22,      23,      24,        25,          26,          27,          28,       29,         30,        31,        32,                 33,           34,             35,           36,          37,             38,         39,       40,      41,       42,        43,        44,          45,        46,               47,         48,           49,        50,      51,           52,      53,          54,               55,            56,         57,          58,       59,           60,      61,              62)
	CZ.state <- c("FL",    "XX",    "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK",       "AK",    "CA",        "CA",        "TX",    "TX",    "AL",    "GA",      "TX",        "AL",        "AR",        "GA",     "NC",       "OK",      "SC",      "TX",               "UT",         "AR",           "DC",         "DE",        "GA",           "KY",       "NC",     "NJ",    "NY",     "VA",      "CO",      "OK",        "TX",      "CT",             "IA",       "MA",         "NC",      "NE",    "NJ",         "NY",    "RI",        "CO",             "UT",          "IA",       "NY",        "WI",     "CO",         "UT",    "CO",            "WI")
	CZ.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks","Juneau","LosAngeles","Sacramento","Dallas","Austin","Mobile","Savannah","SanAntonio","Birmingham","LittleRock","Atlanta","Charlotte","Oklahoma","Columbia","FortWorthAlliance","SaintGeorge","SpringfieldMO","BaltimoreMD","Wilmington","ChattanoogaTN","Lexington","Raleigh","Newark","NewYork","Richmond","Trinidad","AmarilloTX","Amarillo","HartfordBradley","DesMoines","BostonLogan","ElkinsWV","Omaha", "AllentownPA","Albany","Providence","ColoradoSprings","SaltLakeCity","MasonCity","Binghamton","Madison","EagleCounty","Vernal","GunnisonCounty","DuluthMN")
	CZ.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B","Zone3C",      "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8",    "Zone8", "Zone3C",    "Zone3C",    "Zone2A","Zone2A","Zone2A","Zone2A",  "Zone2B",    "Zone3A",    "Zone3A",    "Zone3A", "Zone3A",   "Zone3A",  "Zone3A",  "Zone3A",           "Zone3B",     "Zone4A",       "Zone4A",     "Zone4A",    "Zone4A",       "Zone4A",   "Zone4A", "Zone4A","Zone4A", "Zone4A",  "Zone4B",  "Zone4B",    "Zone4B",  "Zone5A",         "Zone5A",   "Zone5A",     "Zone5A",  "Zone5A","Zone5A",     "Zone5A","Zone5A",    "Zone5B",         "Zone5B",      "Zone6A",   "Zone6A",    "Zone6A", "Zone6B",     "Zone6B","Zone7",         "Zone7")
	CZ.array <- data.frame(city = CZ.city,zone = CZ.label,state.abb=CZ.state)
	CZ.array[,"identifier"] <- paste(CZ.array[,"zone"],CZ.array[,"state.abb"],sep="_")


	# -----------------------------------------
	CZ.all.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,           10,       11,       12,      13,         14,          15,      16,      17)
	CZ.all.state <- c("FL",     "XX",   "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK")
	CZ.all.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks")
	CZ.all.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B", "Zone3C",     "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8")
	CZ.all.array <- data.frame(city = CZ.all.city,zone = CZ.all.label,state.abb=CZ.all.state)
	CZ.all.array[,"identifier"] <- paste(CZ.all.array[,"zone"],CZ.all.array[,"state.abb"],sep="_")



	myState <- data.frame(state.abb = CZ.state,climate.city = CZ.city,climate.zone=CZ.label)


	# A. determine if it is for state analysis based on case name
	auxi.type <- rep("regular",dim(dataFrame.IN)[1])
	auxi.type[grep("\\.state",dataFrame.IN[,"Case"])] <- "state"

	# B. remove "_" in climate city
	dataFrame.IN[,"Case"] <- sub("San_Francisco","SanFrancisco",dataFrame.IN[,"Case"])
	dataFrame.IN[,"Case"] <- sub("El_Paso",      "ElPaso",      dataFrame.IN[,"Case"])
	dataFrame.IN[,"Case"] <- sub("Los_Angeles",  "LosAngeles",  dataFrame.IN[,"Case"])

	# C. split the case name based on a delimeter "_"
	tmp <- unlist(strsplit(dataFrame.IN[,"Case"],"_"))
	auxi.case <- tmp[seq(from=1,to=length(tmp),by=4)]		# [case]
	auxi.bldg <- tmp[seq(from=2,to=length(tmp),by=4)]		# [prototype]
	auxi.std  <- tmp[seq(from=3,to=length(tmp),by=4)]		# [standard]
	auxi.city <- tmp[seq(from=4,to=length(tmp),by=4)]		# [climate.city]
	auxi.cz   <- CZ.array[match(auxi.city,CZ.array[,"city"]),"zone"]



	# get te auxi.standard
	auxi.standard <- paste(auxi.case,auxi.std,sep="_")
	# auxi.standard <- sub("ASHRAE30pct","ASHRAE90.1",auxi.standard)
	# auxi.standard <- sub("PI.Oct2012", "ASHRAE90.1",auxi.standard)

	# determine if it is for state analysis based on case name
	auxi.type <- rep("regular",dim(dataFrame.IN)[1])
	auxi.type[grep("\\.state",auxi.case)] <- "state"

	# find index in [myState] which matching [auxi.city] and [myState[,"climate.city"]]
	idx.in.myState  <- match(auxi.city,myState[,"climate.city"])
	auxi.state.abb <- myState[idx.in.myState,"state.abb"]


	dataFrame.OUT <- cbind(dataFrame.IN,
			      A.type       = auxi.type,						# add "regular" and "state" identification
			      A.CZ.zone    = auxi.cz,						# add climate zone identification
			      A.CZ.city    = auxi.city,						# use "CZ.city" to replace "location"
			      A.CZ.climate = paste(auxi.cz,"(",auxi.city,")",sep=""),
			      A.stateAbb   = auxi.state.abb,					# add state abbreviation
			      A.standard   = auxi.standard
			      )
	return(dataFrame.OUT)
}

# -------------------------------------------------------------------------------------------------
# function: expandEconSZvav
#
# this is a function to use and/or split some of the fields of a data frame reading from "summary_flag_output.csv" file
# input:  a data frame reading from a summary file of EPlus which always has some common fields such as "case", "project", "prototype", "standard", "location"
# output: return a list which consists of two data frames [myData.econ] and [myData.szvav], one for economizer data and the other for sz vav data
# March 1, 2013: "proc_idf_get_flag.pl" has been revised to include an extraction of damper type, so there is one more output from this subroutine, i.e., damper
# -------------------------------------------------------------------------------------------------
expandEconSZvav <- function(dataFrame.IN)
{
	CZ.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,            10,      11,       12,      13,         14,         15,        16,     17,         18,      19,          20,          21,      22,      23,      24,        25,          26,          27,          28,       29,         30,        31,        32,                 33,           34,             35,           36,          37,             38,         39,       40,      41,       42,        43,        44,          45,        46,               47,         48,           49,        50,      51,           52,      53,          54,               55,            56,         57,          58,       59,           60,      61,              62)
	CZ.state <- c("FL",    "XX",    "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK",       "AK",    "CA",        "CA",        "TX",    "TX",    "AL",    "GA",      "TX",        "AL",        "AR",        "GA",     "NC",       "OK",      "SC",      "TX",               "UT",         "AR",           "DC",         "DE",        "GA",           "KY",       "NC",     "NJ",    "NY",     "VA",      "CO",      "OK",        "TX",      "CT",             "IA",       "MA",         "NC",      "NE",    "NJ",         "NY",    "RI",        "CO",             "UT",          "IA",       "NY",        "WI",     "CO",         "UT",    "CO",            "WI")
	CZ.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks","Juneau","LosAngeles","Sacramento","Dallas","Austin","Mobile","Savannah","SanAntonio","Birmingham","LittleRock","Atlanta","Charlotte","Oklahoma","Columbia","FortWorthAlliance","SaintGeorge","SpringfieldMO","BaltimoreMD","Wilmington","ChattanoogaTN","Lexington","Raleigh","Newark","NewYork","Richmond","Trinidad","AmarilloTX","Amarillo","HartfordBradley","DesMoines","BostonLogan","ElkinsWV","Omaha", "AllentownPA","Albany","Providence","ColoradoSprings","SaltLakeCity","MasonCity","Binghamton","Madison","EagleCounty","Vernal","GunnisonCounty","DuluthMN")
	CZ.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B","Zone3C",      "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8",    "Zone8", "Zone3C",    "Zone3C",    "Zone2A","Zone2A","Zone2A","Zone2A",  "Zone2B",    "Zone3A",    "Zone3A",    "Zone3A", "Zone3A",   "Zone3A",  "Zone3A",  "Zone3A",           "Zone3B",     "Zone4A",       "Zone4A",     "Zone4A",    "Zone4A",       "Zone4A",   "Zone4A", "Zone4A","Zone4A", "Zone4A",  "Zone4B",  "Zone4B",    "Zone4B",  "Zone5A",         "Zone5A",   "Zone5A",     "Zone5A",  "Zone5A","Zone5A",     "Zone5A","Zone5A",    "Zone5B",         "Zone5B",      "Zone6A",   "Zone6A",    "Zone6A", "Zone6B",     "Zone6B","Zone7",         "Zone7")
	CZ.array <- data.frame(city = CZ.city,zone = CZ.label,state.abb=CZ.state)
	CZ.array[,"identifier"] <- paste(CZ.array[,"zone"],CZ.array[,"state.abb"],sep="_")

	# -----------------------------------------
	CZ.all.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,           10,       11,       12,      13,         14,          15,      16,      17)
	CZ.all.state <- c("FL",     "XX",   "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK")
	CZ.all.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks")
	CZ.all.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B", "Zone3C",     "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8")
	CZ.all.array <- data.frame(city = CZ.all.city,zone = CZ.all.label,state.abb=CZ.all.state)
	CZ.all.array[,"identifier"] <- paste(CZ.all.array[,"zone"],CZ.all.array[,"state.abb"],sep="_")


	myState <- data.frame(state.abb = CZ.state,climate.city = CZ.city,climate.zone=CZ.label)


	# add auxilary fields for identification
	# A. determine if it is for state analysis based on case name
	auxi.type <- rep("regular",dim(dataFrame.IN)[1])
	auxi.type[grep("\\.state",dataFrame.IN[,"project"])] <- "state"

	# B. remove "_" in climate city
	dataFrame.IN[,"location"] <- sub("San_Francisco","SanFrancisco",dataFrame.IN[,"location"])
	dataFrame.IN[,"location"] <- sub("El_Paso",      "ElPaso",      dataFrame.IN[,"location"])
	dataFrame.IN[,"location"] <- sub("Los_Angeles",  "LosAngeles",  dataFrame.IN[,"location"])

	# C. get climate zone label for climate city
	auxi.cz   <- CZ.array[match(dataFrame.IN[,"location"],CZ.array[,"city"]),"zone"]

	# D. find index in [myState] which matching [parm.city] and [myState[,"climate.city"]]
	idx.in.myState  <- match(dataFrame.IN[,"location"],myState[,"climate.city"])
	auxi.state.abb <- myState[idx.in.myState,"state.abb"]

	# E. add a combined string with name and year of the code
	auxi.STD <- paste(dataFrame.IN[,"project"],dataFrame.IN[,"standard"],sep="_")

	dataFrame.OUT <- cbind(dataFrame.IN,
			       A.type       = auxi.type,						# add "regular" and "state" identification
			       A.CZ.zone    = auxi.cz,							# add climate zone identification
			       A.CZ.city    = dataFrame.IN[,"location"],				# use "CZ.city" to replace "location"
			       A.CZ.climate = paste(auxi.cz,"(",dataFrame.IN[,"location"],")",sep=""),
			       A.stateAbb   = auxi.state.abb,						# add state abbreviation
			       A.standard   = auxi.STD)

	#
	# there are extra charaters in the "econoSys" and "szvavSys" fileds which need to be removed
	# these extra character varies from prototype to prototype
	#
	this.prototype <- unique(dataFrame.OUT[,"prototype"])
	if (length(this.prototype) != 1)
	{
		cat("There are more than one prototype in the data frame, Check why!\n")
		die
	}else{
		string.econ1.rm <- "_OA_Controller"
		string.econ2.rm <- " OA Controller"
		string.szvav.rm <- "_fan_speed_control"	
		string.damp1.rm <- "_OA_Controller"
		string.damp2.rm <- " OA Controller"


		# remove extra characyters in the system names
		dataFrame.OUT[,"econoSys"]  <- sub(string.econ1.rm,"",dataFrame.OUT[,"econoSys"], ignore.case=TRUE)
		dataFrame.OUT[,"econoSys"]  <- sub(string.econ2.rm,"",dataFrame.OUT[,"econoSys"], ignore.case=TRUE)
		dataFrame.OUT[,"szvavSys"]  <- sub(string.szvav.rm,"",dataFrame.OUT[,"szvavSys"], ignore.case=TRUE)
		dataFrame.OUT[,"damperSys"] <- sub(string.damp1.rm,"",dataFrame.OUT[,"damperSys"],ignore.case=TRUE)
		dataFrame.OUT[,"damperSys"] <- sub(string.damp2.rm,"",dataFrame.OUT[,"damperSys"],ignore.case=TRUE)

		# convert "NoEconomizer" to "no" for "econoFlag" field
		dataFrame.OUT[,"econoFlag"]  <- sub("\\s*NoEconomizer\\s*","No",dataFrame.OUT[,"econoFlag"],ignore.case=TRUE)

		# convert "no" to "n/a" for "szvavFlag" field
		dataFrame.OUT[,"szvavFlag"]  <- sub("\\s*no\\s*","n/a",dataFrame.OUT[,"szvavFlag"],ignore.case=TRUE)

		# convert "no" to "n/a" for "szvavFlag" field
		dataFrame.OUT[,"szvavFlag"]  <- sub("\\s*yes\\s*","VFD",dataFrame.OUT[,"szvavFlag"],ignore.case=TRUE)


		# remove extra character in the damper type field
		dataFrame.OUT[,"damperType"]  <- sub(".*MinOA_Sched.*",                "gravity",  dataFrame.OUT[,"damperType"],ignore.case=TRUE)
		dataFrame.OUT[,"damperType"]  <- sub(".*MinOA_MotorizedDamper_Sched.*","motorized",dataFrame.OUT[,"damperType"],ignore.case=TRUE)



		# use row indices as row name
		row.names(dataFrame.OUT) <- seq(from=1,to=dim(dataFrame.OUT)[1])

		# split the data for both "economizer" and "sz vav"		
		dataFrame.econ.OUT   <- dataFrame.OUT[(!(is.na(dataFrame.OUT[,"econoSys"]))),]
		dataFrame.szvav.OUT  <- dataFrame.OUT[(!(is.na(dataFrame.OUT[,"szvavSys"]))),]
		dataFrame.damper.OUT <- dataFrame.OUT[(!(is.na(dataFrame.OUT[,"damperSys"]))),]

		# still sort the row according to original indices
		o <- order(row.names(dataFrame.econ.OUT))
		dataFrame.econ.OUT <- dataFrame.econ.OUT[o,]

		o <- order(row.names(dataFrame.szvav.OUT))
		dataFrame.szvav.OUT <- dataFrame.szvav.OUT[o,]	

		o <- order(row.names(dataFrame.damper.OUT))
		dataFrame.damper.OUT <- dataFrame.damper.OUT[o,]				

		output.list <- list(myData.econ   = dataFrame.econ.OUT,
				    myData.szvav  = dataFrame.szvav.OUT,
				    myData.damper = dataFrame.damper.OUT)

		return(output.list)
	}
}

# -------------------------------------------------------------------------------------------------
# function: keepSourceFL
#
# this is a function to copy the source file which we extracted information from and save it for verification purpose
# input:  the path of the source and destination and the file name
# output: no trtune information
# -------------------------------------------------------------------------------------------------
keepSourceFL <- function(Path.source,Path.target,source.file)
{
	# check the existence of path and file
	if (!file.exists(Path.target)){print(paste("NOT existing:[",Path.target,"].",            sep=""));dir.create(Path.target,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.source)){ stop(paste("NOT existing:[",Path.source,"].  Check why!",sep=""));die}
	if (!file.exists(source.file)){ stop(paste("NOT existing:[",source.file,"].  Check why!",sep=""));die}

	# file.copy(sources,targets,overwrite=TRUE)	# do not use file.copy since time stamp is changed
	target.file <- paste(Path.target,gsub(paste(Path.source,"/",sep=""),"",source.file),sep="/")

	# copy
	if(.Platform$OS.type == "unix") 
	{
		system(paste("cp -p ",shQuote(source.file),shQuote(target.file)))	# it is very important to put a qutation for the string, otherwise does not work
	}else{
		source.file <- gsub("/","\\\\",source.file)
		target.file <- gsub("/","\\\\",target.file)
		system(paste(Sys.getenv("COMSPEC"),"/c","copy ",source.file,target.file))
	}
}

# -------------------------------------------------------------------------------------------------
# function: readDataSource
#
# this function is used to read all data files where HVAC information will be extracted from
# input:  The prototype name
# output: The file name of a R binary file which consists of a list of data frames [list.df], 
#                                                            a list of system names [list.sys] 
#                                                            and the actual data frames all save in a R binary data file [source_data.Rdata].
# The file name will be used to load the saved R binary data file in the main code for each prototype.
# 
# The systems and data need to bextracted are specified for each prototypes in this subroutine
# If any new data need to be extracted OR existed extracted data need to be deleted, revise this subroutine.
# -------------------------------------------------------------------------------------------------
readDataSource <- function(this.prototype)
{

	# define a binary file to hold all data frames generated by this subroutine
	FL.Rdata <- paste(paste(Path.orig.OUT,this.prototype,sep="/"),"source_data.Rdata",sep="/")
	if (file.exists(FL.Rdata)){print(paste(FL.Rdata,"exist.Delete it!"));file.remove(FL.Rdata)}

	if       (this.prototype == "OfficeSmall")
	{
		# create a sub-folder to hold all raw summary file for faciliating check and verification
		Path.source <- paste(Path.Project,this.prototype,"stateAnal2012_sim",sep="/")	# exception: OA will keep using "Path.OA"
		Path.target <- paste(Path.orig.OUT,this.prototype,sep="/")

		# OA and Parm file name for current prototype
		filename.OA   <- paste("airflow",this.prototype,"system_all.csv",sep="_")
		filename.Parm <- paste("stateAnal2012_",this.prototype,"_moreCase_parm.csv",sep="")



		# -------------------------------------------------------------------------------------------
		# define system and process on the state level
		# -------------------------------------------------------------------------------------------		
		sys.types     <- c("PSZ-AC:1","PSZ-AC:2","PSZ-AC:3","PSZ-AC:4","PSZ-AC:5") 		
		col.opt.start <- "HtgSetP_Sch"	# March 1, 2013: Parm variable name for optimum start in the current prototype	

		# -------------------------------------------------------------------------------------------
		# reading data
		# -------------------------------------------------------------------------------------------
		# 1. Cooling Coil Capacity and 
		#    Read filename.DXCoil file to collect total coil capacity							
		FL.dx.coil  <- paste(Path.source,filename.DXCoil,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.dx.coil)
		if (!file.exists(FL.dx.coil)){stop(paste("NOT existing:[",FL.dx.coil, "].  Check why!",sep=""));die}		
		myDXCoil <- read.csv(file=FL.dx.coil,header=TRUE,stringsAsFactors=FALSE)
		myDXCoil <- expandSummary(myDXCoil)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 3. OA rate
		#    Read airflow*.csv file to collect system OA rate		
		FL.oa <- paste(Path.OA,filename.OA,sep="/")	
		keepSourceFL(Path.OA,Path.target,FL.oa)
		if (!file.exists(FL.oa)){stop(paste("NOT existing:[",FL.oa, "].  Check why!",sep=""));die}
		myOA <- read.csv(file=FL.oa,header=TRUE,stringsAsFactors=FALSE)
		myOA <- expandOA(myOA)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 4. Damper Type
		#    Read summary_flag_output.csv file to collect system economizer type
		FL.econSZvav <- paste(Path.source,filename.econSZvav,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.econSZvav)
		if (!file.exists(FL.econSZvav)){stop(paste("NOT existing:[",FL.econSZvav, "].  Check why!",sep=""));die}
		myEconSZvav <- read.csv(file=FL.econSZvav,header=TRUE,stringsAsFactors=FALSE)
		myList <- expandEconSZvav(myEconSZvav)
		myDamper <- myList[[3]]		# the first data frame in the list is [myData.econ]
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 2. Supply flow rate: March 1, 2013: revised based on Brian's updated list for extraction
		# 6. Supply Fan Power
		#    Read summary_Fans.sizing.csv file to collect supply air flow rate
		FL.fan <- paste(Path.source,filename.Fan,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.fan)
		if (!file.exists(FL.fan)){stop(paste("NOT existing:[",FL.fan, "].  Check why!",sep=""));die}
		myFan  <- read.csv(file=FL.fan,header=TRUE,stringsAsFactors=FALSE)
		myFan  <- expandSummary(myFan)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 5. optimum start: March 1, 2013: revised based on Brian's updated list for extraction
		#    Read parm csv file to collect {optimum start flag}		
		FL.parm <- paste(Path.source,filename.Parm,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.parm)
		if (!file.exists(FL.parm)){stop(paste("NOT existing:[",FL.parm, "].  Check why!",sep=""));die}
		myParm <- read.csv(file=FL.parm,header=TRUE,stringsAsFactors=FALSE)
		myParm <- expandParm(myParm)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.parm,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.parm,"]\n",sep=""),file=FL.LOG,append=TRUE)	

		# save all data frame created into a Rdata file to be loaded	
		list.sys <- c("sys.types","col.opt.start")
		list.df  <- c("myDXCoil","myOA","myDamper","myFan","myParm")
	      # command.string <- paste("save(file=\"",FL.Rdata,"\",list.sys,list.df,myDXCoil,myOA,myDamper,myFan,myParm,sys.types,col.opt.start)",sep="")		      
		command.string <- paste("save(file=\"",FL.Rdata,"\",",paste(c("list.sys","list.df",c(list.df,list.sys)),collapse=","),")",sep="")			
		eval(parse(text=command.string))


	}else if (this.prototype == "OfficeLarge")
	{
		# create a sub-folder to hold all raw summary file for faciliating check and verification
		Path.source <- paste(Path.Project,this.prototype,"stateAnal2012_sim",sep="/")	# exception: OA will keep using "Path.OA"
		Path.target <- paste(Path.orig.OUT,this.prototype,sep="/")

		# OA and Parm file name for current prototype
		filename.OA   <- paste("airflow",this.prototype,"system_all.csv",sep="_")
		filename.Parm <- paste("stateAnal2012_",this.prototype,"_moreCase_parm.csv",sep="")

		# -------------------------------------------------------------------------------------------
		# define system and process on the state level
		# -------------------------------------------------------------------------------------------		               
		sys.main          <- c("CAV_BAS","VAV_BOT WITH REHEAT","VAV_MID WITH REHEAT","VAV_TOP WITH REHEAT") 	# systems in the fan summary file
		sys.DataCN        <- c("DATACENTER BASEMENT","DATACENTER BOT","DATACENTER MID","DATACENTER TOP") 	# data center fan system
		sys.erv           <- c("VAV_bot_WITH_REHEAT","VAV_mid_WITH_REHEAT","VAV_top_WITH_REHEAT") 		# systems for ERV
		sys.chiller       <- c("COOLSYS1 CHILLER1","COOLSYS1 CHILLER2")						# chillers, cooling tower
		sys.boiler        <- c("HEATSYS1 BOILER")								# boiler
		sys.coolingTower  <- c("TOWERWATERSYS COOLTOWER 1","TOWERWATERSYS COOLTOWER 2")				# cooling tower
		sys.pump.main     <- c("COOLSYS1 PUMP","COOLSYS1 PUMP SECONDARY","HEATSYS1 PUMP","TOWERWATERSYS PUMP")	# all pumps (chiller water, hot water and condensed water 
		sys.pump.DataCN   <- c("PLANT CIRC PUMP")								# data center pump
		sys.CK            <- c("add_ck_vav_bot","add_ck_vav_mid","add_ck_vav_top")				# VAV for CK
		sys.econ          <- c("CAV_bas","VAV_bot WITH REHEAT","VAV_mid WITH REHEAT","VAV_top WITH REHEAT","AirLoop DataCenter Basement","AirLoop DataCenter bot","AirLoop DataCenter mid","AirLoop DataCenter top") 
		sys.damper.main   <- c("CAV_bas","VAV_bot WITH REHEAT","VAV_mid WITH REHEAT","VAV_top WITH REHEAT") 	# systems in the fan summary file
		sys.damper.DataCN <- c("AirLoop DataCenter Basement","AirLoop DataCenter bot","AirLoop DataCenter mid","AirLoop DataCenter top") 	# data center fan system
		sys.optStart      <- c("VAV_BOT","VAV_MID","VAV_TOP","CAV_BAS") 					# March 1, 2013:
		sys.pump.secondary<- c("COOLSYS1 PUMP SECONDARY")							# March 4, 2013:
		col.opt.start     <- "HtgSetP_Sch"									# March 1, 2013: Parm variable name for optimum start in the current prototype	



		# -------------------------------------------------------------------------------------------
		# reading data
		# -------------------------------------------------------------------------------------------	
		# 1.  Supply flow rate
		# 17. Supply Fan Power
		#    Read summary_Fans.sizing.csv file to collect supply air flow rate
		FL.fan <- paste(Path.source,filename.Fan,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.fan)
		if (!file.exists(FL.fan)){stop(paste("NOT existing:[",FL.fan, "].  Check why!",sep=""));die}
		myFan  <- read.csv(file=FL.fan,header=TRUE,stringsAsFactors=FALSE)
		myFan  <- expandSummary(myFan)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 3a. Cooling Coil Capacity and 
		# 4a. Cooling Coil type
		#    Read filename.DXCoil file to collect total coil capacity
		FL.dx.coil <- paste(Path.source,filename.DXCoil,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.dx.coil)
		if (!file.exists(FL.dx.coil)){stop(paste("NOT existing:[",FL.dx.coil, "].  Check why!",sep=""));die}
		myDXCoil  <- read.csv(file=FL.dx.coil,header=TRUE,stringsAsFactors=FALSE)
		myDXCoil  <- expandSummary(myDXCoil)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 3b. Cooling Coil Capacity and 
		# 4b. Cooling Coil type
		# Read summary_datacenter_output.csv file to collect datacenter DX coil capacity
		FL.dx.coil2 <- paste(Path.source,filename.DataCenter,sep="/")
		keepSourceFL(Path.source,Path.target,FL.dx.coil2)
		if (!file.exists(FL.dx.coil2)){stop(paste("NOT existing:[",FL.dx.coil2, "].  Check why!",sep=""));die}		
		myDXCoil2  <- read.csv(file=FL.dx.coil2,header=TRUE,stringsAsFactors=FALSE)
		myDXCoil2  <- expandSummary(myDXCoil2)  
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil2,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil2,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 2.  Supply air temperature reset yes/no
		# 3.  Ventilation reset
		# 7.  ERV flag
		# 8.  optimum start
		# 16. pump pressure setpoint reset
		#    Read parm csv file to collect {supply air temp. reset flag, ventilation reset flag, economizer type and flag, energy recoveery flag}		
		FL.parm <- paste(Path.source,filename.Parm,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.parm)
		if (!file.exists(FL.parm)){stop(paste("NOT existing:[",FL.parm, "].  Check why!",sep=""));die}
		myParm <- read.csv(file=FL.parm,header=TRUE,stringsAsFactors=FALSE)
		myParm <- expandParm(myParm)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.parm,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.parm,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 4a. OA rate
		#    Read airflow*.csv file to collect system OA rate		
		FL.oa <- paste(Path.OA,filename.OA,sep="/")	
		keepSourceFL(Path.OA,Path.target,FL.oa)
		if (!file.exists(FL.oa)){stop(paste("NOT existing:[",FL.oa, "].  Check why!",sep=""));die}
		myOA <- read.csv(file=FL.oa,header=TRUE,stringsAsFactors=FALSE)
		myOA <- expandOA(myOA)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 5. damper
		# 6. Economizer Type
		#    Read summary_flag_output.csv file to collect system economizer type
		FL.econSZvav <- paste(Path.source,filename.econSZvav,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.econSZvav)
		if (!file.exists(FL.econSZvav)){stop(paste("NOT existing:[",FL.econSZvav, "].  Check why!",sep=""));die}
		myEconSZvav <- read.csv(file=FL.econSZvav,header=TRUE,stringsAsFactors=FALSE)
		myList   <- expandEconSZvav(myEconSZvav)
		myEcon   <- myList[[1]]		# the first data frame in the list is [myData.econ]
		myDamper <- myList[[3]]		# the thrid  data frame in the list is [myData.damper]	# March 1, 2013:
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""),file=FL.LOG,append=TRUE)


		# 9.  Chiller capacity
		# 10. Cooling Tower capacity
		# 11. Boiler capacity
		#     Read summary_Central-Plant.sizing.csv file to collect plant capacity
		FL.plant <- paste(Path.source,filename.Plant,sep="/")
		keepSourceFL(Path.source,Path.target,FL.plant)
		if (!file.exists(FL.plant)){stop(paste("NOT existing:[",FL.plant, "].  Check why!",sep=""));die}
		myPlant  <- read.csv(file=FL.plant,header=TRUE,stringsAsFactors=FALSE)
		myPlant  <- expandSummary(myPlant) 
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.plant,"]\n",sep="")) 
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.plant,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 12. Chiller Water flow rate
		# 13. Hot Water flow rate
		# 14. Condenser water flow rate
		#     Read summary_pump_flowrate_output.csv file to collect pump loop flow rate
		FL.pump.rate <- paste(Path.source,filename.PumpRate,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.pump.rate)
		if (!file.exists(FL.pump.rate)){stop(paste("NOT existing:[",FL.pump.rate, "].  Check why!",sep=""));die}
		myPumpRate  <- read.csv(file=FL.pump.rate,header=TRUE,stringsAsFactors=FALSE)
		myPumpRate  <- expandSummary(myPumpRate)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.pump.rate,"]\n",sep=""))   
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.pump.rate,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 15. pump flow control
		# 18. Pump Power
		#     Read summary_Pumps.sizing.csv file to collect pump power
		FL.pump <- paste(Path.source,filename.PumpSize,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.pump)
		if (!file.exists(FL.pump)){stop(paste("NOT existing:[",FL.pump, "].  Check why!",sep=""));die}
		myPump  <- read.csv(file=FL.pump,header=TRUE,stringsAsFactors=FALSE)
		myPump  <- expandSummary(myPump)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.pump,"]\n",sep=""))    
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.pump,"]\n",sep=""),file=FL.LOG,append=TRUE)   

		# 4b. February 14, 2013: for replacement of mutz OA: mutz adjusted OA for this prototype
		myOA.mutz <- subset(myData.OA.mutz,prototype == this.prototype)
		cat(paste(this.prototype,"\tmutz adjusted OA have been extracted!\n",sep="")) 
		cat(paste(this.prototype,"\tmutz adjusted OA have been extracted!\n",sep=""),file=FL.LOG,append=TRUE) 

		# save all data frame created into a Rdata file to be loaded
		list.sys <- c("sys.main","sys.DataCN","sys.erv","sys.chiller","sys.boiler","sys.coolingTower","sys.pump.main","sys.pump.DataCN","sys.CK","sys.econ","sys.damper.main","sys.damper.DataCN","sys.optStart","sys.pump.secondary","col.opt.start")
		list.df  <- c("myOA","myDXCoil","myDXCoil2","myFan","myParm","myEcon","myDamper","myPlant","myPumpRate","myPump","myOA.mutz")

	      # command.string <- paste("save(file=\"",FL.Rdata,"\",list.sys,list.df,myOA,myDXCoil,myDXCoil2,myFan,myParm,myEcon,myDamper,myPlant,myPumpRate,myPump,myOA.mutz,sys.main,sys.DataCN,sys.erv,sys.chiller,sys.boiler,sys.coolingTower,sys.pump.main,sys.pump.DataCN,sys.CK,sys.econ,sys.damper.main,sys.damper.DataCN,sys.optStart,sys.pump.secondary,col.opt.start)",sep="")			
		command.string <- paste("save(file=\"",FL.Rdata,"\",",paste(c("list.sys","list.df",c(list.df,list.sys)),collapse=","),")",sep="")
		eval(parse(text=command.string))

	}else if (this.prototype == "RetailStandalone")
	{
		# create a sub-folder to hold all raw summary file for faciliating check and verification
		Path.source <- paste(Path.Project,this.prototype,"stateAnal2012_sim",sep="/")	# exception: OA will keep using "Path.OA"
		Path.target <- paste(Path.orig.OUT,this.prototype,sep="/")

		# OA and Parm file name for current prototype
		filename.OA   <- paste("airflow",this.prototype,"system_all.csv",sep="_")
		filename.Parm <- paste("stateAnal2012_",this.prototype,"_moreCase_parm.csv",sep="")

		# -------------------------------------------------------------------------------------------
		# define system and process on the state level
		# -------------------------------------------------------------------------------------------		
		sys.types     <- c("PSZ-AC:1","PSZ-AC:2","PSZ-AC:3","PSZ-AC:4") 				# sort(unique(myDXCoil[,"name"]))
		sys.fan1      <- c("FRONT_ENTRY UNIT HEATERFAN") 						# sort(unique(myDXCoil[,"name"]))
		sys.fan2      <- c("PSZ-AC:1","PSZ-AC:2","PSZ-AC:3","PSZ-AC:4") 				# sort(unique(myDXCoil[,"name"]))
		sys.econ      <- c("PSZ-AC:1","PSZ-AC:2","PSZ-AC:3","PSZ-AC:4")					# systems: need to remove "_OA_Controller"
		sys.optStart  <- c("FRONT_ENTRY UNIT HEATER","PSZ-AC:1","PSZ-AC:2","PSZ-AC:3","PSZ-AC:4") 	# March 1, 2013:
		sys.sz_vav    <- c("PSZ1","PSZ2")								# March 1, 2013: only need PSZ1 and PSZ2
		sys.damper    <- c("PSZ-AC:1","PSZ-AC:2","PSZ-AC:3","PSZ-AC:4")					# March 1, 2013: systems: need to remove "_OA_Controller"
		sys.erv       <- c("PSZ_AC_2")									# March 1, 2013:
		col.opt.start <- "HtgSetP_Sch"									# March 1, 2013: Parm variable name for optimum start in the current prototype	

		# -------------------------------------------------------------------------------------------
		# reading data
		# -------------------------------------------------------------------------------------------	
		# 
		# 1. Cooling Capacity
		#    Read summary_DX-Coils.sizing.csv file to collect total coil capacity
		FL.dx.coil <- paste(Path.source,filename.DXCoil,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.dx.coil)
		if (!file.exists(FL.dx.coil)){stop(paste("NOT existing:[",FL.dx.coil, "].  Check why!",sep=""));die}		
		myDXCoil  <- read.csv(file=FL.dx.coil,header=TRUE,stringsAsFactors=FALSE)
		myDXCoil  <- expandSummary(myDXCoil)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 
		# 4. system OA rate
		#    Read airflow*.csv file to collect system OA rate
		FL.oa <- paste(Path.OA,filename.OA,sep="/")
		keepSourceFL(Path.OA,Path.target,FL.oa)
		if (!file.exists(FL.oa)){stop(paste("NOT existing:[",FL.oa, "].  Check why!",sep=""));die}
		myOA <- read.csv(file=FL.oa,header=TRUE,stringsAsFactors=FALSE)
		myOA <- expandOA(myOA)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 3. SZ VAV 
		# 5. damper
		# 6. economizer
		#    Read summary_flag_output.csv file to collect system economizer type
		FL.econSZvav <- paste(Path.source,filename.econSZvav,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.econSZvav)
		if (!file.exists(FL.econSZvav)){stop(paste("NOT existing:[",FL.econSZvav, "].  Check why!",sep=""));die}
		myEconSZvav <- read.csv(file=FL.econSZvav,header=TRUE,stringsAsFactors=FALSE)
		myList   <- expandEconSZvav(myEconSZvav)
		myEcon   <- myList[[1]]		# the first  data frame in the list is [myData.econ]
		mySZvav  <- myList[[2]]		# the second data frame in the list is [myData.szvav]
		myDamper <- myList[[3]]		# the thrid  data frame in the list is [myData.damper]	# March 1, 2013:
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 2  supply flow rate
		# 9. supply fan power
		#    Read summary_Fans.sizing.csv file to collect supply air flow rate
		FL.fan <- paste(Path.source,filename.Fan,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.fan)
		if (!file.exists(FL.fan)){stop(paste("NOT existing:[",FL.fan, "].  Check why!",sep=""));die}
		myFan  <- read.csv(file=FL.fan,header=TRUE,stringsAsFactors=FALSE)
		myFan  <- expandSummary(myFan)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 7. erv: March 1, 2013: revised based on Brian's updated list for extraction
		# 8. optimum start
		#    Read parm csv file to collect {optimum start flag}		
		FL.parm <- paste(Path.source,filename.Parm,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.parm)
		if (!file.exists(FL.parm)){stop(paste("NOT existing:[",FL.parm, "].  Check why!",sep=""));die}
		myParm <- read.csv(file=FL.parm,header=TRUE,stringsAsFactors=FALSE)
		myParm <- expandParm(myParm)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.parm,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.parm,"]\n",sep=""),file=FL.LOG,append=TRUE)	

		# save all data frame created into a Rdata file to be loaded
		# save all data frame created into a Rdata file to be loaded
		list.sys <- c("sys.types","sys.fan1","sys.fan2","sys.econ","sys.optStart","sys.sz_vav","sys.damper","sys.erv","col.opt.start")
		list.df  <- c("myOA","myDXCoil","myFan","myParm","myEcon","mySZvav","myDamper")

	      # command.string <- paste("save(file=\"",FL.Rdata,"\",list.sys,list.df,myOA,myDXCoil,myFan,myParm,myEcon,mySZvav,myDamper,sys.types,sys.fan1,sys.fan2,sys.econ,sys.optStart,sys.sz_vav,sys.damper,sys.erv,col.opt.start)",sep="")			
		command.string <- paste("save(file=\"",FL.Rdata,"\",",paste(c("list.sys","list.df",c(list.df,list.sys)),collapse=","),")",sep="")
		eval(parse(text=command.string))

	}else if (this.prototype == "SchoolPrimary")
	{
		# create a sub-folder to hold all raw summary file for faciliating check and verification
		Path.source <- paste(Path.Project,this.prototype,"stateAnal2012_sim",sep="/")	# exception: OA will keep using "Path.OA"
		Path.target <- paste(Path.orig.OUT,this.prototype,sep="/")

		# OA and Parm file name for current prototype
		filename.OA   <- paste("airflow",this.prototype,"system_all.csv",sep="_")
		filename.Parm <- paste("stateAnal2012_",this.prototype,"_moreCase_parm.csv",sep="")

		# -------------------------------------------------------------------------------------------
		# define system and process on the state level
		# -------------------------------------------------------------------------------------------		
		sys.VAV       <- c("VAV_POD_1","VAV_POD_2","VAV_POD_3","VAV_OTHER") 								# VAV systems
		sys.PSZ       <- c("PSZ-AC_1:6","PSZ-AC_2:5","PSZ-AC_2:7")									# CAV systems
		sys.plant     <- c("HEATSYS1")													# Boiler
		sys.pump      <- c("HEATSYS1")											  		# How Water: March 1, 2013: deleted the "SHWSYS1" 
		sys.CKflag    <- c("add_ck_vav_other","add_ck_vav_pod_1","add_ck_vav_pod_2","add_ck_vav_pod_3")					# VAV for CK
		sys.ERVflag   <- c("VAV_Pod_1_ERV","VAV_Pod_2_ERV","VAV_Pod_3_ERV","VAV_OTHER_ERV","PSZ_AC_2_5_ERV","PSZ_AC_2_7_ERV") 		# system type for erv flag
		sys.econ      <- c("VAV_POD_1","VAV_POD_2","VAV_POD_3","VAV_OTHER","PSZ-AC_2:5","PSZ-AC_1:6","PSZ-AC_2:7")			# systems: need to remove "_OA_Controller"
		sys.sz_vav    <- c("PSZ2_5","PSZ1_6","PSZ2_7")											# systems: need to remove "_fan_speed_control"
		sys.damper    <- c("VAV_POD_1","VAV_POD_2","VAV_POD_3","VAV_OTHER","PSZ-AC_2:5","PSZ-AC_1:6","PSZ-AC_2:7")			# March 1, 2013:
		sys.optStart  <- c("VAV_POD_1","VAV_POD_2","VAV_POD_3","VAV_OTHER","PSZ-AC_1:6","PSZ-AC_2:5","PSZ-AC_2:7") 			# March 1, 2013:
		col.opt.start <- "HtgSetP_Sch"													# March 1, 2013: Parm variable name for optimum start in the current prototype	



		# -------------------------------------------------------------------------------------------
		# reading data
		# -------------------------------------------------------------------------------------------
		#
		# 1. Cooling capacity  
		#    Read summary_DX-Coils.sizing.csv file to collect total coil capacity
		FL.dx.coil <- paste(Path.source,filename.DXCoil,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.dx.coil)
		if (!file.exists(FL.dx.coil)){stop(paste("NOT existing:[",FL.dx.coil, "].  Check why!",sep=""));die}		
		myDXCoil  <- read.csv(file=FL.dx.coil,header=TRUE,stringsAsFactors=FALSE)
		myDXCoil  <- expandSummary(myDXCoil)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 4.  Supply Air Temperature reset
		# 5.  Ventilation reset
		# 9.  ERV flag
		# 11. optimum start
		#    Read parm csv file to collect {supply air temp. reset flag, ventilation reset flag, economizer type and flag, energy recoveery flag}
		FL.parm <- paste(Path.source,filename.Parm,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.parm)
		if (!file.exists(FL.parm)){stop(paste("NOT existing:[",FL.parm, "].  Check why!",sep=""));die}
		myParm  <- read.csv(file=FL.parm,header=TRUE,stringsAsFactors=FALSE)
		myParm  <- expandParm(myParm)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.parm,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.parm,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 6a. system OA rate
		#    Read airflow*.csv file to collect system OA rate
		FL.oa <- paste(Path.OA,filename.OA,sep="/")	
		keepSourceFL(Path.OA,Path.target,FL.oa)
		if (!file.exists(FL.oa)){stop(paste("NOT existing:[",FL.oa, "].  Check why!",sep=""));die}
		myOA  <- read.csv(file=FL.oa,header=TRUE,stringsAsFactors=FALSE)
		myOA  <- expandOA(myOA)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 3. SZ VAV
		# 7. damper
		# 8. economizer 
		#    Read summary_flag_output.csv file to collect system economizer type
		FL.econSZvav <- paste(Path.source,filename.econSZvav,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.econSZvav)
		if (!file.exists(FL.econSZvav)){stop(paste("NOT existing:[",FL.econSZvav, "].  Check why!",sep=""));die}
		myEconSZvav <- read.csv(file=FL.econSZvav,header=TRUE,stringsAsFactors=FALSE)
		myList   <- expandEconSZvav(myEconSZvav)
		myEcon   <- myList[[1]]		# the first  data frame in the list is [myData.econ]
		mySZvav  <- myList[[2]]		# the second data frame in the list is [myData.szvav]
		myDamper <- myList[[3]]		# the thrid  data frame in the list is [myData.damper]	# March 1, 2013:
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 2.  supply flow rate
		# 10. Kitchen exhaust fan flow rate
		# 14. supply fan power
		#    Read summary_Fans.sizing.csv file to collect supply air flow rate
		FL.fan <- paste(Path.source,filename.Fan,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.fan)
		if (!file.exists(FL.fan)){stop(paste("NOT existing:[",FL.fan, "].  Check why!",sep=""));die}
		myFan  <- read.csv(file=FL.fan,header=TRUE,stringsAsFactors=FALSE)
		myFan  <- expandSummary(myFan)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 12. Boiler Capacity
		#     Read summary_Central-Plant.sizing.csv file to collect plant capacity
		FL.plant <- paste(Path.source,filename.Plant,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.plant)
		if (!file.exists(FL.plant)){stop(paste("NOT existing:[",FL.plant, "].  Check why!",sep=""));die}
		myPlant  <- read.csv(file=FL.plant,header=TRUE,stringsAsFactors=FALSE)
		myPlant  <- expandSummary(myPlant)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.plant,"]\n",sep="")) 
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.plant,"]\n",sep=""),file=FL.LOG,append=TRUE) 

		# 13. pump loop flow rate
		#     Read summary_pump_flowrate_output.csv file to collect pump loop flow rate
		FL.pump.rate <- paste(Path.source,filename.PumpRate,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.pump.rate)
		if (!file.exists(FL.pump.rate)){stop(paste("NOT existing:[",FL.pump.rate, "].  Check why!",sep=""));die}
		myPumpRate  <- read.csv(file=FL.pump.rate,header=TRUE,stringsAsFactors=FALSE)
		myPumpRate  <- expandSummary(myPumpRate)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.pump.rate,"]\n",sep="")) 
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.pump.rate,"]\n",sep=""),file=FL.LOG,append=TRUE)  

		# 15. pump power
		#     Read summary_Pumps.sizing.csv file to collect pump power
		FL.pump <- paste(Path.source,filename.PumpSize,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.pump)
		if (!file.exists(FL.pump)){stop(paste("NOT existing:[",FL.pump, "].  Check why!",sep=""));die}
		myPump  <- read.csv(file=FL.pump,header=TRUE,stringsAsFactors=FALSE)
		myPump  <- expandSummary(myPump)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.pump,"]\n",sep="")) 
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.pump,"]\n",sep=""),file=FL.LOG,append=TRUE)    

		# 6b. February 14, 2013: for replacement of mutz OA: mutz adjusted OA for this prototype
		myOA.mutz <- subset(myData.OA.mutz,prototype == this.prototype)
		cat(paste(this.prototype,"\tmutz adjusted OA have been extracted!\n",sep="")) 
		cat(paste(this.prototype,"\tmutz adjusted OA have been extracted!\n",sep=""),file=FL.LOG,append=TRUE) 	

		# save all data frame created into a Rdata file to be loaded
		list.sys <- c("sys.VAV","sys.PSZ","sys.plant","sys.pump","sys.CKflag","sys.ERVflag","sys.econ","sys.sz_vav","sys.damper","sys.optStart","col.opt.start")			
		list.df  <- c("myOA","myDXCoil","myFan","myParm","myEcon","mySZvav","myDamper","myPlant","myPumpRate","myPump","myOA.mutz")

	      # command.string <- paste("save(file=\"",FL.Rdata,"\",list.sys,list.df,myOA,myDXCoil,myFan,myParm,myEcon,mySZvav,myDamper,myPlant,myPumpRate,myPump,myOA.mutz,sys.VAV,sys.PSZ,sys.plant,sys.pump,sys.CKflag,sys.ERVflag,sys.econ,sys.sz_vav,sys.damper,sys.optStart,col.opt.start)",sep="")			
		command.string <- paste("save(file=\"",FL.Rdata,"\",",paste(c("list.sys","list.df",c(list.df,list.sys)),collapse=","),")",sep="")
		eval(parse(text=command.string))

	}else if (this.prototype == "HotelSmall")
	{
		# create a sub-folder to hold all raw summary file for faciliating check and verification
		Path.source <- paste(Path.Project,this.prototype,"stateAnal2012_sim",sep="/")	# exception: OA will keep using "Path.OA"
		Path.target <- paste(Path.orig.OUT,this.prototype,sep="/")

		# OA and Parm file name for current prototype
		filename.OA   <- paste("airflow",this.prototype,"system_all.csv",sep="_")
		filename.Parm <- paste("stateAnal2012_",this.prototype,"_moreCase_parm.csv",sep="")


		# -------------------------------------------------------------------------------------------
		# define system and process on the state level
		# -------------------------------------------------------------------------------------------		
		sys.types     <- c("SAC_FRONTOFFICE","SAC_FRONTLOUNGE","SAC_MEETINGROOM","SAC_EXC_EMPLGE_RESTRM") 
		sys.econ      <- c("SAC_FrontOffice","SAC_FrontLounge","SAC_MeetingRoom","SAC_Exc_EmpLge_RestRm")		# systems: need to remove " OA Controller"
		sys.sz_vav    <- c("SAC_FO","SAC_FL","SAC_MR","SAC_Exc")							# systems: need to remove "_fan_speed_control"
		sys.damper    <- c("SAC_FrontLounge")										# March 1, 2013
		sys.fan       <- c("SAC_FRONTLOUNGE","SAC_EXC_EMPLGE_RESTRM") 							# March 1, 2013
		col.opt.start <- "HtgSetP_Sch"

		# -------------------------------------------------------------------------------------------
		# reading data
		# -------------------------------------------------------------------------------------------
		# 
		# 1. Cooling capacity   
		#    Read summary_DX-Coils.sizing.csv file to collect total coil capacity
		FL.dx.coil <- paste(Path.source,filename.DXCoil,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.dx.coil)
		if (!file.exists(FL.dx.coil)){stop(paste("NOT existing:[",FL.dx.coil, "].  Check why!",sep=""));die}
		myDXCoil  <- read.csv(file=FL.dx.coil,header=TRUE,stringsAsFactors=FALSE)
		myDXCoil  <- expandSummary(myDXCoil)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 4. damper
		# 5. economizer 
		#    Read summary_flag_output.csv file to collect system economizer type
		FL.econSZvav <- paste(Path.source,filename.econSZvav,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.econSZvav)
		if (!file.exists(FL.econSZvav)){stop(paste("NOT existing:[",FL.econSZvav, "].  Check why!",sep=""));die}
		myEconSZvav <- read.csv(file=FL.econSZvav,header=TRUE,stringsAsFactors=FALSE)
		myList   <- expandEconSZvav(myEconSZvav)
		myEcon   <- myList[[1]]		# the first  data frame in the list is [myData.econ]
		mySZvav  <- myList[[2]]		# the second data frame in the list is [myData.szvav]
		myDamper <- myList[[3]]		# the first data frame in the list is [myData.econ]
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.econSZvav,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 
		# 3. system OA rate
		#    Read airflow*.csv file to collect system OA rate
		FL.oa <- paste(Path.OA,filename.OA,sep="/")	
		keepSourceFL(Path.OA,Path.target,FL.oa)
		if (!file.exists(FL.oa)){stop(paste("NOT existing:[",FL.oa, "].  Check why!",sep=""));die}
		myOA <- read.csv(file=FL.oa,header=TRUE,stringsAsFactors=FALSE)
		myOA <- expandOA(myOA)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 2. supply flow rate
		# 7. supply fan power
		#    Read summary_Fans.sizing.csv file to collect supply air flow rate
		FL.fan <- paste(Path.source,filename.Fan,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.fan)
		if (!file.exists(FL.fan)){stop(paste("NOT existing:[",FL.fan, "].  Check why!",sep=""));die}
		myFan  <- read.csv(file=FL.fan,header=TRUE,stringsAsFactors=FALSE)
		myFan  <- expandSummary(myFan)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""),file=FL.LOG,append=TRUE)	

		# save all data frame created into a Rdata file to be loaded
		list.sys <- c("sys.types","sys.econ","sys.sz_vav","sys.damper","sys.fan","col.opt.start")			
		list.df  <- c("myOA","myDXCoil","myFan","myEcon","mySZvav","myDamper")

	      # command.string <- paste("save(file=\"",FL.Rdata,"\",list.sys,list.df,myOA,myDXCoil,myFan,myEcon,mySZvav,myDamper,sys.types,sys.econ,sys.sz_vav,sys.damper,sys.fan,col.opt.start)",sep="")			
		command.string <- paste("save(file=\"",FL.Rdata,"\",",paste(c("list.sys","list.df",c(list.df,list.sys)),collapse=","),")",sep="")
		eval(parse(text=command.string))

	}else if (this.prototype == "ApartmentMidRise")
	{
		# create a sub-folder to hold all raw summary file for faciliating check and verification
		Path.source <- paste(Path.Project,this.prototype,"stateAnal2012_sim",sep="/")	# exception: OA will keep using "Path.OA"
		Path.target <- paste(Path.orig.OUT,this.prototype,sep="/")

		# OA and Parm file name for current prototype
		filename.OA   <- paste("airflow",this.prototype,"system_all.csv",sep="_")
		filename.Parm <- paste("stateAnal2012_",this.prototype,"_moreCase_parm.csv",sep="")


		# -------------------------------------------------------------------------------------------
		# reading data
		# -------------------------------------------------------------------------------------------	
		# 
		# 1. Cooling capacity
		# 2. Cooling Coil Type    
		#    Read summary_DX-Coils.sizing.csv file to collect total coil capacity
		FL.dx.coil <- paste(Path.source,filename.DXCoil,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.dx.coil)
		if (!file.exists(FL.dx.coil)){stop(paste("NOT existing:[",FL.dx.coil, "].  Check why!",sep=""));die}
		myDXCoil  <- read.csv(file=FL.dx.coil,header=TRUE,stringsAsFactors=FALSE)
		myDXCoil  <- expandSummary(myDXCoil)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.dx.coil,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 
		# 3. system OA rate
		#    Read airflow*.csv file to collect system OA rate
		FL.oa <- paste(Path.OA,filename.OA,sep="/")	
		keepSourceFL(Path.OA,Path.target,FL.oa)
		if (!file.exists(FL.oa)){stop(paste("NOT existing:[",FL.oa, "].  Check why!",sep=""));die}
		myOA <- read.csv(file=FL.oa,header=TRUE,stringsAsFactors=FALSE)
		myOA <- expandOA(myOA)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.oa,"]\n",sep=""),file=FL.LOG,append=TRUE)

		# 
		# 4. Supply flow rate
		# 5. supply fan type
		# 6. supply fan power
		#    Read summary_Fans.sizing.csv file to collect supply air flow rate
		FL.fan <- paste(Path.source,filename.Fan,sep="/")	
		keepSourceFL(Path.source,Path.target,FL.fan)
		if (!file.exists(FL.fan)){stop(paste("NOT existing:[",FL.fan, "].  Check why!",sep=""));die}
		myFan  <- read.csv(file=FL.fan,header=TRUE,stringsAsFactors=FALSE)
		myFan  <- expandSummary(myFan)
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""))
		cat(paste(this.prototype,"\tthe E+ simulation results have been read in from [",FL.fan,"]\n",sep=""),file=FL.LOG,append=TRUE)	


		# -------------------------------------------------------------------------------------------
		# define system and process on the state level
		# For ApartmentMidRise, this block has to be here after the data have been read in.
		# For AprtmentMidRise, the names are extracted from the data frame, so this code block has to be here after the data frames are areayd
		# -------------------------------------------------------------------------------------------		
		sys.coil <- sort(unique(myDXCoil[,"Name"])) #c("SAC_FRONTOFFICE","SAC_FRONTLOUNGE","SAC_MEETINGROOM","SAC_EXC_EMPLGE_RESTRM") #sort(unique(myDXCoil[,"name"]))
		sys.fan  <- sort(unique(myFan[,"Name"])) 
		sys.oa   <- sort(unique(myOA[,"systemName"])) 

		# save all data frame created into a Rdata file to be loaded
		list.sys <- c("sys.coil","sys.fan","sys.oa")			
		list.df  <- c("myOA","myDXCoil","myFan")

	      # command.string <- paste("save(file=\"",FL.Rdata,"\",list.sys,list.df,myOA,myDXCoil,myFan,sys.coil,sys.fan,sys.oa,col.opt.start)",sep="")			
		command.string <- paste("save(file=\"",FL.Rdata,"\",",paste(c("list.sys","list.df",c(list.df,list.sys)),collapse=","),")",sep="")
		eval(parse(text=command.string))

	}

	# return the file name of the R data file
	return(FL.Rdata)
}


# -------------------------------------------------------------------------------------------------
# function: output_OfficeSmall
#
# this function is used to organize the output of a give prototype in the desired format and put in a data frame [myOut.clm]
# input:  The prototype name
# output: the organized output for the given prototype in a data frame [myOut.clm].  
#
# The output is prototype specific, so we have one of such a function for each prototype
# -------------------------------------------------------------------------------------------------
output_OfficeSmall <- function(thisPrototype)
{	
	# *** climate loop ***
	idx.climate <- 0
	# For each climate zone, collect cooling coil capacity and system OA rates
	for (this.climate in climateZone.array)
	{
		idx.climate <- idx.climate + 1

		# [DATA SUBSET] of current state-standard-climate
		myData.thisClimate.dx   <- subset(myData.thisStandard.dx,  A.CZ.city == this.climate)
		myData.thisClimate.oa   <- subset(myData.thisStandard.oa,  A.CZ.city == this.climate)
		myData.thisClimate.fan  <- subset(myData.thisStandard.fan, A.CZ.city == this.climate)		# March 1, 2013:
		myData.thisClimate.parm <- subset(myData.thisStandard.parm,A.CZ.city == this.climate)		# March 1, 2013:
		if (length(grep("_YES_",myData.thisClimate.parm[,col.opt.start],ignore.case=TRUE)))		# March 1, 2013:
		{												# March 1, 2013:
			optimum.start.flag <- "yes"								# March 1, 2013:
		}else if (length(grep("_NO_",myData.thisClimate.parm[,col.opt.start],ignore.case=TRUE)))	# March 1, 2013:
		{												# March 1, 2013:
			optimum.start.flag <- "no"								# March 1, 2013:
		}else												# March 1, 2013:
		{												# March 1, 2013:	
			optimum.start.flag <- "NA"								# March 1, 2013:
		}												# March 1, 2013:
		myData.thisClimate.damper  <- subset(myData.thisStandard.damper,    A.CZ.city == this.climate)	# March 1, 2013:


		idx.systype <- 0
		# 
		# 1. Cooling Capacity
		#    For each system type, collect the cooling capacity and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.dx[match(paste(sys.types,"HEAT PUMP DX COOLING COIL",sep=" "),myData.thisClimate.dx[,"Name"]),"Nominal.Total.Capacity..W.",drop=FALSE] * factor_w_ton
		row.names(a) <- paste(sys.types,"Cooling Capacity [ton]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# # 
		# # 2. Cooling Coil Type: March 1, 2013: revised based on Brian's updated list for extraction
		# #    For each system type, collect the cooling coil type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.dx[match(paste(sys.types,"HEAT PUMP DX COOLING COIL",sep=" "),myData.thisClimate.dx[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.types,"Cooling Coil Type",saep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 
		# 2. supply air flow: March 1, 2013: revised based on Brian's updated list for extraction
		# For each main airloop system type, collect the supply air flow rate and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.types,"FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Max.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_cfm
		row.names(a) <- paste(sys.types,"Supply Flow Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 
		# 3. OA rate
		#    For each system type, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.oa[match(sys.types,myData.thisClimate.oa[,"systemName"]),"value",drop=FALSE]*factor_m3s_cfm 
		row.names(a) <- paste(sys.types,"OA Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 
		# 4. damper type: March 1, 2013: revised based on Brian's updated list for extraction
		# For all systems, collect the damper type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.damper[match(sys.types,myData.thisClimate.damper[,"damperSys"]),"damperType",drop=FALSE]
		row.names(a) <- paste(sys.types,"OA Damper [gravity, motorized]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 
		# 5. optimum start flag: March 1, 2013: revised based on Brian's updated list for extraction 
		#    For each system type, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(matrix(rep(optimum.start.flag,length(sys.types)),length(sys.types),1))
		row.names(a) <- paste(sys.types,"Optimum Start [yes/no]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 
		# 6. supply fan power: March 1, 2013: revised based on Brian's updated list for extraction
		# For each main airloop system type, collect the supply fan power and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.types,"FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.types,"Fan Power [bhp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}	

		# Then combind those columns for each climate zone
		if (idx.climate == 1)
		{
			myOut.clm <- myOut.sys
		}else{
			myOut.clm <- cbind(myOut.clm,myOut.sys) 
		}
	}

	return(myOut.clm)
}


# -------------------------------------------------------------------------------------------------
# function: output_OfficeLarge
#
# this function is used to organize the output of a give prototype in the desired format and put in a data frame [myOut.clm]
# input:  The prototype name
# output: the organized output for the given prototype in a data frame [myOut.clm].  
#
# The output is prototype specific, so we have one of such a function for each prototype
# -------------------------------------------------------------------------------------------------output_OfficeLarge <- function(thisPrototype)
output_OfficeLarge <- function(thisPrototype)
{	

	# *** climate loop ***
	idx.climate <- 0
	# For each climate zone, collect cooling coil capacity and system OA rates
	for (this.climate in climateZone.array)
	{
		idx.climate <- idx.climate + 1

		# [DATA SUBSET] of current state-standard-climate
		myData.thisClimate.fan      <- subset(myData.thisStandard.fan,     A.CZ.city == this.climate)
		myData.thisClimate.dx       <- subset(myData.thisStandard.dx,      A.CZ.city == this.climate)
		myData.thisClimate.dx2      <- subset(myData.thisStandard.dx2,     A.CZ.city == this.climate)
		myData.thisClimate.parm     <- subset(myData.thisStandard.parm,    A.CZ.city == this.climate)
		myData.thisClimate.oa       <- subset(myData.thisStandard.oa,      A.CZ.city == this.climate)
		myData.thisClimate.plant    <- subset(myData.thisStandard.plant,   A.CZ.city == this.climate)
		myData.thisClimate.pumprate <- subset(myData.thisStandard.pumprate,A.CZ.city == this.climate)
		myData.thisClimate.pump     <- subset(myData.thisStandard.pump,    A.CZ.city == this.climate)
		myData.thisClimate.econ     <- subset(myData.thisStandard.econ,    A.CZ.city == this.climate)
		if (length(grep("_YES_",myData.thisClimate.parm[,col.opt.start],ignore.case=TRUE)))		# March 1, 2013:
		{												# March 1, 2013:
			optimum.start.flag <- "yes"								# March 1, 2013:
		}else if (length(grep("_NO_",myData.thisClimate.parm[,col.opt.start],ignore.case=TRUE)))	# March 1, 2013:
		{												# March 1, 2013:
			optimum.start.flag <- "no"								# March 1, 2013:
		}else												# March 1, 2013:
		{												# March 1, 2013:	
			optimum.start.flag <- "NA"								# March 1, 2013:
		}												# March 1, 2013:
		myData.thisClimate.damper  <- subset(myData.thisStandard.damper,    A.CZ.city == this.climate)	# March 1, 2013:


		idx.systype <- 0
		# 1a. supply air flow
		# For each main airloop system type, collect the supply air flow rate and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.main,"FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Max.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_cfm
		row.names(a) <- paste(sys.main,"Supply Flow Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# # 1b. supply air flow of datacenter
		# # For each datacenter water-cooled DX system type, collect the supply air flow rate and write those in the summary file      
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.fan[match(paste(sys.DataCN,"SUPPLY FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Max.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_cfm
		# row.names(a) <- paste(sys.DataCN,"Supply Flow Rate [cfm]",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}



		# # 2a. supply fan type: March 1, 2013: revised based on Brian's updated list for extraction
		# # For each main airloop system type, collect the supply fan type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.fan[match(paste(sys.main,"FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.main,"Fan Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# # 2a. supply fan type of data center: March 1, 2013: revised based on Brian's updated list for extraction
		# # For each datacenter water-cooled DX system type, collect the supply fan type and write those in the summary file      
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.fan[match(paste(sys.DataCN,"SUPPLY FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.DataCN,"Fan Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# # 3a. cooling capacity: March 1, 2013: revised based on Brian's updated list for extraction
		# # For each main airloop system type, collect the cooling coil capacity and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.dx[match(paste(sys.main,"COOLC",sep="_"),myData.thisClimate.dx[,"Name"]),"Nominal.Total.Capacity..W.",drop=FALSE]*factor_w_ton
		# row.names(a) <- paste(sys.main,"Cooling Capacity [ton]",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}					

		# # 3b. cooling capacity of data center: March 1, 2013: revised based on Brian's updated list for extraction
		# # For each datacenter water-cooled DX system, collect the cooling coil capacity and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.dx2[match(paste("HEAT PUMP COOLING MODE AIRLOOP",sys.DataCN,sep=" "),myData.thisClimate.dx2[,"name"]),"capacity",drop=FALSE]*factor_w_ton
		# row.names(a) <- paste(sys.DataCN,"Cooling Capacity [ton]",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# # 4a. cooling coil type: March 1, 2013: revised based on Brian's updated list for extraction
		# # For each main airloop system type, collect the cooling coil type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.dx[match(paste(sys.main,"COOLC",sep="_"),myData.thisClimate.dx[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.main,"Cooling Coil Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# # 4b. cooling coil type of data center: March 1, 2013: revised based on Brian's updated list for extraction
		# # For each datacenter water-cooled DX system, collect the cooling coil type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.dx2[match(paste("HEAT PUMP COOLING MODE AIRLOOP",sys.DataCN,sep=" "),myData.thisClimate.dx2[,"name"]),"type",drop=FALSE]
		# row.names(a) <- paste(sys.DataCN,"Cooling Coil Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 2. supply air temperature reset
		# For all systems, collect the supply air temperature reset flag and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(t(myData.thisClimate.parm[,"SAT_reset_switch"]))
		row.names(a) <- "Supply Air Temp. Reset Flag"
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 3. Ventilation reset flag
		# For each main airloop system type, collect the ventilation reset flag and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(t(myData.thisClimate.parm[,sys.CK]))
		row.names(a) <- paste("Ventilation Reset Flag",sys.CK,sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 4a. system OA rate
		# For each main airloop system type, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.oa[match(sys.main,myData.thisClimate.oa[,"systemName"]),"value",drop=FALSE]*factor_m3s_cfm
		row.names(a) <- paste(sys.main,"OA Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 4b. system OA rate of data center
		# For each datacenter water-cooled DX system type, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.oa[match(paste("AIRLOOP",sys.DataCN,sep=" "),myData.thisClimate.oa[,"systemName"]),"value",drop=FALSE]*factor_m3s_cfm
		row.names(a) <- paste(sys.DataCN,"OA Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}					


		# 5a. damper type: March 1, 2013: revised based on Brian's updated list for extraction 
		# For all systems, collect the damper type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.damper[match(sys.damper.main,myData.thisClimate.damper[,"damperSys"]),"damperType",drop=FALSE]
		row.names(a) <- paste(sys.damper.main,"OA Damper [gravity, motorized]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 5b. damper type: March 1, 2013: revised based on Brian's updated list for extraction 
		# For all systems, collect the damper type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.damper[match(sys.damper.DataCN,myData.thisClimate.damper[,"damperSys"]),"damperType",drop=FALSE]
		row.names(a) <- paste(sys.damper.DataCN,"OA Damper [gravity, motorized]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 6. system economizer type
		# For all systems, collect the economizer type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.econ[match(sys.econ,myData.thisClimate.econ[,"econoSys"]),"econoFlag",drop=FALSE]
		row.names(a) <- paste(sys.econ,"(economizer)",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}



		# 7. ERV flag
		# For each main airloop system type, collect the ERV flags and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(t(myData.thisClimate.parm[,paste(sys.erv,"ERV",sep="_")]))
		row.names(a) <- paste(sys.erv,"ERV Flag",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 8. optimum start flag: March 1, 2013: revised based on Brian's updated list for extraction 
		#    For each system type, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(matrix(rep(optimum.start.flag,length(sys.optStart)),length(sys.optStart),1))
		row.names(a) <- paste(sys.optStart,"Optimum Start [yes/no]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 9.  chiller capacity
		# For chiller and cooling tower systems, collect the plant equipment capacity and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.plant[match(sys.chiller,myData.thisClimate.plant[,"Name"]),"Nominal.Capacity..W.",drop=FALSE]*factor_w_ton
		row.names(a) <- paste(sys.chiller,"Capacity [ton]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 10. cooling tower capacity
		# For each datacenter water-cooled DX system type, collect the cooler capacities and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.plant[match(sys.coolingTower,myData.thisClimate.plant[,"Name"]),"Nominal.Capacity..W.",drop=FALSE]*factor_w_ton
		row.names(a) <- paste(sys.coolingTower,"Capacity [ton]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 11. boiler capacity
		# For boilers, collect the plant equipment capacity and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.plant[match(sys.boiler,myData.thisClimate.plant[,"Name"]),"Nominal.Capacity..W.",drop=FALSE]*factor_w_mmbtuh
		row.names(a) <- paste(sys.boiler,"Capacity [MMBTU/h]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}	

		# 12. flowrate of central plant:  chiller water flow rate
		# 13. flowrate of central plant:  hot water flow rate
		# 14. flowrate of central plant:  condensed water flow rate
		# For plant loop systems, collect the pump flow rate and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.pumprate[match(sys.pump.main,myData.thisClimate.pumprate[,"name"]),"Rated.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_galmin
		row.names(a) <- paste(sys.pump.main,"Flow Rate [gal/min]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}					

		# # 13b. chiller water flow rate of data center
		# # For the datacenter plant loop, collect the pump flow rate and write those in the summary file      
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.pumprate[match(sys.pump.DataCN,myData.thisClimate.pumprate[,"name"]),"Rated.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_galmin
		# row.names(a) <- paste("Datacenter",sys.pump.DataCN,"Flow Rate [gal/min]",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}						

		# 16. CoolSys1 Pump Secondary Pressure Setpoint Reset ("PumpCurve_falg" variable from parm csv file							
		idx.systype <- idx.systype + 1
		a <- data.frame(t(myData.thisClimate.parm[,"PumpCurve_flag"]))
		row.names(a) <- paste(sys.pump.secondary,"Pressure setpoint reset [yes,no]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 17a. supply fan power
		# For each main airloop system type, collect the supply fan power and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.main,"FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.main,"Fan Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}	

		# 17b. supply fan power of data center
		# For each datacenter water-cooled DX system type, collect the supply fan power and write those in the summary file      
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.DataCN,"SUPPLY FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.DataCN,"Fan Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}	

		# 18a. pump power
		# For each main plant loop, collect the pump power and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.pump[match(sys.pump.main,myData.thisClimate.pump[,"Name"]),"Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.pump.main,"Pump Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 18b. pump power of data center
		# For the datacenter plant loop, collect the pump power and write those in the summary file      
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.pump[match(sys.pump.DataCN,myData.thisClimate.pump[,"Name"]),"Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste("Datacenter",sys.pump.DataCN,"Pump Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# Then combind those columns for each climate zone
		if (idx.climate == 1)
		{
			myOut.clm <- myOut.sys
		}else{
			myOut.clm <- cbind(myOut.clm,myOut.sys) 
		}
	}

	return(myOut.clm)
}

# -------------------------------------------------------------------------------------------------
# function: output_RetailStandalone
#
# this function is used to organize the output of a give prototype in the desired format and put in a data frame [myOut.clm]
# input:  The prototype name
# output: the organized output for the given prototype in a data frame [myOut.clm].  
#
# The output is prototype specific, so we have one of such a function for each prototype
# -------------------------------------------------------------------------------------------------
output_RetailStandalone <- function(thisPrototype)
{
	# *** climate loop ***
	idx.climate <- 0
	# For each climate zone, collect cooling coil capacity and system OA rates
	for (this.climate in climateZone.array)
	{
		idx.climate <- idx.climate + 1

		# [DATA SUBSET] of current state-standard-climate
		myData.thisClimate.fan      <- subset(myData.thisStandard.fan,     A.CZ.city == this.climate)
		myData.thisClimate.dx       <- subset(myData.thisStandard.dx,      A.CZ.city == this.climate)
		myData.thisClimate.oa       <- subset(myData.thisStandard.oa,      A.CZ.city == this.climate)
		myData.thisClimate.econ     <- subset(myData.thisStandard.econ,    A.CZ.city == this.climate)
		myData.thisClimate.szvav    <- subset(myData.thisStandard.szvav,   A.CZ.city == this.climate)
		myData.thisClimate.parm     <- subset(myData.thisStandard.parm,    A.CZ.city == this.climate)	# March 1, 2013:
		if (length(grep("_YES_",myData.thisClimate.parm[,col.opt.start],ignore.case=TRUE)))		# March 1, 2013:
		{												# March 1, 2013:
			optimum.start.flag <- "yes"								# March 1, 2013:
		}else if (length(grep("_NO_",myData.thisClimate.parm[,col.opt.start],ignore.case=TRUE)))	# March 1, 2013:
		{												# March 1, 2013:
			optimum.start.flag <- "no"								# March 1, 2013:
		}else												# March 1, 2013:
		{												# March 1, 2013:	
			optimum.start.flag <- "NA"								# March 1, 2013:
		}												# March 1, 2013:
		myData.thisClimate.damper  <- subset(myData.thisStandard.damper,    A.CZ.city == this.climate)	# March 1, 2013:


		idx.systype <- 0
		# 1. cooling capacity
		# For each system, collect the cooling capacity and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.dx[match(paste(sys.types,"UNITARY_PACKAGE_COOLCOIL",sep="_"),myData.thisClimate.dx[,"Name"]),"Nominal.Total.Capacity..W.",drop=FALSE] * factor_w_ton
		row.names(a) <- paste(sys.types,"Cooling Capacity [ton]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# # 2. cooling coil type: March 1, 2013: revised based on Brian's updated list for extraction
		# # For each system, collect the cooling coil type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.dx[match(paste(sys.types,"UNITARY_PACKAGE_COOLCOIL",sep="_"),myData.thisClimate.dx[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.types,"Cooling Coil Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 2. supply air flow: March 1, 2013: revised based on Brian's updated list for extraction 
		# For each main airloop system type, collect the supply air flow rate and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.types,"UNITARY_PACKAGE_FAN",sep="_"),myData.thisClimate.fan[,"Name"]),"Max.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_cfm
		row.names(a) <- paste(sys.types,"Supply Flow Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 3. SZ VAV flag
		# For all systems, collect the sz vav flag and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.szvav[match(sys.sz_vav,myData.thisClimate.szvav[,"szvavSys"]),"szvavFlag",drop=FALSE]
		row.names(a) <- paste(sys.sz_vav,"(SZ VAV)",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}							

		# 4. system OA rate
		# For each system, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.oa[match(sys.types,myData.thisClimate.oa[,"systemName"]),"value",drop=FALSE]*factor_m3s_cfm 
		row.names(a) <- paste(sys.types,"OA Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 5. damper type: March 1, 2013: revised based on Brian's updated list for extraction 
		# For all systems, collect the damper type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.damper[match(sys.damper,myData.thisClimate.damper[,"damperSys"]),"damperType",drop=FALSE]
		row.names(a) <- paste(sys.damper,"OA Damper [gravity, motorized]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 6. economizer type
		# For all systems, collect the economizer type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.econ[match(sys.econ,myData.thisClimate.econ[,"econoSys"]),"econoFlag",drop=FALSE]
		row.names(a) <- paste(sys.econ,"(economizer)",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 8. ERV: March 1, 2013: revised based on Brian's updated list for extraction 
		# For each unitary system, collect the ERV flags and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(t(myData.thisClimate.parm[,paste(sys.erv,"_ERV",sep="")]))
		row.names(a) <- paste(sys.erv,"Energy Recovery [yes/no]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 8. optimum start flag: March 1, 2013: revised based on Brian's updated list for extraction 
		#    For each system type, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(matrix(rep(optimum.start.flag,length(sys.optStart)),length(sys.optStart),1))
		row.names(a) <- paste(sys.optStart,"Optimum Start [yes/no]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# # 6. supply fan type: # March 1, 2013: revised based on Brian's updated list for extraction
		# # For each system, collect the supply fan type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.fan[match(paste(sys.types,"UNITARY_PACKAGE_FAN",sep="_"),myData.thisClimate.fan[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.types,"Fan Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 9a. supply fan power: March 1, 2013: revised based on Brian's updated list for extraction 
		# For each system, collect the supply fan power and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(sys.fan1,myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.fan1,"Fan Power [bhp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}	

		# 9b. supply fan power
		# For each system, collect the supply fan power and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.fan2,"UNITARY_PACKAGE_FAN",sep="_"),myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.fan2,"Fan Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}	

		# Then combind those columns for each climate zone
		if (idx.climate == 1)
		{
			myOut.clm <- myOut.sys
		}else{
			myOut.clm <- cbind(myOut.clm,myOut.sys) 
		}
	}

	return(myOut.clm)
}

# -------------------------------------------------------------------------------------------------
# function: output_SchoolPrimary
#
# this function is used to organize the output of a give prototype in the desired format and put in a data frame [myOut.clm]
# input:  The prototype name
# output: the organized output for the given prototype in a data frame [myOut.clm].  
#
# The output is prototype specific, so we have one of such a function for each prototype
# -------------------------------------------------------------------------------------------------
output_SchoolPrimary <- function(thisPrototype)
{

	# *** climate loop ***
	idx.climate <- 0
	# For each climate zone, collect cooling coil capacity and system OA rates
	for (this.climate in climateZone.array)
	{
		idx.climate <- idx.climate + 1

		# [DATA SUBSET] of current state-standard-climate
		myData.thisClimate.fan      <- subset(myData.thisStandard.fan,     A.CZ.city == this.climate)
		myData.thisClimate.dx       <- subset(myData.thisStandard.dx,      A.CZ.city == this.climate)
		myData.thisClimate.oa       <- subset(myData.thisStandard.oa,      A.CZ.city == this.climate)
		myData.thisClimate.parm     <- subset(myData.thisStandard.parm,    A.CZ.city == this.climate)
		myData.thisClimate.plant    <- subset(myData.thisStandard.plant,   A.CZ.city == this.climate)
		myData.thisClimate.pumprate <- subset(myData.thisStandard.pumprate,A.CZ.city == this.climate)
		myData.thisClimate.pump     <- subset(myData.thisStandard.pump,    A.CZ.city == this.climate)
		myData.thisClimate.econ     <- subset(myData.thisStandard.econ,    A.CZ.city == this.climate)
		myData.thisClimate.szvav    <- subset(myData.thisStandard.szvav,   A.CZ.city == this.climate)								
		if (length(grep("_YES_",myData.thisClimate.parm[,col.opt.start],ignore.case=TRUE)))		# March 1, 2013:
		{												# March 1, 2013:
			optimum.start.flag <- "yes"								# March 1, 2013:
		}else if (length(grep("_NO_",myData.thisClimate.parm[,col.opt.start],ignore.case=TRUE)))	# March 1, 2013:
		{												# March 1, 2013:
			optimum.start.flag <- "no"								# March 1, 2013:
		}else												# March 1, 2013:
		{												# March 1, 2013:	
			optimum.start.flag <- "NA"								# March 1, 2013:
		}												# March 1, 2013:
		myData.thisClimate.damper  <- subset(myData.thisStandard.damper,    A.CZ.city == this.climate)	# March 1, 2013:



		idx.systype <- 0

		# 1a. cooling capacity of VAV system
		# For each main airloop system type, collect the cooling capacity and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.dx[match(paste(sys.VAV,"COOLC DXCOIL",sep="_"),myData.thisClimate.dx[,"Name"]),"Nominal.Total.Capacity..W.",drop=FALSE]*factor_w_ton
		row.names(a) <- paste(sys.VAV,"Cooling Capacity [ton]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}					

		# 1b. cooling capacity of unitary system
		# For each unitary system, collect the cooling capacity and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.dx[match(paste(sys.PSZ,"UNITARY_PACKAGE_COOLCOIL",sep="_"),myData.thisClimate.dx[,"Name"]),"Nominal.Total.Capacity..W.",drop=FALSE]*factor_w_ton
		row.names(a) <- paste(sys.PSZ,"Cooling Capacity [ton]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# # 2a. cooling type of VAV system: March 1, 2013: 
		# # For each main airloop system, collect the cooling coil type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.dx[match(paste(sys.VAV,"COOLC DXCOIL",sep="_"),myData.thisClimate.dx[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.VAV,"Cooling Coil Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# # 2b. cooling type of unitary system: March 1, 2013: 
		# # For each unitary system, collect the cooling coil type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.dx[match(paste(sys.PSZ,"UNITARY_PACKAGE_COOLCOIL",sep="_"),myData.thisClimate.dx[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.PSZ,"Cooling Coil Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 2a. supply air flow: March 1, 2013: revised based on Brian's updated list for extraction 
		# For each main airloop system type, collect the supply air flow rate and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.VAV,"FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Max.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_cfm
		row.names(a) <- paste(sys.VAV,"Supply Flow Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 2b. supply air flow: March 1, 2013: revised based on Brian's updated list for extraction 
		# For each main airloop system type, collect the supply air flow rate and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.PSZ,"UNITARY_PACKAGE_FAN",sep="_"),myData.thisClimate.fan[,"Name"]),"Max.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_cfm
		row.names(a) <- paste(sys.PSZ,"Supply Flow Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 3. SZ VAV flag
		# For all systems, collect the sz vav flag and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.szvav[match(sys.sz_vav,myData.thisClimate.szvav[,"szvavSys"]),"szvavFlag",drop=FALSE]
		row.names(a) <- paste(sys.sz_vav,"(SZ VAV)",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 4. supply air temperature reset
		# For all systems, collect the supply air temperature reset flag and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(t(myData.thisClimate.parm[,"SAT_reset_switch"]))
		row.names(a) <- "Supply Air Temp. Reset Flag"
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}						

		# 5. ventilationreset
		# For each main airloop system type, collect the ventilation reset flag and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(t(myData.thisClimate.parm[,sys.CKflag]))
		row.names(a) <- paste("Ventilation Reset Flag",sys.CKflag,sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 6a. system OA flow rate of VAV system
		# For each main airloop system type, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.oa[match(sys.VAV,myData.thisClimate.oa[,"systemName"]),"value",drop=FALSE]*factor_m3s_cfm 
		row.names(a) <- paste(sys.VAV,"OA Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 6b. system OA flow rate of unitary system
		# For each unitary system type, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.oa[match(sys.PSZ,myData.thisClimate.oa[,"systemName"]),"value",drop=FALSE]*factor_m3s_cfm 
		row.names(a) <- paste(sys.PSZ,"OA Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 7. damper type: March 1, 2013: revised based on Brian's updated list for extraction 
		# For all systems, collect the damper type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.damper[match(sys.damper,myData.thisClimate.damper[,"damperSys"]),"damperType",drop=FALSE]
		row.names(a) <- paste(sys.damper,"OA Damper [gravity, motorized]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 8. economizer type
		# For all systems, collect the economizer type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.econ[match(sys.econ,myData.thisClimate.econ[,"econoSys"]),"econoFlag",drop=FALSE]
		row.names(a) <- paste(sys.econ,"(economizer)",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 9. ERV
		# For each unitary system, collect the ERV flags and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(t(myData.thisClimate.parm[,sys.ERVflag]))
		row.names(a) <- paste(sys.ERVflag,"Flag",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 10. Kitchen Exhaust Fan Flow Rate
		# Collect the kitchen exhaust fan flow rate and write those in the summary file      
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match("KITCHEN_ZN_1_FLR_1 EXHAUST FAN",myData.thisClimate.fan[,"Name"]),"Max.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_cfm
		row.names(a) <- paste("Kitchen Exhaust Fan Flow [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 11. optimum start flag: March 1, 2013: revised based on Brian's updated list for extraction 
		#    For each system type, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- data.frame(matrix(rep(optimum.start.flag,length(sys.optStart)),length(sys.optStart),1))
		row.names(a) <- paste(sys.optStart,"Optimum Start [yes/no]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 12. Boiler Capacity
		# For boilers, collect the plant equipment capacity and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.plant[match(paste(sys.plant,"BOILER",sep=" "),myData.thisClimate.plant[,"Name"]),"Nominal.Capacity..W.",drop=FALSE]*factor_w_mmbtuh
		row.names(a) <- paste(sys.plant,"Capacity [MMBTU/h]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 13. Hot Water Pump Flow Rate
		# For plant loop systems, collect the pump flow rate and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.pumprate[match(paste(sys.pump,"PUMP",sep=" "),myData.thisClimate.pumprate[,"name"]),"Rated.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_galmin
		row.names(a) <- paste(sys.pump,"Flow Rate [gal/min]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# # 12a. Supply Fan Type of VAV system: March 1, 2013: 
		# # For each main airloop system, collect the supply fan type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.fan[match(paste(sys.VAV,"FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.VAV,"Fan Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# # 12a. Supply Fan Type of unitary system: March 1, 2013: 
		# # For each unitary system, collect the supply fan type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.fan[match(paste(sys.PSZ,"UNITARY_PACKAGE_FAN",sep="_"),myData.thisClimate.fan[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.PSZ,"Fan Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 14a. Supply Fan Power of VAV system
		# For each main airloop system type, collect the supply fan power and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.VAV,"FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.VAV,"Fan Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 14b. Supply Fan Power of unitary system
		# For unitary systems, collect the supply fan power and write those in the summary file      
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.PSZ,"UNITARY_PACKAGE_FAN",sep="_"),myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.PSZ,"Fan Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 15. pump power
		# Collect the pump power and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.pump[match(paste(sys.pump,"PUMP",sep=" "),myData.thisClimate.pump[,"Name"]),"Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.pump,"Pump Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# Then combind those columns for each climate zone
		if (idx.climate == 1)
		{
			myOut.clm <- myOut.sys
		}else{
			myOut.clm <- cbind(myOut.clm,myOut.sys) 
		}
	}	


	return(myOut.clm)
}

# -------------------------------------------------------------------------------------------------
# function: output_HotelSmall
#
# this function is used to organize the output of a give prototype in the desired format and put in a data frame [myOut.clm]
# input:  The prototype name
# output: the organized output for the given prototype in a data frame [myOut.clm].  
#
# The output is prototype specific, so we have one of such a function for each prototype
# -------------------------------------------------------------------------------------------------
output_HotelSmall <- function(thisPrototype)
{
	# *** climate loop ***
	idx.climate <- 0
	# For each climate zone, collect cooling coil capacity and system OA rates
	for (this.climate in climateZone.array)
	{
		idx.climate <- idx.climate + 1

		# [DATA SUBSET] of current state-standard-climate
		myData.thisClimate.fan      <- subset(myData.thisStandard.fan,     A.CZ.city == this.climate)
		myData.thisClimate.dx       <- subset(myData.thisStandard.dx,      A.CZ.city == this.climate)
		myData.thisClimate.oa       <- subset(myData.thisStandard.oa,      A.CZ.city == this.climate)
		myData.thisClimate.econ     <- subset(myData.thisStandard.econ,    A.CZ.city == this.climate)
		myData.thisClimate.szvav    <- subset(myData.thisStandard.szvav,   A.CZ.city == this.climate)	
		myData.thisClimate.damper   <- subset(myData.thisStandard.damper,  A.CZ.city == this.climate)	# March 1, 2013:

		idx.systype <- 0

		# 1. Cooling Capacity
		# For each system, collect the cooling capacity and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.dx[match(paste(sys.types,"COOLING COIL",sep=" "),myData.thisClimate.dx[,"Name"]),"Nominal.Total.Capacity..W.",drop=FALSE] * factor_w_ton
		row.names(a) <- paste(sys.types,"Cooling Capacity [ton]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# # 2. Cooling Coil Type: March 1, 2013
		# # For each system, collect the cooling coil type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.dx[match(paste(sys.types,"COOLING COIL",sep=" "),myData.thisClimate.dx[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.types,"Cooling Coil Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 
		# 2. supply air flow: March 1, 2013: revised based on Brian's updated list for extraction
		# For each main airloop system type, collect the supply air flow rate and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.types,"SUPPLY FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Max.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_cfm
		row.names(a) <- paste(sys.types,"Supply Flow Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 3. system OA
		# For each system, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.oa[match(sys.types,myData.thisClimate.oa[,"systemName"]),"value",drop=FALSE]*factor_m3s_cfm 
		row.names(a) <- paste(sys.types,"OA Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 
		# 4. damper type: March 1, 2013: revised based on Brian's updated list for extraction
		# For all systems, collect the damper type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.damper[match(sys.damper,myData.thisClimate.damper[,"damperSys"]),"damperType",drop=FALSE]
		row.names(a) <- paste(sys.damper,"OA Damper [gravity, motorized]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 5. economizer type
		# For all systems, collect the economizer type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.econ[match(sys.econ,myData.thisClimate.econ[,"econoSys"]),"econoFlag",drop=FALSE]
		row.names(a) <- paste(sys.econ,"(economizer)",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# # 5. SZ VAV flag: March 1, 2013
		# # For all systems, collect the sz vav flag and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.szvav[match(sys.sz_vav,myData.thisClimate.szvav[,"szvavSys"]),"szvavFlag",drop=FALSE]
		# row.names(a) <- paste(sys.sz_vav,"(SZ VAV)",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# # 6. supply fan type: March 1, 2013
		# # # For each system, collect the supply fan type and write those in the summary file
		# idx.systype <- idx.systype + 1
		# a <- myData.thisClimate.fan[match(paste(sys.types,"SUPPLY FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Type",drop=FALSE]
		# row.names(a) <- paste(sys.types,"Fan Type",sep=" ")
		#     names(a) <- paste(this.climate,this.standard,sep=" ")					
		# if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 7. supply fan power: March 1, 2013
		# For each system, collect the supply fan power and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(paste(sys.fan,"SUPPLY FAN",sep=" "),myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.fan,"Fan Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}	



		# Then combind those columns for each climate zone
		if (idx.climate == 1)
		{
			myOut.clm <- myOut.sys
		}else{
			myOut.clm <- cbind(myOut.clm,myOut.sys) 
		}
	}



	return(myOut.clm)
}

# -------------------------------------------------------------------------------------------------
# function: output_ApartmentMidRise
#
# this function is used to organize the output of a give prototype in the desired format and put in a data frame [myOut.clm]
# input:  The prototype name
# output: the organized output for the given prototype in a data frame [myOut.clm].  
#
# The output is prototype specific, so we have one of such a function for each prototype
# -------------------------------------------------------------------------------------------------
output_ApartmentMidRise <- function(thisPrototype)
{
	# *** climate loop ***
	idx.climate <- 0
	# For each climate zone, collect cooling coil capacity and system OA rates
	for (this.climate in climateZone.array)
	{
		idx.climate <- idx.climate + 1

		# [DATA SUBSET] of current state-standard-climate
		myData.thisClimate.fan      <- subset(myData.thisStandard.fan,     A.CZ.city == this.climate)
		myData.thisClimate.dx       <- subset(myData.thisStandard.dx,      A.CZ.city == this.climate)
		myData.thisClimate.oa       <- subset(myData.thisStandard.oa,      A.CZ.city == this.climate)

		idx.systype <- 0

		# 1. Cooling Capacity
		# For each system, collect the cooling capacity and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.dx[match(sys.coil,myData.thisClimate.dx[,"Name"]),"Nominal.Total.Capacity..W.",drop=FALSE]*factor_w_ton
		row.names(a) <- paste(sys.coil,"Cooling Capacity [ton]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 2. Cooling Coil Type
		# For each system, collect the cooling coil type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.dx[match(sys.coil,myData.thisClimate.dx[,"Name"]),"Type",drop=FALSE]
		row.names(a) <- paste(sys.coil,"Cooling Coil Type",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")					
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 3. system OA rate
		# For each system, collect the system OA rates and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.oa[match(sys.oa,myData.thisClimate.oa[,"systemName"]),"value",drop=FALSE]*factor_m3s_cfm 
		row.names(a) <- paste(sys.oa,"OA Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 4. Supply flow rate
		# For each system, collect the supply flow rate and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(sys.fan,myData.thisClimate.fan[,"Name"]),"Max.Flow.Rate..m3.s.",drop=FALSE]*factor_m3s_cfm 
		row.names(a) <- paste(sys.fan,"Supply Flow Rate [cfm]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}

		# 5. Supply Fan Type
		# For each system, collect the supply fan type and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(sys.fan,myData.thisClimate.fan[,"Name"]),"Type",drop=FALSE]
		row.names(a) <- paste(sys.fan,"Fan Type",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# 6. Supply Fan Power
		# For each system, collect the supply fan power and write those in the summary file
		idx.systype <- idx.systype + 1
		a <- myData.thisClimate.fan[match(sys.fan,myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
		row.names(a) <- paste(sys.fan,"Fan Power [hp]",sep=" ")
		    names(a) <- paste(this.climate,this.standard,sep=" ")
		if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}


		# Then combind those columns for each climate zone
		if (idx.climate == 1)
		{
			myOut.clm <- myOut.sys
		}else{
			myOut.clm <- cbind(myOut.clm,myOut.sys) 
		}
	}	# end of this climate


	return(myOut.clm)
}
# ***********************************************************************************************************
	
	


# ***********************************************************************************************************
#
#                                MAIN
#
#
# set "regular" for verifcation using regular cases and set "state" for state analysis
# we need to run three times here
# (1) this.analysis <- "regular" and this.regular  <- "IECC"	
# (2) this.analysis <- "regular" and this.regular  <- "ASHRAE"	
# (3) this.analysis <- "state"  does not matter what is the value for this.regular
# ***********************************************************************************************************
for (run.idx in c(1,2,3))
{
	# run.idx <- 1				# change this from 1 to 3
	if (run.idx == 1)
	{
		this.analysis <- "regular"	# this.regular == "regular" will need to choose the values for this.analysis
		this.regular  <- "IECC"		# a switch to process "901_2007 vs 901_2010" or "IECC_2009" vs "IECC_2012"
	}else if (run.idx==2){
		this.analysis <- "regular"	# this.regular == "regular" will need to choose the values for this.analysis
		this.regular  <- "ASHRAE"	# a switch to process "901_2007 vs 901_2010" or "IECC_2009" vs "IECC_2012"
	}else if (run.idx==3){
		this.analysis <- "state"	# this.regular will not take affect when this.analysis is "state"
	}
						# For each standard, collect cooling coil capacity and system OA rates
	if (this.analysis == "regular")
	{
		if (this.regular == "ASHRAE")
		{
			list.standards         <- c("ASHRAE30pct_STD2007","ASHRAE30pct_STD2010")
			this.regular.stdString <- c("ASHRAE901_2010_vs_ASHRAE901_2007")
		}else if (this.regular == "IECC")
		{
			list.standards         <- c("IECC_STD2009","IECC_STD2012")
			this.regular.stdString <- c("IECC_2012_vs_IECC_2009")
		}
	}


	# ***********************************************************************************************************

	# February 1, 2013: Handle Columbia District explicitly
	state.name.exp <- c(state.name,"District of Columbia")
	state.abb.exp  <- c(state.abb,"DC")

	# setup start date and time
	start_time <- date();
	Start.time <- Sys.time()
	set.seed(12345, kind = NULL)	# set seed of random number

	library("reshape")
	# close all devices which are currently open
	device.list <- dev.list()
	if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}


	# conversion factors: taken from http://online.unitconverterpro.com/

	factor_w_ton      <- 0.00028434513609		# http://online.unitconverterpro.com/unit-conversion/convert-alpha/convert.php?cat=power
	factor_m3s_cfm    <- 2118.8800032		# http://online.unitconverterpro.com/unit-conversion/convert-alpha/convert.php?cat=flow
	factor_w_mmbtuh   <- 3.4121416331/1000000	# http://online.unitconverterpro.com/unit-conversion/convert-alpha/convert.php?cat=power
	factor_m3s_galmin <- 15850.323141		# http://online.unitconverterpro.com/unit-conversion/convert-alpha/convert.php?cat=flow
	factor_w_hp       <- 0.0013410220896		# http://online.unitconverterpro.com/unit-conversion/convert-alpha/convert.php?cat=power

	# factor_w_ton      <- 0.000284345		# [W] to [Ton]	originally used by Heejin
	# factor_m3s_cfm    <- 2118.88			# [m3/s] to [cfm] originally used by Heejin
	# factor_w_mmbtuh   <- 3.415179/1000000		# from [W] to [MMBTU/h] originally used by Heejin
	# factor_m3s_galmin <- 15850.32			# [m3/s] to [gallon/min] originally used by Heejin
	# factor_w_hp       <- 0.00134102209		# [W] to [hp] originally used by Heejin


	# today's month, day and year in the format of "Thu Jun 16 08:48:36 2011", 5 fields separated by space
	today.month  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
	today.day    <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
	today.year   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]
	today.hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
	today.minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
	today.second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]

	# The paths need to be modified once it is used for the actual state analysis which has different folder	
	if(.Platform$OS.type == "unix") 
	{
		# the folder to test the script
		Path.Script 	<- "/phome/comstd/CEAstate/stateAnal2012/_p.bin"
		Path.Project 	<- "/phome/comstd/CEAstate/stateAnal2012/"
		Path.OA 	<- "/phome/comstd/CEAstate/stateAnal2012/_p.addendumCK/eio_data_extraction"
		Path.OA.mutz 	<- "/phome/comstd/CEAstate/stateAnal2012/_p.addendumCK/calculation_revised"

		# the folder of actual state analysis  	
		# Path.Project 	<- "/phome/comstd/CEAstate/stateAnal2012"
		# Path.OA 	<- "/phome/comstd/CEAstate/stateAnal2012/_p.addendumCK/eio_data_extraction"  	
	}else{
		# the folder to test the script
		Path.Script 	<- "Y:/CEAstate/stateAnal2012/_p.bin"
		Path.Project 	<- "Y:/CEAstate/stateAnal2012/"
		Path.OA 	<- "Y:/CEAstate/stateAnal2012/_p.addendumCK/eio_data_extraction"
		Path.OA.mutz	<- "Y:/CEAstate/stateAnal2012/_p.addendumCK/calculation_revised"

		# the folder of actual state analysis  	
		# Path.Script 	<- "Y:/CEAstate/stateAnal2012/_p.bin"
		# Path.Project 	<- "Y:/CEAstate/stateAnal2012/"
		# Path.OA 	<- "Y:/CEAstate/stateAnal2012/_p.addendumCK/eio_data_extraction"

		# the folder to test the script in the laptop
		# Path.Script 	<- "C:/Yulong_Projects/LinusCluster/CEAstate/stateAnal2012/_p.bin"
		# Path.Project 	<- "C:/Yulong_Projects/LinusCluster/CEAstate/stateAnal2012/"
		# Path.OA 	<- "C:/Yulong_Projects/LinusCluster/CEAstate/stateAnal2012/_p.addendumCK/eio_data_extraction"
	}
	setwd(Path.Project)




	# specify the list of prototypes
	prototypes.array <- c("OfficeSmall","OfficeLarge","RetailStandalone","SchoolPrimary","HotelSmall","ApartmentMidRise");

	# specify the climate city/climate zone mapping
	# CZ.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,           10,       11,       12,      13,         14,          15,      16,      17,        18,       19,          20,          21,      22)
	# CZ.state <- c("FL",     "XX",   "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK",       "AK",    "CA",        "CA",        "TX",    "TX")
	# CZ.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks","Juneau","LosAngeles","Sacramento","Dallas","Austin")
	# CZ.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B", "Zone3C",     "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8",    "Zone8", "Zone3C",    "Zone3C",    "Zone2A","Zone2A")
	CZ.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,            10,      11,       12,      13,         14,         15,        16,     17,         18,      19,          20,          21,      22,      23,      24,        25,          26,          27,          28,       29,         30,        31,        32,                 33,           34,             35,           36,          37,             38,         39,       40,      41,       42,        43,        44,          45,        46,               47,         48,           49,        50,      51,           52,      53,          54,               55,            56,         57,          58,       59,           60,      61,              62)
	CZ.state <- c("FL",    "XX",    "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK",       "AK",    "CA",        "CA",        "TX",    "TX",    "AL",    "GA",      "TX",        "AL",        "AR",        "GA",     "NC",       "OK",      "SC",      "TX",               "UT",         "AR",           "DC",         "DE",        "GA",           "KY",       "NC",     "NJ",    "NY",     "VA",      "CO",      "OK",        "TX",      "CT",             "IA",       "MA",         "NC",      "NE",    "NJ",         "NY",    "RI",        "CO",             "UT",          "IA",       "NY",        "WI",     "CO",         "UT",    "CO",            "WI")
	CZ.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks","Juneau","LosAngeles","Sacramento","Dallas","Austin","Mobile","Savannah","SanAntonio","Birmingham","LittleRock","Atlanta","Charlotte","Oklahoma","Columbia","FortWorthAlliance","SaintGeorge","SpringfieldMO","BaltimoreMD","Wilmington","ChattanoogaTN","Lexington","Raleigh","Newark","NewYork","Richmond","Trinidad","AmarilloTX","Amarillo","HartfordBradley","DesMoines","BostonLogan","ElkinsWV","Omaha", "AllentownPA","Albany","Providence","ColoradoSprings","SaltLakeCity","MasonCity","Binghamton","Madison","EagleCounty","Vernal","GunnisonCounty","DuluthMN")
	CZ.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B","Zone3C",      "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8",    "Zone8", "Zone3C",    "Zone3C",    "Zone2A","Zone2A","Zone2A","Zone2A",  "Zone2B",    "Zone3A",    "Zone3A",    "Zone3A", "Zone3A",   "Zone3A",  "Zone3A",  "Zone3A",           "Zone3B",     "Zone4A",       "Zone4A",     "Zone4A",    "Zone4A",       "Zone4A",   "Zone4A", "Zone4A","Zone4A", "Zone4A",  "Zone4B",  "Zone4B",    "Zone4B",  "Zone5A",         "Zone5A",   "Zone5A",     "Zone5A",  "Zone5A","Zone5A",     "Zone5A","Zone5A",    "Zone5B",         "Zone5B",      "Zone6A",   "Zone6A",    "Zone6A", "Zone6B",     "Zone6B","Zone7",         "Zone7")
	CZ.array <- data.frame(city = CZ.city,zone = CZ.label,state.abb=CZ.state)
	CZ.array[,"identifier"] <- paste(CZ.array[,"zone"],CZ.array[,"state.abb"],sep="_")





	# -----------------------------------------
	CZ.all.idx   <- c( 1,       2,       3,        4,        5,        6,       7,             8,          9,           10,       11,       12,      13,         14,          15,      16,      17)
	CZ.all.state <- c("FL",     "XX",   "TX",     "AZ",     "TN",     "TX",    "CA",          "MD",       "TN",         "OR",    "IL",     "ID",    "XX",       "VT",        "MT",     "MN",   "AK")
	CZ.all.city  <- c("Miami", "Riyadh","Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Vancouver","Burlington","Helena","Duluth","Fairbanks")
	CZ.all.label <- c("Zone1A","Zone1B","Zone2A", "Zone2B", "Zone3A", "Zone3B", "Zone3C",     "Zone4A",   "Zone4B",     "Zone4C","Zone5A", "Zone5B","Zone5C",   "Zone6A",    "Zone6B","Zone7", "Zone8")
	CZ.all.array <- data.frame(city = CZ.all.city,zone = CZ.all.label,state.abb=CZ.all.state)
	CZ.all.array[,"identifier"] <- paste(CZ.all.array[,"zone"],CZ.all.array[,"state.abb"],sep="_")

	# get the state abbreviation
	myState <- data.frame(state.abb = CZ.state,climate.city = CZ.city,climate.zone=CZ.label)




	# specify the path for outputs
	Path.main.OUT       <- paste(Path.Project,"_StateAnal2012_HVAC_Results",                 sep="/")
	Path.general.OUT    <- paste(Path.Project,"_StateAnal2012_HVAC_Results","misc_output",   sep="/")
	Path.stateAux.OUT   <- paste(Path.Project,"_StateAnal2012_HVAC_Results","state_output",  sep="/")
	Path.regularAux.OUT <- paste(Path.Project,"_StateAnal2012_HVAC_Results","regular_output",sep="/")
	Path.orig.OUT       <- paste(Path.Project,"_StateAnal2012_HVAC_Results","summary_inOut", sep="/")
	if (!file.exists(Path.main.OUT))      {print(paste("NOT existing:",Path.main.OUT));      dir.create(Path.main.OUT,      showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.general.OUT))   {print(paste("NOT existing:",Path.general.OUT));   dir.create(Path.general.OUT,   showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.stateAux.OUT))  {print(paste("NOT existing:",Path.stateAux.OUT));  dir.create(Path.stateAux.OUT,  showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.regularAux.OUT)){print(paste("NOT existing:",Path.regularAux.OUT));dir.create(Path.regularAux.OUT,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.orig.OUT))      {print(paste("NOT existing:",Path.orig.OUT));      dir.create(Path.orig.OUT,      showWarnings=TRUE,recursive=TRUE)}

	# OUTPUT files
	if (this.analysis == "state")
	{
		FL.OBJ  <- paste(Path.general.OUT,paste(this.analysis,"_HVAC_extracted.Rdata",sep=""),sep="/")		
		FL.CSV  <- paste(Path.general.OUT,paste(this.analysis,"_HVAC_extracted.csv",  sep=""),sep="/")		
		FL.LOG  <- paste(Path.general.OUT,paste(this.analysis,"_HVAC_extracted.log",  sep=""),sep="/")		
	}else if (this.analysis == "regular")
	{
		FL.OBJ  <- paste(Path.general.OUT,paste(this.analysis,"_",this.regular,"_HVAC_extracted.Rdata",sep=""),sep="/")		
		FL.CSV  <- paste(Path.general.OUT,paste(this.analysis,"_",this.regular,"_HVAC_extracted.csv",  sep=""),sep="/")		
		FL.LOG  <- paste(Path.general.OUT,paste(this.analysis,"_",this.regular,"_HVAC_extracted.log",  sep=""),sep="/")		
	}
	if (file.exists(FL.OBJ)) {print(paste(FL.OBJ, "exist.Delete it!"));file.remove(FL.OBJ)}	
	if (file.exists(FL.CSV)) {print(paste(FL.CSV, "exist.Delete it!"));file.remove(FL.CSV)}	
	if (file.exists(FL.LOG)) {print(paste(FL.LOG, "exist.Delete it!"));file.remove(FL.LOG)}	




	# -----------------------------------------------------------------------------------------------------------
	# Feb 14, 2013:  for prototypes subjected to mutz, the VAV system OA need to be replaced by file from mutz calculation.
	# -----------------------------------------------------------------------------------------------------------
	FL.OA.mutz <- paste(Path.OA.mutz,"assemble_mutz_OA.csv",sep="/")
	myData.OA.mutz <- read.csv(FL.OA.mutz,sep=",",header=TRUE,stringsAsFactors=FALSE)
	myData.OA.mutz[,"A.type"] <- rep("regular",dim(myData.OA.mutz)[1])
	myData.OA.mutz[grep("state",myData.OA.mutz[,"OA.scenario"]),"A.type"] <- "state"
	myData.OA.mutz[,"climate"] <- sub("San_Francisco","SanFrancisco",myData.OA.mutz[,"climate"])
	myData.OA.mutz[,"climate"] <- sub("El_Paso",      "ElPaso",      myData.OA.mutz[,"climate"])
	myData.OA.mutz[,"climate"] <- sub("Los_Angeles",  "LosAngeles",  myData.OA.mutz[,"climate"])

	myData.OA.mutz[,"A.CZ.zone"] <- myData.OA.mutz[,"climate.zone"]
	myData.OA.mutz[,"A.CZ.city"] <- myData.OA.mutz[,"climate"]
	myData.OA.mutz[,"A.CZ.climate"] <- paste(myData.OA.mutz[,"A.CZ.zone"],"(",myData.OA.mutz[,"A.CZ.city"],")",sep="")

	idx.in.myState  <- match(myData.OA.mutz[,"climate"],myState[,"climate.city"])
	myData.OA.mutz[,"A.stateAbb"] <- myState[idx.in.myState,"state.abb"]

	names(myData.OA.mutz) <- sub("OA.scenario","OA_scenario",names(myData.OA.mutz))
	keepSourceFL(Path.OA.mutz,Path.orig.OUT,FL.OA.mutz)
	# -----------------------------------------------------------------------------------------------------------	

	#
	# specify the summary file names to be read in
	filename.DXCoil     <- "summary_DX-Coils.sizing.csv"
	filename.Fan        <- "summary_Fans.sizing.csv"
	filename.Plant      <- "summary_Central-Plant.sizing.csv"
	filename.DataCenter <- "summary_datacenter_output.csv"
	filename.PumpRate   <- "summary_pump_flowrate_output.csv"
	filename.PumpSize   <- "summary_Pumps.sizing.csv"
	filename.econSZvav  <- "summary_flag_output.csv"

	# define an arrays to hold the data frame dynamically generated in the process
	# it is expected to have 12 data frame generated, 2 ("base" and "advn")for each of the number of prototypes (6 for state analysis)
	# the name of the data frame will be something like "[prototype.name]_[base|advn]"
	# assumption:
	# 6 prototype
	# 2 standards 
	# all 50 possible state: February 1, 2013:  It is actually 51 states including Washington DC
	no.bldg      <- length(prototypes.array)
	no.state.abb <- length(state.abb.exp)	
	df.array  <- array(rep(NA,no.bldg*2*no.state.abb),dim=c(6,2,51),dimnames=list(prototypes.array,c("base","advn"),state.abb.exp))

	#
	# starting the extraction
	#
	cat("start the process....................\n");

	# prototypes.array <- c("ApartmentMidRise");	# ,"OfficeLarge","RetailStandalone","SchoolPrimary","HotelSmall")
	# *** prototype loop ***
	idx.prototype <- 0
	for (this.prototype in prototypes.array)
	{
	
		# call "readDataSource" to read data from a variety of sources
		FL.Rdata <- readDataSource(this.prototype)
		load(FL.Rdata)
				
		# ---------------------------------------------------------------------------------------------------
		# This section is for small office cost data extraction
		# ---------------------------------------------------------------------------------------------------
		if(this.prototype == "OfficeSmall")
		{
			idx.prototype <- idx.prototype + 1

			# --------------------------------------------------------------------------------------------
			# subsetting the data for both "state" and "regular" analysis
			# --------------------------------------------------------------------------------------------
			if (this.analysis == "state")
			{               
				# [DATA SUBSET] of state analysis cases
				myOA.state     <- subset(myOA,    A.type == "state")
				myDXCoil.state <- subset(myDXCoil,A.type == "state")
				myFan.state    <- subset(myFan,   A.type == "state")	# March 1, 2013: 
				myParm.state   <- subset(myParm,  A.type == "state")	# March 1, 2013:
				myDamper.state <- subset(myDamper,A.type == "state")	# March 1, 2013: 

				cat(paste("[",this.prototype,"]: Successfully isolate the data for state analysis!\n",sep=""))
			}else if(this.analysis == "regular"){
				# [DATA SUBSET] of state analysis cases
				if (this.regular == "ASHRAE")
				{
					myOA.regular     <- subset(myOA,    A.type == "regular" & (OA_scenario=="ASHRAE901_STD2007_OA2004" | OA_scenario=="ASHRAE901_STD2010_OA2004"))
					myDXCoil.regular <- subset(myDXCoil,A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myFan.regular    <- subset(myFan,   A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))	# March 1, 2013: 
					myParm.regular   <- subset(myParm,  A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))	# March 1, 2013: 
					myDamper.regular <- subset(myDamper,A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))	# March 1, 2013: 
				}else if (this.regular == "IECC")
				{
					myOA.regular     <- subset(myOA,    A.type == "regular" & (OA_scenario=="IECC_STD2009_IMC2009" | OA_scenario=="IECC_STD2012_IMC2012"))
					myDXCoil.regular <- subset(myDXCoil,A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))	
					myFan.regular    <- subset(myFan,   A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))			# March 1, 2013: 
					myParm.regular   <- subset(myParm,  A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))			# March 1, 2013: 
					myDamper.regular <- subset(myDamper,A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))			# March 1, 2013: 
				}

				climateZone.array <- sort(unique(myOA.regular[,"A.CZ.city"]))

				cat(paste("[",this.prototype,"]: Successfully isolate the data for verifying on regular cases!\n",sep=""))
			}


			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------

			# *******************************************************************************************
			# for "state" analysis
			# *******************************************************************************************
			if (this.analysis == "state")
			{
				# get the number of state
				state.abb.array  <- unique(myOA.state[,"A.stateAbb"])
				state.name.array <- state.name.exp[match(state.abb.array,state.abb.exp)]
				no.state         <- length(state.abb.array)

				# *** state loop ***
				for (this.state in state.abb.array)
				{
					# [DATA SUBSET] of current state
					myOA.thisState     <- subset(myOA.state,    A.stateAbb == this.state)
					myDXCoil.thisState <- subset(myDXCoil.state,A.stateAbb == this.state)
					myFan.thisState    <- subset(myFan.state,   A.stateAbb == this.state)				# March 1, 2013:
					myParm.thisState   <- subset(myParm.state,  A.stateAbb == this.state)				# March 1, 2013:
					myDamper.thisState <- subset(myDamper.state,A.stateAbb == this.state)				# March 1, 2013:

					# climate cities within the current state
					cz.OA.thisState     <- sort(unique(myOA.thisState[,"A.CZ.city"]))
					cz.DXCoil.thisState <- sort(unique(myDXCoil.thisState[,"A.CZ.city"]))
					cz.Fan.thisState    <- sort(unique(myFan.thisState[,"A.CZ.city"]))				# March 1, 2013:
					cz.Parm.thisState   <- sort(unique(myParm.thisState[,"A.CZ.city"]))				# March 1, 2013:
					cz.Damper.thisState <- sort(unique(myDamper.thisState[,"A.CZ.city"]))				# March 1, 2013:

					# a quick check on the consistency of climate cities among different sources
					if (setequal(cz.OA.thisState,cz.DXCoil.thisState) & setequal(cz.DXCoil.thisState,cz.Fan.thisState) & setequal(cz.Fan.thisState,cz.Parm.thisState) & setequal(cz.Parm.thisState,cz.Damper.thisState))
					{
						climateZone.array <- cz.OA.thisState
						standards.array   <- unique(myDXCoil.thisState[,"A.standard"])				# the standard name consists of both character and year and in no order
						standards.year    <- sub("(.*[^\\d+])(\\d+)(.*)","\\2",standards.array,perl=TRUE)	# extract the digit year from the standards array
					  names(standards.array)  <- standards.year							# use numeric standard year as the name of the standard
						tmp <- c("base","advn");names(tmp) <- sort(standards.year)				# define a temporal array [tmp] to use sorted standard year mapping "base" and "advn"
					  names(standards.array)  <- (tmp[names(standards.array)])					# re-name the standard with "base" and "advn" notation

						# a quick check on the number of standards
						if (length(standards.array) != 2)
						{
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""))
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
							die;
						}
					}else{
						cat(paste("the list of climate cities in the OA and DX Coil, Fan, Parm, Damper (idf) files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""))
						cat(paste("the list of climate cities in the OA and DX Coil, Fan, Parm, Damper (idf) files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
						die;			
					}

					# *** standard loop ***
					idx.standard <- 0
					# For each standard, collect cooling coil capacity and system OA rates
					for (this.standard in standards.array)
					{
						idx.standard <- idx.standard + 1 

						# the "base" or "advn" lable of this standard
						std.label <- names(standards.array[standards.array == this.standard])

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,this.state,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,this.state] <- df.name				

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.dx     <- subset(myDXCoil.thisState,A.standard == this.standard)
						myData.thisStandard.fan    <- subset(myFan.thisState,   A.standard == this.standard)	# March 1, 2013:
						myData.thisStandard.parm   <- subset(myParm.thisState,  A.standard == this.standard)	# March 1, 2013:
						myData.thisStandard.damper <- subset(myDamper.thisState,A.standard == this.standard)	# March 1, 2013: 


						# get the corresponding OA scenario name
						this.OA.scenario <- mapOAscenario(myData.thisStandard.dx,this.standard)

						# [DATA SUBSET] of current state-standard: (b) extract the OA subset of data belong to this prototype-state-standard combination
						myData.thisStandard.oa <- subset(myOA.thisState,OA_scenario == this.OA.scenario) 

						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.stateAux.OUT,paste(this.analysis,"_",this.state,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}

						# call "output_OfficeSmall" to get output in the desired format
						myOut.clm <- output_OfficeSmall(this.prototype)

						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	
						
						
					}	# end of standard loop
				}		# end of state loop
			}else if (this.analysis == "regular"){
			# *******************************************************************************************
			# for "regular" analysis
			# *******************************************************************************************		
					# *** standard loop ***
					idx.standard <- 0				

					for (this.standard in list.standards)
					{
						idx.standard <- idx.standard + 1 

						if (this.regular == "ASHRAE")
						{
							this.OA.scenario <- paste("ASHRAE901",sub("ASHRAE30pct_","",this.standard),"OA2004",sep="_")
						}else if (this.regular == "IECC")
						{
							this.OA.scenario <- paste(this.standard,paste("IMC",sub("IECC_STD","",this.standard),sep=""),sep="_")
						}

						# the "base" or "advn" lable of this standard
						std.label <- c("base","advn")[idx.standard]

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,1] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.dx     <- subset(myDXCoil.regular,  A.standard == this.standard)
						myData.thisStandard.oa     <- subset(myOA.regular,     OA_scenario == this.OA.scenario) 						
						myData.thisStandard.fan    <- subset(myFan.regular,     A.standard == this.standard)	# March 1, 2013:
						myData.thisStandard.parm   <- subset(myParm.regular,    A.standard == this.standard)	# March 1, 2013:
						myData.thisStandard.damper <- subset(myDamper.regular  ,A.standard == this.standard)	# March 1, 2013: 
						

						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.regularAux.OUT,paste(this.analysis,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}

						# call "output_OfficeSmall" to get output in the desired format
						myOut.clm <- output_OfficeSmall(this.prototype)

						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	

									
					}	# end of standard loop	
			}			# end of the "regular" and "state" condition loop

			# delete all data frame before moving to next prototype
				
			
		# ---------------------------------------------------------------------------------------------------
		# This section is for large office cost data extraction
		# ---------------------------------------------------------------------------------------------------
		}else if (this.prototype == "OfficeLarge"){

			idx.prototype <- idx.prototype + 1



			# --------------------------------------------------------------------------------------------
			# subsetting the data for both "state" and "regular" analysis
			# --------------------------------------------------------------------------------------------
			if (this.analysis == "state")
			{               
				# [DATA SUBSET] of state analysis cases
				myFan.state      <- subset(myFan,     A.type == "state")
				myParm.state     <- subset(myParm,    A.type == "state")
				myOA.state       <- subset(myOA,      A.type == "state")
				myOA.mutz.state  <- subset(myOA.mutz, A.type == "state") 	# February 14, 2013: for replacement of mutz OA
				myDXCoil.state   <- subset(myDXCoil,  A.type == "state")
				myDXCoil2.state  <- subset(myDXCoil2, A.type == "state")
				myPlant.state    <- subset(myPlant,   A.type == "state")
				myPumpRate.state <- subset(myPumpRate,A.type == "state")
				myPump.state     <- subset(myPump,    A.type == "state")
				myEcon.state     <- subset(myEcon,    A.type == "state") 
				myDamper.state   <- subset(myDamper,  A.type == "state")	# March 1, 2013: 


				cat(paste("[",this.prototype,"]: Successfully isolate the data for state analysis!\n",sep=""))
			}else if(this.analysis == "regular"){
				if (this.regular == "ASHRAE")
				{		
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,     A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myParm.regular     <- subset(myParm,    A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myOA.regular       <- subset(myOA,      A.type == "regular" & (OA_scenario=="ASHRAE901_STD2007_OA2004" | OA_scenario=="ASHRAE901_STD2010_OA2004"))
					myOA.mutz.regular  <- subset(myOA.mutz, A.type == "regular" & (OA_scenario=="ASHRAE901_STD2007_OA2004" | OA_scenario=="ASHRAE901_STD2010_OA2004"))	 # February 14, 2013: for replacement of mutz OA
					myDXCoil.regular   <- subset(myDXCoil,  A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myDXCoil2.regular  <- subset(myDXCoil2, A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myPlant.regular    <- subset(myPlant,   A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myPumpRate.regular <- subset(myPumpRate,A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myPump.regular     <- subset(myPump,    A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myEcon.regular     <- subset(myEcon,    A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myDamper.regular   <- subset(myDamper,  A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))		# March 1, 2013: 

				}else if (this.regular == "IECC")
				{
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,     A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myParm.regular     <- subset(myParm,    A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myOA.regular       <- subset(myOA,      A.type == "regular" & (OA_scenario=="IECC_STD2009_IMC2009" | OA_scenario=="IECC_STD2012_IMC2012"))
					myOA.mutz.regular  <- subset(myOA.mutz, A.type == "regular" & (OA_scenario=="IECC_STD2009_IMC2009" | OA_scenario=="IECC_STD2012_IMC2012")) 		# February 14, 2013: for replacement of mutz OA
					myDXCoil.regular   <- subset(myDXCoil,  A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myDXCoil2.regular  <- subset(myDXCoil2, A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myPlant.regular    <- subset(myPlant,   A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myPumpRate.regular <- subset(myPumpRate,A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myPump.regular     <- subset(myPump,    A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myEcon.regular     <- subset(myEcon,    A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myDamper.regular   <- subset(myDamper,  A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))			# March 1, 2013: 										

				}

				climateZone.array <- sort(unique(myOA.regular[,"A.CZ.city"]))

				cat(paste("[",this.prototype,"]: Successfully isolate the data for verifying on regular cases!\n",sep=""))
			}
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------


			# *******************************************************************************************
			# for "state" analysis
			# *******************************************************************************************
			if (this.analysis == "state")
			{

				# get the number of state
				state.abb.array  <- unique(myOA.state[,"A.stateAbb"])
				state.name.array <- state.name.exp[match(state.abb.array,state.abb.exp)]
				no.state         <- length(state.abb.array)

				# *** state loop ***
				for (this.state in state.abb.array)
				{
					cat(paste("........................ processing [",this.state,"] .......................\n",sep=""))

					# [DATA SUBSET] of current state
					myFan.thisState      <- subset(myFan.state,      A.stateAbb == this.state)
					myParm.thisState     <- subset(myParm.state,     A.stateAbb == this.state)
					myOA.thisState       <- subset(myOA.state,       A.stateAbb == this.state)
					myOA.mutz.thisState  <- subset(myOA.mutz.state,  A.stateAbb == this.state) 			# February 14, 2013: for replacement of mutz OA
					myDXCoil.thisState   <- subset(myDXCoil.state,   A.stateAbb == this.state)
					myDXCoil2.thisState  <- subset(myDXCoil2.state,  A.stateAbb == this.state)
					myPlant.thisState    <- subset(myPlant.state,    A.stateAbb == this.state)
					myPumpRate.thisState <- subset(myPumpRate.state, A.stateAbb == this.state)
					myPump.thisState     <- subset(myPump.state,     A.stateAbb == this.state)
					myEcon.thisState     <- subset(myEcon.state,     A.stateAbb == this.state)
					myDamper.thisState   <- subset(myDamper.state,   A.stateAbb == this.state)			# March 1, 2013:

					# climate cities within the current state			
					cz.Fan.thisState      <- sort(unique(myFan.thisState[,"A.CZ.city"]))
					cz.Parm.thisState     <- sort(unique(myParm.thisState[,"A.CZ.city"]))
					cz.OA.thisState       <- sort(unique(myOA.thisState[,"A.CZ.city"]))
					cz.OA.mutz.thisState  <- sort(unique(myOA.mutz.thisState[,"A.CZ.city"])) 			# February 14, 2013: for replacement of mutz OA
					cz.DXCoil.thisState   <- sort(unique(myDXCoil.thisState[,"A.CZ.city"]))
					cz.DXCoil2.thisState  <- sort(unique(myDXCoil2.thisState[,"A.CZ.city"]))
					cz.Plant.thisState    <- sort(unique(myPlant.thisState[,"A.CZ.city"]))
					cz.PumpRate.thisState <- sort(unique(myPumpRate.thisState[,"A.CZ.city"]))
					cz.Pump.thisState     <- sort(unique(myPump.thisState[,"A.CZ.city"]))
					cz.Econ.thisState     <- sort(unique(myEcon.thisState[,"A.CZ.city"]))
					cz.Damper.thisState   <- sort(unique(myDamper.thisState[,"A.CZ.city"]))				# March 1, 2013:


					# a quick check on the consistency of climate cities among different sources
					if (setequal(cz.Fan.thisState,cz.Parm.thisState) & setequal(cz.Parm.thisState,cz.OA.thisState) & setequal(cz.OA.thisState,cz.OA.mutz.thisState) & setequal(cz.OA.mutz.thisState,cz.DXCoil.thisState) & setequal(cz.DXCoil.thisState,cz.DXCoil2.thisState) & setequal(cz.DXCoil2.thisState,cz.Plant.thisState) & setequal(cz.Plant.thisState,cz.PumpRate.thisState) & setequal(cz.PumpRate.thisState,cz.Pump.thisState) & setequal(cz.Pump.thisState,cz.Econ.thisState) & setequal(cz.Econ.thisState,cz.Damper.thisState))
					{
						climateZone.array <- cz.OA.thisState
						standards.array   <- unique(myDXCoil.thisState[,"A.standard"])				# the standard name consists of both character and year and in no order
						standards.year    <- sub("(.*[^\\d+])(\\d+)(.*)","\\2",standards.array,perl=TRUE)	# extract the digit year from the standards array
					  names(standards.array)  <- standards.year							# use numeric standard year as the name of the standard
						tmp <- c("base","advn");names(tmp) <- sort(standards.year)				# define a temporal array [tmp] to use sorted standard year mapping "base" and "advn"
					  names(standards.array)  <- (tmp[names(standards.array)])					# re-name the standard with "base" and "advn" notation


						# a quick check on the number of standards
						if (length(standards.array) != 2)
						{
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""))
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
							die;
						}
					}else{
						cat(paste("the list of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""))
						cat(paste("the list of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
						die;			
					}


					# *** standard loop ***
					idx.standard <- 0
					# For each standard, collect data
					for (this.standard in standards.array)
					{
						idx.standard <- idx.standard + 1 

						# the "base" or "advn" lable of this standard
						std.label <- names(standards.array[standards.array == this.standard])

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,this.state,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,this.state] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.thisState,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.thisState,  A.standard == this.standard)
						myData.thisStandard.dx2      <- subset(myDXCoil2.thisState, A.standard == this.standard)
						myData.thisStandard.parm     <- subset(myParm.thisState,    A.standard == this.standard)
						myData.thisStandard.plant    <- subset(myPlant.thisState,   A.standard == this.standard)
						myData.thisStandard.pumprate <- subset(myPumpRate.thisState,A.standard == this.standard) 
						myData.thisStandard.pump     <- subset(myPump.thisState,    A.standard == this.standard)  
						myData.thisStandard.econ     <- subset(myEcon.thisState,    A.standard == this.standard)  
						myData.thisStandard.damper   <- subset(myDamper.thisState,  A.standard == this.standard)	# March 1, 2013: 

						# get the corresponding OA scenario name
						this.OA.scenario <- mapOAscenario(myData.thisStandard.dx,this.standard)

						# [DATA SUBSET] of current state-standard: (b) extract the OA subset of data belong to this prototype-state-standard combination
						myData.thisStandard.oa      <- subset(myOA.thisState,     OA_scenario == this.OA.scenario) 
						myData.thisStandard.oa.mutz <- subset(myOA.mutz.thisState,OA_scenario == this.OA.scenario)  	# February 14, 2013: for replacement of mutz OA

						# -------------------------------------------------------------------
						# replace the system OA of VAV system with those calculated from mutz			 	# February 14, 2013: for replacement of mutz OA
						# -------------------------------------------------------------------
						for (i in seq(from=1,to = dim(myData.thisStandard.oa)[1],by=1))
						{
							# cat(paste(i,"\n",sep=""))

							if (length(grep("VAV",myData.thisStandard.oa[i,"systemName"],ignore.case = FALSE)) | length(grep("AHU-2",myData.thisStandard.oa[i,"systemName"],ignore.case = FALSE)))	# note: "VAV" used for all prototypes except "OutPatientHealthCare which used "AHU-2"
							{
								tmp.OA.old <- myData.thisStandard.oa[i,"value"] 
								tmp.sys    <- myData.thisStandard.oa[i,"systemName"]
								tmp.city   <- myData.thisStandard.oa[i,"A.CZ.climate"]
								tmp.OAsc   <- myData.thisStandard.oa[i,"OA_scenario"]
								tmp.bldg   <- myData.thisStandard.oa[i,"prototype"]
								tmp.state  <- myData.thisStandard.oa[i,"A.stateAbb"]
								tmp.OA.new <- myData.thisStandard.oa.mutz[myData.thisStandard.oa.mutz[,"sys"]          == tmp.sys   & 
													  myData.thisStandard.oa.mutz[,"prototype"]    == tmp.bldg  & 
													  myData.thisStandard.oa.mutz[,"OA_scenario"]  == tmp.OAsc  & 
													  myData.thisStandard.oa.mutz[,"A.stateAbb"]   == tmp.state & 
													  myData.thisStandard.oa.mutz[,"A.CZ.climate"] == tmp.city  ,"Vot"]

								myData.thisStandard.oa[i,"value"] <- tmp.OA.new	

								# 
								# cat(paste("\t\tOA of [",tmp.sys,"] [",tmp.city,"] [",tmp.OAsc,"] [",tmp.bldg,"] [",tmp.state,"] has been replaced from [",tmp.OA.old,"] to [",tmp.OA.new,"]\n",sep=""))
								  cat(paste("\t\tOA of [",tmp.sys,"] [",tmp.city,"] [",tmp.OAsc,"] [",tmp.bldg,"] [",tmp.state,"] has been replaced from [",tmp.OA.old,"] to [",tmp.OA.new,"]\n",sep=""),file=FL.LOG,append=TRUE)
							}
						}
						# -------------------------------------------------------------------




						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.stateAux.OUT,paste(this.analysis,"_",this.state,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}

						# call "output_OfficeLarge" to get output in the desired format
						myOut.clm <- output_OfficeLarge(this.prototype)



						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	
					}	# end of standard loop
				}		# end of state loop
			}else if (this.analysis == "regular"){
			# *******************************************************************************************
			# for "regular" analysis
			# *******************************************************************************************		
					# *** standard loop ***
					idx.standard <- 0

					for (this.standard in list.standards)
					{
						idx.standard <- idx.standard + 1 

						if (this.regular == "ASHRAE")
						{
							this.OA.scenario <- paste("ASHRAE901",sub("ASHRAE30pct_","",this.standard),"OA2004",sep="_")
						}else if (this.regular == "IECC")
						{
							this.OA.scenario <- paste(this.standard,paste("IMC",sub("IECC_STD","",this.standard),sep=""),sep="_")
						}


						# the "base" or "advn" lable of this standard
						std.label <- c("base","advn")[idx.standard]

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,1] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.regular,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.regular,  A.standard == this.standard)
						myData.thisStandard.dx2      <- subset(myDXCoil2.regular, A.standard == this.standard)
						myData.thisStandard.parm     <- subset(myParm.regular,    A.standard == this.standard)
						myData.thisStandard.plant    <- subset(myPlant.regular,   A.standard == this.standard)
						myData.thisStandard.pumprate <- subset(myPumpRate.regular,A.standard == this.standard) 
						myData.thisStandard.pump     <- subset(myPump.regular,    A.standard == this.standard)  
						myData.thisStandard.econ     <- subset(myEcon.regular,    A.standard == this.standard) 
						myData.thisStandard.damper   <- subset(myDamper.regular,  A.standard == this.standard)		# March 1, 2013: 
						myData.thisStandard.oa       <- subset(myOA.regular,     OA_scenario == this.OA.scenario) 
						myData.thisStandard.oa.mutz  <- subset(myOA.mutz.regular,OA_scenario == this.OA.scenario)	# February 14, 2013: for replacement of mutz OA 

						# -------------------------------------------------------------------
						# replace the system OA of VAV system with those calculated from mutz			 	# February 14, 2013: for replacement of mutz OA
						# -------------------------------------------------------------------
						for (i in seq(from=1,to = dim(myData.thisStandard.oa)[1],by=1))
						{
							# cat(paste(i,"\n",sep=""))

							if (length(grep("VAV",myData.thisStandard.oa[i,"systemName"],ignore.case = FALSE)) | length(grep("AHU-2",myData.thisStandard.oa[i,"systemName"],ignore.case = FALSE)))	# note: "VAV" used for all prototypes except "OutPatientHealthCare which used "AHU-2"
							{
								tmp.OA.old <- myData.thisStandard.oa[i,"value"] 
								tmp.sys    <- myData.thisStandard.oa[i,"systemName"]
								tmp.city   <- myData.thisStandard.oa[i,"A.CZ.climate"]
								tmp.OAsc   <- myData.thisStandard.oa[i,"OA_scenario"]
								tmp.bldg   <- myData.thisStandard.oa[i,"prototype"]
								tmp.state  <- myData.thisStandard.oa[i,"A.stateAbb"]
								tmp.OA.new <- myData.thisStandard.oa.mutz[myData.thisStandard.oa.mutz[,"sys"]          == tmp.sys   & 
													  myData.thisStandard.oa.mutz[,"prototype"]    == tmp.bldg  & 
													  myData.thisStandard.oa.mutz[,"OA_scenario"]  == tmp.OAsc  & 
													  myData.thisStandard.oa.mutz[,"A.stateAbb"]   == tmp.state & 
													  myData.thisStandard.oa.mutz[,"A.CZ.climate"] == tmp.city  ,"Vot"]

								myData.thisStandard.oa[i,"value"] <- tmp.OA.new	

								# 
								# cat(paste("\t\tOA of [",tmp.sys,"] [",tmp.city,"] [",tmp.OAsc,"] [",tmp.bldg,"] [",tmp.state,"] has been replaced from [",tmp.OA.old,"] to [",tmp.OA.new,"]\n",sep=""))
								  cat(paste("\t\tOA of [",tmp.sys,"] [",tmp.city,"] [",tmp.OAsc,"] [",tmp.bldg,"] [",tmp.state,"] has been replaced from [",tmp.OA.old,"] to [",tmp.OA.new,"]\n",sep=""),file=FL.LOG,append=TRUE)
							}
						}
						# -------------------------------------------------------------------



						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.regularAux.OUT,paste(this.analysis,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}

						# call "output_OfficeLarge" to get output in the desired format
						myOut.clm <- output_OfficeLarge(this.prototype)


						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	
					}	# end of standard loop					
			}			# end of the "regular" and "state" condition loop				

		# ---------------------------------------------------------------------------------------------------
		# This section is for Standalone Retail cost data extraction
		# ---------------------------------------------------------------------------------------------------
		}else if (this.prototype == "RetailStandalone"){

			idx.prototype <- idx.prototype + 1


			# --------------------------------------------------------------------------------------------
			# subsetting the data for both "state" and "regular" analysis
			# --------------------------------------------------------------------------------------------
			if (this.analysis == "state")
			{ 
				# [DATA SUBSET] of state analysis cases
				myFan.state      <- subset(myFan,     A.type == "state")
				myOA.state       <- subset(myOA,      A.type == "state")
				myDXCoil.state   <- subset(myDXCoil,  A.type == "state")
				myEcon.state     <- subset(myEcon,    A.type == "state") 
				mySZvav.state    <- subset(mySZvav,   A.type == "state") 
				myDamper.state   <- subset(myDamper,  A.type == "state")	# March 1, 2013: 
				myParm.state     <- subset(myParm,    A.type == "state")	# March 1, 2013:
				


				cat(paste("[",this.prototype,"]: Successfully isolate the data for state analysis!\n",sep=""))
			}else if(this.analysis == "regular"){
				if (this.regular == "ASHRAE")
				{		
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,      A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myOA.regular       <- subset(myOA,       A.type == "regular" & (OA_scenario=="ASHRAE901_STD2007_OA2004" | OA_scenario=="ASHRAE901_STD2010_OA2004"))
					myDXCoil.regular   <- subset(myDXCoil,   A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myEcon.regular     <- subset(myEcon,     A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010")) 
					mySZvav.regular    <- subset(mySZvav,    A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010")) 
					myDamper.regular   <- subset(myDamper,   A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))		# March 1, 2013: 
					myParm.regular     <- subset(myParm,     A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))		# March 1, 2013: 									
				}else if (this.regular == "IECC")
				{
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,      A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myOA.regular       <- subset(myOA,       A.type == "regular" & (OA_scenario=="IECC_STD2009_IMC2009" | OA_scenario=="IECC_STD2012_IMC2012"))
					myDXCoil.regular   <- subset(myDXCoil,   A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myEcon.regular     <- subset(myEcon,     A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012")) 
					mySZvav.regular    <- subset(mySZvav,    A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012")) 
					myDamper.regular   <- subset(myDamper,   A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))			# March 1, 2013: 
					myParm.regular     <- subset(myParm,     A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))			# March 1, 2013: 										
				}			

				climateZone.array <- sort(unique(myOA.regular[,"A.CZ.city"]))

				cat(paste("[",this.prototype,"]: Successfully isolate the data for verifying on regular cases!\n",sep=""))
			}
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------


			# *******************************************************************************************
			# for "state" analysis
			# *******************************************************************************************
			if (this.analysis == "state")
			{
				# get the number of state
				state.abb.array  <- unique(myOA.state[,"A.stateAbb"])
				state.name.array <- state.name.exp[match(state.abb.array,state.abb.exp)]
				no.state         <- length(state.abb.array)

				# *** state loop ***
				for (this.state in state.abb.array)
				{
					# [DATA SUBSET] of current state
					myFan.thisState      <- subset(myFan.state,      A.stateAbb == this.state)
					myOA.thisState       <- subset(myOA.state,       A.stateAbb == this.state)
					myDXCoil.thisState   <- subset(myDXCoil.state,   A.stateAbb == this.state)
					myEcon.thisState     <- subset(myEcon.state,     A.stateAbb == this.state)
					mySZvav.thisState    <- subset(mySZvav.state,    A.stateAbb == this.state)
					myDamper.thisState   <- subset(myDamper.state,   A.stateAbb == this.state)			# March 1, 2013:
					myParm.thisState     <- subset(myParm.state,     A.stateAbb == this.state)			# March 1, 2013:

					# climate cities within the current state			
					cz.Fan.thisState      <- sort(unique(myFan.thisState[,"A.CZ.city"]))
					cz.OA.thisState       <- sort(unique(myOA.thisState[,"A.CZ.city"]))
					cz.DXCoil.thisState   <- sort(unique(myDXCoil.thisState[,"A.CZ.city"]))			
					cz.Econ.thisState     <- sort(unique(myEcon.thisState[,"A.CZ.city"]))
					cz.SZvav.thisState    <- sort(unique(mySZvav.thisState[,"A.CZ.city"]))					
					cz.Damper.thisState   <- sort(unique(myDamper.thisState[,"A.CZ.city"]))				# March 1, 2013:
					cz.Parm.thisState     <- sort(unique(myParm.thisState[,"A.CZ.city"]))				# March 1, 2013:
					

					# a quick check on the consistency of climate cities among different sources
					if (setequal(cz.Fan.thisState,cz.OA.thisState) & setequal(cz.OA.thisState,cz.DXCoil.thisState) & setequal(cz.DXCoil.thisState,cz.Econ.thisState) & setequal(cz.Econ.thisState,cz.SZvav.thisState) & setequal(cz.SZvav.thisState,cz.Damper.thisState) & setequal(cz.Damper.thisState,cz.Parm.thisState))
					{
						climateZone.array <- cz.OA.thisState
						standards.array   <- unique(myDXCoil.thisState[,"A.standard"])				# the standard name consists of both character and year and in no order
						standards.year    <- sub("(.*[^\\d+])(\\d+)(.*)","\\2",standards.array,perl=TRUE)	# extract the digit year from the standards array
					  names(standards.array)  <- standards.year							# use numeric standard year as the name of the standard
						tmp <- c("base","advn");names(tmp) <- sort(standards.year)				# define a temporal array [tmp] to use sorted standard year mapping "base" and "advn"
					  names(standards.array)  <- (tmp[names(standards.array)])					# re-name the standard with "base" and "advn" notation

						# a quick check on the number of standards
						if (length(standards.array) != 2)
						{
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""))
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
							die;
						}
					}else{
						cat(paste("the lis of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""))
						cat(paste("the lis of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
						die;			
					}


					# *** standard loop ***
					idx.standard <- 0
					# For each standard, collect data
					for (this.standard in standards.array)
					{
						idx.standard <- idx.standard + 1 

						# the "base" or "advn" lable of this standard
						std.label <- names(standards.array[standards.array == this.standard])

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,this.state,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,this.state] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.thisState,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.thisState,  A.standard == this.standard)
						myData.thisStandard.econ     <- subset(myEcon.thisState,    A.standard == this.standard) 
						myData.thisStandard.szvav    <- subset(mySZvav.thisState,   A.standard == this.standard) 
						myData.thisStandard.damper   <- subset(myDamper.thisState,  A.standard == this.standard)	# March 1, 2013: 
						myData.thisStandard.parm     <- subset(myParm.thisState,    A.standard == this.standard)	# March 1, 2013:
						
						

						# get the corresponding OA scenario name
						this.OA.scenario <- mapOAscenario(myData.thisStandard.dx,this.standard)

						# [DATA SUBSET] of current state-standard: (b) extract the OA subset of data belong to this prototype-state-standard combination
						myData.thisStandard.oa <- subset(myOA.thisState,OA_scenario == this.OA.scenario) 


						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.stateAux.OUT,paste(this.analysis,"_",this.state,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}

						# call "output_RetailStandalone" to get output in the desired format
						myOut.clm <- output_RetailStandalone(this.prototype)

						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	
					}	# end of standard loop
				}		# end of state loop
			}else if (this.analysis == "regular"){
			# *******************************************************************************************
			# for "regular" analysis
			# *******************************************************************************************		
					# *** standard loop ***
					idx.standard <- 0

					for (this.standard in list.standards)				
					{							
						idx.standard <- idx.standard + 1 

						if (this.regular == "ASHRAE")
						{
							this.OA.scenario <- paste("ASHRAE901",sub("ASHRAE30pct_","",this.standard),"OA2004",sep="_")
						}else if (this.regular == "IECC")
						{
							this.OA.scenario <- paste(this.standard,paste("IMC",sub("IECC_STD","",this.standard),sep=""),sep="_")
						}




						# the "base" or "advn" lable of this standard
						std.label <- c("base","advn")[idx.standard]

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,1] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.regular,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.regular,  A.standard == this.standard)
						myData.thisStandard.econ     <- subset(myEcon.regular,    A.standard == this.standard) 
						myData.thisStandard.szvav    <- subset(mySZvav.regular,   A.standard == this.standard)
						myData.thisStandard.damper   <- subset(myDamper.regular,  A.standard == this.standard)	# March 1, 2013: 
						myData.thisStandard.parm     <- subset(myParm.regular,    A.standard == this.standard)	# March 1, 2013:
						myData.thisStandard.oa       <- subset(myOA.regular,     OA_scenario == this.OA.scenario) 

						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.regularAux.OUT,paste(this.analysis,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}


						# call "output_RetailStandalone" to get output in the desired format
						myOut.clm <- output_RetailStandalone(this.prototype)
						
						
						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	
					}	# end of standard loop
			}			# end of the "regular" and "state" condition loop					


		# ---------------------------------------------------------------------------------------------------
		# This section is for Primary School cost data extraction
		# ---------------------------------------------------------------------------------------------------
		}else if (this.prototype == "SchoolPrimary"){

			idx.prototype <- idx.prototype + 1

   


			# --------------------------------------------------------------------------------------------
			# subsetting the data for both "state" and "regular" analysis
			# --------------------------------------------------------------------------------------------
			if (this.analysis == "state")
			{   		
				# [DATA SUBSET] of state analysis cases
				myFan.state      <- subset(myFan,     A.type == "state")
				myParm.state     <- subset(myParm,    A.type == "state")
				myOA.state       <- subset(myOA,      A.type == "state")
				myOA.mutz.state  <- subset(myOA.mutz, A.type == "state") 	# February 14, 2013: for replacement of mutz OA
				myDXCoil.state   <- subset(myDXCoil,  A.type == "state")
				myPlant.state    <- subset(myPlant,   A.type == "state")
				myPumpRate.state <- subset(myPumpRate,A.type == "state")
				myPump.state     <- subset(myPump,    A.type == "state")
				myEcon.state     <- subset(myEcon,    A.type == "state") 
				mySZvav.state    <- subset(mySZvav,   A.type == "state") 
				myDamper.state   <- subset(myDamper,  A.type == "state")	# March 1, 2013: 
				cat(paste("[",this.prototype,"]: Successfully isolate the data for state analysis!\n",sep=""))
			}else if(this.analysis == "regular"){
				if (this.regular == "ASHRAE")
				{		
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,     A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myParm.regular     <- subset(myParm,    A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myOA.regular       <- subset(myOA,      A.type == "regular" & (OA_scenario=="ASHRAE901_STD2007_OA2004" | OA_scenario=="ASHRAE901_STD2010_OA2004"))
					myOA.mutz.regular  <- subset(myOA.mutz, A.type == "regular" & (OA_scenario=="ASHRAE901_STD2007_OA2004" | OA_scenario=="ASHRAE901_STD2010_OA2004"))	 # February 14, 2013: for replacement of mutz OA
					myDXCoil.regular   <- subset(myDXCoil,  A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myPlant.regular    <- subset(myPlant,   A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myPumpRate.regular <- subset(myPumpRate,A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myPump.regular     <- subset(myPump,    A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))
					myEcon.regular     <- subset(myEcon,    A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010")) 
					mySZvav.regular    <- subset(mySZvav,   A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010")) 				
					myDamper.regular   <- subset(myDamper,  A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))		# March 1, 2013: 
				}else if (this.regular == "IECC")
				{
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,     A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myParm.regular     <- subset(myParm,    A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myOA.regular       <- subset(myOA,      A.type == "regular" & (OA_scenario=="IECC_STD2009_IMC2009" | OA_scenario=="IECC_STD2012_IMC2012"))
					myOA.mutz.regular  <- subset(myOA.mutz, A.type == "regular" & (OA_scenario=="IECC_STD2009_IMC2009" | OA_scenario=="IECC_STD2012_IMC2012")) 		# February 14, 2013: for replacement of mutz OA
					myDXCoil.regular   <- subset(myDXCoil,  A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myPlant.regular    <- subset(myPlant,   A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myPumpRate.regular <- subset(myPumpRate,A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myPump.regular     <- subset(myPump,    A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))
					myEcon.regular     <- subset(myEcon,    A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012")) 
					mySZvav.regular    <- subset(mySZvav,   A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012")) 
					myDamper.regular   <- subset(myDamper,   A.type == "regular" & (A.standard =="IECC_STD2009"        | A.standard =="IECC_STD2012"))			# March 1, 2013: 
				}
				climateZone.array <- sort(unique(myOA.regular[,"A.CZ.city"]))

				cat(paste("[",this.prototype,"]: Successfully isolate the data for verifying on regular cases!\n",sep=""))
			}
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------				


			# *******************************************************************************************
			# for "state" analysis
			# *******************************************************************************************
			if (this.analysis == "state")
			{

				# get the number of state
				state.abb.array  <- unique(myOA.state[,"A.stateAbb"])
				state.name.array <- state.name.exp[match(state.abb.array,state.abb.exp)]
				no.state         <- length(state.abb.array)

				# *** state loop ***
				for (this.state in state.abb.array)
				{
					cat(paste("........................ processing [",this.state,"] .......................\n",sep=""))

					# [DATA SUBSET] of current state
					myFan.thisState      <- subset(myFan.state,      A.stateAbb == this.state)
					myParm.thisState     <- subset(myParm.state,     A.stateAbb == this.state)
					myOA.thisState       <- subset(myOA.state,       A.stateAbb == this.state)
					myOA.mutz.thisState  <- subset(myOA.mutz.state,  A.stateAbb == this.state) 			# February 14, 2013: for replacement of mutz OA
					myDXCoil.thisState   <- subset(myDXCoil.state,   A.stateAbb == this.state)
					myPlant.thisState    <- subset(myPlant.state,    A.stateAbb == this.state)
					myPumpRate.thisState <- subset(myPumpRate.state, A.stateAbb == this.state)
					myPump.thisState     <- subset(myPump.state,     A.stateAbb == this.state)
					myEcon.thisState     <- subset(myEcon.state,     A.stateAbb == this.state)
					mySZvav.thisState    <- subset(mySZvav.state,    A.stateAbb == this.state)
					myDamper.thisState   <- subset(myDamper.state,   A.stateAbb == this.state)			# March 1, 2013:

					# climate cities within the current state			
					cz.Fan.thisState      <- sort(unique(myFan.thisState[,"A.CZ.city"]))
					cz.Parm.thisState     <- sort(unique(myParm.thisState[,"A.CZ.city"]))
					cz.OA.thisState       <- sort(unique(myOA.thisState[,"A.CZ.city"]))
					cz.OA.mutz.thisState  <- sort(unique(myOA.mutz.thisState[,"A.CZ.city"])) 			# February 14, 2013: for replacement of mutz OA
					cz.DXCoil.thisState   <- sort(unique(myDXCoil.thisState[,"A.CZ.city"]))
					cz.Plant.thisState    <- sort(unique(myPlant.thisState[,"A.CZ.city"]))
					cz.PumpRate.thisState <- sort(unique(myPumpRate.thisState[,"A.CZ.city"]))
					cz.Pump.thisState     <- sort(unique(myPump.thisState[,"A.CZ.city"]))	
					cz.Econ.thisState     <- sort(unique(myEcon.thisState[,"A.CZ.city"]))
					cz.SZvav.thisState    <- sort(unique(mySZvav.thisState[,"A.CZ.city"]))
					cz.Damper.thisState   <- sort(unique(myDamper.thisState[,"A.CZ.city"]))				# March 1, 2013:


					# a quick check on the consistency of climate cities among different sources
					if (setequal(cz.Fan.thisState,cz.Parm.thisState) & setequal(cz.Parm.thisState,cz.OA.thisState) & setequal(cz.OA.thisState,cz.DXCoil.thisState) & setequal(cz.DXCoil.thisState,cz.Plant.thisState) & setequal(cz.Plant.thisState,cz.PumpRate.thisState) & setequal(cz.PumpRate.thisState,cz.Pump.thisState) & setequal(cz.Pump.thisState,cz.Econ.thisState) & setequal(cz.Econ.thisState,cz.SZvav.thisState) & setequal(cz.SZvav.thisState,cz.Damper.thisState))
					{
						climateZone.array <- cz.OA.thisState
						standards.array   <- unique(myDXCoil.thisState[,"A.standard"])				# the standard name consists of both character and year and in no order
						standards.year    <- sub("(.*[^\\d+])(\\d+)(.*)","\\2",standards.array,perl=TRUE)	# extract the digit year from the standards array
					  names(standards.array)  <- standards.year							# use numeric standard year as the name of the standard
						tmp <- c("base","advn");names(tmp) <- sort(standards.year)				# define a temporal array [tmp] to use sorted standard year mapping "base" and "advn"
					  names(standards.array)  <- (tmp[names(standards.array)])					# re-name the standard with "base" and "advn" notation

						# a quick check on the number of standards
						if (length(standards.array) != 2)
						{
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""))
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
							die;
						}
					}else{
						cat(paste("the lis of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""))
						cat(paste("the lis of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
						die;			
					}


					# *** standard loop ***
					idx.standard <- 0
					# For each standard, collect data
					for (this.standard in standards.array)
					{
						idx.standard <- idx.standard + 1 

						# the "base" or "advn" lable of this standard
						std.label <- names(standards.array[standards.array == this.standard])

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,this.state,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,this.state] <- df.name


						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.thisState,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.thisState,  A.standard == this.standard)
						myData.thisStandard.parm     <- subset(myParm.thisState,    A.standard == this.standard)
						myData.thisStandard.plant    <- subset(myPlant.thisState,   A.standard == this.standard)
						myData.thisStandard.pumprate <- subset(myPumpRate.thisState,A.standard == this.standard) 
						myData.thisStandard.pump     <- subset(myPump.thisState,    A.standard == this.standard)  
						myData.thisStandard.econ     <- subset(myEcon.thisState,    A.standard == this.standard) 
						myData.thisStandard.szvav    <- subset(mySZvav.thisState,   A.standard == this.standard)
						myData.thisStandard.damper   <- subset(myDamper.thisState,  A.standard == this.standard)	# March 1, 2013: 

						# get the corresponding OA scenario name
						this.OA.scenario <- mapOAscenario(myData.thisStandard.dx,this.standard)


						# [DATA SUBSET] of current state-standard: (b) extract the OA subset of data belong to this prototype-state-standard combination
						myData.thisStandard.oa      <- subset(myOA.thisState,     OA_scenario == this.OA.scenario) 
						myData.thisStandard.oa.mutz <- subset(myOA.mutz.thisState,OA_scenario == this.OA.scenario)  	# February 14, 2013: for replacement of mutz OA

						# -------------------------------------------------------------------
						# replace the system OA of VAV system with those calculated from mutz			 	# February 14, 2013: for replacement of mutz OA
						# -------------------------------------------------------------------
						for (i in seq(from=1,to = dim(myData.thisStandard.oa)[1],by=1))
						{
							# cat(paste(i,"\n",sep=""))

							if (length(grep("VAV",myData.thisStandard.oa[i,"systemName"],ignore.case = FALSE)) | length(grep("AHU-2",myData.thisStandard.oa[i,"systemName"],ignore.case = FALSE)))	# note: "VAV" used for all prototypes except "OutPatientHealthCare which used "AHU-2"
							{
								tmp.OA.old <- myData.thisStandard.oa[i,"value"] 
								tmp.sys    <- myData.thisStandard.oa[i,"systemName"]
								tmp.city   <- myData.thisStandard.oa[i,"A.CZ.climate"]
								tmp.OAsc   <- myData.thisStandard.oa[i,"OA_scenario"]
								tmp.bldg   <- myData.thisStandard.oa[i,"prototype"]
								tmp.state  <- myData.thisStandard.oa[i,"A.stateAbb"]
								tmp.OA.new <- myData.thisStandard.oa.mutz[myData.thisStandard.oa.mutz[,"sys"]          == tmp.sys   & 
													  myData.thisStandard.oa.mutz[,"prototype"]    == tmp.bldg  & 
													  myData.thisStandard.oa.mutz[,"OA_scenario"]  == tmp.OAsc  & 
													  myData.thisStandard.oa.mutz[,"A.stateAbb"]   == tmp.state & 
													  myData.thisStandard.oa.mutz[,"A.CZ.climate"] == tmp.city  ,"Vot"]

								myData.thisStandard.oa[i,"value"] <- tmp.OA.new	

								# 
								# cat(paste("\t\tOA of [",tmp.sys,"] [",tmp.city,"] [",tmp.OAsc,"] [",tmp.bldg,"] [",tmp.state,"] has been replaced from [",tmp.OA.old,"] to [",tmp.OA.new,"]\n",sep=""))
								  cat(paste("\t\tOA of [",tmp.sys,"] [",tmp.city,"] [",tmp.OAsc,"] [",tmp.bldg,"] [",tmp.state,"] has been replaced from [",tmp.OA.old,"] to [",tmp.OA.new,"]\n",sep=""),file=FL.LOG,append=TRUE)
							}
						}
						# -------------------------------------------------------------------

						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.stateAux.OUT,paste(this.analysis,"_",this.state,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}

						# call "output_SchoolPrimary" to get output in the desired format
						myOut.clm <- output_SchoolPrimary(this.prototype)

						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	
					}	# end of standard loop
				}		# end of state loop
			}else if (this.analysis == "regular"){
			# *******************************************************************************************
			# for "regular" analysis
			# *******************************************************************************************		
					# *** standard loop ***
					idx.standard <- 0

					for (this.standard in list.standards)
					{
						idx.standard <- idx.standard + 1 

						if (this.regular == "ASHRAE")
						{
							this.OA.scenario <- paste("ASHRAE901",sub("ASHRAE30pct_","",this.standard),"OA2004",sep="_")
						}else if (this.regular == "IECC")
						{
							this.OA.scenario <- paste(this.standard,paste("IMC",sub("IECC_STD","",this.standard),sep=""),sep="_")
						}

						# the "base" or "advn" lable of this standard
						std.label <- c("base","advn")[idx.standard]

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,1] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.regular,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.regular,  A.standard == this.standard)
						myData.thisStandard.parm     <- subset(myParm.regular,    A.standard == this.standard)
						myData.thisStandard.plant    <- subset(myPlant.regular,   A.standard == this.standard)
						myData.thisStandard.pumprate <- subset(myPumpRate.regular,A.standard == this.standard) 
						myData.thisStandard.pump     <- subset(myPump.regular,    A.standard == this.standard)  
						myData.thisStandard.econ     <- subset(myEcon.regular,    A.standard == this.standard) 
						myData.thisStandard.szvav    <- subset(mySZvav.regular,   A.standard == this.standard)
						myData.thisStandard.damper   <- subset(myDamper.regular,  A.standard == this.standard)		# March 1, 2013: 
						myData.thisStandard.oa       <- subset(myOA.regular,     OA_scenario == this.OA.scenario) 
						myData.thisStandard.oa.mutz  <- subset(myOA.mutz.regular,OA_scenario == this.OA.scenario)	# February 14, 2013: for replacement of mutz OA 

						# -------------------------------------------------------------------
						# replace the system OA of VAV system with those calculated from mutz			 	# February 14, 2013: for replacement of mutz OA
						# -------------------------------------------------------------------
						for (i in seq(from=1,to = dim(myData.thisStandard.oa)[1],by=1))
						{
							# cat(paste(i,"\n",sep=""))

							if (length(grep("VAV",myData.thisStandard.oa[i,"systemName"],ignore.case = FALSE)) | length(grep("AHU-2",myData.thisStandard.oa[i,"systemName"],ignore.case = FALSE)))	# note: "VAV" used for all prototypes except "OutPatientHealthCare which used "AHU-2"
							{
								tmp.OA.old <- myData.thisStandard.oa[i,"value"] 
								tmp.sys    <- myData.thisStandard.oa[i,"systemName"]
								tmp.city   <- myData.thisStandard.oa[i,"A.CZ.climate"]
								tmp.OAsc   <- myData.thisStandard.oa[i,"OA_scenario"]
								tmp.bldg   <- myData.thisStandard.oa[i,"prototype"]
								tmp.state  <- myData.thisStandard.oa[i,"A.stateAbb"]
								tmp.OA.new <- myData.thisStandard.oa.mutz[myData.thisStandard.oa.mutz[,"sys"]          == tmp.sys   & 
													  myData.thisStandard.oa.mutz[,"prototype"]    == tmp.bldg  & 
													  myData.thisStandard.oa.mutz[,"OA_scenario"]  == tmp.OAsc  & 
													  myData.thisStandard.oa.mutz[,"A.stateAbb"]   == tmp.state & 
													  myData.thisStandard.oa.mutz[,"A.CZ.climate"] == tmp.city  ,"Vot"]

								myData.thisStandard.oa[i,"value"] <- tmp.OA.new	

								# 
								# cat(paste("\t\tOA of [",tmp.sys,"] [",tmp.city,"] [",tmp.OAsc,"] [",tmp.bldg,"] [",tmp.state,"] has been replaced from [",tmp.OA.old,"] to [",tmp.OA.new,"]\n",sep=""))
								  cat(paste("\t\tOA of [",tmp.sys,"] [",tmp.city,"] [",tmp.OAsc,"] [",tmp.bldg,"] [",tmp.state,"] has been replaced from [",tmp.OA.old,"] to [",tmp.OA.new,"]\n",sep=""),file=FL.LOG,append=TRUE)
							}
						}
						# -------------------------------------------------------------------



						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.regularAux.OUT,paste(this.analysis,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}

						# call "output_SchoolPrimary" to get output in the desired format
						myOut.clm <- output_SchoolPrimary(this.prototype)
						
						
						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	
					}	# end of standard loop						
			}			# end of the "regular" and "state" condition loop			


		# ---------------------------------------------------------------------------------------------------
		# This section is for Small Hotel cost data extraction
		# ---------------------------------------------------------------------------------------------------
		}else if (this.prototype == "HotelSmall"){

			idx.prototype <- idx.prototype + 1




			# --------------------------------------------------------------------------------------------
			# subsetting the data for both "state" and "regular" analysis
			# --------------------------------------------------------------------------------------------
			if (this.analysis == "state")
			{ 

				# [DATA SUBSET] of state analysis cases
				myFan.state      <- subset(myFan,     A.type == "state")
				myOA.state       <- subset(myOA,      A.type == "state")
				myDXCoil.state   <- subset(myDXCoil,  A.type == "state")
				myEcon.state     <- subset(myEcon,    A.type == "state") 
				mySZvav.state    <- subset(mySZvav,   A.type == "state") 
				myDamper.state   <- subset(myDamper,  A.type == "state")	# March 1, 2013: 

				cat(paste("[",this.prototype,"]: Successfully isolate the data for state analysis!\n",sep=""))
			}else if(this.analysis == "regular"){
				if (this.regular == "ASHRAE")
				{		
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,     A.type == "regular" & (standard =="STD2007"                    | standard == "STD2010"))
					myOA.regular       <- subset(myOA,      A.type == "regular" & (OA_scenario=="ASHRAE901_STD2007_OA2004" | OA_scenario=="ASHRAE901_STD2010_OA2004"))
					myDXCoil.regular   <- subset(myDXCoil,  A.type == "regular" & (standard =="STD2007"                    | standard == "STD2010"))
					myEcon.regular     <- subset(myEcon,    A.type == "regular" & (standard =="STD2007"                    | standard == "STD2010")) 
					mySZvav.regular    <- subset(mySZvav,   A.type == "regular" & (standard =="STD2007"                    | standard == "STD2010")) 
					myDamper.regular   <- subset(myDamper,  A.type == "regular" & (A.standard =="ASHRAE30pct_STD2007"      | A.standard =="ASHRAE30pct_STD2010"))	# March 1, 2013: 
				}else if(this.regular == "IECC")
				{
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,     A.type == "regular" & (standard =="STD2009"                | standard == "STD2012"))
					myOA.regular       <- subset(myOA,      A.type == "regular" & (OA_scenario=="IECC_STD2009_IMC2009" | OA_scenario=="IECC_STD2012_IMC2012"))
					myDXCoil.regular   <- subset(myDXCoil,  A.type == "regular" & (standard =="STD2009"                | standard == "STD2012"))
					myEcon.regular     <- subset(myEcon,    A.type == "regular" & (standard =="STD2009"                | standard == "STD2012")) 
					mySZvav.regular    <- subset(mySZvav,   A.type == "regular" & (standard =="STD2009"                | standard == "STD2012")) 		
					myDamper.regular   <- subset(myDamper,  A.type == "regular" & (A.standard =="IECC_STD2009"         | A.standard =="IECC_STD2012"))			# March 1, 2013: 
				}
				climateZone.array <- sort(unique(myOA.regular[,"A.CZ.city"]))

				cat(paste("[",this.prototype,"]: Successfully isolate the data for verifying on regular cases!\n",sep=""))
			}
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------


			# *******************************************************************************************
			# for "state" analysis
			# *******************************************************************************************
			if (this.analysis == "state")
			{
				# get the number of state
				state.abb.array  <- unique(myOA.state[,"A.stateAbb"])
				state.name.array <- state.name.exp[match(state.abb.array,state.abb.exp)]
				no.state         <- length(state.abb.array)

				# *** state loop ***
				for (this.state in state.abb.array)
				{
					# [DATA SUBSET] of current state
					myFan.thisState      <- subset(myFan.state,      A.stateAbb == this.state)
					myOA.thisState       <- subset(myOA.state,       A.stateAbb == this.state)
					myDXCoil.thisState   <- subset(myDXCoil.state,   A.stateAbb == this.state)
					myEcon.thisState     <- subset(myEcon.state,     A.stateAbb == this.state)
					mySZvav.thisState    <- subset(mySZvav.state,    A.stateAbb == this.state)
					myDamper.thisState   <- subset(myDamper.state,   A.stateAbb == this.state)			# March 1, 2013:

					# climate cities within the current state			
					cz.Fan.thisState      <- sort(unique(myFan.thisState[,"A.CZ.city"]))
					cz.OA.thisState       <- sort(unique(myOA.thisState[,"A.CZ.city"]))
					cz.DXCoil.thisState   <- sort(unique(myDXCoil.thisState[,"A.CZ.city"]))
					cz.Econ.thisState     <- sort(unique(myEcon.thisState[,"A.CZ.city"]))
					cz.SZvav.thisState    <- sort(unique(mySZvav.thisState[,"A.CZ.city"]))	
					cz.Damper.thisState   <- sort(unique(myDamper.thisState[,"A.CZ.city"]))				# March 1, 2013:


					# a quick check on the consistency of climate cities among different sources
					if (setequal(cz.Fan.thisState,cz.OA.thisState) & setequal(cz.OA.thisState,cz.DXCoil.thisState) & setequal(cz.DXCoil.thisState,cz.Econ.thisState) & setequal(cz.Econ.thisState,cz.SZvav.thisState)  & setequal(cz.SZvav.thisState,cz.Damper.thisState))
					{
						climateZone.array <- cz.OA.thisState
						standards.array   <- unique(myDXCoil.thisState[,"A.standard"])				# the standard name consists of both character and year and in no order
						standards.year    <- sub("(.*[^\\d+])(\\d+)(.*)","\\2",standards.array,perl=TRUE)	# extract the digit year from the standards array
					  names(standards.array)  <- standards.year							# use numeric standard year as the name of the standard
						tmp <- c("base","advn");names(tmp) <- sort(standards.year)				# define a temporal array [tmp] to use sorted standard year mapping "base" and "advn"
					  names(standards.array)  <- (tmp[names(standards.array)])					# re-name the standard with "base" and "advn" notation

						# a quick check on the number of standards
						if (length(standards.array) != 2)
						{
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""))
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
							die;
						}
					}else{
						cat(paste("the lis of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""))
						cat(paste("the lis of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
						die;			
					}

					# *** standard loop ***
					idx.standard <- 0
					# For each standard, collect data
					for (this.standard in standards.array)
					{
						idx.standard <- idx.standard + 1 

						# the "base" or "advn" lable of this standard
						std.label <- names(standards.array[standards.array == this.standard])

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,this.state,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,this.state] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.thisState,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.thisState,  A.standard == this.standard)
						myData.thisStandard.econ     <- subset(myEcon.thisState,    A.standard == this.standard) 
						myData.thisStandard.szvav    <- subset(mySZvav.thisState,   A.standard == this.standard)
						myData.thisStandard.damper   <- subset(myDamper.thisState,  A.standard == this.standard)	# March 1, 2013: 

						# get the corresponding OA scenario name
						this.OA.scenario <- mapOAscenario(myData.thisStandard.dx,this.standard)

						# [DATA SUBSET] of current state-standard: (b) extract the OA subset of data belong to this prototype-state-standard combination
						myData.thisStandard.oa <- subset(myOA.thisState,OA_scenario == this.OA.scenario) 


						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.stateAux.OUT,paste(this.analysis,"_",this.state,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}

						# call "output_HotelSmall" to get output in the desired format
						myOut.clm <- output_HotelSmall(this.prototype)
						

						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)					
					}	# end of standard loop
				}		# end of state loop
			}else if (this.analysis == "regular"){
			# *******************************************************************************************
			# for "regular" analysis
			# *******************************************************************************************		
					# *** standard loop ***
					idx.standard <- 0

					for (this.standard in list.standards)
					{
						idx.standard <- idx.standard + 1 

						if (this.regular == "ASHRAE")
						{
							this.OA.scenario <- paste("ASHRAE901",sub("ASHRAE30pct_","",this.standard),"OA2004",sep="_")
						}else if (this.regular == "IECC")
						{
							this.OA.scenario <- paste(this.standard,paste("IMC",sub("IECC_STD","",this.standard),sep=""),sep="_")
						}

						# the "base" or "advn" lable of this standard
						std.label <- c("base","advn")[idx.standard]

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,1] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.regular,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.regular,  A.standard == this.standard)
						myData.thisStandard.econ     <- subset(myEcon.regular,    A.standard == this.standard) 
						myData.thisStandard.damper   <- subset(myDamper.regular,  A.standard == this.standard)	# March 1, 2013: 
						myData.thisStandard.oa       <- subset(myOA.regular,     OA_scenario == this.OA.scenario) 

						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.regularAux.OUT,paste(this.analysis,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}

						# call "output_HotelSmall" to get output in the desired format
						myOut.clm <- output_HotelSmall(this.prototype)
						

						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)					
					}	# end of standard loop					
			}			# end of the "regular" and "state" condition loop




		# ---------------------------------------------------------------------------------------------------
		# This section is for MidRise Apartment cost data extraction
		# ---------------------------------------------------------------------------------------------------
		}else if (this.prototype == "ApartmentMidRise"){

			idx.prototype <- idx.prototype + 1

			# --------------------------------------------------------------------------------------------
			# subsetting the data for both "state" and "regular" analysis
			# --------------------------------------------------------------------------------------------
			if (this.analysis == "state")
			{ 
				# [DATA SUBSET] of state analysis cases
				myFan.state      <- subset(myFan,     A.type == "state")
				myOA.state       <- subset(myOA,      A.type == "state")
				myDXCoil.state   <- subset(myDXCoil,  A.type == "state")	 

				cat(paste("[",this.prototype,"]: Successfully isolate the data for state analysis!\n",sep=""))
			}else if(this.analysis == "regular"){
				if (this.regular == "ASHRAE")
				{		
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,     A.type == "regular" & (standard =="STD2007" | standard == "STD2010"))
					myOA.regular       <- subset(myOA,      A.type == "regular" & (OA_scenario=="ASHRAE901_STD2007_OA2004" | OA_scenario=="ASHRAE901_STD2010_OA2004"))
					myDXCoil.regular   <- subset(myDXCoil,  A.type == "regular" & (standard =="STD2007" | standard == "STD2010"))
				}else if (this.regular == "IECC")
				{
					# [DATA SUBSET] of regular analysis cases
					myFan.regular      <- subset(myFan,     A.type == "regular" & (standard =="STD2009" | standard == "STD2012"))
					myOA.regular       <- subset(myOA,      A.type == "regular" & (OA_scenario=="IECC_STD2009_IMC2009" | OA_scenario=="IECC_STD2012_IMC2012"))
					myDXCoil.regular   <- subset(myDXCoil,  A.type == "regular" & (standard =="STD2009" | standard == "STD2012"))			
				}
				climateZone.array <- sort(unique(myOA.regular[,"A.CZ.city"]))

				cat(paste("[",this.prototype,"]: Successfully isolate the data for verifying on regular cases!\n",sep=""))
			}
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------
			# --------------------------------------------------------------------------------------------


			# *******************************************************************************************
			# for "state" analysis
			# *******************************************************************************************
			if (this.analysis == "state")
			{

				# get the number of state
				state.abb.array  <- unique(myOA.state[,"A.stateAbb"])
				state.name.array <- state.name.exp[match(state.abb.array,state.abb.exp)]
				no.state         <- length(state.abb.array)

				# *** state loop ***
				for (this.state in state.abb.array)
				{
					# [DATA SUBSET] of current state
					myFan.thisState      <- subset(myFan.state,      A.stateAbb == this.state)
					myOA.thisState       <- subset(myOA.state,       A.stateAbb == this.state)
					myDXCoil.thisState   <- subset(myDXCoil.state,   A.stateAbb == this.state)

					# climate cities within the current state			
					cz.Fan.thisState      <- sort(unique(myFan.thisState[,"A.CZ.city"]))
					cz.OA.thisState       <- sort(unique(myOA.thisState[,"A.CZ.city"]))
					cz.DXCoil.thisState   <- sort(unique(myDXCoil.thisState[,"A.CZ.city"]))


					# a quick check on the consistency of climate cities among different sources
					if (setequal(cz.Fan.thisState,cz.OA.thisState) & setequal(cz.OA.thisState,cz.DXCoil.thisState))
					{
						climateZone.array <- cz.OA.thisState
						standards.array   <- unique(myDXCoil.thisState[,"A.standard"])				# the standard name consists of both character and year and in no order
						standards.year    <- sub("(.*[^\\d+])(\\d+)(.*)","\\2",standards.array,perl=TRUE)	# extract the digit year from the standards array
					  names(standards.array)  <- standards.year							# use numeric standard year as the name of the standard
						tmp <- c("base","advn");names(tmp) <- sort(standards.year)				# define a temporal array [tmp] to use sorted standard year mapping "base" and "advn"
					  names(standards.array)  <- (tmp[names(standards.array)])					# re-name the standard with "base" and "advn" notation

						# a quick check on the number of standards
						if (length(standards.array) != 2)
						{
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""))
							cat(paste("the number of standards is not 2 for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
							die;
						}
					}else{
						cat(paste("the lis of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""))
						cat(paste("the lis of climate cities in the Fan, Parm, OA,DX Coil, DX Coil2, Plant, PumpRate,Pump files are NOT the same for [",this.prototype,"] at [",this.state,"]\n",sep=""),file=FL.LOG,append=TRUE)
						die;			
					}

					# *** standard loop ***
					idx.standard <- 0
					# For each standard, collect data
					for (this.standard in standards.array)
					{
						idx.standard <- idx.standard + 1 

						# the "base" or "advn" lable of this standard
						std.label <- names(standards.array[standards.array == this.standard])

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,this.state,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,this.state] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.thisState,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.thisState,  A.standard == this.standard)

						# get the corresponding OA scenario name
						this.OA.scenario <- mapOAscenario(myData.thisStandard.dx,this.standard)

						# [DATA SUBSET] of current state-standard: (b) extract the OA subset of data belong to this prototype-state-standard combination
						myData.thisStandard.oa <- subset(myOA.thisState,OA_scenario == this.OA.scenario) 


						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.stateAux.OUT,paste(this.analysis,"_",this.state,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}


						# call "output_ApartmentMidRise" to get output in the desired format
						myOut.clm <- output_ApartmentMidRise(this.prototype)
						
						
						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	
					}	# end of standard loop
				}		# end of this state loop
			}else if (this.analysis == "regular"){
			# *******************************************************************************************
			# for "regular" analysis
			# *******************************************************************************************		
					# *** standard loop ***
					idx.standard <- 0
					# # For each standard, collect cooling coil capacity and system OA rates
					# if (this.regular == "ASHRAE")
					# {
					# 	list.standards         <- c("ASHRAE30pct_STD2007","ASHRAE30pct_STD2010")
					# }else if (this.regular == "IECC")
					# {
					# 	list.standards         <- c("IECC_STD2009","IECC_STD2012")
					# }

					for (this.standard in list.standards)
					{
						idx.standard <- idx.standard + 1 

						if (this.regular == "ASHRAE")
						{
							this.OA.scenario <- paste("ASHRAE901",sub("ASHRAE30pct_","",this.standard),"OA2004",sep="_")
						}else if (this.regular == "IECC")
						{
							this.OA.scenario <- paste(this.standard,paste("IMC",sub("IECC_STD","",this.standard),sep=""),sep="_")
						}

						# the "base" or "advn" lable of this standard
						std.label <- c("base","advn")[idx.standard]

						# dynamically define the data frame "prototype.name]_[base|advn]" 
						# this data frame is used to hold the extracted data for current standard of current prototype across all climate zones
						df.name <- paste(std.label,this.prototype,sep="_")

						# assign this dynamically generate data frame to the data frame arrays
						df.array[this.prototype,std.label,1] <- df.name

						# [DATA SUBSET] of current state-standard: (a) extract the DXCoil subset of data belong to this prototype-state-standard combination
						myData.thisStandard.fan      <- subset(myFan.regular,     A.standard == this.standard)
						myData.thisStandard.dx       <- subset(myDXCoil.regular,  A.standard == this.standard)
						myData.thisStandard.oa       <- subset(myOA.regular,     OA_scenario == this.OA.scenario) 

						# ----------------------------------------------------------------------------------
						# Create a summary file for each standard
						# ----------------------------------------------------------------------------------
						FL.auxi.out <- paste(Path.regularAux.OUT,paste(this.analysis,"_",this.prototype,"_",this.standard,".csv",sep=""),sep="/")
						if (file.exists(FL.auxi.out)){print(paste(FL.auxi.out,"exist.Delete it!"));file.remove(FL.auxi.out)}


						# call "output_ApartmentMidRise" to get output in the desired format
						myOut.clm <- output_ApartmentMidRise(this.prototype)

						# Write a summary output file for each standard
						cat(",",file=FL.auxi.out,append=TRUE)
						write.table(myOut.clm,file=FL.auxi.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

						# assign [myOut.clm] to the dynamially generated data frame df.name
						command.string  <- paste(df.name," <- myOut.clm",sep="")	# assign [myOut.clm] to the dynamically generated data frame for current standard-prototype combination
						eval(parse(text=command.string))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""))
						cat(paste(this.prototype,"\tall extracted data for [",std.label,"] standard which is [",this.standard,"] has been saved into a data frame [",df.name,"]\n",sep=""),file=FL.LOG,append=TRUE)	
					}	# end of standard loop					
			}			# end of the "regular" and "state" condition loop										
		}			# end of list of state loop
		
		cat(paste("Deleting inputted data frames and derived data frames for [",this.prototype,"]\n",sep=""))
		cat(paste("Deleting inputted data frames and derived data frames for [",this.prototype,"]\n",sep=""),file=FL.LOG,append=TRUE)		
		# to avoid potential problem, the data frames reading for this prototypes (i.e., "list.df", "list.sys") are removed.  
		command.string <- paste("rm(",paste(c("myOut.clm",c(list.df,list.sys)),collapse=","),")",sep="")
		eval(parse(text=command.string))
		
		# cannot do this because the data frame "myData.OA.mutz" is not in the local level
		# # also delete data frames starting with "myData"
		# command.string <- paste("rm(",paste(grep("^myData",ls(),value=TRUE),collapse=","),")",sep="")
		# eval(parse(text=command.string))	
		
		# # also delete data frames starting with "my" 
		# command.string <- paste("rm(",paste(grep("^my",ls(),value=TRUE),collapse=","),")",sep="")
		# eval(parse(text=command.string))		
		
	}				# end of prototype loop
	cat(paste("HVAC data have been processed for all states and all prototypes!\n",sep=""))
	cat(paste("HVAC data have been processed for all states and all prototypes!\n"),file=FL.LOG,append=TRUE)	




	# -----------------------------------------------------------------------------------------------------------
	#
	#                          ORGANIZED OUTPUT
	#
	# the desired output of each prototypes will be in two data frames, i.e., "base_[protptyeName]_[stateAbb]" for state analysis and base_[protptyeName]" for regular analysis, base cases
	#                                                                         "advn_[protptyeName]_[stateAbb]" for state analysis and base_[protptyeName]" for regular analysis, advn cases
	# Current analysis we will have 2 (base/advn) by 6 (prototypes) by (22 (state) + 1 (regular)) = 276 of such data frames
	# -----------------------------------------------------------------------------------------------------------
	if (this.analysis == "state")
	{
		# April 25, 2013: add an overall output file for keeping data from all states
		FL.allState.out <- paste(Path.main.OUT,paste("allStates.csv",sep=""),sep="/")							# April 25, 2013, added an overall output for all states
		if (file.exists(FL.allState.out)){print(paste(FL.allState.out,"exist.Delete it!"));file.remove(FL.allState.out)}		# April 25, 2013, added an overall output for all states
		
		
		for (this.state.abb in state.abb.array)
		{

			this.state.name <- state.name.array[match(this.state.abb,state.abb.array)]
			
			# output the state name and abbreviation
			cat(paste(paste(this.state.name,this.state.abb,sep=","),"\n\n",sep=""),file=FL.allState.out,append=TRUE)		# April 25, 2013, added an overall output for all states

			# Create one summary file for each state
			FL.state.out <- paste(Path.main.OUT,paste(this.analysis,"_",this.state.name,".csv",sep=""),sep="/")
			if (file.exists(FL.state.out)){print(paste(FL.state.out,"exist.Delete it!"));file.remove(FL.state.out)}

			cat(paste("[",this.state.name,"]: re-arrange data outputted to [",FL.state.out,"]!\n",sep=""))
			cat(paste("[",this.state.name,"]: re-arrange data outputted to [",FL.state.out,"]!\n",sep=""),file=FL.LOG,append=TRUE)	

			for (this.prototype in prototypes.array)
			{

				# output the prototype label
				cat(paste(this.prototype,"\n",sep=""),file=FL.state.out,   append=TRUE)
				cat(paste(this.prototype,"\n",sep=""),file=FL.allState.out,append=TRUE)						# April 25, 2013, added an overall output for all states

				df.base <- paste("base",this.prototype,this.state.abb,sep="_")
				df.advn <- paste("advn",this.prototype,this.state.abb,sep="_")

				# reassign the actual "base" and "advn" data frames of this prototype for this state into two fixed data frames [df.base], [df.advn]
				command.string  <- paste("myBase <- ",df.base,sep="")	# assign [df.base] to the "myBase"
				eval(parse(text=command.string))

				command.string  <- paste("myAdvn <- ",df.advn,sep="")	# assign [df.advn] to the "myAdvn"
				eval(parse(text=command.string))		

				# a quick check
				row.names.base <- row.names(myBase)
				row.names.advn <- row.names(myAdvn)
				if ((!(setequal(row.names.base,row.names.advn))) | (dim(myBase)[1] != dim(myAdvn)[1]) | (dim(myBase)[2] != dim(myAdvn)[2]))
				{
					cat(paste("[",this.prototype,"] at [",this.state.abb,"]: either the row names or the dimentiosn of [myBase] is different from those of [myAdvn], please check!\n",sep=""))
					cat(paste("[",this.prototype,"] at [",this.state.abb,"]: either the row names or the dimentiosn of [myBase] is different from those of [myAdvn], please check!\n",sep=""),file=FL.LOG,append=TRUE)
					die
				}else{
					# get the climate city names: assume "base" and "advn" have the same climate zones 
					col.names.base<- unlist(strsplit(names(myBase),"\\s+",perl=TRUE))
					CZ.city.local <- col.names.base[seq(from=1,to=length(col.names.base),by=2)]
					CZ.zone.local <- as.character(CZ.array[match(CZ.city.local,CZ.array[,"city"]),"zone"])
					CZ.local      <- data.frame(city = CZ.city.local,zone = CZ.zone.local)
					o             <- order(CZ.local[,"zone"])	# sort the zone accord
					CZ.local      <- CZ.local[o,]			# an array consists of both climate city name and climate zone label for the CZ within the current state


					# loop through the list of local climate zones and first base stabdard and then advanced standard
					idx.CZ <- 0
					# re-arrange the format for output:
					for (this.CZ.city in CZ.local[,"city"])
					{
						idx.CZ <- idx.CZ + 1

						# get the climate zone name, climate zone label, base standard name, advn standard name for this prototype-state combination
						this.CZ.zone <- CZ.local[CZ.local[,"city"] == this.CZ.city,"zone"]
						this.base.std <- unlist(strsplit(names(myBase[1]),"\\s+",perl=TRUE))[2]	# the standard name is the second part separated by the space in the field name
						this.advn.std <- unlist(strsplit(names(myAdvn[1]),"\\s+",perl=TRUE))[2]	# the standard name is the second part separated by the space in the field name

						# the field name to be used for the combined data frame
						col.name.base <- paste(paste(this.CZ.zone,"(",this.CZ.city,")",sep=""),this.base.std,sep=" ")
						col.name.advn <- paste(paste(this.CZ.zone,"(",this.CZ.city,")",sep=""),this.advn.std,sep=" ")

						# combine the data at this climate zone
							  tmp  <- data.frame(myBase[,grep(this.CZ.city,names(myBase),value=TRUE)],myAdvn[,grep(this.CZ.city,names(myAdvn),value=TRUE)])
						    names(tmp) <- c(col.name.base,col.name.advn)
						row.names(tmp) <- row.names.base
						if (idx.CZ == 1)
						{
							myComb.state <- tmp
						}else{
							myComb.state <- cbind(myComb.state,tmp)				
						}
					}

					# output the organized output for current state
					cat(paste(",",sep=""),   file=FL.state.out,append=TRUE)
					write.table(myComb.state,file=FL.state.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					cat(paste("\n\n",sep=""),file=FL.state.out,append=TRUE)	
					
					# output the organized output for current state								# April 25, 2013, added an overall output for all states
					cat(paste(",",sep=""),   file=FL.allState.out,append=TRUE)						# April 25, 2013, added an overall output for all states
					write.table(myComb.state,file=FL.allState.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)	# April 25, 2013, added an overall output for all states
					cat(paste("\n\n",sep=""),file=FL.allState.out,append=TRUE)						# April 25, 2013, added an overall output for all states	
				}
				cat(paste("\tprocessed data for [",this.prototype,"]!\n",sep=""))
				cat(paste("\tprocessed data for [",this.prototype,"]!\n",sep=""),file=FL.LOG,append=TRUE)			

				# put all data into a csv file
				cat(paste(paste(this.analysis,this.state.abb,this.prototype,sep="-"),",",sep=""),file=FL.CSV,append=TRUE)
				write.table(file=FL.CSV,cbind(myBase,myAdvn),sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
				cat("\n\n",file=FL.CSV,append=TRUE)			
			}
		}
	}else if (this.analysis == "regular")
	{		
		# Create one summary file for each state
		FL.regular.out <- paste(Path.main.OUT,paste(this.analysis,"_",this.regular.stdString,".csv",sep=""),sep="/")

		if (file.exists(FL.regular.out)){print(paste(FL.regular.out,"exist.Delete it!"));file.remove(FL.regular.out)}

		cat(paste("re-arrange data outputted to [",FL.regular.out,"]!\n",sep=""))
		cat(paste("re-arrange data outputted to [",FL.regular.out,"]!\n",sep=""),file=FL.LOG,append=TRUE)	

		for (this.prototype in prototypes.array)
		{	

			# output the prototype label
			cat(paste(this.prototype,"\n",sep=""),file=FL.regular.out,append=TRUE)

			df.base <- paste("base",this.prototype,sep="_")
			df.advn <- paste("advn",this.prototype,sep="_")

			# reassign the actual "base" and "advn" data frames of this prototype for this state into two fixed data frames [df.base], [df.advn]
			command.string  <- paste("myBase <- ",df.base,sep="")	# assign [df.base] to the "myBase"
			eval(parse(text=command.string))

			command.string  <- paste("myAdvn <- ",df.advn,sep="")	# assign [df.advn] to the "myAdvn"
			eval(parse(text=command.string))		

			# a quick check
			row.names.base <- row.names(myBase)
			row.names.advn <- row.names(myAdvn)
			if ((!(setequal(row.names.base,row.names.advn))) | (dim(myBase)[1] != dim(myAdvn)[1]) | (dim(myBase)[2] != dim(myAdvn)[2]))
			{
				cat(paste("[",this.prototype,"]: either the row names or the dimentiosn of [myBase] is different from those of [myAdvn], please check!\n",sep=""))
				cat(paste("[",this.prototype,"]: either the row names or the dimentiosn of [myBase] is different from those of [myAdvn], please check!\n",sep=""),file=FL.LOG,append=TRUE)
				die
			}else{
				# get the climate city names: assume "base" and "advn" have the same climate zones 
				col.names.base<- unlist(strsplit(names(myBase),"\\s+",perl=TRUE))
				CZ.city.local <- col.names.base[seq(from=1,to=length(col.names.base),by=2)]
				CZ.zone.local <- as.character(CZ.array[match(CZ.city.local,CZ.array[,"city"]),"zone"])
				CZ.local      <- data.frame(city = CZ.city.local,zone = CZ.zone.local)
				o             <- order(CZ.local[,"zone"])	# sort the zone accord
				CZ.local      <- CZ.local[o,]			# an array consists of both climate city name and climate zone label for the CZ within the current state


				# loop through the list of local climate zones and first base stabdard and then advanced standard
				idx.CZ <- 0
				# re-arrange the format for output:
				for (this.CZ.city in CZ.local[,"city"])
				{
					idx.CZ <- idx.CZ + 1

					# get the climate zone name, climate zone label, base standard name, advn standard name for this prototype-state combination
					this.CZ.zone <- CZ.local[CZ.local[,"city"] == this.CZ.city,"zone"]
					this.base.std <- unlist(strsplit(names(myBase[1]),"\\s+",perl=TRUE))[2]	# the standard name is the second part separated by the space in the field name
					this.advn.std <- unlist(strsplit(names(myAdvn[1]),"\\s+",perl=TRUE))[2]	# the standard name is the second part separated by the space in the field name

					# the field name to be used for the combined data frame
					col.name.base <- paste(paste(this.CZ.zone,"(",this.CZ.city,")",sep=""),this.base.std,sep=" ")
					col.name.advn <- paste(paste(this.CZ.zone,"(",this.CZ.city,")",sep=""),this.advn.std,sep=" ")

					# combine the data at this climate zone
						  tmp  <- data.frame(myBase[,grep(this.CZ.city,names(myBase),value=TRUE)],myAdvn[,grep(this.CZ.city,names(myAdvn),value=TRUE)])
					    names(tmp) <- c(col.name.base,col.name.advn)
					row.names(tmp) <- row.names.base
					if (idx.CZ == 1)
					{
						myComb.regular <- tmp
					}else{
						myComb.regular <- cbind(myComb.regular,tmp)				
					}
				}

				# output the organized output for current state
				cat(paste(",",sep=""),file=FL.regular.out,append=TRUE)
				write.table(myComb.regular,file=FL.regular.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
				cat(paste("\n\n",sep=""),file=FL.regular.out,append=TRUE)	

			}

			cat(paste("\tprocessed data for [",this.prototype,"]!\n",sep=""))
			cat(paste("\tprocessed data for [",this.prototype,"]!\n",sep=""),file=FL.LOG,append=TRUE)			

			# put all data into a csv file
			cat(paste(paste(this.analysis,this.prototype,sep="-"),",",sep=""),file=FL.CSV,append=TRUE)
			write.table(file=FL.CSV,cbind(myBase,myAdvn),sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat("\n\n",file=FL.CSV,append=TRUE)		
		}
	}

	# -------------------------------------------------------------------------------------------------
	# save everything in a R object file
	# -------------------------------------------------------------------------------------------------
	 save(list = ls(all=TRUE), file = FL.OBJ)


	# -------------------------------------------------------------------------------------------------
	# return to the script folder
	# -------------------------------------------------------------------------------------------------
	setwd(Path.Script)
}

# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\nextract_HVAC_revised.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\nextract_HVAC_revised.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [extract_HVAC_revised.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [extract_HVAC_revised.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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




























































