#
# Figure2_42_barchart_PTRMS_4episode.R
#
# !!!!!!! Need to run under R2.11.1 (2010-05-31) 64 bit !!!!!!!!!!!
# !!!!!!! Need remove packages "matrixcalc","ggplot2","openair" in the "package_loading.R" file.
#
# Work on R2.11.1 (2010-05-31) but not on R 2.15.2 (2012-10-26)
#
# July 11, 2013: revised for generating better figures for the manuscript
#
#
# created on August 10, 2009
# (a) combine the three sets of barcharts (i.e, PTRMS-raw, PTRMS-carbonConc., OHreacticity) from three separate plotting scripts (40a_barchart_PTRMS.R,40b_barchart_PTRMS_carbonConc.R,40c_barchart_PTRMS_OHreactivity.R) into a single plotting script
# (b) add a synoptics loop to consider the 4 distinct meteorological regimes identified by Carl.
#
#  See Carl's email on August 07, 2009 3:57 pm
# (1) Sep 4th - 8th, northerly primarily northeast flow
# (2) Sep 9th-11th,  southeasterly or easterly flow
# (3) Sep 15th-18th, south from the Gulf
# (4) Sep 25th-27th, northly again
#
# 
#
# Figure2_42_barchart_PTRMS_4episode.R (CST time which is CDT + 1)  (or CDT = CST - 1)
#
# INPUT
# 7a_merge4sites/
#
# OUTPUT
# Figure2_42_barchart_PTRMS_4episode/
#
# -------------------------------------------------------------------------
#
# 
# 0. eliminate all stuff
# 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# 
# 1. load libraries
# 
library(lattice)
library(chron)	
library(gplots)

# date/time information
time.info <- "LST which is CST: according to Shelly Sep 23, 2008 email.  UTC=CST+6, UTC=CDT+5 in Summer, UTC=CDT+6 in Winter"

# 
# 2. setup plotting limits for x axis
# 
xlim4plot <- c(chron(dates="9/1/2006", times="0:0:0",format=c('m/d/y','h:m:s')),chron(dates="10/1/2006",times="23:55:0",format=c('m/d/y','h:m:s')))
sites     <- c("Aldine","Bayland","DeerPark")

day.label <- c( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
day.names <- c( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)


TP.label <- c("Sep4_8","Sep9_11","Sep15_18","Sep25_27")
TP.names <- c("Sep4_8","Sep9_11","Sep15_18","Sep25_27")

#
# define a complete color array
#
col.array.complete <- c("red","blue","green","cyan","magenta","black","purple","brown")
time.split <- c("all data","mid morning","mid afternoon", "mid night","daytime","nighttime")
time.split <- c("all data")

# -----------------------------------------------------------------------------------------------------------
# 3. mz and chemical mapping table for PTRMS data
# The OH-rate coefficients are taken from AE 43, 1131-1135(2009) which is saved as 
# -----------------------------------------------------------------------------------------------------------
mapping.tab <- data.frame(mz      = c( 33,       35,                  42,            43,       45,            47,       57,                59,                61,           63,                69,        71,        73,                                79,       93,       95,      105,      107,          109,      121,          135,          137),
                          species = c("Methanol","Hydrogen-sulphide","Acetonitrile","Propene","Acetaldehyde","Ethanol","Butenes+acrolein","Acetone+propanal","Acetic-acid","Dimethylsulphide","Isoprene","MVK+MACR","2-butanone+butanal+methylglyoxal","Benzene","Toluene","Phenol","Styrene","C2-benzenes","Cresols","C3-Benzenes","C4-benzenes","Mono-terpenes"),
                          carbon  = c( 1,        1,                   2,             3,        2,             2,        3,                 3,                 2,            2,                 5,         4,         4,                                 6,        7,        6,       8,        8,            7,        9,            10,           10),
                          OHrate  = c( 0.0933,   0,                   0,             2.6303,   1.5849,        0.3236,   5.1286,            0.02188,           0.07943,      0.4786,            10,        4.3652,    0.8333,                            0.123,    0.6026,   2.6303,  5.7544,   1.7378,       5.1286,   2.7542,       0.4571,       0))
#                                                NA                   NA                                                                                                                                                                                                                                                                           NA             
# NOTE: I reassigned NA to 0 to avoid potential problems
# ------------------------------------------------------------------------------------------------------------



#
# add a measure array
#
measure.array <- c("mean","median")
measure.array <- c("mean")

# 
# 4. change to the script directory
# 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "V:/Analysis_Revised/0_scripts"
}else{
	Path.Current <- "V:/Analysis_Revised/0_scripts"
}
setwd(Path.Current)


# ---------------------------------------------------------------------------------------------------
# 5. define a summary function
# ---------------------------------------------------------------------------------------------------
source(paste(Path.Current,"my_functions_YLX.R",sep="/"))
source(paste(Path.Current,"package_loading.R",sep="/"))

Path.in.mother   <- "../7a_merge4sites"				# INPUT processed result directory
Path.out.mother  <- "../Figure2_42_barchart_PTRMS_4episode"	# OUTPUT processed result directory
Path.LOG         <- "../0_log"					# OUTPUT log  directory

if (!file.exists(Path.out.mother)){print(paste("NOT existing:",Path.out.mother));dir.create(Path.out.mother,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.LOG)){print(paste("NOT existing:",Path.LOG));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}


plot.types <- c("Conc","carbonConc","OHreactivity")
plot.types <- c(       "carbonConc")
cat("1\n")

# -----------------------------------------------------------------------------------------------------------
# 6. setup LOG file
# -----------------------------------------------------------------------------------------------------------
FL.TIME     <- paste(Path.LOG,"time.log",sep="/")					# OUTPUT Time Log file for all scripts
FL.LOG      <- paste(Path.LOG,"Figure2_42_barchart_PTRMS_4episode.log",sep="/")		# OUTPUT Log file

if (file.exists(FL.LOG)){print(paste(FL.LOG," exist. Delete it!"));file.remove(FL.LOG)}

#
# averaging options
#
avg.options <- c("raw","avg_60minu","avg_1minu","avg_5minu","avg_10minu","avg_30minu")
avg.options <- c("raw")

#
# looping through averaging options
#
for (avg.opt in avg.options)
{
	#
	# there is subfolder for each averaging option
	#
	Path.in <- paste(Path.in.mother,avg.opt,sep="/")
	if (!file.exists(Path.in)){stop(paste("NOT existing:",Path.in))}
	
	Path.out <- paste(Path.out.mother,avg.opt,sep="/")
	if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}


	#
	# averaging option
	#
	print(paste("----------------------------- average option:",avg.opt,"--------------------------\n",sep=""))

	cat(paste("\n(",avg.opt,") now processing data for averaging option (",avg.opt,")!\n",sep=""),file=FL.LOG, append=TRUE)
	
	cat("2\n")
	
	# -----------------------------------------------------------------------------------------
	# looping through sets 
	# ----------------------------------------------------------------------------------------- 
	for (set in c(1))
	{
		if (set == 1)
		{
			list.VOC <- c("mz43","mz69","mz71","mz79","mz93","mz107","mz121")		# 
			no.column <- 2
		}
		if (set == 2)
		{
			list.VOC <- c("mz43","mz45","mz59","mz61","mz71")	# common high abundant species
			no.column <- length(list.VOC)
		}
		if (set == 3)
		{
			list.VOC <- c("mz43","mz69","mz71","mz107","mz121")	# common high OH-rate species
			no.column <- length(list.VOC)
		}
		
		
		FL.OBJ       <- paste(Path.in,paste("mergedData_",avg.opt,".Rdata",sep=""),sep="/")				# INPUT R data object file


		cat("3a\n")

		# -----------------------------------------------------------------------------------------------------------
		# 1. load merged data
		# -----------------------------------------------------------------------------------------------------------
		load(FL.OBJ)
		cat(paste("(",avg.opt,") final merged file is loaded for (",avg.opt,")!\n",sep=""),file=FL.LOG,append=TRUE)	

		#
		# note the difference data object name for "raw" and "averaged" data
		#
		if(avg.opt == "raw"){work.data<-data.merged}else{work.data<-data.merged.avg}
		# ********************************************
		# note: the data object loaded is [work.data]
		# ********************************************
		
		
		#
		# add the fl;ag of 4 distinct time periods Carl Identified on August 7, 2009
		#
		time.period <- rep("other",dim(work.data)[1])
		time.period[work.data[,"CST.Day"]>=4  & work.data[,"CST.Day"]<=8]  <- "Sep4_8" 
		time.period[work.data[,"CST.Day"]>=9  & work.data[,"CST.Day"]<=11] <- "Sep9_11"
		time.period[work.data[,"CST.Day"]>=15 & work.data[,"CST.Day"]<=18] <- "Sep15_18"
		time.period[work.data[,"CST.Day"]>=25 & work.data[,"CST.Day"]<=27] <- "Sep25_27"					

		#
		# add the period identified by Carl as an additional column of the data objects [work.data]
		#
		work.data <- data.frame(work.data[time.period != "other",],time.period = time.period[time.period != "other"])
		

		#
		# convert "time.period" to factor
		#
		work.data[,"time.period"] <- factor(work.data[,"time.period"],levels=TP.label,labels=TP.names,ordered=TRUE)
	
	
		cat("3b\n")
		
		#
		# looping through the plot types
		#
		for (plot.type in plot.types)	# i.e., "Conc","carbonConc","OHreactivity"
		{
			# the data for plotting is denoted in [data.4plot] rather than [work.data]
			data.4plot <- work.data


			if (plot.type == "Conc")
			{
				# -----------------------------------------------------------------------------------------------------------
				# convert the concentration into concentration of carbons
				# -----------------------------------------------------------------------------------------------------------
				list.VOC_all <- grep("mz",names(work.data),value=TRUE)
			}
			
			
			if (plot.type == "carbonConc")
			{
				# -----------------------------------------------------------------------------------------------------------
				# convert the concentration into concentration of carbons
				# -----------------------------------------------------------------------------------------------------------
				list.VOC_all <- grep("mz",names(work.data),value=TRUE)
				for (idx.VOC in list.VOC_all)
				{
					carbon.VOC <- mapping.tab[paste("mz",mapping.tab[,"mz"],sep="")==idx.VOC,"carbon"]
					data.4plot[,idx.VOC] <- work.data[,idx.VOC]*carbon.VOC
				}
			}
			
			if (plot.type == "OHreactivity")
			{
				# -----------------------------------------------------------------------------------------------------------
				# convert the concentration into OH-reactivity 
				# -----------------------------------------------------------------------------------------------------------
				list.VOC_all <- grep("mz",names(work.data),value=TRUE)
				for (idx.VOC in list.VOC_all)
				{
					OHrate.VOC <- mapping.tab[paste("mz",mapping.tab[,"mz"],sep="")==idx.VOC,"OHrate"]
					data.4plot[,idx.VOC] <- work.data[,idx.VOC]*OHrate.VOC
				}
			}
			
			cat("4a\n")

			#
			# define files
			#
			Path.plot <- paste(Path.out,plot.type,sep="/")
			if (!file.exists(Path.plot)){print(paste("NOT existing:",Path.plot));dir.create(Path.plot,showWarnings=TRUE,recursive=TRUE)}

			
			FL.BAR.PDF   <- paste(Path.plot,paste("set",set,"_barchart_",avg.opt,".pdf",sep=""),sep="/")		# OUTPUT barchart in Concentration unit
			TI.BAR.PDF   <- paste("barchart_",avg.opt,".pdf",sep="")
			FL.SUM.OBJ   <- paste(Path.plot,paste("set",set,"_Summary_",avg.opt,".Rdata",sep=""),sep="/")		# OUTPUT aggregated data in R object file
			FL.SUM.CSV   <- paste(Path.plot,paste("set",set,"_Summary_",avg.opt,".csv",sep=""),sep="/")		# OUTPUT aggregated data in csv format
			if (file.exists(FL.SUM.CSV)){print(paste(FL.SUM.CSV," exist. Delete it!"));file.remove(FL.SUM.CSV)}
			if (file.exists(FL.BAR.PDF)){print(paste(FL.BAR.PDF," exist. Delete it!"));file.remove(FL.BAR.PDF)}
			if (file.exists(FL.SUM.OBJ)){print(paste(FL.SUM.OBJ," exist. Delete it!"));file.remove(FL.SUM.OBJ)}


			# -----------------------------------------------------------------------------------------
			# open PDF file for output the plots
			# -----------------------------------------------------------------------------------------
			pdf(file = FL.BAR.PDF,title=TI.BAR.PDF,paper="a4r", width=0, height=0)


			#
			# only get the subset data for the VOC to be plotted (i.e., isoprene (mz69", propene (mz43) and benzene (mz79) currently)
			# [data.subset.wide] is a subset of [data.4plot]
			#
			data.subset.wide <- subset(data.4plot,site.name != "MoodyTower",select = c("site.name","CST.Date","CST.Time","CST.Month","CST.Day","CST.Year","CST.Hour","CST.Minute","CST.Second","time.period",list.VOC_all))


			cat("4b\n")
			
			# 
			# loopping through time split
			# the data used for plotting for each time splitting is [data.Conc.wide] which is a subset of [data.subset.wide]
			#
			for (lab.time in time.split)
			{
				if (lab.time == "all data")
				{
					data.Conc.wide <- data.subset.wide
				}
				if (lab.time == "mid morning")
				{
					data.Conc.wide <- subset(data.subset.wide,CST.Hour>=6 & CST.Hour<=11)			# mid-morning between  6 am to 11 am (see Carl's email on June 25, 2009 12:46pm)
				}
				if (lab.time == "mid afternoon")
				{
					data.Conc.wide <- subset(data.subset.wide,CST.Hour>=13 & CST.Hour<=16)			# mid-afternoon between 13 pm to 16 pm (1pm to 4pm) (see Carl's email on June 25, 2009 12:46pm)
				}
				if (lab.time == "mid night")
				{
					data.Conc.wide <- subset(data.subset.wide,CST.Hour>=22 | CST.Hour<=2)			# mid-night between 22 pm to 2 am (10pm to 2am) (see Carl's email on June 25, 2009 12:46pm)
				}
				if (lab.time == "daytime")
				{
					data.Conc.wide <- subset(data.subset.wide,CST.Hour>6 & CST.Hour<=18)			# daytime
				}
				if (lab.time == "nighttime")
				{
					data.Conc.wide <- subset(data.subset.wide,(CST.Hour>18 & CST.Hour<=24) | (CST.Hour<=6))	# nighttime
				}			


				# **************************************************************
				# for each time split option, the data object is [data.Conc.wide]
				# **************************************************************
				#
				# split the data fields into two parts in order to make a long format
				#
				lab.mz    <- grep("mz",names(data.Conc.wide),value=TRUE)								# the three mz defined here


				cat("5a\n")

				# ---------------------------------------------------------------------------------
				# loopping through different aggregation measure
				# ---------------------------------------------------------------------------------
				for (name.measure in measure.array)
				{
					cat(paste("current aggregation option is ",name.measure,"\n",sep=""))
					cat(paste("current aggregation option is ",name.measure,"\n",sep=""),file=FL.LOG,append=TRUE)

					# aggrgate: note: only "Minute" is in factor class
					data.avg.wide  <- aggregate(data.Conc.wide[,lab.mz,drop=FALSE],list(Site=data.Conc.wide[,"site.name"],
													    time.period=data.Conc.wide[,"time.period"]),
													    name.measure,na.rm=TRUE)

					o <- order(data.avg.wide[,"Site"],data.avg.wide[,"time.period"])
					data.avg.wide <- data.avg.wide[o,]
					# **************************************************************
					# the aggregated data object is [data.avg.wide]
					# **************************************************************


					lab.other <- names(data.avg.wide)[!(names(data.avg.wide) %in% lab.mz)]						# the other fileds other than the mzs
					lab.VOC_all <- as.character(mapping.tab[mapping.tab[,"mz"] %in% sub('^\\D+','', lab.mz,perl=TRUE),"species"])	# the VOC name corresponding to the lab.mz (NOTE: not necessary in the same order, but does not matter if we only want to extract the fields)
					lab.VOC     <- as.character(mapping.tab[mapping.tab[,"mz"] %in% sub('^\\D+','', list.VOC,perl=TRUE),"species"])



					# rename the mz fields with the VOC names
					labs    <- names(data.avg.wide)
					for (lab in labs)
					{
						if(length(grep("mz",lab)))
						{
							this.VOC <- mapping.tab[mapping.tab[,"mz"] %in% sub('^\\D+','', lab,perl=TRUE),"species"]

							# replace the "mz" label in the field name with the "VOC" name
							labs[labs==lab] <- as.character(this.VOC)
						}
					}
					names(data.avg.wide) <- labs


					cat("6a\n")
					
					#
					# make the mz fields long format
					#
					tmp.CONC     <- stack(data.avg.wide[,lab.VOC])									# stack the mz fields
					tmp.PERC     <- stack((data.avg.wide[,lab.VOC]/apply(data.avg.wide[,lab.VOC_all],1,sum,na.rm=TRUE))*100)	# convert Conc into % and then stack them

					names(tmp.CONC) <- c("Conc","VOC")		# rename the default "value" and "ind" to "Conc" and "VOC"
					names(tmp.PERC) <- c("Perc","VOC")		# rename the default "value" and "ind" to "Perc" and "VOC"

					# make the other fields consistent with the mz long format
					tmp.other  <- NA
					for (idx.VOC in lab.VOC)
					{
						tmp.other <- rbind(tmp.other,data.avg.wide[,lab.other])
					}
					tmp.other <- tmp.other[-1,]			# remove the first NA row which is for initialization

					#
					# make the long format for the data subset 
					#
					data.Conc.long <- cbind(tmp.other,tmp.CONC)
					data.Perc.long <- cbind(tmp.other,tmp.PERC)


					#### #
					#### # add the time period Carl Identified on August 7, 2009
					#### #
					#### time.period <- rep("NA",dim(data.Conc.long)[1])
					#### time.period[data.Conc.long[,"Day"]>=4  & data.Conc.long[,"Day"]<=8]  <- "Period1" 
					#### time.period[data.Conc.long[,"Day"]>=9  & data.Conc.long[,"Day"]<=11] <- "Period2"
					#### time.period[data.Conc.long[,"Day"]>=15 & data.Conc.long[,"Day"]<=18] <- "Period3"
					#### time.period[data.Conc.long[,"Day"]>=25 & data.Conc.long[,"Day"]<=27] <- "Period4"					
					#### 
					#### #
					#### # add the period identified by Carl as an additional column of the data objects [data.Conc.long], [data.Perc.long]
					#### #
					#### data.Conc.long <- data.frame(data.Conc.long[time.period != "other",],time.period = time.period[time.period != "other"])
					#### data.Perc.long <- data.frame(data.Perc.long[time.period != "other",],time.period = time.period[time.period != "other"])
					#### 
					#### 
					#### #
					#### # convert "Day" to factor
					#### #
					#### if (!is.factor(data.Conc.long[,"Day"])){data.Conc.long[,"Day"] <- factor(data.Conc.long[,"Day"],levels=day.label,labels=day.names)}
					#### if (!is.factor(data.Perc.long[,"Day"])){data.Perc.long[,"Day"] <- factor(data.Perc.long[,"Day"],levels=day.label,labels=day.names)}

					#
					# replacing NA with 0 for plotting purpose
					#
					data.Conc.long[is.na(data.Conc.long)] <- 0
					data.Perc.long[is.na(data.Perc.long)] <- 0
					# *************************************************************************************
					# the long format of the aggregated data object is [data.Conc.long] & [data.Perc.long]
					# *************************************************************************************

					#### #
					#### # output the processed data in object file
					#### #
					#### save(data.4plot,data.Conc.wide,data.avg.wide,data.Conc.long,data.Perc.long,file = FL.SUM.OBJ)
					#### 
					#### #
					#### # output the processed data in CSV format
					#### #			
					#### cat("\n\n(",avg.opt,") data loaded,",file=FL.SUM.CSV,append=TRUE)
					#### write.table(data.4plot,file = FL.SUM.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
					#### 
					#### cat("\n\n(",avg.opt,") time split data,",file=FL.SUM.CSV,append=TRUE)
					#### write.table(data.Conc.wide,file = FL.SUM.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
					#### 
					#### cat("\n\n(",avg.opt,") aggregated data,",file=FL.SUM.CSV,append=TRUE)
					#### write.table(data.avg.wide,file = FL.SUM.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)

					#### cat("\n\n(",avg.opt,") data for conc barchart,",file=FL.SUM.CSV,append=TRUE)
					#### write.table(data.Conc.long,file = FL.SUM.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
					#### 
					#### cat("\n\n(",avg.opt,") data for perc barchart,",file=FL.SUM.CSV,append=TRUE)
					#### write.table(data.Perc.long,file = FL.SUM.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)


					#
					#
					#
					if (plot.type == "Conc")
					{
						ylab1 <- "Mixing Ratio (ppb)"
						ylab2 <- "%"
						main1 <- paste(paste("(",lab.time," ",name.measure,")",sep="")," concentration of ",paste(lab.VOC,collapse=", "),sep="")
						main2 <- paste("% ",paste("(",lab.time," ",name.measure,")",sep="")," concentration of ",paste(lab.VOC,collapse=", "),sep="")
					}
					if (plot.type == "carbonConc")
					{

						ylab1 <- "Mixing Ratio (ppbC)"
						ylab2 <- "%"
						main1 <- paste(paste("(",lab.time," ",name.measure,")",sep="")," concentration of ",paste(lab.VOC,collapse=", "),sep="")
						main2 <- paste("% ",paste("(",lab.time," ",name.measure,")",sep="")," concentration of ",paste(lab.VOC,collapse=", "),sep="")
					}

					if (plot.type == "OHreactivity")
					{
						ylab1 <- expression(paste("(10"^{11},") x [","cm"^{3},"molecule"^{-1},"s"^{-1},"] at 298K and 1atm",sep=""))
						ylab2 <- "%"
						main1 <- paste(paste("(",lab.time," ",name.measure,")",sep="")," OH-rate of ",paste(lab.VOC,collapse=", "),sep="")
						main2 <- paste("% ",paste("(",lab.time," ",name.measure,")",sep="")," OH-rate of ",paste(lab.VOC,collapse=", "),sep="")
					}
					
					cat("6b\n")

					#
					# Making bar chart on concentration
					#
					no.VOC    <- nlevels(data.Conc.long[,"VOC"])
					no.SITE   <- 3
					col.array <- col.array.complete[1:no.VOC]
					plot.obj  <- barchart(Conc ~ time.period | Site, data = data.Conc.long,
							      groups = VOC, layout = c(1,no.SITE), stack = TRUE, 
							      key = list(text=list(levels(data.Conc.long[,"VOC"]),cex=1,col=col.array),
							      		 rectangles=list(col=col.array), space = "top",columns=4),
							      ylab = list(label=ylab1,cex=2),
							      xlab = list(label="Time Period of September 2006",cex=2),
							    # main = list(label=main1,cex=2),
							      main = "",
							      col=col.array,
							      scales = list(x = list(rot = 45),cex=2),
							      as.table = TRUE,
							      between=list(y=1.5),
							      par.strip.text = list(cex = 1),
							      strip=strip.custom(strip.levels=TRUE,strip.names=TRUE),cex=2)
					plot(plot.obj)
					
					
					# save an individual JPG file
					FL.PNG <- paste(Path.plot,paste("Figure2.png",sep=""),sep="/")	
					if (file.exists(FL.PNG)){print(paste(FL.PNG," exist. Delete it!"));file.remove(FL.PNG)}
					png(filename = FL.PNG,width = 1024, height = 1024, units = "px", pointsize = 12)
						plot(plot.obj)	
					dev.off()     		


					cat("6c\n")
					#### #
					#### # Making bar chart on percentage
					#### #
					#### no.VOC <- nlevels(data.Perc.long[,"VOC"])
					#### no.SITE   <- 3
					#### col.array <- col.array.complete[1:nlevels(data.Perc.long[,"VOC"])]
					#### plot.obj  <- barchart(Perc ~ time.period | Site, data = data.Perc.long,
					#### 		      groups = VOC, layout = c(1,no.SITE), stack = TRUE, 
					#### 		      key = list(text=list(levels(data.Perc.long[,"VOC"]),col=col.array),
					#### 				 rectangles=list(col=col.array,col=col.array), space = "top",columns=no.column),
					#### 		      ylab = ylab2,
					#### 		      xlab = "Time Period of September 2006",
					#### 		      main = main2,
					#### 
					#### 		      col=col.array,
					#### 		      scales = list(x = list(rot = 45)),
					#### 		      as.table = TRUE,
					#### 		      between=list(y=0.5),
					#### 		      strip=strip.custom(strip.levels=TRUE,strip.names=TRUE))
					#### plot(plot.obj)
					
					cat("6b\n")					
				}	# end of the aggregation measure loop
				
				cat("4c\n")
			}		# end of time split loop
			
			cat("6b\n")
			
			dev.off()
		}	# end of plot type (raw, carbonConc or OHrate) loop
	}		# end of set loop
}			# end of avg option loop

# ---------------------------------------------------------------------------------------------------
# 40. time used for completing this script
# ---------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("Figure2_42_barchart_PTRMS_4episode.R is finished successfully!\n",sep=" "))
cat(paste("Figure2_42_barchart_PTRMS_4episode.R is finished successfully!\n",sep=" "),file=FL.LOG,append=TRUE)


cat(paste("Processing time for Figure2_42_barchart_PTRMS_4episode.R is",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)


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

	    
