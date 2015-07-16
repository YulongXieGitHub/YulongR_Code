#
# 9c_PTRMS_gas.R
#
#
# Revised on July 17, 2009  by created a new folder "/Analysis_Revised/" and removed zeros from the data before any analysis
#
# INPUT
# "../7a_merge4sites/avg_5minu"	
# "../9a_gasDataProcess"		
#
# OUTPUT
# "../9c_PTRMS_gas"
# figures of correlated pairs of PTRMS and gaseous species
#
# -------------------------------------------------------------------------
# this script is analyze PTRMS data (5 minute averaged data) and Gaseous data together
# Only use CO at "Long" for "Bayland", i.e., take "CO.Bayland1" and "CO.Bayland".
#
# -----------------------------------------------------------------------------------------------------------
#
# 9c_PTRMS_gas.R (CST time which is CDT + 1)  (or CDT = CST - 1)
#
# create on Oct 28, 2008
# Revised on Nov 4, 2008
# 
# November 13, 2008
# Re-organize the file structure:
# up to this date are based on data with negative values retained.
# all analyses up to this date are saved in the directory of "\Analysis_withNegativeRetained\"
#
# A new folder "\Analysis_Revised\" is created on this date to accomodate the analysis when negative values are deleted!!
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
gases     <- c("CO","NOx","NOy","O3")

# -----------------------------------------------------------------------------------------------------------
# 3. mz and chemical mapping table for PTRMS data
# -----------------------------------------------------------------------------------------------------------
mapping.tab <- data.frame(mz      = c( 33,       35,                  42,            43,       45,            47,       57,                59,                61,           63,                69,        71,        73,                                79,       93,       95,      105,      107,          109,      121,          135,          137),
                          species = c("methanol","hydrogen-sulphide","Acetonitrile","Propene","Acetaldehyde","ethanol","Butenes+acrolein","Acetone+propanal","Acetic-acid","Dimethylsulphide","Isoprene","MVK+MACR","2-butanone+butanal+methylglyoxal","Benzene","Toluene","phenol","styrene","C2-benzenes","cresols","C3-Benzenes","C4-benzenes","Mono-terpenes"))


# 
# 4. change to the script directory
# 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_Revised/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2008_TCEQ/Analysis_Revised/0_scripts"
}
setwd(Path.Current)


# ---------------------------------------------------------------------------------------------------
# 5. define a summary function
# ---------------------------------------------------------------------------------------------------
source(paste(Path.Current,"my_functions.R",sep="/"))

# --------------------------------------------------------------------------------------------------- 
# 6. setup output and log directory
# --------------------------------------------------------------------------------------------------- 
Path.PTRMS <- "../7a_merge4sites/avg_5minu"	# INPUT of PTRMS data average on 1 hour interval
Path.in    <- "../9a_gasDataProcess"		# INPUT processed result directory
Path.out   <- "../9c_PTRMS_gas"			# OUTPUT processed result directory
Path.log   <- "../0_log"			# OUTPUT log  directory

if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ---------------------------------------------------------------------------------------------------
# 7. create a LOG file and a TIME Recording file
# ---------------------------------------------------------------------------------------------------
FL.TIME     <- paste(Path.log,"time.log",sep="/")			# OUTPUT Time Log file for all scripts
FL.LOG      <- paste(Path.log,"9c_PTRMS_gas.log",sep="/")		# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG, " exist. Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file of \"9c_PTRMS_Gas.R\" has been setup!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("Log file of \"9c_PTRMS_Gas.R\" has been setup!\n",sep=""))

# ---------------------------------------------------------------------------------------------------
# 8. create other output files
# ---------------------------------------------------------------------------------------------------
FL.Merged.OBJ <- paste(Path.out,"merged_gas_PTRMS.Rdata",sep="/")	# OUTPUT merged data
FL.corr.tab   <- paste(Path.out,"corr_tab_gas_PTRMS.csv",sep="/")	# OUTPUT correlation coefficient table

if (file.exists(FL.Merged.OBJ)){print(paste(FL.Merged.OBJ, " exist. Delete it!"));file.remove(FL.Merged.OBJ)}
if (file.exists(FL.corr.tab)){print(paste(FL.corr.tab, " exist. Delete it!"));file.remove(FL.corr.tab)}

cat(paste("\nDefined output files!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\nDefined output files!\n",sep=""))

# --------------------------------------------------------------------------------------------------- 
# 9.define INPUT files
# ---------------------------------------------------------------------------------------------------
# create a R object for output
FL.OBJ   <- paste(Path.in,paste("DataGaseous.Rdata",sep=""),sep="/")	# INPUT Rdata
FL.PTRMS <- paste(Path.PTRMS,"mergedData_avg_5minu.Rdata",sep="/")	# INPUT PTRMS data

# ---------------------------------------------------------------------------------------------------
# 10. load gaseous data
# ---------------------------------------------------------------------------------------------------
load(FL.OBJ)

# a. first change "chem" from factor to character
data.gas.wide[,"dates"] <- as.character(data.gas.wide[,"dates"])
data.gas.wide[,"times"] <- as.character(data.gas.wide[,"times"])

# b. only consider CO of Bayland taken from "Long", i.e., CO.Bayland1
data.gas.wide <- subset(data.gas.wide,select=-CO.Bayland2,drop=FALSE)


# c. add chron date to the gaseous data
data.gas.wide <- data.frame(date.chron = chron(dates=data.gas.wide[,"dates"],times=data.gas.wide[,"times"],format=c('m/d/y','h:m:s')),	
                            data.gas.wide)

# d. split into each site
data.Aldine.gas          <- subset(data.gas.wide,select=c("date.chron","dates","times","year","month","day","hour","minute","second",grep("Aldine",names(data.gas.wide),value=TRUE,perl=TRUE)))
names(data.Aldine.gas)   <- c("date.chron","dates","times","year","month","day","hour","minute","second",sub("\\.\\D+\\d?","",grep("Aldine",names(data.gas.wide),value=TRUE,perl=TRUE),perl=TRUE))

data.Bayland.gas         <- subset(data.gas.wide,select=c("date.chron","dates","times","year","month","day","hour","minute","second",grep("Bayland",names(data.gas.wide),value=TRUE,perl=TRUE)))
names(data.Bayland.gas)  <- c("date.chron","dates","times","year","month","day","hour","minute","second",sub("\\.\\D+\\d?","",grep("Bayland",names(data.gas.wide),value=TRUE,perl=TRUE),perl=TRUE))

data.DeerPark.gas        <- subset(data.gas.wide,select=c("date.chron","dates","times","year","month","day","hour","minute","second",grep("DeerPark",names(data.gas.wide),value=TRUE,perl=TRUE)))
names(data.DeerPark.gas) <- c("date.chron","dates","times","year","month","day","hour","minute","second",sub("\\.\\D+\\d?","",grep("DeerPark",names(data.gas.wide),value=TRUE,perl=TRUE),perl=TRUE))

cat(paste("\nGaseous data are loaded and split!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\nGaseous data are loaded and split!\n",sep=""))


# ---------------------------------------------------------------------------------------------------
# 11. load PTRMS data
# ---------------------------------------------------------------------------------------------------
load(FL.PTRMS)
cat(paste("\nPTRMS data are loaded and split!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\nPTRMS data are loaded and split!\n",sep=""))

# ---------------------------------------------------------------------------------------------------
# 12. loopping through site to merged the data and do the correlation analysis
# ---------------------------------------------------------------------------------------------------
for (site in sites)
{
	cat(paste("\nStart correlate PTRMS and Gaseous data at (",site,")!\n",sep=""),file=FL.LOG, append=TRUE)
	cat(paste("\nStart correlate PTRMS and Gaseous data at (",site,")!\n",sep=""))

	# -------------------------------------------------------------------------------------------
	# 13. merge PTRMS and Gaseous data
	# -------------------------------------------------------------------------------------------
	# a. merge the two data block according to the sampling date/time
	if (site == "Aldine"){data.merged.wide <- merge(data.Aldine.avg,data.Aldine.gas,by.x="CST.chron",by.y="date.chron",all.x=TRUE,all.y=TRUE)}
	if (site == "Bayland"){data.merged.wide <- merge(data.Bayland.avg,data.Bayland.gas,by.x="CST.chron",by.y="date.chron",all.x=TRUE,all.y=TRUE)}
	if (site == "DeerPark"){data.merged.wide <- merge(data.DeerPark.avg,data.DeerPark.gas,by.x="CST.chron",by.y="date.chron",all.x=TRUE,all.y=TRUE)}
	cat(paste("\nmerge PTRMS and Gaseous data at (",site,")!\n",sep=""),file=FL.LOG, append=TRUE)
	cat(paste("\nmerge PTRMS and Gaseous data at (",site,")!\n",sep=""))

	
	# b. get the list of gases and the list of mzs at current site
	lab.gas <- c("CO","NOx","NOy","O3")
	lab.mzs <- grep("mz",names(data.merged.wide),value=TRUE,perl=TRUE)
	lab.uni <- union(lab.gas,lab.mzs)

	# c. remove sample when both PTRMS and gases are empty
	data.tmp      <- subset(data.merged.wide,select=lab.uni,drop=FALSE)	# only the PTRMS and gases data
	blank.samp    <- apply(data.tmp,1,function(x) sum(is.na(x))==length(x))	# LOGIC: if all mzs and gases are NA, means sample is blank
	blank.samp.no <- sum(blank.samp[blank.samp])				# numble of blank sample
	cat(paste("\n there are ",blank.samp.no," blank samples at (",site,")\n",sep=""),file=FL.LOG, append=TRUE)
	cat(paste("\n there are ",blank.samp.no," blank samples at (",site,")\n",sep=""))

	if (blank.samp.no>0)							# if there are blank samples, delete them
	{
		blank.samp.idx <- seq(1:length(blank.samp))[blank.samp]		# index of blank sample

		# remove blank sample from data
		data.merged.wide <- data.merged.wide[-(blank.samp.idx),]	# MPTRMS & Gaseous data after blank samples removed
		row.names(data.merged.wide) <- seq(1:dim(data.merged.wide)[1])	# this is necessary, otherwise the default will be the row index of the data before the deletion
	}
	no.sample.nonBlk <- dim(data.merged.wide)[1]				# number of samples after deleted blank samples
	cat(paste("\n there are ",no.sample.nonBlk," samples after ",blank.samp.no," blank samples deleted at (",site,")!\n",sep=""),file=FL.LOG, append=TRUE)
	cat(paste("\n there are ",no.sample.nonBlk," samples after ",blank.samp.no," blank samples deleted at (",site,")!\n",sep=""))

	# d. sort the merged data
	o <- order(data.merged.wide[,"CST.chron"])
	data.merged.wide <- data.merged.wide[o,]
	cat(paste("\nwide version merged data are sorted according to date/time at (",site,")!\n",sep=""),file=FL.LOG, append=TRUE)
	cat(paste("\nwide version merged data are sorted according to date/time at (",site,")!\n",sep=""))

	# e: prepare to make a long format
	tmp.gas      <- stack(data.merged.wide[,lab.gas])					# stack the 4 gas
	tmp.gas.part <- data.frame(gas.value = rep(tmp.gas[,"values"],length(lab.mzs)),		# repeat the stacked gaste number of MZs
				   gas.name  = rep(tmp.gas[,"ind"],length(lab.mzs)))

	tmp.Month  <- rep(rep(data.merged.wide[,"month"],length(lab.gas)),length(lab.mzs))	# repeat the "Month" number of VOCs times and them number of MZs times                             
	tmp.Day    <- rep(rep(data.merged.wide[,"day"],length(lab.gas)),length(lab.mzs))	# repeat the "Day" number of VOCs times and them number of MZs times
	tmp.Year   <- rep(rep(data.merged.wide[,"year"],length(lab.gas)),length(lab.mzs))	# repeat the "Year" number of VOCs times and them number of MZs times
	tmp.Hour   <- rep(rep(data.merged.wide[,"hour"],length(lab.gas)),length(lab.mzs))	# repeat the "Hour" number of VOCs times and them number of MZs times                             
	tmp.Minute <- rep(rep(data.merged.wide[,"minute"],length(lab.gas)),length(lab.mzs))	# repeat the "Minute" number of VOCs times and them number of MZs times
	tmp.Second <- rep(rep(data.merged.wide[,"second"],length(lab.gas)),length(lab.mzs))	# repeat the "Second" number of VOCs times and them number of MZs times

	tmp.PTRMS.value  <- NULL
	tmp.PTRMS.name   <- NULL
	for (idx.mzs in lab.mzs)
	{
		tmp.PTRMS.value <- c(tmp.PTRMS.value,rep(data.merged.wide[,idx.mzs],length(lab.gas)))
		tmp.PTRMS.name  <- c(tmp.PTRMS.name,rep(rep(idx.mzs,dim(data.merged.wide)[1]),length(lab.gas)))
	}

	# f. construct the long format of the data
	data.merged.long <- data.frame(Month        = tmp.Month,
				       Day          = tmp.Day,
				       Year         = tmp.Year,
				       Hour         = tmp.Hour,
				       Minute       = tmp.Minute,
				       Second       = tmp.Second,
				       gas.value    = tmp.gas.part[,"gas.value"],
				       gas.name     = tmp.gas.part[,"gas.name"],
				       PTRMS.value  = tmp.PTRMS.value,
				       PTRMS.name   = tmp.PTRMS.name)

	 rm(tmp.gas,tmp.gas.part,tmp.Day,tmp.Hour,tmp.Minute,tmp.Month,tmp.PTRMS.name,tmp.PTRMS.value,tmp.Second,tmp.Year)

	# g. convert the factors "gas.name" and "PTRMS.name" into characters.
	data.merged.long[,"gas.name"]   <- as.character(data.merged.long[,"gas.name"])
	data.merged.long[,"PTRMS.name"] <- as.character(data.merged.long[,"PTRMS.name"])
	cat(paste("\nlong version merged data at (",site,")!\n",sep=""),file=FL.LOG, append=TRUE)
	cat(paste("\nlong version merged data at (",site,")!\n",sep=""))

	# -------------------------------------------------------------------------------------------
	# 14. calculate correlation coefficients
	# -------------------------------------------------------------------------------------------
	# a. split the data into a gas and a PTRMS blockes for correlation coefficients calculation
	data.gas   <- subset(data.merged.wide,select=lab.gas)
	data.PTRMS <- subset(data.merged.wide,select=lab.mzs)
	
	# b. calculate correlation coefficients
	corr.coef.pearson  <- cor(data.gas,data.PTRMS,use = "pairwise.complete.obs",method = c("pearson"))
	corr.coef.spearman <- cor(data.gas,data.PTRMS,use = "pairwise.complete.obs",method = c("spearman"))
	
	# c. sort the corr.coef.pearson and record the value of the 10th largest one (only used for the situation when no highly correlated pairs exist, we will plot the 10 largest corelated pairs instead no matter how big/small the correlations are)
	corr.coef.sorted <- sort(abs(corr.coef.pearson),decreasing=TRUE)
	largest10 <- corr.coef.sorted[10]
	
	# d. trun it to data frame (has to be after the sorting)
	corr.coef.pearson  <- data.frame(corr.coef.pearson)
	corr.coef.spearman <- data.frame(corr.coef.spearman)

	# e. write out the correlation between gaseous species and PTRMS
	cat(paste("\n(",site,") -------------------------- correlation coefficients between PTRMS and gaseous species --------------------------\n",sep=""),	
		    file=FL.corr.tab,
		    append=TRUE)

	cat(paste("\n(Pearson-",site,") ,",sep=""),file=FL.corr.tab,append=TRUE)	# this is to put something in the first entry of the headline (note, no newline)	
	write.table(corr.coef.pearson, 
		    file = FL.corr.tab,      
		    sep = ",", 
		    col.names = TRUE, 
		    row.names = TRUE,
		    append = TRUE)
	cat(paste("\n(",site,")  Pearson correlation coefficients between PTRMS and gaseous species are written out to a file ",FL.corr.tab,"\n\n",sep=""))	


	cat(paste("\n(Spearman-",site,") ,",sep=""),file=FL.corr.tab,append=TRUE)	# this is to put something in the first entry of the headline (note, no newline)	
	write.table(corr.coef.spearman, 
		    file = FL.corr.tab,      
		    sep = ",", 
		    col.names = TRUE, 
		    row.names = TRUE,
		    append = TRUE)
	cat(paste("\n(",site,")  Spearman correlation coefficients between PTRMS and gaseous species are written out to a file ",FL.corr.tab,"\n\n",sep=""))	


	# NOTE: the truncation of the corr coeff matrix is conducted ONLY on pearson coefficients
	# f. display the largest 50 (100/2) pairs of the largest correlated pairs
	if(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.99]))<100)
	{
		corr.coef.trun <- corr.coef.pearson; corr.coef.trun[abs(corr.coef.trun)<0.99] <- NA
		print(paste(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.99]))," pairs with r>0.99;  Only plot those pairs with corr.coef.pearson larger than 0.99\n"))		
	}else{
		corr.coef.trun <- corr.coef.pearson; corr.coef.trun[abs(corr.coef.trun)<1.00] <- NA
		print(paste(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)==1.00]))," pairs with r=1.00;  Only plot those pairs with corr.coef.pearson larger than 1.00\n"))		
	}

	if(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.98]))<100)
	{
		corr.coef.trun <- corr.coef.pearson; corr.coef.trun[abs(corr.coef.trun)<0.98] <- NA
		print(paste(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.98]))," pairs with r>0.98;  Only plot those pairs with corr.coef.pearson larger than 0.98\n"))		
	}


	if(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.95]))<100)
	{
		corr.coef.trun <- corr.coef.pearson; corr.coef.trun[abs(corr.coef.trun)<0.95] <- NA
		print(paste(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.95]))," pairs with r>0.95;  Only plot those pairs with corr.coef.pearson larger than 0.95\n"))		
	}

	if(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.90]))<100)
	{
		corr.coef.trun <- corr.coef.pearson; corr.coef.trun[abs(corr.coef.trun)<0.90] <- NA
		print(paste(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.90]))," pairs with r>0.90;  Only plot those pairs with corr.coef.pearson larger than 0.90\n"))		
	}

	if(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.80]))<100)
	{
		corr.coef.trun <- corr.coef.pearson; corr.coef.trun[abs(corr.coef.trun)<0.80] <- NA
		print(paste(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.80]))," pairs with r>0.80;  Only plot those pairs with corr.coef.pearson larger than 0.90\n"))		
	}

	if(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.70]))<100)
	{
		corr.coef.trun <- corr.coef.pearson; corr.coef.trun[abs(corr.coef.trun)<0.70] <- NA
		print(paste(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>0.70]))," pairs with r>0.70;  Only plot those pairs with corr.coef.pearson larger than 0.90\n"))		
	}

	# if no pair retained so far, return the 10 largest correlated pairs
	if(sum(corr.coef.trun,na.rm=TRUE)==0)
	{
		corr.coef.trun <- corr.coef.pearson
		corr.coef.trun[abs(corr.coef.trun)<largest10] <- NA
		print(paste(sum(!is.na(corr.coef.pearson[abs(corr.coef.pearson)>largest10])),"  the several pairs with largest correlation coefficients\n"))		
	}
	cat(paste("\ncorrelation coefficients are computed at (",site,")!\n",sep=""),file=FL.LOG, append=TRUE)
	cat(paste("\ncorrelation coefficients are computed at (",site,")!\n",sep=""))


	# -------------------------------------------------------------------------------------------
	# 15. plot the time series and scatter plot of the highly correlated PTRMS MZs and the gaseous species
	# -------------------------------------------------------------------------------------------
	# a. retrieve those pairs with highest correlation
	index   <- which(!is.na(corr.coef.trun))
	idx.col <- ceiling(index / dim(corr.coef.trun)[1])
	idx.row <- index - (idx.col-1)*dim(corr.coef.trun)[1]

	# b. plot the retained pairs
	cat(paste("\n(",site,") create PTRMS and gaseous species scatterplot at (",site,")\n",sep=""),file=FL.LOG,append=TRUE)
	cat(paste("\n(",site,") create PTRMS and gaseous species scatterplot at (",site,")\n",sep=""))
	
	# c. define PDF file for current site for scatter plot plotting
	FL.corrpair.pdf <- paste(Path.out,paste("gas_PTRMS_corrpair_",site,".pdf",sep=""),sep="/")	# OUTPUT Scatter Plot in PS file
	TI.corrpair.pdf <- paste("gas_PTRMS_corrpair_",site,".pdf",sep="")				# OUTPUT Scatter Plot in PS file
	if (file.exists(FL.corrpair.pdf)){print(paste(FL.corrpair.pdf, " exist. Delete it!"));file.remove(FL.corrpair.pdf)}

	# d. open the PDF file  
	pdf(file = FL.corrpair.pdf,title=TI.corrpair.pdf,paper="a4r", width=0, height=0)

	cat(paste("\n(",site,")  scatter plots are generated for the following PTRMS and gaseous species pairs for (",site,")\n",sep=""),file=FL.LOG,append=TRUE)	
	PAIR_NAME1 <- NULL
	PAIR_NAME2 <- NULL
	PAIR_CORR  <- NULL
	for (idx in seq(along=idx.col))
	{		
		# aa. correlation coefficient value between the time series of the PTRMS mz and the gaseous species
		cor.pearson  <- as.character(corr.coef.trun[idx.row[idx],idx.col[idx]])
		cor.spearman <- as.character(corr.coef.spearman[idx.row[idx],idx.col[idx]])

		# bb. only keep several decimals of the correlation coefficients
		if (!is.na(cor.pearson) & (cor.pearson != "1"))	# the reason for this condition is that: when there is only one pair of data, R will be "NA"
		{
			ltrs <- substring(cor.pearson,1:nchar(cor.pearson),1:nchar(cor.pearson))
			cor.pearson <- substr(cor.pearson,1,which(ltrs==".")+3)	
			
			ltrs <- substring(cor.spearman,1:nchar(cor.spearman),1:nchar(cor.spearman))
			cor.spearman <- substr(cor.spearman,1,which(ltrs==".")+3)				
		}

		# cc.  get the name of the PTRMS mzs and the gaseous species
		name1 <- row.names(corr.coef.trun)[idx.row[idx]]	# name of gaseous species VOC
		name2 <-     names(corr.coef.trun)[idx.col[idx]]	# code of PTRMS mz
		chem2 <- mapping.tab[mapping.tab[,"mz"]==sub('^\\D+','',name2,perl=TRUE),"species"]	# name of PTRMS mz

		#####  
		# dd. Since I did not find how to use dual axes for xyplot, I code the way below but could not find how to turn the scatter plot into square (pty="s" does not work.
		# par(mfrow=c(2,1),mar=c(5,4,4,4)+0.3)
		#
		# instead of use mfrow above, use layout to control (see RGraphics, page 82) Nov 14, 2008
		#
		par(mar=c(5, 4, 4, 5)) 
		layout(rbind(c(1,1,1),
			     c(0,2,0)),
			     respect=rbind(FALSE,TRUE))	

		plot(data.merged.wide[,"CST.chron"],data.merged.wide[,name1],
		     type="l",lty=1,lwd=0.15,col="red",col.lab="red",
		     xlab="date/time (CST)",
		     ylab=paste("Conc(ppbv) ",name1,sep=""),
		     main=paste("(",site,")  Corr coef of ",name1," and ",chem2,"(",name2,") is ",cor.pearson,"|",cor.spearman,sep=""))
		par(new=TRUE)
		plot(data.merged.wide[,"CST.chron"],data.merged.wide[,name2],
		     type="l",lty=1,lwd=0.15,col="blue",col.lab="blue",
		     axes=FALSE,bty="n",
		     xlab="",ylab="")
		axis(side=4,at=pretty(range(data.merged.wide[,name2],na.rm=TRUE)))
		mtext(col="blue",paste("Conc(ppbv) ",chem2,"(",name2,")",sep=""),side=4,line=3)
		
		plot(data.merged.wide[,name1],data.merged.wide[,name2],
		     type="p",pch=".",col="red",cex=4,
		     pty="s",
		     xlab=name1,ylab=paste(chem2,"(",name2,")",sep=""),
		     main=paste("(",site,")  scatter plot of ",name1," vs ",chem2,"(",name2,")",sep=""))
		
		#####  # Since I did not find how to use dual axes for xyplot, I code the way above, but could not find how to turn the scatter plot into square (pty="s" does not work.
		#####  key.4plot <- list(space = "top", 
		##### 		  columns=2,
		##### 		  text  = list(c(name1,paste(chem2,"(",name2,")",sep=""))),
		##### 		 #####  points= list(pch=1:6,col=c("black","red","blue","green","magenta","cyan","pink")),
		##### 		 lines = list(lty=1:2,col=c("red","blue")))		
		##### plot.obj1 <- xyplot(as.numeric(data.merged.wide[,name1]) + as.numeric(data.merged.wide[,name2]) ~ data.merged.wide[,"CST.chron"],
		##### 		    col=c("red","blue"),
		##### 		    type=c("l","l"),
		##### 		    lty=c(1,2),
		##### 		    xlab="date/time",ylab="Conc(ppbv)",
		##### 		    key = key.4plot,
		##### 		    main=paste("(",site,")  Corr coef of ",name1," and ",chem2,"(",name2,") is ",cor.pearson,"|",cor.spearman,sep=""))
		##### plot.obj2 <- xyplot(as.numeric(data.merged.wide[,name2]) ~ as.numeric(data.merged.wide[,name1]),
		##### 		    pch=".",col="black",cex=3,
		##### 		    xlab=name1,ylab=paste(chem2,"(",name2,")",sep=""),
		##### 		    aspect=1,
		##### 		    main=paste("(",site,")  scatter plot of ",name1," vs ",chem2,"(",name2,")",sep=""))
		##### plot(plot.obj1,split=c(1,1,1,2))
		##### plot(plot.obj2,split=c(1,2,1,2),newpage=FALSE)

		PAIR_NAME1 <- c(PAIR_NAME1,name1)
		PAIR_NAME2 <- c(PAIR_NAME2,paste(chem2,"(",name2,")",sep=""))
		PAIR_CORR  <- rbind(PAIR_CORR,c(cor.pearson,cor.spearman))
	}
	
	# e. output the correlation coefficients of the highly correlated pairs
	cat(paste("\n\n\n(",site,")",length(idx.col)," gaseous species and PTRMS pairs with highest correlation coefficients ",FL.corr.tab,"\n\n",sep=""),append=TRUE)	

	PAIR_OUT <- data.frame(PAIR_CORR,name1=PAIR_NAME1,name2=PAIR_NAME2)
	names(PAIR_OUT) <- c("corr.Pearson","corr.Spearman","name1","name2")

	cat(paste(length(idx.col)," highly correlated pairs,",sep=""),file=FL.corr.tab,append=TRUE)	
	write.table(PAIR_OUT, 
		    file = FL.corr.tab,      
		    sep = ",", 
		    col.names = TRUE, 
		    row.names = TRUE,
		    append = TRUE)

	cat(paste("\n(",site,")  high correlated mz pairs are plotted in ",FL.corrpair.pdf,sep=""))	
	dev.off() 
	cat(paste("\n(",site,") PTRMS and gaseous species scatterplot created at (",site,")\n",sep=""),file=FL.LOG,append=TRUE)
	cat(paste("\n(",site,") PTRMS and gaseous species scatterplot created at (",site,")\n",sep=""))
	
	# -------------------------------------------------------------------------------------------
	# 16. reassign names to the merged long format (for output only)
	# ------------------------------------------------------------------------------------------- 
	if (site == "Aldine")
	{
		data.Aldine.long <- data.merged.long
		data.Aldine.wide <- data.merged.wide
	}
	if (site == "Bayland")
	{
		data.Bayland.long <- data.merged.long
		data.Bayland.wide <- data.merged.wide
	}
	if (site == "DeerPark")
	{
		data.DeerPark.long <- data.merged.long
		data.DeerPark.wide <- data.merged.wide
	}	
}	# end of the site loop




# 19. save the merged data
save(data.Aldine.wide,data.Aldine.long,data.Bayland.wide,data.Bayland.long,data.DeerPark.wide,data.DeerPark.long,file = FL.Merged.OBJ)
cat(paste("gas and PTRMS data were merged atAldine, Bayland and DeerPark,",sep=""),file=FL.LOG,append=TRUE)
cat(paste("gas and PTRMS data were merged atAldine, Bayland and DeerPark,",sep=""))


cat(paste("all important data objects are saved in ",FL.Merged.OBJ,"\n",sep=""),file=FL.LOG,append=TRUE)


#
# 20. List of data objects saved in the R object file
#
list.object <- c("data.Aldine.wide","data.Aldine.long","data.Bayland.wide","data.Bayland.long","data.DeerPark.wide","data.DeerPark.long")
cat(paste("\n The following data objects are saved in the R object file (",FL.Merged.OBJ,")\n\n\n\n\n",sep=""))
cat(paste("\n The following data objects are saved in the R object file (",FL.Merged.OBJ,")\n\n\n\n\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste(list.object,collapse="\n"))
cat(paste(list.object,collapse="\n"),file=FL.LOG,append=TRUE)


# ---------------------------------------------------------------------------------------------------
# 40. time used for completing this script
# ---------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("9c_PTRMS_Gas.R is finished successfully!\n",sep=" "))
cat(paste("9c_PTRMS_Gas.R is finished successfully!\n",sep=" "),file=FL.LOG,append=TRUE)


cat(paste("Processing time for 9c_PTRMS_Gas.R is",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),
	    file=FL.TIME,
	    append=TRUE)


