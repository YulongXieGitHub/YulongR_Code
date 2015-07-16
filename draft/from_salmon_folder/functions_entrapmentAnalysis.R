#
# eliminate all stuff
rm(list = ls(all = TRUE))

# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# close all devices which have been opened
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}


packages.desired   <- c("akima","bitops","caTools","chron","cshapes","cwhmisc","data.table","Defaults","fortunes","gplots","gtools","iterators","itertools","lme4","locfit","maptools","mlmRev","neuralnet","plyr","psych","quantmod","reshape","reshape2","rJava","RODBC","scatterplot3d","sp","splus2R","stringr","survey","timeDate","TTR","xts","zoo")
packages.needed    <- c("chron","RODBC","timeDate","stats","lattice","graphics","cwhmisc","reshape")
packages.loaded    <- search()						# packages already loaded
packages.available <- (unlist(library()$results))[,"Package"]		# packages installed which are ready to load
packages.libPath   <- (unlist(library()$results))[,"LibPath"][1]	# the path to install package

for (package.needed in packages.needed)
{
	if (length(grep(package.needed,packages.loaded,perl=TRUE,value=TRUE))>0)
	{
		# package needed has already been loaded
		cat(paste("Package \"",package.needed,"\" has already been loaded\n",sep=""))
	}else{
		# package needed has NOT been loaded
		if (length(grep(package.needed,packages.available,perl=TRUE,value=TRUE))<=0)
		{
			# package needed which has NOT been loaded has NOT been installed, install it
			install.packages(package.needed, 
	                                 lib       = packages.libPath, 
	                                 repos     = "http://lib.stat.cmu.edu/R/CRAN",
	                                 available = NULL, destdir = NULL,dependencies = NA, type = getOption("pkgType"),clean = FALSE)   
			cat(paste("Package \"",package.needed,"\" does not exist and has just been installed\n",sep=""))	                                 
		}
		
		# now load it
		command.string <- paste("library(",package.needed,")",sep="")
		eval(parse(text=command.string))
		cat(paste("Package \"",package.needed,"\" has just been loaded\n",sep=""))	
	}
}






# today's month, day and year in the format of "Thu Jun 16 08:48:36 2011", 5 fields separated by space
today.month  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
today.day    <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
today.year   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]
today.hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
today.minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
today.second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]

# a function took from the boot strap package
norm.inter <- function(t,alpha)
#
#  Interpolation on the normal quantile scale.  For a non-integer
#  order statistic this function interpolates between the surrounding
#  order statistics using the normal quantile scale.  See equation
#  5.8 of Davison and Hinkley (1997)
#
{
    t <- t[is.finite(t)]
    R <- length(t)
    rk <- (R+1)*alpha
    if (!all(rk>1 & rk<R))
        warning("extreme order statistics used as endpoints")
    k <- trunc(rk)
    inds <- seq_along(k)
    out <- inds
    kvs <- k[k>0 & k<R]
    tstar <- sort(t, partial = sort(union(c(1, R), c(kvs, kvs+1))))
    ints <- (k == rk)
    if (any(ints)) out[inds[ints]] <- tstar[k[inds[ints]]]
    out[k == 0] <- tstar[1L]
    out[k == R] <- tstar[R]
    not <- function(v) xor(rep(TRUE,length(v)),v)
    temp <- inds[not(ints) & k != 0 & k != R]
    temp1 <- qnorm(alpha[temp])
    temp2 <- qnorm(k[temp]/(R+1))
    temp3 <- qnorm((k[temp]+1)/(R+1))
    tk <- tstar[k[temp]]
    tk1 <- tstar[k[temp]+1L]
    out[temp] <- tk + (temp1-temp2)/(temp3-temp2)*(tk1 - tk)
    cbind(round(rk, 2), out)
}



# -------------------------------------------------------------------------------------------------
# define label for the day of the week
# -------------------------------------------------------------------------------------------------
week.label     <- c( 1,       2,       3,        4,          5,         6,       7)
week.names     <- c("Sun",   "Mon",   "Tue",    "Wed",      "Thu",     "Fri",   "Sat")
week.fullNames <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
names(week.fullNames) <- week.names



# -------------------------------------------------------------------------------------------------
# a function to calculate the variety of counts per segment:
# inputs: data subset, biweek.idx and segment list of the entire data set
# -------------------------------------------------------------------------------------------------
count.perSegment <- function(my.data,my.biweek.idx,my.allSegment)
{

	idx <- 0
	for (this.segment in sort(my.allSegment))
	{
		idx <- idx + 1
		myData.sub <- subset(my.data,segment == this.segment)
		
		if (dim(myData.sub)[1] > 0)
		{
			no.sites.visited        <- length(unique(myData.sub[,"transect"]))
			no.sites.ent.yes1       <- length(unique(myData.sub[myData.sub[,"entrapments.present"]=="yes","transect"]))	# This calculation is not correct
			no.sites.ent.no1        <- no.sites.visited - no.sites.ent.yes1							# This calculation is not correct
			no.entrapment.yes       <- dim(myData.sub[myData.sub[,"entrapments.present"]=="yes",])[1]
			no.entrapment.wt.chinook<- dim(myData.sub[myData.sub[,"fish.exist"]=="yes",])[1]				# November 28, 2012: since the replacement of "fish.present" with "fish.exist", this function will not work for the script before revision.			
			no.entrapment.unknown   <- dim(myData.sub[myData.sub[,"fate"]=="Unknown",])[1]
			no.entrapment.reflood   <- dim(myData.sub[myData.sub[,"fate"]=="Reflood",])[1]
			no.entrapment.dewatered <- dim(myData.sub[myData.sub[,"fate"]=="Dewatered",])[1]
			no.entrapment.thermal   <- dim(myData.sub[myData.sub[,"fate"]=="Temp > 27C",])[1]
			no.fish.alive           <- sum(myData.sub[,"fish.alive"],na.rm=TRUE)
			no.fish.dead            <- sum(myData.sub[,"fish.dead"], na.rm=TRUE)
			no.fish.total           <- sum(myData.sub[,"fish.total"],na.rm=TRUE)
		
			no.entrapment.lethalFate<- dim(myData.sub[myData.sub[,"lethal"] == "yes",])[1]						# number of entrapments (data points) when the entrapment fate is known as lethal
			no.entrapment.knownFate <- dim(myData.sub[((myData.sub[,"lethal"] == "yes") | (myData.sub[,"lethal"] == "no")),])[1]	# number of entrapments (data points) when the entrapment fate is known as lethal ot not lethal, i.e., is not "unknown"

			fish.mortality          <- sum(myData.sub[,"mortality"],  na.rm=TRUE)							# different from fish.dead or fish.alive.  fish.alive could be fish.mortality if the entrapment is lethal.

			fish.mortality.knownFate<- sum(myData.sub[myData.sub[,"lethal"] == "yes" | myData.sub[,"lethal"] == "no","mortality"],  na.rm=TRUE)			# different from fish.dead or fish.alive.  fish.alive could be fish.mortality if the entrapment is lethal.
			fish.total.knownFate    <- sum(myData.sub[myData.sub[,"lethal"] == "yes" | myData.sub[,"lethal"] == "no","fish.total"], na.rm=TRUE)			# different from fish.dead or fish.alive.  fish.alive could be fish.mortality if the entrapment is lethal.
			
			mortalityRate.entrapment<- no.entrapment.lethalFate / no.entrapment.knownFate
			mortalityRate.fish      <- fish.mortality.knownFate / fish.total.knownFate
			
			perc.ent.wt.chinook     <- no.entrapment.wt.chinook / no.entrapment.yes 
			chinook.per.ent         <- no.fish.total / no.entrapment.yes 
			
			
			# -------------------------------------------------------------------------------------------------
			# number of total entrapments of each unique sample (i.e., date-transect combination): count the number of all quardrants ("sampled"="Y" or "N") for a give transect at a given day 
				ent.count  <- aggregate(myData.sub[,c("entrapment.yes")],list(myData.sub[,"biweek.idx"],myData.sub[,"transect"]), FUN=length)			# the number of records of a "date-segment-transect" combination
			  names(ent.count) <- c(       "biweek.idx","transect","ent.count.all")

			# number of sampled entrapments of each unique sample (i.e., date-transect combination):  count the number of "Y" quardrants ("sampled"="Y")        for a give transect at a given day 
				ent.yes    <- aggregate(myData.sub[,c("entrapment.yes")],list(myData.sub[,"biweek.idx"],myData.sub[,"transect"]), FUN=sum,na.rm=T)		# the number of sampled "Yes" records of a "date-segment-transect" combination
			  names(ent.yes)   <- c(       "biweek.idx","transect","ent.count.yes")

			# number of not-sampled entrapments of each unique sample (i.e., date-transect combination):  count the number of "N" quardrants ("sampled"="N")        for a give transect at a given day 
				ent.no     <- aggregate(myData.sub[,c("entrapment.no")], list(myData.sub[,"biweek.idx"],myData.sub[,"transect"]), FUN=sum,na.rm=T)		# the number of sampled "Yes" records of a "date-segment-transect" combination
			  names(ent.no)    <- c(       "biweek.idx","transect","ent.count.no")
			cat(paste("count numbers of total, sampled and not-sampled entrapments in the segments\n",sep=""))
			cat(paste("count numbers of total, sampled and not-sampled entrapments in the segments\n",sep=""),file=FL.LOG,append=TRUE)

			# assemble the counts into a dataframe.  The numbers of total entrapments, sampled entrapments and not sampled entrapments are used to create the "entrapments Sampled" and "entrapments Not Sampled" statistics
			mydata.ent <- cbind(ent.count,
					    ent.count.yes = ent.yes[,"ent.count.yes"],
					    ent.count.no  = ent.no[,"ent.count.no"])                

			# assign "entrapments Sampled No":	if total entrapments == not sampled entrapments, i.e., ent.count == ent.no,  assign 1 otherwise 0  
			#        "entrapments Sampled Yes":	if total entrapments ==     sampled entrapments, i.e., ent.count == ent.yes, assign 1 otherwise 0  
			#        "entrapments Sampled YesNo":	if sampled entrapments > 0,                i.e., ent.count.yes > 0,        assign 1 otherwise 0    This is the "entrapments Sampled Yes" in the summary tab "Stranding Summary"
			mydata.ent <- cbind(mydata.ent,
					    ent.no    = rep(0,dim(mydata.ent)[1]),	# initialize "Plots Sampled : No"    with 0 
					    ent.yes   = rep(0,dim(mydata.ent)[1]),	# initialize "Plots Sampled : Yes"   with 0
					    ent.yesNo = rep(0,dim(mydata.ent)[1]))	# initialize "Plots Sampled : YesNo" with 0            
			# assign values
			mydata.ent[mydata.ent[,"ent.count.all"] == mydata.ent[,"ent.count.no"], "ent.no"]    <- 1	# the sum of sampled "no"  in the data packet is the same as the length of the data packet, means all records in the data packet are "No".
			mydata.ent[mydata.ent[,"ent.count.all"] == mydata.ent[,"ent.count.yes"],"ent.yes"]   <- 1	# the sum of sampled "yes" in the data packet is the same as the length of the data packet, means all records in the data packet are "Yes".
			mydata.ent[mydata.ent[,"ent.count.yes"] > 0,                            "ent.yesNo"] <- 1	# the sum of sampled "yes" in the data packet is not zero                                 , means at least there is sampled "Yes" records
			cat(paste("creat a data.frame of [mydata.ent]\n",sep=""))
			cat(paste("creat a data.frame of [mydata.ent]\n",sep=""),file=FL.LOG,append=TRUE)

			# -------------------------------------------------------------------------------------------------
			# count entrapments sampled "No" and "Yes"
			 no.sites.ent.yes2 <- sum(mydata.ent[,"ent.yesNo"],na.rm=TRUE)	# this is "Plots Sampled Yes" in the summary tab "Stranding Summary"
			 no.sites.ent.no2  <- sum(mydata.ent[,"ent.no"],  na.rm=TRUE)	# this is "Plots Sampled No"  in the summary tab "Stranding Summary"
			cat(paste("the count the Yes/No transect/quardrant\n",sep=""))
			cat(paste("the count the Yes/No transect/quardrant\n",sep=""),file=FL.LOG,append=TRUE)
			# -------------------------------------------------------------------------------------------------
			
			
			if (idx == 1)
			{
				output <- data.frame(segment            = this.segment,
						     sites.visited      = no.sites.visited,
						     sites.ent.yes      = no.sites.ent.yes2,
						     sites.ent.no       = no.sites.ent.no2,
						     ent.sampled        = no.entrapment.yes,
						     ent.wt.chinook     = no.entrapment.wt.chinook,
						     ent.fate.unknown   = no.entrapment.unknown,
						     ent.fate.reflood   = no.entrapment.reflood,
						     ent.fate.dewatered = no.entrapment.dewatered,
						     ent.fate.thermal   = no.entrapment.thermal,
						     fish.alive         = no.fish.alive,
						     fish.dead          = no.fish.dead,
						     fish.total         = no.fish.total,
						     sites.ent.yes1     = no.sites.ent.yes1,		
						     sites.ent.no1      = no.sites.ent.no1,
						     fish.mortality     = fish.mortality,
						     no.ent.lethal.fate = no.entrapment.lethalFate,
						     no.ent.known.fate  = no.entrapment.knownFate,
						     mort.rate.ent      = mortalityRate.entrapment,
						     fish.morts.known   = fish.mortality.knownFate,
						     fish.total.known   = fish.total.knownFate,
						     mort.rate.fish     = mortalityRate.fish,
						     perc.ent.wt.chinook= perc.ent.wt.chinook,
						     chinook.per.ent    = chinook.per.ent)		
			}else{
				output <- rbind(output,        
				                   c(segment            = this.segment,
				                     sites.visited      = no.sites.visited,
				                     sites.ent.yes      = no.sites.ent.yes2,
				                     sites.ent.no       = no.sites.ent.no2,
				                     ent.sampled        = no.entrapment.yes,
				                     ent.wt.chinook     = no.entrapment.wt.chinook,
				                     ent.fate.unknown   = no.entrapment.unknown,
				                     ent.fate.reflood   = no.entrapment.reflood,
				                     ent.fate.dewatered = no.entrapment.dewatered,
				                     ent.fate.thermal   = no.entrapment.thermal,
				                     fish.alive         = no.fish.alive,
				                     fish.dead          = no.fish.dead,
				                     fish.total         = no.fish.total,
				                     sites.ent.yes1     = no.sites.ent.yes1,		# this calculation is not correct
				                     sites.ent.no1      = no.sites.ent.no1,
				                     fish.mortality     = fish.mortality,
						     no.ent.lethal.fate = no.entrapment.lethalFate,
						     no.ent.known.fate  = no.entrapment.knownFate,
						     mort.rate.ent      = mortalityRate.entrapment,
						     fish.morts.known   = fish.mortality.knownFate,
						     fish.total.known   = fish.total.knownFate,						     
						     mort.rate.fish     = mortalityRate.fish,
						     perc.ent.wt.chinook= perc.ent.wt.chinook,
						     chinook.per.ent    = chinook.per.ent))		# this calculation is not correct	
			}
		}else{
			if (idx == 1)
			{
				output <- data.frame(segment            = this.segment,
				                     sites.visited      = 0,
				                     sites.ent.yes      = 0,
				                     sites.ent.no       = 0,
				                     ent.sampled        = 0,
				                     ent.wt.chinook     = 0,
				                     ent.fate.unknown   = 0,
				                     ent.fate.reflood   = 0,
				                     ent.fate.dewatered = 0,
				                     ent.fate.thermal   = 0,
				                     fish.alive         = 0,
				                     fish.dead          = 0,
				                     fish.total         = 0,
				                     sites.ent.yes1     = 0,
				                     sites.ent.no1      = 0,
				                     fish.mortality     = 0,
						     no.ent.lethal.fate = 0,
						     no.ent.known.fate  = 0,
						     mort.rate.ent      = 0,
						     fish.morts.known   = 0,
						     fish.total.known   = 0,						     
						     mort.rate.fish     = 0,
						     perc.ent.wt.chinook= 0,
						     chinook.per.ent    = 0)
			}else{
				output <- rbind(output,        
				                   c(segment            = this.segment,
				                     sites.visited      = 0,
				                     sites.ent.yes      = 0,
				                     sites.ent.no       = 0,
				                     ent.sampled        = 0,
				                     ent.wt.chinook     = 0,
				                     ent.fate.unknown   = 0,
				                     ent.fate.reflood   = 0,
				                     ent.fate.dewatered = 0,
				                     ent.fate.thermal   = 0,
				                     fish.alive         = 0,
				                     fish.dead          = 0,
				                     fish.total         = 0,
				                     sites.ent.yes1     = 0,
				                     sites.ent.no1      = 0,
				                     fish.mortality     = 0,
						     no.ent.lethal.fate = 0,
						     no.ent.known.fate  = 0,
						     mort.rate.ent      = 0,
						     fish.morts.known   = 0,
						     fish.total.known   = 0,						     
						     mort.rate.fish     = 0,
						     perc.ent.wt.chinook= 0,
						     chinook.per.ent    = 0)				                     
				                     )
			}		
		}		
	}
	
	# do a total
	output <- rbind(output,
			c(segment = 9,apply(output[,-1],2,FUN=sum,na.rm=TRUE)))
	output[output[,"segment"] == 9,"segment"] <- "total"
	
	# do it for section level
	output <- rbind(output,
			c(segment = 10,apply(output[c(1,2),    -1],2,FUN=sum,na.rm=TRUE)),
			c(segment = 11,apply(output[c(3,4,5,6),-1],2,FUN=sum,na.rm=TRUE)),
			c(segment = 12,apply(output[c(7,8),    -1],2,FUN=sum,na.rm=TRUE)))
	output[output[,"segment"] ==10,"segment"] <- "section 1"	
	output[output[,"segment"] ==11,"segment"] <- "section 2"
	output[output[,"segment"] ==12,"segment"] <- "section 3"
	
	# recalculate rate for "total" and the sections
	index <- output[,"segment"] == "total" | output[,"segment"] == "section 1" | output[,"segment"] == "section 2" | output[,"segment"] == "section 3"
	output[index,"mort.rate.ent"]       <- output[index,"no.ent.lethal.fate"] / output[index,"no.ent.known.fate"]
	output[index,"mort.rate.fish"]      <- output[index,"fish.morts.known"]   / output[index,"fish.total.known"]
	output[index,"perc.ent.wt.chinook"] <- output[index,"ent.wt.chinook"]     / output[index,"ent.sampled"]
	output[index,"chinook.per.ent"]     <- output[index,"fish.total"]         / output[index,"ent.sampled"]

		
	return(output)
	

}







# -------------------------------------------------------------------------------------------------
# a function to calculate some statistics in the data frame: TO make the script concise, put all the code block for stat into a function
# -------------------------------------------------------------------------------------------------
stat.data <- function(my.data,my.biweek,my.segment)
{
	# [my.data] is the input data frame 
	stat.number     <- dim(my.data)[1]							# the number of samples
	stat.1          <- sum(my.data[my.data[,"binary"] == 1,"binary"])			# the number of 1 samples
	stat.1perc      <- round(100*sum(my.data[,"binary"]) / dim(my.data)[1], digits = 2)	# the percentage of 1 status
	stat.0perc      <- round(100 - stat.1perc, digits = 2)					# the percentage of 0 status
	stat.mean.morts <- round(mean(my.data[,"morts"])	, digits = 3)			# the mean mortality
	stat.max.morts  <- round(max(my.data[,"morts"])	        , digits = 3)			# the max mortality
	stat.mean.multi <- round(mean(my.data[,"multiplier"])	, digits = 3)			# the mean multiplier
	stat.max.multi  <- round(max(my.data[,"multiplier"])	, digits = 3)			# the max multiplier
	stat.min.multi  <- round(min(my.data[,"multiplier"])	, digits = 3)			# the min multiplier

	my.stat <- data.frame(  biweek     = my.biweek,
				segment    = my.segment,
				number.all = stat.number,
				number.1   = stat.1,
				perc.1     = stat.1perc,
				perc.0     = stat.0perc,
				mean.morts = stat.mean.morts,
				max.morts  = stat.max.morts,
				mean.multi = stat.mean.multi,
				min.multi  = stat.min.multi,
				max.multi  = stat.max.multi)			      
	return(my.stat)	
}

