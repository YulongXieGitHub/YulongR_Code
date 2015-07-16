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
aggCount.perSegment <- function(my.data,my.biweek.idx,my.allSegment)
{
	# overall statistics
		 site.visited <- tapply(my.data[,"transect"],list(my.data[,"segment"]), FUN=unique)	# get the list of the unique transect surveyed in each segment
	      no.site.visited <- data.frame(no.site.visited = sapply(site.visited,length))		# get the number of the unique surveyed transects
	list.segment.this     <- row.names(no.site.visited)						# list of unique segments occurred in this data set 
	cat(paste("put the actual segment occurred in a list which will be used to determine how many segment missed in a strata\n",sep=""))
	cat(paste("put the actual segment occurred in a list which will be used to determine how many segment missed in a strata\n",sep=""),file=FL.LOG,append=TRUE)

	     list.segment <- my.allSegment								# unique segment occurred in this data set	
	   missed.segment <- setdiff(list.segment,list.segment.this)					# the strata missed
	no.missed.segment <- length(missed.segment)							# the number of missed strata in this boot sample
	cat(paste("count site visited\n",sep=""))
	cat(paste("count site visited\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# Accumulate Area
	area.sampled      <- data.frame(area.sampled      = tapply(my.data[,"area"],       list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "area"       in the sampled plot
	fish.all          <- data.frame(fish.all          = tapply(my.data[,"morts"],      list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "morts"      in the sampled plot
	fish.alive        <- data.frame(fish.alive        = tapply(my.data[,"fish.alive"], list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "fish.alive" in the sampled plot
	fish.dead         <- data.frame(fish.dead         = tapply(my.data[,"fish.dead"],  list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "fish.dead"  in the sampled plot
	number.sample     <- data.frame(number.sample     = tapply(my.data[,"sampled.yes"],list(my.data[,"segment"]), FUN=length))		# number of samples 
	count.sampled     <- data.frame(count.sampled     = tapply(my.data[,"sampled.yes"],list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# number of samples with at least one plot sampled
	count.not.sampled <- data.frame(count.not.sampled = tapply(my.data[,"sampled.no"], list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# number of samples without sampled any plot
	cat(paste("count area and fish in the sampled plot\n",sep=""))
	cat(paste("count area and fish in the sampled plot\n",sep=""),file=FL.LOG,append=TRUE)

	# add the missed strata to the statistics arrays
	if (no.missed.segment > 0)
	{
		cat(paste("(biweek period ",biweek.idx,"): missed sampling in ",no.missed.segment," segment\n",sep=""))
		cat(paste("(biweek period ",biweek.idx,"): missed sampling in ",no.missed.segment," segment\n",sep=""),file=FL.LOG,append=TRUE)
		for (add.segment in missed.segment)
		{
		
			command.string <- paste("number.sample  <- rbind(number.sample,\"", add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))
		
			command.string <- paste("count.sampled  <- rbind(count.sampled,\"", add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.not.sampled  <- rbind(count.not.sampled,\"", add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("no.site.visited  <- rbind(no.site.visited,\"", add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.sampled  <- rbind(area.sampled,\"", add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("fish.all  <- rbind(fish.all,\"", add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("fish.alive  <- rbind(fish.alive,\"", add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("fish.dead  <- rbind(fish.dead,\"", add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))			
		}
		cat(paste("(biweek period ",biweek.idx,"): missing segment have been inserted\n",sep=""))
		cat(paste("(biweek period ",biweek.idx,"): missing segment have been inserted\n",sep=""),file=FL.LOG,append=TRUE)				
	}


	# sorted according to segment index
	no.site.visited   <- no.site.visited [sort(row.names(no.site.visited )),,drop=F]
	area.sampled      <- area.sampled[sort(row.names(area.sampled)),,drop=F]
	fish.all          <- fish.all[sort(row.names(fish.all)),,drop=F]
	fish.dead         <- fish.dead[sort(row.names(fish.dead)),,drop=F]
	fish.alive        <- fish.alive[sort(row.names(fish.alive)),,drop=F]
	number.sample     <- number.sample[sort(row.names(number.sample)),,drop=F]
	count.sampled     <- count.sampled[sort(row.names(count.sampled)),,drop=F]
	count.not.sampled <- count.not.sampled[sort(row.names(count.not.sampled)),,drop=F]

	cat(paste("sort the calculated according to segment\n",sep=""))
	cat(paste("sort the calculated according to segment\n",sep=""),file=FL.LOG,append=TRUE)

	# combine to a single PlotsSampled Table
	my.table <- cbind(Count.siteVisited = no.site.visited,
			  number.sample     = number.sample,
			  count.not.sampled = count.not.sampled,
			  count.sampled     = count.sampled,
			  area.sampled      = area.sampled,
			  fish.all          = fish.all,                             
			  fish.dead         = fish.dead,
			  fish.alive        = fish.alive) 
	names(my.table) <-sub("^no.site.visited$",  "no.transect.visited", names(my.table))			  
	names(my.table) <-sub("^count.not.sampled$","transect.not.sampled",names(my.table))			  
	names(my.table) <-sub("^count.sampled$",    "transect.sampled",    names(my.table))		# from aggregated data		  
	names(my.table) <-sub("^count.sampled2$",   "transect.sampled",    names(my.table))		# from raw data			  
	names(my.table) <-sub("^number.sample$",    "plots.sampled",       names(my.table))		# from aggregated data
	names(my.table) <-sub("^no.yesRecords$",    "plots.sampled",       names(my.table))		# from raw data
	names(my.table) <-sub("^area.sampled$",     "area.sampled",        names(my.table))			  	
	names(my.table) <-sub("^fish.dead$",        "fish.dead",           names(my.table))			  
	
			  
	# added a sum across all segments
	my.table <- rbind(my.table,
			  total=apply(my.table,2,FUN=sum,na.rm=TRUE))			  

	cat("return from \"aggCount.perSegment\"\n")
	return(my.table)
}


# -------------------------------------------------------------------------------------------------
# a function to calculate the variety of counts per segment:
# inputs: data subset, biweek.idx and segment list of the entire data set
# -------------------------------------------------------------------------------------------------
aggCount.perBiweek <- function(my.data,my.allBiweek,my.segment.idx)
{
	segment.idx <- my.segment.idx;
	
	# overall statistics
		 site.visited <- tapply(my.data[,"transect"],list(my.data[,"biweek.idx"]), FUN=unique)	# get the list of the unique transect surveyed in each segment
	      no.site.visited <- data.frame(no.site.visited = sapply(site.visited,length))		# get the number of the unique surveyed transects
	list.biweek.this     <- row.names(no.site.visited)						# list of unique segments occurred in this data set 
	cat(paste("put the actual segment occurred in a list which will be used to determine how many segment missed in a strata\n",sep=""))
	cat(paste("put the actual segment occurred in a list which will be used to determine how many segment missed in a strata\n",sep=""),file=FL.LOG,append=TRUE)

	     list.biweek <- my.allBiweek								# unique segment occurred in this data set	
	   missed.biweek <- setdiff(list.biweek,list.biweek.this)					# the strata missed
	no.missed.biweek <- length(missed.biweek)							# the number of missed strata in this boot sample
	cat(paste("count site visited\n",sep=""))
	cat(paste("count site visited\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# Accumulate Area
	area.sampled      <- data.frame(area.sampled      = tapply(my.data[,"area"],       list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "area"       in the sampled plot
	fish.all          <- data.frame(fish.all          = tapply(my.data[,"morts"],      list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "morts"      in the sampled plot
	fish.alive        <- data.frame(fish.alive        = tapply(my.data[,"fish.alive"], list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "fish.alive" in the sampled plot
	fish.dead         <- data.frame(fish.dead         = tapply(my.data[,"fish.dead"],  list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "fish.dead"  in the sampled plot
	number.sample     <- data.frame(number.sample     = tapply(my.data[,"sampled.yes"],list(my.data[,"biweek.idx"]), FUN=length))		# number of samples 
	count.sampled     <- data.frame(count.sampled     = tapply(my.data[,"sampled.yes"],list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# number of samples with at least one plot sampled
	count.not.sampled <- data.frame(count.not.sampled = tapply(my.data[,"sampled.no"], list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# number of samples without sampled any plot
	cat(paste("count area and fish in the sampled plot\n",sep=""))
	cat(paste("count area and fish in the sampled plot\n",sep=""),file=FL.LOG,append=TRUE)

	# add the missed strata to the statistics arrays
	if (no.missed.biweek > 0)
	{
		cat(paste("(segment ",segment.idx,"): missed sampling in ",no.missed.biweek," biweek periods\n",sep=""))
		cat(paste("(segment ",segment.idx,"): missed sampling in ",no.missed.biweek," biweek periods\n",sep=""),file=FL.LOG,append=TRUE)
		for (add.biweek in missed.biweek)
		{
		
			command.string <- paste("number.sample  <- rbind(number.sample,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))
		
			command.string <- paste("count.sampled  <- rbind(count.sampled,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.not.sampled  <- rbind(count.not.sampled,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("no.site.visited  <- rbind(no.site.visited,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.sampled  <- rbind(area.sampled,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("fish.all  <- rbind(fish.all,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("fish.alive  <- rbind(fish.alive,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("fish.dead  <- rbind(fish.dead,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))			
		}
		cat(paste("(segment ",segment.idx,"): missing biweek periods have been inserted\n",sep=""))
		cat(paste("(segment ",segment.idx,"): missing biweek periods have been inserted\n",sep=""),file=FL.LOG,append=TRUE)		
	}


	# sorted according to segment index
	no.site.visited   <- no.site.visited [sort(row.names(no.site.visited )),,drop=F]
	area.sampled      <- area.sampled[sort(row.names(area.sampled)),,drop=F]
	fish.all          <- fish.all[sort(row.names(fish.all)),,drop=F]
	fish.dead         <- fish.dead[sort(row.names(fish.dead)),,drop=F]
	fish.alive        <- fish.alive[sort(row.names(fish.alive)),,drop=F]
	number.sample     <- number.sample[sort(row.names(number.sample)),,drop=F]
	count.sampled     <- count.sampled[sort(row.names(count.sampled)),,drop=F]
	count.not.sampled <- count.not.sampled[sort(row.names(count.not.sampled)),,drop=F]

	cat(paste("sort the calculated according to segment\n",sep=""))
	cat(paste("sort the calculated according to segment\n",sep=""),file=FL.LOG,append=TRUE)

	# combine to a single PlotsSampled Table
	my.table <- cbind(Count.siteVisited = no.site.visited,
			  number.sample     = number.sample,
			  count.not.sampled = count.not.sampled,
			  count.sampled     = count.sampled,
			  area.sampled      = area.sampled,
			  fish.all          = fish.all,                             
			  fish.dead         = fish.dead,
			  fish.alive        = fish.alive)    
			  
	names(my.table) <-sub("^no.site.visited$",  "no.transect.visited", names(my.table))			  
	names(my.table) <-sub("^count.not.sampled$","transect.not.sampled",names(my.table))			  
	names(my.table) <-sub("^count.sampled$",    "transect.sampled",    names(my.table))		# from aggregated data		  
	names(my.table) <-sub("^count.sampled2$",   "transect.sampled",    names(my.table))		# from raw data			  
	names(my.table) <-sub("^number.sample$",    "plots.sampled",       names(my.table))		# from aggregated data
	names(my.table) <-sub("^no.yesRecords$",    "plots.sampled",       names(my.table))		# from raw data
	names(my.table) <-sub("^area.sampled$",     "area.sampled",        names(my.table))			  	
	names(my.table) <-sub("^fish.dead$",        "fish.dead",           names(my.table))			  
			  
			  
	# added a sum across all segments
	my.table <- rbind(my.table,
			  total=apply(my.table,2,FUN=sum,na.rm=TRUE))			  

	cat("return from \"aggCount.perBiweek\"\n")
	return(my.table)
}


# -------------------------------------------------------------------------------------------------
# a function to calculate the variety of counts per segment:
# inputs: data subset, biweek.idx and segment list of the entire data set
# -------------------------------------------------------------------------------------------------
count.perSegment <- function(my.data,my.biweek.idx,my.allSegment)
{

	
	biweek.idx <- my.biweek.idx;
	# overall statistics
		 site.visited <- tapply(my.data[,"transect"],list(my.data[,"segment"]), FUN=unique)	# get the list of the unique transect surveyed in each segment
	      no.site.visited <- data.frame(no.site.visited = sapply(site.visited,length))		# get the number of the unique surveyed transects
	list.segment.this     <- row.names(no.site.visited)						# list of unique segments occurred in this data set 
	cat(paste("put the actual segment occurred in a list which will be used to determine how many segment missed in a strata\n",sep=""))
	cat(paste("put the actual segment occurred in a list which will be used to determine how many segment missed in a strata\n",sep=""),file=FL.LOG,append=TRUE)

	     list.segment <- my.allSegment								# unique segment occurred in this data set	
	   missed.segment <- setdiff(list.segment,list.segment.this)					# the strata missed
	no.missed.segment <- length(missed.segment)							# the number of missed strata in this boot sample
	cat(paste("count site visited\n",sep=""))
	cat(paste("count site visited\n",sep=""),file=FL.LOG,append=TRUE)


	# -------------------------------------------------------------------------------------------------
	# Accumulate Area
	area.sampled <- data.frame(area.sampled = tapply(my.data[,"area"],      list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "area"       in the sampled plot
	area.wet     <- data.frame(area.wet     = tapply(my.data[,"area.wet"],  list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "area.wet"   in the sampled plot
	area.dry     <- data.frame(area.dry     = tapply(my.data[,"area.dry"],  list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "area.dry"   in the sampled plot
	fish.all     <- data.frame(fish.all     = tapply(my.data[,"morts"],     list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "morts"      in the sampled plot
	fish.alive   <- data.frame(fish.alive   = tapply(my.data[,"fish.alive"],list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "fish.alive" in the sampled plot
	fish.dead    <- data.frame(fish.dead    = tapply(my.data[,"fish.dead"], list(my.data[,"segment"]), FUN=sum,na.rm=TRUE))	# all "fish.dead"  in the sampled plot
	cat(paste("count area and fish in the sampled plot\n",sep=""))
	cat(paste("count area and fish in the sampled plot\n",sep=""),file=FL.LOG,append=TRUE)


	# -------------------------------------------------------------------------------------------------
	# create a field of [no.yesRecords] in in terms of segment
	      no.yesRecords <- data.frame(no.yesRecords = tapply(my.data[,"sampled.yes"],list(my.data[,"segment"]), FUN=sum))	# all "Y" in the sampled field
	cat(paste("count number of plots sampled\n",sep=""))
	cat(paste("count number of plots sampled\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# Note: there should be no transect appreaing in different days of the same biweek period, but the data does have cases violated this assumption!!!!!!!!!!!!)
	# number of total plots of each unique sample (i.e., date-transect combination): count the number of all quardrants ("sampled"="Y" or "N") for a give transect at a given day 
	    # plots.count  <- aggregate(my.data[,c("sampled.yes")],list(my.data[,"date"],my.data[,"segment"],my.data[,"transect"]), FUN=length)		# the number of records of a "date-segment-transect" combination
	      plots.count  <- aggregate(my.data[,c("sampled.yes")],list(                 my.data[,"segment"],my.data[,"transect"]), FUN=length)		# the number of records of a "date-segment-transect" combination
      # names(plots.count) <- c("date","segment","transect","plots.count.all")
	names(plots.count) <- c(       "segment","transect","plots.count.all")

	# number of sampled plots of each unique sample (i.e., date-transect combination):  count the number of "Y" quardrants ("sampled"="Y")        for a give transect at a given day 
	    # plots.yes    <- aggregate(my.data[,c("sampled.yes")],list(my.data[,"date"],my.data[,"segment"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-segment-transect" combination
	      plots.yes    <- aggregate(my.data[,c("sampled.yes")],list(                 my.data[,"segment"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-segment-transect" combination
      # names(plots.yes)   <- c("date","segment","transect","plots.count.yes")
	names(plots.yes)   <- c(       "segment","transect","plots.count.yes")

	# number of not-sampled plots of each unique sample (i.e., date-transect combination):  count the number of "N" quardrants ("sampled"="N")        for a give transect at a given day 
	    # plots.no     <- aggregate(my.data[,c("sampled.no")],list(my.data[,"date"], my.data[,"segment"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-segment-transect" combination
	      plots.no     <- aggregate(my.data[,c("sampled.no")],list(                  my.data[,"segment"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-segment-transect" combination
      # names(plots.no)    <- c("date","segment","transect","plots.count.no")
	names(plots.no)    <- c(       "segment","transect","plots.count.no")
	
	cat(paste("count numbers of total, sampled and not-sampled plots in the segments\n",sep=""))
	cat(paste("count numbers of total, sampled and not-sampled plots in the segments\n",sep=""),file=FL.LOG,append=TRUE)

	# assemble the counts into a dataframe.  The numbers of total plots, sampled plots and not sampled plots are used to create the "Plots Sampled" and "Plots Not Sampled" statistics
	my.data.plots <- cbind(plots.count,
			       plots.count.yes = plots.yes[,"plots.count.yes"],
			       plots.count.no  = plots.no[,"plots.count.no"])                

	# assign "plots Sampled No":	if total plots == not sampled plots, i.e., plots.count == plots.no,  assign 1 otherwise 0  
	#        "plots Sampled Yes":	if total plots ==     sampled plots, i.e., plots.count == plots.yes, assign 1 otherwise 0  
	#        "plots Sampled YesNo":	if sampled plots > 0,                i.e., plots.count.yes > 0,        assign 1 otherwise 0    This is the "plots Sampled Yes" in the summary tab "Stranding Summary"
	my.data.plots <- cbind(my.data.plots,
			       plots.no    = rep(0,dim(my.data.plots)[1]),	# initialize "Plots Sampled : No"    with 0 
			       plots.yes   = rep(0,dim(my.data.plots)[1]),	# initialize "Plots Sampled : Yes"   with 0
			       plots.yesNo = rep(0,dim(my.data.plots)[1]))	# initialize "Plots Sampled : YesNo" with 0            
	# assign values
	my.data.plots[my.data.plots[,"plots.count.all"] == my.data.plots[,"plots.count.no"], "plots.no"]    <- 1	# the sum of sampled "no"  in the data packet is the same as the length of the data packet, means all records in the data packet are "No".
	my.data.plots[my.data.plots[,"plots.count.all"] == my.data.plots[,"plots.count.yes"],"plots.yes"]   <- 1	# the sum of sampled "yes" in the data packet is the same as the length of the data packet, means all records in the data packet are "Yes".
	my.data.plots[my.data.plots[,"plots.count.yes"] > 0,                                 "plots.yesNo"] <- 1	# the sum of sampled "yes" in the data packet is not zero                                 , means at least there is sampled "Yes" records
	cat(paste("creat a data.frame of [my.data.plots]\n",sep=""))
	cat(paste("creat a data.frame of [my.data.plots]\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# count plots sampled "No" and "Yes"
	 count.sampled1    <- data.frame(count.sampled1    = tapply(my.data.plots[,"plots.yes"],  list(my.data.plots[,"segment"]), FUN=sum))
	 count.sampled2    <- data.frame(count.sampled2    = tapply(my.data.plots[,"plots.yesNo"],list(my.data.plots[,"segment"]), FUN=sum))	# this is "Plots Sampled Yes" in the summary tab "Stranding Summary"
	 count.not.sampled <- data.frame(count.not.sampled = tapply(my.data.plots[,"plots.no"],   list(my.data.plots[,"segment"]), FUN=sum))	# this is "Plots Sampled No"  in the summary tab "Stranding Summary"
	cat(paste("the count the Yes/No transect/qyardrant\n",sep=""))
	cat(paste("the count the Yes/No transect/qyardrant\n",sep=""),file=FL.LOG,append=TRUE)

	# add the missed strata to the statistics arrays
	if (no.missed.segment > 0)
	{
		cat(paste("(biweek period ",biweek.idx,"): missed sampling in ",no.missed.segment," segment\n",sep=""))
		cat(paste("(biweek period ",biweek.idx,"): missed sampling in ",no.missed.segment," segment\n",sep=""),file=FL.LOG,append=TRUE)
		for (add.segment in missed.segment)
		{
			command.string <- paste("count.sampled1      <- rbind(count.sampled1,\"",    add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.sampled2      <- rbind(count.sampled2,\"",    add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.not.sampled   <- rbind(count.not.sampled,\"", add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("no.yesRecords       <- rbind(no.yesRecords,\"",     add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("no.site.visited     <- rbind(no.site.visited,\"",   add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.sampled        <- rbind(area.sampled,\"",      add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.wet            <- rbind(area.wet,\"",          add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))
			
			command.string <- paste("area.dry            <- rbind(area.dry,\"",          add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))
			
			command.string <- paste("fish.all            <- rbind(fish.all,\"",          add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("fish.alive          <- rbind(fish.alive,\"",        add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("fish.dead           <- rbind(fish.dead,\"",         add.segment,"\"=NA)",sep="")
			eval(parse(text=command.string))			
		}
		cat(paste("(biweek period ",biweek.idx,"): missing segments have been inserted\n",sep=""))
		cat(paste("(biweek period ",biweek.idx,"): missing segments have been inserted\n",sep=""),file=FL.LOG,append=TRUE)		
	}

	# sorted according to segment index
	count.sampled1       <- count.sampled1[sort(row.names(count.sampled1)),,drop=F]
	count.sampled2       <- count.sampled2[sort(row.names(count.sampled2)),,drop=F]
	count.not.sampled    <- count.not.sampled[sort(row.names(count.not.sampled)),,drop=F]
	no.yesRecords        <- no.yesRecords[sort(row.names(no.yesRecords)),,drop=F]
	no.site.visited      <- no.site.visited [sort(row.names(no.site.visited )),,drop=F]
	area.sampled         <- area.sampled[sort(row.names(area.sampled)),,drop=F]
	area.wet             <- area.wet[sort(row.names(area.wet)),,drop=F]
	area.dry             <- area.dry[sort(row.names(area.dry)),,drop=F]
	fish.all             <- fish.all[sort(row.names(fish.all)),,drop=F]
	fish.dead            <- fish.dead[sort(row.names(fish.dead)),,drop=F]
	fish.alive           <- fish.alive[sort(row.names(fish.alive)),,drop=F]
	cat(paste("sort the calculated according to segment\n",sep=""))
	cat(paste("sort the calculated according to segment\n",sep=""),file=FL.LOG,append=TRUE)



	# combine to a single PlotsSampled Table
	my.table <- cbind(Count.siteVisited = no.site.visited,
			  count.sampled.No  = count.not.sampled,
			  count.sampled.Yes = count.sampled2,
			  count.sampled     = count.sampled1,
			  Plots             = no.yesRecords,
			  area.sampled      = area.sampled,
			  area.wet          = area.wet,
			  area.dry          = area.dry,
			  fish.all          = fish.all,                             
			  fish.dead         = fish.dead,
			  fish.alive        = fish.alive)    
			  
	names(my.table) <-sub("^no.site.visited$",  "no.transect.visited", names(my.table))			  
	names(my.table) <-sub("^count.not.sampled$","transect.not.sampled",names(my.table))			  
	names(my.table) <-sub("^count.sampled$",    "transect.sampled",    names(my.table))		# from aggregated data		  
	names(my.table) <-sub("^count.sampled2$",   "transect.sampled",    names(my.table))		# from raw data			  
	names(my.table) <-sub("^number.sample$",    "plots.sampled",       names(my.table))		# from aggregated data
	names(my.table) <-sub("^no.yesRecords$",    "plots.sampled",       names(my.table))		# from raw data
	names(my.table) <-sub("^area.sampled$",     "area.sampled",        names(my.table))			
	names(my.table) <-sub("^area.wet$",         "area.wet",            names(my.table))			
	names(my.table) <-sub("^area.dry$",         "area.dry",            names(my.table))			
	names(my.table) <-sub("^fish.dead$",        "fish.dead",           names(my.table))			  
			  

	# added a sum across all segments
	my.table <- rbind(my.table,
			  total=apply(my.table,2,FUN=sum,na.rm=TRUE))
	cat(paste("put the sorted calculated quantities in a Table\n",sep=""))
	cat(paste("put the sorted calculated quantities in a Table\n",sep=""),file=FL.LOG,append=TRUE)


	Table.siteVisited <- rbind(no.site.visited,total=sum(no.site.visited))				# put number of sites visited into a TABLE
	cat(paste("put the site visted alone into a Table\n",sep=""))
	cat(paste("put the site visted alone into a Table\n",sep=""),file=FL.LOG,append=TRUE)


	return(my.table)
}






# -------------------------------------------------------------------------------------------------
# a function to calculate the variety of counts per biweek period:
# inputs: data subset, segment index and biweek list of the entire data set
# -------------------------------------------------------------------------------------------------
count.perBiweek <- function(my.data,my.allBiweek,my.segment.idx)
{
	segment.idx <- my.segment.idx;
	# overall statistics
		 site.visited <- tapply(my.data[,"transect"],list(my.data[,"biweek.idx"]), FUN=unique)	# get the list of the unique transect surveyed in each segment
	      no.site.visited <- data.frame(no.site.visited = sapply(site.visited,length))		# get the number of the unique surveyed transects
	list.biweek.this     <- row.names(no.site.visited)						# list of unique segments occurred in this data set 
	cat(paste("put the actual segment occurred in a list which will be used to determine how many segment missed in a strata\n",sep=""))
	cat(paste("put the actual segment occurred in a list which will be used to determine how many segment missed in a strata\n",sep=""),file=FL.LOG,append=TRUE)

	     list.biweek  <- my.allBiweek								# unique segment occurred in this data set	
	   missed.biweek <- setdiff(list.biweek,list.biweek.this)					# the strata missed
	no.missed.biweek <- length(missed.biweek)							# the number of missed strata in this boot sample
	cat(paste("count site visited\n",sep=""))
	cat(paste("count site visited\n",sep=""),file=FL.LOG,append=TRUE)
	
	# -------------------------------------------------------------------------------------------------
	# Accumulate Area
	area.sampled <- data.frame(area.sampled = tapply(my.data[,"area"],      list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "area"       in the sampled plot
	area.wet     <- data.frame(area.wet     = tapply(my.data[,"area.wet"],  list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "area.wet"   in the sampled plot
	area.dry     <- data.frame(area.dry     = tapply(my.data[,"area.dry"],  list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "area.dry"   in the sampled plot
	fish.all     <- data.frame(fish.all     = tapply(my.data[,"morts"],     list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "morts"      in the sampled plot
	fish.alive   <- data.frame(fish.alive   = tapply(my.data[,"fish.alive"],list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "fish.alive" in the sampled plot
	fish.dead    <- data.frame(fish.dead    = tapply(my.data[,"fish.dead"], list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "fish.dead"  in the sampled plot
	cat(paste("count area and fish in the sampled plot\n",sep=""))
	cat(paste("count area and fish in the sampled plot\n",sep=""),file=FL.LOG,append=TRUE)


	# -------------------------------------------------------------------------------------------------
	# create a field of [no.yesRecords] in in terms of segment
	      no.yesRecords <- data.frame(no.yesRecords = tapply(my.data[,"sampled.yes"],list(my.data[,"biweek.idx"]), FUN=sum))	# all "Y" in the sampled field
	cat(paste("count number of plots sampled\n",sep=""))
	cat(paste("count number of plots sampled\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# number of total plots of each unique sample (i.e., date-transect combination): count the number of all quardrants ("sampled"="Y" or "N") for a give transect at a given day 
	#       plots.count  <- aggregate(my.data[,c("sampled.yes")],list(my.data[,"date"],my.data[,"biweek.idx"],my.data[,"transect"]), FUN=length)		# the number of records of a "date-segment-transect" combination
	# names(plots.count) <- c("date","biweek.idx","transect","plots.count.all")
	        plots.count  <- aggregate(my.data[,c("sampled.yes")],list(                 my.data[,"biweek.idx"],my.data[,"transect"]), FUN=length)		# the number of records of a "date-segment-transect" combination
	  names(plots.count) <- c(       "biweek.idx","transect","plots.count.all")

	# number of sampled plots of each unique sample (i.e., date-transect combination):  count the number of "Y" quardrants ("sampled"="Y")        for a give transect at a given day 
	#       plots.yes    <- aggregate(my.data[,c("sampled.yes")],list(my.data[,"date"],my.data[,"biweek.idx"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-segment-transect" combination
	# names(plots.yes)   <- c("date","biweek.idx","transect","plots.count.yes")
	        plots.yes    <- aggregate(my.data[,c("sampled.yes")],list(                 my.data[,"biweek.idx"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-segment-transect" combination
	  names(plots.yes)   <- c(       "biweek.idx","transect","plots.count.yes")

	# number of not-sampled plots of each unique sample (i.e., date-transect combination):  count the number of "N" quardrants ("sampled"="N")        for a give transect at a given day 
	#       plots.no     <- aggregate(my.data[,c("sampled.no")],list(my.data[,"date"],my.data[,"biweek.idx"],my.data[,"transect"]), FUN=sum,na.rm=T)		# the number of sampled "Yes" records of a "date-segment-transect" combination
	# names(plots.no)    <- c("date","biweek.idx","transect","plots.count.no")
	        plots.no     <- aggregate(my.data[,c("sampled.no")],list(                  my.data[,"biweek.idx"],my.data[,"transect"]), FUN=sum,na.rm=T)		# the number of sampled "Yes" records of a "date-segment-transect" combination
	  names(plots.no)    <- c(       "biweek.idx","transect","plots.count.no")
	cat(paste("count numbers of total, sampled and not-sampled plots in the segments\n",sep=""))
	cat(paste("count numbers of total, sampled and not-sampled plots in the segments\n",sep=""),file=FL.LOG,append=TRUE)

	# assemble the counts into a dataframe.  The numbers of total plots, sampled plots and not sampled plots are used to create the "Plots Sampled" and "Plots Not Sampled" statistics
	my.data.plots <- cbind(plots.count,
			       plots.count.yes = plots.yes[,"plots.count.yes"],
			       plots.count.no  = plots.no[,"plots.count.no"])                

	# assign "plots Sampled No":	if total plots == not sampled plots, i.e., plots.count == plots.no,  assign 1 otherwise 0  
	#        "plots Sampled Yes":	if total plots ==     sampled plots, i.e., plots.count == plots.yes, assign 1 otherwise 0  
	#        "plots Sampled YesNo":	if sampled plots > 0,                i.e., plots.count.yes > 0,        assign 1 otherwise 0    This is the "plots Sampled Yes" in the summary tab "Stranding Summary"
	my.data.plots <- cbind(my.data.plots,
			       plots.no    = rep(0,dim(my.data.plots)[1]),	# initialize "Plots Sampled : No"    with 0 
			       plots.yes   = rep(0,dim(my.data.plots)[1]),	# initialize "Plots Sampled : Yes"   with 0
			       plots.yesNo = rep(0,dim(my.data.plots)[1]))	# initialize "Plots Sampled : YesNo" with 0            
	# assign values
	my.data.plots[my.data.plots[,"plots.count.all"] == my.data.plots[,"plots.count.no"], "plots.no"]    <- 1	# the sum of sampled "no"  in the data packet is the same as the length of the data packet, means all records in the data packet are "No".
	my.data.plots[my.data.plots[,"plots.count.all"] == my.data.plots[,"plots.count.yes"],"plots.yes"]   <- 1	# the sum of sampled "yes" in the data packet is the same as the length of the data packet, means all records in the data packet are "Yes".
	my.data.plots[my.data.plots[,"plots.count.yes"] > 0,                                 "plots.yesNo"] <- 1	# the sum of sampled "yes" in the data packet is not zero                                 , means at least there is sampled "Yes" records
	cat(paste("creat a data.frame of [my.data.plots]\n",sep=""))
	cat(paste("creat a data.frame of [my.data.plots]\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# count plots sampled "No" and "Yes"
	 count.sampled1    <- data.frame(count.sampled1    = tapply(my.data.plots[,"plots.yes"],  list(my.data.plots[,"biweek.idx"]), FUN=sum))
	 count.sampled2    <- data.frame(count.sampled2    = tapply(my.data.plots[,"plots.yesNo"],list(my.data.plots[,"biweek.idx"]), FUN=sum))	# this is "Plots Sampled Yes" in the summary tab "Stranding Summary"
	 count.not.sampled <- data.frame(count.not.sampled = tapply(my.data.plots[,"plots.no"],   list(my.data.plots[,"biweek.idx"]), FUN=sum))	# this is "Plots Sampled No"  in the summary tab "Stranding Summary"
	cat(paste("the count the Yes/No transect/qyardrant\n",sep=""))
	cat(paste("the count the Yes/No transect/qyardrant\n",sep=""),file=FL.LOG,append=TRUE)

	# add the missed strata to the statistics arrays
	if (no.missed.biweek > 0)
	{
		cat(paste("(segment ",segment.idx,"): missed sampling in ",no.missed.biweek," biweek periods\n",sep=""))
		cat(paste("(segment ",segment.idx,"): missed sampling in ",no.missed.biweek," biweek periods\n",sep=""),file=FL.LOG,append=TRUE)
		for (add.biweek in missed.biweek)
		{
			command.string <- paste("count.sampled1  <- rbind(count.sampled1,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.sampled2  <- rbind(count.sampled2,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.not.sampled  <- rbind(count.not.sampled,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("no.yesRecords  <- rbind(no.yesRecords,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("no.site.visited  <- rbind(no.site.visited,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.sampled  <- rbind(area.sampled,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.wet  <- rbind(area.wet,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))
			
			command.string <- paste("area.dry  <- rbind(area.dry,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("fish.all  <- rbind(fish.all,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("fish.alive  <- rbind(fish.alive,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("fish.dead  <- rbind(fish.dead,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))			
		}
		cat(paste("(segment ",segment.idx,"): missing biweek periods have been inserted\n",sep=""))
		cat(paste("(segment ",segment.idx,"): missing biweek periods have been inserted\n",sep=""),file=FL.LOG,append=TRUE)				
	}

	# sorted according to segment index
	count.sampled1       <- count.sampled1[sort(row.names(count.sampled1)),,drop=F]
	count.sampled2       <- count.sampled2[sort(row.names(count.sampled2)),,drop=F]
	count.not.sampled    <- count.not.sampled[sort(row.names(count.not.sampled)),,drop=F]
	no.yesRecords        <- no.yesRecords[sort(row.names(no.yesRecords)),,drop=F]
	no.site.visited      <- no.site.visited [sort(row.names(no.site.visited )),,drop=F]
	area.sampled         <- area.sampled[sort(row.names(area.sampled)),,drop=F]
	area.wet             <- area.wet[sort(row.names(area.wet)),,drop=F]
	area.dry             <- area.dry[sort(row.names(area.dry)),,drop=F]
	fish.all             <- fish.all[sort(row.names(fish.all)),,drop=F]
	fish.dead            <- fish.dead[sort(row.names(fish.dead)),,drop=F]
	fish.alive           <- fish.alive[sort(row.names(fish.alive)),,drop=F]
	cat(paste("sort the calculated according to segment\n",sep=""))
	cat(paste("sort the calculated according to segment\n",sep=""),file=FL.LOG,append=TRUE)

	# combine to a single PlotsSampled Table
	my.table <- cbind(Count.siteVisited = no.site.visited,
			  count.sampled.No  = count.not.sampled,
			  count.sampled.Yes = count.sampled2,
			  count.sampled     = count.sampled1,
			  Plots             = no.yesRecords,
			  area.sampled      = area.sampled,
			  area.wet          = area.wet,
			  area.dry          = area.dry,
			  fish.all          = fish.all,                             
			  fish.dead         = fish.dead,
			  fish.alive        = fish.alive)    
			  
	names(my.table) <-sub("^no.site.visited$",  "no.transect.visited", names(my.table))			  
	names(my.table) <-sub("^count.not.sampled$","transect.not.sampled",names(my.table))			  
	names(my.table) <-sub("^count.sampled$",    "transect.sampled",    names(my.table))		# from aggregated data		  
	names(my.table) <-sub("^count.sampled2$",   "transect.sampled",    names(my.table))		# from raw data			  
	names(my.table) <-sub("^number.sample$",    "plots.sampled",       names(my.table))		# from aggregated data
	names(my.table) <-sub("^no.yesRecords$",    "plots.sampled",       names(my.table))		# from raw data
	names(my.table) <-sub("^area.sampled$",     "area.sampled",        names(my.table))			  	
	names(my.table) <-sub("^area.wet$",         "area.wet",            names(my.table))
	names(my.table) <-sub("^area.dry$",         "area.dry",            names(my.table))
	names(my.table) <-sub("^fish.dead$",        "fish.dead",           names(my.table))			  


	# added a sum across all segments
	my.table <- rbind(my.table,
			  total=apply(my.table,2,FUN=sum,na.rm=TRUE))
	cat(paste("put the sorted calculated quantities in a Table\n",sep=""))
	cat(paste("put the sorted calculated quantities in a Table\n",sep=""),file=FL.LOG,append=TRUE)


	Table.siteVisited <- rbind(no.site.visited,total=sum(no.site.visited))				# put number of sites visited into a TABLE
	cat(paste("put the site visted alone into a Table\n",sep=""))
	cat(paste("put the site visted alone into a Table\n",sep=""),file=FL.LOG,append=TRUE)
	
	cat("return from \"count.perBiweek\"\n")
	
	return(my.table)
}






















# -------------------------------------------------------------------------------------------------
# a function to calculate the variety of counts per section:
# inputs: data subset, biweek.idx and section list of the entire data set
# -------------------------------------------------------------------------------------------------
count.perSectionBiweek <- function(my.data,my.biweek.idx,my.allSection)
{
	biweek.idx <- my.biweek.idx;
	# overall statistics
		 site.visited <- tapply(my.data[,"transect"],list(my.data[,"section"]), FUN=unique)	# get the list of the unique transect surveyed in each section
	      no.site.visited <- data.frame(no.site.visited = sapply(site.visited,length))		# get the number of the unique surveyed transects
	list.section.this     <- row.names(no.site.visited)						# list of unique sections occurred in this data set 
	cat(paste("put the actual section occurred in a list which will be used to determine how many section missed in a strata\n",sep=""))
	cat(paste("put the actual section occurred in a list which will be used to determine how many section missed in a strata\n",sep=""),file=FL.LOG,append=TRUE)

	     list.section <- my.allSection								# unique section occurred in this data set	
	   missed.section <- setdiff(list.section,list.section.this)					# the strata missed
	no.missed.section <- length(missed.section)							# the number of missed strata in this boot sample
	cat(paste("count site visited\n",sep=""))
	cat(paste("count site visited\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# Accumulate Area
	area.sampled <- data.frame(area.sampled = tapply(my.data[,"area"],      list(my.data[,"section"]), FUN=sum,na.rm=TRUE))	# all "area"       in the sampled plot
	area.wet     <- data.frame(area.wet     = tapply(my.data[,"area.wet"],  list(my.data[,"section"]), FUN=sum,na.rm=TRUE))	# all "area.wet"   in the sampled plot
	area.dry     <- data.frame(area.dry     = tapply(my.data[,"area.dry"],  list(my.data[,"section"]), FUN=sum,na.rm=TRUE))	# all "area.dry"   in the sampled plot
	fish.all     <- data.frame(fish.all     = tapply(my.data[,"morts"],     list(my.data[,"section"]), FUN=sum,na.rm=TRUE))	# all "morts"      in the sampled plot
	fish.alive   <- data.frame(fish.alive   = tapply(my.data[,"fish.alive"],list(my.data[,"section"]), FUN=sum,na.rm=TRUE))	# all "fish.alive" in the sampled plot
	fish.dead    <- data.frame(fish.dead    = tapply(my.data[,"fish.dead"], list(my.data[,"section"]), FUN=sum,na.rm=TRUE))	# all "fish.dead"  in the sampled plot
	cat(paste("count area and fish in the sampled plot\n",sep=""))
	cat(paste("count area and fish in the sampled plot\n",sep=""),file=FL.LOG,append=TRUE)


	# -------------------------------------------------------------------------------------------------
	# create a field of [no.yesRecords] in in terms of section
	      no.yesRecords <- data.frame(no.yesRecords = tapply(my.data[,"sampled.yes"],list(my.data[,"section"]), FUN=sum))	# all "Y" in the sampled field
	cat(paste("count number of plots sampled\n",sep=""))
	cat(paste("count number of plots sampled\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# number of total plots of each unique sample (i.e., date-transect combination): count the number of all quardrants ("sampled"="Y" or "N") for a give transect at a given day 
	#       plots.count  <- aggregate(my.data[,c("sampled.yes")],list(my.data[,"date"],my.data[,"section"],my.data[,"transect"]), FUN=length)		# the number of records of a "date-section-transect" combination
	# names(plots.count) <- c("date","section","transect","plots.count.all")
	        plots.count  <- aggregate(my.data[,c("sampled.yes")],list(                 my.data[,"section"],my.data[,"transect"]), FUN=length)		# the number of records of a "date-section-transect" combination
	  names(plots.count) <- c(       "section","transect","plots.count.all")

	# number of sampled plots of each unique sample (i.e., date-transect combination):  count the number of "Y" quardrants ("sampled"="Y")        for a give transect at a given day 
	#       plots.yes    <- aggregate(my.data[,c("sampled.yes")],list(my.data[,"date"],my.data[,"section"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-section-transect" combination
	# names(plots.yes)   <- c("date","section","transect","plots.count.yes")
	        plots.yes    <- aggregate(my.data[,c("sampled.yes")],list(                 my.data[,"section"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-section-transect" combination
	  names(plots.yes)   <- c(       "section","transect","plots.count.yes")

	# number of not-sampled plots of each unique sample (i.e., date-transect combination):  count the number of "N" quardrants ("sampled"="N")        for a give transect at a given day 
	#       plots.no     <- aggregate(my.data[,c("sampled.no")],list(my.data[,"date"],my.data[,"section"],my.data[,"transect"]), FUN=sum,na.rm=T)		# the number of sampled "Yes" records of a "date-section-transect" combination
	# names(plots.no)    <- c("date","section","transect","plots.count.no")
	        plots.no     <- aggregate(my.data[,c("sampled.no")],list(                  my.data[,"section"],my.data[,"transect"]), FUN=sum,na.rm=T)		# the number of sampled "Yes" records of a "date-section-transect" combination
	  names(plots.no)    <- c(       "section","transect","plots.count.no")
	cat(paste("count numbers of total, sampled and not-sampled plots in the sections\n",sep=""))
	cat(paste("count numbers of total, sampled and not-sampled plots in the sections\n",sep=""),file=FL.LOG,append=TRUE)

	# assemble the counts into a dataframe.  The numbers of total plots, sampled plots and not sampled plots are used to create the "Plots Sampled" and "Plots Not Sampled" statistics
	my.data.plots <- cbind(plots.count,
			       plots.count.yes = plots.yes[,"plots.count.yes"],
			       plots.count.no  = plots.no[,"plots.count.no"])                

	# assign "plots Sampled No":	if total plots == not sampled plots, i.e., plots.count == plots.no,  assign 1 otherwise 0  
	#        "plots Sampled Yes":	if total plots ==     sampled plots, i.e., plots.count == plots.yes, assign 1 otherwise 0  
	#        "plots Sampled YesNo":	if sampled plots > 0,                i.e., plots.count.yes > 0,        assign 1 otherwise 0    This is the "plots Sampled Yes" in the summary tab "Stranding Summary"
	my.data.plots <- cbind(my.data.plots,
			       plots.no    = rep(0,dim(my.data.plots)[1]),	# initialize "Plots Sampled : No"    with 0 
			       plots.yes   = rep(0,dim(my.data.plots)[1]),	# initialize "Plots Sampled : Yes"   with 0
			       plots.yesNo = rep(0,dim(my.data.plots)[1]))	# initialize "Plots Sampled : YesNo" with 0            
	# assign values
	my.data.plots[my.data.plots[,"plots.count.all"] == my.data.plots[,"plots.count.no"], "plots.no"]    <- 1	# the sum of sampled "no"  in the data packet is the same as the length of the data packet, means all records in the data packet are "No".
	my.data.plots[my.data.plots[,"plots.count.all"] == my.data.plots[,"plots.count.yes"],"plots.yes"]   <- 1	# the sum of sampled "yes" in the data packet is the same as the length of the data packet, means all records in the data packet are "Yes".
	my.data.plots[my.data.plots[,"plots.count.yes"] > 0,                                 "plots.yesNo"] <- 1	# the sum of sampled "yes" in the data packet is not zero                                 , means at least there is sampled "Yes" records
	cat(paste("creat a data.frame of [my.data.plots]\n",sep=""))
	cat(paste("creat a data.frame of [my.data.plots]\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# count plots sampled "No" and "Yes"
	 count.sampled1    <- data.frame(count.sampled1    = tapply(my.data.plots[,"plots.yes"],  list(my.data.plots[,"section"]), FUN=sum))
	 count.sampled2    <- data.frame(count.sampled2    = tapply(my.data.plots[,"plots.yesNo"],list(my.data.plots[,"section"]), FUN=sum))	# this is "Plots Sampled Yes" in the summary tab "Stranding Summary"
	 count.not.sampled <- data.frame(count.not.sampled = tapply(my.data.plots[,"plots.no"],   list(my.data.plots[,"section"]), FUN=sum))	# this is "Plots Sampled No"  in the summary tab "Stranding Summary"
	cat(paste("the count the Yes/No transect/qyardrant\n",sep=""))
	cat(paste("the count the Yes/No transect/qyardrant\n",sep=""),file=FL.LOG,append=TRUE)

	# add the missed strata to the statistics arrays
	if (no.missed.section > 0)
	{
		cat(paste("(biweek period ",biweek.idx,"): missed sampling in ",no.missed.section," section\n",sep=""))
		cat(paste("(biweek period ",biweek.idx,"): missed sampling in ",no.missed.section," section\n",sep=""),file=FL.LOG,append=TRUE)
		for (add.section in missed.section)
		{
			command.string <- paste("count.sampled1  <- rbind(count.sampled1,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.sampled2  <- rbind(count.sampled2,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.not.sampled  <- rbind(count.not.sampled,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("no.yesRecords  <- rbind(no.yesRecords,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("no.site.visited  <- rbind(no.site.visited,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.sampled  <- rbind(area.sampled,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.wet  <- rbind(area.wet,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))
			
			command.string <- paste("area.dry  <- rbind(area.dry,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))
			
			
			command.string <- paste("fish.all  <- rbind(fish.all,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("fish.alive  <- rbind(fish.alive,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("fish.dead  <- rbind(fish.dead,\"", add.section,"\"=NA)",sep="")
			eval(parse(text=command.string))			
		}
		cat(paste("(biweek period ",biweek.idx,"): missing sections have been inserted\n",sep=""))
		cat(paste("(biweek period ",biweek.idx,"): missing sections have been inserted\n",sep=""),file=FL.LOG,append=TRUE)		
	}

	# sorted according to section index
	count.sampled1       <- count.sampled1[sort(row.names(count.sampled1)),,drop=F]
	count.sampled2       <- count.sampled2[sort(row.names(count.sampled2)),,drop=F]
	count.not.sampled    <- count.not.sampled[sort(row.names(count.not.sampled)),,drop=F]
	no.yesRecords        <- no.yesRecords[sort(row.names(no.yesRecords)),,drop=F]
	no.site.visited      <- no.site.visited [sort(row.names(no.site.visited )),,drop=F]
	area.sampled         <- area.sampled[sort(row.names(area.sampled)),,drop=F]
	area.wet             <- area.wet[sort(row.names(area.wet)),,drop=F]
	area.dry             <- area.dry[sort(row.names(area.dry)),,drop=F]
	fish.all             <- fish.all[sort(row.names(fish.all)),,drop=F]
	fish.dead            <- fish.dead[sort(row.names(fish.dead)),,drop=F]
	fish.alive           <- fish.alive[sort(row.names(fish.alive)),,drop=F]
	cat(paste("sort the calculated according to section\n",sep=""))
	cat(paste("sort the calculated according to section\n",sep=""),file=FL.LOG,append=TRUE)



	# combine to a single PlotsSampled Table
	my.table <- cbind(Count.siteVisited = no.site.visited,
			  count.sampled.No  = count.not.sampled,
			  count.sampled.Yes = count.sampled2,
			  count.sampled     = count.sampled1,
			  Plots             = no.yesRecords,
			  area.sampled      = area.sampled,
			  area.wet          = area.wet,
			  area.dry          = area.dry,
			  fish.all          = fish.all,                             
			  fish.dead         = fish.dead,
			  fish.alive        = fish.alive)    
			  
	names(my.table) <-sub("^no.site.visited$",  "no.transect.visited", names(my.table))			  
	names(my.table) <-sub("^count.not.sampled$","transect.not.sampled",names(my.table))			  
	names(my.table) <-sub("^count.sampled$",    "transect.sampled",    names(my.table))		# from aggregated data		  
	names(my.table) <-sub("^count.sampled2$",   "transect.sampled",    names(my.table))		# from raw data			  
	names(my.table) <-sub("^number.sample$",    "plots.sampled",       names(my.table))		# from aggregated data
	names(my.table) <-sub("^no.yesRecords$",    "plots.sampled",       names(my.table))		# from raw data
	names(my.table) <-sub("^area.sampled$",     "area.sampled",        names(my.table))	
	names(my.table) <-sub("^area.wet$",         "area.wet",            names(my.table))
	names(my.table) <-sub("^area.dry$",         "area.dry",            names(my.table))
	names(my.table) <-sub("^fish.dead$",        "fish.dead",           names(my.table))			  
			  

	# added a sum across all sections
	my.table <- rbind(my.table,
			  total=apply(my.table,2,FUN=sum,na.rm=TRUE))
	cat(paste("put the sorted calculated quantities in a Table\n",sep=""))
	cat(paste("put the sorted calculated quantities in a Table\n",sep=""),file=FL.LOG,append=TRUE)


	Table.siteVisited <- rbind(no.site.visited,total=sum(no.site.visited))				# put number of sites visited into a TABLE
	cat(paste("put the site visted alone into a Table\n",sep=""))
	cat(paste("put the site visted alone into a Table\n",sep=""),file=FL.LOG,append=TRUE)
	
	cat("return from \"count.perSection\"\n")

	return(my.table)
}






# -------------------------------------------------------------------------------------------------
# a function to calculate the variety of counts per biweek period:
# inputs: data subset, section index and biweek list of the entire data set
# -------------------------------------------------------------------------------------------------
count.perBiweekSection <- function(my.data,my.allBiweek,my.section.idx)
{
	section.idx <- my.section.idx;
	# overall statistics
		 site.visited <- tapply(my.data[,"transect"],list(my.data[,"biweek.idx"]), FUN=unique)	# get the list of the unique transect surveyed in each section
	      no.site.visited <- data.frame(no.site.visited = sapply(site.visited,length))		# get the number of the unique surveyed transects
	list.biweek.this     <- row.names(no.site.visited)						# list of unique sections occurred in this data set 
	cat(paste("put the actual section occurred in a list which will be used to determine how many section missed in a strata\n",sep=""))
	cat(paste("put the actual section occurred in a list which will be used to determine how many section missed in a strata\n",sep=""),file=FL.LOG,append=TRUE)

	     list.biweek  <- my.allBiweek								# unique section occurred in this data set	
	   missed.biweek <- setdiff(list.biweek,list.biweek.this)					# the strata missed
	no.missed.biweek <- length(missed.biweek)							# the number of missed strata in this boot sample
	cat(paste("count site visited\n",sep=""))
	cat(paste("count site visited\n",sep=""),file=FL.LOG,append=TRUE)
	
	# -------------------------------------------------------------------------------------------------
	# Accumulate Area
	area.sampled <- data.frame(area.sampled = tapply(my.data[,"area"],      list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "area"       in the sampled plot
	area.wet     <- data.frame(area.wet     = tapply(my.data[,"area.wet"],  list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "area.wet"   in the sampled plot
	area.dry     <- data.frame(area.dry     = tapply(my.data[,"area.dry"],  list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "area.dry"   in the sampled plot
	fish.all     <- data.frame(fish.all     = tapply(my.data[,"morts"],     list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "morts"      in the sampled plot
	fish.alive   <- data.frame(fish.alive   = tapply(my.data[,"fish.alive"],list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "fish.alive" in the sampled plot
	fish.dead    <- data.frame(fish.dead    = tapply(my.data[,"fish.dead"], list(my.data[,"biweek.idx"]), FUN=sum,na.rm=TRUE))	# all "fish.dead"  in the sampled plot
	cat(paste("count area and fish in the sampled plot\n",sep=""))
	cat(paste("count area and fish in the sampled plot\n",sep=""),file=FL.LOG,append=TRUE)


	# -------------------------------------------------------------------------------------------------
	# create a field of [no.yesRecords] in in terms of section
	      no.yesRecords <- data.frame(no.yesRecords = tapply(my.data[,"sampled.yes"],list(my.data[,"biweek.idx"]), FUN=sum))	# all "Y" in the sampled field
	cat(paste("count number of plots sampled\n",sep=""))
	cat(paste("count number of plots sampled\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# number of total plots of each unique sample (i.e., date-transect combination): count the number of all quardrants ("sampled"="Y" or "N") for a give transect at a given day 
	#       plots.count  <- aggregate(my.data[,c("sampled.yes")],list(my.data[,"date"],my.data[,"biweek.idx"],my.data[,"transect"]), FUN=length)		# the number of records of a "date-section-transect" combination
	# names(plots.count) <- c("date","biweek.idx","transect","plots.count.all")
	        plots.count  <- aggregate(my.data[,c("sampled.yes")],list(                 my.data[,"biweek.idx"],my.data[,"transect"]), FUN=length)		# the number of records of a "date-section-transect" combination
	  names(plots.count) <- c(       "biweek.idx","transect","plots.count.all")

	# number of sampled plots of each unique sample (i.e., date-transect combination):  count the number of "Y" quardrants ("sampled"="Y")        for a give transect at a given day 
	#       plots.yes    <- aggregate(my.data[,c("sampled.yes")],list(my.data[,"date"],my.data[,"biweek.idx"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-section-transect" combination
	# names(plots.yes)   <- c("date","biweek.idx","transect","plots.count.yes")
	        plots.yes    <- aggregate(my.data[,c("sampled.yes")],list(                 my.data[,"biweek.idx"],my.data[,"transect"]), FUN=sum,na.rm=T)	# the number of sampled "Yes" records of a "date-section-transect" combination
	  names(plots.yes)   <- c(       "biweek.idx","transect","plots.count.yes")

	# number of not-sampled plots of each unique sample (i.e., date-transect combination):  count the number of "N" quardrants ("sampled"="N")        for a give transect at a given day 
	#       plots.no     <- aggregate(my.data[,c("sampled.no")],list(my.data[,"date"],my.data[,"biweek.idx"],my.data[,"transect"]), FUN=sum,na.rm=T)		# the number of sampled "Yes" records of a "date-section-transect" combination
	# names(plots.no)    <- c("date","biweek.idx","transect","plots.count.no")
	        plots.no     <- aggregate(my.data[,c("sampled.no")],list(                  my.data[,"biweek.idx"],my.data[,"transect"]), FUN=sum,na.rm=T)		# the number of sampled "Yes" records of a "date-section-transect" combination
	  names(plots.no)    <- c(       "biweek.idx","transect","plots.count.no")
	cat(paste("count numbers of total, sampled and not-sampled plots in the sections\n",sep=""))
	cat(paste("count numbers of total, sampled and not-sampled plots in the sections\n",sep=""),file=FL.LOG,append=TRUE)

	# assemble the counts into a dataframe.  The numbers of total plots, sampled plots and not sampled plots are used to create the "Plots Sampled" and "Plots Not Sampled" statistics
	my.data.plots <- cbind(plots.count,
			       plots.count.yes = plots.yes[,"plots.count.yes"],
			       plots.count.no  = plots.no[,"plots.count.no"])                

	# assign "plots Sampled No":	if total plots == not sampled plots, i.e., plots.count == plots.no,  assign 1 otherwise 0  
	#        "plots Sampled Yes":	if total plots ==     sampled plots, i.e., plots.count == plots.yes, assign 1 otherwise 0  
	#        "plots Sampled YesNo":	if sampled plots > 0,                i.e., plots.count.yes > 0,        assign 1 otherwise 0    This is the "plots Sampled Yes" in the summary tab "Stranding Summary"
	my.data.plots <- cbind(my.data.plots,
			       plots.no    = rep(0,dim(my.data.plots)[1]),	# initialize "Plots Sampled : No"    with 0 
			       plots.yes   = rep(0,dim(my.data.plots)[1]),	# initialize "Plots Sampled : Yes"   with 0
			       plots.yesNo = rep(0,dim(my.data.plots)[1]))	# initialize "Plots Sampled : YesNo" with 0            
	# assign values
	my.data.plots[my.data.plots[,"plots.count.all"] == my.data.plots[,"plots.count.no"], "plots.no"]    <- 1	# the sum of sampled "no"  in the data packet is the same as the length of the data packet, means all records in the data packet are "No".
	my.data.plots[my.data.plots[,"plots.count.all"] == my.data.plots[,"plots.count.yes"],"plots.yes"]   <- 1	# the sum of sampled "yes" in the data packet is the same as the length of the data packet, means all records in the data packet are "Yes".
	my.data.plots[my.data.plots[,"plots.count.yes"] > 0,                                 "plots.yesNo"] <- 1	# the sum of sampled "yes" in the data packet is not zero                                 , means at least there is sampled "Yes" records
	cat(paste("creat a data.frame of [my.data.plots]\n",sep=""))
	cat(paste("creat a data.frame of [my.data.plots]\n",sep=""),file=FL.LOG,append=TRUE)

	# -------------------------------------------------------------------------------------------------
	# count plots sampled "No" and "Yes"
	 count.sampled1    <- data.frame(count.sampled1    = tapply(my.data.plots[,"plots.yes"],  list(my.data.plots[,"biweek.idx"]), FUN=sum))
	 count.sampled2    <- data.frame(count.sampled2    = tapply(my.data.plots[,"plots.yesNo"],list(my.data.plots[,"biweek.idx"]), FUN=sum))	# this is "Plots Sampled Yes" in the summary tab "Stranding Summary"
	 count.not.sampled <- data.frame(count.not.sampled = tapply(my.data.plots[,"plots.no"],   list(my.data.plots[,"biweek.idx"]), FUN=sum))	# this is "Plots Sampled No"  in the summary tab "Stranding Summary"
	cat(paste("the count the Yes/No transect/qyardrant\n",sep=""))
	cat(paste("the count the Yes/No transect/qyardrant\n",sep=""),file=FL.LOG,append=TRUE)

	# add the missed strata to the statistics arrays
	if (no.missed.biweek > 0)
	{
		cat(paste("(section ",section.idx,"): missed sampling in ",no.missed.biweek," biweek periods\n",sep=""))
		cat(paste("(section ",section.idx,"): missed sampling in ",no.missed.biweek," biweek periods\n",sep=""),file=FL.LOG,append=TRUE)
		for (add.biweek in missed.biweek)
		{
			command.string <- paste("count.sampled1  <- rbind(count.sampled1,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.sampled2  <- rbind(count.sampled2,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("count.not.sampled  <- rbind(count.not.sampled,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("no.yesRecords  <- rbind(no.yesRecords,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("no.site.visited  <- rbind(no.site.visited,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.sampled  <- rbind(area.sampled,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("area.wet  <- rbind(area.wet,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))
			
			command.string <- paste("area.dry  <- rbind(area.dry,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))
			
			
			command.string <- paste("fish.all  <- rbind(fish.all,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))

			command.string <- paste("fish.alive  <- rbind(fish.alive,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))	

			command.string <- paste("fish.dead  <- rbind(fish.dead,\"", add.biweek,"\"=NA)",sep="")
			eval(parse(text=command.string))			
		}
		cat(paste("(section ",section.idx,"): missing biweek periods have been inserted\n",sep=""))
		cat(paste("(section ",section.idx,"): missing biweek periods have been inserted\n",sep=""),file=FL.LOG,append=TRUE)				
	}

	# sorted according to section index
	count.sampled1       <- count.sampled1[sort(row.names(count.sampled1)),,drop=F]
	count.sampled2       <- count.sampled2[sort(row.names(count.sampled2)),,drop=F]
	count.not.sampled    <- count.not.sampled[sort(row.names(count.not.sampled)),,drop=F]
	no.yesRecords        <- no.yesRecords[sort(row.names(no.yesRecords)),,drop=F]
	no.site.visited      <- no.site.visited [sort(row.names(no.site.visited )),,drop=F]
	area.sampled         <- area.sampled[sort(row.names(area.sampled)),,drop=F]
	area.wet             <- area.wet[sort(row.names(area.wet)),,drop=F]
	area.dry             <- area.dry[sort(row.names(area.dry)),,drop=F]
	fish.all             <- fish.all[sort(row.names(fish.all)),,drop=F]
	fish.dead            <- fish.dead[sort(row.names(fish.dead)),,drop=F]
	fish.alive           <- fish.alive[sort(row.names(fish.alive)),,drop=F]
	cat(paste("sort the calculated according to section\n",sep=""))
	cat(paste("sort the calculated according to section\n",sep=""),file=FL.LOG,append=TRUE)

	# combine to a single PlotsSampled Table
	my.table <- cbind(Count.siteVisited = no.site.visited,
			  count.sampled.No  = count.not.sampled,
			  count.sampled.Yes = count.sampled2,
			  count.sampled     = count.sampled1,
			  Plots             = no.yesRecords,
			  area.sampled      = area.sampled,
			  area.wet          = area.wet,
			  area.dry          = area.dry,
			  fish.all          = fish.all,                             
			  fish.dead         = fish.dead,
			  fish.alive        = fish.alive)    
			  
	names(my.table) <-sub("^no.site.visited$",  "no.transect.visited", names(my.table))			  
	names(my.table) <-sub("^count.not.sampled$","transect.not.sampled",names(my.table))			  
	names(my.table) <-sub("^count.sampled$",    "transect.sampled",    names(my.table))		# from aggregated data		  
	names(my.table) <-sub("^count.sampled2$",   "transect.sampled",    names(my.table))		# from raw data			  
	names(my.table) <-sub("^number.sample$",    "plots.sampled",       names(my.table))		# from aggregated data
	names(my.table) <-sub("^no.yesRecords$",    "plots.sampled",       names(my.table))		# from raw data
	names(my.table) <-sub("^area.sampled$",     "area.sampled",        names(my.table))			  	
	names(my.table) <-sub("^area.wet$",         "area.wet",            names(my.table))
	names(my.table) <-sub("^area.dry$",         "area.dry",            names(my.table))
	names(my.table) <-sub("^fish.dead$",        "fish.dead",           names(my.table))			  


	# added a sum across all sections
	my.table <- rbind(my.table,
			  total=apply(my.table,2,FUN=sum,na.rm=TRUE))
	cat(paste("put the sorted calculated quantities in a Table\n",sep=""))
	cat(paste("put the sorted calculated quantities in a Table\n",sep=""),file=FL.LOG,append=TRUE)


	Table.siteVisited <- rbind(no.site.visited,total=sum(no.site.visited))				# put number of sites visited into a TABLE
	cat(paste("put the site visted alone into a Table\n",sep=""))
	cat(paste("put the site visted alone into a Table\n",sep=""),file=FL.LOG,append=TRUE)
	
	cat("return from \"count.perBiweek\"\n")
	
	return(my.table)
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

