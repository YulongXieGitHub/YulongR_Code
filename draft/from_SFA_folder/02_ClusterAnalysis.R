#
# 02_ClusterAnalysis.R 
#
# http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
#
# Sep 13, 2014
#

# eliminate all stuff
rm(list = ls(all = TRUE))



# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# library("reshape2")
# library("lattice")
# library(latticeExtra)
# library(missMDA)
# library(cluster)
# library(HSAUR)
# library(fpc)
# library(chron)

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

col.array <- c("red","blue","green","magenta","cyan","black","purple","pink","brown")

# today's month, day and year in the format of "Thu Jun 16 08:48:36 2011", 5 fields separated by space
today.month  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
today.day    <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
today.year   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]
today.hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
today.minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
today.second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]
cat(paste("1: specification.\n",sep=""))
# ------------------------------------------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Script  <- "/phome/resstd/mh_rule/vm/2014analysis/0_script"
	Path.Project <- "/phome/resstd/mh_rule/vm/2014analysis"
}else{
	Path.Script  <- "D:/YuLong_Projects/Yulong_Geology/FY2014_SFA/0_scripts"
	Path.Project <- "D:/YuLong_Projects/Yulong_Geology/FY2014_SFA"
}


Well.Shallow <- c("2-7","2-8","2-9","2-11","2-12","2-13","2-14","2-15","2-16","2-17","2-18","2-19","2-20","2-21","2-22","2-23","2-24","2-26","2-29","2-34","2-37","3-23","3-24","3-25","3-27","3-28","3-29","3-30","3-35")




# **********************************************************************************
# log file
# **********************************************************************************
Path.LOG  <- paste(Path.Project,"0_log",    sep="/")
Path.Cord <- paste(Path.Project,"0_data",   sep="/")
if (!file.exists(Path.LOG)) {print(paste("NOT existing:",Path.LOG));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Cord)){print(paste("NOT existing:",Path.Cord," Check Why!\n"));die}


FL.LOG  <- paste(Path.LOG,"02_ClusterAnalysis.log",sep="/")
FL.Cord <- paste(Path.Cord,"well_list_full.txt",sep="/")
if   (file.exists(FL.LOG))  {print(paste(FL.LOG, "exist.Delete it!"));file.remove(FL.LOG)}	
if (!(file.exists(FL.Cord))){print(paste(FL.Cord," does not exist. Check why!"));die}	
cat(paste("1: defined path and file names.\n",sep=""))
cat(paste("1: defined path and file names.\n",sep=""),file=FL.LOG,append=TRUE)



# read the well coordinate data
myXY <- read.table(file=FL.Cord,sep="\t",header=TRUE)
names(myXY) <- c("WellID","X","Y","Elev(m)","top_screen_ft","bottom_screen_ft","HR_Depth")

# remove "399-" from the wellID
myXY[,"WellID"] <- gsub("399-","",myXY[,"WellID"])

# change "#-0#" to "#-#'
myXY[,"WellID"] <- sub("-0","-",myXY[,"WellID"])


for (this.year in c(2011,2010))
{
	Path.IN   <- paste(Path.Project,"1_results",this.year,sep="/")
	Path.OUT  <- paste(Path.Project,"2_ClusterAnalysis",this.year,sep="/")
	if (!file.exists(Path.IN)) {print(paste("NOT existing:",Path.IN," Check Why!\n"));die}
	if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
	
	FL.Data <- paste(Path.IN,  paste("verify_merged_",this.year,"_complete_Imput.Rdata",sep=""),sep="/")
	FL.PDF  <- paste(Path.OUT, "ClusterAnalysis.pdf",sep="/")
	if (!(file.exists(FL.Data))){print(paste(FL.Data," does not exist. Check why!"));die}	
	if   (file.exists(FL.PDF))  {print(paste(FL.PDF, "exist.Delete it!"));file.remove(FL.PDF)}	

	# OPEN PDF FILE
	pdf(file = FL.PDF,paper="special", width=17, height=11,bg = "transparent")

	# load data
	load(FL.Data)
	myData <- t(myData.U.complete)	# which is a 90 by 26 data frame



	### myData.Wide.U[,"chron.date"] <-  chron(dates  = myData.Wide.U[,"date"],
	### 		                       times  = myData.Wide.U[,"time"],
	### 		                       format = c('m/d/y','h:m:s'))


	# *********************************************************************************
	# 1. kMean
	# *********************************************************************************
	wss <- (nrow(myData)-1)*sum(apply(myData,2,var))
	  for (i in 2:15) wss[i] <- sum(kmeans(myData,
					       centers=i)$withinss)
	plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares",main=paste("Year ",this.year,": 1. Kmeans: Within groups sun of squares using Kmeans.",sep=""))
	cat(paste("1. kMeans\n",sep=""))     



	for (no.cls in c(3,4,5,6,7,8,9))
	{
		if (no.cls <=3)
		{
			par(mfrow=c(2,2))
		}else if (no.cls <= 5)
		{
			par(mfrow=c(2,3))
		}else if (no.cls <= 8)
		{
			par(mfrow=c(3,3))
		}else if (no.cls >=9)
		{
			par(mfrow=c(3,4))
		}

		# check the location of the wells when no of cluster = 5
		# use 5 classes Kmean
		model.kMeans  <- kmeans(myData,centers=no.cls)
		class.label     <- model.kMeans$cluster

		# put class index and well coordinate into a data frame
		myData.chron <- chron(dates = paste(sub("(.*)-(.*)-(.*)","\\2",colnames(myData)),sub("(.*)-(.*)-(.*)","\\3",colnames(myData)),sub("(.*)-(.*)-(.*)","\\1",colnames(myData)),sep="/"),
				      times = "0:0:0",
				      format = c('m/d/y','h:m:s'))
		myTmp <- data.frame(WellID    = rownames(myData),
				    class.label = class.label)

		# merge the cluster index with the XY coordinates
		myMerged <- merge(myTmp[,c("WellID","class.label")],myXY[,c("WellID","X","Y")],by=c("WellID"),all=FALSE)                     
		rownames(myMerged) <- myMerged[,"WellID"]
		cat(paste("1B. Kmean for 5 clusters of year ",this.year,".\n",sep=""))
		cat(paste("1B. Kmean for 5 clusters of year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

		# time series of the well in each class

		for (this.class in seq(1:no.cls))
		{
			idx.this.cls     <- names(class.label)[class.label==this.class]
			U.this.cls       <- myData[idx.this.cls,]
			U.range.this.cls <- range(U.this.cls,na.rm=TRUE)


			plot(myData[idx.this.cls[1],] ~ myData.chron,type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",gsub("\\.loess","",paste(idx.this.cls,collapse=","))),sep=""))
			if (length(idx.this.cls) > 1)
			{
				for (count in seq(from=2,to=length(idx.this.cls)))
				{
					lines(myData[idx.this.cls[count],] ~ myData.chron,pch=16,col=col.array[count])
				}
			}

		}
		cat(paste("1C. overlay time series of the well in the classes of 5 cls kmean of year ",this.year,".\n",sep=""))
		cat(paste("1C. overlay time series of the well in the classes of 5 cls kmean of year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

		# plot the well coordinate in each class
		count.cls <- 0
		xlim <- range(myMerged[,"X"])
		ylim <- range(myMerged[,"Y"])
		for (this.class in seq(1:no.cls))
		{
			count.cls <- count.cls + 1

			# idx.this.cls     <- names(class.label)[class.label==this.class]
			myCoord          <- myMerged[myMerged[,"class.label"] ==this.class,]

			if (count.cls ==1)
			{
				plot(myCoord[idx.this.cls[1],"X"], myCoord[idx.this.cls[1],"Y"],type="p",pch=16,col=col.array[count.cls],xlim=xlim,ylim=ylim,xlab="X",ylab="Y",main=paste("Year ",this.year,": Well Locations (",no.cls,") classes from kMeans",sep=""))
			}else{
				points(myCoord[idx.this.cls[1],"X"], myCoord[idx.this.cls[1],"Y"],pch=16,col=col.array[count.cls])
			}

			if (length(idx.this.cls) > 1)
			{
				for (count in seq(from=2,to=length(idx.this.cls)))
				{
					points(myCoord[idx.this.cls[count],"X"], myCoord[idx.this.cls[count],"Y"],pch=16,col=col.array[count.cls])
				}
			}
			text(myCoord[,"X"],myCoord[,"Y"],labels=rownames(myCoord),col=col.array[count.cls],cex=1)	
		}
	}



	# *********************************************************************************
	# 2. fpc package: Flexible procedures for clustering: 'princomp' can only be used with more units than variables (http://cran.r-project.org/web/packages/fpc/fpc.pdf)
	# *********************************************************************************
	par(mfrow=c(1,2))
	library(fpc)
	pamk.best <- pamk(t(myData))		# Partitioning around medoids with estimation of number of clusters
	cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
	plot(pam(t(myData), pamk.best$nc),main = paste("Year ",this.year,": 2. fpc package",sep=""))

	asw <- numeric(20)
	for (k in 2:20)
	  asw[[k]] <- pam(myData, k) $ silinfo $ avg.width
	k.best <- which.max(asw)
	cat("silhouette-optimal number of clusters:", k.best, "\n")
	detach(package:fpc)
	cat(paste("2. fpc package of year ",this.year,"\n",sep=""))

	# *********************************************************************************
	# 3: vegan package: Community Ecology Package  (http://cran.r-project.org/web/packages/vegan/vegan.pdf)
	# *********************************************************************************
	library(vegan)
	fit <- cascadeKM(scale(myData, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
	plot(fit, sortg = TRUE, grpmts.plot = TRUE,main=paste("Year ",this.year,": 3. vegan",sep=""))
	calinski.best <- as.numeric(which.max(fit$results[2,]))
	cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
	detach(package:vegan)
	cat(paste("3. vegan package of year ",this.year,"\n",sep=""))

	# *********************************************************************************
	# 4, Mclust: Normal Mixture Modeling for Model-Based Clustering,Classification, and Density Estimation (http://cran.r-project.org/web/packages/mclust/mclust.pdf)
	# *********************************************************************************
	par(mfrow=c(3,1))
	library(mclust)
	d_clust <- Mclust(myData, G=1:7,modelNames = c("EII", "VII", "EEI", "EVI", "VEI", "VVI"))  	# ,"EEE","EEV","VEV","VVV"))
	myModel.BIC <- mclustBIC(myData,G=1:20)
	plot(myModel.BIC,main=paste("Year ",this.year,": 4. Mclust",sep=""))


	myModel.BIC.prior <- mclustBIC(myData,prior=priorControl(),G=1:20)
	plot(myModel.BIC.prior,main=paste("Year ",this.year,": 4. Mclust with Prior",sep=""))


	d_clust <- Mclust(myData, G=5,modelNames = c("EII", "VII", "EEI", "EVI", "VEI", "VVI"))  	# ,"EEE","EEV","VEV","VVV"))
	classes <- d_clust$classification
	class1 <- classes[classes==1]
	class2 <- classes[classes==2]
	class3 <- classes[classes==3]
	class4 <- classes[classes==4]
	class5 <- classes[classes==5]

	m.best <- dim(d_clust$z)[2]
	cat("model-based optimal number of clusters:", m.best, "\n")
	plot(d_clust)
	detach(package:mclust)
	cat(paste("4. Mclust package of year ",this.year,"\n",sep=""))


	# *********************************************************************************
	# 5, apcluster: Affinity Propagation Clustering (http://cran.r-project.org/web/packages/apcluster/apcluster.pdf)
	# *********************************************************************************
	par(mfrow=c(1,2))
	library(apcluster)
	d.apclus <- apcluster(negDistMat(r=2), myData)
	cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")

	heatmap(d.apclus,main="apcluster")
	# plot(d.apclus, myData,main=paste("Year ",this.year,": 5. apcluster",sep=""))

	# aggre
	aggModel <- aggExCluster(negDistMat(r=2), myData)
	plot(aggModel,main="5. agg apcluster")
	detach(package:apcluster)
	cat(paste("5. apclsuter package of year ",this.year,"\n",sep=""))

	# *********************************************************************************
	# 6. clsuter: Cluster Analysis Extended Rousseeuw et al (http://cran.r-project.org/web/packages/cluster/cluster.pdf)
	# *********************************************************************************
	par(mfrow=c(1,1))
	library(cluster)
	myModel <- clusGap(myData, kmeans, 10, B = 100, verbose = interactive())
	plot(myModel[[1]][,"gap",drop=FALSE],xlab = "Number of Clsuter",ylab="Gap",main=paste("Year ",this.year,": 6. cluster: Estimating The Number of Clsuters via the Gap Statistic",sep=""))
	detach(package:cluster)
	cat(paste("6. cluster package of year ",this.year,"\n",sep=""))

	# 7. NbClust)
	# library(NbClust)
	# nb <- NbClust(myData, diss="NULL", distance = "euclidean", 
	#         min.nc=2, max.nc=15, method = "kmeans", 
	#         index = "alllong", alphaBeale = 0.1)
	# hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
	# There are only 26 nonmissing observations out of a possible 26 observations.Error in NbClust(myData, diss = "NULL", distance = "euclidean", min.nc = 2,  : 
	#  The TSS matrix is indefinite. There must be too many missing values. The index cannot be calculated.
	cat(paste("7. NbClust package of year ",this.year,"\n",sep=""))


	# *********************************************************************************
	# 8. 
	# *********************************************************************************
	d_dist <- dist(myData)   # find distance matrix 
	plot(hclust(d_dist),xlab="Well Name",ylab="Distance",main=paste("Year ",this.year,": 8. Cluster Dendrogram",sep=""))  
	cat(paste("8. dendrogram of year ",this.year,"\n",sep=""))


	# 9. package bclust: http://cran.r-project.org/web/packages/bclust/bclust.pdf
	par(mfrow=c(1,1))
	library(bclust)
	d.bclus <- bclust(myData, transformed.par = c(0, -50, log(16), 0, 0, 0))
	# viplot(imp(d.bclus)$var); 
	plot(d.bclus,main=paste("Year ",this.year,": 9. blcust",sep="")); 
	ditplot(d.bclus); 
	dptplot(d.bclus, scale = 20, horizbar.plot = TRUE,varimp = imp(d.bclus)$var, horizbar.distance = 0, dendrogram.lwd = 2)
	# I just include the dendrogram here
	detach(package:bclust)
	cat(paste("9. bclust package of year ",this.year,"\n",sep=""))

	# *********************************************************************************
	# 10. pvclust
	# *********************************************************************************
	library(pvclust)
	library(MASS)
	model.pv <- pvclust(t(myData))
	plot(model.pv,main=paste("Year ",this.year,": 10. pvclsut",sep=""))
	detach(package:pvclust)
	detach(package:MASS)
	cat(paste("10. pvclust package of year ",this.year,"\n",sep=""))

	dev.off()
}




# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n02_ClusterAnalysis.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n02_ClusterAnalysis.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [02_ClusterAnalysis.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [02_ClusterAnalysis.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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
	# cat(paste(pkg.loaded[[i]]$Package,":",pkg.loaded[[i]]$Version," ",pkg.loaded[[i]]$Date,"\n",sep=" "),file=FL.LOG,append=TRUE)
}




# 