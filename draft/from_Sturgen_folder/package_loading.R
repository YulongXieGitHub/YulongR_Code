#
# package_loading
#
# this is to check the available packages against the needed packages, and install the missing one.
#
# -----------------------------------------------------
# to detach a already loaded package
# -----------------------------------------------------
# detach("package:timeDate",character.only=TRUE)
# or
# pkg <- "package:timeDate"
# detach(pkg,character.only=TRUE)
# 
# -----------------------------------------------------
# load already installed packages
# -----------------------------------------------------
#

args <- commandArgs(trailingOnly = TRUE)
type.analysis = "glm"


packages.desired   <- c("akima","bitops","coda","caTools","chron","cshapes","cwhmisc","data.table","Defaults","fortunes","gplots","gtools","iterators","itertools","lme4","locfit","maptools","mlmRev","neuralnet","plyr","psych","quantmod","reshape","reshape2","rjags","rJava","RODBC","scatterplot3d","sp","splus2R","stringr","survey","timeDate","TTR","xts","zoo")
packages.all.needed    <- c("chron","RODBC","timeDate","stats","lattice","graphics")
packages.glm.needed   <- c("R2WinBUGS","BRugs","coda","rjags","sandwich","lmtest","MASS","car","pscl")
packages.bayes.needed <- c("R2WinBUGS","BRugs","coda","rjags","sandwich","lmtest","MASS","car","pscl")

if (length(grep("Bayes",type.analysis, ignore.case=TRUE)))
{
	packages.needed <- c(packages.bayes.needed,packages.all.needed)
}else if (length(grep("glm",type.analysis, ignore.case=TRUE)))
{
	packages.needed <- c(packages.glm.needed,packages.all.needed)
}else
{
	packages.needed <- packages.all.needed
}

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



              