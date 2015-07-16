# pairs and splom in lattice
data(USArrests)
pairs(USArrests)
splom(~USArrests)
splom(~USArrests,panel=function(x,y) {panel.xyplot(x,y);panel.loess(x,y)})


library(lattice)
trellis.device()

data(Oxboys,package="nlme")	# need to install the "nlme" package first
xyplot(height~age | Subject,data=Oxboys,ylab="Height (cm)")

xyplot(height~age | Subject,data=Oxboys,ylab="Height (cm)",
          aspect="xy",		# calculate an optimal aspect ratio
          panel = function(x,y) {panel.grid();panel.xyplot(x,y)})
          
          
          

cat("2,3,5,7","11,13,15,17",file="test.dat",sep="\n")
 scan(file="test.dat",what=list(x=0,y=0,z=0,t=0),flush=TRUE,sep=",")
 
 
 state.names <- c("WA","VT","OR","NV","WY")
grep('W.', state.name)
state.name
grep('W.', state.name)
state.name[grep('W.', state.name)]
state.name[grep('W*', state.name)]
state.name[grep('y', state.name)]
state.name[grep('^L', state.name)]


# Load up the test data...
load("c:\wherever\test.Rdata")

data<-data("Turnout",package="lattice")
options(prompt="R> ")

#
# Chapter 1 of "A Handbook of Statistical Analysis Using R, Brian S Everitt and Torsten Horthorn"
#
# Change the prompt
options(prompt="R> ")	

# Install a package from URL
install.packages("sandwich")

# load a package
library("sandwich")

# check the packages in the library
library()

# check the content of a package
help(package="e1071")

# vignette
 vignette("sandwich",package="sandwich")

# load a source file (note R requires a forward slash)
source("../HSAUR/R/tables.R")

# load a data frame from a package
data("Forbes2000",package="HSAUR")

# check the class of the data frame
class(Forbes2000)

# check the content of the data frame
str(Forbes2000)

# check the variable name in data frame
names(Forbes2000)

# create a data frame
state=seq(1:10)
city<-log(state)+rnorm(state,sd=0.2)
province=rep(c("AB","BC"),c(6,4))	# "AB" repeat 6 times and "BC" repeats 4 times
dframe<-data.frame(state,city,province)
str(dframe)
names(dframe)
levels(dframe[,"state"])	# NOTE: (1) the quotation mark is needed; (2) NULL will be retrned for non-categorical variables
nlevels(dframe[,"province"])
row.names(dframe)		# get the row names of the data frame
row.names(dframe)<-c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10")	# re-assign row names
