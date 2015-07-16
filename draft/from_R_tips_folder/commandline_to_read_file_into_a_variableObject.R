# an example to read data into an object which is a variable
# Jan 10, 2012
#                               ----              ---                    ---               --- (need to put it in "")
	command.string <- paste("\"",object.blind,"\" <- read.table(file=\"",FL.IN.blind,  "\",header=TRUE,colClasses = c(\"numeric\",\"numeric\",\"numeric\",\"character\"),sep=\"\")",sep="")
	eval(parse(text=command.string))

# When [this.dataObj] is a variable which is an object, to assign this data object to a new data object [myData.EPlus], do the folloiwng
# note: we  shold not to put quote around [this.dataObject]                
	command.string <- paste("myData.EPlus <- ",this.dataObj,sep="")
	eval(parse(text=command.string))
