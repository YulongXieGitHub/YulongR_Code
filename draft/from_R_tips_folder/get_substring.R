# -------------------------------------------------------------------------------------------------
# example 1
# -------------------------------------------------------------------------------------------------
standards.array <- c("IECCplus.state_STD2012" "IECC.state_STD2009" )
standard.year   <- sub("(.*[^\\d+])(\\d+)(.*)","\\2",standards.array,perl=TRUE)	# the first part is everything without digits, the second parts is a digit string and the 3rd part is everything after that digit string
# we will get "2012" and "2009"

	
# -------------------------------------------------------------------------------------------------
# example 2
# -------------------------------------------------------------------------------------------------
	
	
	# 4. only keep the Daylight illum fields
	tmp.data <- tmp.data[,grep("Daylight\\.Illum",names(tmp.data))]
	
	# 5. use more meaningful field for the illuminance fields
	field.names <- names(tmp.data)
	field.names <- sub("[a-zA-Z]+_([a-zA-Z]+_[a-zA-Z]+_\\d+)\\D+(\\d+)\\D+","\\1_sensor\\2",field.names,perl=TRUE)	# note: cannot rely on the heading of the csv file to determine the pattern.  Need to look at the head after data are loaded into R.

	# further replcae ZB index with facade label and replace sensor 1 & 2 with "primIllLux" and "secIllLux" to be consistent with Radiance output file
	field.names <- sub("ZN_1","south",field.names,perl=TRUE)
	field.names <- sub("ZN_2","east", field.names,perl=TRUE)
	field.names <- sub("ZN_3","north",field.names,perl=TRUE)
	field.names <- sub("ZN_4","west", field.names,perl=TRUE)
	field.names <- sub("sensor1","primIllLux",field.names,perl=TRUE)
	field.names <- sub("sensor2","secIllLux", field.names,perl=TRUE)
