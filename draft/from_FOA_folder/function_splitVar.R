
##  Need to clean up Yulong's pasted values for example ###

#' Strip pasted variables
split_variables = function(x,value_column="value",label_column="item"){
	xdata = x[,c(value_column,label_column)]
	xdata[,1] = as.character(xdata[,1])
	xdata[,2] = as.character(xdata[,2])
	out = ddply(.data=xdata,.variables=label_column,.fun=function(x){
	splits = strsplit(x[,1],"_")	
	splits_df = data.frame(do.call("rbind",splits),stringsAsFactors=FALSE)
	colnames(splits_df) = rep(x[1,2],ncol(splits_df))
	splits_df = data.frame(splits_df,stringsAsFactors=FALSE)

	## fix know character concatenations
	# Current known is I, II, and III.  These are quality of installation values
	#I = 1, II=.95, and III = .9
	for (i in 1:ncol(splits_df)){
		splits_df[,i] = gsub("I","1",gsub("II","2",gsub("III","3",splits_df[,i])))
		splits_df[,i] = as.numeric(splits_df[,i])
	}
	temp = melt(splits_df,na.rm=TRUE)
	temp$variable = as.character(temp$variable)
	temp

	} )
	out$variable_2 = tolower(gsub("Compound","",out$variable))
	out
}

