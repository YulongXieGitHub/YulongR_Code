library(ggplot2)
library(plyr)
library(reshape)

setwd("C:\\Users\\d3m793\\Copy\\Codes\\Analysis\\")

alabama = readRDS("AL_2015Jun15_4sample.rds")

### Code Requirements 
#### not usable quickly as the variable names and scales do not appear to match.
IECC_2015 = readRDS("IECC_2015.rsd")
IECC_2009 = readRDS("IECC_2009.rsd")



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
		splits_df[,i] = gsub("I","1",gsub("II","0.95",gsub("III","0.90",splits_df[,i])))
		splits_df[,i] = as.numeric(splits_df[,i])
	}
	temp = melt(splits_df,na.rm=TRUE)
	temp$variable = as.character(temp$variable)
	temp

	} )
	out$variable_2 = tolower(gsub("Compound","",out$variable))
	out
}


alabama_long = split_variables(x=alabama)


ggplot(data=alabama_long)+
geom_jitter(aes(x=variable_2,y=value))+
#geom_hline(data=IECC_2009,aes(y=CZ2))+
theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
	  axis.text.y=element_text(colour="black"))+
labs(y="Code Item Observed Range",x="",title="Example Switching from geom_hist to geom_jitter()")+
facet_wrap(~variable_2,scales="free")
ggsave("yulong_code_item_distribution_example.png",width=7,height=10)