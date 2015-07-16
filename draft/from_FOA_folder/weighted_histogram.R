library(ggplot2)
library(grid)
library(scales)


#' @title Plot a weighted histogram based on input weights for each group
#' Built to take data from two groups where the observed data is equal in each
#' group but the histogram needs to show the distribution proportion to the population
#' in the total population covering both groups.
#' @param data A data frame with at least the two columns needed to make the histogram
#' @param value_column The column in 'data' that contains the values to be used in the histogram
#' @param group_column The column in 'data' that contains the grouping factor (needs to be a factor)
#' @param weight_data  The group weighting to be used with the histogram.  A data.frame with two columns.
#' One column of the same name as 'group_column' and with the same factor labels as text.  A second column
#' with the proportional weighting that sums to 1 with the column name 'weight'.
#' @param type Defaults to 'over' which is similar to 'dodge' in ggplot but does overlay instead of dodge. 
#' The other option is 'stack' which is the default in 'ggplot2' for 'geom_histogram' 
#' @param overlay_alpha This is the alpha shading value used in 'ggplot2' for the groups 2 through number of groups
#' @examples sims = 25000
#' groupA = data.frame(value=rnorm(sims,25,5),group="A")
#' groupB = data.frame(value=rnorm(sims,21,2),group="B")
#' data_equal = rbind(groupA,groupB)
#' weighted_histogram(data=data_equal,weight_data=data.frame(group=c("A","B"),weight=c(.8,.2)))
#' weighted_histogram(data=data_equal,weight_data=data.frame(group=c("A","B"),weight=c(.8,.2)),type="stack")
#' @export
#' @seealso \code{\link(ggplot2)}
#' @return a ggplot object

weighted_histogram = function(data,value_column="value",group_column="group",weight_data,type="over",overlay_alpha=1){
	if (!is.factor(data[,group_column])) stop ("The 'group_column' is not a factor")
	if (!is.numeric(data[,value_column])) stop ("The 'value_column' is not numeric")
	# build data.frame with factor group numbers, labels and the correct weights
	scale_values = data.frame(group_number=1:length(levels(data[,group_column])),
				   group_name=levels(data[,group_column]),stringsAsFactors=FALSE)
	# build a check on merge
	pre_dim = nrow(scale_values)

	scale_values = merge(scale_values,weight_data,by.x="group_name",by.y=group_column)

	if (pre_dim != nrow(scale_values)) stop("Merge did not work correctly. Are your group names the same?")

	eplot = ggplot(data=data,aes_string(x=value_column,fill=group_column))+geom_histogram(colour=I("white"))
	eplot_data = ggplot_build(eplot)

	tweak = eplot_data$data[[1]]
	# > head(tweak)
	#      fill y count         x   ndensity     ncount     density PANEL group ymin ymax      xmin     xmax
	# 1 #F8766D 0     0  9.093506 0.00000000 0.00000000 0.000000000     1     1    0    0  8.487272  9.69974
	# 2 #00BFC4 0     0  9.093506 0.00000000 0.00000000 0.000000000     1     2    0    0  8.487272  9.69974

	tweak_plot = tweak[,c("count","x","group")]
	tweak_plot$count_scaled = NA
	runs = rep(NA,nrow(scale_values))
	for (j in 1:nrow(scale_values)){
		runs[j] = sum(subset(tweak,group==j)$count)
		tweak_plot$count_scaled[tweak_plot$group==j] = tweak_plot$count[tweak_plot$group==j]*scale_values$weight[scale_values$group_number==j]
		if (j > 1) if (!identical(runs[j-1],runs[j])) stop ("Observations in each group are not equal")
		}
	tweak_plot$runs = runs[1]
	if (type=="over"){
		out=ggplot(data =subset(tweak_plot,group==1),aes(x=x,y=count_scaled/runs,fill=factor(group)))+
		geom_histogram(stat="identity",colour=I("white"))
		for (j in 2:nrow(scale_values)){
		out = out + geom_histogram(data=subset(tweak_plot,group==j),stat="identity",colour=I("white"),alpha=overlay_alpha) 	
		}
	}
	if (type=="stack"){
		out=ggplot(data =tweak_plot,aes(x=x,y=count_scaled/runs,fill=factor(group)))+
		geom_histogram(stat="identity",colour=I("white"))
	}
	out= out+scale_y_continuous(labels=percent)+
		scale_fill_discrete(labels=scale_values$group_name,breaks=factor(scale_values$group_number))+
		theme(axis.text=element_text(colour="black"))+
		labs(y="Estimated EUI Building Performance Percentage\n",fill=group_column,x="\nEUI Value")
	return(out)

}

# sims = 25000
# groupA = data.frame(value=rnorm(sims,25,5),group="A")
# groupB = data.frame(value=rnorm(sims,21,2),group="B")
# groupC = data.frame(value=rnorm(sims,17,3),group="C")
# data_equal = rbind(groupA,groupB,groupC)
# weighted_histogram(data=data_equal,weight_data=data.frame(group=c("A","B","C"),weight=c(.8,.2,.1)))
# weighted_histogram(data=data_equal,weight_data=data.frame(group=c("A","B","C"),weight=c(.8,.2,.1)),overlay_alpha=.5)

# weighted_histogram(data=data_equal,weight_data=data.frame(group=c("A","B","C"),weight=c(.8,.2,.1)),type="stack")







# ### example scripts as building ###

# runs = 25000

# gA_300 = data.frame(value=rnorm(runs*.8,25,5),group="A")
# gB_1200 = data.frame(value=rnorm(runs*.2,21,2),group="B")

# groupA = data.frame(value=rnorm(runs,25,5),group="A")
# groupB = data.frame(value=rnorm(runs,21,2),group="B")

# data_equal = rbind(groupA,groupB)
# data_balanced = rbind(gA_300,gB_1200)



# bplot = ggplot(data=data_balanced,aes(x=value,fill=group))+geom_histogram(colour=I("white"))


# scale_values = data.frame(group_number=1:length(levels(data_equal$group)),
# 	group_name=levels(data_equal$group),weight=c(0.8, 0.2))

# eplot = ggplot(data=data_equal,aes(x=value,fill=group))+geom_histogram(colour=I("white"))
# eplot_data = ggplot_build(eplot)

# tweak = eplot_data$data[[1]]
# # > head(tweak)
# #      fill y count         x   ndensity     ncount     density PANEL group ymin ymax      xmin     xmax
# # 1 #F8766D 0     0  9.093506 0.00000000 0.00000000 0.000000000     1     1    0    0  8.487272  9.69974
# # 2 #00BFC4 0     0  9.093506 0.00000000 0.00000000 0.000000000     1     2    0    0  8.487272  9.69974
# # 3 #F8766D 2     2 10.305973 0.01273885 0.01273885 0.001099686     1     1    0    2  9.699740 10.91221
# # 4 #00BFC4 2     0 10.305973 0.00000000 0.00000000 0.000000000     1     2    2    2  9.699740 10.91221
# # 5 #F8766D 5     5 11.518441 0.03184713 0.03184713 0.002749215     1     1    0    5 10.912207 12.12467
# # 6 #00BFC4 5     0 11.518441 0.00000000 0.00000000 0.000000000     1     2    5    5 10.912207 12.12467

# # 
# tweak_plot = tweak[,c("count","x","group")]
# tweak_plot$count_scaled = NA

# for (j in 1:nrow(scale_values)){
# 	tweak_plot$count_scaled[tweak_plot$group==j] = tweak_plot$count[tweak_plot$group==j]*scale_values$weight[scale_values$group_number==j]

# }



# ggplot(data =subset(tweak_plot,group==1),aes(x=x,y=count_scaled/runs,fill=factor(group)))+
# geom_histogram(stat="identity",colour=I("white"))+
# geom_histogram(data=subset(tweak_plot,group==2),stat="identity",colour=I("white"))+
# scale_y_continuous(labels=percent)+
# theme(axis.text=element_text(colour="black"))+
# labs(y="Estimated EUI Building Performance Percentage")





# ##### These plots just show an example of how they look the same (within random noise)

# ggplot(data =subset(tweak_plot,group==1),aes(x=x,y=count_scaled,fill=factor(group)))+
# geom_histogram(stat="identity",colour=I("white"))+
# geom_histogram(data=subset(tweak_plot,group==2),stat="identity",colour=I("white"))+
# scale_y_continuous(limits=c(0,3000))

# ggplot(data =subset(data_balanced,group=="A"),aes(x=value,fill=group))+
# geom_histogram(colour=I("white"),binwidth=1.55)+
# geom_histogram(data=subset(data_balanced,group=="B"),colour=I("white"),binwidth=1.55)+
# scale_y_continuous(limits=c(0,3000),trans="sqrt")



#  #bplot_guts = ggplot_gtable(bplot_data)

#  #grid.draw(bplot_guts)


# #with(tweak,tapply(ndensity,group,sum))
#  #ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
