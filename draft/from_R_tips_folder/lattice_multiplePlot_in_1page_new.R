for (var.num in x.vars.num)
{
	# all data
	command.string <- paste("plot.obj1 <- xyplot(",y.vars.num," ~ ",var.num,", data = myData.work,xlab=\"",var.num,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" versus \",\"",var.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
	eval(parse(text=command.string))

	command.string <- paste("plot.obj2 <- bwplot(",var.num," ~ as.factor(",y.vars.num,"), data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" versus \",\"",var.num,"\",sep=\"\"),col=\"red\")",sep="")
	eval(parse(text=command.string))

	# upper stream
	command.string <- paste("plot.obj3 <- xyplot(",y.vars.num," ~ ",var.num,", data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",var.num,"\",ylab=\"",y.vars.num,"\",main=paste(\"(Upper) \",\"",y.vars.num,"\",\" versus \",\"",var.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
	eval(parse(text=command.string))

	command.string <- paste("plot.obj4 <- bwplot(",var.num," ~ as.factor(",y.vars.num,"), data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(Upper) \",\"",y.vars.num,"\",\" versus \",\"",var.num,"\",sep=\"\"),col=\"blue\")",sep="")
	eval(parse(text=command.string))
	
	# lower stream
	command.string <- paste("plot.obj5 <- xyplot(",y.vars.num," ~ ",var.num,", data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",var.num,"\",ylab=\"",y.vars.num,"\",main=paste(\"(Lower) \",\"",y.vars.num,"\",\" versus \",\"",var.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
	eval(parse(text=command.string))

	command.string <- paste("plot.obj6 <- bwplot(",var.num," ~ as.factor(",y.vars.num,"), data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(Lower) \",\"",y.vars.num,"\",\" versus \",\"",var.num,"\",sep=\"\"),col=\"green\")",sep="")
	eval(parse(text=command.string))
	
	# split = c(x,y,nx,ny)
	plot(plot.obj1,split=c(1,1,2,3))
	plot(plot.obj2,split=c(2,1,2,3),newpage=FALSE)	
	plot(plot.obj3,split=c(1,2,2,3),newpage=FALSE)	
	plot(plot.obj4,split=c(2,2,2,3),newpage=FALSE)	
	plot(plot.obj5,split=c(1,3,2,3),newpage=FALSE)	
	plot(plot.obj6,split=c(2,3,2,3),newpage=FALSE)	
	
	# plot against segment
	command.string <- paste("plot.obj <- xyplot(",y.vars.num," ~ ",var.num," | segment, data = myData.work,xlab=\"",var.num,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" versus \",\"",var.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
	eval(parse(text=command.string))
	
}
dev.off()