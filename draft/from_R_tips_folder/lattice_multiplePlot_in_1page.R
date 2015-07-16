	key.4plot <- list(space = "top", 
			  columns=2,
			  text  = list(c(code1,code2)),
			 # points= list(pch=1:6,col=c("black","red","blue","green","magenta","cyan","pink")),
			 lines = list(lty=1:2,col=c("red","blue")))		
	plot.obj1 <- xyplot(as.numeric(VOC.wide[,code1]) + as.numeric(VOC.wide[,code2]) ~ VOC.wide.dateChron,
			    col=c("red","blue"),
			    type=c("l","l"),
			    lty=c(1,2),
			    xlab="date/time(CST)",ylab="conc(ppm)",
			    key = key.4plot,
			    main=paste("(DeerPark)  Corr coef of ",name1,"(",code1,") and ",name2,"(",code2,") is ",cor.pearson,"|",cor.spearman,sep=""))
	plot.obj2 <- xyplot(as.numeric(VOC.wide[,code2]) ~ as.numeric(VOC.wide[,code1]),
	                    pch=".",col="cyan",cex=2,
			    xlab=code1,ylab=code2,
			    aspect=1,
			    main=paste("(DeerPark)  scatter plot of ",name1,"(",code1,") vs ",name2,"(",code2,")",sep=""))
	plot(plot.obj1,split=c(1,1,1,2))
	plot(plot.obj2,split=c(1,2,1,2),newpage=FALSE)
