		y.limit <- range(SUM.allDays.roomWise[,"on.freq"],na.rm=TRUE)

		# round to tenth place		
		y.limit <- c(floor(floor(min(y.limit))/10)*10,ceiling(ceiling(max(y.limit))/10)*10)
		y.limit <- c(0,1)
		key.dailyCurve <- list(title="",columns=5,space="top",cex=0.75,
				       text=list(paste(levels(as.factor(SUM.allDays.roomWise[,"room.name"])),sep=" "),cex=1.0),
				       type = "b",
				       lines=list(cex=1.0,
				                  lty=lty.array[1: nlevels(as.factor(SUM.allDays.roomWise[,"room.name"]))],
						  col=col.array[1: nlevels(as.factor(SUM.allDays.roomWise[,"room.name"]))],
						  pch=pch.array[1: nlevels(as.factor(SUM.allDays.roomWise[,"room.name"]))]))

		# 				   
		plot.obj <- xyplot(SUM.allDays.roomWise[,"on.freq"] ~ SUM.allDays.roomWise[,"hour"],
				   group = SUM.allDays.roomWise[,"room.name"],
				   xlab = list("Hour",cex=2.0),
				   ylab = list("ON Fraction",cex=2.0),
				   main = list(paste("Plug Load at Guest Rooms",sep=""),cex=2.5),
				   cex  = 1.0,type="b",
		   		   between = list(x=0.5,y=0.5),				   
				   key = key.dailyCurve,
				   lty=lty.array[1: nlevels(as.factor(SUM.allDays.roomWise[,"room.name"]))],		   
				   col=col.array[1: nlevels(as.factor(SUM.allDays.roomWise[,"room.name"]))],
				   pch=pch.array[1: nlevels(as.factor(SUM.allDays.roomWise[,"room.name"]))],
				   scales=list(x = list(at     = seq(from=0,to=23,by=1),
							labels = seq(from=0,to=23,by=1),
							limits = c(-1,25),
							cex = 1.5),
					       y = list(y.limit,cex = 1.5))
				    )	
		plot(plot.obj)  
		
		png(file = FL.profile3.PNG,width=960,height=700,unit="px",bg="transparent",pointsize=12)		
			plot(plot.obj) 