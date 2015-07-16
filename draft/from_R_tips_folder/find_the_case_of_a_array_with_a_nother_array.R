# here fan.types is an array
#      myData.thisClimate.fan is a data frame, the "Name" field consists some element of "fan.types".
# need to find those rows where  myData.thisClimate.fan[,"Name"] matches "fan.types" 
fan.types  <- sort(unique(myFan[,"Name"])) 

					# 5. Supply Fan Power
						# For each system, collect the supply fan power and write those in the summary file
						idx.systype <- idx.systype + 1
						a <- myData.thisClimate.fan[match(fan.types,myData.thisClimate.fan[,"Name"]),"Rated.Power..W.",drop=FALSE]*factor_w_hp
						row.names(a) <- paste(fan.types,"Fan Power [hp]",sep=" ")
						    names(a) <- paste(this.climate,this.standard,sep=" ")
						if (idx.systype == 1){myOut.sys <- a}else{myOut.sys <- rbind(myOut.sys,a)}
