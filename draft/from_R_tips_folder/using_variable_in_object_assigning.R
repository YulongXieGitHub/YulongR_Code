					idx.EPlus <- paste("EPLus-",run.string,"-",climate.city,"-",facade,"-",floor.lab,sep="")
					idx.RAD   <- paste("RAD-",run.string,"-",climate.city,"-",facade,"-",floor.lab,sep="")
					
					# put the two new column in the data frame
					command.string <- 
					paste("all.data <- cbind(all.data,",
					                         "\"",idx.EPlus,"\" = myData.EPlus[,\"",col.EPlus,"\"],",
					                         "\"",idx.RAD,  "\" = myData.RAD[,\"",col.RAD,"\"])",
					sep="")
					
					eval(parse(text=command.string))