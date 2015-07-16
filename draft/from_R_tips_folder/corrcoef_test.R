				# R.pearson  <- as.character(cor(MS.Aldine[,idx.Aldine],MS.Bayland[,idx.Bayland],method="pearson",use="pairwise.complete.obs"))
				# July 29, 2013:
				# install.packages("Hmisc")
				# library("Hmisc")
				corr.pearson  <- rcorr(MS.Bayland[,idx.Bayland],MS.DeerPark[,idx.DeerPark],type="pearson")
				corr.spearman <- rcorr(MS.Bayland[,idx.Bayland],MS.DeerPark[,idx.DeerPark],type="spearman")
				R.pearson     <- corr.pearson[[1]][1,2]
				P.pearson     <- corr.pearson[[3]][1,2]
				R.spearman    <- corr.spearman[[1]][1,2]
				P.spearman    <- corr.spearman[[3]][1,2]
				S.pearson     <- paste(R.pearson," |",P.pearson,sep="")
				S.spearman    <- paste(R.spearman," |",P.spearman,sep="")