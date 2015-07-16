# -----------------------------------------------------------------------
# function: model.output.glm
# -----------------------------------------------------------------------
model.output.glm = function(Model.ID,model.string,model.count,model.no.var,model.formula,model.coef,model.sum,model.data.fit,var.rm) 
{
	null.df        <- model.sum[1]
	resi.df        <- model.sum[2]
	null.deviance  <- model.sum[3]
	resi.deviance  <- model.sum[4]
	aic            <- model.sum[5]
	iter           <- model.sum[6]

	# write the model out 
	model.coef[,"signif"] <- rep("",dim(model.coef)[1])
	model.coef[model.coef[,"Pr(>|z|)"] <= 0.001,"signif"]                                  <- "***"
	model.coef[model.coef[,"Pr(>|z|)"] >  0.001 & model.coef[,"Pr(>|z|)"] < 0.01,"signif"] <- " **"
	model.coef[model.coef[,"Pr(>|z|)"] >  0.01  & model.coef[,"Pr(>|z|)"] < 0.05,"signif"] <- "  *"
	model.coef[model.coef[,"Pr(>|z|)"] >  0.05  & model.coef[,"Pr(>|z|)"] < 0.10,"signif"] <- "  ."
	
	model.out.line1     <- paste("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",sep=" ")
	model.null.deviance <- paste("    Null deviance:",null.deviance,"on",null.df,    "degree of freedom",sep=" ")
	model.resi.deviance <- paste("Residual deviance:",resi.deviance,"on",resi.df,"degree of freedom",sep=" ")
	model.aic           <- paste("AIC:",aic,sep=" ")
	model.iter          <- paste("Number of Fisher Scoring iterations:",iter,sep=" ")


	# cat(paste("Call:\n",model.formula,"\n",sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste(model.string,            sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste("Coefficients:\n",       sep=""),file=FL.OUT.Model,append=TRUE)
	write.table(model.coef,file=FL.OUT.Model,row.names=TRUE,col.names=TRUE,sep=",",append=TRUE)
	cat(paste(model.out.line1,"\n",    sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste(model.null.deviance,"\n",sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste(model.resi.deviance,"\n",sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste(model.aic,"\n",          sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste(model.iter,"\n",         sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste("\n\n",                  sep=""),file=FL.OUT.Model,append=TRUE)

	cat(paste(model.ID,",",            sep=""),file=FL.OUT.Data,append=TRUE)
	write.table(model.data.fit,file=FL.OUT.Data,row.names=TRUE,col.names=TRUE,sep=",",append=TRUE)

	# put summary into the summary array
	summary.this <- c(ID       = Model.ID,
			  model.idx= model.count,
			  var.rm   = var.rm,
			  no.var   = model.no.var,
			  dev.null = round(null.deviance,digits=1),
			  df.null  = null.df,
			  dev.res  = round(resi.deviance,digits=1),
			  df.res   = resi.df,
			  AIC      = round(aic,digits=1))

	return(summary.this)
}


# -----------------------------------------------------------------------
# function: model.output.pscl
# -----------------------------------------------------------------------
model.output.pscl = function(Model.ID,model.string,model.count,model.no.var,model.formula,model.coef.count,model.coef.zero,model.sum,model.data.fit,var.rm) 
{
	null.df        <- model.sum[1]
	resi.df        <- model.sum[2]
	loglik         <- model.sum[3]
	aic            <- model.sum[4]

	# write the model out 
	model.coef.count[,"signif"] <- rep("",dim(model.coef.count)[1])
	model.coef.count[is.na(model.coef.count[,"Pr(>|z|)"]),                                                                                     "signif"] <- "NA "
	model.coef.count[(!(is.na(model.coef.count[,"Pr(>|z|)"]))) & model.coef.count[,"Pr(>|z|)"] <= 0.001,                                       "signif"] <- "***"
	model.coef.count[(!(is.na(model.coef.count[,"Pr(>|z|)"]))) & model.coef.count[,"Pr(>|z|)"] >  0.001 & model.coef.count[,"Pr(>|z|)"] < 0.01,"signif"] <- " **"
	model.coef.count[(!(is.na(model.coef.count[,"Pr(>|z|)"]))) & model.coef.count[,"Pr(>|z|)"] >  0.01  & model.coef.count[,"Pr(>|z|)"] < 0.05,"signif"] <- "  *"
	model.coef.count[(!(is.na(model.coef.count[,"Pr(>|z|)"]))) & model.coef.count[,"Pr(>|z|)"] >  0.05  & model.coef.count[,"Pr(>|z|)"] < 0.10,"signif"] <- "  ."

	# write the model out 
	model.coef.zero[,"signif"] <- rep("",dim(model.coef.zero)[1])
	model.coef.zero[is.na(model.coef.zero[,"Pr(>|z|)"]),                                                                                   "signif"] <- "NA "
	model.coef.zero[(!(is.na(model.coef.zero[,"Pr(>|z|)"]))) & model.coef.zero[,"Pr(>|z|)"] <= 0.001,                                      "signif"] <- "***"
	model.coef.zero[(!(is.na(model.coef.zero[,"Pr(>|z|)"]))) & model.coef.zero[,"Pr(>|z|)"] >  0.001 & model.coef.zero[,"Pr(>|z|)"] < 0.01,"signif"] <- " **"
	model.coef.zero[(!(is.na(model.coef.zero[,"Pr(>|z|)"]))) & model.coef.zero[,"Pr(>|z|)"] >  0.01  & model.coef.zero[,"Pr(>|z|)"] < 0.05,"signif"] <- "  *"
	model.coef.zero[(!(is.na(model.coef.zero[,"Pr(>|z|)"]))) & model.coef.zero[,"Pr(>|z|)"] >  0.05  & model.coef.zero[,"Pr(>|z|)"] < 0.10,"signif"] <- "  ."
	
	
	model.out.line1 <- paste("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",sep=" ")
	model.loglik    <- paste("logLik:",loglik,sep=" ")
	model.aic       <- paste("AIC:",aic,sep=" ")


	# cat(paste("Call:\n",model.formula,"\n",sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste(model.string,               sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste("Coefficients of Count:\n", sep=""),file=FL.OUT.Model,append=TRUE)
	write.table(model.coef.count,file=FL.OUT.Model,row.names=TRUE,col.names=TRUE,sep=",",append=TRUE)
	cat(paste("\nCoefficients of Zero:\n",sep=""),file=FL.OUT.Model,append=TRUE)
	write.table(model.coef.zero,file=FL.OUT.Model,row.names=TRUE,col.names=TRUE,sep=",",append=TRUE)
	cat(paste(model.out.line1,"\n",       sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste(model.loglik,"\n",          sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste(model.aic,"\n",             sep=""),file=FL.OUT.Model,append=TRUE)
	cat(paste("\n\n",                     sep=""),file=FL.OUT.Model,append=TRUE)

	cat(paste(model.ID,",",               sep=""),file=FL.OUT.Data,append=TRUE)
	write.table(model.data.fit,file=FL.OUT.Data,row.names=TRUE,col.names=TRUE,sep=",",append=TRUE)

	# put summary into the summary array
	summary.this <- c(ID       = Model.ID,
			  model.idx= model.count,
			  var.rm   = var.rm,
			  no.var   = model.no.var,
			  df.null  = null.df,
			  df.res   = resi.df,
			  logLik   = loglik,
			  AIC      = aic)

	return(summary.this)
}

