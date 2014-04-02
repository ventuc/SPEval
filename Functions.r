# Functions.r: this file contains all the required functions designed to solve
# Service Portfolio Evaluation problems

# Copyright (C) 2014  Marco Rossetti

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


###############################################################################
# readData:
#	read data from structureFile and parametersFile
###############################################################################
readData <- function(structureFile,parametersFile,nDesignStages) {

	# read data
	structureFileContent <- read.table(structureFile,sep=',',header=TRUE,quote = "\"",stringsAsFactors=FALSE,dec = ".")
	
	parametersFileContent <- read.table(parametersFile,sep=',',header=TRUE,quote = "\"",stringsAsFactors=FALSE,dec = ".")
	
	# retains only part-of relationships
	structure <- structureFileContent[which(structureFileContent[["type"]]=='part-of'),c("parent","child")]
	
	# expands is-a relationships (at the time it doesn't work!)
	if (FALSE){
		for (alias in which(structureFileContent[["type"]]=='is-a')) {
			parentName <- structureFileContent[["parent"]][alias]
			childName <- structureFileContent[["child"]][alias]
			structure <- rbind.data.frame(structure,
											data.frame(
												Parent=childName,
												Child=structure[structure[,1]==parentName,2]
											)
										)
		}
	}

	parameters <- parametersFileContent[order(parametersFileContent[["id"]]),]
	nServices <- length(parameters[["id"]])

	# validation
	validation(structure,parameters,'InputData')
	
	# add metadata
	parameters[["count"]] <- 1
	parameters[,paste("sav",1:nDesignStages,sep="")] <- 0
	parameters[["aggName"]] <- parameters[["name"]]

	returnObj <- list()
	returnObj[["structure"]] <- structure
	returnObj[["parameters"]] <- parameters
	
	return(returnObj)
}

###############################################################################
# validation:
#	check the service repository
###############################################################################
validation <- function(structure,parameters,classInUse) {

	# check duplicated services
	if (length(parameters[["id"]]) != length(unique(parameters[["id"]]))) {
		cat('ERROR:',classInUse,'\n')
		stop('Services duplicated in the parameters file')
	}
	
	# check if every service in the structure has parameters
	if (NA %in% match(unique(unlist(structure)),parameters[["id"]])) {
		cat('ERROR:',classInUse,'\n')
		stop('Not all services are in the parameters file')
	}
	
	# check if cds respects structural coherence
	initServicesIdx <- which(parameters[["cds"]]>0)
	parentChildren <- structure[which(match(structure[["parent"]],parameters[["id"]][initServicesIdx])>0),]
	parentsIdx <- match(parentChildren[["parent"]],parameters[["id"]])
	childrenIdx <- match(parentChildren[["child"]],parameters[["id"]])
	if (length(parentsIdx) > 0) {
		errors <- which(parameters[["cds"]][parentsIdx]>parameters[["cds"]][childrenIdx])
		if (length(errors) > 0) {
			cat('ERROR:',classInUse,'\n')
			stop(paste("'",parameters[["id"]][parentsIdx[errors]],"' is at level ",parameters[["cds"]][parentsIdx[errors]]," while '",parameters[["id"]][childrenIdx[errors]],"' is at level ",parameters[["cds"]][childrenIdx[errors]],"\n",sep=''))
		}
	}
	
	# check if efforts are coherent within classes
	classesToCheck <- c("PEq","CEq")
	for (classToCheck in classesToCheck) {
		for (class in unique(parameters[[classToCheck]])) {
			if (length(which(apply(parameters[which(parameters[[classToCheck]]==class),paste("ef",1:nDesignStages,sep="")],2,function(r) length(unique(r)))>1)) > 0) {
				cat('ERROR:',classInUse,'\n')
				stop(paste("Equivalence class ",class," of type '",classToCheck,"' has not the same efforts for all services",sep=''))
			}		
		}
	}

	ret <- getAm(structure[["child"]],structure[["parent"]],parameters[["id"]])
	if (length(which(diag(ret$amExtended)>0)) > 0) {
		cat('ERROR:',classInUse,'\n')
		stop(paste("Found loops on the following services: ",paste(names(which(diag(ret$amExtended)>0)),collapse=' '),sep=''))
	}
}

###############################################################################
# getAm:
#	compute the adjacency matrix and the extended adjacency matrix with
#	connected vertices
###############################################################################
getAm <- function(children,parents,services) {

	# create adjacency matrix
	am <- matrix(0, nrow = length(services), ncol = length(services))
	am[cbind(match(children,services),match(parents,services))] <- 1
	
	rownames(am) <- services
	colnames(am) <- services
	
	amExtended <- am
	amNew <- amExtended + amExtended%*%amExtended
	amNew[amNew>0] <- 1
	while (!identical(amExtended,amNew)) {
		amExtended <- amNew
		amNew <- amExtended + amExtended%*%amExtended
		amNew[amNew>0] <- 1
	}
	
	return(list(am=am,amExtended=amExtended))
}

###############################################################################
# getClassStructure:
#	replace equivalent services with the service that has the highest cds
###############################################################################
getClassStructure <- function(structure,parameters,classInUse,nDesignStages) {

	#parameters[,paste("sav",1:nDesignStages,sep="")] <- 0

	for (class in unique(parameters[[classInUse]])) {
		eqServ <- parameters[which(parameters[[classInUse]]==class),"id"]
		
		if (length(eqServ) > 1) {
			maxCds <- which.max(parameters[which(parameters[[classInUse]]==class),"cds"])
			maxRow <- which(parameters[["id"]] %in% eqServ[maxCds])
			otherRows <- which(parameters[["id"]] %in% eqServ[-maxCds])
		
			# change structure
			structure[["parent"]][which(structure[["parent"]] %in% eqServ[-maxCds])] <- eqServ[maxCds]
			structure[["child"]][which(structure[["child"]] %in% eqServ[-maxCds])] <- eqServ[maxCds]
			
			# aggregate counts
			parameters[maxRow,"count"] <- sum(parameters[c(maxRow,otherRows),"count"])
			
			parameters[maxRow,"aggName"] <- paste(parameters[c(maxRow,otherRows),"aggName"],collapse=',')
			
			# compute savings
			for (l in 1:nDesignStages) {
				lessCds <- which(parameters[otherRows,"cds"] < l)
				if (length(lessCds) > 0) {
					parameters[maxRow,paste("sav",l,sep="")] <- parameters[maxRow,paste("sav",l,sep="")]+
						sum(parameters[otherRows[lessCds],paste("ef",l,sep="")])	
				}
			}
			
			# remove equivalent services
			parameters <- parameters[-which(parameters[["id"]] %in% eqServ[-maxCds]),]
		}
	}
	structure <- unique(structure)
	
	# validation
	validation(structure,parameters,classInUse)

	returnObj <- list()
	returnObj[["structure"]] <- structure
	returnObj[["parameters"]] <- parameters
	
	return(returnObj)
}

###############################################################################
# assessment:
#	evaluate forward and backward assessment
###############################################################################
assessment <- function(parameters,nDesignStages,type) {
	effort <- 0
	for (i in 1:nrow(parameters)) {
		if (parameters[["cds"]][i] > 0) {
			for (j in 1:parameters[["cds"]][i]) {
				effort <- effort + parameters[[paste("ef",j,sep="")]][i]
			}
		}
	}
	if (type == 'forward')
		effort <- sum(parameters[,paste("ef",1:nDesignStages,sep="")]) - effort
	return(effort)
}

###############################################################################
# createModel:
#	create lp model
###############################################################################
createModel <- function(structure,parameters,lpProblemFile,nDesignStages,depth,type) {
	
	parents <- sort(unique(structure[["parent"]]))
	nParents <- length(parents)
	nServices <- length(parameters[["id"]])
	
	##################################
	# CONTRAINTS
	##################################
	
	# basic structure
	f_con <- matrix (0, nrow=nParents, ncol=nServices)
	
	for (i in 1:nParents) {
		idx_parent <- match(parents[i],parameters[["id"]])
		idx_children <- match(structure[which(structure[["parent"]]==parents[i]),2],parameters[["id"]])
		f_con[i,idx_parent] <- -length(idx_children)
		f_con[i,idx_children] <- 1	
	}

	# duplicate structure for all development levels
	f_con_full <- matrix(0, nrow = nParents*nDesignStages, ncol = nServices*nDesignStages)

	colNames <- c()
	rowNames <- c()
	for (i in 1:nDesignStages) {
		f_con_full[((i-1)*nParents+1):(i*nParents),((i-1)*nServices+1):(i*nServices)] <- f_con
		colNames <- c(colNames,paste('cds',i,' - ',parameters[["name"]],sep=''))
		rowNames <- c(rowNames,paste('cds',i,' - ',parents,sep=''))
	}
	colnames(f_con_full) <- colNames

	# add contraints between development levels
	for (l in 1:(nDesignStages-1)) {
		for (i in seq_along(parameters[["id"]])) {
			constr <- rep(0,nServices*nDesignStages)
			constr[i+(l-1)*nServices] <- 1
			constr[i+l*nServices] <- -1
			f_con_full <- rbind(f_con_full,constr)
		}
		rowNames <- c(rowNames,paste('ef',(l+1),' - ',parameters[["name"]],sep=''))
	}
	rownames(f_con_full) <- rowNames

	# set up costs and services already modeled
	effortConstraint <- rep(0,nServices*nDesignStages)
	cdsConstraint <- rep(0,nServices*nDesignStages)
	idx <- 1:nServices
	for (l in 1:nDesignStages) {
		effortConstraint[idx+(l-1)*nServices] <- as.numeric(parameters[[paste("ef",l,sep="")]])
		
		# set effort to 0 for services already developed
		idxToSetZero <- which(l<=parameters[["cds"]])
		if (length(idxToSetZero) > 0) {
			effortConstraint[idxToSetZero+(l-1)*nServices] <- 0
			cdsConstraint[idxToSetZero+(l-1)*nServices] <- 1
		}
	}

	f_con_full <- rbind(f_con_full,"cds"=cdsConstraint)
	f_con_full <- rbind(f_con_full,"effort"=effortConstraint)

	##################################
	# DIRECTIONS and RHS
	##################################
	f_dir <- rep(">=",nrow(f_con_full))
	f_dir[nrow(f_con_full)] <- "<="
	f_rhs <- rep(0,nrow(f_con_full))
	f_rhs[nrow(f_con_full)-1] <- sum(cdsConstraint)

	##################################
	# OBJ FUNCTION
	##################################
	f_obj <- rep(0,nServices*nDesignStages)
	if (type == 'count') {
		f_obj[1:nServices+(nDesignStages-1)*nServices] <- parameters[["count"]]
	}
	if (type == 'savingsMax') {
		f_obj[1:nServices+(nDesignStages-1)*nServices] <- rowSums(parameters[paste("sav",1:nDesignStages,sep="")])
	}
	if (type == 'savingsAny') {	
		for (l in 1:nDesignStages) {
			f_obj[1:nServices+(l-1)*nServices] <- parameters[[paste("sav",l,sep="")]]
		}
	}
	
	f_obj_out <- f_obj
	
	# add a little negative weight proportional to the effort
	# just to avoid different solutions with different effort
	# but the same objective value
	if (length(which(f_obj>0)) > 0)
		f_obj <- f_obj - effortConstraint/sum(effortConstraint)*min(f_obj[f_obj>0])

	
	##################################
	# CREATE MODEL
	##################################
	lps_model <- make.lp(nrow = 0, ncol = ncol(f_con_full))
	
	for (i in 1:nrow(f_con_full)) {
		add.constraint(lps_model, f_con_full[i,], f_dir[i], f_rhs[i])
	}
	
	dimnames(lps_model) <- list(rownames(f_con_full),colnames(f_con_full))
	
	set.type(lps_model, 1:(nServices*nDesignStages), "binary")
	set.objfn(lps_model, f_obj)
	
	lp.control(lps_model, sense = 'max', bb.depthlimit = depth)
	
	write.lp(lps_model, lpProblemFile, type = c("lp"), use.names = FALSE)
	
	return(list(lps_model=lps_model,f_obj=f_obj_out))
}

###############################################################################
# outputResults:
#	output results for a given range of efforts
###############################################################################
outputResults <- function(lps_model,f_obj,parameters,effortRange,logFile,outputFile,timeout,type) {

	lp.control(lps_model, timeout = timeout)
	
	nServices <- length(parameters[["id"]])
	
	colNames <- c()
	for (i in 1:(ncol(lps_model)/nServices)) {
		colNames <- c(colNames,paste('cds',i,' - ',parameters[["aggName"]],sep=''))
	}

	# initialize output and log
	output <- file(outputFile,"w")
	cat('',file=logFile)

	# initialize time vars
	lastTime <- Sys.time()
	lastEffort <- -1

	for (rhs in effortRange) {

		# set new rhs (effort contraint upper bound)
		set.rhs(lps_model, rhs, nrow(lps_model))
		
		# get solution
		result <- solveProblem(lps_model)
		
		# log result
		cat('Time elapsed',paste(round(Sys.time()-lastTime,2)),
			'Effort bound',rhs,
			'Real effort',result$effort,
			'Exit',result$exit,
			'Obj',result$objective,
			'\n',
			sep='\t',
			file=logFile,
			append=TRUE)
		lastTime <- Sys.time()
		
		# if only one effort value is specified, or effort used is changed and the solution is optimal
		# print result in output file
		if (length(effortRange) == 1 || (result$exit == 0 && round(lastEffort-result$effort,6) != 0)) {
			if (result$exit == 1)
				prefix <- 'SUBOPTIMAL'
			else if (result$exit == 2)
				prefix <- 'INFEASIBLE'
			else
				prefix <- ''
			
			varAnyToPrint <- c()
			varTopToPrint <- c()
			fullSolution <- rep(0,nServices)
			realObj <- 0
			
			if (result$exit == 0) {
				lastEffort <- result$effort
				realObj <- sum(result$solution * f_obj)
				for (l in nDesignStages:1) {
					devLevSolution <- result$solution[((l-1)*nServices+1):(l*nServices)]
					idxToPrint <- setdiff(which(devLevSolution>0),which(fullSolution>0))
					idxToPrint <- idxToPrint[parameters[["cds"]][idxToPrint]<l|parameters[[paste("sav",l,sep="")]][idxToPrint]>0]
					varAnyToPrint <- c(varAnyToPrint,colNames[idxToPrint+(l-1)*nServices])
					if (l == nDesignStages) varTopToPrint <- c(varTopToPrint,colNames[idxToPrint+(l-1)*nServices])
					fullSolution <- fullSolution + devLevSolution
				}
			}
			if (type == 'count') {
				writeLines(c(paste(prefix,'Effort bound',rhs,'Real effort',lastEffort,'Num Services',realObj),varTopToPrint),con = output)
			}
			if (type == 'savingsMax') {
				writeLines(c(paste(prefix,'Effort bound',rhs,'Real effort',lastEffort,'Savings',realObj),varTopToPrint),con = output)
			}
			if (type == 'savingsAny') {
				writeLines(c(paste(prefix,'Effort bound',rhs,'Real effort',lastEffort,'Savings',realObj),varAnyToPrint),con = output)
			}
			writeLines('====================================================================',con = output)
			
		}
	}

	close(output)
}

###############################################################################
# solveProblem:
#	solve the lp problem
###############################################################################
solveProblem <- function(lps_model) {
	
	result					<- list()
	result[["exit"]]		<- solve(lps_model)
	result[["solution"]]	<- get.variables(lps_model)
	result[["effort"]]		<- get.constraints(lps_model)[nrow(lps_model)]
	result[["objective"]]	<- get.objective(lps_model)
	
	return(result)
}