# Service: this script starts a web server for web services requests
#
# Requires: Rook, jsonlite
#
# Copyright (C) 2014  Claudio Venturini
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
# The service exposes several apps, each implementing a specific type
# of optimization (i.e. optimizes using a different objective function).
# 
# Apps listen at URL http://<server_address>:<server_port>/custom/<app_name>
# 
# Each app expects input data encoded in JSON. Input data are:
# 1. the service repository (the set of services with their part-of and is-a relationships)
# 2. the service portfolio (the production lines under which the services are produced and their development stage)
# 
# Example:
# {
#	"portfolio": [
#		{
#			"id": 1,
#			"ceq": 1,
#			"peq": 1,
#			"name": "pl1.S1",
#			"cds": 1,
#			"effort": [ 25,10,35,15,30 ]
#		},
#		{
#			"id": 2,
#			"ceq": 2,
#			"peq": 2,
#			"name": "pl1.S2",
#			"cds": 2,
#			"effort": [ 10,5,15,10,15 ]
#		},
#		{
#			"id": 3,
#			"ceq": 3,
#			"peq": 3,
#			"name": "pl1.S3",
#			"cds": 1,
#			"effort": [ 15,10,15,10,20 ]
#		},
#		{
#			"id": 4,
#			"ceq": 4,
#			"peq": 4,
#			"name": "pl1.S4",
#			"cds": 3,
#			"effort": [ 10,5,15,15,25 ]
#		},
#		{
#			"id": 5,
#			"ceq": 5,
#			"peq": 5,
#			"name": "pl1.S5",
#			"cds": 1,
#			"effort": [ 10,15,15,5,30 ]
#		},
#		{
#			"id": 6,
#			"ceq": 6,
#			"peq": 6,
#			"name": "pl1.S6",
#			"cds": 2,
#			"effort": [ 5,5,15,5,10 ]
#		},
#		{
#			"id": 7,
#			"ceq": 7,
#			"peq": 7,
#			"name": "pl1.S7",
#			"cds": 2,
#			"effort": [ 10,5,10,10,10 ]
#		},
#		{
#			"id": 13,
#			"ceq": 7,
#			"peq": 7,
#			"name": "pl2.S11",
#			"cds": 2,
#			"effort": [ 10,5,10,10,10 ]
#		},
#		{
#			"id": 8,
#			"ceq": 8,
#			"peq": 8,
#			"name": "pl1.S8",
#			"cds": 1,
#			"effort": [ 20,10,35,20,35 ]
#		},
#		{
#			"id": 9,
#			"ceq": 9,
#			"peq": 9,
#			"name": "pl1.S9",
#			"cds": 2,
#			"effort": [ 15,5,30,10,30 ]
#		},
#		{
#			"id": 10,
#			"ceq": 10,
#			"peq": 10,
#			"name": "pl2.S10",
#			"cds": 1,
#			"effort": [ 40,10,35,20,45 ]
#		},
#		{
#			"id": 11,
#			"ceq": 6,
#			"peq": 11,
#			"name": "pl2.S12",
#			"cds": 2,
#			"effort": [ 5,5,15,5,10 ]
#		},
#		{
#			"id": 12,
#			"ceq": 5,
#			"peq": 12,
#			"name": "pl2.S13",
#			"cds": 4,
#			"effort": [ 10,15,15,5,30 ]
#		},
#		{
#			"id": 14,
#			"ceq": 14,
#			"peq": 14,
#			"name": "pl3.S14",
#			"cds": 2,
#			"effort": [ 15,10,15,20,15 ]
#		},
#		{
#			"id": 15,
#			"ceq": 15,
#			"peq": 15,
#			"name": "pl3.S15",
#			"cds": 2,
#			"effort": [ 10,5,10,20,15 ]
#		},
#		{
#			"id": 16,
#			"ceq": 7,
#			"peq": 16,
#			"name": "pl3.S16",
#			"cds": 2,
#			"effort": [ 10,5,10,10,10 ]
#		}
#	],
#	"services": [
#		{
#			"parent": 1,
#			"child": 2,
#			"type": "part-of"
#		},
#		{
#			"parent": 1,
#			"child": 3,
#			"type": "part-of"
#		},
#		{
#			"parent": 1,
#			"child": 4,
#			"type": "part-of"
#		},
#		{
#			"parent": 8,
#			"child": 9,
#			"type": "part-of"
#		},
#		{
#			"parent": 8,
#			"child": 5,
#			"type": "part-of"
#		},
#		{
#			"parent": 3,
#			"child": 5,
#			"type": "part-of"
#		},
#		{
#			"parent": 3,
#			"child": 6,
#			"type": "part-of"
#		},
#		{
#			"parent": 3,
#			"child": 7,
#			"type": "part-of"
#		},
#		{
#			"parent": 10,
#			"child": 13,
#			"type": "part-of"
#		},
#		{
#			"parent": 10,
#			"child": 11,
#			"type": "part-of"
#		},
#		{
#			"parent": 10,
#			"child": 12,
#			"type": "part-of"
#		},
#		{
#			"parent": 14,
#			"child": 15,
#			"type": "part-of"
#		},
#		{
#			"parent": 14,
#			"child": 16,
#			"type": "part-of"
#		}
#	]
# }
#

library("Rook")
library("jsonlite")
library("lpSolveAPI")

source("OptimizationStrategy.r")
source("DefaultOptimizationStrategy.r")

devDir <- "D:\\Users\\Claudio\\Desktop\\seva\\dev\\"

# Reads the JSON body of the given request and unserializes it, if needed (takes care of the encoding)
readJSONRequest = function(req, unserialize = TRUE){
	jsonBytes = req$body()$read()
	
	valid = FALSE
	data = NULL
	
	if (length(jsonBytes)){
		json = rawToChar(jsonBytes)
		Encoding(json) <- "UTF-8"
		
		# Validates the JSON string
		valid <- validate(json)
		
		if (valid){
			if (unserialize){
				data <- fromJSON(json)
			}
			else data = json
		}
	}
	
	return(list(valid = valid, data = data))
}

# Builder for optimization strategy apps
OptimizationAppBuilder = setRefClass("OptimizationAppBuilder",
	fields = list(strategy = "OptimizationStrategy"),
	methods = list(
		app = function(){
			return(
				function(env){
					req = Rook::Request$new(env)
					res = Rook::Response$new()
					
					# Input data
					input = readJSONRequest(req)
					
					# If data is not valid, error
					if (input$valid == FALSE){
						res = Rook::Response$new(status = 400)
						res$write("Request doesn't contain valid JSON-encoded input data")
					}
					# Does the job
					else {
						# Cleans data
						input$data <- cleanData(input$data)
						
						# Validates data
						validateData(input$data)
						
						# Inits output data list
						output <- list()
						
						# Optimizes portfolio w.r.t. production equivalence classes
						peqData = applyReuse(input$data, "peq")

						# Optimizes portfolio w.r.t. conceptual equivalence classes
						ceqData = applyReuse(input$data, "ceq")
						
						# Backward assessment
						worstBackwardAssessment <- backwardAssessment(input$data$portfolio)
						currentBackwardAssessment <- backwardAssessment(peqData$portfolio)
						bestBackwardAssessment <- backwardAssessment(ceqData$portfolio)
						
						output$backwardAssessment = list(worst = unbox(worstBackwardAssessment), current = unbox(currentBackwardAssessment), best = unbox(bestBackwardAssessment))
						
						# Forward assessment
						currentForwardAssessment <- forwardAssessment(peqData$portfolio)
						bestForwardAssessment <- forwardAssessment(ceqData$portfolio)
						
						output$forwardAssessment = list(current = unbox(currentForwardAssessment), best = unbox(bestForwardAssessment))
						
						# Optimization (with different budget values)
						budgets <- seq(0, 1000, 10)
						nBudgets <- length(budgets)
						
						output$optimization = vector("list", length = nBudgets)
						
						for (b in 1:nBudgets){
							budget <- budgets[b]
							
							# Builds the optimization model
							lpModel <- strategy$createModel(peqData, -50, budget)
							
							# Solves the problem
							result <- solveProblem(lpModel)
							
							output$optimization[[b]] = list(budget = unbox(budget), status = unbox(result$status))
						}
						
						# Response
						res$header("Content-Type", "application/json")
						
						json = toJSON(output, simplifyVector = TRUE)
						res$write(json)
					}
					
					res$finish()
				}
			)
		}
	)
)

# Cleans input data
cleanData = function(data){
	# Retains only part-of relationships between services (and ignores the "type" column)
	data$services <- data$services[which(data$services[["type"]] == "part-of"), which(colnames(data$services) != "type")]
	
	# Convert efforts data as a matrix
	data$portfolio[["effort"]] <- t(matrix(unlist(data$portfolio[["effort"]]), ncol = nrow(data$portfolio), nrow = 5))
	
	# Sorts by ID
	data$portfolio <- data$portfolio[order(data$portfolio[["id"]]),]
	
	# Adds some metadata
	data$portfolio[["count"]] <- 1
	data$portfolio[["saving"]] <- matrix(0, nrow = nrow(data$portfolio), ncol = 5)
	data$portfolio[["aggregateName"]] <- data$portfolio[["name"]]
	
	return(data)
}

# Validates input data
validateData <- function(data){
	# Check duplicate services
	if (length(data$portfolio[["id"]]) != length(unique(data$portfolio[["id"]]))){
		stop("Found duplicate service")
	}
	
	# Checks that every service is in the portfolio
	if (NA %in% match(unique(unlist(data$services)), data$portfolio[["id"]])){
		stop("Not all services are in the portfolio")
	}
	
	# Checks that the CDS respects structural coherence (no parent service can have a CDS bigger than any of its children)
	initiatedServicesIdx <- which(data$portfolio[["cds"]] > 0)
	childrenOfInitiadedServices <- data$services[which(match(data$services[["parent"]], data$portfolio[["id"]][initiatedServicesIdx])>0),]
	parentsIdx <- match(childrenOfInitiadedServices[["parent"]], data$portfolio[["id"]])
	childrenIdx <- match(childrenOfInitiadedServices[["child"]], data$portfolio[["id"]])
	if (length(parentsIdx) > 0){
		errors <- which(data$portfolio[["cds"]][parentsIdx] > data$portfolio[["cds"]][childrenIdx])
		if (length(errors) > 0){
			stop(paste("'", data$portfolio[["id"]][parentsIdx[errors]], "' is at level ", data$portfolio[["cds"]][parentsIdx[errors]], " while '", data$portfolio[["id"]][childrenIdx[errors]],"' is at level ", data$portfolio[["cds"]][childrenIdx[errors]], "\n", sep = ''))
		}
	}
	
	# Checks that efforts of every service in the same equivalence class are coherent
	for (eqClassType in c("peq", "ceq")){
		for (eqClass in unique(data$portfolio[[eqClassType]])){
			# Efforts of services in the current equivalence class
			eqClassServices = data$portfolio[which(data$portfolio[[eqClassType]] == eqClass), "effort"]
			
			# Finds a mismatch by counting the number of non unique efforts for each design stage
			mismatch <- (length(which(apply(eqClassServices, 2, function(i) length(unique(i))) > 1)) > 0)
			
			if (mismatch){
				stop(paste("Equivalence class ", eqClass, " of type '", eqClassType, "' has not the same efforts for all services", sep = ''))
			}
		}
	}
	
	# Checks that there are no loops in services structure
	if (hasLoops(data$services)){
		stop("Found loops in the services structure")
	}
}

# Checks if the given services structure has loops by computing the extended adjacency matrix
hasLoops <- function(services){
	servicesIDs <- sort(unique(unlist(services)))
	nServices = length(servicesIDs)
	
	# Creates the adjacency matrix
	am <- matrix(0, nrow = nServices, ncol = nServices)
	am[cbind(match(services[["child"]], servicesIDs), match(services[["parent"]], servicesIDs))] <- 1
	
	# Names columns and rows
	rownames(am) <- servicesIDs
	colnames(am) <- servicesIDs
	
	# Creates the extended adjacency matrix
	amExtended <- am
	amNew <- amExtended + amExtended%*%amExtended
	amNew[amNew>0] <- 1
	while (!identical(amExtended,amNew)) {
		amExtended <- amNew
		amNew <- amExtended + amExtended%*%amExtended
		amNew[amNew>0] <- 1
	}
	
	# Checks for loops (the diagonal must be all-0)
	hasLoops <- (length(which(diag(amExtended) > 0)) > 0)
	
	return(hasLoops)
}

# Replace equivalent services (defined by the given equivalence class: "ceq", "peq")
# in the portfolio with the ones at the highest design stage
applyReuse <- function(services, eqClassType){
	for (eqClass in unique(services$portfolio[[eqClassType]])){
		# Indexes of services in the current equivalence class
		servicesIDx <- which(services$portfolio[[eqClassType]] == eqClass)
		
		# IDs of services in the current equivalence class
		servicesIDs <- services$portfolio[servicesIDx, "id"]
		
		if (length(servicesIDs) > 1){
			# Index of the service with maximum CDS (in the set of services of the current equivalence class)
			maxCds <- which.max(services$portfolio[servicesIDx, "cds"])
			
			# Updates the structure
			services$services[["parent"]][which(services$services[["parent"]] %in% servicesIDs[-maxCds])] <- servicesIDs[maxCds]
			services$services[["child"]][which(services$services[["child"]] %in% servicesIDs[-maxCds])] <- servicesIDs[maxCds]
			
			# Index of the service with maximum CDS (w.r.t. the portfolio)
			maxRow <- which(services$portfolio[["id"]] %in% servicesIDs[maxCds])
			# Indexes of the services with non-maximum CDS (w.r.t. the portfolio)
			otherRows <- which(services$portfolio[["id"]] %in% servicesIDs[-maxCds])
			
			# Updates the aggregates of equivalent services
			services$portfolio[maxRow, "count"] <- sum(services$portfolio[c(maxRow, otherRows), "count"])
			services$portfolio[maxRow, "aggregateName"] <- paste(services$portfolio[c(maxRow, otherRows), "aggregateName"], collapse = ',')
			
			# Computes the savings thanks to reuse
			for (designStage in 1:5){
				# Indexes of equivalent services at a design stage lower than the current
				lessCds <- which(services$portfolio[otherRows, "cds"] < designStage)
				
				# Sums old saving of the aggregated service and the efforts saved by aggregating other services
				if (length(lessCds) > 0){
					services$portfolio[maxRow, "saving"][,designStage] <- services$portfolio[maxRow, "saving"][,designStage] + sum(services$portfolio[otherRows[lessCds], "effort"][, designStage])
				}
			}
			
			# Removes equivalent services
			services$portfolio <- services$portfolio[-which(services$portfolio[["id"]] %in% servicesIDs[-maxCds]),]
		}
	}
	
	# Removes equivalent services from the structure (now they're equal, so they're duplicate)
	services$services <- unique(services$services)
	
	# Validation
	validateData(services)
	
	return(services)
}

# Backward assessment of the portfolio
backwardAssessment <- function(portfolio){
	effort <- 0
	for (i in 1:nrow(portfolio)) {
		if (portfolio[i, "cds"] > 0) {
			for (j in 1:portfolio[i, "cds"]){
				effort <- effort + portfolio[i, "effort"][j]
			}
		}
	}
	
	return(effort)
}

# Forward assessment of the portfolio
forwardAssessment <- function(portfolio){
	return(sum(portfolio[["effort"]]) - backwardAssessment(portfolio))
}

# Solves a linear programming model problem
solveProblem <- function(lpModel){
	lp.control(lpModel, timeout = 200)
	
	result <- solve(lpModel)
	
	status <- "error"
	if (result == 0){
		status = "solved"
	}
	else if (result == 1){
		status = "suboptimal"
	}
	else if (result == 2){
		status = "infeasible"
	}
	
	return(list(
		status = status,
		solution = get.variables(lpModel),
		effort = get.constraints(lpModel)[nrow(lpModel)],
		objective = get.objective(lpModel)
	))
}

# Builders of the apps to deploy along with app names
oabDefault = OptimizationAppBuilder$new(strategy = DefaultOptimizationStrategy$new())

# Creates the server
server <- Rhttpd$new()

# Deploys each strategy app
server$add(
	name = "default",
	app = oabDefault$app()
)

# Starts the server
server$start(port = 9876)

server

# Stay alive (ugly but effective...)
while (TRUE) {
    Sys.sleep(1);
}
