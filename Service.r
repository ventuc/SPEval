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

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
#			"effort": [25, 10, 35, 15, 30]
#		},
#		{
#			"id": 2,
#			"ceq": 2,
#			"peq": 2,
#			"name": "pl1.S2",
#			"cds": 2,
#			"effort": [10, 5, 15, 10, 15]
#		},
#		{
#			"id": 3,
#			"ceq": 2,
#			"peq": 2,
#			"name": "pl1.S2",
#			"cds": 2,
#			"effort": [10, 5, 15, 10, 15]
#		}
#	],
#	"services": [
#		{
#			"parent": 1,
#			"child": 2,
#			"type": "part-of"
#		},
#		{
#			"parent": 2,
#			"child": 3,
#			"type": "part-of"
#		},
#		{
#			"parent": 3,
#			"child": 4,
#			"type": "is-a"
#		}
#	]
# }
#

library("Rook")
library("jsonlite")

source("OptimizationStrategy.r")
source("DefaultOptimizationStrategy.r")

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
						
						# Response
						res$header("Content-Type", "application/json")
						
						json = toJSON(input$data)
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
	
	# Sorts by ID
	data$portfolio <- data$portfolio[order(data$portfolio[["id"]]),]
	
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
			mismatch <- (length(which(apply(matrix(unlist(eqClassServices), nrow = 5, ncol = length(eqClassServices)), 1, function(i) length(unique(i))) > 1)) > 0)
			
			if (mismatch){
				stop(paste("Equivalence class ", class, " of type '", eqClassType, "' has not the same efforts for all services", sep = ''))
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
