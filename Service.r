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
						# Clean data
						input$data <- cleanData(input$data)
						
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

cleanData = function(data){
	# Retains only part-of relationships between services
	data$services <- data$services[which(data$services[["type"]] == "part-of"),]
	
	
	return(data)
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
