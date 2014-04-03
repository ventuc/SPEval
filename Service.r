# Service: this script starts a web server for web services requests
#
# Copyright (C) 2014  Claudio Venturini
#
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

library("Rook")

source("OptimizationStrategy.r")
source("DefaultOptimizationStrategy.r")

# Builder for optimization strategy apps
OptimizationAppBuilder = setRefClass("OptimizationAppBuilder",
	fields = list(strategy = "OptimizationStrategy"),
	methods = list(
		app = function(){
			return(
				function(env){
					print("called!");
					req = Rook::Request$new(env)
					numbers = as.numeric(unlist(strsplit(req$params()$numbers, ",")))
					results = list()
					results$mean = mean(numbers)
					results$sd = sd(numbers)
					res = Rook::Response$new()
					for (r in results){
						res$write("<p>")
						res$write(r)
						res$write("</p>")
					}
					res$finish()
				}
			)
		}
	)
)

# Builders of the apps to deploy along with app names
oabDefault = OptimizationAppBuilder$new(strategy = DefaultOptimizationStrategy$new())

# Creates the server
server <- Rhttpd$new()

# Deploys each strategy app
server$add(
	name = "default",
	app = oabDefault$app()
)

# test app
server$add(
	name ="summarize",
	app  = function(env){
		req = Rook::Request$new(env)
		numbers = as.numeric(unlist(strsplit(req$params()$numbers, ",")))
		results = list()
		results$mean = mean(numbers)
		results$sd = sd(numbers)
		res = Rook::Response$new()
		res$write(paste(results, sep=" "))
		res$finish()
	}
)

# Starts the server
server$start(port = 9876)

server

# Stay alive (ugly but effective...)
while (TRUE) {
    Sys.sleep(1);
}
