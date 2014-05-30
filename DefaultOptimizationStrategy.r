# DefaultOptimizationStrategy: default optimization strategy
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

DefaultOptimizationStrategy = setRefClass("DefaultOptimizationStrategy",
	contains = "OptimizationStrategy",
	methods = list(
		builObjectiveFunction = function(data){
			# Number of services
			nServices <- nrow(data$portfolio)
			
			# Objective function
			objFn <- rep(0, nServices * 5)
			objFn[1:nServices + 4 * nServices] <- data$portfolio[["count"]]
			
			# Adds a little negative weight proportional to the
			# effort in order to avoid equally optimal solutions
			# with different efforts
			actualObjFn <- objFn
			if (length(which(objFn > 0)) > 0){
				# Computes the effort needed for each design stage of each service
				effort <- rep(0, nServices * 5)
				for (l in 1:5){
					effort[1:nServices + (l - 1) * nServices] <- data$portfolio[["effort"]][, l]
					
					# Indexes of services for which the current stage has been already completed, w.r.t to the portfolio
					idxCompleted <- which(data$portfolio[["cds"]] >= l)
					
					# Effort needed is 0 for stages already completed
					if (length(idxCompleted) > 0) {
						effort[idxCompleted + (l - 1) * nServices] <- 0
					}
				}
				
				actualObjFn <- actualObjFn - effort / sum(effort) * min(actualObjFn[actualObjFn > 0])
			}
			
			return(actualObjFn)
		}
	)
)
