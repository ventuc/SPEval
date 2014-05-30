# OptimizationStrategy: base reference class for optimization strategies
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

library("lpSolveAPI")

Model = setRefClass("Model",
	fields = list(depth = "integer", timeout = "integer", .model = "list"),
	methods = list(
		initialize = function(objectiveFunction, constraints, sense = "max"){
			# Inits timeout and depth
			.self$timeout <- as.integer(200)
			.self$depth <- as.integer(-50)
			
			# Builds the linear programming model
			lpModel <- make.lp(nrow = 0, ncol = ncol(constraints$coeffs))
			
			# Adds the constraints
			for (i in 1:nrow(constraints$coeffs)){
				add.constraint(lpModel, constraints$coeffs[i,], constraints$conditions[i], constraints$rhs[i])
			}
			dimnames(lpModel) <- list(rownames(constraints$coeffs),colnames(constraints$coeffs))
			
			# Variables are binary
			set.type(lpModel, 1:ncol(constraints$coeffs), "binary")
			
			# Sets the objective function
			set.objfn(lpModel, objectiveFunction)
			
			# Sets the optimization direction (maximization or minimization)
			if (length(sense) <= 0 || length(sense) > 1 || length(which(sense == c("min", "max"))) <= 0){
				stop("Sense not valid: must be either \"min\" or \"max\"")
			}
			lp.control(lpModel, sense = sense)
			
			.self$.model <- list(
				objectiveFunction = objectiveFunction,
				constraints = constraints,
				sense = sense,
				lpModel = lpModel
			)
		},
		# Gets the objective function of this problem model
		getObjectiveFunction = function(){
			return(.objectiveFunction)
		},
		# Gets the set of constraints of this problem model
		getConstraints = function(){
			return(.constraints)
		},
		# Gets the direction of optimization of this problem model
		getSense = function(){
			return(.sense)
		},
		# Gets the underlying lpSolveAPI linear programming model
		getModel = function(){
			return(.model)
		},
		# Gets the depth limit for branch & bound
		getDepth = function(){
			return(depth)
		},
		# Sets the depth limit for branch & bound
		setDepth = function(depth){
			if (mode(depth) != "numeric" || length(depth) <= 0 || length(depth) > 1){
				stop("Depth not valid")
			}
			
			.self$depth <- as.integer(depth)
		},
		# Gets the timeout for problem resolution, in seconds
		getTimeout = function(){
			return(timeout)
		},
		# Sets the timeout for problem resolution, in seconds
		setTimeout = function(timeout){
			if (mode(timeout) != "numeric" || length(timeout) <= 0 || length(timeout) > 1){
				stop("Timeout not valid")
			}
			
			.self$timeout <- as.integer(timeout)
		},
		# Solves this problem model
		solveProblem = function(){
			# Sets the depth limit and the timeout for the execution of branch & bound
			lp.control(.model$lpModel, bb.depthlimit = depth, timeout = timeout)
			
			# Solves
			result <- solve(.model$lpModel)
			
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
				solution = get.variables(.model$lpModel),
				effort = get.constraints(.model$lpModel)[nrow(.model$lpModel)],
				objective = get.objective(.model$lpModel)
			))
		}
	)
)
