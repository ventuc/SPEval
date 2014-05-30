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
	methods = list(createModel = function(data, depth, budget){
		nServices <- nrow(data$portfolio)
		
		# Builds the constraints
		constraints <- buildConstraints(data, budget)
		
		# Builds the objective function
		objFn <- builObjectiveFunction(data)
		
		# Builds the linear programming model
		lpModel <- make.lp(nrow = 0, ncol = ncol(constraints$constraints))
		
		for (i in 1:nrow(constraints$constraints)){
			add.constraint(lpModel, constraints$constraints[i,], constraints$conditions[i], constraints$rhs[i])
		}
		
		dimnames(lpModel) <- list(rownames(constraints$constraints),colnames(constraints$constraints))
		
		set.type(lpModel, 1:(nServices * 5), "binary")
		set.objfn(lpModel, objFn)
		
		lp.control(lpModel, sense = 'max', bb.depthlimit = depth)
		
		return(lpModel)
	},
	buildConstraints = function(data, budget){
		# Number of services
		nServices <- nrow(data$portfolio)
		
		# -------------------------------------------------------------
		# Builds constraints on services structure, in order to respect
		# dependencies given by part-of relationships (one constraint
		# for each design stage of each composite service)
		# -------------------------------------------------------------
		
		# Composite services
		composites <- sort(unique(data$services[["parent"]]))
		nComposites <- length(composites)
		compositesNames <- data$portfolio[match(composites, data$portfolio[["id"]]), "name"]
		
		# Partial constraints matrix
		partialConstraints <- matrix(0, nrow = nComposites, ncol = nServices)
		for (i in 1:nComposites){
			# Index of the composite service w.r.t. the portfolio
			idxComposite <- match(composites[i], data$portfolio[["id"]])
			
			# Indexes of the elementary services w.r.t. the portfolio
			idxElementary <- match(data$services[["child"]][which(data$services[["parent"]] == composites[i])], data$portfolio[["id"]])
			
			partialConstraints[i, idxComposite] <- -length(idxElementary)
			partialConstraints[i, idxElementary] <- 1
		}
		
		# Constraint matrix (replicates the partial matrix for each design stage)
		constraints <- matrix(0, nrow = nComposites * 5, ncol = nServices * 5)
		colNames <- c()
		rowNames <- c()
		for (i in 1:5){
			constraints[((i - 1) * nComposites + 1):(i * nComposites), ((i - 1) * nServices + 1):(i * nServices)] <- partialConstraints
			colNames <- c(colNames, paste('cds', i, ' - ', data$portfolio[["name"]], sep = ''))
			rowNames <- c(rowNames, paste('cmpst[cds', i, ' - ', compositesNames, ']', sep = ''))
		}
		colnames(constraints) <- colNames
		
		# Right hand side constants and condition types
		rhs <- rep(0, nrow(constraints))
		conditions <- rep(">=", nrow(constraints))
		
		# -------------------------------------------------------------
		# Builds constraints due to dependencies between design stages
		# (one constraint for each couple of consecutive design stages
		# for each service)
		# -------------------------------------------------------------
		
		for (l in 1:4){
			for (i in seq_along(data$portfolio[["id"]])){
				constraint <- rep(0, nServices * 5)
				constraint[i + (l - 1) * nServices] <- 1
				constraint[i + l * nServices] <- -1
				constraints <- rbind(constraints, constraint)
			}
			rowNames <- c(rowNames, paste('dsdep[cds', (l + 1), ' - ', data$portfolio[["name"]], ']', sep = ''))
		}
		rownames(constraints) <- rowNames
		
		# Right hand side constants and condition types
		rhs <- c(rhs, rep(0, nServices * 4))
		conditions <- c(conditions, rep(">=", nServices * 4))
		
		# -------------------------------------------------------------
		# Builds the constraint that ensure the selection of services
		# already developed, and the constraint that limits the
		# selection to the available budget (design stage completed
		# don't impact on the computation of the total effort spent)
		# -------------------------------------------------------------
		
		budgetConstraint <- rep(0, nServices * 5)
		developedStagesConstraint <- rep(0, nServices * 5)
		for (l in 1:5){
			budgetConstraint[1:nServices + (l - 1) * nServices] <- data$portfolio[["effort"]][, l]
			
			# Indexes of services for which the current stage has been already completed, w.r.t to the portfolio
			idxCompleted <- which(data$portfolio[["cds"]] >= l)
			
			# Sets effort needed to 0 for stages already completed
			if (length(idxCompleted) > 0) {
				budgetConstraint[idxCompleted + (l - 1) * nServices] <- 0
				developedStagesConstraint[idxCompleted + (l - 1) * nServices] <- 1
			}
		}
		
		# Appends the constraints, the condition and the right hand side constants
		constraints <- rbind(constraints, "efbdg" = budgetConstraint)
		rhs <- c(rhs, budget)
		conditions <- c(conditions, "<=")
		
		constraints <- rbind(constraints, "dsdev" = developedStagesConstraint)
		rhs <- c(rhs, sum(developedStagesConstraint))
		conditions <- c(conditions, ">=")
		
		return(list(constraints = constraints, conditions = conditions, rhs = rhs))
	},
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
	})
)
