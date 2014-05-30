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

source("Model.r")

ModelBuilder = setRefClass("ModelBuilder",
	fields = list(data = "list", strategy = "OptimizationStrategy", .baseConstraints = "list"),
	methods = list(
		initialize = function(data, strategy){
			# Init
			.self$data <- data
			.self$strategy <- strategy
			
			# Builds and caches the generic constraints
			.self$.baseConstraints <- buildBaseConstraints()
		},
		generateModel = function(budget){
			# Builds the budget constraint ("efbdg")
			budgetConstraint <- buildBudgetConstraint(budget)
			
			# Merges all the constraints
			rowNames <- rownames(.baseConstraints$coeffs)
			coeffs <- rbind(.baseConstraints$coeffs, budgetConstraint$coeffs)
			rownames(coeffs) <- c(rowNames, "efbdg")
			
			constraints = list(
				coeffs = coeffs,
				conditions = c(.baseConstraints$conditions, budgetConstraint$condition),
				rhs = c(.baseConstraints$rhs, budgetConstraint$rhs)
			)
			
			# Builds the objective function
			objFn <- strategy$builObjectiveFunction(data)
			
			# Creates the problem model
			model <- Model$new(objFn, constraints)
			
			return(model)
		},
		buildBaseConstraints = function(){
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
			
			# Partial coefficients matrix
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
			coeffs <- matrix(0, nrow = nComposites * 5, ncol = nServices * 5)
			colNames <- c()
			rowNames <- c()
			for (i in 1:5){
				coeffs[((i - 1) * nComposites + 1):(i * nComposites), ((i - 1) * nServices + 1):(i * nServices)] <- partialConstraints
				colNames <- c(colNames, paste('cds', i, ' - ', data$portfolio[["name"]], sep = ''))
				rowNames <- c(rowNames, paste('cmpst[cds', i, ' - ', compositesNames, ']', sep = ''))
			}
			colnames(coeffs) <- colNames
			
			# Right hand side constants and condition types
			rhs <- rep(0, nrow(coeffs))
			conditions <- rep(">=", nrow(coeffs))
			
			# -------------------------------------------------------------
			# Builds constraints due to dependencies between design stages
			# (one constraint for each couple of consecutive design stages
			# for each service)
			# -------------------------------------------------------------
			
			for (l in 1:4){
				for (i in seq_along(data$portfolio[["id"]])){
					coeff <- rep(0, nServices * 5)
					coeff[i + (l - 1) * nServices] <- 1
					coeff[i + l * nServices] <- -1
					coeffs <- rbind(coeffs, coeff)
				}
				rowNames <- c(rowNames, paste('dsdep[cds', (l + 1), ' - ', data$portfolio[["name"]], ']', sep = ''))
			}
			rownames(coeffs) <- rowNames
			
			# Right hand side constants and condition types
			rhs <- c(rhs, rep(0, nServices * 4))
			conditions <- c(conditions, rep(">=", nServices * 4))
			
			# -------------------------------------------------------------
			# Builds the constraint that ensure the selection of services
			# already developed, and the constraint that limits the
			# selection to the available budget (design stage completed
			# don't impact on the computation of the total effort spent)
			# -------------------------------------------------------------
			
			developedStagesCoeff <- rep(0, nServices * 5)
			for (l in 1:5){
				# Indexes of services for which the current stage has been already completed, w.r.t to the portfolio
				idxCompleted <- which(data$portfolio[["cds"]] >= l)
				
				# Sets effort needed to 0 for stages already completed
				if (length(idxCompleted) > 0) {
					developedStagesCoeff[idxCompleted + (l - 1) * nServices] <- 1
				}
			}
			
			# Appends the constraints, the condition and the right hand side constants
			coeffs <- rbind(coeffs, "dsdev" = developedStagesCoeff)
			rhs <- c(rhs, sum(developedStagesCoeff))
			conditions <- c(conditions, ">=")
			
			return(list(coeffs = coeffs, conditions = conditions, rhs = rhs))
		},
		# Builds the constraint that limits the selection to the
		# available budget (design stage completed don't impact on the
		# computation of the total effort spent)
		buildBudgetConstraint = function(budget){
			# Number of services
			nServices <- nrow(data$portfolio)
			
			# Coefficients
			budgetCoeff <- rep(0, nServices * 5)
			for (l in 1:5){
				budgetCoeff[1:nServices + (l - 1) * nServices] <- data$portfolio[["effort"]][, l]
				
				# Indexes of services for which the current stage has been already completed, w.r.t to the portfolio
				idxCompleted <- which(data$portfolio[["cds"]] >= l)
				
				# Sets effort needed to 0 for stages already completed
				if (length(idxCompleted) > 0) {
					budgetCoeff[idxCompleted + (l - 1) * nServices] <- 0
				}
			}
			
			constraint <- list(coeffs = budgetCoeff, condition = "<=", rhs = budget)
			
			return(constraint)
		}
	)
)
