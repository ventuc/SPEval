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
	methods = list(createModel = function(data, depth){
		nServices <- nrow(data$portfolio)
		
		# Builds the constraints
		constraints <- buildConstraints(data)
		
		print(constraints)

		##################################
		# OBJ FUNCTION
		##################################
		f_obj <- rep(0,nServices*nDesignStages)
		f_obj[1:nServices+(nDesignStages-1)*nServices] <- parameters[["count"]]
		
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
	},
	buildConstraints = function(data){
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
		rhs <- c(rhs, 0)
		conditions <- c(conditions, "<=")
		
		constraints <- rbind(constraints, "dsdev" = developedStagesConstraint)
		rhs <- c(rhs, sum(developedStagesConstraint))
		conditions <- c(conditions, ">=")
		
		return(list(constraints = constraints, conditions = conditions, rhs = rhs)
	})
)
