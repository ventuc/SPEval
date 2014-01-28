# SPEval.r: this file is a script that sets all the
# parameters and launches the functions designed to solve Service
# Portfolio Evaluation problems

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


rm(list = ls())

##################################
# PARAMETERS
##################################
wd <- '/Users/Marco/Desktop/ServiceReuse'
structureFile 				<- './data/CaseStudy_S.csv'
parametersFile 				<- './data/CaseStudy_P.csv'
backwardExe					<- TRUE
forwardExe					<- TRUE
productionLinesExe			<- TRUE
productionLinesLpFile		<- './problem/productionLinesProblem.lp'
productionLinesLogFile 		<- './log/productionLinesLog.txt'
productionLinesOutputFile	<- './output/productionLinesOutput.txt'
portfolioExe				<- TRUE
portfolioLpFile				<- './problem/portfolioProblem.lp'
portfolioLogFile 			<- './log/portfolioLog.txt'
portfolioOutputFile			<- './output/portfolioOutput.txt'
savingsTopExe				<- TRUE
savingsTopLogFile 			<- './log/savingsTopLog.txt'
savingsTopLpFile			<- './problem/savingsTopProblem.lp'
savingsTopOutputFile		<- './output/savingsTopOutput.txt'
savingsAnyExe				<- TRUE
savingsAnyLpFile			<- './problem/savingsAnyProblem.lp'
savingsAnyLogFile 			<- './log/savingsAnyLog.txt'
savingsAnyOutputFile		<- './output/savingsAnyOutput.txt'
nDesignStages				<- 5


effortRange					<- seq(0,1000,10)
depth						<- -50
timeout						<- 200

setwd(wd)
library("lpSolveAPI")
source('./Functions.r')

##################################
# READ DATA AND CREATE MODEL
##################################
productionLinesRawData	<- readData(structureFile,parametersFile,nDesignStages)
productionLinesData <- getClassStructure(productionLinesRawData$structure,productionLinesRawData$parameters,'PEq',nDesignStages)
portfolioData <- getClassStructure(productionLinesData$structure,productionLinesData$parameters,'CEq',nDesignStages)
cat('############################################\n')
cat('Production lines:\n')
cat('############################################\n')
print(productionLinesData)
cat('\n\n')
cat('############################################\n')
cat('Portfolio:\n')
cat('############################################\n')
print(portfolioData)
cat('\n\n')


##################################
# BACKWARD ASSESSMENT
##################################
if (backwardExe) {
	worstBackwardAssessment		<- assessment(productionLinesRawData$parameters,nDesignStages,'backward')
	currentBackwardAssessment	<- assessment(productionLinesData$parameters,nDesignStages,'backward')
	bestBackwardAssessment		<- assessment(portfolioData$parameters,nDesignStages,'backward')
	cat('############################################\n')
	cat('Backward assessment:\n')
	cat('############################################\n')
	cat('\tWorst:\t\t\t',worstBackwardAssessment,'\n')
	cat('\tCurrent:\t\t',currentBackwardAssessment,'\n')
	cat('\tBest:\t\t\t',bestBackwardAssessment,'\n')
	cat('\n\n')
}


##################################
# FORWARD ASSESSMENT
##################################
if (forwardExe) {
	productionLinesForwardAssessment	<- assessment(productionLinesData$parameters,nDesignStages,'forward')
	portfolioForwardAssessment			<- assessment(portfolioData$parameters,nDesignStages,'forward')
	cat('############################################\n')
	cat('Forward assessment:\n')
	cat('############################################\n')
	cat('\tProduction lines:\t',productionLinesForwardAssessment,'\n')
	cat('\tPortfolio:\t\t',portfolioForwardAssessment,'\n')
	cat('\n\n')
}


##################################
# EFFORT ALLOCATION PRODUCTION LINES
##################################
if (productionLinesExe) {
	cat('############################################\n')
	cat('Effort allocation production lines...')

	productionLinesModel <- createModel(productionLinesData$structure,productionLinesData$parameters,productionLinesLpFile,nDesignStages,depth,'count')
	outputResults(productionLinesModel$lps_model,productionLinesModel$f_obj,productionLinesData$parameters,effortRange,productionLinesLogFile,productionLinesOutputFile,timeout,'count')
	
	cat('\tDONE\n')
	cat('############################################\n')
}


##################################
# EFFORT ALLOCATION PORTFOLIO
##################################
if (portfolioExe) {
	cat('############################################\n')
	cat('Effort allocation portfolio...')

	portfolioModel <- createModel(portfolioData$structure,portfolioData$parameters,portfolioLpFile,nDesignStages,depth,'count')
	outputResults(portfolioModel$lps_model,portfolioModel$f_obj,portfolioData$parameters,effortRange,portfolioLogFile,portfolioOutputFile,timeout,'count')
	
	cat('\t\tDONE\n')
	cat('############################################\n')
}


##################################
# SAVINGS OPTIMIZATION TOP
##################################
if (savingsTopExe) {
	cat('############################################\n')
	cat('Top savings optimization...')

	savingsTopModel <- createModel(portfolioData$structure,portfolioData$parameters,savingsTopLpFile,nDesignStages,depth,'savingsMax')
	outputResults(savingsTopModel$lps_model,savingsTopModel$f_obj,portfolioData$parameters,effortRange,savingsTopLogFile,savingsTopOutputFile,timeout,'savingsMax')
	
	cat('\t\tDONE\n')
	cat('############################################\n')
}


##################################
# SAVINGS OPTIMIZATION ANY
##################################
if (savingsAnyExe) {
	cat('############################################\n')
	cat('Any savings optimization...')

	savingsAnyModel <- createModel(portfolioData$structure,portfolioData$parameters,savingsAnyLpFile,nDesignStages,depth,'savingsAny')
	outputResults(savingsAnyModel$lps_model,savingsAnyModel$f_obj,portfolioData$parameters,effortRange,savingsAnyLogFile,savingsAnyOutputFile,timeout,'savingsAny')
	
	cat('\t\tDONE\n')
	cat('############################################\n')
}
