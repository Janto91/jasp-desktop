#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

ReliabilityAnalysis <- function(dataset, options, perform = "run",
								callback = function(...) 0, state = NULL, ...) {

	variables <- unlist(options$variables)

	## Retrieve State
	# future idea: add correlation matrix to state and make all scale statistics modular.
	resultsAlpha <- state[["resultsAlpha"]]
	resultsAlpha[["ciAlpha"]] <- state[["confAlpha"]]

	# Store results

	results <- list()

	errorList <- NULL
	
	if (!is.null(variables) && length(variables) > 0) {
		if (is.null(resultsAlpha)) { # was the main analysis retrieved from state?
			
			# check for errors
			anyErrors <- .hasErrors(dataset = dataset, perform = perform,
									type = c("infinity", "variance", "observations"),
									observations.amount = " < 3",
									exitAnalysisIfErrors = TRUE)
			
			resultsAlpha <- .reliabilityResults(dataset, options, variables, perform)
			
		} else if (is.null(state[["confAlpha"]])) { # resultsAlpha retrieved from state, only recalculate CI

			resultsAlpha[["ciAlpha"]] <- .reliabilityAlphaCI(relyFit = resultsAlpha, ci = options[["confAlphaLevel"]])
			
		}
	}
	results[["reliabilityScale"]] <- .reliabalityScaleTable(resultsAlpha, dataset, options, variables, perform)

	if (options$alphaItem || options$gutmannItem || options$itemRestCor || options$meanItem || options$sdItem || options[["mcDonaldItem"]]) {
		results[["reliabilityItemsObj"]] <- .reliabalityItemsTable(resultsAlpha, options, variables, perform)
	} else {
		results[["reliabilityItemsObj"]] <- NULL
	}

	# Save state
	state <- list(
	  options = options,
	  resultsAlpha = resultsAlpha,
	  confAlpha = resultsAlpha[["ciAlpha"]]
	)

	if (perform == "init") {

		return(list(results=results, status="inited", state=state))

	} else {

		return(list(results=results, status="complete", state=state))

	}
}

.reliabilityResults <- function (dataset, options, variables, perform) {

	relyFit <- NULL

	if (perform == "run" && !is.null(variables) && length(variables) > 1) {

		# obtain smoothed correlation and covariance matrix
		dataList <- .reliabilityConvertDataToCorrelation(dataset, options)
		nObs <- nrow(dataset)
		nVar <- ncol(dataset)
		
		# generate key for reverse scaled items
		key <- NULL
		if (length(options[["reverseScaledItems"]]) > 0) {

			key <- rep(1, length(variables))
			key[match(.v(unlist(options[["reverseScaledItems"]])), colnames(dataset))] <- -1

		}

		# calculate chronbach alpha, gutmanns lambda6, and average inter item corrrelation
		relyFit <- psych::alpha(dataList[["covariance"]], key = key)
		
		# because we supply a correlation matrix and not raw data, we have to add these ourselves
		relyFit[["total"]][["mean"]] <- mean(dataList[["itemMeans"]])
		relyFit[["total"]][["sd"]] <- stats::sd(dataList[["itemMeans"]])
		relyFit[["item.stats"]][["mean"]] <- dataList[["itemMeans"]]
		relyFit[["item.stats"]][["sd"]] <- dataList[["itemSds"]]
		relyFit[["nObs"]] <- nObs
		
		# calculate confidence interval for chronbach alpha
		relyFit[["ciAlpha"]] <- .reliabilityAlphaCI(relyFit = relyFit,	ci = options[["confAlphaLevel"]])

		# calculate the greatest lower bound -- only possible for more than 2 variables.
		if (nVar < 3) {

			relyFit[["glb"]] <- "."

		} else { # try since the glb is error prone icm reverse scaled items. Requires further investigation/ this might be a bug in psych.

			relyFit[["glb"]] <- try(psych::glb(r = dataList[["correlation"]], key = key)[["glb.max"]], silent = TRUE)

		}

		# calculate McDonalds omega
		omega <- psych::omega(m = dataList[["correlation"]], nfactors = 1, flip = FALSE, plot = FALSE, 
							  n.iter = 1, n.obs = nObs)[["omega.tot"]]

		# calculate McDonalds omega if item dropped
		omegaDropped <- NULL
		if (nVar > 2) {
			omegaDropped <- numeric(length = nVar)
			for (i in 1:nVar) {
				omegaDropped[i] <- psych::omega(m = dataList[["correlation"]][-i, -i], 
												nfactors = 1, n.iter = 1, n.obs = nObs,
												flip = FALSE, plot = FALSE)[["omega.tot"]]
			}
		}
		else {
		  for (i in 1:nVar) {
		    omegaDropped[i] <- "."
		  }
		}

		relyFit[["omega"]] <- omega
		relyFit[["omegaDropped"]] <- omegaDropped

	}

	return(relyFit)

}

.reliabalityScaleTable <- function (r, dataset, options, variables, perform) {

  table <- jasp.data.frame(colnames=c("scale", "mean", "sd", "alpha", "lambda", "omega", "glb", "aic", "lowerCI", "upperCI"))

	if (!is.null(r)) {

		footnotes <- .newFootnotes()

		if (options[["missingValues"]] == "excludeCasesListwise") {

			exclwise = " listwise"

		} else {

			exclwise = " pairwise"

		}

		nObs <- nrow(dataset)
		nExcluded <- sum(!complete.cases(dataset))
		nValid <- nObs - nExcluded

		# message <- paste("Scale consists of items ", paste0(variables, collapse = ", "))
		message <- sprintf("Of the observations, %d were used, %d were excluded%s, and %d were provided.",
						   nValid, nExcluded, exclwise, nObs)

		if (options[["glbScale"]]) {

			if (length(variables) <= 2) {

				message <- paste(message, "Warning: Greatest lower bound can only be calculated for three or more variables.")

			} else if (isTryError(r[["glb"]])) {

				message <- paste(message, "Warning: Greatest lower bound could not be calculated.")

			}

		}

		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = message)
		attr(table, "jasp.footnotes") <- footnotes

		table <- rbind(table, c(scale = "scale", mean = r[["total"]][["mean"]], sd = r[["total"]][["sd"]], alpha = r[["total"]][["raw_alpha"]], lambda = r[["total"]][["G6(smc)"]], omega = r[["omega"]], glb = r[["glb"]], aic = r[["total"]][["average_r"]], lowerCI = r[["ciAlpha"]][[1]], upperCI = r[["ciAlpha"]][[2]]))

	} else {

	  table <- rbind(table, c(scale = "scale", mean = ".", sd = ".", alpha = ".", lambda = ".", omega = ".", glb = ".", aic = ".", lowerCI = ".", upperCI = "."))

	}

  if (options$confAlpha) {
    schema <- list(
      list(name="lowerCI", overTitle=paste0(options$confAlphaLevel * 100, "% Confidence Interval")),
      list(name="upperCI", overTitle=paste0(options$confAlphaLevel * 100, "% Confidence Interval"))
    )
    attr(table, "jasp.schema") <- schema
  }
  
  if (! options$alphaScale)
    table <- subset(table, select=-alpha)
  
  if (! options$gutmannScale)
    table <- subset(table, select=-lambda)
  
  if (! options$mcDonaldScale)
    table <- subset(table, select=-omega)
  
  if (! options$glbScale)
    table <- subset(table, select=-glb)
  
  if (! options$averageInterItemCor)
    table <- subset(table, select=-aic)
  
  if (! options$meanScale)
    table <- subset(table, select=-mean)
  
  if (! options$sdScale)
    table <- subset(table, select=-sd)
  
  if (! options$confAlpha)
    table <- subset(table, select=-c(lowerCI, upperCI))

	return(table)

}

.reliabalityItemsTable <- function (r, options, variables, perform) {

  table <- jasp.data.frame(colnames=c("variable", "mean", "sd", "irc", "alpha", "lambda", "omega"))

	footnotes <- .newFootnotes()

	if (length(options$reverseScaledItems) > 0) {
		message <- "reverse-scaled item"
		.addFootnote(footnotes, symbol = "\u207B", text=message)
		attr(table, "jasp.footnotes") <- footnotes
	}

	# can only be computed if there are at least 3 variables.
	if (options[["mcDonaldItem"]] && length(variables) < 3) {

		message <- "Warning: McDonald's \u03C9 if item dropped can only be calculated for three or more variables."
		.addFootnote(footnotes, text = message)

		attr(table, "jasp.footnotes") <- footnotes
	}


	if (!is.null(r)) {

		for (var in variables) {

		  index <- which(var == variables)

		  if (var %in% options$reverseScaledItems) {
		    case <- paste0(var,"\u207B")
		  } else {
		    case <- var
		  }

		  table <- rbind(table, c(variable = case, mean = r$item.stats[index,"mean"], sd = r$item.stats[index,"sd"], irc = r$item.stats[index,"r.drop"], alpha = r$alpha.drop[index,"raw_alpha"], lambda = r$alpha.drop[index, "G6(smc)"], omega = r[["omegaDropped"]][index]))

		}

	} else {

		variablesTemp <- variables

		if (is.null(variables))
			variablesTemp <- "..."

		for (var in variablesTemp) {

		  table <- rbind(table, c(variable=var, mean = ".", sd = ".", irc = ".", alpha = ".", lambda = ".", omega = "."))

		}
	}

	if (options$alphaItem || options$gutmannItem || options$mcDonaldItem) {
	  schema <- list(
	    list(name="alpha", overTitle=paste0("If item dropped")),
	    list(name="lambda", overTitle=paste0("If item dropped")),
	    list(name="omega", overTitle=paste0("If item dropped"))
	  )
	  attr(table, "jasp.schema") <- schema
	}
	
	if (! options$alphaItem)
	  table <- subset(table, select=-alpha)
	
	if (! options$gutmannItem)
	  table <- subset(table, select=-lambda)
	
	if (! options$mcDonaldItem)
	  table <- subset(table, select=-omega)
	
	if (! options$itemRestCor)
	  table <- subset(table, select=-irc)
	
	if (! options$meanItem)
	  table <- subset(table, select=-mean)
	
	if (! options$sdItem)
	  table <- subset(table, select=-sd)

	return(table)

}

.reliabilityAlphaCI <- function(relyFit, ci, nullAlpha = 0) {

	# code taken and modified from http://www.psyctc.org/stats/R/Feldt1.html
	# considering using the bootstrapped version inside psych as an alternative

	#***********************************************************#
	#* program using methods described in Feldt, Woodruff &    *#
	#* Salih (1987) Applied Psychological Measurement 11(1),   *#
	#* pp. 93-103 to carry out omnibus inferential test of     *#
	#* similarity of alpha values from a single sample         *#
	#***********************************************************#
	
	# relyFit is the output from psych::alpha and must contain the sample size as nObs
	# ci is the width of the confidence interval about obs.a desired
	
	estAlpha = relyFit[["total"]][["raw_alpha"]]
	nVar = relyFit[["nvar"]]
	nObs = relyFit[["nObs"]]

	if(estAlpha > nullAlpha) {
		f <- (1 - estAlpha) / (1 - nullAlpha)
	} else {
		f <- (1 - nullAlpha) / (1 - estAlpha)
	}
	nDen <- (nObs - 1) * (nVar - 1)
	nNum <- nObs - 1
	null.p <- stats::pf(f, nNum, nDen) # set the upper and lower p values for the desired C.I.
	p1 <- (1 - ci)/2
	p2 <- ci + p1 # corresponding F values
	f1 <- stats::qf(p1, nNum, nDen)
	f2 <- stats::qf(p2, nNum, nDen) # confidence interval
	lwr <- 1 - (1 - estAlpha) * f2
	upr <- 1 - (1 - estAlpha) * f1
	return(c(lwr, upr))
}

.reliabilityConvertDataToCorrelation <- function(dataset, options) {
	
	if (options[["missingValues"]] == "excludeCasesListwise") {
		
		dataset <- dataset[complete.cases(dataset), ]
		
	}
	
	means = colMeans(dataset, na.rm = TRUE)
	covmat <- stats::cov(dataset, use = "pairwise")
	stdev <- sqrt(diag(covmat))
	cormat <- psych::cor.smooth(stats::cov2cor(covmat), eig.tol = sqrt(.Machine[["double.eps"]]))
	
	return(list(
		correlation = cormat,
		itemSds = stdev,
		itemMeans = means,
		# direct line from: corpcor::rebuild.cov
		covariance = sweep(sweep(cormat, 1, stdev, "*"), 2, stdev, "*")
	))
	
}
