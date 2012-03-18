#  Copyright (C) 2011 Julia Schiffner
#  Copyright (C) Friedrich Leisch and Evgenia Dimitriadou
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

#' description
#'
#' These functions are based on the function \code{\link[mlbench]{mlbench.1spiral}} from package \pkg{mlbench}.
#'
#' @title Create Spiral Data
#'
#' @param n Number of observations.
#' @param cycles Number of cycles the spirals make.
#' @param data A \code{data.frame}.
#'
#' @return
#' returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#' @examples
#' # Generate a training and a test set
#' train <- spiralData(1000)
#' test <- spiralData(1000)
#'
#' # Generate a grid of points
#' x.1 <- x.2 <- seq(0.01,1,0.01)
#' grid <- expand.grid(x.1 = x.1, x.2 = x.2)
#'
#' # Calculate the posterior probablities for all grid points
#' gridPosterior <- spiralPosterior(grid)
#'
#' # Draw contour lines of posterior probabilities and plot training observations
#' contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
#' points(train$x, col = train$y)
#'
#' # Calculate Bayes error
#' ybayes <- spiralBayesClass(test$x)
#' mean(ybayes != test$y)
#'
#' if (require(MASS)) {
#'	
#' 	   # Fit an LDA model and calculate misclassification rate on the test data set
#'     tr <- lda(y ~ ., data = as.data.frame(train))	
#'     pred <- predict(tr, as.data.frame(test))	
#'     mean(pred$class != test$y)
#' 
#'     # Draw decision boundary
#'     gridPred <- predict(tr, grid)
#'     contour(x.1, x.2, matrix(gridPred$posterior[,1], length(x.1)), col = "red", levels = 0.5, add = TRUE)
#'
#' }
#'
# @references
#'
#' @aliases spiralData spiralLabels spiralPosterior spiralBayesClass
#'
#' @rdname spiralData
#'
#' @export
#' @import mlbench

# mlbench.1spiral <- function (n, cycles = 1, sd = 0) 
# {
    # w <- seq(0, by = cycles/n, length = n)
    # x <- matrix(0, nrow = n, ncol = 2)
    # x[, 1] <- (2 * w + 1) * cos(2 * pi * w)/3
    # x[, 2] <- (2 * w + 1) * sin(2 * pi * w)/3
    # if (sd > 0) {
        # e <- rnorm(n, sd = sd)
        # xs <- cos(2 * pi * w) - pi * (2 * w + 1) * sin(2 * pi * 
            # w)
        # ys <- sin(2 * pi * w) + pi * (2 * w + 1) * cos(2 * pi * 
            # w)
        # nrm <- sqrt(xs^2 + ys^2)
        # x[, 1] <- x[, 1] + e * ys/nrm
        # x[, 2] <- x[, 2] - e * xs/nrm
    # }
    # x
# }
# x <- seq(-1,1,0.1)
# grid <- expand.grid(x.1 = x, x.2 = x)
# p <- mlbench.1spiral(1000, cycles = 3)
# post <- apply(grid, 1, function(z) min(sqrt(colSums((t(p) - z)^2))))
# ## distance between 2 arms of spiral is sqrt(2)*2/3
# post[post > 1/3] <-  1/3
# p.min <- min(post)
# p.max <- max(post)
# post <- (post - p.min)/(p.max - p.min)
# image(x,x,matrix(post, length(x)))
# contour(x,x,matrix(post, length(x)), add = TRUE)
# points(p)



spiralData <- function(n = 100, cycles = 1) {
	sp <- mlbench:::mlbench.1spiral(n = 1000, cycles = cycles, sd = 0)
	r <- max(abs(range(sp[,1])))
	data <- matrix(rnorm(2 * n, sd = sqrt(r)), ncol = 2)
	posterior <- apply(data, 1, function(z) min(sqrt(colSums((t(sp) - z)^2))))
	posterior[posterior > 1/3] <- 1/3
	posterior.min <- min(posterior)
	posterior.max <- max(posterior)
	posterior <- (posterior - posterior.min)/(posterior.max - posterior.min)
	y <- as.factor(sapply(posterior, function(x) sample(1:2, size = 1, prob = c(1-x,x))))
	data <- list(x = data, y = y)
	class(data) <- c("locClass.spiralData", "locClass")
	attr(data, "cycles") <- cycles
	return(data)    
}



#' @return returns a factor of class labels.
#'
#' @rdname spiralData
#'
#' @export

spiralLabels <- function(data, cycles = 1) {
	sp <- mlbench:::mlbench.1spiral(n = 1000, cycles = cycles, sd = 0)
	posterior <- apply(data, 1, function(z) min(sqrt(colSums((t(sp) - z)^2))))
	posterior[posterior > 1/3] <- 1/3
	posterior.min <- min(posterior)
	posterior.max <- max(posterior)
	posterior <- (posterior - posterior.min)/(posterior.max - posterior.min)
	classes <- as.factor(sapply(posterior, function(x) sample(1:2, size = 1, prob = c(1-x,x))))
	return(classes)
}	



#' @return returns a matrix of posterior probabilities.
#'
#' @rdname spiralData
#'
#' @export

spiralPosterior <- function(data, cycles = 1) {
	sp <- mlbench:::mlbench.1spiral(n = 1000, cycles = cycles, sd = 0)
	posterior <- apply(data, 1, function(z) min(sqrt(colSums((t(sp) - z)^2))))
	posterior[posterior > 1/3] <- 1/3
	posterior.min <- min(posterior)
	posterior.max <- max(posterior)
	posterior <- (posterior - posterior.min)/(posterior.max - posterior.min)
    posterior <- cbind(1 - posterior, posterior)
    colnames(posterior) <- paste("posterior", 1:2, sep = ".")
	return(posterior)	
}



#' @return returns a factor of Bayes predictions.
#'
#' @rdname spiralData
#'
#' @export

spiralBayesClass <- function(data, cycles = 1) {
	sp <- mlbench:::mlbench.1spiral(n = 1000, cycles = cycles, sd = 0)
	posterior <- apply(data, 1, function(z) min(sqrt(colSums((t(sp) - z)^2))))
	posterior[posterior > 1/3] <- 1/3
	posterior.min <- min(posterior)
	posterior.max <- max(posterior)
	posterior <- (posterior - posterior.min)/(posterior.max - posterior.min)
	bayesclass <- as.factor(round(posterior) + 1)
	return(bayesclass)
}