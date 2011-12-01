#  Copyright (C) 2011 J. Schiffner
#  Copyright (C) 1994-2003 W. N. Venables and B. D. Ripley
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

#' A local version of a single-hidden-layer neural network for classification that puts increased emphasis on a good model fit near the decision boundary.
#'
#' The idea of Hand and Vinciotti (2003) to put increased weight on observations near the decision boundary is generalized to the multiclass case and applied to 
#' neural networks.
#' Since the decision boundary is not known in advance an iterative procedure is required.
#' First, an unweighted neural network is fitted to the data. 
#' Based on the differences between the two largest estimated posterior probabilities observation weights are calculated.
#' Then a weighted neural network (see \code{\link[nnet]{nnet}}) from package \pkg{nnet} is fitted using these weights. 
#' Calculation of weights and model fitting is done several times in turn. 
#' The number of iterations is determined by the \code{itr}-argument that defaults to 3.
#'
#' The name of the window function (\code{wf}) can be specified as a character string.
#' In this case the window function is generated internally in \code{dalda}. Currently
#' supported are \code{"biweight"}, \code{"cauchy"}, \code{"cosine"}, \code{"epanechnikov"}, 
#' \code{"exponential"}, \code{"gaussian"}, \code{"optcosine"}, \code{"rectangular"} and 
#' \code{"triangular"}.
#'
#' Moreover, it is possible to generate the window functions mentioned above in advance 
#' (see \code{\link[=biweight]{wfs}}) and pass them to \code{dalda}. 
#'
#' Any other function implementing a window function can also be used as \code{wf} argument.
#' This allows the user to try own window functions.
#' See help on \code{\link[=biweight]{wfs}} for details.
#'
#' If the predictor variables include factors, the formula interface must be used in order 
#' to get a correct model matrix.
#'
#' In contrast to \code{\link[nnet]{nnet}} this function is only appropriate for classification
#' problems. As response in \code{formula} only factors are allowed. If the
#' response is not a factor, it is coerced to a factor with a warning.
#' An appropriate classification network is constructed; this has one output and entropy fit if the
#' number of levels is two, and a number of outputs equal to the number
#' of classes and a softmax output stage for more levels. 
#' If you use the default method, you get only meaningful results if \code{y} is a 0-1 class indicator
#' matrix.
#' 
#' Optimization is done via the BFGS method of \code{\link{optim}}.
#'
#' @title Discriminant Adaptive Neural Network
#'
#' @param formula A formula of the form \code{groups ~ x1 + x2 + \dots}, that is, the response
#' is the grouping \code{factor} and the right hand side specifies the (normally non-\code{factor})
#' discriminators.  
#' @param data A \code{data.frame} from which variables specified in \code{formula} are to be taken.
#' @param x (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} containing the explanatory variables.
#' @param y (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} of target values for examples.
#' @param weights Initial observation weights (defaults to a vector of 1s).
#' @param wf A window function which is used to calculate weights that are introduced into 
#'   the fitting process. Either a character string or a function, e.g. \code{wf = function(x) exp(-x)}.
#'   For details see the documentation for \code{\link[=biweight]{wfs}}.
#' @param bw (Required only if \code{wf} is a string.) The bandwidth parameter of the window function. (See \code{\link[=biweight]{wfs}}.)
#' @param k (Required only if \code{wf} is a string.) The number of nearest neighbors of the decision boundary to be used in the fitting process. 
#'   (See \code{\link[=biweight]{wfs}}.)
#' @param nn.only (Required only if \code{wf} is a string indicating a window function with infinite support and if \code{k} is specified.) Should
#' only the \code{k} nearest neighbors or all observations receive positive weights? (See \code{\link[=biweight]{wfs}}.)
#' @param itr Number of iterations for model fitting, defaults to 3. See also the Details section.
#' @param \dots Further arguments to \code{\link[nnet]{nnet}}.
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.) 
#' @param na.action A function to specify the action to be taken if NAs are found. The default action is first
#'   the \code{na.action} setting of \code{\link{options}} and second \code{\link{na.fail}} if that is unset. 
#'   An alternative is \code{\link{na.omit}}, which leads to rejection of cases with missing values on any required 
#'   variable. (NOTE: If given, this argument must be named.)
#' @param contrasts A list of contrasts to be used for some or all of the factors appearing as variables in the model formula.
#
# @param size Number of units in the hidden layer. Can be zero if there are skip-layer units.
# @param contrasts A list of contrasts to be used for some or all of the factors appearing as variables in the model formula.
# @param Wts Initial parameter vector. If missing chosen at random.
# @param mask Logical vector indicating which parameters should be optimized (default all).
# @param linout Switch for linear output units. Default logistic output units.
# @param entropy Switch for entropy (= maximum conditional likelihood) fitting. Default by least-squares.
# @param softmax Switch for softmax (log-linear model) and maximum conditional
#   likelihood fitting. \code{linout}, \code{entropy}, \code{softmax} and \code{censored} are mutually
#   exclusive.
# @param censored A variant on \code{softmax}, in which non-zero targets mean possible
#   classes. Thus for \code{softmax} a row of \code{(0, 1, 1)} means one example
#   each of classes 2 and 3, but for \code{censored} it means one example whose
#   class is only known to be 2 or 3.
# @param skip Switch to add skip-layer connections from input to output.
# @param rang Initial random weights on [-\code{rang}, \code{rang}].  Value about 0.5 unless the
#   inputs are large, in which case it should be chosen so that
#   \code{rang} * max(\code{|x|}) is about 1.
# @param decay Parameter for weight decay. Default 0.
# @param maxit Maximum number of iterations. Default 100.
# @param trace Switch for tracing optimization. Default \code{TRUE}.
# @param MaxNWts The maximum allowable number of weights.  There is no intrinsic limit
#   in the code, but increasing \code{MaxNWts} will probably allow fits that
#   are very slow and time-consuming.
# @param abstol Stop if the fit criterion falls below \code{abstol}, indicating an
#   essentially perfect fit.
# @param reltol  Stop if the optimizer is unable to reduce the fit criterion by a
#   factor of at least \code{1 - reltol}.
#'
#' @return
#' An object of class \code{"dannet"} or \code{"dannet.formula"} inheriting from \code{"nnet"}. A \code{list} mostly containing internal structure,
#' but with the following components: 
#'  \item{wts}{The best set of weights found.}
#'  \item{value}{Value of fitting criterion plus weight decay term.}
#'  \item{fitted.values}{The fitted values for the training data.}
#'  \item{residuals}{The residuals for the training data.}
#'  \item{convergence}{1 if the maximum number of iterations was reached, otherwise 0.}
#'  \item{weights}{A list of length \code{itr + 1}. The initial observation weights (a vector of 1s if none were given) and the observation
#'	  weights calculated in the individual iterations.}
#'  \item{itr}{The number of iterations used.}
#'  \item{wf}{The window function used. Always a function, even if the input was a string.}
#'  \item{bw}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The bandwidth used, \code{NULL} if \code{bw} was not specified.}
#'	\item{k}{(Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}}.) 
#'	  The number of nearest neighbors used, \code{NULL} if \code{k} was not specified.}
#'  \item{nn.only}{(Logical. Only if \code{wf} is a string or was generated by means of one of the functions documented in \code{\link[=biweight]{wfs}} and if \code{k} was
#'	 specified.) \code{TRUE} if only the \code{k} nearest neighbors recieve a positive weight, \code{FALSE} otherwise.}
#'  \item{adaptive}{(Logical.) \code{TRUE} if the bandwidth of \code{wf} is adaptive to the local density of data points, \code{FALSE} if the bandwidth
#'	  is fixed.}
#'  \item{call}{The (matched) function call.}
#'
#' @references Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' Ripley, B. D. (1996) \emph{Pattern Recognition and Neural Networks}. Cambridge.
#'
#' Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth edition. Springer.
#'
#' @seealso \code{\link{predict.dannet}}, \code{\link[nnet]{nnet}} and 
#' \code{\link{dalr}} for discriminant adaptive logistic regression.
#'
# @examples
# fit <- dalda(Species ~ Sepal.Length + Sepal.Width, data = iris,
#     wf = "gaussian", bw = 0.5)
# pred <- predict(fit)
# mean(pred$class != iris$Species)
#
#' @keywords classif multivariate
#'
#' @aliases dannet dannet.default dannet.formula
#'
#' @export
#'
#' @import nnet

dannet <- function(x, ...)
	UseMethod("dannet")	



#' @rdname dannet
#' @method dannet formula
#'
#' @S3method dannet formula

dannet.formula <- function(formula, data, weights, ..., subset, na.action, contrasts = NULL) {
    class.ind <- function(cl)
    {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval.parent(m$data)))
        m$data <- as.data.frame(data)
    m$... <- m$contrasts <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    x <- model.matrix(Terms, m, contrasts)
    cons <- attr(x, "contrast")
    xint <- match("(Intercept)", colnames(x), nomatch=0L)
    if (xint > 0L) x <- x[, -xint, drop=FALSE] # Bias term is used for intercepts
    w <- model.weights(m)
    if (length(w) == 0L) 
    	w <- rep(1, nrow(x))
    y <- model.response(m)
    if (!is.factor(y)) {
    	y <- as.factor(y)
    	warning("'y' was coerced to a factor")
    }
    lev <- lev1 <- levels(y)
    counts <- table(y)
    if (any(counts == 0L)) {
        empty <- lev[counts == 0L]
        warning(sprintf(ngettext(length(empty),
                                 "group %s is empty",
                                 "groups %s are empty"),
                        paste(empty, collapse=" ")), domain = NA)
        lev1 <- lev[counts > 0L]
        y <- factor(y, levels=lev1)
    }
    if (length(lev1) == 0L)
    	stop("training data from only one class given")
    if (length(lev) == 2L) {
        y <- as.vector(unclass(y)) - 1
        res <- dannet.default(x = x, y = y, weights = w, entropy = TRUE, ...)
    } else {
        y <- class.ind(y)
        res <- dannet.default(x = x, y = y, weights = w, softmax = TRUE, ...)
    }
    res$lev <- lev
    res$lev1 <- lev1
    res$terms <- Terms
    res$coefnames <- colnames(x)
    res$call <- match.call()
    res$na.action <- attr(m, "na.action")
    res$contrasts <- cons
    res$xlevels <- .getXlevels(Terms, m)
    class(res) <- c("dannet.formula", "dannet", "nnet")
    res
}



#' @rdname dannet
#' @method dannet default
#'
#' @S3method dannet default

##...: size, Wts, mask, linout, entropy, softmax, censored, skip, rang, decay, maxit, Hess, trace, MaxNWts, abstol, reltol, ...
dannet.default <- function(x, y, wf = c("biweight", "cauchy", "cosine", "epanechnikov", 
	"exponential", "gaussian", "optcosine", "rectangular", "triangular"), bw, k, nn.only, itr = 3, weights, ...) {
	dannet.fit <- function(x, y, wf, itr, weights = rep(1, nrow(x)), ...) {
		ntr <- nrow(x)
		w <- list()
		weights <- weights/sum(weights) * ntr     		# rescale weights such that they sum up to ntr
		w[[1]] <- weights
		res <- nnet.default(x = x, y = y, weights = weights, ...)
		for(i in seq_len(itr)) {
			post <- predict(res, type = "raw")
			if (any(!is.finite(post)))
				stop("inifinite, NA or NaN values in 'post', may indiciate numerical problems due to small observation weights, please check your settings of 'bw', 'k' and 'wf'")
			if (ncol(y) == 1L) {	             		# in this case predict.nnet returns posteriors for only one class
				w[[i+1]] <- wf(abs(2*as.vector(post) - 1))	# largest if both probabilities are equal
			} else {
				spost <- apply(post, 1, sort, decreasing = TRUE)
				w[[i+1]] <- wf(spost[1,] - spost[2,])    # largest if both probabilities are equal
			}
			w[[i+1]] <- w[[i+1]]/sum(w[[i+1]]) * ntr     # rescale weights such that they sum up to ntr
			if (any(w[[i]] %*% y == 0L)) {               # class where all weights are zero
				warning("training data from only one group, breaking out of iterative procedure")
				break
			} else {	
				res <- nnet.default(x = x, y = y, weights = w[[i+1]], ...)
			}
		}
		names(w) <- seq_along(w) - 1
		res$weights <- w
		return(res)
	}
    x <- as.matrix(x)
    y <- as.matrix(y)
    if (any(is.na(x)))
    	stop("missing values in 'x'")
    if (any(is.na(y)))
    	stop("missing values in 'y'")
    ntr <- nrow(x)

    if (!missing(itr)) {
    	if (itr < 1)
			stop("'itr' must be > 1")
    	if (abs(itr - round(itr)) > .Machine$double.eps^0.5)
       		warning("'itr' is not a natural number and is rounded off")
    }
    if (is.character(wf)) {
    	m <- match.call(expand.dots = FALSE)
    	m$n <- ntr
    	m[[1L]] <- as.name("generatewf")
    	wf <- eval.parent(m)
    } else if (is.function(wf)) {
    	if (!missing(k))
    		warning("argument 'k' is ignored")
    	if (!missing(bw))
    		warning("argument 'bw' is ignored")
    	if (!missing(nn.only))
    		warning("argument 'nn.only' is ignored")
    	if(!is.null(attr(wf, "adaptive"))) {
    		if(attr(wf, "adaptive")) {
    			if(!is.null(attr(wf, "k")) && attr(wf, "k") + 1 > ntr)
    				stop("'k + 1' is larger than 'nrow(x)'")
    		} else {
    			if(!is.null(attr(wf, "k")) && attr(wf, "k") > ntr)
    				stop("'k' is larger than 'nrow(x)'")
    		}
    	}
    } else
    	stop("argument 'wf' has to be either a character or a function")
#	if (!is.null(attr(wf, "adaptive")) && attr(wf, "adaptive") && attr(wf, "name") == "rectangular" && attr(wf, "k") == n) { # todo
#    	itr <- 0
#    	warning("nonlocal solution")	
#    }   
	res <- dannet.fit(x = x, y = y, wf = wf, itr = itr, weights = weights, ...)
    res <- c(res, list(itr = itr, wf = wf, bw = attr(wf, "bw"), k = attr(wf, "k"), nn.only = attr(wf, "nn.only"), adaptive = attr(wf, "adaptive")))
    cl <- match.call()
    cl[[1]] <- as.name("dannet")
    res$call <- cl
    class(res) <- c("dannet", "nnet")
    return(res)
}



#' @param x A \code{dannet} object.
#' @param ... Further arguments to \code{\link{print}}.
#'
#' @method print dannet
#' @nord
#'
#' @S3method print dannet

print.dannet <- function (x, ...) {
    NextMethod(x, ...)
    if(!is.null(attr(x$wf, "name"))) {
        cat("\nWindow function: ")
        cat(deparse(attr(x$wf, "name")), sep = "\n")  
    } else {
        cat("\nWindow function:\n")
        cat(deparse(x$wf), sep = "\n")  
    }
    if(!is.null(x$bw))
        cat("Bandwidth: ", x$bw, "\n")
    if(!is.null(x$k))
    	cat("k: ", x$k, "\n")
    if(!is.null(x$nn.only))
    	cat("Nearest neighbors only: ", x$nn.only, "\n")
    if(!is.null(x$adaptive))
    	cat("Adaptive bandwidth: ", x$adaptive, "\n")
    cat("Iterations: ", x$itr, "\n")
	invisible(x)
}



#  file nnet/man/predict.nnet.Rd
#  Copyright (C) 1994-9 W. N. Venables and B. D. Ripley
#' Predict new examples by a trained discriminant adaptive neural net.
#'
#' This function is a method for the generic function \code{predict()} for class 
#' \code{"dannet"}. 
#' It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
#' appropriate class, or directly by calling \code{predict.dannet(x)} regardless of 
#' the class of the object. 
#'
#' In contrast to \code{\link[nnet]{predict.nnet}} \code{predict.dannet} does not have
#' a \code{type} argument. Since \code{dannet} is only suitabe for classification, both, the 
#' predicted posterior probabilities and the class labels, are returned.
#'
#' @title Predict New Examples by a Trained Discriminant Adaptive Neural Net
#'
#' @param object Object of class \code{"dannet"}.
#' @param newdata A \code{matrix} or \code{data.frame} of test examples. A vector is considered to be
#' a row vector comprising a single case.
#' @param \dots Arguments passed to or from other methods.
#'
#' @return A \code{list} with components:
#' \item{class}{The predicted class labels (a \code{factor}).}
#' \item{posterior}{Matrix of class posterior probabilities.}
#'
#' @references
#' Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' Ripley, B. D. (1996) \emph{Pattern Recognition and Neural Networks}. Cambridge.
#'
#' Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth edition. Springer.
#'
#' @seealso \code{\link{dannet}}, \code{\link[nnet]{nnet}}.
#'
# @examples
# ## comparison with nnet:
# library(MASS)
# fit1 <- nnet(Species ~ Sepal.Length + Sepal.Width, data = iris)
# pred <- predict(fit1)
# mean(pred$class != iris$Species)
# 
# fit2 <- dannet(Species ~ Sepal.Length + Sepal.Width, data = iris,
#     wf = "gaussian", bw = 0.5)
# pred <- predict(fit2)
# mean(pred$class != iris$Species)
# 
# ## plot of decision boundary (maximum posterior probabilities):
# grid <- expand.grid(Sepal.Length = seq(4,8,0.1), Sepal.Width = seq(2,5,0.1)) 
# 
# predgrid1 <- predict(fit1, newdata = grid)$posterior
# predgrid2 <- predict(fit2, newdata = grid)$posterior
# 
# par(mfrow = c(1,2))
# contour(seq(4,8,0.1), seq(2,5,0.1), 
#     matrix(as.numeric(apply(predgrid1, 1, max)), nrow = length(seq(4,8,0.1))))
# contour(seq(4,8,0.1), seq(2,5,0.1), 
#     matrix(as.numeric(apply(predgrid2, 1, max)), nrow = length(seq(4,8,0.1))))
# points(iris$Sepal.Length, iris$Sepal.Width, pch = 19, 
#     cex = fit2$weights[[3]]*2, col = as.numeric(iris$Species))
# 
#' @keywords classif neural
#' 
#' @method predict dannet
#' @rdname predict.dannet
#'
#' @S3method predict dannet
#'
#' @import nnet

predict.dannet <- function(object, newdata, ...) {
    if (!inherits(object, "dannet")) 
        stop("object not of class \"dannet\"")
    posterior <- NextMethod(object, newdata, type = "raw", ...)
    if (length(object$lev) == 2) {
    	posterior <- cbind(1 - posterior, posterior) # posterior is probability for 2nd factor level
    	colnames(posterior) <- object$lev 
    }
	gr <- factor(object$lev1[max.col(posterior)], levels = object$lev)
	return(list(class = gr, posterior = posterior))
}