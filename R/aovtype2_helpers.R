# Utility functions fom car package by J. Fox

ConjComp <- function(X, Z = diag( nrow(X)), ip = diag(nrow(X))) {
  xq <- qr(t(Z) %*% ip %*% X)
  if (xq$rank == 0) return(Z)
  Z %*% qr.Q(xq, complete = TRUE) [ ,-(1:xq$rank)] 
}

relatives <- function(term, names, factors){
  is.relative <- function(term1, term2) {
    all(!(factors[,term1]&(!factors[,term2])))
  }
  if(length(names) == 1) return(NULL)
  which.term <- which(term==names)
  (1:length(names))[-which.term][sapply(names[-which.term], 
                                        function(term2) is.relative(term, term2))]
}



print.univaov <- function(x, digits = max(getOption("digits") - 2L, 3L), 
                          style=c("wide", "long"), 
                          by=c("response", "term"),
                          ...){
  style <- match.arg(style)
  if (style == "wide") {
    cat("\n Type", x$type, "Sums of Squares\n")
    print(x$SS, digits=digits)
    cat("\n F-tests\n")
    F <- x$F
    print(round(F, 2))
    cat("\n p-values\n")
    p <- format.pval(x$p)
    p <- matrix(p, nrow=nrow(F))
    rownames(p) <- rownames(F)
    colnames(p) <- colnames(F)
    print(p, quote=FALSE)
    if (!is.null(x$p.adjust)){
      cat("\n p-values adjusted (by term) for simultaneous inference by", x$p.adjust.method, "method\n")
      p.adjust <- format.pval(x$p.adjust)
      p.adjust <- matrix(p.adjust, nrow=nrow(F))
      rownames(p.adjust) <- rownames(F)
      colnames(p.adjust) <- colnames(F)
      print(p.adjust, quote=FALSE)
    }
  }
  else {
    x.df <- as.data.frame(x, by=by)
    x.df$F <- round(x.df$F, 2)
    x.df$p <- format.pval(x.df$p)
    if (!is.null(x$p.adjust)) x.df$"adjusted p" <- format.pval(x.df$"adjusted p")
    cat("\n Type", x$type, "Sums of Squares and F tests\n")
    print(x.df, quote=FALSE, digits=digits)
  }
  invisible(x)
}

as.data.frame.univaov <- function(x, row.names, optional, by=c("response", "term"), ...) {
  melt <- function(data, varnames = names(dimnames(data)), value.name = "value") {
    dn <- dimnames(data)
    labels <- expand.grid( dn[[1]], dn[[2]])
    colnames(labels) <- varnames
    value_df <- setNames(data.frame(as.vector(data)), value.name)
    cbind(labels, value_df)
  }
  nv <- ncol(x$F)
  nt <- nrow(x$F)
  by <- match.arg(by)
  if (by=="response") {
    vn <- c("term", "response")
    df <- matrix(x$SS[1:nt, "df", drop=FALSE], nrow=nt, ncol=nv)
    SS <- melt(x$SS[1:nt, -1, drop=FALSE], varnames=vn, value.name="SS")	
    F <- melt(x$F, varnames=vn, value.name="F")
    p <- melt(x$p, varnames=vn, value.name="p")
    if (!is.null(x$p.adjust)) p.adjust <- melt(x$p.adjust, varnames=vn, value.name="adjusted p")
  }
  else {
    vn <- rev(c("term", "response"))
    df <- t(matrix(x$SS[1:nt, "df", drop=FALSE], nrow=nt, ncol=nv))
    SS <- melt(t(x$SS[1:nt, -1, drop=FALSE]), varnames=vn, value.name="SS")	
    F <- melt(t(x$F), varnames=vn, value.name="F")
    p <- melt(t(x$p), varnames=vn, value.name="p")
    if (!is.null(x$p.adjust)) p.adjust <- melt(t(x$p.adjust), varnames=vn, value.name="adjusted p")
  }
  
  result <- cbind(SS[,c(2,1,3)], df=c(df), F=F[,"F"], p=p[,"p"])
  if (!is.null(x$p.adjust)) result <- cbind(result, "adjusted p"=p.adjust[, "adjusted p"])
  result
}


assignVector <- function(model){
  m <- model.matrix(model)
  assign <- attr(m, "assign")
  if (!is.null(assign)) return (assign)
  m <- model.matrix(formula(model), data=model.frame(model))
  assign <- attr(m, "assign")
  if (!has.intercept(model)) assign <- assign[assign != 0]
  assign
}



has.intercept <- function (model, ...) {
	UseMethod("has.intercept")
}

has.intercept.default <- function(model, ...) any(names(coefficients(model))=="(Intercept)")

term.names <- function (model, ...) {
	UseMethod("term.names")
}

term.names.default <- function (model, ...) {
	term.names <- labels(terms(model))
	if (has.intercept(model)) c("(Intercept)", term.names)
	else term.names
}

predictor.names <- function(model, ...) {
	UseMethod("predictor.names")
}

predictor.names.default <- function(model, ...){
	predictors <- attr(terms(model), "variables")
	as.character(predictors[3:length(predictors)])
}

responseName <- function (model, ...) {
	UseMethod("responseName")
}

responseName.default <- function (model, ...) deparse(attr(terms(model), "variables")[[2]])

response <- function(model, ...) {
	UseMethod("response")
}

response.default <- function (model, ...) model.response(model.frame(model))

is.aliased <- function(model){
	!is.null(alias(model)$Complete)
}

df.terms <- function(model, term, ...){
	UseMethod("df.terms")
}

df.terms.default <- function(model, term, ...){
	if (is.aliased(model)) stop("Model has aliased term(s); df ambiguous.")
	if (!missing(term) && 1 == length(term)){
		assign <- attr(model.matrix(model), "assign")
		which.term <- which(term == labels(terms(model)))
		if (0 == length(which.term)) stop(paste(term, "is not in the model."))
		sum(assign == which.term)
	}
	else {
		terms <- if (missing(term)) labels(terms(model)) else term
		result <- numeric(0)
		for (term in terms) result <- c(result, Recall(model, term))
		names(result) <- terms
		result
	}
}

inv <- function(x) solve(x)

coefnames2bs <- function(g, para.names, parameterPrefix="b"){
	metas <- c("(", ")", "[", "]", "{", "}", ".", "*", "+", "^", "$", ":", "|")
	metas2 <- paste("\\", metas, sep="")
	metas3 <- paste("\\\\", metas, sep="")
	for (i in seq(along=metas))
		para.names <- gsub(metas2[i], metas3[i], para.names) # fix up metacharacters
	para.order <- order(nchar(para.names), decreasing=TRUE)
	para.names <- para.names[para.order] # avoid partial-name substitution
	std.names <- if ("(Intercept)" %in% para.names)
				paste(parameterPrefix, 0:(length(para.names) - 1), sep = "")
			else paste(parameterPrefix, 1:length(para.names), sep = "")
	std.names.ordered <- std.names[para.order]
	for (i in seq(along=para.names)){
		g <- gsub(para.names[i], std.names.ordered[i], g)
	}
	list(g=g, std.names=std.names)
}


has.intercept.matrix <- function (model, ...) {
  "(Intercept)" %in% colnames(model)
}


printHypothesis <- function(L, rhs, cnames){
  hyp <- rep("", nrow(L))
  for (i in 1:nrow(L)){
    sel <- L[i,] != 0
    h <- L[i, sel]
    h <- ifelse(h < 0, as.character(h), paste("+", h, sep=""))
    nms <- cnames[sel]
    h <- paste(h, nms)
    h <- gsub("-", " - ", h)
    h <- gsub("+", "  + ", h, fixed=TRUE)
    h <- paste(h, collapse="")
    h <- gsub("  ", " ", h, fixed=TRUE)
    h <- sub("^\\ \\+", "", h)
    h <- sub("^\\ ", "", h)
    h <- sub("^-\\ ", "-", h)
    h <- paste(" ", h, sep="")
    h <- paste(h, "=", rhs[i])
    h <- gsub(" 1([^[:alnum:]_.]+)[ *]*", "",
              gsub("-1([^[:alnum:]_.]+)[ *]*", "-",
                   gsub("- +1 +", "-1 ", h)))
    h <- sub("Intercept)", "(Intercept)", h)
    h <- gsub("-", " - ", h)
    h <- gsub("+", "  + ", h, fixed=TRUE)
    h <- gsub("  ", " ", h, fixed=TRUE)
    h <- sub("^ *", "", h)
    hyp[i] <- h
  }
  hyp
}

linearHypothesis <- function (model, ...)
  UseMethod("linearHypothesis")

lht <- function (model, ...)
  UseMethod("linearHypothesis")

linearHypothesis.nlsList <- function(model,  ..., vcov., coef.){
  vcov.nlsList <- function(object, ...) {
    vlist <- lapply(object, vcov)
    ng <- length(vlist)
    nv <- dim(vlist[[1]])[1]
    v <- matrix(0, nrow=ng*nv, ncol=ng*nv)
    for (j in 1:ng){
      cells <- ((j-1)*nv + 1):(j*nv)
      v[cells, cells] <- vlist[[j]]
    }
    v
  }
  linearHypothesis.default(model, vcov.=vcov.nlsList(model), 
                           coef.=unlist(lapply(model, coef)), ...)}


linearHypothesis.default <- function(model, hypothesis.matrix, rhs=NULL,
                                     test=c("Chisq", "F"), vcov.=NULL, singular.ok=FALSE, verbose=FALSE,
                                     coef. = coef(model), ...){
  df <- df.residual(model)
  if (is.null(df)) df <- Inf ## if no residual df available
  if (df == 0) stop("residual df = 0")
  V <- if (is.null(vcov.)) vcov(model, complete=FALSE)
  else if (is.function(vcov.)) vcov.(model) else vcov.
  b <- coef.
  if (any(aliased <- is.na(b)) && !singular.ok)
    stop("there are aliased coefficients in the model")
  b <- b[!aliased]
  if (is.null(b)) stop(paste("there is no coef() method for models of class",
                             paste(class(model), collapse=", ")))
  if (is.character(hypothesis.matrix)) {
    L <- makeHypothesis(names(b), hypothesis.matrix, rhs)
    if (is.null(dim(L))) L <- t(L)
    rhs <- L[, NCOL(L)]
    L <- L[, -NCOL(L), drop = FALSE]
    rownames(L) <- hypothesis.matrix
  }
  else {
    L <- if (is.null(dim(hypothesis.matrix))) t(hypothesis.matrix)
    else hypothesis.matrix
    if (is.null(rhs)) rhs <- rep(0, nrow(L))
  }
  q <- NROW(L)
  value.hyp <- L %*% b - rhs
  vcov.hyp <- L %*% V %*% t(L)
  if (verbose){
    cat("\nHypothesis matrix:\n")
    print(L)
    cat("\nRight-hand-side vector:\n")
    print(rhs)
    cat("\nEstimated linear function (hypothesis.matrix %*% coef - rhs)\n")
    print(drop(value.hyp))
    cat("\n")
    if (length(vcov.hyp) == 1) cat("\nEstimated variance of linear function\n")
    else cat("\nEstimated variance/covariance matrix for linear function\n")
    print(drop(vcov.hyp))
    cat("\n")
  }
  SSH <- as.vector(t(value.hyp) %*% solve(vcov.hyp) %*% value.hyp)
  test <- match.arg(test)
  if (!(is.finite(df) && df > 0)) test <- "Chisq"
  name <- try(formula(model), silent = TRUE)
  if (inherits(name, "try-error")) name <- substitute(model)
  title <- "Linear hypothesis test\n\nHypothesis:"
  topnote <- paste("Model 1: restricted model","\n", "Model 2: ",
                   paste(deparse(name), collapse = "\n"), sep = "")
  note <- if (is.null(vcov.)) ""
  else "\nNote: Coefficient covariance matrix supplied.\n"
  rval <- matrix(rep(NA, 8), ncol = 4)
  colnames(rval) <- c("Res.Df", "Df", test, paste("Pr(>", test, ")", sep = ""))
  rownames(rval) <- 1:2
  rval[,1] <- c(df+q, df)
  if (test == "F") {
    f <- SSH/q
    p <- pf(f, q, df, lower.tail = FALSE)
    rval[2, 2:4] <- c(q, f, p)
  }
  else {
    p <- pchisq(SSH, q, lower.tail = FALSE)
    rval[2, 2:4] <- c(q, SSH, p)
  }
  if (!(is.finite(df) && df > 0)) rval <- rval[,-1]
  result <- structure(as.data.frame(rval),
                      heading = c(title, printHypothesis(L, rhs, names(b)), "", topnote, note),
                      class = c("anova", "data.frame"))
  attr(result, "value") <- value.hyp
  attr(result, "vcov") <- vcov.hyp
  result
}


linearHypothesis.lm <- function(model, hypothesis.matrix, rhs=NULL,
                                test=c("F", "Chisq"), vcov.=NULL,
                                singular.ok=FALSE, ...){
  if (df.residual(model) == 0) stop("residual df = 0")
  if (deviance(model) < sqrt(.Machine$double.eps)) stop("residual sum of squares is 0 (within rounding error)")
  if (!singular.ok && is.aliased(model))
    stop("there are aliased coefficients in the model.")
  test <- match.arg(test)
  rval <- linearHypothesis.default(model, hypothesis.matrix, rhs = rhs,
                                   test = test, vcov. = vcov., singular.ok=singular.ok, ...)
  if (is.null(vcov.)) {
    rval2 <- matrix(rep(NA, 4), ncol = 2)
    colnames(rval2) <- c("RSS", "Sum of Sq")
    SSH <- rval[2,test]
    if (test == "F") SSH <- SSH * abs(rval[2, "Df"])
    df <- rval[2, "Res.Df"]
    error.SS <- deviance(model)
    rval2[,1] <- c(error.SS + SSH * error.SS/df, error.SS)
    rval2[2,2] <- abs(diff(rval2[,1]))
    rval2 <- cbind(rval, rval2)[,c(1, 5, 2, 6, 3, 4)]
    class(rval2) <- c("anova", "data.frame")
    attr(rval2, "heading") <- attr(rval, "heading")
    attr(rval2, "value") <- attr(rval, "value")
    attr(rval2, "vcov") <- attr(rval, "vcov")
    rval <- rval2
  }
  rval
}

