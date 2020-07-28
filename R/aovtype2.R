#' Anova Tables for Type 2 sums of squares
#'
#' Calculates and displays type-II analysis-of-variance tables for model objects 
#' produced by aov. This is a vastly reduced version of the Anova function from
#' package car
#'
#' Details about how the function works in order of steps taken.
#' Type-II tests are invariant with respect to (full-rank) contrast coding.
#' Type-II tests are calculated according to the principle of marginality, 
#' testing each term after all others, except ignoring the term's higher-order
#' relatives.  This definition of Type-II tests corresponds to the tests 
#' produced by SAS for analysis-of-variance models, where all of the 
#' predictors are factors, but not more generally (i.e., when there are 
#' quantitative predictors). 
#'
#' @usage aovtype2(mod)
#' @param mod aov model object from base R.
#' @return An object of class "anova", which usually is printed. 
#'
#' @references: Fox, J. (2016) Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.
#'
#' @author John Fox jfox@mcmaster.ca; as modified by Chuck Powell
#' @seealso \code{\link[stats]{aov}}
#' @examples
#'
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$am <- factor(mtcars$am)
#' mod <- aov(hp ~ cyl * am, data = mtcars)
#' aovtype2(mod)
#'   
#' @export
#'
aovtype2 <- function(mod) 
{
  if (class(mod)[[1]] != "aov") stop("Function is only for aov models")
    else class(mod) <- "lm"
  if (df.residual(mod) == 0) stop("residual df = 0")
  if (deviance(mod) < sqrt(.Machine$double.eps)) stop("residual sum of squares is 0 (within rounding error)")
  
  SS.term <- function(term){
    which.term <- which(term == names)
    subs.term <- which(assign == which.term)
    relatives <- relatives(term, names, fac)
    subs.relatives <- NULL
    for (relative in relatives) 
      subs.relatives <- c(subs.relatives, which(assign == relative))
    hyp.matrix.1 <- I.p[subs.relatives,,drop=FALSE]
    hyp.matrix.1 <- hyp.matrix.1[, not.aliased, drop=FALSE]
    hyp.matrix.2 <- I.p[c(subs.relatives,subs.term),,drop=FALSE]
    hyp.matrix.2 <- hyp.matrix.2[, not.aliased, drop=FALSE]
    hyp.matrix.term <- if (nrow(hyp.matrix.1) == 0) hyp.matrix.2
    else t(ConjComp(t(hyp.matrix.1), t(hyp.matrix.2), vcov(mod, complete=FALSE)))
    hyp.matrix.term <- hyp.matrix.term[!apply(hyp.matrix.term, 1, 
                                              function(x) all(x == 0)), , drop=FALSE]
    if (nrow(hyp.matrix.term) == 0)
      return(c(SS=NA, df=0))
    lh <- linearHypothesis(mod, 
                           hyp.matrix.term, 
                           singular.ok = TRUE)
    abs(c(SS=lh$"Sum of Sq"[2], df=lh$Df[2]))
  }
  
  not.aliased <- !is.na(coef(mod))
  if (!all(not.aliased))
    stop("there are aliased coefficients in the model")
  fac <- attr(terms(mod), "factors")
  intercept <- has.intercept(mod)
  I.p <- diag(length(coefficients(mod)))
  assign <- mod$assign
  assign[!not.aliased] <- NA
  names <- term.names(mod)
  if (intercept) names <-names[-1]
  n.terms <- length(names)
  p <- df <- f <- SS <- rep(0, n.terms + 1)
  sumry <- summary(mod, corr = FALSE)
  SS[n.terms + 1] <- sumry$sigma^2*mod$df.residual 
  
  df[n.terms + 1] <- mod$df.residual 
  p[n.terms + 1] <- f[n.terms + 1] <- NA
  for (i in 1:n.terms){
    ss <- SS.term(names[i])
    SS[i] <- ss["SS"]
    df[i] <- ss["df"]
    f[i] <- df[n.terms+1]*SS[i]/(df[i]*SS[n.terms + 1])
    p[i] <- pf(f[i], df[i], df[n.terms + 1], lower.tail = FALSE)
  }    
  result <- data.frame(SS, df, f, p)
  row.names(result) <- c(names,"Residuals")
  names(result) <- c("Sum Sq", "Df", "F value", "Pr(>F)")
  class(result) <- c("anova", "data.frame")
  attr(result, "heading") <- c("Anova Table (Type II tests)\n", 
                               paste("Response:", responseName(mod)))
  result
}


