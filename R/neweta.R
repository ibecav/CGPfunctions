#' Calculates eta squared for an AOV model using the Type II method
#' 
#' Takes an aov object and returns a standard AOV table with eta squared
#' computed
#' 
#' There are three ways to compute eta squared this function only uses Type II
#' 
#' **This function is deprecated as of version 0.5** please consider using
#' \code{\link[sjstats]{eta_sq}} instead
#' 
#' @usage neweta(MyAOV)
#' @param MyAOV a valid aov object such as those produced by
#' \code{aov(dv~iv1*iv2)}
#' @return Returns a tibble containing the AOV output similar to
#' \code{summary(aov(MyAOV))} but with eta squared computed and appended as an
#' additional column
#' @author Chuck Powell
#' @seealso \code{\link{Plot2WayANOVA}}
#' @references neweta function is a shortened and simplified verion of Dani
#' Navarro's \code{\link[lsr]{etaSquared}}
#' @examples
#' 
#' neweta(aov(mpg~am*cyl, mtcars))
#' @export
#' 
neweta <- function (MyAOV)
{
  ss.tot <- sum((MyAOV$model[, 1] - mean(MyAOV$model[, 1]))^2)
  ss.res <- sum((MyAOV$residuals)^2)
  terms <- attr(MyAOV$terms, "factors")[-1, , drop = FALSE]
  l <- attr(MyAOV$terms, "term.labels")
  ss <- matrix(NA, length(l), 1)
  rownames(ss) <- l
  for (i in seq_along(ss)) {
    vars.this.term <- which(terms[, i] != 0)
    dependent.terms <- which(apply(terms[vars.this.term, , drop = FALSE], 2, prod) > 0)
    m0 <- lm(MyAOV$terms[-dependent.terms], MyAOV$model)
    if (length(dependent.terms) > 1) {
      m1 <- lm(MyAOV$terms[-setdiff(dependent.terms, i)], MyAOV$model)
      ss[i] <- anova(m0, m1)$`Sum of Sq`[2]
    }
    else {
      ss[i] <- anova(m0, MyAOV)$`Sum of Sq`[2]
    }
  }
  ss <- rbind(ss, ss.res)
  eta2 <- ss/ss.tot
  k <- length(ss)
  df <- anova(MyAOV)[, "Df"]
  ms <- ss/df
  Fval <- ms/ms[k]
  p <- 1 - pf(Fval, df, rep.int(df[k], k))
  E <- cbind(df, ss, ms, Fval, p, eta2)
  E[k, 4:5] <- NA
  colnames(E) <- c("Df", "Sum Sq", "Mean Sq", "F value", "p", "eta sq")
  rownames(E) <- rownames(ss)
  rownames(E)[k] <- "Residuals"
  E<-as_tibble(E,rownames = "Source")
  E$Source<-factor(E$Source)
  E$Df<-as.integer(E$Df)
  E$`F value` <- round(E$`F value`,1)
  E$p <- round(E$p,3)
  E$`eta sq` <- round(E$`eta sq`,3)
  E$sigstars <- unclass(symnum(E$p, corr = FALSE, na = TRUE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "")))
  E[4, 8] <- NA
  E <- E[,c(1:6,8,7)]
  return(tibble::as_tibble(E))
}
