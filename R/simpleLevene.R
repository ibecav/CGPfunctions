# moved from Rcmdr 13 July 2004

# levene.test.default function slightly modified and generalized from Brian Ripley via R-help
# the original generic version was contributed by Derek Ogle
# last modified 2019-02-01 by J. Fox

# leveneTest <- function (y) {
#   UseMethod("leveneTest") 
# }

leveneTest.default <- function (y, group) { # original levene.test
  if (!is.numeric(y)) 
    stop(deparse(substitute(y)), " is not a numeric variable")
  if (!is.factor(group)) {
    warning(deparse(substitute(group)), " coerced to factor.")
    group <- as.factor(group)
  }
  valid <- complete.cases(y, group)
  meds <- tapply(y[valid], group[valid], median)
  resp <- abs(y - meds[group])
  table <- anova(lm(resp ~ group))[, c(1, 4, 5)]
  rownames(table)[2] <- " "
  attr(table, "heading") <- paste("Brown-Forsythe Test for Homogeneity of Variance using median", sep="")
  table
}


leveneTest.formula <- function(y, data) {
  form <- y
  mf <- if (missing(data)) model.frame(form) else model.frame(form, data)
  if (any(sapply(2:dim(mf)[2], function(j) is.numeric(mf[[j]])))) 
    stop("Levene's test is not appropriate with quantitative explanatory variables.")
  y <- mf[,1]
  if(dim(mf)[2]==2) group <- mf[,2]
  else {
    if (length(grep("\\+ | \\| | \\^ | \\:",form))>0) stop("Model must be completely crossed formula only.")
    group <- interaction(mf[,2:dim(mf)[2]])
  }
  leveneTest.default(y = y, group = group)
}

# 
# 
# leveneTest.aov <- function(y) {
#   m <- model.frame(y)
#   m$..y <- model.response(m)
#   f <- formula(y)
#   f[2] <- expression(..y)
#   leveneTest.formula(f, data=m)
# }