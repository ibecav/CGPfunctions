#' @title Justification for titles, subtitles and captions.
#' @name justifyme
#' @author Chuck Powell
#'
#' @param x A numeric or character vector.
#' @return a numeric value suitable for `ggplot2` `hjust` value.
#'
#' @keywords internal

justifyme <- function(x) {
  if (tolower(stringr::str_sub(x, 1, 1)) %in% c("l", "c", "r")) {
    whichletter <- tolower(stringr::str_sub(x, 1, 1))
    justification <- dplyr::case_when(
      whichletter == "l" ~ 0,
      whichletter == "c" ~ .5,
      whichletter == "r" ~ 1,
      TRUE ~ .5
    )
  } else if (x >= 0 && x <= 1) {
    justification <- x
  } else {
    justification <- 0.5
  }
  # return the changed plot
  return(justification)
}

#' Test whether a vector or df column is not of type factor
#' @param x an integer
#' @keywords internal
#' @noRd
not_a_factor <- function(x){
  !is.factor(x)
}

#' @title Choose display type for BF formatting.
#' @name bf_display
#' @author Chuck Powell
#'
#' @param bf A numeric vector containing one or more BF values.
#' @param display_type A string containing which option one of
#'   "support", "logged", or "sensible".
#' @param k A numeric for the number of rounded digits.
#' @return a formatted character string.
#'

bf_display <- function(bf = NULL,
                       display_type = "bf",
                       k = 2) {
  results <- tibble::enframe(bf, name = NULL, value = "bf")
  results <- results %>%
    mutate(
      support = case_when(
        bf < .01 ~ " data support is extreme",
        bf < .03 & bf >= .01 ~ " data support is very strong",
        bf < .1 & bf >= .03 ~ " data support is strong",
        bf < 1 / 3 & bf >= .1 ~ " data support is moderate",
        bf < 1 & bf >= 1 / 3 ~ " data support is anecdotal",
        bf >= 1 & bf < 3 ~ " data support is anecdotal",
        bf >= 3 & bf < 10 ~ " data support is moderate",
        bf >= 10 & bf < 30 ~ " data support is strong",
        bf >= 30 & bf < 100 ~ " data support is very strong",
        bf >= 100 ~ " data support is extreme"
      )
    ) %>%
    mutate(logged = case_when(
      bf < 1 ~ paste0(" log(BF01)=", round(log(1 / bf), k)),
      bf >= 1 ~ paste0(" log(BF10)=", round(log(bf), k))
          )) %>%
    mutate(sensible = case_when(
      bf < 1 ~ paste0(" is ", number_to_word(1 / bf), " to 1"),
      bf >= 1 ~ paste0(" is ", number_to_word(bf), " to 1")
          )) %>%
    mutate(astext = case_when(
      bf < 1 ~ paste0("=", round((1 / bf), k)),
      bf >= 1 ~ paste0("=", round(bf, k))
    ))
  
  if (display_type == "support") {
    return(pull(results, support))
  } else if (display_type == "log") {
    return(pull(results, logged))
  } else if (display_type == "sensible") {
    return(pull(results, sensible))
  } else {
    return(pull(results, astext))
  }
  return(results)
}


#### -----  internal function to format p values ----- ####
#' Internal function to format p values
#' @param pvals a vector of p values
#' @keywords internal
#' @noRd
pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE) {
  
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }
  
  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
          return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2)) else
        return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}

#' @name number_to_word
#' @title Convert a vector of numbers to large-number word representation
#' @description Converts a vector of numbers to a character string approximation
#'   using the "short scale" version of large number names. e.g. 312e6 returns
#'   as '300 million.' Simultaneously returns a numeric representation of the
#'   approximation.
#' @author Tom Hopper
#' @param x A vector of numbers to convert.
#' @param nsmall Optional. An integer number of digits to include to the right of the the leading digit
#' @return A string representation of the number
#' @keywords internal

number_to_word <- function(x, nsmall = 0) {
  # provide the short scale version (used in American English)
  lookup <- tibble(expon = c(33, 30, 27, 24, 21, 18, 15, 12, 9, 6, 3,
                             0, -3, -6, -9, -12),
                   word = c("decillion", "nonillian", "octillian",
                            "septillion", "sextillion", "quintillion", "quadrillion",
                            "trillion", "billion", "million", "thousand",
                            "", "thousandth", "millionth", "billionth",
                            "trillionth"))
  
  # Get the exponent and round to the nearest multiple of 3 so
    # we can look up values in our lookup table and return the number
    # with the correct digits.
    x_exp <- exponent(x)
    x_exp <- ifelse(x_exp != 0, floor(x_exp / 3.0) * 3, 0)
    # Look up the word for the number
    x_name <- rep(NA, times = length(x_exp))
    for (i in 1:length(x_exp)) {
      if (is.na(x_exp[i]) | is.infinite(abs(x_exp[i]))) {
        x_name[i] <- ""
      } else {
        x_name[i] <- ifelse(x_exp[i] != 0, lookup$word[lookup$expon == x_exp[i]], "")
      }
    }
    # Convert x to a number with -3 < exponent() < 3
    # then combine the number and x_name.
    # e.g. 200,000,000,000 should return
    # "200 billion"
    x_n <- rep(NA, times = length(x_exp))
    for (i in 1:length(x_exp)) {
      if (is.na(x_exp[i])) {
        x_n[i] <- NA
      } else {
        if (is.infinite(x_exp[i])) {
          x_n[i] <- ifelse(x_exp[i] > 0, x[i], 0)
        } else {
          if (abs(x_exp[i]) >= 3) {
            x_n[i] <- x[i] / 10^x_exp[i]
            x_n[i] <- round(x_n[i] / 10^exponent(x_n[i]), nsmall) * 10^exponent(x_n[i])
          } else {
            if (is.na(x_exp[i])) {
              x_n[i] <- NA
            } else {
              x_n[i] <- round(x[i] / 10^(exponent(x[i])-1), nsmall) * 10^(exponent(x[i]) - 1)
            }
          }
        }
      }
    }
    # Create word equivalent of approximate number
    x_name <- paste0(as.character(x_n), ifelse(nchar(x_name > 0), " ", ""), x_name)
    x_name <- trimws(x_name)
    x_name <- ifelse(x_name == "NA", NA, x_name)
    x_n <- x_n * 10 ^ x_exp

  # Return string representation
  return(x_name)
}

## Extract mantissa and exponent from vector ####
#' @name exponent
#' @title Exponent of a number in scientific notation
#' @description Returns the exponent of a number as it is written in scientific
#'   notation (powers of 10).
#' @references Thanks to Stackoverflow answer by Paul McMurdie \url{https://stackoverflow.com/a/25555105}
#' @author Tom Hopper
#' @param x (required) numeric. A number.
#' @return the exponent of the scientific notation representation of the number \code{x}
#' @keywords internal
#'  

exponent <- function(x) {
  # check missing
  if (!missing(x)) {
    if (is.numeric(x)) {
      expon <- floor(log10(abs(x)))
      return(expon)
    } else {
      if (is.complex(x)) {
        invisible(NaN)
        #warning("x may not be a complex number.")
        stop("x may not be a complex number.")
      } else {
        invisible(NaN)
        stop("x must be numeric.")
      }
    }
  } else {
    invisible(NaN)
    stop("The numeric vector x must be supplied. e.g. exponent(x = 5753).")
  }
}

#' @title Brown-Forsythe Test for Homogeneity of Variance using median
#' @name BrownForsytheTest
#' @author J. Fox, Chuck Powell
#'
#' @param formula A fully crossed anova formula.
#' @param data A datafram containing the data.
#' @return a table containing the results.
#'
#' @keywords internal
## moved from Rcmdr 13 July 2004
## levene.test.default function slightly modified and generalized from Brian Ripley via R-help
## the original generic version was contributed by Derek Ogle
## last modified 2019-02-01 by J. Fox 
## simplified and moved into package July 2020

BrownForsytheTest <- function(formula, data) {
  mf <- model.frame(formula, data)
  y <- mf[,1]
  group <- interaction(mf[,2:dim(mf)[2]])
  valid <- complete.cases(y, group)
  meds <- tapply(y[valid], group[valid], median)
  resp <- abs(y - meds[group])
  table <- anova(lm(resp ~ group))[, c(1, 4, 5)]
  rownames(table)[2] <- " "
  attr(table, "heading") <- paste("Brown-Forsythe Test for Homogeneity of Variance using median", sep="")
  table
}

#' @title Tidy Tables for htest objects
#' @name newbroom
#'
#' @description Produces tidy tibbles of results from htest objects.  
#' This is a vastly reduced version of the tidy.htest function from
#' package broom
#'
#' @usage newbroom(x)
#' @param x An `htest` object, such as those created by [stats::cor.test()],
#'   [stats::t.test()], [stats::wilcox.test()], [stats::chisq.test()], etc.
#' @return An object of class "tibble". 
#'
#' @examples
#'
#' chit <- chisq.test(xtabs(Freq ~ Sex + Class, data = as.data.frame(Titanic)))
#' CGPfunctions:::newbroom(chit)
#' @seealso [stats::t.test()], [stats::oneway.test()]
#'   [stats::wilcox.test()], [stats::chisq.test()]
#' @keywords internal
newbroom <- function(x) {
  
  ret <- x[c("estimate", "statistic", "p.value", "parameter")]
  
  # estimate may have multiple values
  if (length(ret$estimate) > 1) {
    names(ret$estimate) <- paste0("estimate", seq_along(ret$estimate))
    ret <- c(ret$estimate, ret)
    ret$estimate <- NULL
    
    # special case: in a t-test, estimate = estimate1 - estimate2
    if (x$method %in% c("Welch Two Sample t-test", " Two Sample t-test")) {
      ret <- c(estimate = ret$estimate1 - ret$estimate2, ret)
    }
  }
  
  
  # parameter may have multiple values as well, such as oneway.test
  if (length(x$parameter) > 1) {
    ret$parameter <- NULL
    if (is.null(names(x$parameter))) {
      warning("Multiple unnamed parameters in hypothesis test; dropping them")
    } else {
      message(
        "Multiple parameters; naming those columns ",
        paste(make.names(names(x$parameter)), collapse = ", ")
      )
      # rename num df to num.df and denom df to denom.df
      np <- names(x$parameter)
      np <- stringr::str_replace(np, "num df", "num.df")
      np <- stringr::str_replace(np, "denom df", "den.df")
      names(x$parameter) <- np
      ret <- append(ret, x$parameter, after = 1)
    }
  }
  
  ret <- purrr::compact(ret)
  if (!is.null(x$conf.int)) {
    ret <- c(ret, conf.low = x$conf.int[1], conf.high = x$conf.int[2])
  }
  if (!is.null(x$method)) {
    ret <- c(ret, method = as.character(x$method))
  }
  if (!is.null(x$alternative)) {
    ret <- c(ret, alternative = as.character(x$alternative))
  }
  
  as_tibble(ret)
}



