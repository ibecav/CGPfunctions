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


### -----  internal function to format p values =====================

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

