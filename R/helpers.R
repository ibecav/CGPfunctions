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
    mutate(
      sensible = case_when(
        bf < .000001 ~">=1,000,000",
        bf < .001 & bf >= .000001 ~ ">=1,000",
        bf < .01 & bf >= .001 ~ ">=100",
        bf < 1 & bf >= .01 ~ paste0("=", round(1 / bf, k)),
        bf >= 1 & bf < 100 ~ paste0("=", round(bf, k)),
        bf >= 100 & bf < 1000 ~ ">=100",
        bf >= 1000 & bf < 1000000 ~ ">=1,000",
        bf >= 1000000 ~ ">=1,000,000"
      )) %>%
    mutate(astext = case_when(
      bf < 1 ~ paste0("=", round((1 / bf), k)),
      bf >= 1 ~ paste0("=", round(bf, k))
    ))
  
  #  return(results)
  
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



