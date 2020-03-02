#' Cross two vectors of variable names from a dataframe
#'
#' @param data the dataframe or tibble the variables are contained in.
#' @param x,y These are either character or integer vectors containing
#'   the names, e.g. "am" or the column numbers e.g. 9
#' @param verbose the default is FALSE, setting to TRUE will cat additional
#'   output to the screen
#'
#' @return a list with two sublists `lista` and `listb`. Very handy for
#'   feeding the lists to `purrr` for further processing.
#' @author Chuck Powell
#' @export
#'
#' @examples
#' cross2_var_vectors(mtcars, 9, c(2, 10:11))
#' cross2_var_vectors(mtcars, "am", c("cyl", "gear", "carb"))
#' x2 <- c("am", "carb")
#' y2 <- c("vs", "cyl", "gear")
#' cross2_var_vectors(mtcars, x2, y2, verbose = TRUE)
#' 
#' \dontrun{
#' variables_list <- cross2_var_vectors(mtcars, x2, y2)
#' mytitles <- stringr::str_c(
#'   stringr::str_to_title(variables_list$listb),
#'   " by ",
#'   stringr::str_to_title(variables_list$lista),
#'   " in mtcars data"
#'   )
#' purrr::pmap(
#' .l = list(
#'    x = variables_list[[1]], # variables_list$lista
#'    y = variables_list[[2]], # variables_list$listb
#'    title = mytitles
#' ),
#' .f = CGPfunctions::PlotXTabs2,
#' data = mtcars,
#' ylab = NULL,
#' perc.k = 1,
#' palette = "Set2"
#' )
#' 
#' }
#' 
cross2_var_vectors <- function(data,
                               x,
                               y,
                               verbose = FALSE) {
  # some quick sanity checks
  if (!is.data.frame(data)) {
    stop("data must be a dataframe or tibble")
  }

  if (!is.character(y) && !is.numeric(y)) {
    stop("x and y need to be either character or numeric vectors")
  }

  if (!is.character(x) && !is.numeric(x)) {
    stop("x and y need to be either character or numeric vectors")
  }
########
  if (is.numeric(x) && (max(x) > ncol(data) || min(x) < 1)) {
    stop("x contains one or more items that are not a valid column number")
  }

  if (is.numeric(y) && (max(y) > ncol(data) || min(y) < 1)) {
    stop("y contains one or more items that are not a valid column number")
  }
########
  if (is.character(x) && !all(x %in% colnames(data))) {
    stop("x contains one or more items that are not columns in data")
  }

  if (is.character(y) && !all(y %in% colnames(data))) {
    stop("y contains one or more items that are not columns in data")
  }

  if (any(colnames(data[x]) %in% colnames(data[y]))) {
    stop("x and y cannot contain a common variable")
  }

  # initialize two empty lists of the proper length x * y
  listb <- lista <- vector(
    mode = "list",
    length = (length(x) * length(y))
  )

  # do the work by looping through both vectors
  current_combo <- 1

  for (j in seq_along(x)) {
    for (k in seq_along(y)) {
      listb[[current_combo]] <- colnames(data[x[[j]]])
      lista[[current_combo]] <- colnames(data[y[[k]]])
      if (verbose) {
        cat("Pair ",
          current_combo,
          " lista = ",
          lista[[current_combo]],
          " listb = ",
          listb[[current_combo]], "\n",
          sep = ""
        )
      }
      current_combo <- current_combo + 1
    }
  }

  # return a list with two sublists
  return(list(lista = lista, listb = listb))
}
