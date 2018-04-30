#' This function imports/loads packages as in 'Python', i.e., ``import package as alias''
#'
#' @param package Package name (unquoted).
#' @param alias Alias (unquoted) for the package.
#' @examples
#' import(dplyr, d)
#' df <- data.frame(a=1:3, b=4:6)
#' df %>% d$filter(a == 2)
import_pack <- function(package, alias) {
  if (missing(package) | missing(alias))
    stop("Both arguments must be passed.", call. = FALSE)
  package. <- as.character(substitute(package))
  alias. <- as.character(substitute(alias))
  base::suppressPackageStartupMessages(library(package., character.only=TRUE))
  base::assign(alias., loadNamespace(package.), inherits = TRUE)
}

#' This function imports/loads functions as in 'Python', i.e., ``from package import function as alias''
#'
#' @param package Package name (unquoted).
#' @param fun Function name (unquoted).
#' @param alias Alias (unquoted) for the function.
#' @examples
#' import_fun(dplyr, filter, fil)
#' df <- data.frame(a=1:3, b=4:6)
#' fil(df, a == 2)
import_fun <- function(package, fun, alias) {
  if(missing(package) | missing(fun) | missing(alias))
    stop("All three arguments must be passed.", call. = FALSE)
  alias. <- as.character(substitute(alias))
  fun. <- as.character(substitute(fun))
  package. <-  as.character(substitute(package))
  fun.p <- utils::getFromNamespace(fun., package.)
  base::assign(alias., fun.p, inherits = TRUE)
}