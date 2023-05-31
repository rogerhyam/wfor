#' Match Names
#'
#' @param searchTerms The search terms to be looked up.
#'
#' @return NULL
#' @export
#'
#' @examples
#' match("Rhododendron ponticum")
match <- function(searchTerms){
  c("results", searchTerms)
}
