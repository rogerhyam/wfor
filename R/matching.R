
# define the graphql query as a static string that we param
graphql_match_query = "query NameMatch($searchString: String){
  taxonNameSuggestion(termsString: $searchString){
    id
    fullNameStringPlain
    authorsString
    citationMicro
    currentPreferredUsage{
      id
      classificationId
      hasName{
        id
        fullNameStringPlain
      }
    }
  }
}"


#' Match a single name string against the API
#'
#' @param search_string The string representation of the plant name from the data.
#' @param fallback_to_genus If an exact match is not found then fallback to matching a genus
#'
#' @return List of lists containing matching results from API
#' @export
#'
#' @examples
#' match_name("Rhododendron ponticum")
match_name <- function(search_string = "", fallback_to_genus = FALSE){

  # create a request object
  req <- httr2::request(paste(unlist(options("wfo.api_uri")[1]),collapse=""))

  # prepare the body
  variables <- list(searchString = search_string)
  payload <- list(query = graphql_match_query, variables = variables)

  # set the body
  req <- httr2::req_body_json(req, data = payload, auto_unbox = TRUE)

  # actually run the requst
  resp <- httr2::req_perform(req)
  #resp_content_type(resp)

  # return the whole thing as a dataframe
  httr2::resp_body_json(resp)

}
