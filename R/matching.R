
# define the graphql query as a static string that we param
graphql_match_query = "query NameMatch($searchString: String, $fallbackToGenus: Boolean)
{
  taxonNameMatch(
    inputString: $searchString
    checkHomonyms: false
    checkRank: false
    fallbackToGenus: $fallbackToGenus
  ) {
    inputString
    searchString
    match {
      id
      fullNameStringPlain
      role
    }
    candidates {
      id
      fullNameStringPlain
      role
    }
    error
    errorMessage
    method
    narrative
  }
}"


#' Match a single name string against the API
#'
#' @param search_string The string representation of the plant name from the data.
#' @param fallback_to_genus If an exact match is not found then fallback to matching a genus
#'
#' @return List containing data about the matched name or null
#' @export
#' @examples
#' match_name("Rhododendron ponticum")
match_name <- function(search_string = "", fallback_to_genus = FALSE, interactive = TRUE){

  response <- call_name_match_api(search_string = search_string, fallback_to_genus = FALSE)

  match <- response$data$taxonNameMatch$match;

  # we don't have a match and we are in interactive mode so give a choice
  if(is.null(match) && interactive){
    match <- pick_name_from_list(response$data$taxonNameMatch$candidates, search_string)
  }

  return(match)
}

pick_name_from_list <- function(candidates, search_string, offset = 0, page_size = 10){

  start_page <- offset + 1
  end_page <- start_page + page_size -1

  # don't over shoot the end of the list
  if(end_page > length(candidates)) end_page = length(candidates)
  if(start_page < 1) start_page = 1

  cat(sprintf("Matching string:\t%s\n", search_string ))

  for (i in start_page:end_page) {
    line <- sprintf("%i\t%s\t%s  [%s]\n",
                    i,
                    candidates[[i]]$id,
                    candidates[[i]]$fullNameStringPlain,
                    candidates[[i]]$role)
    cat(line)
  }

  prompt <- "Enter a number"

  if(end_page < length(candidates)) prompt <- paste(prompt, "M for more", sep = ", ")
  if(start_page > 1) prompt <- paste(prompt, "L for less", sep = ", ")
  prompt <- paste(prompt, "S to skip or a valid WFO ID or anything else to continue: ", sep = ", ")

  input = trimws(readline(prompt))
  input_number = suppressWarnings(as.numeric(input))
  if( !is.na(input_number) && input_number >= start_page && input_number <= end_page){
    index <- as.integer(input)
    return(candidates[[index]])
  }else{

    # paging
    if(tolower(input) == "m" && end_page < length(candidates)) return(pick_name_from_list(candidates, search_string, offset + page_size, page_size))
    if(tolower(input) == "l" && start_page > 1) return(pick_name_from_list(candidates, search_string, offset - page_size, page_size))

    # Are they skipping
    if(tolower(input) == "s") return("SKIP")

    # does it match a valid WFO ID?
    if(grepl("^wfo-[0-9]{10}$", input)) return(input)

    # fall through to giving up - check they want to
    user_input <- readline("Do you want to give up on matching? (y/n)  ")
    if(!tolower(user_input) == 'y'){
      # they don't want to quit
      return(pick_name_from_list(candidates, search_string, offset, page_size))
    }else{
      return(NULL) # get out of here with nothing - they have given up
    }
  }

}

#' Call the api for a name match
#'
#' @param search_string The taxon name to be searched on
#' @param fallback_to_genus True if a match at genus level will do
#'
#' @return List or String or Null
#'
#' @examples
#' match_name("Rhododendron ponticum L")
#' match_name("Rhododendron ponticum")
#' match_name("Rhododendron pontica")
call_name_match_api <- function(search_string = "", fallback_to_genus = FALSE){

  # create a request object
  req <- httr2::request(paste(unlist(options("wfo.api_uri")[1]),collapse=""))

  # prepare the body
  variables <- list(searchString = search_string, fallbackToGenus = fallback_to_genus)
  payload <- list(query = graphql_match_query, variables = variables)


  # set the body
  req <- httr2::req_body_json(req, data = payload, auto_unbox = TRUE)

  # actually run the requst
  resp <- httr2::req_perform(req)
  #resp_content_type(resp)

  # return the whole thing as a list of lists
  httr2::resp_body_json(resp)

}
