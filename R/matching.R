# we create an environment for the package so that we can cache
# variables for at least the duration of the session
the <- new.env(parent = emptyenv())

# the name cache keeps a mapping between name strings and matched name
# objects so we don't need to call the API for strings we have resolved
# already in this session
the$wfo_name_cache <- list()

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


#' Match names in a data frame
#'
#' @param df
#' @param fallback_to_genus
#' @param interactive
#'
#' @return
#' @export
#'
#' @examples
match_df_names <- function(df, name_col, authors_col = NULL, fallback_to_genus = FALSE, interactive = TRUE){

  # check the name column exists
  if(!name_col %in% colnames(df)){
    cat(sprintf("There is no column called '%s' in the data frame.", name_col ))
    return(NULL)
  }

  # check the authors exist if they supply one
  if(!is.null(authors_col) && !authors_col %in% colnames(df)){
    cat(sprintf("There is no column called '%s' in the data frame.", authors_col ))
    return(NULL)
  }

  # Check if the df has the wfo_ columns in it or not
  if(!"wfo_id" %in% colnames(df)){
    df$wfo_id <- ""
    cat("Added wfo_id column to the dataframe.\n")
  }
  if(!"wfo_name" %in% colnames(df)){
    df$wfo_name <- ""
    cat("Added wfo_name column to the dataframe. This will contain the full name as in the WFO Plant List\n")
  }
  if(!"wfo_path" %in% colnames(df)){
    df$wfo_path <- ""
    cat("Added wfo_path column to the dataframe. This will contain the current status of the name in the WFO Plant List as a sanity check.\n")
  }
  if(!"wfo_method" %in% colnames(df)){
    df$wfo_method <- ""
    cat("Added wfo_method column to the dataframe. This will contain an indication of how the name was matched.\n")
  }

  # Work through the df and fill in the values

  for (i in 1:nrow(df)) {

    # if we have already done this row then we just go to the next one
    if(grepl("^wfo-[0-9]{10}$", df[i, "wfo_id"]) || df[i, "wfo_id"] == "SKIP" ) next

    # get the name string to look up
    name_string <- df[i, name_col]

    # if a separate authors column is specified
    # concatenate that onto it
    if(!is.null(authors_col)){
      name_string <- paste(name_string, df[i, authors_col])
    }

    # Do we have the results cached already?
    if (name_string %in% names(the$wfo_name_cache) ){
      n <- the$wfo_name_cache[name_string]
      df[i, "wfo_id"] <- n$id
      # FIXME other fields
      next
    }

    # actually do the matching
    resp <- match_name(name_string, fallback_to_genus, interactive)

    # Did we find nothing? Just carry on
    if(is.null(resp)) break

    # Have we been asked to skip?
    if(is.character(resp) && resp == "SKIP"){
      df[i, "wfo_id"] <- "SKIP"
      next
    }

    # Did it return a matched name?
    if(is.list(resp)){
      df[i, "wfo_id"] <- resp$id
      # FIXME other fields
      # put it in the cache so we don't look it up again.
      the$wfo_name_cache[name_string] <- resp
    }

  } # through the rows in the df

  return(df)

}

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
