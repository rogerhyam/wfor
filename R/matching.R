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
      wfoPath
    }
    candidates {
      id
      fullNameStringPlain
      role
      wfoPath
    }
    error
    errorMessage
    method
    narrative
  }
}"


#' Match names in a data frame
#'
#' @param df The source data.frame. The script will return a copy of this with changes made.
#' @param name_col A string. REQUIRED. The name of the column in the data frame that contains the plant name to be matched.
#'     If the column values do not include the author strings for the plant names then a authors_col should be
#'     specified.
#' @param authors_col A string. The name of the column containing the authors for the plant name, if they
#'     are not included in the name_col column.
#' @param fallback_to_genus A boolean.
#'     If TRUE and no unambiguous match is found for the an attempt will be made to match the first word
#'     of the name string to a genus.
#'     If FALSE (the default) the whole string has to find an unambiguous match.
#' @param interactive A boolean.
#'     If TRUE (the default) then script will stop and present a pick list if a precise match can't be found.
#'     If FALSE then script will just continue to the next row if no unambiguous match is found.
#'
#' @return A data.frame with the wfo_* columns populated.
#' @export
#'
#' @examples
#' # auto match the Belgian Magnoliopsida test data
#' `mags_example <- wfo_match_df_names(Belgian_Magnoliopsida_sp_ssp_var_2011, name_col="scientific_name", authors_col="authorship", interactive=FALSE)`
#' # check how many  have been matched using the [wfo_stat_matches()] function.
#' `wfo_stat_matches(mags_example)`
#' # do a second pass interactively to resolve unmatched names (or until you get tired)
#' `mags_example <- wfo_match_df_names(mags_example, name_col="scientific_name", authors_col="authorship", interactive=TRUE)`
#' # do a third pass matching names to the nearest genus
#' `mags_example <- wfo_match_df_names(mags_example, name_col="scientific_name", authors_col= "authorship", fallback_to_genus=TRUE)`
#'
wfo_match_df_names <- function(df, name_col, authors_col = NULL, fallback_to_genus = FALSE, interactive = TRUE){

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
    df$wfo_id <- NA
    cat("Added wfo_id column to the dataframe.\n")
  }
  if(!"wfo_name" %in% colnames(df)){
    df$wfo_name <- NA
    cat("Added wfo_name column to the dataframe. This will contain the full name as in the WFO Plant List\n")
  }
  if(!"wfo_path" %in% colnames(df)){
    df$wfo_path <- NA
    cat("Added wfo_path column to the dataframe. This will contain the current status of the name in the WFO Plant List as a sanity check.\n")
  }
  if(!"wfo_method" %in% colnames(df)){
    df$wfo_method <- NA
    cat("Added wfo_method column to the dataframe. This will contain an indication of how the name was matched.\n")
  }

  # Work through the df and fill in the values
  row_count = nrow(df)
  for (i in 1:nrow(df)) {

    # if we have already done this row then we just go to the next one
    # if(grepl("^wfo-[0-9]{10}$", df[i, "wfo_id"]) || df[i, "wfo_id"] == "SKIP" ) next
    if(!is.na(df[i, "wfo_id"])) next

    # get the name string to look up
    name_string <- df[i, name_col]

    # if a separate authors column is specified
    # concatenate that onto it
    if(!is.null(authors_col)){
      name_string <- paste(name_string, df[i, authors_col])
    }

    cat(sprintf("%i of %i\t%s\t", i, row_count, stringr::str_pad(name_string, 60, "right") ))

    # Do we have the results cached already?
    if (name_string %in% names(the$wfo_name_cache) ){
      n <- the$wfo_name_cache[name_string]
      df[i, "wfo_id"] <- n[[1]]$id
      df[i, "wfo_name"] <- n[[1]]$fullNameStringPlain
      df[i, "wfo_path"] <- n[[1]]$wfoPath
      df[i, "wfo_method"] <- n[[1]]$method
      # FIXME other fields
      cat(sprintf("%s\t from CACHE\n", n[[1]]$id ))
      next
    }

    # actually do the matching
    resp <- wfo_match_name(name_string, fallback_to_genus, interactive)

    # Did we find nothing?
    if(is.null(resp)){
      if(interactive){
        cat("STOPPED\n")
        break # in interactive mode this is a signal to stop
      }else{
        cat("No match. Continuing.\n")
        next # in non-interactive mode we just carry on to the next one
      }
    }

    # Have we been asked to skip?
    if(is.character(resp) && resp == "SKIP"){
      df[i, "wfo_id"] <- "SKIP"
      cat("SKIPPED\n")
      next
    }

    # FIXME they may have returned a wfo id entered manually

    # Did it return a matched name?
    if(is.list(resp)){
      df[i, "wfo_id"] <- resp$id
      df[i, "wfo_name"] <- resp$fullNameStringPlain
      df[i, "wfo_path"] <- resp$wfoPath
      df[i, "wfo_method"] <- resp$method
      # put it in the cache so we don't look it up again.
      the$wfo_name_cache[name_string] <- list(resp)
      cat(sprintf("%s\t %s\n", resp$id, resp$method ))
    }

  } # through the rows in the df

  report_cache_status()

  return(df)

}

#' Report on the name cache status
#' recommending that it is saved
#'
#' @return NULL
#' @noRd
#' @examples
#' `report_cache_status()`
report_cache_status <- function(){
  if(length(the$wfo_name_cache) > 100 ){
    cat("\n--- Name Cache Status ---")
    cat(sprintf("\nThe cache contains %s name strings.", format(length(the$wfo_name_cache), big.mark=",")  ))
    cat("\nConsider saving a copy of the cache so you can use it in the next session.")
    cat("\nThis will preserve all the mapping decisions you have made, reduce load on")
    cat("\nthe server and be quicker to run future matches.")
    cat("\n\t> my_name_cache <- wfo_get_name_cache()")
    cat("\n\t... in another session ...")
    cat("\n\t> wfo_set_name_cache(my_name_cache)")
    cat("\n")
  }
}

#' Match a single name string against the API
#'
#' @param search_string The string representation of the plant name from the data.
#' @param fallback_to_genus If an exact match is not found then fallback to matching a genus
#' @param interactive A boolean.
#'     If TRUE (the default) then script will stop and present a pick list if a precise match can't be found.
#'     If FALSE then script will just continue to the next row if no unambiguous match is found.
#'
#' @return List containing data about the matched name or null
#' @export
#' @examples
#' `wfo_match_name("Rhododendron ponticum")`
wfo_match_name <- function(search_string = NA, fallback_to_genus = FALSE, interactive = TRUE){

  response <- call_name_match_api(search_string = search_string, fallback_to_genus = fallback_to_genus)

  match <- response$data$taxonNameMatch$match;

  # we don't have a match and we are in interactive mode so give a choice
  if(is.null(match)){
    if(interactive) match <- pick_name_from_list(response$data$taxonNameMatch$candidates, search_string)
  }else{
    if(fallback_to_genus){
      match$method <- "AUTO GENUS"
    }else{
      match$method <- "AUTO"
    }
  }

  return(match)
}

# handles display and selection from a picklist
pick_name_from_list <- function(candidates, search_string, offset = 0, page_size = 10){

  start_page <- offset + 1
  end_page <- start_page + page_size -1

  # don't over shoot the end of the list
  if(end_page > length(candidates)) end_page = length(candidates)
  if(start_page < 1) start_page = 1

  cat("\n\n--- Pick a name ---")
  cat(sprintf("\nMatching string:\t%s\n", search_string ))

  for (i in start_page:end_page) {
    line <- sprintf("%s%s\t%s\t%s\t\t%s\n",
                    stringr::str_pad(as.character(i), 4, "right"),
                    candidates[[i]]$id,
                    candidates[[i]]$fullNameStringPlain,
                    candidates[[i]]$role,
                    candidates[[i]]$wfoPath
                    )
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
    match <- candidates[[index]]
    match$method <- "MANUAL"
    return(match)
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
#' @noRd
#' @examples
#' `call_name_match_api("Rhododendron ponticum L")`
call_name_match_api <- function(search_string = NA, fallback_to_genus = FALSE){

  # create a request object
  req <- httr2::request(paste(unlist(options("wfo.api_uri")[1]),collapse=""))

  # prepare the body
  variables <- list(searchString = search_string, fallbackToGenus = fallback_to_genus)
  payload <- list(query = graphql_match_query, variables = variables)


  # set the body
  req <- httr2::req_body_json(req, data = payload, auto_unbox = TRUE)

  # actually run the request
  resp <- httr2::req_perform(req)
  #resp_content_type(resp)

  # return the whole thing as a list of lists
  httr2::resp_body_json(resp)

}

#' Clear skipped rows
#'
#' @param df The data frame for the skips to be cleared from
#'
#' @return The updated data frame
#' @export
#'
wfo_clear_skips <- function(df){
  df$wfo_id[df$wfo_id == "SKIP"] <- NA
  return(df)
}

#' Returns the name cache created in this session.
#' Useful to prevent repeatedly resolving issues and
#' looking up the same name string in the WFO API
#' thus increasing speed and consistency.
#' Use with [wfo_set_name_cache()]
#'
#' @return List of name objects
#' @export
#'
#' @examples
#' `my_name_cache <- wfo_get_name_cache()`
#' # ... in another session ...
#' `wfo_set_name_cache(my_name_cache)`
wfo_get_name_cache <- function(){
  return(the$wfo_name_cache)
}

#' Set the name cache to be used in this session.
#' Useful to prevent repeatedly resolving issues and
#' looking up the same name string in the WFO API
#' thus increasing speed and consistency.
#' Use with [wfo_set_name_cache()]
#'
#' @param name_cache The new name cache
#' @return NULL
#' @export
#'
#' @examples
#' `my_name_cache <- wfo_get_name_cache()`
#' # ... in another session ...
#' `wfo_set_name_cache(my_name_cache)`
wfo_set_name_cache <- function(name_cache){
  the$wfo_name_cache <- name_cache
  return(invisible(NULL))
}
