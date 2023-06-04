# This file contains functions for reporting
# on data frames that have been matched to WFO ID


#' Statistics on WFO matches within a data frame.
#' A utility function to see what is going on.
#'
#' @param df A data.frame containing matches in wfo_* columns.
#'
#' @return NULL
#' @export
#'
#' @examples
#' `wfo_stat_matches(df)`
wfo_stat_matches <- function(df){

  row_count <- nrow(df)

  # wfo_id column
  cat("\n-- MATCHES --")
  if("wfo_id" %in% colnames(df)){

    matched <- length(which(grepl("^wfo-[0-9]{10}$", df$wfo_id)))
    matched_pretty <-  format(matched, big.mark=",")
    matched_percent <- format( (matched/row_count)*100, digits=2 )
    line <- sprintf("\nMatched:\t%s\t%s %%", matched_pretty, matched_percent)
    cat(line)

    skipped <- length(which(df$wfo_id == "SKIP"))
    skipped_pretty <-  format(skipped, big.mark=",")
    skipped_percent <- format( (skipped/row_count)*100, digits=2 )
    line <- sprintf("\nSkipped:\t%s\t%s %%", skipped_pretty, skipped_percent)
    cat(line)

    blank <- length(which(is.na(df$wfo_id)))
    blank_pretty <-  format(blank, big.mark=",")
    blank_percent <- format( (blank/row_count)*100, digits=2 )
    line <- sprintf("\nBlank:\t\t%s\t%s %%", blank_pretty, blank_percent)
    cat(line)

  }else{
    cat("\nError: wfo_id column missing")
  }

  cat("\n-- METHODS --")
  if("wfo_method" %in% colnames(df)){
    t <- table(df$wfo_method)

    for (i in 1:length(names(t))) {
      val <- names(t)[[i]]
      c <- t[[val]]
      c_pretty <- format(c, big.mark=",")
      line <- sprintf("\n%s\t%s", val, c_pretty)
      cat(line)
    }

  }else{
    cat("\nError: wfo_method column missing")
  }

}
