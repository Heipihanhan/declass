
#' List collections.
#'
#' This functions lists all collections that're avaialble in the Declassification Engine API.
#'
#' @param return_type The format of content displayed, in list or raw.
#' @return The name of collections.
#' @examples
#' declass_collection("list")
#' declass_collection("string")
#' @export
declass_collection <- function(return_type = "string"){
  url <- "http://api.declassification-engine.org/declass/v0.4/collections"
  parsed_json <- jsonlite::fromJSON(url)
  collections <- parsed_json$results$collections
  if (return_type == "string") {
    return(collections)
  } else if (return_type == "list") {
    raw <- httr::content(httr::GET(url))
    return(raw)
  } else {
    notice <- "Return_type not supported"
    return(notice)
  }
}
