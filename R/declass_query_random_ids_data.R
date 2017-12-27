#' Query documents based on randomly sampled IDs.
#'
#' This function queries documents by randomly sampled IDs.
#'
#' @param limit The number of randomply sampled IDs.
#' @return Content of the documents
#' @examples
#' declass_query_random_ids_data(3)
#' @export
declass_query_random_ids_data <- function(limit) {
  ids <- declass_query_random_ids(limit)
  id_list <- paste(ids$id, collapse = ",")
  id_list <- utils::URLencode(id_list)
  return (declass_query_id_data(id_list))
}
