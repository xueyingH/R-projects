#' download details of artworks to local files
#'
#' This function should be used after the get_search_results() function. Users can specify how many artworks they want to download, and the function can download the images, maps and realted artworks.
#' @param df The dataframe returned by the get_search_results() function. Default to realism
#' @param n The number of artworks you want to download. Default to 10
#'
#' @keywords download
#'
#' @import httr
#' @import rvest
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import jsonlite
#' @import webshot
#' @import utils
#' @importFrom utils download.file
#'
#' @examples
#' download_details(n = 120)
#' @export


download_details <- function(df = realism, n = 10){
  if(n > nrow(df)){
    warning("The number of artworks you chose to download exceeds the limit. Please choose a smaller number!")
  }else{
    for(i in 1:n){
      dir.create(toString(df[i,"ID"]))
    }
    for(i in 1:n){
      dir.create(paste0(toString(df[i,"ID"]),"/","images"))
      dir.create(paste0(toString(df[i,"ID"]),"/","relatedartworks"))
    }
    for(i in 1:n){
      download.file(toString(df[i,"Link"]), destfile = paste0(toString(df[i,"ID"]),"/images/",basename(toString(df[i,"Link"]))), method="curl", extra="-k")
      get_map(objectIDs = df[i,"ID"], path = paste0(toString(df[i,"ID"]),"/maps/"))
      relatedartworks_df <- get_related_artworks(objectIDs = df[i,"ID"])
      print(relatedartworks_df)
      saveRDS(relatedartworks_df, file = paste0(toString(df[i,"ID"]),"/","relatedartworks/relatedartworks_df.RDS"))
    }
  }
}
