#' Find artworks realted to one theme or topic
#'
#' This function allows users give a certain theme or topic for searching, and will return the details of all artworks related to the search.
#' @param search The theme or topic you want to search for. Default to Realism
#' @param famous If the artworks are famous. Default to TRUE
#' @param onview If the artworks are on view in the museum. Default to TRUE
#' @return Dataframe
#' @keywords search
#'
#' @import httr
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import jsonlite
#'
#' @examples
#' get_search_results(search = "Realism", famous = TRUE, onview = TRUE)
#' get_search_results(search = "Impressionism")
#' @export


get_search_results <- function(search = "Realism", famous = TRUE, onview = TRUE) {

  base_url <- "https://collectionapi.metmuseum.org/public/collection/v1"
  endpoint <- "https://collectionapi.metmuseum.org/public/collection/v1/search"

  query_params <- list(
    q = search,
    isHighlight = famous,
    isOnView = onview
  )
  resp <- GET(endpoint, query = query_params)
  cat("The status of request is ",resp$status_code, "\n")
  cat("There are ",content(resp)$total, " works about ", search, " in the Metropolitan Museum.")


  objectIDs <- unlist(content(resp)$objectIDs)

  ID <- list()
  Department <- list()
  Title <- list()
  Medium <- list()
  Artist <- list()
  Nationality <- list()
  Year <- list()
  Link <- list()
  objectURL <- list()

  for(i in 1:content(resp)$total){
    url <- str_c(base_url, "/objects/", objectIDs[i])
    object <- GET(url)

    if(http_error(object)){
      warning("The request produced an error.")
    }
    ID[i] <- objectIDs[i]
    Department[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$department
    Title[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$title
    Medium[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$medium
    Artist[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$artistDisplayName
    Nationality[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$artistNationality
    Year[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$objectDate
    Link[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$primaryImage
    objectURL[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$objectURL
  }

  df <- data.frame("ID" = unlist(ID),"Artist" = unlist(Artist), "Nationality" = unlist(Nationality), "Year" = unlist(Year), "Title" =  unlist(Title), "Medium" = unlist(Medium),"Department" = unlist(Department),"Link" = unlist(Link), "Look_for_more_details" = unlist(objectURL))

  return(df)
}
