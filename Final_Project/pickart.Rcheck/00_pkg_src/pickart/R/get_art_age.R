#' Find artworks in certain age
#'
#' This function allows you to search a type of artworks during a period. You can specify the type of artworks, the start and the end of the period.
#' @param Begin The start of the period. Defaults to 1600
#' @param End The end of the period. Defaults to 1900
#' @param class The type of artworks you want to choose. Defaults to Paintings
#' @return Dataframe
#' @keywords age
#' @export
#' @examples
#' get_art_age()
#' get_art_age(Begin = 1620, End = 1650, class = "Sculpture")

get_art_age <- function(Begin = 1600, End = 1900, class = "Paintings"){
  base_url <- "https://collectionapi.metmuseum.org/public/collection/v1"
  endpoint <- "https://collectionapi.metmuseum.org/public/collection/v1/search"

  query_params <- list(
    q ="",
    dateBegin = Begin,
    dateEnd = End
  )
  resp <- GET(endpoint, query = query_params)
  cat("The status of request is ",resp$status_code, "\n")

  #library(jsonlite)
  objectIDs <- unlist(content(resp)$objectIDs)

  Title <- list()
  ObjectDate <- list()
  Artist <- list()
  Nationality <- list()
  Birthdate <- list()
  Deathdate <- list()
  Classification <- list()

  for(i in 1:content(resp)$total){
    url <- str_c(base_url, "/objects/", objectIDs[i])
    object <- GET(url)

    if(http_error(object)){
      warning("The request produced an error.")
    }

    Title[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$title
    ObjectDate[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$objectDate
    Artist[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$artistDisplayName
    Nationality[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$artistNationality
    Birthdate[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$artistBeginDate
    Deathdate[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$artistEndDate
    Classification[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$classification

  }
  age_df <- data.frame( "Title" = unlist(Title),"ObjectDate" = unlist(ObjectDate), "Artist" = unlist(Artist), "Nationality" = unlist(Nationality), "Birthdate" = unlist(Birthdate),"Deathdate" = unlist(Deathdate),"Classification" = unlist(Classification))

  class <- age_df %>%
    filter(Classification == class)

  if(nrow(class)==0){
    warning("There is no artworks in this classification during this period")
  }else{
    age_pattern <- "[0-9]+"
    age <- str_subset(class$ObjectDate, pattern = age_pattern)
    age_round <- str_extract(class$ObjectDate, pattern = age_pattern)
    country_pattern <- "Netherlandish"
    country <- str_subset(class$Nationality, pattern = country_pattern)
    country_uniform <- str_replace_all(class$Nationality, pattern = country_pattern, replacement = "Dutch" )
    class$ObjectDate <- as.numeric(age_round)
    class$Nationality <- country_uniform
    return(class)
  }
}
