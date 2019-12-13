#' Get artworks in certain departments
#'
#' This function allows you to get details of artworks in the departments you are interested in.
#'
#' @return Dataframe
#'
#' @import httr
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import jsonlite
#'
#' @param dep_list a list of names of departments that you want to know their ids. Defaults to c('American Decorative Arts','Arms and Armor','Greek and Roman Art').
#' @param metadataDate the function will return any objects with updated data after this date. Defaults to "2019-11-30".
#'
#' @examples
#' get_artworks_in_dep()
#' get_artworks_in_dep(metadataDate = "2019-12-9")
#' @export

get_artworks_in_dep <- function(dep_list = c('American Decorative Arts','Arms and Armor','Greek and Roman Art'), metadataDate = "2019-11-30"){
  ids <- get_dep_ids(dep_list)
  base_url <- "https://collectionapi.metmuseum.org/public/collection/v1/objects"
  if(metadataDate >= as.Date(str_c(format(Sys.Date(), "%Y-%m"), "-1"))){
    warning("The latest update date is the last day of last month! Please choose a date before that and re-enter.")
  } else{
    query_params <- list(
      metadataDate = metadataDate,
      departmentIds = ids
    )

    resp <- GET(base_url, query = query_params)

    if(http_error(resp)){
      warning("The request produced an error.")
    } else {
      object_html <- content(resp)
      objectIDs <- unlist(object_html$objectIDs)
      ID <- list()
      Department <- list()
      Title <- list()
      isHighlight <- list()
      Classification <- list()
      objectURL <- list()
      for(i in 1:content(resp)$total){
        url <- str_c(base_url, "/", objectIDs[i])
        object <- GET(url)
        Sys.sleep(0.1)
        if(http_error(object)){
          warning("The request produced an error.")
        }else{
          ID[i] <- objectIDs[i]
          Department[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$department
          Title[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$title
          isHighlight[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$isHighlight
          Classification[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$classification
          objectURL[i] <- fromJSON(content(object,"text"), simplifyVector = FALSE)$objectURL
        }
      }
      artworks_in_dep_df <- data.frame("ID"= unlist(ID), "Department" = unlist(Department), "Title" = unlist(Title), "Popular" = unlist(isHighlight), "Classification" = unlist(Classification),"Look_for_more_details" = unlist(objectURL))


      return(artworks_in_dep_df)

    }
  }
}


#' Compare the percentage of popular artworks
#'
#' This function should be used after calling the get_artworks_in_dep() function, and this function can calculate the percentage of popular artworks in those departments you choose in the last function.
#'
#' @return Dataframe
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#'
#' @param df The dataframe returned by the function get_artworks_in_dep(). Defaults to artworks_in_dep
#'
#' @examples
#' get_popular_perc()
#' @export



get_popular_perc <- function(df = artworks_in_dep){
  popular <- df %>%
    count(Department, Popular) %>%
    group_by(Department) %>%
    mutate(prop = n/sum(n)*100) %>%
    arrange(Department, prop)

  get_popular <- function(dep){
    dep[dep$Popular == TRUE, "Prop"]
    dep_brief <- dep %>%
      mutate(Popular_prop = dep[dep$Popular == TRUE, "Prop"]) %>%
      subset(select = -Prop ) %>%
      spread(Popular, n)

    brief <- dep_brief %>%
      mutate(Popular = dep_brief$"TRUE", Unpopular = dep_brief$"FALSE") %>%
      select(Department, Popular_prop, Popular, Unpopular)

    return (brief)
  }

  dep <- split(popular, popular$Department)
  popular_df <- data.frame(matrix(ncol = 4, nrow = 0))
  for(i in 1:length(dep)){
    dep_sub <- as.data.frame(dep[i])
    colnames(dep_sub) <- c("Department", "Popular", "n", "Prop")
    popular_dep <- get_popular(dep_sub)
    popular_df <- rbind(popular_df,popular_dep)
  }
  return(popular_df)
}


#' Get the popular artworks
#'
#' This function should be used after calling the get_artworks_in_dep() function, and this function can return the details of popular artworks in the departments you choose.
#'
#' @return Dataframe
#'
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#'
#' @param df The dataframe returned by the function get_artworks_in_dep(). Defaults to artworks_in_dep
#'
#' @examples
#' get_popular_items()
#' @export

get_popular_items <- function(df = artworks_in_dep){
  popular_items <- df %>%
    filter(Popular == TRUE) %>%
    select(Department, Title, Classification,"Look_for_more_details") %>%
    arrange(Department)
  return(popular_items)
}

