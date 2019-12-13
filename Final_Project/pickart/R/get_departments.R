#' Get departments ids and names
#'
#' This function allows you to get the ids and names of departments in the MET museum.
#'
#' @return Dataframe
#' @keywords department
#'
#' @import httr
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import jsonlite
#'
#' @examples
#' get_departments()
#'
#' @export


get_departments <- function() {
  url <- "https://collectionapi.metmuseum.org/public/collection/v1/departments"
  resp <- GET(url)
  Sys.sleep(0.1)
  if(http_error(resp)){
    warning("The request produced an error.")
  }else{
    resp_html <- content(resp)
    df <- fromJSON(content(resp,"text"), simplifyDataFrame = TRUE)
    departments <- as.data.frame(df)
    rownames(departments) <- c()
    colnames(departments) <- c("id", "department")
    return(departments)
  }
}


#' Get departments ids
#'
#' This function will return department ids separated by "|" when users input a list of department names
#'
#'
#' @import httr
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import jsonlite
#' @param dep_list a list of names of departments that you want to know their ids.Defaults to c('American Decorative Arts','Arms and Armor','Greek and Roman Art').
#' @examples
#' get_dep_ids()
#'
#' @export

get_dep_ids <- function(dep_list = c('American Decorative Arts','Arms and Armor','Greek and Roman Art')){
  get_id <- function(dep_name){
    departments[departments$department == dep_name, 'id']
  }
  id_list <- list()
  for( i in 1:length(dep_list)){
    id_list[i] <- get_id(dep_list[i])
  }
  ids <- str_c(id_list, collapse = "|")
  return(ids)
}
