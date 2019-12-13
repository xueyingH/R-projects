#' download map of certain artwork
#'
#' This function allows users to download the maps of artworks by inputting the objectIDs of the artwork.
#' @param objectIDs The id of the artwork. Default to 547802
#' @param path the function will create a file folder with the name of path to store the map. Default to "maps/", so it will create a file folder called maps and store files.
#'
#' @keywords map
#'
#' @import httr
#' @import rvest
#' @import stringr
#' @import webshot
#'
#' @examples
#' get_map(objectIDs = 547802)
#' get_map(objectIDs = 14019)
#'
#' @export


get_map <- function(objectIDs = 547802, path = "maps/"){
  url <- "https://www.metmuseum.org/art/collection/search/"
  httr::set_config(httr::user_agent(url))
  endpoint <- str_c(url, objectIDs)
  end <- GET(endpoint)
  Sys.sleep(0.1)
  if(http_error(end)){
    warning("The request produced an error.")
  }else{
    end_html <- content(end)
    #get the location of the artwork in the museum
    location_element <- html_nodes(end_html, css=".artwork__location--gallery")
    location <- html_text(location_element)

    if (length(location)==0) {

      return('Not on view')

    } else {
      map_element <-html_nodes(end_html, xpath = "//div/div/div/div/p/span/a/@href")
      map <- html_text(map_element)
      #download the map

      location_url <- map
      webshot(location_url, paste0(path, objectIDs,".png"), vwidth = 1488, vheight = 1116, delay = 5)
      return(location)
    }
  }

}
