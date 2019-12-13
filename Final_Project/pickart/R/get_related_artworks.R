#' get a dataframe of realted artworks about certain artwork
#'
#' This function will crawl the related artworks about certain artwork, and users can specify the artwork by objectIDs.
#' @param objectIDs The id of the artwork. Default to 547802
#'
#' @keywords relatedartworks
#'
#' @import httr
#' @import rvest
#' @import stringr
#'
#' @examples
#' get_related_artworks(objectIDs = 547802)
#'
#' @export


get_related_artworks <- function(objectIDs = 547802){
  url <- "https://www.metmuseum.org/art/collection/search/"
  httr::set_config(httr::user_agent(url))
  endpoint <- str_c(url, objectIDs)
  end <- GET(endpoint)
  Sys.sleep(0.1)
  if(http_error(end)){
    warning("The request produced an error.")
  }else{
    end_html <- content(end)
  }
  #get the title of related artworks
  r_title_o <-html_nodes(end_html, css = ".card__title")
  r_title <- str_trim(html_text(r_title_o))

  #get the artist of related artworks
  r_artist_o <-html_nodes(end_html, css = ".card__meta-item.card__meta-artist")
  r_artist_t <- str_trim(html_text(r_artist_o))
  r_artist <- str_replace_all(r_artist_t, pattern = "\\r|\\n\\s+", replacement = "")
  r_artist <- str_replace(r_artist, pattern ="^Artist:", replacement = "")

  #get the date of related artworks
  r_date_o<-html_nodes(end_html, css = ".card__meta-item.card__meta-date")
  r_date_t <- str_trim(html_text(r_date_o))
  r_date<-str_replace_all(r_date_t, pattern = "\\r|\\n\\s+", replacement = "")
  r_date <- str_replace(r_date, pattern ="^Date:", replacement = "")

  #get the medium of related artworks
  r_medium_o<-html_nodes(end_html, css = ".card__meta-item.card__meta-medium")
  r_medium_t <- str_trim(html_text(r_medium_o))
  r_medium<-str_replace_all(r_medium_t, pattern = "\\r|\\n\\s+", replacement = "")
  r_medium <- str_replace(r_medium, pattern ="^Medium:", replacement = "")

  #get the accession of related artworks
  r_accession_o<-html_nodes(end_html, css = ".card__meta-item.card__meta-accession")
  r_accession_t <- str_trim(html_text(r_accession_o))
  r_accession<-str_replace_all(r_accession_t, pattern = "\\r|\\n\\s+", replacement = "")
  r_accession <- str_replace(r_accession, pattern ="^Accession:", replacement = "")

  #get the display status of related artworks
  r_gallery_o<-html_nodes(end_html, css = ".card__meta-item.card__meta-gallery")
  r_gallery_t <- str_trim(html_text(r_gallery_o))
  r_gallery<-str_replace_all(r_gallery_t, pattern = "\\r|\\n\\s+", replacement = "")
  r_gallery <- str_replace(r_gallery, pattern ="^On view in:", replacement = "")

  #get the links of related artworks
  rl <- html_nodes(end_html, css = ".gtm__relatedartwork")
  rlinks <-html_attr(rl,"href")
  rlinks <- unique(rlinks)
  r_links <- unlist(lapply(rlinks, function(x) str_c("https://www.metmuseum.org",x)))

  #get the picture link of related artworks
  r_pic<-html_nodes(end_html, xpath = "/html/body/div/div/div/div/div/figure/a/img/@data-src")
  r_picture <- html_text(r_pic)

  #make a dataframe about all the related artworks
  related_artworks <- data.frame(cbind("Title" = r_title, "Artist" = r_artist, "Date" = r_date, "Medium" = r_medium, "Accession" = r_accession, "Gallery" = r_gallery, "Link" = r_links, "Picture" = r_picture))

  return(related_artworks)
}
