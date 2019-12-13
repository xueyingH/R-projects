#' Find represent departments
#'
#' This function should be used after get_search_results(), it can help users to analyze the search results can return the departments which have the most artworks of the theme.
#' @param df The dataframe returned by the get_search_results() function. Default to realism
#' @return Dataframe and plot
#'
#' @import httr
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import jsonlite
#' @import ggplot2
#'
#' @examples
#' find_represent_dep()
#' @export

find_represent_dep <- function(df = realism){
  # find the search results mainly distributed in which departments
  mostcommon <- df %>%
    group_by(Department) %>%
    count() %>%
    arrange(desc(n))
  # visualize the main departments
  mostcommon_plot <- ggplot(head(mostcommon), aes(x=Department, y=n, color = Department)) + geom_col() +labs(
    title = "Most common in which department?",
    y = "Number"
  )+ theme(plot.title = element_text(hjust = 0.5),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank())
  print(mostcommon)
  return(mostcommon_plot)
}


#' Find represent artists
#'
#' This function should be used after get_search_results(), it can help users to analyze the search results can return the artists who create the most artworks of the theme.
#' @param df The dataframe returned by the get_search_results() function. Default to realism
#' @return Dataframe
#'
#' @import httr
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import tidyverse
#' @import jsonlite
#' @importFrom utils head
#' @examples
#' find_represent_artists()
#' @export

find_represent_artists <- function(df = realism){
  represent_artists <- df %>%
    subset(!Artist == "") %>%
    group_by(Artist) %>%
    count() %>%
    arrange(desc(n))
  represent_artists_top5 <- head(represent_artists,5)
  return(represent_artists_top5)
}
