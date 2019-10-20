#' Information from dataframe
#'
#' This function takes in dataframe and outputs the column names, their types, the proportion of missing values and the five most common elements.
#' @export
#' @examples
#' datainf(WDI(),10)


datainf <- function(df= WDI(), num=5){
  library(tidyr)
  library(dplyr)
  library(WDI)

  # checking error
  if(!is.data.frame(df)) {
    stop("This is not a data frame. Please use this function in a dataframe!")
  }

  # column names
  print(colnames(df))

  #column type
  col_type <- lapply(df, class)
  print(col_type)

  #the most common elements in each column
  for (i in 1:length(df)){
    print(names(sort(summary(as.factor(df[,i]), decreasing=T)[1:num])))
  }
  #the proportion of missing values in each column
  missing_perc <- df %>%
    summarize_all(funs(sum(is.na(.)) / length(.)))
  return(missing_perc)

}
