#' Print column classes
#'
#' Prints the class of each column; just saves a small amount of typing
#' Works with dataframes and data tables
#'
#'
#' @param df A data frame or data table
#' @import data.table
#' @export
#' @examples
#' abc <- as.data.frame(a = c(1, 2, 3, 4, 5),
#'                      b = c("a", "b", "c", "d", "e")
#'                     )
#' cc(abc)

cc <- function(df){
    print(sapply(df, class))
}
