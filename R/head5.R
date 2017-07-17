#' Print first 5 rows and first 5 columns
#'
#' A slightly modified head function for large datasets with many columns.
#' Works with dataframes and data tables
#'
#'
#' @param df A data frame or data table
#' @param nc Number of columns to print, default 5
#' @param nr Number of rows to print, default 5
#' @export head5
#' @examples
#' abc <- data.table(matrix(1:2000, 20, 100))
#' head5(abc)

head5 <- function(df, nc=5, nr=5){
    if(any(class(df)=="data.table")){
        print(head(df[1:nr,1:nc]))
    }else{
        print(head(df[,c(1:nc)], n=nr))
    }
}
