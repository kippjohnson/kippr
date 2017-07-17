#' Remove 0 variance columns or rows
#'
#' Remove 0 variance columns from a dataframe
#'
#' @param df The input data frame
#' @param dfdim 2 for columns, 1 for rows
#' @export
#' @examples
#' abd <- as.data.frame(list(a=c(1:10),b=c(21:30),d=rep(5,10)))
#' rm0var(abd)

rm0var <- function(df, dfdim=2){
    novar_cols <- names( which(apply(df, dfdim, var) == 0 ))
    df <- subset(df, select = !(colnames(df) %in% novar_cols))
    return(df)
}
