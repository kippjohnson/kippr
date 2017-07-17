#' Make good names
#'
#' Generates good names: lower-cased and appropriate for R
#'
#' @param string The string(s) to make into good names
#' @export goodnames
#' @examples
#' bad.names <- c("ABC","A bad name", "A + B")
#' goodnames(bad.names)

goodnames <- function(string){
    out <- tolower(
        make.names(string)
    )
    return(out)
}
