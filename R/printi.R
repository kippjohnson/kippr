#' Print every n instances of i
#'
#' @param i The object being iterated
#' @param n Print every n iterations
#' @param stringname Print stringname before i
#' @export
#' @examples
#' for(i in 1:100){printi(i, 10)}

printi <- function(i, n=10, stringname="Iteration "){
    if(i %% n == 0){
        print(paste0(stringname,i))
        }
    }
