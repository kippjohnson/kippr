#' Easily exclude name columns in glm()
#'
#' Preserve the " Y ~ . " formula in a glm, but exclude specific named columns
#' from the "." (i.e., exclude a column or columns from the rest of the
#' variables in the dataset).
#'
#' @param The Y variable
#' @param indata The dataset
#' @param excl.cols Vector of column names or column numbers to include
#' @export
#' @examples
#'
#' m1 <- excl.glm("conc", Theoph, excl.cols="Subject")
#' summary(m1)
#'
#' m1 <- excl.glm("conc", Theoph, excl.cols=c(1,2))
#' summary(m1)


excl.glm <- function(depvar, indata, excl.cols=NULL, ...){
    form <- as.formula(paste0(depvar, " ~ ."))

    if(class(excl.cols)=="character"){
        mod <- glm(form,
               data=indata[, !(colnames(indata) %in% excl.cols)])
        }else if(class(excl.cols)=="numeric"){
        mod <- glm(form,
                   data=indata[, -excl.cols])
        }
    return(mod)
}