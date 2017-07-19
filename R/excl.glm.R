#' Easily exclude columns in glm()
#'
#' Preserve the " Y ~ . " formula in a glm, but exclude specific named columns
#' from the "." (i.e., exclude a column or columns from the rest of the
#' variables in the dataset).
#'
#' @param Y Dependent variable
#' @param indata Dataset
#' @param excl.cols Vector of column names or vector of column numbers to exclude
#' @export
#' @examples
#'
#' data(infert)
#' m1 <- excl.glm("conc", Theoph, excl.cols="Subject")
#' summary(m1)
#'
#' m2 <- excl.glm("conc", Theoph, excl.cols=c(1,2))
#' summary(m2)
#'
#' m3 <- excl.glm("case", infert, excl.cols="Subject", family=binomial())
#' summary(m3)

excl.glm <- function(depvar, indata, excl.cols=NULL, ...){

    if(class(excl.cols)=="character"){ # remove by row name

        data <- indata[, !(colnames(indata) %in% excl.cols)]

        }else if(class(excl.cols)=="numeric"){ # remove by row index

        data <- indata[, -excl.cols]
                }

    form <- as.formula(paste0(depvar, " ~ ."))
    args <- list(formula = form, data = indata, ...)
    mod <- do.call(glm, args)

    return(mod)
}


