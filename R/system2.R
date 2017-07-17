#' Print system commands to KnitR Output
#' Taken from Stack Overflow
#'
#' @example
#' \donotrun{
#' cmd <- paste0(
#' plink2,
#' " --bfile ", mega.bedfile,
#' " --extract ", var.file,
#' " --recode A",
#' " --threads ", nthreads,
#' " --pca 10",
#' " --out BioMe_htn_variant_subset"
#')
#'}

system2 <- function(...){
    stopifnot(!any(names(list(...)) %in% "intern"))
    result <- base::system(..., intern = TRUE)
    cat(paste0(result, "\n"))
}
