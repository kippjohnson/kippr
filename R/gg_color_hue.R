#' Add together two numbers.
#'
#' @param n Number of colors needed
#' @param alpha Alpha value for palette
#' @export gg_color_hue
#' @return A vector with n colors
#' @examples
#' gg_color_hue(6)

gg_color_hue <- function(n, alpha=1.0) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100, alpha=alpha)[1:n]
}



