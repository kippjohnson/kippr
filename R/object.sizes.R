

object.sizes <- function(){

    sz <- sapply(eval(ls(envir=globalenv())),
                 function(x) { format( (object.size(get(x))), units="MB") })

    x0 <- sapply(eval(ls(envir=globalenv())),
                   function(x) { object.size(get(x)) })

    sumx <- sum(x0)

    print(paste("Total MB:", signif( (sumx/ (1024^2)), 3)), quote=F)

    return(print(sort(sz), quote=FALSE))
}

object.sizes()
