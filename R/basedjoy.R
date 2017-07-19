#' (Partial) Reimplementation of the ggjoy package in base R graphics
#' For those who are allergic to ggplot
#'
#' @param density.var The variable you want to calculate densities for
#' @param grouping.var A grouping variable (not: doesn't work with continuous variable yet)
#' @param dataset The input dataset
#' @param shrinkfactor Controls space between the distributions
#' @param fill.col Fill color for the distributions
#' @param add.grid Should we continue the x lines past calculated density
#' @param gridcolor Color of gridlines, only if add.grid is TRUE
#' @param xlabtext Self explanatory
#' @param ylabtext Self explanatory
#' @param addgroupnames Add names of the grouping variable to Y axis?
#' @param title Set "main=title"
#' @param ... Additional graphical parameters to pass to par()
#' @export basedjoy
#' @examples
#' Using the iris dataset:
#' basedjoy(density.var="Sepal.Length", grouping.var="Species", data=iris,
#' mai=c(1,1,1,1))
#'

basedjoy <- function(density.var, grouping.var, dataset, shrinkfactor=2.5,
                     global.bw=TRUE, user.bw=NA,
                     fill.col="grey", add.grid=TRUE, gridcolor="black",
                     xlabtext="", ylabtext="", addgroupnames=TRUE,
                     title="",
                     userfromto=FALSE, userfrom=NA, userto=NA,
                     ...){

  dots <- list(...)
  do.call(par, dots)

  group.names <- unique(dataset[,grouping.var]) # get the group names
  n.groups <- length(group.names)

  if(global.bw==TRUE){
    bw.total <- bw.nrd(dataset[, density.var]) # use same bandwidth for each plot
  }else if(global.bw==FALSE | !(is.na(user.bw))){
     bw.user <- bw.user
  }

  min.density.var <- min(
    by(iris[, "Sepal.Length"], iris[,"Species"],
       function(x) min(density(x)$x))
    )

  max.density.var <- min(
    by(iris[, "Sepal.Length"], iris[,"Species"],
       function(x) max(density(x)$x))
  )

  # Establish the maximum Y value
  y.max <- -10
  for(i in 1:n.groups){
    dvec <- dataset[dataset[, grouping.var] == group.names[i], density.var]

    if(userfromto==FALSE){
    if(global.bw==TRUE & is.na(user.bw)){
        dens <- density(x=dvec, bw=bw.total) # if global bw
    }else if(global.bw==FALSE & is.na(user.bw)){
        dens <- density(x=dvec, bw=bw.user) # if specificed bw
    }else{
        dens <- density(x=dvec) # default calc of bw
    }
    }else if(userfromto==TRUE){
        dens <- density(x=dvec, bw=bw.user,
                        from=userfrom, to=userto)
        }

    y.var <- max(dens$y+(n.groups-i)/shrinkfactor)
    if(y.var > y.max){y.max <- y.var}
  }

  # Plot the null plot
  seqlength <- 100
  plot(x=seq(max.density.var, max.density.var, length.out=seqlength),
       y=seq(0, y.max/shrinkfactor, length.out=seqlength),
       type="n",
       xlab="",
       ylab="",
       yaxt="n",
       xaxt="n",
       main=title)

  title(xlab=xlabtext, ylab=ylabtext)

  # Add in each distribution
  ypos.store <- numeric(length=n.groups)
  for(i in 1:n.groups){

    dvec <- dataset[dataset[, grouping.var] == group.names[i], density.var]

    dens <- density(x=dvec, bw=bw.total)

    x.var <- dens$x
    y.var <- dens$y+(n.groups-i)/shrinkfactor
    ypos.store[i] <- min(y.var)

    if(add.grid==TRUE){abline(h=min(y.var/shrinkfactor), col=gridcolor)}

    lines(x.var, y.var/shrinkfactor)
    polygon(x.var, y.var/shrinkfactor, col=fill.col)
  }

  if(addgroupnames==TRUE){
    axis(2,
         at=ypos.store/shrinkfactor,
         labels=group.names,
         las=1)
  }

};



