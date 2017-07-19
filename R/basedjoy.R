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
#' @export basedjoy
#' @examples
#' Using the iris dataset:
#' basedjoy(density.var="Sepal.Length", grouping.var="Species", data=iris,
#' mai=c(1,1,1,1))
#'
#' Using an example dataset from ggjoy
#' library(ggjoy)
#' lincoln_df <- as.data.frame(lincoln_weather)
#' lincoln_df$mean.temp <- as.double(lincoln_df$`Mean Temperature [F]`)
#' lincoln_df$mnth <- as.factor(lincoln_df$Month)
#'
#' basedjoy("mean.temp", "mnth", lincoln_df, shrinkfactor=50,
#'         title="Monthly Temperature in Lincoln", mai=c(0.5,1.25,1,0.5))



basedjoy <- function(density.var, grouping.var, dataset, shrinkfactor=2.5,
                     fill.col="grey", add.grid=TRUE, gridcolor="black",
                     xlabtext="", ylabtext="", addgroupnames=TRUE,
                     title="",
                     ...){

  dots <- list(...)
  do.call(par, dots)

  group.names <- unique(dataset[,grouping.var]) # get the group names
  n.groups <- length(group.names)

  bw.total <- signif(bw.nrd(dataset[, density.var]),3) # use same bandwidth for each plot
  print(paste("Using density bandwidth", bw.total))

  data.min <- min(dataset[, density.var]) - 3 * bw.total
  data.max <- max(dataset[, density.var]) + 3 * bw.total

  # Establish the maximum Y value
  y.max <- -Inf
  for(i in 1:n.groups){
    dvec <- dataset[dataset[, grouping.var] == group.names[i], density.var]
    dens <- density(x=dvec, bw=bw.total, from=data.min, to=data.max)

    y.var <- max(dens$y+(n.groups-i)/shrinkfactor)
    if(y.var > y.max){y.max <- y.var}
  }

  # Plot the null plot
  seqlength <- 1e5

  plot(x=seq(data.min, data.max, length.out=seqlength),
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
    dens <- density(x=dvec, bw=bw.total, from=data.min, to=data.max)

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


#basedjoy("mean.temp", "mnth", lincoln_df, shrinkfactor=1)
