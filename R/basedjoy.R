#' (Partial) Reimplementation of the ggjoy package in base R graphics.
#'
#' ggjoy for those who are allergic to ggplot. Original ggjoy by Claus Wilke. Mistakes my own.
#'
#' @param density.var The variable you want to calculate densities for
#' @param grouping.var A grouping variable (Note: doesn't work with continuous variable yet)
#' @param dataset The input dataset, currently must be a dataframe
#' @param shrinkfactor Controls Y overlap/distance between the distributions. Main tuning parameter for the plot. This is essentially the perspective. At 0, you are looking straight down at the distributions and will see nothing. At infinite, all density plots sit same Y axis.
#' @param xstretch x limits controlled by min/max +/- (xstretch * kernel bandwidth). 3 is default in ggjoy.
#' @param fill.col Fill color for the distributions; either one color or a vector of multiple, in which case a gradient will be built of length unique(grouping.var)
#' @param add.grid Should we continue the x lines past calculated density
#' @param gridcolor Color of gridlines, only if add.grid is TRUE
#' @param xlabtext Self explanatory
#' @param ylabtext Self explanatory
#' @param addgroupnames Add names of the grouping variable to Y axis?
#' @param xlabadjLR Float; adjust the group names left or right (if addgroupnames is TRUE)
#' @param title Set "main=title"
#' @param global.lwd line width of the density plot and gridlines (if applicable)
#' @param boxtype Box drawn around plot, options in c("o","7","l","c","u") for lines or "n" for no lines (default)
#' @param verbose Logical; print the kernel bandwidth?
#' @param ... Additional graphical parameters to pass to par()
#' @import graphics
#' @import stats
#' @import grDevices
#' @import utils
#' @export
#' @examples
#' # Using the iris dataset:
#' basedjoy(density.var="Sepal.Length", grouping.var="Species", data=iris,
#' shrinkfactor=10, boxtype="o", mai=c(1,1,1,1))
#'
#' # Using an example dataset from ggjoy
#' library(ggjoy)
#' lincoln_df <- as.data.frame(lincoln_weather)
#' lincoln_df$mean.temp <- as.double(lincoln_df$`Mean Temperature [F]`)
#' lincoln_df$mnth <- as.factor(lincoln_df$Month)
#'
#' basedjoy("mean.temp", "mnth", lincoln_df, shrinkfactor=50,
#'         title="Monthly Temperature in Lincoln", mai=c(0.5,1.25,1,0.5))
#'
#'
#' # Explore how different values of shrinkfactor change perspective
#' par(mfrow=c(2,2))
#' for(i in 2:5){
#'    sf <- (i)^3
#'    basedjoy("mean.temp", "mnth", lincoln_df, shrinkfactor=sf, xstretch=3,
#'             title=paste("shrinkfactor =", sf),
#'             mai=c(0.75,0.75,0.5,0.1), xlabtext="Degrees Fahrenheit", ylabtext="Month",
#'             xlabadjLR=-1.5)
#'             }


basedjoy <- function(density.var, grouping.var, dataset,
                     shrinkfactor=NA,
                     xstretch = 3,
                     fill.col="grey", add.grid=TRUE,
                     x.gridcolor=NA, y.gridcolor="lightgrey",
                     xlabtext="", ylabtext="", addgroupnames=TRUE,
                     xlabadjLR=-1.5,
                     global.lwd=1.5, boxtype="n",
                     title="",
                     verbose=FALSE,
                     ...){

  if(is.na(shrinkfactor)){
      warning("
           You need to input a shrinkfactor into the function call. See the examples
           in '?basedjoy'. shrinkfactor is essentially the perspective which you are
           looking at the distributions. Trying now with shrinkfactor=10")
      shrinkfactor <- 10
  }

  dots <- list(...)
  do.call(par, dots) # add dots parameters to par()

  if(boxtype=="n"){
      boxtype.xaxis <- NA
  }else{
      boxtype.xaxis <- 1
      }

  group.names <- unique(dataset[,grouping.var]) # get the group names
  n.groups <- length(group.names)

  bw.total <- signif(bw.nrd0(dataset[, density.var]), 5) # use same bandwidth for each plot

  if(verbose){ print(paste("Using density bandwidth:", bw.total)) }

  data.min <- min(dataset[, density.var]) - xstretch * bw.total
  data.max <- max(dataset[, density.var]) + xstretch * bw.total

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
       bty=boxtype,
       main=title)

  if(add.grid==TRUE){
      grid(col=y.gridcolor, ny=0, lty=1)
  }

  title(ylab=ylabtext)
  title(xlab=xlabtext)

  fill.cols <- character(length=n.groups)
  if(length(fill.col)==1){
      fill.cols <- rep(fill.col, times=n.groups)
    }else if(length(fill.col>1)){
          colfunc <- colorRampPalette(fill.col)
          fill.cols <- colfunc(n.groups)
          }

  # Add in each distribution
  ypos.store <- numeric(length=n.groups)
  for(i in 1:n.groups){

    dvec <- dataset[dataset[, grouping.var] == group.names[i], density.var]
    dens <- density(x=dvec, bw=bw.total, from=data.min, to=data.max)

    x.var <- dens$x
    y.var <- dens$y+(n.groups-i)/shrinkfactor
    ypos.store[i] <- min(y.var)

    if(add.grid==TRUE){
        abline(h=min(y.var/shrinkfactor),
               col=x.gridcolor, lwd=global.lwd);
        }

    lines(x.var, y.var/shrinkfactor, lwd=global.lwd)
    polygon(x.var, y.var/shrinkfactor, col=fill.cols[i])
  }

  if(addgroupnames==TRUE){
    axis(2,
         at=ypos.store/shrinkfactor,
         labels=group.names,
         las=1,
         lwd=boxtype.xaxis,
         padj=-0.2,
         line=xlabadjLR,
         hadj=1)
    }
}
