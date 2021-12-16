albatros <- function(p, n, axes = list(xlimit = NULL, ylimit = NULL,
      lefttext = NULL, righttext = NULL),
   contours = list(type = NULL, contvals = NULL, ltys = NULL, contlabs = NULL),
   plotpars = list(yscale = NULL, pchs = NULL, cols = NULL), ...) {
   xtrans <- function(p) {
      p2 <- ifelse(p > 0.5, 1 - p, p)
      res <- log(1 / p2, base = 10)
      res <- ifelse(p > 0.5, -res, res)
      res
   }
   ytrans <- function(n) {
      if(is.null(plotpars$yscale)) {
         yscale <- "sqrt"
      } else yscale <- plotpars$yscale
      if(yscale == "classic") {
         res <- log(n, base = 10) ^ 2
      } else if(yscale == "n") {
         res <- n
      }  else if(yscale == "sqrt") {
         res <- sqrt(n)
      } else {
         warning("Defaulting yscale to sqrt")
         res <- sqrt(n)
      }     
      res
   }
#
   if(length(p) != length(n)) {
      stop(paste("p", length(p), " and n", length(n), "must be same length"))
   }
   keep <- (p < 1) & (p > 0)
   validp <- p[keep] # remove zeroes and ones for axis calc
   extreme <- min(min(validp), min(1 - validp))
# set up axes
   if(is.null(axes$xlimit)) {
      xlims <- c(-xtrans(extreme), xtrans(extreme))
   } else {
      xlims <- c(-xtrans(axes$xlimit), xtrans(axes$xlimit))
   }
   if(is.null(axes$ylimit)) {
      ylims <- c(ytrans(min(n)) * 0.9, ytrans(max(n)) * 1.1)
   } else {
      ylims <- c(ytrans(axes$ylimit[1]), ytrans(axes$ylimit[2]))
   }
#   
   if(is.null(plotpars$pchs)) {
      pchs <- rep(1, length(p))
   } else {
      pchs <- plotpars$pchs
   }
   if(is.null(plotpars$cols)) {
      cols <- rep("lightgrey", length(p))
   } else {
      cols <- plotpars$cols
   }
   plot(xtrans(p), ytrans(n), 
     xlim = xlims, ylim = ylims, pch = pchs,
     xaxt = "n", xlab = "p value", yaxt = "n", ylab = "N", ...
   )
#
# set up x-axis values and plot axis
   sigs <- c(0.05, 0.01)
   i <- 2
   while(sigs[i] > extreme) {
     sigs[i+1] <- sigs[i] / 10
     i <- i + 1
   }
   axis(1, at = c(-xtrans(sigs), 0, xtrans(sigs)),
      labels = c(sigs, "null", sigs), cex.axis = 0.75, las = 3,
         ...)
# set up y-axis values and plot axis
   ylabs <- c(1, 2, 5)
   maxn <- max(n)
   i <- 3
   while(ylabs[i] < maxn) {
      ylabs[i+1] <- ylabs[i - 2] * 10
      i <- i + 1
   }
   axis(2, at = ytrans(ylabs), labels = ylabs, cex.axis = 0.75,
      las = 2, ...)
# plot contours
   docontours(contours$type, sigs, contours$contvals, xtrans, ytrans,
      xlims, ylims, contours$ltys, contours$contlabs)
   if(!is.null(axes$lefttext)) {
      mtext(axes$lefttext, side = 1, line = 3, adj = 0)
   }
   if(!is.null(axes$righttext)) {
      mtext(axes$righttext, side = 1, line = 3, adj = 1)
   }
   res <- list(xlims = xlims, ylims = ylims)
   res
}
