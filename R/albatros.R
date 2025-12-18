albatros <- function(p, n, axes = list(xlimit = NULL, ylimit = NULL,
      lefttext = NULL, righttext = NULL, xlas = 3, ylas = 2),
   contours = list(type = NULL, contvals = NULL, ltys = NULL, contlabs = NULL),
   plotpars = list(yscale = "sqrt", yfun = NULL, pchs = NULL, cols = NULL), ...) {
#
# define axis transformation functions
xtrans <- makexscale(p)
ytrans <- makeyscale(plotpars$yscale, plotpars$yfun) 
#
   if(length(p) != length(n)) {
      stop(paste("p", length(p), " and n", length(n), "must be same length"))
   }
   keep <- (p < 1) & (p > 0)
   validp <- p[keep] # remove zeroes and ones for axis calc and plotting
   validn <- n[keep] 
   is.na(validn) <- n < 2 # need to remove illegal n as well
   extreme <- min(min(validp), min(1 - validp))
# set up axes
   if(is.null(axes$xlimit)) { # set limits for x
      xlims <- c(-xtrans(extreme), xtrans(extreme))
   } else {
      xlims <- c(-xtrans(axes$xlimit), xtrans(axes$xlimit))
   }
   if(is.null(axes$ylimit)) { # set limit for y allowing space
      ylims <- c(ytrans(min(validn, na.rm = TRUE)) * 0.9,
         ytrans(max(validn, na.rm = TRUE)) * 1.1)
   } else {
      ylims <- c(ytrans(axes$ylimit[1]), ytrans(axes$ylimit[2]))
   }
#   
   if(is.null(plotpars$pchs)) {
      pchs <- rep(1, length(validp))
   } else {
      pchs <- plotpars$pchs
   }
   if(is.null(plotpars$cols)) {
      cols <- rep("lightgrey", length(validp))
   } else {
      cols <- plotpars$cols
   }
   # now plot points without any axes
   plot(xtrans(validp), ytrans(validn), 
     xlim = xlims, ylim = ylims, pch = pchs,
     xaxt = "n", xlab = "p value", yaxt = "n", ylab = "N", ...
   )
#
# set up x-axis values and plot axis
   sigs <- c(0.05, 0.01)
   i <- 2
   while(sigs[i] > extreme) { # include 0.001, 0.0001 as necessary
     sigs[i+1] <- sigs[i] / 10
     i <- i + 1
   }
   axis(1, at = c(-xtrans(sigs), 0, xtrans(sigs)),
      labels = c(sigs, "null", sigs), cex.axis = 0.75,
         las = axes$xlas, ...)
# set up y-axis values and plot axis
   maxn <- max(validn, na.rm = TRUE)
   if(maxn <= 500) {
      ylabs <- c(1, 2, 5)
      i <- 3
      while(ylabs[i] < maxn) { # include 10, 20, 50 as necessary
         ylabs[i+1] <- ylabs[i - 2] * 10
         i <- i + 1
      }
   } else {
      ylabs <- c(1, 5)
      i <- 2
      while(ylabs[i] < maxn) { # include 10, 50, 100 as necessaryw
         ylabs[i + 1] <- ylabs[i - 1] * 10
         i <- i + 1
      }
   }
   axis(2, at = ytrans(ylabs), labels = ylabs, cex.axis = 0.75,
      las = axes$ylas, ...)
# plot contours
   docontours(contours$type, sigs, contours$contvals, xtrans, ytrans,
      xlims, ylims, contours$ltys, contours$contlabs)
   if(!is.null(axes$lefttext)) {
      mtext(axes$lefttext, side = 1, line = 3, adj = 0)
   }
   if(!is.null(axes$righttext)) {
      mtext(axes$righttext, side = 1, line = 3, adj = 1)
   }
   # what to return here as well?
   res <- list(xlims = xlims, ylims = ylims, validp = validp,
      validn = validn)
   res
}
