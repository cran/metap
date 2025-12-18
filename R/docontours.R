docontours <- function(conttype, xvals, contvals, xtrans, ytrans,
   xlims, ylims, ltys, contlabs) {
#
# conttype Character: one of "smd", "or", "corr"}
# xvals A vector of values to evaluate the contour}
# contvals The values at which contours are drawn}
# xtrans A function for transforming the x-axis}
# ytrans A function for transforming the y-axis}
# xlims The limits for the x-axis in plotting units}
# ylims The limits for the y-axis in plottng units}
# ltys A vector of line types for the contours}
# contlabs Logical, draw the labels of the contours?}
   corr2n <- function(p, r) {
      res <- qnorm(p) ^ 2 / atanh(r) ^ 2 + 3
      res
   }
#
   smd2n <- function(p, smd) {
      res <- qnorm(p) ^ 2 * (8 + smd ^2) / (2 * smd ^ 2)
      res 
   }
#
   or2n <- function(p, or) {
      lnor2 <- log(or) ^ 2
      res <- qnorm(p) ^ 2 * (8 * pi ^ 2 + 3 * lnor2) / (6 * lnor2)
      res
   }
#
   valid <- TRUE
   if(is.null(conttype)) {
      valid <- FALSE
   } else if(is.null(contvals)) {
      valid <- FALSE
   } else if(!conttype %in% c("smd", "or", "corr")) {
      valid <- FALSE
      warning(paste("Unrecognised effect type", conttype))
   } else if(conttype == "smd") {
      contfunc <- smd2n # set contfunc here as function for contours
   } else if(conttype == "corr") {
      contfunc <- corr2n
   } else if(conttype == "or") {
      contfunc <- or2n
   }
   if(valid) {
      pvals <- c(0.4, xvals)
      # now set up line types
      l <- length(contvals)
      if(is.null(ltys) | length(ltys != l)) {
         ltys <- 1:l
      } else if(length(ltys) == 1) {
         ltys <- rep(ltys[1], l)
      }
      locallabs <- TRUE
      if(!is.null(contlabs)) locallabs <- contlabs
      for(i in 1:l) {
         nvals <- contfunc(pvals, contvals[i])
         temp <- spline(xtrans(pvals), ytrans(nvals), n = 101)
         # now find where it leaves plotting area
         len <- length(temp$x)
         if(locallabs) {
            j <- 1
            while(temp$x[j] < xlims[2] & temp$y[j] < ylims[2] & j < len) {
               j <- j + 1
            }
            textx <- temp$x[j]
            texty <- temp$y[j]
            text(textx, texty, as.character(contvals[i]), pos = 2)
        } 
        # shall we move this out of the function
         lines(temp, lty = ltys[i])
         lines(spline(-xtrans(pvals), ytrans(nvals), n = 101), lty = ltys[i])
      }
      # shall we return anything here including when !valid
   }
}
