truncated <- function(p, ptrunc = NULL, rtrunc = NULL, ...) {
#
dop <- function(validp, ptrunc) {
   if(requireNamespace("TFisher")) {
      if((ptrunc <= 0) | (ptrunc > 1))
      {
         warning("The value of ptrunc must be between 0 and 1")
         warning("Setting p to 0.5")
         ptrunc <- 0.5
      }
      nincl <- sum(1L * (validp < ptrunc))
      nexcl <- sum(1L * (validp >= ptrunc))
      if(nincl < 1) warning("No p values left after truncation")
      if(nexcl < 1) message("No studies removed by truncation")
      stat <- TFisher::stat.tpm(validp, ptrunc)
      pval <- 1 - TFisher::p.tpm(stat, length(validp), ptrunc)
      res <- list(p = pval, stat = stat, validp = validp,
         ptrunc = ptrunc, n = c(incl = nincl, excl = nexcl),
         method = "truncated")
   } else {
      warning("Need to install TFisher for this option")
      res <- list(p = NA, stat = NA)
   }
   res
}
#
dor <- function(validp, rtrunc, ...) {
   if(requireNamespace("mutoss")) {
      if((rtrunc < 1) | (rtrunc > length(validp)))
      {
         l <- length(validp)
         warning(paste("The value of rtrunc must be between 1 and "), l)
         warning(paste("Setting rtrunc to", l %/% 2))
         rtrunc <- l %/% 2
      }
      nincl <- rtrunc
      nexcl <- length(validp) - nincl
      if(nincl < 1) warning("No p values left after truncation")
      if(nexcl < 1) message("No studies removed by truncation")
      temp <- mutoss::ranktruncated(validp, rtrunc, ...)
      stat <- temp$RTP$Statistic
      pval <- temp$RTP$p.Value
      res <- list(p = pval, stat = stat, validp = validp,
         rtrunc = rtrunc, n = c(incl = nincl, excl = nexcl),
         method = "rank")
      } else {
         warning("Need to install mutoss for this option")
         res <- list(p = NA, stat = NA)
      }
   res
}
#
   keep <- (p > 0) & (p <= 1)
   if(sum(1L * keep) < 2) {
      warning("Must have at least two valid p values")
      res <- list(p = NA, stat = NA)
   } else {
      validp <- p[keep]
      if(length(validp) != length(p)) {
         warning("Some studies omitted")
      }
      usetpm <- !is.null(ptrunc)
      userank <- !is.null(rtrunc)
      if(usetpm & userank) {
         warning("Specifying both ptrunc and rtrunc undefined")
         res <- list(p = NA, stat = NA)
      } else if(usetpm) {
         res <- dop(validp, ptrunc)
      } else if(userank) {
         res <- dor(validp, rtrunc)
      } else {
         warning("Must specify one of ptrunc or rtrunc")
         res <- list(p = NA, stat = NA)
      }
   }
   class(res) <- c("truncated", "list")
   res
}
print.truncated <- function(x, ...) {
   cat("stat = ", x$stat, " p = ", x$p, "\n")
   invisible(x)
}
summary.truncated <- function(object, ...) {
   with(object, {
      cat("stat = ", stat, " p = ", p, "\n")
      if(!is.na(stat)) {
         cat(paste("Using", n["incl"], "values of",
            n["incl"] + n["excl"], "valid p values\n"))
         if(method == "truncated") {
            cat("Truncated at p= ", ptrunc, "\n")
         }
         if(method == "rank") {
            cat("Truncated at rank= ", rtrunc, "\n")
         }
      } # end valid stat
   }) # end with
   invisible(object)
}
plot.truncated <- function(x, pparams = list(pchs = c(16, 1),
      pcols = c("black", "black")), ...) {
   if(is.na(x$stat)) {
      warning("Nothing to plot")
   } else {
      with(x, {
         p <- sort(validp, decreasing = FALSE)
         nused <- n["incl"]
         k <- length(p)
         plot(1:k, c(rep(0, k-1), 1), type = "n", xlab = "Index",
	        ylab = "p", ...)
         if(nused == k) {
            points(1:nused, p[1:nused], pch = pparams$pchs[1],
               col = pparams$pcols[1])
         } else if(nused == 0) {
            points((nused+1):k, p[(nused+1):k], pch = pparams$pchs[2],
               col = pparams$pcols[2])
         } else {
            points(1:nused, p[1:nused], pch = pparams$pchs[1],
               col = pparams$pcols[1])
            points((nused+1):k, p[(nused+1):k], pch = pparams$pchs[2],
               col = pparams$pcols[2])
         }
      })
   }
   invisible(x)
}
