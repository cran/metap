invt <-
function(p, k, data = NULL, subset = NULL, na.action = na.fail) {
   if(is.null(data)) data <- sys.frame(sys.parent())
   mf <- match.call()
   mf$data <- NULL
   mf$subset <- NULL
   mf$na.action <- NULL
   mf[[1]] <- as.name("data.frame")
   mf <- eval(mf, data)
   if(!is.null(subset)) mf <- mf[subset,]
   mf <- na.action(mf)
   p <- as.numeric(mf$p)
   k <- as.numeric(mf$k)
   if(length(k) == 1) {
      k <- rep(k, length(p))
   } else {
   }
   keep <- (p > 0) & (p <= 1) & (k > 2)
   invalid <- sum(1L * keep) < 2
   if(invalid) {
      warning("Must have at least two valid p values")
      res <- list(z = NA_real_, p = NA_real_, validp = p[keep])
   } else {
      pvals <- p[keep]
      k <- k[keep]
      if(length(pvals) != length(p)) {
         warning("Some studies omitted")
      }
      z <- sum(qt(pvals, k, lower.tail = FALSE)) /
         sqrt(sum(k / (k - 2)))
      res <- list(z = z,
         p = pnorm(z, lower.tail = FALSE), validp = pvals)
   }
   class(res) <- c("invt", "metap")
   res
}
print.invt <- function(x, ...) {
   cat("z = ", x$z, " p = ", x$p, "\n")
   invisible(x)
}
