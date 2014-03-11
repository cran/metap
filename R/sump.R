sump <-
function(p)  {
   keep <- (p >= 0) & (p <= 1)
   pi <- sum(p[keep])
   k <- length(p[keep])
   if(sum(1L * keep) < 2)
      stop("Must have at least two valid p values")
   conservativep <- exp( k * log(pi) - lgamma(k + 1))
   nterm <- floor(pi) + 1 # how many values of sump
   denom <- lfactorial(k)
   psum <- 0
   for (i in 1:nterm) {
      term <- lchoose(k, i - 1) + k * log(pi - i + 1) - denom
      pm <- 2 * (i %% 2) - 1
      psum <- psum + pm * exp(term)
   }
   if(k != length(p)) {
      warning("Some studies omitted")
   }
   res <- list(psum = psum, conservativep = conservativep, validp = p[keep])
   class(res) <- c("sump", "metap")
   res
}
print.sump <- function(x, ...) {
   cat("psum = ", x$psum, "\n")
   invisible(x)
}
