sump <-
function(p)  {
   keep <- (p >= 0) & (p <= 1)
   invalid <- sum(1L * keep) < 2
   if(invalid) {
      warning("Must have at least two valid p values")
      res <- list(p = NA_real_, conservativep = NA_real_, validp = p[keep])
   } else {
      sigmap <- sum(p[keep])
      k <- length(p[keep])
      conservativep <- exp( k * log(sigmap) - lgamma(k + 1))
      nterm <- floor(sigmap) + 1 # how many values of sump
      denom <- lfactorial(k)
      psum <- 0
      terms <- vector("numeric", nterm)
      for (i in 1:nterm) {
         terms[i] <- lchoose(k, i - 1) + k * log(sigmap - i + 1) - denom
         pm <- 2 * (i %% 2) - 1
         psum <- psum + pm * exp(terms[i])
      }
      if(k != length(p)) {
         warning("Some studies omitted")
      }
      if(sigmap > 20) {
         warning("Likely to be unreliable, check with another method")
      }
      res <- list(p = psum, conservativep = conservativep, validp = p[keep])
   }
   class(res) <- c("sump", "metap")
   res
}
print.sump <- function(x, ...) {
   cat("psum = ", x$p, "\n")
   invisible(x)
}
