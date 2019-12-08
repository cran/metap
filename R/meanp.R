meanp <-
function(p) {
   keep <- (p >= 0) & (p <= 1)
   invalid <- sum(1L * keep) < 4
   if(invalid) {
      warning("Must have at least four valid p values")
      res <- list(z = NA_real_, p = NA_real_,
         validp = p[keep])
   } else {
      pi <- mean(p[keep])
      k <- length(p[keep])
      z <- (0.5 - pi) * sqrt(12 * k)
      if(k != length(p)) {
         warning("Some studies omitted")
      }
      res <- list(z = z, p = pnorm(z, lower.tail = FALSE),
         validp = p[keep])
   }
   class(res) <- c("meanp", "metap")
   res
}
print.meanp <- function(x, ...) {
   cat("z = ", x$z, " p = ", x$p, "\n")
   invisible(x)
}
