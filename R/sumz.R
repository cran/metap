sumz <-
function(p)  {
   keep <- (p > 0) & (p < 1)
   zp <- qnorm(p[keep], lower.tail = FALSE)
   k <- length(zp)
   if(sum(1L * keep) < 2)
      stop("Must have at least two valid p values")
   zz <- sum(zp) / sqrt(k)
   if(length(zp) != length(p)) {
      warning("Some studies omitted")
   }
   res <- list(z = zz, p = pnorm(zz, lower.tail = FALSE),
      validp = p[keep])
   class(res) <- c("sumz", "metap")
   res
}
print.sumz <- function(x, ...) {
   cat("sumz = ", x$z, "p = ", x$p, "\n")
   invisible(x)
}
