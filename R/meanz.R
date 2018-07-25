meanz <-
function(p)  {
   keep <- (p > 0) & (p < 1)
   if(sum(1L * keep) < 2)
      stop("Must have at least two valid p values")
   if(sum(1L * keep) != length(p)) {
      warning("Some studies omitted")
   }
   zvals <- (qnorm(p[keep], lower.tail = FALSE))
   zp <- mean(zvals) / (sd(zvals) / sqrt(length(p[keep])))
   res <- list(z = zp, p = pnorm(zp, lower.tail = FALSE),
      validp = p[keep])
   class(res) <- c("meanz", "metap")
   res
}
print.meanz <- function(x, ...) {
   cat("meanz = ", x$z, "p = ", x$p, "\n")
   invisible(x)
}
