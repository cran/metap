logitp <-
function(p, log.p = FALSE)  {
   keep <- (p > 0) & (p < 1)
   invalid <- sum(1L * keep) < 2
   if(invalid) {
      warning("Must have at least two valid p values")
      res <- list(t = NA_real_, df = NA_integer_, p = NA_real_,
         validp = p[keep])
   } else {
      psum <- sum(log(p[keep] / (1 - p[keep])))
      k <- length(p[keep])
      mult <- -1 / sqrt(k * pi ^ 2 * (5 * k + 2) / (3 * (5 * k + 4)))
      if(k != length(p)) {
         warning("Some studies omitted")
      }
      t <- mult * psum
      df <- (5 * k + 4)
      res <- list(t = t, df = df, 
         p = pt(t, df, lower.tail = FALSE, log.p = log.p),
         validp = p[keep])
   }
   class(res) <- c("logitp", "metap")
   res
}
print.logitp <- function(x, ...) {
   cat("t = ", x$t, " with df = ", x$df, " p = ", x$p, "\n")
   invisible(x)
}
