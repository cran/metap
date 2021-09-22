wilkinsonp <-
function(p, r = 1, alpha = 0.05, log.p = FALSE) {
   alpha <- ifelse(alpha > 1, alpha / 100, alpha) # if percent
   stopifnot(alpha > 0, alpha < 1)
   alpha <- ifelse(alpha > 0.5, 1 - alpha, alpha) # map to 0 to 0.5
   keep <- (p >= 0) & (p <= 1)
   invalid <- sum(1L * keep) < 2
   if (invalid)   {
      warning("Must have at least two valid p values")
      res <- list(p = NA_real_, pr = NA_real_,
         r = r, critp = NA_real_, alpha = alpha, validp = p[keep])
   } else {
      pi <- p[keep]
      k <- length(pi)
      if(k != length(p)) {
         warning("Some studies omitted")
      }
      if((r < 1) | (r > k)) {
         r <- 1
         warning("Illegal r set to 1")
      }
      pi <- sort(pi)
      pr <- pi[r]
      res <- list(p = pbeta(pr, r, k + 1 - r, log.p = log.p), pr = pr,
         r = r, critp = qbeta(alpha, r, k + 1 - r), alpha = alpha, validp = pi)
    }
   class(res) <- c("wilkinsonp", "metap")
   res
}
maximump <- function(p,  alpha = 0.05, log.p = FALSE) {
   keep <- (p >= 0) & (p <= 1)
   validp <- p[keep]
   k <- length(validp)
   res <- wilkinsonp(p, r = k, alpha, log.p)
   class(res) <- c("maximump", "metap")
   res
}
minimump <- function(p,  alpha = 0.05, log.p = FALSE) {
   res <- wilkinsonp(p, r = 1, alpha, log.p)
   class(res) <- c("minimump", "metap")
   res
}
print.wilkinsonp <- function(x, ...) {
   r <- x$r
   p <- x$p
   if(r == 1) {
      cat("p = ", p, " using ", paste("minimum p", sep = ""), "\n")
   } else if(r == 2) {
      cat("p = ", p, " using ", paste(r, "nd minimum p", sep = ""), "\n")
   } else  if(r == 3) {
      cat("p = ", p, " using ", paste(r, "rd minimum p", sep = ""), "\n")
   } else {
      cat("p = ", p, " using ", paste(r, "th minimum p", sep = ""), "\n")
   }
   invisible(x)
}
print.maximump <- function(x, ...) {
   p <- x$p
   cat("p = ", p, " using maximum p", "\n")
   invisible(x)
}
print.minimump <- function(x, ...) {
   p <- x$p
   cat("p = ", p, " using minimum p", "\n")
   invisible(x)
}
