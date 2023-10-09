plotp <- function(pvals, plotversion = "qqconf", ...) {
   keep <- !is.na(pvals)
   validp <- pvals[keep] # remove NA
   keep <- validp >= 0 & validp <= 1
   if(sum(1L * keep) < 1) stop("No points to plot")
   n <- length(validp)
   if(sum(1L * keep) != n) warning("Out of range points omitted")
   y <- validp[keep]
   n <= length(y)
   if(plotversion == "qqconf") {
      if(requireNamespace("qqconf")) {
         qqconf::qq_conf_plot(y, distribution = qunif, ...)
      } else {
         warning("qqconf not installed, consider setting plotversion to old")
      }
   } else {
      qqplot(qunif(ppoints(n)), y, xlab = "Theoretical",
         ylab = "Empirical", ...)
      qqline(y, distribution = qunif, ...)
   }
   res <- list(validp = y)
   invisible(res)
}
plot.metap <- function(x, plotversion = "qqconf", ...) {
   plotp(x$validp, plotversion = plotversion, ...)
   invisible(x)
}
