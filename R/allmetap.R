allmetap <- function(p, method = NULL) {
   if(is.null(method)) stop("Must specify a method")
mydata <-
  '"funcs","eponyms"
   "logitp",
   "maximump",
   "meanp",
   "minimump","Tippett"
   "sumlog","Fisher"
   "sump","Edgington"
   "sumz","Stouffer"
'
   details <- read.csv(textConnection(mydata), stringsAsFactors = FALSE)
   row.names(details) <- c("logitp", "maximump", "meanp", "minimump",
      "sumlog", "sump", "sumz")
# if all specified reset funcnames
funcnames <- method
if("all" %in% method) funcnames <- row.names(details)
# check all funcnames are valid functions
keep <- funcnames %in% row.names(details)
if(sum(keep * 1L) != length(funcnames)) warning("Some unsupported methods specified")
funcnames <- funcnames[keep]
if(length(funcnames) < 1) stop("No supported methods specified")
helper <- function(x) {
   thisfunc <- match.fun(row.names(details[x,]))
   res <- try(thisfunc(p), silent = TRUE)
   if(inherits(res, "try-error")) {
      res <- list(p = NA, valid = NA)
   } else {
      res <- list(p = res$p, valid = length(res$validp))
   }
   res
}
   res <- as.data.frame(t(sapply(funcnames, helper)))
   eponym <- vector("character", nrow(res))
   for(i in 1:nrow(res)) {
      eponym[i] <- details[row.names(res[i,]),"eponyms"]
   }
   res$eponym <- eponym
   class(res) <- c("allmetap", "data.frame")
   res
}
print.allmetap <- function(x, digits = 5, ...) {
   print(format(x, digits = digits, ...), ...)
   invisible(x)
}
