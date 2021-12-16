allmetap <- function(p, method = NULL, log.p = FALSE) {
   if(is.null(method)) stop("Must specify a method")
mydata <-
  '"funcs","eponyms"
   "logitp",
   "maximump",
   "meanp",
   "meanz",
   "minimump","Tippett"
   "sumlog","Fisher"
   "sump","Edgington"
   "sumz","Stouffer"
'
   con <- textConnection(mydata)
   details <- read.csv(con, stringsAsFactors = FALSE)
   row.names(details) <- c("logitp", "maximump", "meanp", "meanz",
      "minimump", "sumlog", "sump", "sumz")
   close(con)
# now which have log.p as parameter
logpfuncs <- c("logitp", "meanz", "sumlog", "sumz")
# if all specified reset funcnames
funcnames <- method
if("all" %in% method) funcnames <- row.names(details)
# check all funcnames are valid functions
keep <- funcnames %in% row.names(details)
if(sum(keep * 1L) != length(funcnames)) warning("Some unsupported methods specified")
if(log.p) {
   keep2 <- funcnames %in% logpfuncs
} else {
   keep2 <- keep
}
funcnames <- funcnames[keep2]
if(length(funcnames) < 1) stop("No supported methods specified")
helper <- function(x) {
   thisfunc <- match.fun(row.names(details[x,]))
   if(log.p) {
      res <- try(thisfunc(p, log.p = log.p), silent = TRUE)
   } else {
      res <- try(thisfunc(p), silent = TRUE)
   }
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
