makexscale <- function(p) {
# out of range p removed by caller
   xtrans <- function(p) {
      p2 <- ifelse(p > 0.5, 1 - p, p)
      res <- log(1 / p2, base = 10)
      res <- ifelse(p > 0.5, -res, res)
      res
   }
   xtrans
}
makeyscale <-
function(yscale, userfun)  {
   nfunc <- function(n) n
   classicfunc <- function(n) {
      (log(n, base = 10))^2
   }
   if(is.null(yscale)) {
      yscale <- "sqrt"
      warning("No yscale specified: defaulting to sqrt")
   }
   if(yscale == "classic") {
      res <- classicfunc
   } else if(yscale == "n") {
      res <- nfunc
   } else if(yscale == "sqrt") {
      res <- sqrt
   } else if(yscale == "userfun") {
      if(is.null(userfun)) {
         warning("You need to specify a function for userfun")
         warning("Defaulting to sqrt")
         res <- sqrt
      } else {
         res <- userfun
      }
   } else {
      warning("Unrecognised y scale, defaulted to sqrt")
      res <- sqrt
   }
   res
}
