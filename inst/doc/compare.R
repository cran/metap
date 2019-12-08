### R code from vignette source 'compare.Rnw'

###################################################
### code chunk number 1: compare.Rnw:169-170
###################################################
cancel <- c(0.001, 0.001, 0.999, 0.999)


###################################################
### code chunk number 2: compare.Rnw:174-192
###################################################
library(metap)
data(validity)
genvec <- function(pvals, kvals, fun, name) {
   ps <- length(pvals)
   ks <- length(kvals)
   temp <- matrix(-1, nrow = ps, ncol = ks)
   for(i in 1:ps)
   for(j in 1:ks) {
      temp[i, j] <- fun(rep(pvals[i], kvals[j]))$p
   }
   temp2 <- as.vector(temp)
   res <- data.frame(method = rep(name, length(temp2)),
      p = rep(pvals, ks),
      k = rep(kvals, each = ps),
      g = temp2
   )
   res
}


###################################################
### code chunk number 3: compare.Rnw:219-227
###################################################
   kvals <- c(4, 5, 6, 8, 10, 15, 20)
   pvals <- c(0.2, 0.3, 0.3679, 0.4, 0.5, 0.6)
   dat <- rbind(
      genvec(pvals, kvals, logitp, "logitp"),
      genvec(pvals, kvals, meanz, "meanz"),
      genvec(pvals, kvals, sumlog, "sumlog"),
      genvec(pvals, kvals, sumz, "sumz")
   )


###################################################
### code chunk number 4: transeqp
###################################################
   lattice::xyplot(g ~ k | method, groups = p, type = "l", data = dat,
      auto.key = list(space = "left", lines = TRUE, title = "p"),
      ylab = "g(p)"
   )


###################################################
### code chunk number 5: compare.Rnw:370-379
###################################################
   kvals <- c(4, 5, 6, 8, 10, 15, 20)
   pvals <- c(0.2, 0.3, 0.3679, 0.4, 0.5, 0.6)
   dat <- rbind(
      genvec(pvals, kvals, meanp, "meanp"),
      genvec(pvals, kvals, maximump, "maximump"),
      genvec(pvals, kvals, minimump, "minimump"),
      genvec(pvals, kvals, sump, "sump"),
      genvec(pvals, kvals, votep, "votep")
   )


###################################################
### code chunk number 6: untranseqp
###################################################
   lattice::xyplot(g ~ k | method, groups = p, type = "l", data = dat,
      auto.key = list(space = "left", lines = TRUE, title = "p"),
      ylab = "g(p)"
   )


