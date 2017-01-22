### R code from vignette source 'metap.Rnw'

###################################################
### code chunk number 1: metap.Rnw:158-159
###################################################
library(metap)


###################################################
### code chunk number 2: metap.Rnw:174-178
###################################################
pvals <- c(0.1, 0.1, 0.9, 0.9, 0.9, 0.9)
istwo <- c(TRUE,  FALSE, TRUE, FALSE, TRUE, FALSE)
toinvert <- c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
two2one(pvals, two = istwo, invert = toinvert)


###################################################
### code chunk number 3: metap.Rnw:183-184
###################################################
data(validity)


###################################################
### code chunk number 4: metap.Rnw:189-190
###################################################
print(validity)


###################################################
### code chunk number 5: metap.Rnw:194-196
###################################################
par(pin = c(3, 3))
schweder(validity)


###################################################
### code chunk number 6: metap.Rnw:237-240
###################################################
par(pin = c(3, 3))
schweder(validity, drawline = c("bh", "ls", "ab"),
   ls.control = list(frac = 0.5), ab.control = list(a = 0, b = 0.01))


###################################################
### code chunk number 7: metap.Rnw:276-292
###################################################
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
### code chunk number 8: metap.Rnw:295-307
###################################################
   kvals <- c(4, 5, 6, 8, 10, 15, 20)
   pvals <- c(0.2, 0.3, 0.3679, 0.4, 0.5, 0.6)
   dat <- rbind(
      genvec(pvals, kvals, logitp, "logitp"),
      genvec(pvals, kvals, meanp, "meanp"),
      genvec(pvals, kvals, maximump, "maximump"),
      genvec(pvals, kvals, minimump, "minimump"),
      genvec(pvals, kvals, sumlog, "sumlog"),
      genvec(pvals, kvals, sump, "sump"),
      genvec(pvals, kvals, sumz, "sumz"),
      genvec(pvals, kvals, votep, "votep")
   )


###################################################
### code chunk number 9: metap.Rnw:311-315
###################################################
   lattice::xyplot(g ~ k | method, groups = p, type = "l", data = dat,
      auto.key = list(space = "left", lines = TRUE, title = "p"),
      ylab = "g(p)"
   )


###################################################
### code chunk number 10: metap.Rnw:375-377
###################################################
pvals <- c(0.001, 0.001, 0.999, 0.999)
sumlog(pvals)


###################################################
### code chunk number 11: metap.Rnw:407-408
###################################################
sumz(pvals)


###################################################
### code chunk number 12: metap.Rnw:452-453
###################################################
logitp(pvals)


###################################################
### code chunk number 13: metap.Rnw:460-463
###################################################
sumlog(validity)
sumz(validity)
logitp(validity)


###################################################
### code chunk number 14: metap.Rnw:507-509
###################################################
minimump(pvals)
maximump(pvals)


###################################################
### code chunk number 15: metap.Rnw:539-540
###################################################
sump(pvals)


###################################################
### code chunk number 16: metap.Rnw:589-590
###################################################
meanp(pvals)


###################################################
### code chunk number 17: metap.Rnw:596-600
###################################################
minimump(validity)
maximump(validity)
sump(validity)
meanp(validity)


###################################################
### code chunk number 18: metap.Rnw:633-634
###################################################
votep(pvals)


###################################################
### code chunk number 19: metap.Rnw:639-640
###################################################
votep(validity)


