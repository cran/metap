### R code from vignette source 'metap.Rnw'

###################################################
### code chunk number 1: metap.Rnw:118-119
###################################################
library(metap)


###################################################
### code chunk number 2: metap.Rnw:136-140
###################################################
pvals <- c(0.1, 0.1, 0.9, 0.9, 0.9, 0.9)
istwo <- c(TRUE,  FALSE, TRUE, FALSE, TRUE, FALSE)
toinvert <- c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
two2one(pvals, two = istwo, invert = toinvert)


###################################################
### code chunk number 3: metap.Rnw:146-147
###################################################
data(validity)


###################################################
### code chunk number 4: metap.Rnw:152-153
###################################################
print(validity)


###################################################
### code chunk number 5: plotp
###################################################
plotp(validity)


###################################################
### code chunk number 6: simple
###################################################
schweder(validity)


###################################################
### code chunk number 7: withlines
###################################################
schweder(validity, drawline = c("bh", "ls", "ab"),
   ls.control = list(frac = 0.5), ab.control = list(a = 0, b = 0.01))


###################################################
### code chunk number 8: metap.Rnw:250-251
###################################################
sumlog(validity)


