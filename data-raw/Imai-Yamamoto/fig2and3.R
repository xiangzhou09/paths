#############################################################################
# Replication R code for Imai and Yamamoto, forthcoming in Political Analysis
#############################################################################
library(foreign)
library(mediation)

load("PA-ImaiYamamoto.RData")

set.seed(249)

nboot <- 1000
clevel <- .9

#############################################################################
# Figure 2: Analysis Assuming the Independence between the Mediators
#############################################################################

model.m.slo <- lm(M1 ~ ttt + gender + educ + educmiss + polint + polintmiss
                       + ideo + ideomiss + school + birth + know + value, data = Slothuus)
model.y.slo <- lm(Y ~ ttt * M1 + gender + educ + educmiss + polint + polintmiss 
                       + ideo + ideomiss + school + birth + know + value, data = Slothuus)
resmed.slo <- mediate(model.m.slo, model.y.slo, treat = "ttt", med = "M1",
                      conf.level = clevel, dropobs = TRUE)
ressens.slo <- medsens(resmed.slo, rho.by = 0.01)

model.m.druck <- lm(M ~ T + year + age + sex + ethnic + knowdum + ondum + pid + ideology,
                    data = DruckNels)
model.y.druck <- lm(Y ~ T * M + year + age + sex + ethnic + knowdum + ondum + pid + ideology,
                    data = DruckNels)
resmed.druck <- mediate(model.m.druck, model.y.druck, treat = "T", med = "M",
                        conf.level = clevel, dropobs = TRUE)
ressens.druck <- medsens(resmed.druck, rho.by = 0.01)

model.m.brader <- lm(emo ~ tone_eth + ppage + ppeducat + ppgender + ppincimp,
                     data = Brader)
model.y.brader <- lm(immigr ~ tone_eth * emo + ppage + ppeducat + ppgender + ppincimp,
                     data = Brader)
resmed.brader <- mediate(model.m.brader, model.y.brader, treat = "tone_eth", med = "emo",
                         conf.level = clevel, dropobs = TRUE)
ressens.brader <- medsens(resmed.brader, rho.by = 0.01)

## Druckman
pdf("medsens-indep-druck.pdf", width=3.25, height=9.75)
par(mfcol=c(3,1), mar=c(5,5,4,0.25), oma=c(2,2,3,0.5))

lims <- c(-0.7,1.6)

# 1. mediate
plot(0, 0, type="n", main="Point Estimates",
     xlab = "Average Causal Mediation Effects", ylab = "",
     xlim = lims, ylim = c(0.5, 4.5), yaxt="n")
segments(resmed.druck$d.avg.ci[1], 4, resmed.druck$d.avg.ci[2], 4, lwd=2)
segments(resmed.druck$d1.ci[1], 3, resmed.druck$d1.ci[2], 3, lwd=2)
segments(resmed.druck$d0.ci[1], 2, resmed.druck$d0.ci[2], 2, lwd=2)
segments(resmed.druck$tau.ci[1], 1, resmed.druck$tau.ci[2], 1, lwd=2)
points(resmed.druck$d.avg, 4, pch=18, cex=2)
points(resmed.druck$d1, 3, pch=18, cex=2)
points(resmed.druck$d0, 2, pch=18, cex=2)
points(resmed.druck$tau.coef, 1, pch=18, cex=2)
abline(v=0)
axis(side = 2, labels = c(
  expression(paste("Average (", bar(bar(delta)), ")")),
  expression(paste("Treated (", bar(delta)[1], ")")),
  expression(paste("Control (", bar(delta)[0], ")")),
  expression(paste("Total (", bar(tau), ")"))),
     at = c(4,3,2,1), las = 2)

# 2. rho
d.avg.druck <- (ressens.druck$d1 + ressens.druck$d0)/2
upper.d.avg.druck <- (ressens.druck$upper.d1 + ressens.druck$upper.d0)/2
lower.d.avg.druck <- (ressens.druck$lower.d1 + ressens.druck$lower.d0)/2
plot(0,0, type="n", main="Sensitivity with Respect to \n Error Correlation", 
     xlab = expression(paste(rho)), 
     ylab = expression(paste(bar(bar(delta)), "(", rho, ")")), 
     xlim = range(ressens.druck$rho), ylim = lims)
polygon(c(ressens.druck$rho, rev(ressens.druck$rho)),
        c(lower.d.avg.druck, rev(upper.d.avg.druck)),
        border = FALSE, col = 8, lty = 2)
lines(ressens.druck$rho, d.avg.druck, lwd = 2)
abline(h=0)
abline(h=resmed.druck$d.avg, lty = "dashed")
abline(v=0)

# 3. R2
x <- ressens.druck
R2Mstar <- seq(0, 1-x$rho.by, 0.01)
R2Ystar <- seq(0, 1-x$rho.by, 0.01)
R2M <- R2Mtilde <- (1-x$r.square.m)*seq(0, 1-x$rho.by, 0.01)
R2Y <- R2Ytilde <- (1-x$r.square.y)*seq(0, 1-x$rho.by, 0.01)

dlength <- length(seq(0, (1-x$rho.by)^2, 0.0001))
R2prod.mat <- outer(R2Mstar, R2Ystar)
Rprod.mat <- round(sqrt(R2prod.mat), digits=4)

levels0 <- pretty(quantile(d.avg.druck, probs=c(0.1,0.9)), 10)
d0.p <- rev(approx(d.avg.druck[1:((length(d.avg.druck)+1)/2)], n=dlength)$y)
d0mat.p <- matrix(d0.p[Rprod.mat/0.0001+1], nrow=length(R2M))
contour(R2M, R2Y, d0mat.p, levels=levels0, xlim = c(0,1), ylim = c(0,1), lwd = 1,
        main = "Sensitivity with Respect to \n Proportion of Variance Explained",
        xlab = "", ylab = "", asp=1)
contour(R2M, R2Y, d0mat.p, levels=0, lwd = 2, add=TRUE, drawlabels=FALSE)
title(xlab=expression(paste(tilde(R)[M]^{2})), line=2.5, cex.lab=.9)
title(ylab=expression(paste(tilde(R)[Y]^2)), line=2.5, cex.lab=.9)
axis(2,at=seq(0,1,by=.1))
axis(1,at=seq(0,1,by=.1))

title(main = "Druckman & Nelson (2003)", cex.main = 1.5, outer = T)

dev.off()

## Slothuus
pdf("medsens-indep-slo.pdf", width=3.25, height=9.75)
par(mfcol=c(3,1), mar=c(5,5,4,0.25), oma=c(2,2,3,0.5))

lims <- c(-0.7, 1.6)

# 1. mediate
plot(0, 0, type="n", main="Point Estimates",
     xlab = "Average Causal Mediation Effects", ylab = "",
     xlim = lims, ylim = c(0.5, 4.5), yaxt="n")
segments(resmed.slo$d.avg.ci[1], 4, resmed.slo$d.avg.ci[2], 4, lwd=2)
segments(resmed.slo$d1.ci[1], 3, resmed.slo$d1.ci[2], 3, lwd=2)
segments(resmed.slo$d0.ci[1], 2, resmed.slo$d0.ci[2], 2, lwd=2)
segments(resmed.slo$tau.ci[1], 1, resmed.slo$tau.ci[2], 1, lwd=2)
points(resmed.slo$d.avg, 4, pch=18, cex=2)
points(resmed.slo$d1, 3, pch=18, cex=2)
points(resmed.slo$d0, 2, pch=18, cex=2)
points(resmed.slo$tau.coef, 1, pch=18, cex=2)
abline(v=0)
axis(side = 2, labels = c(
  expression(paste("Average (", bar(bar(delta)), ")")),
  expression(paste("Treated (", bar(delta)[1], ")")),
  expression(paste("Control (", bar(delta)[0], ")")),
  expression(paste("Total (", bar(tau), ")"))),
     at = c(4,3,2,1), las = 2)

# 2. rho
d.avg.slo <- (ressens.slo$d1 + ressens.slo$d0)/2
upper.d.avg.slo <- (ressens.slo$upper.d1 + ressens.slo$upper.d0)/2
lower.d.avg.slo <- (ressens.slo$lower.d1 + ressens.slo$lower.d0)/2
plot(0,0, type="n", main="Sensitivity with Respect to \n Error Correlation", 
     xlab = expression(paste(rho)), 
     ylab = expression(paste(bar(bar(delta)), "(", rho, ")")), 
     xlim = range(ressens.slo$rho), ylim = lims)
polygon(c(ressens.slo$rho, rev(ressens.slo$rho)),
        c(lower.d.avg.slo, rev(upper.d.avg.slo)),
        border = FALSE, col = 8, lty = 2)
lines(ressens.slo$rho, d.avg.slo, lwd = 2)
abline(h=0)
abline(h=resmed.slo$d.avg, lty = "dashed")
abline(v=0)

# 3. R2
x <- ressens.slo
R2Mstar <- seq(0, 1-x$rho.by, 0.01)
R2Ystar <- seq(0, 1-x$rho.by, 0.01)
R2M <- R2Mtilde <- (1-x$r.square.m)*seq(0, 1-x$rho.by, 0.01)
R2Y <- R2Ytilde <- (1-x$r.square.y)*seq(0, 1-x$rho.by, 0.01)

dlength <- length(seq(0, (1-x$rho.by)^2, 0.0001))
R2prod.mat <- outer(R2Mstar, R2Ystar)
Rprod.mat <- round(sqrt(R2prod.mat), digits=4)

levels0 <- pretty(quantile(d.avg.slo, probs=c(0.1,0.9)), 10)
d0.p <- approx(d.avg.slo[((length(d.avg.slo)+1)/2):length(d.avg.slo)], n=dlength)$y
d0mat.p <- matrix(d0.p[Rprod.mat/0.0001+1], nrow=length(R2M))
contour(R2M, R2Y, d0mat.p, levels=levels0, xlim = c(0,1), ylim = c(0,1), lwd = 1,
        main = "Sensitivity with Respect to \n Proportion of Variance Explained",
        xlab = "", ylab = "", asp = 1)
contour(R2M, R2Y, d0mat.p, levels=0, lwd = 2, add=TRUE, drawlabels=FALSE)
title(xlab=expression(paste(tilde(R)[M]^{2})), line=2.5, cex.lab=.9)
title(ylab=expression(paste(tilde(R)[Y]^2)), line=2.5, cex.lab=.9)
axis(2,at=seq(0,1,by=.1))
axis(1,at=seq(0,1,by=.1))

title(main = "Slothuus (2008)", cex.main = 1.5, outer = T)

dev.off()


## Brader
pdf("medsens-indep-brader.pdf", width=3.25, height=9.75)
par(mfcol=c(3,1), mar=c(5,5,4,0.25), oma=c(2,2,3,0.5))

lims <- c(-0.48, 0.66)

# 1. mediate
plot(0, 0, type="n", main="Point Estimates",
     xlab = "Average Causal Mediation Effects", ylab = "",
     xlim = lims, ylim = c(0.5, 4.5), yaxt="n")
segments(resmed.brader$d.avg.ci[1], 4, resmed.brader$d.avg.ci[2], 4, lwd=2)
segments(resmed.brader$d1.ci[1], 3, resmed.brader$d1.ci[2], 3, lwd=2)
segments(resmed.brader$d0.ci[1], 2, resmed.brader$d0.ci[2], 2, lwd=2)
segments(resmed.brader$tau.ci[1], 1, resmed.brader$tau.ci[2], 1, lwd=2)
points(resmed.brader$d.avg, 4, pch=18, cex=2)
points(resmed.brader$d1, 3, pch=18, cex=2)
points(resmed.brader$d0, 2, pch=18, cex=2)
points(resmed.brader$tau.coef, 1, pch=18, cex=2)
abline(v=0)
axis(side = 2, labels = c(
  expression(paste("Average (", bar(bar(delta)), ")")),
  expression(paste("Treated (", bar(delta)[1], ")")),
  expression(paste("Control (", bar(delta)[0], ")")),
  expression(paste("Total (", bar(tau), ")"))),
     at = c(4,3,2,1), las = 2)

# 2. rho
d.avg.brader <- (ressens.brader$d1 + ressens.brader$d0)/2
upper.d.avg.brader <- (ressens.brader$upper.d1 + ressens.brader$upper.d0)/2
lower.d.avg.brader <- (ressens.brader$lower.d1 + ressens.brader$lower.d0)/2
plot(0,0, type="n", main="Sensitivity with Respect to \n Error Correlation", 
     xlab = expression(paste(rho)), 
     ylab = expression(paste(bar(bar(delta)), "(", rho, ")")), 
     xlim = range(ressens.brader$rho), ylim = lims)
polygon(c(ressens.brader$rho, rev(ressens.brader$rho)),
        c(lower.d.avg.brader, rev(upper.d.avg.brader)),
        border = FALSE, col = 8, lty = 2)
lines(ressens.brader$rho, d.avg.brader, lwd = 2)
abline(h=0)
abline(h=resmed.brader$d.avg, lty = "dashed")
abline(v=0)

# 3. R2
x <- ressens.brader
R2Mstar <- seq(0, 1-x$rho.by, 0.01)
R2Ystar <- seq(0, 1-x$rho.by, 0.01)
R2M <- R2Mtilde <- (1-x$r.square.m)*seq(0, 1-x$rho.by, 0.01)
R2Y <- R2Ytilde <- (1-x$r.square.y)*seq(0, 1-x$rho.by, 0.01)

dlength <- length(seq(0, (1-x$rho.by)^2, 0.0001))
R2prod.mat <- outer(R2Mstar, R2Ystar)
Rprod.mat <- round(sqrt(R2prod.mat), digits=4)

levels0 <- pretty(quantile(d.avg.brader, probs=c(0.1,0.9)), 10)
d0.p <- approx(d.avg.brader[((length(d.avg.brader)+1)/2):length(d.avg.brader)], n=dlength)$y
d0mat.p <- matrix(d0.p[Rprod.mat/0.0001+1], nrow=length(R2M))
contour(R2M, R2Y, d0mat.p, levels=levels0, xlim = c(0,1), ylim = c(0,1), lwd = 1,
        main = "Sensitivity with Respect to \n Proportion of Variance Explained",
        xlab = "", ylab = "", asp = 1)
contour(R2M, R2Y, d0mat.p, levels=0, lwd = 2, add=TRUE, drawlabels=FALSE)
title(xlab=expression(paste(tilde(R)[M]^{2})), line=2.5, cex.lab=.9)
title(ylab=expression(paste(tilde(R)[Y]^2)), line=2.5, cex.lab=.9)
axis(2,at=seq(0,1,by=.1))
axis(1,at=seq(0,1,by=.1))

title(main = "Brader, Valentino & Suhay (2008)", cex.main = 1.5, outer = T)

dev.off()


#############################################################################
# Figure 3: Analysis without the independence assumption
#############################################################################

Xnames.slo <- c("gender", "educ", "educmiss", "polint", "polintmiss", "ideo", "ideomiss",
            "school", "birth", "know", "value")
res.slo <- multimed("Y", "M1", "W1", "ttt", Xnames.slo, 
                    data=Slothuus, sims=nboot, conf.level=clevel)

Xnames.druck <- c("year", "age", "sex", "ethnic", "knowdum", "ondum", "pid", "ideology")
res.druck <- multimed("Y", "M", "W", "T", Xnames.druck, 
                    data=DruckNels, sims=nboot, conf.level=clevel)

Xnames.brader <- c("ppage", "ppeducat", "ppgender", "ppincimp")
res.brader <- multimed("immigr", "emo", "p_harm", "tone_eth", Xnames.brader, 
                    data=Brader, sims=nboot, conf.level=clevel)


## Druckman
pdf("medsens-druck.pdf", width=3.25, height=9.75)
par(mfcol=c(3,1), mar=c(5,5,4,0.25), oma=c(2,2,3,0.5))

lims <- c(-0.7,1.6)

# 1. mediate2
plot(0, 0, type="n", main="Point Estimates",
     xlab = "Average Causal Mediation Effects", ylab = "",
     xlim = lims, ylim = c(0.5, 4.5), yaxt="n")
segments(res.druck$d.ave.ci[1,1], 4, res.druck$d.ave.ci[2,1], 4, lwd=2)
segments(res.druck$d1.ci[1,1], 3, res.druck$d1.ci[2,1], 3, lwd=2)
segments(res.druck$d0.ci[1,1], 2, res.druck$d0.ci[2,1], 2, lwd=2)
segments(res.druck$tau.ci[1], 1, res.druck$tau.ci[2], 1, lwd=2)
points(res.druck$d.ave.ub[1], 4, pch=18, cex=2)
points(res.druck$d1.ub[1], 3, pch=18, cex=2)
points(res.druck$d0.ub[1], 2, pch=18, cex=2)
points(res.druck$tau, 1, pch=18, cex=2)
abline(v=0)
axis(side = 2, labels = c(
  expression(paste("Average (", bar(bar(delta)), ")")),
  expression(paste("Treated (", bar(delta)[1], ")")),
  expression(paste("Control (", bar(delta)[0], ")")),
  expression(paste("Total (", bar(tau), ")"))),
     at = c(4,3,2,1), las = 2)

# 2. sigma
plot(0,0, type="n", main="Sensitivity with Respect to \n Interaction Heterogeneity", 
     xlab = expression(paste(sigma)), 
     ylab = expression(paste(bar(bar(delta)), "(", sigma, ")")), 
     xlim = range(res.druck$sigma), ylim = lims)
polygon(c(res.druck$sigma, rev(res.druck$sigma)),
        c(res.druck$d.ave.ci[1,], rev(res.druck$d.ave.ci[2,])),
        border = FALSE, col = 8, lty = 2)
lines(res.druck$sigma, res.druck$d.ave.lb, lwd = 2)
lines(res.druck$sigma, res.druck$d.ave.ub, lwd = 2)
abline(h=0)
abline(h=res.druck$d.ave.ub[1], lty="dashed")

# 3. R2
plot(0,0, type="n", main="Sensitivity with Respect to \n Importance of Interaction", 
     xlab = expression(paste(tilde(R)^{2})), 
     ylab = expression(paste(bar(bar(delta)), "(", tilde(R)^{2}, ")")), 
     xlim = c(0,1), ylim = lims)
polygon(c(res.druck$R2tilde, rev(res.druck$R2tilde)),
        c(res.druck$d.ave.ci[1,], rev(res.druck$d.ave.ci[2,])),
        border = FALSE, col = 8, lty = 2)
lines(res.druck$R2tilde, res.druck$d.ave.lb, lwd = 2)
lines(res.druck$R2tilde, res.druck$d.ave.ub, lwd = 2)
abline(h=0)
abline(h=res.druck$d.ave.ub[1], lty="dashed")

title(main = "Druckman & Nelson (2003)", cex.main = 1.5, outer = T)

dev.off()


## Slothuus
pdf("medsens-slo.pdf", width=3.25, height=9.75)
par(mfcol=c(3,1), mar=c(5,5,4,0.25), oma=c(2,2,3,0.5))

lims <- c(-0.7, 1.6)

# 1. mediate2
plot(0, 0, type="n", main="Point Estimates",
     xlab = "Average Causal Mediation Effects", ylab = "",
     xlim = lims, ylim = c(0.5, 4.5), yaxt="n")
segments(res.slo$d.ave.ci[1,1], 4, res.slo$d.ave.ci[2,1], 4, lwd=2)
segments(res.slo$d1.ci[1,1], 3, res.slo$d1.ci[2,1], 3, lwd=2)
segments(res.slo$d0.ci[1,1], 2, res.slo$d0.ci[2,1], 2, lwd=2)
segments(res.slo$tau.ci[1], 1, res.slo$tau.ci[2], 1, lwd=2)
points(res.slo$d.ave.ub[1], 4, pch=18, cex=2)
points(res.slo$d1.ub[1], 3, pch=18, cex=2)
points(res.slo$d0.ub[1], 2, pch=18, cex=2)
points(res.slo$tau, 1, pch=18, cex=2)
abline(v=0)
axis(side = 2, labels = c(
  expression(paste("Average (", bar(bar(delta)), ")")),
  expression(paste("Treated (", bar(delta)[1], ")")),
  expression(paste("Control (", bar(delta)[0], ")")),
  expression(paste("Total (", bar(tau), ")"))),
     at = c(4,3,2,1), las = 2)

# 2. sigma
plot(0,0, type="n", main="Sensitivity with Respect to \n Interaction Heterogeneity", 
     xlab = expression(paste(sigma)), 
     ylab = expression(paste(bar(bar(delta)), "(", sigma, ")")), 
     xlim = range(res.slo$sigma), ylim = lims)
polygon(c(res.slo$sigma, rev(res.slo$sigma)),
        c(res.slo$d.ave.ci[1,], rev(res.slo$d.ave.ci[2,])),
        border = FALSE, col = 8, lty = 2)
lines(res.slo$sigma, res.slo$d.ave.lb, lwd = 2)
lines(res.slo$sigma, res.slo$d.ave.ub, lwd = 2)
abline(h=0)
abline(h=res.slo$d.ave.ub[1], lty="dashed")

# 3. R2
plot(0,0, type="n", main="Sensitivity with Respect to \n Importance of Interaction", 
     xlab = expression(paste(tilde(R)^{2})), 
     ylab = expression(paste(bar(bar(delta)), "(", tilde(R)^{2}, ")")), 
     xlim = c(0,1), ylim = lims)
polygon(c(res.slo$R2tilde, rev(res.slo$R2tilde)),
        c(res.slo$d.ave.ci[1,], rev(res.slo$d.ave.ci[2,])),
        border = FALSE, col = 8, lty = 2)
lines(res.slo$R2tilde, res.slo$d.ave.lb, lwd = 2)
lines(res.slo$R2tilde, res.slo$d.ave.ub, lwd = 2)
abline(h=0)
abline(h=res.slo$d.ave.ub[1], lty="dashed")

title(main = "Slothuus (2008)", cex.main = 1.5, outer = T)

dev.off()



## Brader
pdf("medsens-brader.pdf", width=3.25, height=9.75)
par(mfcol=c(3,1), mar=c(5,5,4,0.25), oma=c(2,2,3,0.5))

lims <- c(-0.48, 0.66)

# 1. mediate2
plot(0, 0, type="n", main="Point Estimates",
     xlab = "Average Causal Mediation Effects", ylab = "",
     xlim = lims, ylim = c(0.5, 4.5), yaxt="n")
segments(res.brader$d.ave.ci[1,1], 4, res.brader$d.ave.ci[2,1], 4, lwd=2)
segments(res.brader$d1.ci[1,1], 3, res.brader$d1.ci[2,1], 3, lwd=2)
segments(res.brader$d0.ci[1,1], 2, res.brader$d0.ci[2,1], 2, lwd=2)
segments(res.brader$tau.ci[1], 1, res.brader$tau.ci[2], 1, lwd=2)
points(res.brader$d.ave.ub[1], 4, pch=18, cex=2)
points(res.brader$d1.ub[1], 3, pch=18, cex=2)
points(res.brader$d0.ub[1], 2, pch=18, cex=2)
points(res.brader$tau, 1, pch=18, cex=2)
abline(v=0)
axis(side = 2, labels = c(
  expression(paste("Average (", bar(bar(delta)), ")")),
  expression(paste("Treated (", bar(delta)[1], ")")),
  expression(paste("Control (", bar(delta)[0], ")")),
  expression(paste("Total (", bar(tau), ")"))),
     at = c(4,3,2,1), las = 2)

# 2. sigma
plot(0,0, type="n", main="Sensitivity with Respect to \n Interaction Heterogeneity", 
     xlab = expression(paste(sigma)), 
     ylab = expression(paste(bar(bar(delta)), "(", sigma, ")")), 
     xlim = range(res.brader$sigma), ylim = lims)
polygon(c(res.brader$sigma, rev(res.brader$sigma)),
        c(res.brader$d.ave.ci[1,], rev(res.brader$d.ave.ci[2,])),
        border = FALSE, col = 8, lty = 2)
lines(res.brader$sigma, res.brader$d.ave.lb, lwd = 2)
lines(res.brader$sigma, res.brader$d.ave.ub, lwd = 2)
abline(h=0)
abline(h=res.brader$d.ave.ub[1], lty="dashed")

# 3. R2
plot(0,0, type="n", main="Sensitivity with Respect to \n Importance of Interaction", 
     xlab = expression(paste(tilde(R)^{2})), 
     ylab = expression(paste(bar(bar(delta)), "(", tilde(R)^{2}, ")")), 
     xlim = c(0,1), ylim = lims)
polygon(c(res.brader$R2tilde, rev(res.brader$R2tilde)),
        c(res.brader$d.ave.ci[1,], rev(res.brader$d.ave.ci[2,])),
        border = FALSE, col = 8, lty = 2)
lines(res.brader$R2tilde, res.brader$d.ave.lb, lwd = 2)
lines(res.brader$R2tilde, res.brader$d.ave.ub, lwd = 2)
abline(h=0)
abline(h=res.brader$d.ave.ub[1], lty="dashed")

title(main = "Brader, Valentino and Suhay (2008)", cex.main = 1.5, outer = T)

dev.off()


############################################################################
# Diagnostics reported in Section 4
############################################################################

### Diagnostics for independence between M and W
## Druckman
model.m.diag.druck <- lm(M ~ W + T + year + age + sex + ethnic + knowdum + ondum + pid + ideology,
                         data = DruckNels)
summary(model.m.diag.druck)


## Slothuus
model.m.diag.slo <- lm(M1 ~ W1 + ttt + gender + educ + educmiss + polint + polintmiss 
                       + ideo + ideomiss + school + birth + know + value, data = Slothuus)
summary(model.m.diag.slo)

## Brader
model.m.diag.brader <- lm(emo ~ p_harm + tone_eth + ppage + ppeducat + ppgender + ppincimp,
                          data = Brader)
summary(model.m.diag.brader)


### Diagnostics for independence between W and T
## Druckman
model.m.diag.druck <- lm(W ~ T + year + age + sex + ethnic + knowdum + ondum + pid + ideology,
                         data = DruckNels)
summary(model.m.diag.druck)


## Slothuus
model.m.diag.slo <- lm(W1 ~ ttt + gender + educ + educmiss + polint + polintmiss 
                      + ideo + ideomiss + school + birth + know + value, data = Slothuus)
summary(model.m.diag.slo)

## Brader
model.m.diag.brader <- lm(p_harm ~ tone_eth + ppage + ppeducat + ppgender + ppincimp,
                          data = Brader)
summary(model.m.diag.brader)
