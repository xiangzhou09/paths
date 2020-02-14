############################################################################
# REPLICATION R CODE
# TITLE: "THE LEGACY OF POLITICAL VIOLENCE ACROSS GENERATIONS"
# AUTHORS: NOAM LUPU AND LEONID PEISAKHIN
# JOURNAL: AMERICAN JOURNAL OF POLITICAL SCIENCE
############################################################################

library(foreign)

# Set working directory to folder with replication files
setwd("Place path to data files here")


############################################################################
# FIGURE 1
############################################################################

figure1 <- read.dta(file="Figure1.dta")
attach(figure1)

y.axis <- rep(1:4)
labels <- c("Pre-Soviet wealth", "Dekulakized", "Pro-Soviet", "Pre-Soviet religiosity")
labels.rev <- rev(labels)


jpeg(file="figure1.jpg", width=5, height=3, pointsize=9, units="in", res=600)
par(mar=c(5, 10, 2, 2) + 0.1)
plot(c(-120,120),c(0.5,4.5),type="n",axes=F,
     ylim=range(0.5:4.5),
     xlim=range(-120:120),
     ylab="",
     xlab="Effect on victimization (in SD)",
)
box()
axis(1, at=seq(-120,120,by=40), labels=c(-1.2,-0.8,-0.4,0,0.4,0.8,1.2))
axis(2, at=seq(1,4), las=2, labels=labels.rev)
segments(rev(lo)*100, y.axis, rev(hi)*100, y.axis)
points(rev(interq_effect)*100, y.axis, pch=21, bg=rev(pch))
abline(v=0, lty=3)
dev.off()



############################################################################
# FIGURE 2
############################################################################

figure2 <- read.dta(file="Figure2.dta")
attach(figure2)

y.axis <- rep(1:16)
labels <- c("In-group attachment","Victimhood","Threat perception","","Support for radical Islam","Religiosity","","Support for CT leaders","Celebrate CT holiday","","Support for Chechen rebels","Support for annexation","Pro-Russia vote choice","","Turnout","Willingness to participate")
labels.rev <- rev(labels)


jpeg(file="figure2.jpg", width=6.5, height=6, pointsize=9, units="in", res=600)
par(mar=c(5, 12, 2, 2) + 0.1)
plot(c(-50,70),c(0.5,16.5),type="n",axes=F,
     ylim=range(0.5:16.5),
     xlim=range(-50:70),
     ylab="",
     xlab="Effect of victimization (in SD)",
)
box()
axis(1, at=seq(-50,70,by=10), labels=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7))
axis(2, at=seq(1,16), las=2, labels=labels.rev)
segments(rev(lo)*100, y.axis, rev(hi)*100, y.axis)
points(rev(interq_effect)*100, y.axis, pch=21, bg=rev(pch))
abline(v=0, lty=3)
dev.off()


############################################################################
# FIGURE 3
############################################################################

figure3 <- read.dta(file="Figure3.dta")
attach(figure3)

y.axis <- rep(1:3)
labels <- c("In-group attachment","Victimhood","Threat perception")
labels.rev <- rev(labels)


jpeg(file="figure3.jpg", width=5, height=3, pointsize=9, units="in", res=600)
par(mar=c(5, 12, 2, 2) + 0.1)
plot(c(-50,70),c(0.5,3.5),type="n",axes=F,
     ylim=range(0.5:3.5),
     xlim=range(-50:70),
     ylab="",
     xlab="Effect of victimization (in SD)",
)
box()
axis(1, at=seq(-50,70,by=10), labels=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7))
axis(2, at=seq(1,3), las=2, labels=labels.rev)
segments(rev(lo)*100, y.axis, rev(hi)*100, y.axis)
points(rev(interq_effect)*100, y.axis, pch=21, bg=rev(pch))
abline(v=0, lty=3)
dev.off()


############################################################################
# FIGURE 4
############################################################################

figure4 <- read.dta(file="Figure4.dta")
attach(figure4)

y.axis <- rep(1:11)
labels <- c("1G-2G: In-group attachment","1G-2G: Victimhood","1G-2G: Threat perception","","2G-3G: In-group attachment","2G-3G: Victimhood","2G-3G: Threat perception","","1G-3G: In-group attachment","1G-3G: Victimhood","1G-3G: Threat perception")
labels.rev <- rev(labels)


jpeg(file="figure4.jpg", width=5, height=5, pointsize=9, units="in", res=600)
par(mar=c(5, 12, 2, 2) + 0.1)
plot(c(0,70),c(0.5,11.5),type="n",axes=F,
     ylim=range(0.5:11.5),
     xlim=range(0:70),
     ylab="",
     xlab="Proportion of response similarity",
)
box()
axis(1, at=seq(0,70,by=10), labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7))
axis(2, at=seq(1,11), las=2, labels=labels.rev)
segments(rev(lo)*100, y.axis, rev(hi)*100, y.axis)
points(rev(interq_effect)*100, y.axis, pch=21, bg=rev(pch))
dev.off()


############################################################################
# FIGURE 5
############################################################################


figure5.tatartrust <- read.dta(file="Figure5_tatartrust.dta")
figure5.vicrussia <- read.dta(file="Figure5_vicrussia.dta")
figure5.fear <- read.dta(file="Figure5_fear.dta")
figure5.radicalislam <- read.dta(file="Figure5_radicalislam.dta")
figure5.religiosity <- read.dta(file="Figure5_religiosity.dta")
figure5.tatarpols <- read.dta(file="Figure5_tatarpols.dta")
figure5.flagday <- read.dta(file="Figure5_flagday.dta")
figure5.chechens <- read.dta(file="Figure5_chechens.dta")
figure5.annex <- read.dta(file="Figure5_annex.dta")
figure5.rsupport <- read.dta(file="Figure5_rsupport.dta")
figure5.turnout <- read.dta(file="Figure5_turnout.dta")
figure5.participate <- read.dta(file="Figure5_participate.dta")


jpeg(file="figure5.jpg", width=6.5, height=8, pointsize=9, units="in", res=600)

par(mfrow=c(4,3))
par(mar=c(4, 6, 3, 0) + 0.1)

attach(figure5.tatartrust)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="Effect of ancestor\n victimization (in SD)",
     xlab="",
     main="In-group attachment",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.vicrussia)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="",
     xlab="",
     main="Victimhood",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.fear)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="",
     xlab="",
     main="Threat perception"
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.radicalislam)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="Effect of ancestor\n victimization (in SD)",
     xlab="",
     main="Radical Islam",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.religiosity)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="",
     xlab="",
     main="Religiosity",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.tatarpols)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="",
     xlab="",
     main="Support for CT leaders",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.flagday)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="Effect of ancestor\n victimization (in SD)",
     xlab="",
     main="Celebrate CT holiday",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.chechens)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="",
     xlab="",
     main="Support Chechen rebels",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.annex)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="",
     xlab="",
     main="Support annexation",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.rsupport)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="Effect of ancestor\n victimization (in SD)",
     xlab="Family Discussion",
     main="Pro-Russia vote choice",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.turnout)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="",
     xlab="Family Discussion",
     main="Turnout",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

attach(figure5.participate)
plot(c(1,4),c(-5.5,5.5),type="n",axes=F,
     ylim=range(-5.5:5.5),
     xlim=range(1:4),
     ylab="",
     xlab="Family Discussion",
     main="Willingness to participate",
)
box()
axis(1, at=seq(1,4,by=1))
axis(2, at=seq(-5,5,by=2.5), labels=c(-0.5,-0.25,0,0.25,0.5))
polygon(c(at,rev(at)),c(lo*10,rev(hi*10)),col="grey",border=NA)
segments(1,pe[1]*10,4,pe[301]*10)
abline(h=0,lty=3)

dev.off()


# END