library(mgcv)
library(itsadug)

m1 <- bam(Pos ~ Acc_type, data=progest_df, methods="fREML")
(smry1 <- summary(m1))

progest_keep1<-progest_df
progest_keep2<-progest_df_scbysp

progest_keep<-progest_df_scbysp

progest_df<-progest_keep[progest_keep$Speaker == "GH",]
progest_df<-progest_df_scbysp

#m2 <- bam(Pos ~ Loc + s(Time, by=Loc), data=progest_df)
#(smry2 <- summary(m2))
#gam.check(m2)

#plot(m2)
#plot_smooth(m2, view="Time", plot_all="Loc", rug=FALSE)
plot_diff(m6, view="Time", comp=list(Loc=c('2','3')))

m2 <- bam(Pos ~ Acc_type + s(Time, by=Acc_type), data=progest_df_scbysp)
(smry2 <- summary(m2))
plot_smooth(m2, view="Time", plot_all="Acc_type", rug=FALSE)

m2 <- bam(Pos ~ Loc + s(Time, by=Loc), data=progest_df)
(smry2 <- summary(m2))
plot_smooth(m2, view="Time", plot_all="Loc", rug=FALSE)

# random intercept
m3 <- bam(Pos ~ Loc + s(Time, by=Loc) + s(Speaker,bs="re"), data=progest_df)
(smry3 <- summary(m3))
plot_smooth(m3, view="Time", plot_all="Loc", rug=FALSE, rm.ranef = TRUE)

# random slope
m4 <- bam(Pos ~ Loc + s(Time, by=Loc) + s(Speaker,bs="re") + s(Speaker,Loc,bs="re"), data=progest_df)
(smry4 <- summary(m4))
plot_smooth(m4, view="Time", plot_all="Loc", rug=FALSE, rm.ranef = TRUE)

# non-linear random effects
m5 <- bam(Pos ~ Loc + s(Time, by=Loc) + s(Speaker,Loc,bs="re") + s(Time,Speaker,bs="fs",m=1), data=progest_df)
(smry5 <- summary(m5))
plot_smooth(m5, view="Time", plot_all="Loc", rug=FALSE, rm.ranef = TRUE)

m6 <- bam(Pos ~ Loc + s(Time, by=Loc) + s(Time,Speaker,by=Loc,bs="fs",m=1), data=progest_df)
(smry6 <- summary(m6))
plot_smooth(m6, view="Time", plot_all="Loc", rug=FALSE, rm.ranef = TRUE)
