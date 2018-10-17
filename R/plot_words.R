library(ggplot2)
library(ggthemes)
library(lme4)
library(dplyr)
library(officer)
library(rvg)

#d<-read.table('mean_av_29p.txt',sep=" ")
d<-read.table('mean_av_166f.txt',sep=" ")
d<-read.table('mean_zv_166f.txt',sep=" ")
d<-read.table('mean_sav_durs_166f.txt',sep=" ")
colnames(d)<-c("subject","allwords","av", "w_dur", "phr1_dur", "phr2_dur", "tw1_dur", "pau_dur", "tw2_dur", "tw1_av", "tw2_av", "dum")
#d<-read.table('mean_sagAng.txt',sep=" ")
#d<-read.table('mean_speed.txt',sep=" ")
#d<-read.table('mean_xzspeed.txt',sep=" ")
#d<-read.table('mean_relspeed.txt',sep=" ")
agg<-aggregate(list(Mean = d$av),list(Word = d$allwords), length)

#s1<-agg[c(13,14,17,8,3,5,7,16),]
#rownames(s1)<-NULL
#ggplot(s1, aes(x = reorder(Word,c(1,2,3,4,5,6,7,8)), y = Mean)) + geom_bar(stat = "identity")

#s2<-agg[c(8,18,4,19,15,12,9,11),]
#rownames(s2)<-NULL
#ggplot(s2, aes(x = reorder(Word,c(1,2,3,4,5,6,7,8)), y = Mean)) + geom_bar(stat = "identity")

# fluga - både
d$words <- factor(as.character(d$allwords),
               levels=c("Mobiltelefonen", "nittitalets", "stora", "fluga,", "både", "bland",
                        "företagare", "privatpersoner."))

e<-subset(d, words!="")
ggplot(e, aes(x = words, y = av)) +
    #geom_tufteboxplot() +
    geom_boxplot() +
    scale_y_continuous(name="mean angular velocity (rad/s)") +
    scale_x_discrete(name="word") +
    theme_bw()
    # theme_fivethirtyeight() +
    # theme(
    #     axis.title = element_text()
    # )
ggsave("img/sent1.ps")
    
w45<-droplevels(subset(d, words %in% c("fluga,", "både")))
#w45<-droplevels(subset(d, words %in% c("Mobiltelefonen", "nittitalets")))

w45.null = lmer(av ~ (1+words|subject), data=w45, REML = FALSE)
w45.model = lmer(av ~ words + (1+words|subject), data=w45, REML = FALSE)
anova(w45.null,w45.model)

#w45.model = lmer(av ~ words + (1|subject), data=w45, REML = FALSE)
#Anova(w45.model)

xmdl <- lm(av ~ words, w45)
summary(xmdl)

# (24p: word affected mav (χ2 (1)=27.005, p=0.0000002029), lowering it by about 0.1 rad/s ± 0.018 (standard errors))
# (29p: word affected mav (χ2 (1)=12.174, p=0.0004845), lowering it by about 0.1 rad/s ± 0.02 (standard errors))
# fluga - både
# (166f: word affected mav (χ2 (1)=8.5201, p=0.003512), lowering it by about 0.077 rad/s ± 0.017 (standard errors)))
# stora - fluga
# (166f: word affected mav (χ2 (1)=8.4946, p=0.003562), increasing it by about 0.077 rad/s ± 0.017 (standard errors)))
# Mob. - nitt.
# (166f: word affected mav (χ2 (1)=5.8811, p=0.0153), lowering it by about 0.043 rad/s ± 0.012 (standard errors)))


# flyget - tåget
d$words <- factor(as.character(d$allwords),
                  levels=c("Flyget", "tåget", "bilbranschen", "tävlar", "om", "lönsamhet",
                           "folkets", "gunst."))

e<-subset(d, words!="")
ggplot(e, aes(x = words, y = av)) + geom_boxplot() +
    scale_y_continuous(name="mean angular velocity (rad/s)") +
    scale_x_discrete(name="word")

w12<-droplevels(subset(d, words %in% c("tävlar", "om")))

w12.null = lmer(av ~ (1+words|subject), data=w12, REML = FALSE)
w12.model = lmer(av ~ words + (1+words|subject), data=w12, REML = FALSE)
anova(w12.null,w12.model)

#w12.model = lmer(av ~ words + (1|subject), data=w12, REML = FALSE)
#Anova(w12.model)

xmdl <- lm(av ~ words, w12)
summary(xmdl)

# (24p: word affected mav (χ2 (1)=5.2002, p=0.02258), lowering it by about 0.05 rad/s ± 0.02 (standard errors))
# (29p: word affected mav (χ2 (1)=3.9684, p=0.04636), lowering it by about 0.05 rad/s ± 0.02 (standard errors))
# flyget - tåget
# (166f: word affected mav (χ2 (1)=3.913, p=0.04792), lowering it by about 0.043 rad/s ± 0.017 (standard errors))
# tävlar - om
# (166f: word affected mav (χ2 (1)=4.3803, p=0.03636), lowering it by about 0.032 rad/s ± 0.012 (standard errors))


w4<-subset(d, allwords=="fluga,")
w5<-subset(d, allwords=="både")
t.test(w4$av,w5$av)

w1<-subset(d, allwords=="tävlar")
w2<-subset(d, allwords=="om")
t.test(w1$av,w2$av)


tiff("Fig1.tif", width = 747, height = 400, res = 300, compression = 'lzw')
ggplot(e, aes(x = words, y = av)) + geom_boxplot() +
    scale_y_continuous(name="mean angular velocity (rad/s)") +
    scale_x_discrete(name="word")
dev.off()

bitmap('Fig1.bmp', height = 747, width = 400, units = 'px', type="tifflzw", res=600)


with (w45, plot(words, av))
mapply(abline,
       a=coef(w45.model)$subject$`(Intercept)`-coef(w45.model)$subject$wordsbåde,
       b=coef(w45.model)$subject$wordsbåde)
abline(coef(w45.model)$subject$`(Intercept)`[1],coef(w45.model)$subject$wordsbåde[1])

with (w12, plot(words, av))
mapply(abline,a=coef(w12.model)$subject$`(Intercept)`,b=coef(w12.model)$subject$wordståget)
sjp.lmer(w45.model, type="rs.ri")


# https://stackoverflow.com/questions/48008199/how-can-i-produce-an-estimate-for-a-changing-variable-using-lme4-nlme
# https://mvuorre.github.io/post/2016/order-ggplot-panel-plots/

fm1 <- lmer(av~words+(1+words|subject),w45)
d1 <- coef(fm1)$subject
d1$subject <- rownames(d1)

fm2 <- lmList(av~words|subject,w45)
d2 <- coef(fm2)
d2$subject <- rownames(d2)

w45 <- left_join(w45, d1, by="subject")
w45 <- mutate(w45, subject = reorder(subject, -wordsbåde))
d1 <- mutate(d1, subject = reorder(subject, -wordsbåde))
d2 <- mutate(d2, subject = reorder(subject, -wordsbåde))

theme_set(theme_bw())
ggplot(w45,aes(words,av, colour=subject))+
    geom_point()+
    geom_abline(data=d1,
                mapping=aes(intercept=`(Intercept)`-wordsbåde,
                            slope=wordsbåde,colour=subject)) + 
    geom_abline(data=d2,linetype=2,
                mapping=aes(intercept=`(Intercept)`-wordsbåde,
                            slope=wordsbåde,colour=subject)) +
    facet_wrap("subject")


fm1 <- lmer(av~words+(1+words|subject),w12)
d1 <- coef(fm1)$subject
d1$subject <- rownames(d1)

fm2 <- lmList(av~words|subject,w12)
d2 <- coef(fm2)
d2$subject <- rownames(d2)

w12 <- left_join(w12, d1, by="subject")
w12 <- mutate(w12, subject = reorder(subject, -wordståget))
d1 <- mutate(d1, subject = reorder(subject, -wordståget))
d2 <- mutate(d2, subject = reorder(subject, -wordståget))

ggplot(w12,aes(words,av, colour=subject))+
    geom_point()+
    geom_abline(data=d1,
                mapping=aes(intercept=`(Intercept)`-wordståget,
                            slope=wordståget,colour=subject)) + 
    geom_abline(data=d2,linetype=2,
                mapping=aes(intercept=`(Intercept)`-wordståget,
                            slope=wordståget,colour=subject)) +
    facet_wrap("subject")

# correlation

w4<-droplevels(subset(d, words %in% c("fluga,")))

xmdl <- lm((tw1_av-tw2_av) ~ tw1_dur, w4)
summary(xmdl)

w4.model = lmer(tw1_av-tw2_av ~ tw1_dur + (1|subject), data=w4, REML = FALSE)
Anova(w4.model)

w4.null = lmer(tw1_av-tw2_av ~ (1+tw1_dur|subject), data=w4, REML = FALSE)
w4.model = lmer(tw1_av-tw2_av ~ tw1_dur + (1+tw1_dur|subject), data=w4, REML = FALSE)
anova(w4.null,w4.model)

# (166f: duration of tw1 affected mav difference (χ2 (1)=6.7684, p=0.009278),
# increasing it by about 0.7414 rad/s ± 0.3231 (standard errors)))

dat<-data.frame(w4$tw1_dur,w4$tw1_av-w4$tw2_av) # -> sig!
dat<-data.frame(w4$pau_dur,w4$tw1_av-w4$tw2_av)
dat<-data.frame(w4$tw2_dur,w4$tw1_av-w4$tw2_av) # -> sig!

names(dat) <- c("dur","av_diff")
plot(dat)
res <- resid(mod <- lm(av_diff ~ dur, data = dat))
res.qt <- quantile(res, probs = c(0.05,0.95))
want <- which(res >= res.qt[1] & res <= res.qt[2])

plot(dat, type = "n")
points(dat[-want,], col = "black", pch = 21, bg = "black", cex = 0.8)
points(dat[want,], col = "red", pch = 21, bg = "red", cex = 0.8)
abline(mod, col = "blue", lwd = 2)
title("correlation")
cor(dat)
cor(dat[want,])
cor(dat[-want,])
cor.test(dat$dur, dat$av_diff)

summary(lm(av_diff ~ dur, data = dat))


doc <- read_pptx() 
doc <- add_slide(doc, layout = "Title and Content",
                 master = "Office Theme")
doc <- ph_with_vg(doc, ggobj = gg_plot )
print(doc, target = "img/ph_with_gg.pptx" )
