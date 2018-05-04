library(ggplot2)

d<-read.table('mean_av_24p.txt',sep=" ")
d<-read.table('mean_sagAng.txt',sep=" ")
d<-read.table('mean_speed.txt',sep=" ")
#d<-read.table('mean_xzspeed.txt',sep=" ")
#d<-read.table('mean_relspeed.txt',sep=" ")
agg<-aggregate(list(Mean = d$V2),list(Word = d$V1), length)

s1<-agg[c(13,14,17,8,3,5,7,16),]
rownames(s1)<-NULL
ggplot(s1, aes(x = reorder(Word,c(1,2,3,4,5,6,7,8)), y = Mean)) + geom_bar(stat = "identity")

s2<-agg[c(8,18,4,19,15,12,9,11),]
rownames(s2)<-NULL
ggplot(s2, aes(x = reorder(Word,c(1,2,3,4,5,6,7,8)), y = Mean)) + geom_bar(stat = "identity")

d$V3 <- factor(as.character(d$V1),
               levels=c("Mobiltelefonen", "nittitalets", "stora", "fluga,", "både", "bland",
                        "företagare", "privatpersoner."))
d$V3 <- factor(as.character(d$V1),
               levels=c("Flyget", "tåget", "bilbranschen", "tävlar", "om", "lönsamhet",
                        "folkets", "gunst."))
#with(e, boxplot(V2~V3))
e<-subset(d, V3!="")
ggplot(e, aes(x = V3, y = V2)) + geom_boxplot() +
    scale_y_continuous(name="mean angular velocity (rad/s)") +
    scale_x_discrete(name="word")

w4<-subset(d, V3=="fluga,")
w5<-subset(d, V3=="både")
t.test(w4$V2,w5$V2)

w1<-subset(d, V3=="Flyget")
w2<-subset(d, V3=="tåget")
t.test(w1$V2,w2$V2)


tiff("Fig1.tif", width = 747, height = 400, res = 300, compression = 'lzw')
ggplot(e, aes(x = V3, y = V2)) + geom_boxplot() +
    scale_y_continuous(name="mean angular velocity (rad/s)") +
    scale_x_discrete(name="word")
dev.off()

bitmap('Fig1.bmp', height = 747, width = 400, units = 'px', type="tifflzw", res=600)
