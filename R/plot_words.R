d<-read.table('mean_av.txt',sep=" ")
d<-read.table('mean_sagAng.txt',sep=" ")
d<-read.table('mean_speed.txt',sep=" ")
d<-read.table('mean_xzspeed.txt',sep=" ")
d<-read.table('mean_relspeed.txt',sep=" ")
agg<-aggregate(list(Mean = d$V2),list(Word = d$V1), mean)

s1<-agg[c(13,14,17,7,3,5,10,16),]
rownames(s1)<-NULL
ggplot(s1, aes(x = reorder(Word,c(1,2,3,4,5,6,7,8)), y = Mean)) + geom_bar(stat = "identity")

s2<-agg[c(8,18,4,19,15,12,9,11),]
rownames(s2)<-NULL
ggplot(s2, aes(x = reorder(Word,c(1,2,3,4,5,6,7,8)), y = Mean)) + geom_bar(stat = "identity")

d$V3 <- factor(as.character(d$V1),
               levels=c("Mobiltelefonen", "nittitalets", "stora", "fluga,", "både", "bland",
                        "företagare", "privatpersoner."))
with(d, boxplot(V2~V3))
ggplot(d, aes(x = V3, y = V2)) + geom_boxplot()
