
library(signal)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
# signal, magrittr, dplyr

# fix sudden 'jumps' caused by suboptimal amp->pos conversion
# we could try to guess thr by using percentiles etc
myfixeddiff <- function(x, med=19, thr=5) {
  dx=diff(x)
  rm=runmed(dx,med)
  dx[abs(dx)>thr]=rm[abs(dx)>thr]
  diffinv(dx)+x[1]
}

# importance function based on Ananthakrishnan & Engwall (2011)
myimportance <- function(x) {
    #x<-rbind(NR_x_trace_s,seq(1:6300)*0.005,1)
    #x<-rbind(NR_x_trace_s,NR_y_trace_s,NR_z_trace_s)
    l=dim(x)[2]
    y=x[1,]
    vel=x[1,]
    ang=x[1,]
    
    for (i in 2:(l-1)) {
        vel[i]=sum((x[,i+1]-x[,i-1])^2)^.5
    }
    vel[1]=vel[2]
    vel[l]=vel[l-1]
    maxvel=max(vel)
    
    for (i in 2:(l-1)) {
        #a=x[,i]-x[,i-1]
        a=x[,i-1]-x[,i]
        b=x[,i]-x[,i+1]
        #b=x[,i+1]-x[,i]
        ang[i]=acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
        #if (ang[1] > (pi/2)) {
            
        #}
        #ang[i]=angle3(a,b)
    }
    ang[1]=ang[2]
    ang[l]=ang[l-1]
    maxang=max(ang)
    
    twopi=2*pi
    #y = log((ang/twopi) - (vel/maxvel))
    y = (ang/maxang) - (vel/maxvel)
    #y = -vel
    
    y[1]=y[2]
    y[l]=y[l-1]
    data.frame(imp=y, vel=vel, ang=ang)
}

# central difference
mydiff<-function(x) {
  y=x
  for (i in 2:(length(x)-1)) {
    y[i]=x[i+1]-x[i-1]
  }
  y[1]=y[2]
  y[length(x)]=y[length(x)-1]
  y
}

# filter with padding 
mybutter <- function(x, samplingRate, order, cutoffs, type, ...) {
  pre <- x[1:samplingRate]
  pre[1:samplingRate] <- x[1]
  pos <- x[1:samplingRate]
  pos[1:samplingRate] <- x[length(x)]
  x1<-c(pre,x,pos)
  y<-signal::butter(n = order, W = cutoffs/samplingRate/2, type = type) %>%
    signal::filtfilt(x1)
  y[(samplingRate+1):(length(x1)-samplingRate)]
}

# helper for filter
myfilter <- function(x) {
    mybutter(x, 200, order = 5, cutoffs = 14, type = "low")
}

#sweep = read.table("0010raw.txt",h=T,comment.char="%")
#save(sweep, file="data/0010raw.RData")
load('data/0010raw.RData')

from=2400
to=3600

NR_x_trace_s = myfilter(myfixeddiff(sweep$Ch10_X,19,5))
NR_y_trace_s = myfilter(myfixeddiff(sweep$Ch10_Y,19,5))
NR_z_trace_s = myfilter(myfixeddiff(sweep$Ch10_Z,19,5))


NR_speed=(
  +mydiff(NR_x_trace_s)^2 # f-b
  +mydiff(NR_y_trace_s)^2 # l-r
  +mydiff(NR_z_trace_s)^2 # u-d
  )^.5
plot(NR_speed[from:to],type="l")

x<-seq(1:6300)
e<-rbind(NR_x_trace_s,NR_y_trace_s,NR_z_trace_s)
#e<-rbind(NR_x_trace_s,NR_z_trace_s,0)
#e<-rbind(NR_x_trace_s,x*0.005,1)
#e<-rbind(NR_x_trace_s, 0, 0)
f<-myimportance(e[1:3,])
df<-data.frame(x,e[1,],e[2,],e[3,],f$imp)
#df<-data.frame(x,f$imp,f$vel)
z <- read.zoo(df)
plot(z[from:to])
write.table(f$imp,"NR_imp.txt",col.names = "NR_imp", row.names = F)


# mark maxima
plot(df$f[from:to],type="l")
locmax<-which(diff(sign(diff(df$f[from:to])))==-2)+1
locmaxh<-df$f[locmax+from]
points(locmax,locmaxh)

add_theme <- function() {
    theme_grey() +
        theme(
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()
        )
}

add_maxima <- function(x) {
    locmax<-which(diff(sign(diff(x)))==-2)+1
    locmaxh<-x[locmax]
    annotate("pointrange", locmax, locmaxh, ymin = locmaxh, ymax = locmaxh)
}

p1 <- ggplot(melt(data.frame(x, f$ang), id.vars = 'x'), aes(x = x, y = value)) + 
    geom_line() + 
    coord_cartesian(xlim = c(from, to)) +
    add_maxima(f$ang) +
    facet_grid(~ variable, scales = 'free_y') +
    add_theme()
p2 <- ggplot(melt(data.frame(x, f$vel), id.vars = 'x'), aes(x = x, y = value)) + 
    geom_line() + 
    coord_cartesian(xlim = c(from, to)) +
    add_maxima(f$vel) +
    facet_grid(~ variable, scales = 'free_y') +
    add_theme()
p3 <- ggplot(melt(data.frame(x, f$imp), id.vars = 'x'), aes(x = x, y = value)) + 
    geom_line() + 
    coord_cartesian(xlim = c(from, to)) +
    add_maxima(f$imp) +
    facet_grid(~ variable, scales = 'free_y') +
    add_theme()

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g <- rbind(g1, g2, g3, size= "first")

grid.arrange(
    grobs = list(g)
)

