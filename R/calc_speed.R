
library(signal)
library(magrittr)
# signal, magrittr, dplyr

# fix sudden 'jumps' caused by suboptimal amp->pos conversion
# we could try to guess thr by using percentiles etc
myfixeddiff <- function(x, med=19, thr=5) {
  dx=diff(x)
  rm=runmed(dx,med)
  dx[abs(dx)>thr]=rm[abs(dx)>thr]
  diffinv(dx)+x[1]
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
#load('data/0010raw.RData')

from=1
to=6300

NR_x_trace_s = myfilter(myfixeddiff(sweep$Ch9_X,19,5))
NR_y_trace_s = myfilter(myfixeddiff(sweep$Ch9_Y,19,5))
NR_z_trace_s = myfilter(myfixeddiff(sweep$Ch9_Z,19,5))

NR_speed=(
  +mydiff(NR_x_trace_s)^2 # f-b
  +mydiff(NR_y_trace_s)^2 # l-r
  +mydiff(NR_z_trace_s)^2 # u-d
  )^.5
plot(NR_speed[from:to],type="l")

