
mydiff<-function(x) {
  y=x
  for (i in 2:(length(x)-1)) {
    y[i]=x[i+1]-x[i-1]
  }
  y[1]=y[2]
  y[length(x)]=y[length(x)-1]
  y
}

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

myfilter <- function(x) {
  mybutter(x, 200, order = 5, cutoffs = 20, type = "low")
}

#sweep = read.table("0010raw.txt",h=T,comment.char="%")
#save(sweep, file="data/0010raw.RData")
load('data/0010raw.RData')

NR_x_trace_s = myfilter(sweep$Ch9_X)
NR_y_trace_s = myfilter(sweep$Ch9_Y)
NR_z_trace_s = myfilter(sweep$Ch9_Z)

NR_speed=(mydiff(NR_x_trace_s)^2+mydiff(NR_y_trace_s)^2+mydiff(NR_z_trace_s)^2)^.5
plot(NR_speed,type="l")
