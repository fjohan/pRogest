
# some difference plots

from=1
to=6300

NR_RE_diff=((sweep$Ch9_X[from:to]-sweep$Ch10_X[from:to])^2+
              (sweep$Ch9_Y[from:to]-sweep$Ch10_Y[from:to])^2+
              (sweep$Ch9_Z[from:to]-sweep$Ch10_Z[from:to])^2)^.5
plot(NR_RE_diff,type="l")

NR_diff=(mydiff(sweep$Ch9_X[from:to])^2+
           mydiff(sweep$Ch9_Y[from:to])^2+
           mydiff(sweep$Ch9_Z[from:to])^2)^.5
plot(NR_diff,type="l")

RE_diff=(mydiff(sweep$Ch10_X[from:to])^2+
           mydiff(sweep$Ch10_Y[from:to])^2+
           mydiff(sweep$Ch10_Z[from:to])^2)^.5
plot(RE_diff,type="l")

# individual axes
NR_speed=(mydiff(sweep$Ch9_X[from:to])^2)^.5
plot(NR_speed,type="l")

NR_speed=(mydiff(sweep$Ch9_Y[from:to])^2)^.5
plot(NR_speed,type="l")

NR_speed=(mydiff(sweep$Ch9_Z[from:to])^2)^.5
plot(NR_speed,type="l")

plot(c(
  mydiff(myfilter(sweep$Ch9_X))
  ,mydiff(myfilter(sweep$Ch9_Y))
  ,mydiff(myfilter(sweep$Ch9_Z))
),type="l")

plot(c(
  myfilter(sweep$Ch9_X)
  ,myfilter(sweep$Ch9_Y)
  ,myfilter(sweep$Ch9_Z)
),type="l")

plot(c(
  mydiff(sweep$Ch9_X)
  ,mydiff(sweep$Ch9_Y)
  ,mydiff(sweep$Ch9_Z)
),type="l")

# compare use of myfixeddiff
NR_x_trace_s = myfilter(sweep$Ch9_X)
NR_y_trace_s = myfilter(sweep$Ch9_Y)
NR_z_trace_s = myfilter(sweep$Ch9_Z)

NR_speed=(mydiff(NR_x_trace_s)^2+mydiff(NR_y_trace_s)^2+mydiff(NR_z_trace_s)^2)^.5
plot(NR_speed,type="l")

NR_x_trace_s = myfilter(myfixeddiff(sweep$Ch9_X,19,5))
NR_y_trace_s = myfilter(myfixeddiff(sweep$Ch9_Y,19,5))
NR_z_trace_s = myfilter(myfixeddiff(sweep$Ch9_Z,19,5))

NR_speed=(mydiff(NR_x_trace_s)^2+mydiff(NR_y_trace_s)^2+mydiff(NR_z_trace_s)^2)^.5
lines(NR_speed,col="red")
