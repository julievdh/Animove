## ---- eval=FALSE---------------------------------------------------------
## install.packages("~/Downloads/mrw.tar.gz", repos = NULL, type = "source", dependencies = TRUE)
## install.packages("~/Downloads/waddle.gz", repos = NULL, type = "source", dependencies = TRUE)

## ------------------------------------------------------------------------
require(waddle)
data(Lamprey)

# smooth track for GPS error when stationary
Lamprey.smooth = SmoothTrack(Lamprey, 3) # 3-sample moving average - no padding

## ---- echo=FALSE---------------------------------------------------------
plot.track(Lamprey, main = "Raw")
plot.track(Lamprey.smooth, main = "Smoothed")

## ---- echo=FALSE---------------------------------------------------------
plotCircle = function(x, y, radius, ...)
{
  circle = seq(from = 0, to = 2*pi, length = 200)
  lines(x + radius * cos(circle), y + radius * sin(circle), ...)
}

set.seed(533)
track = cumsum(complex(re = rnorm(100, 0, 1), im = rnorm(100, 0, 1)))
plot(track, type = "l", xlab = "x", ylab = "y", bty = "l", asp = 1)
points(track, pch = 16, cex = 1)
points(track[c(50, 95)], pch = 4, col = "red", cex = 2)
plotCircle(Re(track[95]), Im(track[95]), 5, col = "red")
plotCircle(Re(track[50]), Im(track[50]), 5, col = "red")


## ------------------------------------------------------------------------
require(adehabitatLT)

Lamprey.traj = as.ltraj(data.frame(Lamprey$X, Lamprey$Y), Lamprey$Time, 
                   id = "Lamprey")
Lamprey.smooth.traj = as.ltraj(data.frame(Lamprey.smooth$X, Lamprey.smooth$Y), Lamprey.smooth$Time, 
                   id = "Lamprey")

radii = c(5, 10, 20) # three different radii 
Lamprey.fpt = fpt(Lamprey.traj, radii) # compare FPT for raw and smoothed
Lamprey.smooth.fpt = fpt(Lamprey.smooth.traj, radii)

## ---- echo=FALSE, fig.width=15-------------------------------------------
par(mfrow = c(2, 3))
plot.fpt(Lamprey.fpt, radii, xlab = "Time", main = "Raw: ")
plot.fpt(Lamprey.smooth.fpt, radii, xlab = "Time", main = "Smooth: ")
# smoothed easier to make inference from (not surprisingly)

## ---- echo=FALSE, fig.width=15-------------------------------------------
# how many clusters should there be? 
Lamprey.lav = apply(Lamprey.fpt[[1]], 2, function(x) lavielle(na.omit(x), Lmin = 2, Kmax = 30))
Lamprey.smooth.lav = apply(Lamprey.smooth.fpt[[1]], 2, function(x) lavielle(na.omit(x), Lmin = 2, Kmax = 30))

par(mfrow = c(2, 3))
Lamprey.optseg = unlist(lapply(Lamprey.lav, function(x) { y = chooseseg(x, output = "opt")
                                                title(paste("Raw, opt seg =", y))
                                                y}))
Lamprey.smooth.optseg = unlist(lapply(Lamprey.smooth.lav, function(x) { y = chooseseg(x, output = "opt")
                                                title(paste("Smooth, opt seg =", y))
                                                y}))

## ---- echo=FALSE, fig.width=15-------------------------------------------
# put those edges on boundaries between clusters --  behavioural states
par(mfrow = c(2, 3))
temp = lapply(1:3, function(x) { findpath(Lamprey.lav[[x]], Lamprey.optseg[x])
                                 title(paste("Raw: ", radii[x])) })
temp = lapply(1:3, function(x) { findpath(Lamprey.smooth.lav[[x]], Lamprey.smooth.optseg[x])
                                 title(paste("Smooth: ", radii[x])) })

## ------------------------------------------------------------------------
# Lamprey BPMM - Bayesian Partitioned Markov Model
# first, regularize the data
Lamprey.smooth.reg = InterpolatePoints(Lamprey.smooth, 2, "min")$Data
Lamprey.smooth.traj = as.ltraj(data.frame(Lamprey.smooth.reg$X, Lamprey.smooth.reg$Y), 
                   Lamprey.smooth.reg$Time, id = "Lamprey")

## ---- echo=FALSE, fig.width=15-------------------------------------------
plot(Lamprey.smooth.traj[[1]]$date, Lamprey.smooth.traj[[1]]$dist, xlab="", ylab="Distance (m)", type="l", col="darkgrey")
points(Lamprey.smooth.traj[[1]]$date, Lamprey.smooth.traj[[1]]$dist, pch=19, cex=0.5)
# can see unequal variances -- may need to transform data
## ---- fig.width=15-------------------------------------------------------
# how many models do we need? 
Lamprey.smooth.segments = Prep.segments(Lamprey.smooth.traj, units = "min", 
                            sd = 5, nmodels = 20)

## ---- fig.width=15-------------------------------------------------------
# partition data into that many models
Lamprey.smooth.partition = Partition.segments(Lamprey.smooth.segments)
plot.segments(Lamprey.smooth.partition, 
              xlab="", ylab="Step length (m)")

## ---- fig.width=20-------------------------------------------------------
# BPMM Diagnostics - things aren't actually going that well. 
DiagPlot.segments(Lamprey.smooth.partition)
# our step lengths are not normally distributed
# the observed variance doesn't really fit the modeled variance
# and autocorrelation is still an issue... 

## ------------------------------------------------------------------------
# Lamprey BCPA - format data and set tuning parameters
Lamprey.VT = GetVT(Lamprey.smooth, units = "min")
windowsize = 30 
windowstep = 1 
K = 0.5

## ------------------------------------------------------------------------
Lamprey.ws = WindowSweep(Lamprey.VT, "V*cos(Theta)", # persistence velocity: speed*cos(turningangle)
                          windowsize, windowstep, 
                          plotme = FALSE, K = K, 
                          tau = TRUE, progress = FALSE)

## ---- fig.width=15-------------------------------------------------------
plot(Lamprey.ws, type="smooth", threshold = 2, 
     legend = FALSE) # vertical bars = where behavioural change points

## ---- fig.width=15-------------------------------------------------------
# flat BCPA
plot(Lamprey.ws, type="flat", clusterwidth = 3, 
     legend = FALSE) 

## ---- fig.width=20-------------------------------------------------------
DiagPlot(Lamprey.ws, "smooth") # smooth diagnostics

## ---- fig.width=20-------------------------------------------------------
DiagPlot(Lamprey.ws, "flat") # flat diagnostics

## ---- echo = TRUE--------------------------------------------------------
# Lamprey HMM
require(moveHMM)
Lamprey.hmm = prepData(Lamprey.smooth, type = "UTM", coordNames = c("X", "Y"))

plot(Lamprey.hmm, compact = TRUE)
summary(Lamprey.hmm)

## ---- echo = TRUE--------------------------------------------------------
summary(Lamprey.hmm)

## ---- echo = TRUE--------------------------------------------------------
weibullShape = c(3, 0.5) # values based on data - two distributions to predict step length data
weibullScale = c(10, 10)
wcauchyMu = c(0, pi) # one value is centered on zero, another is flat
wcauchyRho = c(0.7, 0.2)

doubleStateModel = fitHMM(Lamprey.hmm, nbStates = 2, 
            stepPar0 = c(weibullShape, weibullScale), 
            anglePar0 = c(wcauchyMu, wcauchyRho),
            stepDist = "weibull", angleDist = "wrpcauchy")


## ---- echo = FALSE-------------------------------------------------------
doubleStateModel # log likelihood value useful for comparing to different models

## ---- echo = FALSE-------------------------------------------------------
CI(doubleStateModel) # state 1 overlaps zero, state 2 doesn't... 

## ---- echo = FALSE, fig.width=5------------------------------------------
plot(doubleStateModel)

## ---- echo = FALSE, fig.width=16, fig.height=6---------------------------
plotStates(doubleStateModel)

## ---- echo = TRUE--------------------------------------------------------
# fit a 3-state model instead
weibullShape = c(3, 1, 1)
weibullScale = c(20, 1, 2)
wcauchyMu = c(0, pi, 0)
wcauchyRho = c(0.9, 0.2, 0.6)

tripleStateModel = fitHMM(Lamprey.hmm, nbStates = 3, 
            stepPar0 = c(weibullShape, weibullScale), 
            anglePar0 = c(wcauchyMu, wcauchyRho),
            stepDist = "weibull", angleDist = "wrpcauchy")


## ---- echo = FALSE, fig.width=5------------------------------------------
plot(tripleStateModel)

## ---- echo = FALSE, fig.width=16, fig.height=8---------------------------
plotStates(tripleStateModel)

## ---- echo=FALSE, cache=TRUE---------------------------------------------
# test different ways to find starting values
n = 100
nstates = 2
startVals = matrix(nrow = n, ncol = nstates * 4)
angleMean = matrix(nrow = n, ncol = nstates)
angleCon = matrix(nrow = n, ncol = nstates)
stepShape = matrix(nrow = n, ncol = nstates)
stepScale = matrix(nrow = n, ncol = nstates)
nll = vector()
set.seed(532)

## ---- echo=TRUE, cache=TRUE----------------------------------------------
for (i in 1:n)
{
  weibullShape = runif(nstates, 0, 50)
  weibullScale = runif(nstates, 0, 50)
  wcauchyMu = runif(nstates, -pi, pi)
  wcauchyRho = runif(nstates, 0, 1)
  
  startVals[i,] = c(weibullShape, weibullScale, wcauchyMu, wcauchyRho)
    
  tryCatch(
  {
    model = fitHMM(Lamprey.hmm, nbStates = nstates, stepPar0 = c(weibullShape, weibullScale), 
                    anglePar0 = c(wcauchyMu, wcauchyRho),
                    stepDist = "weibull", angleDist = "wrpcauchy")
  
    
    angleMean[i,] = model$mle$anglePar[1,]
    angleCon[i,] = model$mle$anglePar[2,]
    stepShape[i,] = model$mle$stepPar[1,]
    stepScale[i,] = model$mle$stepPar[2,]    
    nll[i] = model$mod$minimum
  }, 
  error = function(e) print(paste("i =", i, conditionMessage(e)))
  )
}

## ---- echo=FALSE---------------------------------------------------------
hist(nll[nll < 1e300], xlab = "neg log like", main = "") # some are infinite
hist(nll[nll < 1400], xlab = "neg log like", main = "")

## ---- echo=FALSE, fig.width=16, fig.height=10----------------------------
# look at what the actual starting values were
par(mfrow = c(2, 2))
colors = ifelse(nll < 1400, "red", ifelse(nll > 1e300, "gray", "black"))
plot(angleMean[,1], angleMean[,2], col = colors, xlab = "state 1", ylab = "state 2", main = "Angle mean")
plot(angleCon[,1], angleCon[,2], col = colors, xlab = "state 1", ylab = "state 2", main = "Angle concentration")
plot(stepShape[,1], stepShape[,2], col = colors, xlab = "state 1", ylab = "state 2", main = "Step shape",
     xlim = c(0, 50), ylim = c(0, 50))
plot(stepScale[,1], stepScale[,2], col = colors, xlab = "state 1", ylab = "state 2", main = "Step scale",
     xlim = c(0, 50), ylim = c(0, 50))

## ---- echo=TRUE----------------------------------------------------------
AIC(doubleStateModel, tripleStateModel)

## ------------------------------------------------------------------------
# Expectation Maximization Binary Clustering
require(EMbC)

# here we use the speeds and turning angles we already calculated,
# you can also pass in a move object directly
clustering = embc(as.matrix(Lamprey.VT[, c("V", "Theta")]))


## ---- echo = FALSE-------------------------------------------------------
sctr(clustering) ## low speed, low angle; low speed, high angle, etc. 

## ---- echo = FALSE-------------------------------------------------------
# view(clustering) will plot trajectory if you use move obj
plot(Lamprey.VT$Z.start, type = "l")
points(Lamprey.VT$Z.start, pch = 16, col = clustering@A)

## ---- echo=FALSE, fig.width=14-------------------------------------------
# simulated data with different behavioural states with different methods
data(Multipaths)
par(mfrow = c(1,3))
plot(Nu.sim)
title("Nu.sim")
plot(Tau.sim)
title("Tau.sim")
plot(BCRW.sim)
title("BCRW.sim")

## ------- APPLY FIRST PASSAGE TIME CODE TO NU.SIM, TAU.SIM and BCRW.SIM 
require(adehabitatLT)
Nu.traj = as.ltraj(data.frame(X = Re(Nu.sim$Z),
                      Y = Im(Nu.sim$Z)),
                      Sys.time() + 1:length(Nu.sim$Z),
                      id = "Nu.sim")

Tau.traj = as.ltraj(data.frame(X = Re(Tau.sim$Z),
                   Y = Im(Tau.sim$Z)),
                   Sys.time() + 1:length(Tau.sim$Z),
                   id = "Tau.sim")

BCRW.traj = as.ltraj(data.frame(X = Re(BCRW.sim$Z),
                   Y = Im(BCRW.sim$Z)),
                   Sys.time() + 1:length(BCRW.sim$Z),
                   id = "BCRW.sim")

radii = c(3, 5, 8, 10, 20) # different radii 
Nu.fpt = fpt(Nu.traj, radii) # 
Tau.fpt = fpt(Tau.traj, radii) # 
BCRW.fpt = fpt(BCRW.traj, radii) # 

## ---- echo=FALSE, fig.width=15-------------------------------------------
par(mfrow = c(3,length(radii)))
plot.fpt(Nu.fpt, radii, xlab = "Time", main = "Nu: ")
plot.fpt(Tau.fpt, radii, xlab = "Time", main = "Tau: ")
plot.fpt(BCRW.fpt, radii, xlab = "Time", main = "BCRW: ")

Nu.lav = apply(Nu.fpt[[1]], 2, function(x) lavielle(na.omit(x), Lmin = 2, Kmax = 10))
Tau.lav = apply(Tau.fpt[[1]], 2, function(x) lavielle(na.omit(x), Lmin = 2, Kmax = 10))
BCRW.lav = apply(BCRW.fpt[[1]], 2, function(x) lavielle(na.omit(x), Lmin = 2, Kmax = 10)) # contains fewer observations?

Nu.optseg = unlist(lapply(Nu.lav, function(x) { y = chooseseg(x, output = "opt")
title(paste("Nu, opt seg =", y))
y}))
Tau.optseg = unlist(lapply(Tau.lav, function(x) { y = chooseseg(x, output = "opt")
title(paste("Tau, opt seg =", y))
y}))
BCRW.optseg = unlist(lapply(BCRW.lav, function(x) { y = chooseseg(x, output = "opt")
title(paste("Tau, opt seg =", y))
y}))

## ---- echo=FALSE, fig.width=15-------------------------------------------
# put those edges on boundaries between clusters --  behavioural states
par(mfrow = c(3, length(radii)))
temp = lapply(1:5, function(x) { findpath(Nu.lav[[x]], Nu.optseg[x])
  title(paste("Nu: ", radii[x])) })
temp = lapply(1:5, function(x) { findpath(Tau.lav[[x]], Nu.optseg[x])
  title(paste("Tau: ", radii[x])) })
temp = lapply(1:5, function(x) { findpath(BCRW.lav[[x]], Nu.optseg[x])
  title(paste("BCRW: ", radii[x])) })

## ---- eval=FALSE---------------------------------------------------------
## data(Multipaths)
## 
## # ltraj object
## Nu.traj = as.ltraj(data.frame(X = Re(Nu.sim$Z),
##                               Y = Im(Nu.sim$Z)),
##                    Sys.time() + 1:length(Nu.sim$Z),
##                    id = "Nu.sim")
## 
## # bcpa object
## Nu.VT = GetVT(data.frame(Z = Nu.sim$Z,
##                          Time = Sys.time() + 1:length(Nu.sim$Z)),
##               units = "sec")
## 
## # moveHMM object
## Nu.hmm = prepData(Nu.traj[[1]], type = "UTM", coordNames = c("x", "y"))
## 

## ---- echo=FALSE---------------------------------------------------------
data(Wolf)
plot.track(Wolf, main = "Wolf")

## ------------------------------------------------------------------------
data(Wolf)
Wolf2 = data.frame(X = tapply(Wolf$X, 
                               substr(Wolf$Time, 1, 10), mean), 
                    Y = tapply(Wolf$Y, 
                               substr(Wolf$Time, 1, 10), mean))
Wolf2$Time = as.POSIXct(row.names(Wolf2))

Wolf.traj = as.ltraj(data.frame(Wolf2$X, Wolf2$Y),
                      as.POSIXct(Wolf2$Time), 
                      id = "Wolf")

