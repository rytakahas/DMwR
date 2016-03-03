# TODO: Add comment
#
# http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR/
# 
###############################################################################

library(DMwR)

library(xts)
x1 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01"), len = 100, by = "day"))
x1[1:5]


x2 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01 13:00"), len = 100, by = "min"))
x2[1:4]

x3 <- xts(rnorm(3), as.Date(c("2005-01-01", "2005-01-10","2005-01-12")))
x3

x1[as.POSIXct("2000-01-04")]
x1["2000-01-05"]
x1["20000105"]
x1["200004"]
x1["2000-03-27/"]
x1["2000-02-26/2000-03-03"]
x1["/20000103"]

mts.vals <- matrix(round(rnorm(25),2),5,5)
colnames(mts.vals) <- paste('ts',1:5,sep = '')
mts <- xts(mts.vals,as.POSIXct(c('2003-01-01','2003-01-04',
						'2003-01-05','2003-01-06','2003-02-16')))
mts
mts["2003-01",c("ts2","ts5")]
index(mts)
coredata(mts)

library(tseries)
NASDAQ <-as.xts(get.hist.quote("^IXIC",start = "1971-02-05",
				quote = c("Open","High","Low","Close","Volume","AdjClose")))

library(quantmod)
setSymbolLookup(IXIC=list(name = '^IXIC', src = 'yahoo'),
				USDEUR = list(name = 'USD/EUR', src = 'oanda'))
getSymbols(c('IXIC','USDEUR'))
head(IXIC)
head(USDEUR)

library("RODBC");
conn <- odbcConnect("Impala")
result <- sqlQuery(conn, "select * from stock limit 3")

library(DBI)
library(RMySQL)
drv <- dbDriver("MySQL")
ch <- dbConnect(drv, dbname = "Quotes", "root", "cloudera")
allQuotes <- dbGetQuery(ch, "select * from gspc")
GSPC <- xts(allQuotes[,-1], order.by = as.Date(allQuotes[,1]))
head(GSPC)
dbDisconnect(ch)
dbUnloadDriver(drv)

setSymbolLookup(NASDAQ=list(name='^IXIC',src='mysql',
		db.fields = c('Index','Open','High','Low','Close','Volume','AdjClose'),
		user = 'root', password = 'cloudera', dbname = 'Quotes'))
getSymbols('NASDAQ')


### Defining the Prediction Tasks
T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10){
	#high, low, and close quotes
	v <- apply(HLC(quotes),1,mean)
	r <- matrix(NA,ncol = n.days, nrow = NROW(quotes))
	for(x in 1:n.days) r[,x] <- Next(Delt(v, k=x),x)
	
	x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < tgt.margin])) 
	if (is.xts(quotes)) xts(x,time(quotes)) else x
}

setSymbolLookup(IBEX35=list(name = '^IBEX', src = 'yahoo'))
getSymbols(c('IBEX35'))

IBEX35 <-as.xts(get.hist.quote("^IBEX",start = "1993-02-15",
				quote = c("Open","High","Low","Close","Volume","AdjClose")))

candleChart(last(IBEX35,'6 months'), theme = 'white', TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = 'AvgPrice')
addT.ind <- newTA(FUN = T.ind, col = 'red', legend = 'tgtRet')
addAvgPrice(on = 1)
addT.ind()

IBEX35[,2] <- IBEX35[,2]+1e-6
IBEX35[,5] <- IBEX35[,5]+1e-6
emv <- EMV(HLC(IBEX35)[,-3], Vo(IBEX35), n=9, maType="EMA", vol.divisor=10000)

myATR <- function(x) ATR(HLC(x))[,'atr']
mySMI <- function(x) SMI(HLC(x))[,'SMI']
myADX <- function(x) ADX(HLC(x))[,'ADX']
myAroon <- function(x) aroon(x[,c('High','Low')])$oscillator
myBB <- function(x) BBands(HLC(x))[,'pctB']
myChaikinVol <- function(x) Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
myEMV <- function(x) EMV(x[,c('High','Low')],x[,'Volume'])[,2]
myMACD <- function(x) MACD(Cl(x))[,2]
myMFI <- function(x) MFI(x[,c("High","Low","Close")], x[,"Volume"])
mySAR <- function(x) SAR(x[,c('High','Close')]) [,1]
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]



library(randomForest)
colnames(IBEX35) <- c("Open", "High", "Low", "Close","Volume","Adjusted")
data.model <- specifyModel(T.ind(IBEX35) ~ Delt(Cl(IBEX35),k=1:10) + 
				myATR(IBEX35) + mySMI(IBEX35) + myADX(IBEX35) + myAroon(IBEX35) + 
				myBB(IBEX35)  + myChaikinVol(IBEX35) + myCLV(IBEX35) + 
				CMO(Cl(IBEX35)) + EMA(Delt(Cl(IBEX35))) + myEMV(IBEX35) + 
				myVolat(IBEX35)  + myMACD(IBEX35) + myMFI(IBEX35) + RSI(Cl(IBEX35)) +
				mySAR(IBEX35) + runMean(Cl(IBEX35)) + runSD(Cl(IBEX35)))
set.seed(1234)
rf <- buildModel(data.model, method = 'randomForest',
			training.per = c(start(IBEX35),index(IBEX35["2016-02-01"])),
			ntree = 500, importance = T)
	
ex.model <- specifyModel(T.ind(CABK.MC) ~ Delt(Cl(CABK.MC),k = 1:3))
data <- modelData(ex.model, data.window = c('2015-09-01','2016-02-01'))

varImpPlot(rf@fitted.model, type = 1)

imp <- importance(rf@fitted.model, type = 1)
rownames(imp)[which (imp > 10)]


data.model <- specifyModel(T.ind(IBEX35) ~ Delt(Cl(IBEX35),k=3) + 
				Delt(Cl(IBEX35),k=5) +  Delt(Cl(IBEX35),k=7) + Delt(Cl(IBEX35),k=8) +
				Delt(Cl(IBEX35),k=10) + myATR(IBEX35) + mySMI(IBEX35) + myADX(IBEX35) + 
				myAroon(IBEX35)  + myCLV(IBEX35))

Tdata.train <- as.data.frame(modelData(data.model,
				   		data.window = c('1973-03-01','1999-12-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
						data.window = c('2000-01-01','2016-04-30'))))
Tform <- as.formula('T.ind.IBEX35 ~ .')



### The Prediction Models
set.seed(1234)
library(nnet)
norm.data <- scale(Tdata.train)
nn <- nnet(Tform, norm.data[1:1000,], size = 10, decay = 0.01,
		maxit = 1000, linout = T, trace = T)
norm.preds <- predict(nn, norm.data[1001:2000, ])
preds <- unscale(norm.preds, norm.data)

sigs.nn <- trading.signals(preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.IBEX35"], 0.1, -0.1)
sigs.PR(sigs.nn, true.sigs)


signals <- trading.signals(Tdata.train[, "T.ind.IBEX35"], 0.1, -0.1)
norm.data <- data.frame(signals = signals, scale(Tdata.train[, -1]))
nn <- nnet(signals ~ ., norm.data[1:1000, ], size = 10, decay = 0.01, 
		maxit = 1000, trace = T)
preds <- predict(nn, norm.data[1001:2000, ], type = "class")
sigs.PR(preds, norm.data[1001:2000, 1])


library(e1071)
sv <- svm(Tform, Tdata.train[1:1000, ], gamma = 0.001, cost = 100)
s.preds <- predict(sv, Tdata.train[1001:2000, ])
sigs.svm <- trading.signals(s.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.IBEX35"], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)


library(kernlab)
data <- cbind(signals = signals, Tdata.train[, -1])
ksv <- ksvm(signals ~ ., data[1:1000, ], C = 10)
ks.preds <- predict(ksv, data[1001:2000, ])
sigs.PR(ks.preds, data[1001:2000, 1])


library(earth)
e <- earth(Tform, Tdata.train[1:1000, ])
e.preds <- predict(e, Tdata.train[1001:2000, ])
sigs.e <- trading.signals(e.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.IBEX35"], 0.1, -0.1)
sigs.PR(sigs.e, true.sigs)


### From Prediction to Action
policy.1 <- function(signals, market, opened.pos, money, bet = 0.2, hold.time = 10,
					exp.prof = 0.025, max.loss = 0.05) {
				d <- NROW(market) # this is the ID of today
				orders <- NULL
				nOs <- NROW(opened.pos)
				
				if(!nOs && signals[d] == 'h') return(orders)
				
				#First lets check if we can open new positions
				# i) long positions
				if(signals[d] == 'b' && !nOs){
					quant <- round(bet * money/market[d,'Close'],0)
					if(quant > 0)
						orders <- rbind(orders,
								data.frame(order = c(1, -1, -1), order.type = c(1,2,3),
										val = c(quant,
												market[d, 'Close']*(1 + exp.prof),
												market[d, 'Close']*(1 - max.loss)
												),
											action = c('open','close','close'),
											posID = c(NA, NA, NA)
											)
									)
				# ii) short position
				} else if (signals[d] == 's' && !nOs){
				  # this is the nr of stocks we already need to buy
				  # because of currently opened short positions
				  need2buy <- sum(opened.pos[opened.pos[,'pos.type'] == -1,
								  "N.stocks"])*market[d,'Close']
				  quant <- round(bet*(money - need2buy)/market[d, 'Close'],0)
				  if(quant > 0)
					  orders <- rbind(orders, 
							  data.frame(order = c(-1, 1, 1), order.type = c(1,2,3), 
									  val = c(quant, 
											  market[d, 'Close']*(1 - exp.prof),
											  market[d, 'Close']*(1 + max.loss)
											  ),
											  action = c('open','close','close'),
											  posID = c(NA, NA, NA)
											  )
								  )
				}
				# Now lets check if we need to close positions
				# because their holding time is over
				if (nOs)
					for(i in 1:nOs){
						if (d - opened.pos[i, 'Odate'] >= hold.time)
							orders <- rbind(orders,
									data.frame(order =- opened.pos[i,'pos.type'],
											order.type = 1,
											val = NA,
											action = 'close',
											posID = rownames(opened.pos)[i]
											)
								)
					}
				orders
}

policy.2 <- function(signals, market, opened.pos, money, bet = 0.2,
					 exp.prof = 0.025, max.loss = 0.05) {
				 d <- NROW(market) # this is the ID of today
				 orders <- NULL
				 nOs <- NROW(opened.pos)
				
				 if(!nOs && signals[d] == 'h') return(orders)
				
				#First lets check if we can open new positions
				# i) long positions
				if(signals[d] == 'b'){
					quant <- round(bet * money/market[d,'Close'],0)
					if(quant > 0)
						orders <- rbind(orders,
								data.frame(order = c(1, -1, -1), order.type = c(1,2,3),
										val = c(quant,
												market[d, 'Close']*(1 + exp.prof),
												market[d, 'Close']*(1 - max.loss)
												),
											action = c('open','close','close'),
											posID = c(NA, NA, NA)
											)
									)
				# ii) short position
				} else if (signals[d] == 's'){
				  # this is the nr of stocks we already need to buy
				  # because of currently opened short positions
				  need2buy <- sum(opened.pos[opened.pos[,'pos.type'] == -1,
								  "N.stocks"])*market[d,'Close']
				  quant <- round(bet*(money - need2buy)/market[d, 'Close'],0)
				  if(quant > 0)
					  orders <- rbind(orders, 
							  data.frame(order = c(-1, 1, 1), order.type = c(1,2,3), 
									  val = c(quant, 
											  market[d, 'Close']*(1 - exp.prof),
											  market[d, 'Close']*(1 + max.loss)
											  ),
											  action = c('open','close','close'),
											  posID = c(NA, NA, NA)
											  )
								  )
				}
				orders
}

# Train and test periods
start <- 100
len.tr <- 2000
len.ts <- 500
tr <- start:(start + len.tr -1)
ts <- (start + len.tr):(start + len.tr + len.ts -1)
# getting quotes for the testing period
date <- rownames(Tdata.train[start + len.tr,])
market <- IBEX35[paste(date, '/',sep = '')][1:len.ts]

# learning the model and obtaining its signal predictions
s <- svm(Tform, Tdata.train[tr,], cost = 10, gamma = 0.01)
p <- predict(s, Tdata.train[ts,])
sig <- trading.signals(p, 0.1, -0.1)

# Now using the simulator trader
t1 <- trading.simulator(market, sig, 'policy.1', list(exp.prof = 0.05,
				bet = 0.2, hold.time = 30))

tradingEvaluation(t1)

plot(t1, market, theme = 'white', name = 'IBEX35')

t2 <- trading.simulator(market, sig, 'policy.2', list(exp.prof = 0.05,
				bet = 0.3))

tradingEvaluation(t2)

plot(t2, market, theme = 'white', name = 'IBEX35')


start <- 100
len.tr <- 1000
len.ts <- 500
tr <- start:(start + len.tr -1)
ts <- (start + len.tr):(start + len.tr + len.ts -1)
s <- svm(Tform, Tdata.train[tr,], cost = 10, gamma = 0.01)
p <- predict(s, Tdata.train[ts,])
sig <- trading.signals(p, 0.1, -0.1)
t2 <- trading.simulator(market, sig, 'policy.2', list(exp.prof = 0.05,
				bet = 0.2))
summary(t2)
tradingEvaluation(t2)


### Model Evaluation and Selection

MC.svmR <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...) {
	require(e1071)
	t <- svm(form, train, ...)
	p <- predict(t, test)
	trading.signals(p, b.t, s.t)
}

MC.svmC <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...) {
	require(e1071)
	tgtName <- all.vars(form)[1]
	train[, tgtName] <- trading.signals(train[, tgtName], b.t, s.t)
	t <- svm(form, train, ...)
	p <- predict(t, test)
	factor(p, levels=c('s', 'h', 'b'))
}

MC.nnetR <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...) {
	require(nnet)
	t <- nnet(form, train, ...)
	p <- predict(t, test)
	trading.signals(p, b.t, s.t)
}

MC.nnetC <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...) {
	require(nnet)
	tgtName <- all.vars(form)[1]
	train[, tgtName] <- trading.signals(train[, tgtName], b.t, s.t)
	t <- nnet(form, train, ...)
	p <- predict(t, test, type = 'class')
	factor(p, levels=c('s', 'h', 'b'))
}

MC.earth <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...) {
	require(earth)
	t <- earth(form, train, ...)
	p <- predict(t, test)
	trading.signals(p, b.t, s.t)
}

singleModel <- function(form, train, test, learner, policy.func, ...){
	p <- do.call(paste('MC', learner, sep = '.'), list(form, train, test, ...))
	eval.stats(form, train, test, p, policy.func = policy.func)
}

slide <- function(form, train, test, learner, relearn.step, policy.func, ...){
	real.learner <- learner(paste('MC', learner, sep = '.'), pars = list( ...))
	p <- slidingWindowTest(real.learner, form, train, test, relearn.step)
	p <- factor(p, levels = 1:3, labels = c('s', 'h', 'b'))
	eval.stats(form, train, test, p, policy.func = policy.func)
}

grow <- function(form, train, test, learner, relearn.step, policy.func, ...){
	real.learner <- learner(paste('MC', learner, sep = '.'), pars = list( ...))
	p <- growingWindowTest(real.learner, form, train, test, relearn.step)
	p <- factor(p, levels = 1:3, labels = c('s', 'h', 'b'))
	eval.stats(form, train, test, p, policy.func = policy.func)
}

eval.stats <- function(form, train, test, preds, b.t = 0.1, s.t = -0.1, ...) {
	# Signals evaluation
	tgtName <- all.vars(form)[1]
	test[,tgtName] <- trading.signals(test[,tgtName], b.t, s.t)
	st <- sigs.PR(preds, test[,tgtName])
	dim(st) <- NULL
	names(st) <- paste(rep(c('prec','rec'),each = 3), c('s', 'b', 'sb'), sep = '.')
	
	# Trading evaluation
	date <- rownames(test)[1] 
	market <- IBEX35[paste(date, "/", sep = '')][1:length(preds),]
	trade.res <- trading.simulator(market, preds, ...)

	c(st, tradingEvaluation(trade.res))
}

pol1 <- function(signals, market, op, money)
	policy.1(signals, market, op, money, 
			bet = 0.2, exp.prof = 0.25, max.loss = 0.05, hold.time = 10)

pol2 <- function(signals, market, op, money)
	policy.1(signals, market, op, money, 
			bet = 0.2, exp.prof = 0.25, max.loss = 0.05, hold.time = 20)

pol3 <- function(signals, market, op, money)
	policy.2(signals, market, op, money, 
			bet = 0.2, exp.prof = 0.25, max.loss = 0.05)

# This list of learners we will use
TODO <- c('svmR', 'svmC', 'earth', 'nnetR', 'nnetC')
# The datasets used in the comparison
DSs <- list(dataset(Tform, Tdata.train, 'CABK.MC'))
# Monte Carlo setting used
MCsetts <- mcSettings(10,
					 50,
					 30,
					 2)
# Variants to try for all learners
VARS <- list()
VARS$svmR <- list(cost = c(10, 150), gamma =c(0.01, 0.001),
			  policy.func = c('pol1','pol2','pol3'))
VARS$svmC <- list(cost = c(10, 150), gamma =c(0.01, 0.001),
			  policy.func = c('pol1','pol2','pol3'))
VARS$earth <- list(nk = c(10, 17), degree =c(1, 2), thresh = c(0.01, 0.001),
			  policy.func = c('pol1','pol2','pol3'))
VARS$nnetR <- list(linout = T, maxit =750, size = c(5, 10),
			   decay = c(0.001, 0.01), policy.func = c('pol1','pol2','pol3'))
VARS$nnetC <- list(maxit =750, size = c(5, 10), decay = c(0.001, 0.01), 
			  policy.func = c('pol1','pol2','pol3'))

#main loop
# main loop
for(td in TODO) {
	assign(td,
			experimentalComparison(
					DSs,         
					c(
						do.call('variants',
								c(list('singleModel',learner = td), VARS[[td]],
								  varsRootName = paste('single',td, sep = '.'))),
						do.call('variants',
								c(list('slide',learner = td, relearn.step = c(60,120)),
								  VARS[[td]],
								  varsRootName = paste('slide', td, sep='.'))),
						do.call('variants',
								c(list('grow',learner=td,
								  relearn.step = c(60,120)),
								  VARS[[td]],
								  varsRootName = paste('grow', td, sep = '.')))
					),
		MCsetts)
	)
	
	# save the results
	save(list = td,file = paste(td, 'Rdata', sep = '.'))
}


load('svmR.Rdata')
load('svmC.Rdata')
load('earth.Rdata')
load('nnetR.Rdata')
load('nnetC.Rdata')

tgtStats <- c('prec.sb','Ret','PercProf','MaxDD','SharpeRatio')
allSysRes <- join(subset(svmR, stats = tgtStats),
				  subset(svmC, stats = tgtStats), 
				  subset(nnetR, stats = tgtStats),
				  subset(nnetC, stats = tgtStats),
				  subset(earth, stats = tgtStats),
				  by = 'variants')
rankSystems(allSysRes, 5, maxs = c(T, T, T, T, T))

summary(subset(svmC, stats = c('Ret', 'RetOverBH', 'PercProf', 'NTrades'), 
					 vars = c('slide.svmC.v5', 'slide.svmC.v6')))

	 
fullResults <- join(svmR, svmC, earth, nnetC, nnetR, by = "variants")
nt <- statScores(fullResults, "NTrades")[[1]]
rt <- statScores(fullResults, "Ret")[[1]]
pp <- statScores(fullResults, "PercProf")[[1]]
s1 <- names(nt)[which (nt > 32)]
s2 <- names(rt)[which (rt > 8)]
s3 <- names(pp)[which (pp > 5)]

namesBest <- intersect(intersect(s1, s2), s3)

summary(subset(fullResults, stats = tgtStats, vars = namesBest))

compAnalysis(subset(fullResults, stats = tgtStats, vars = namesBest ))


plot(subset(fullResults, stats = c('Ret', 'RetOverBH', 'MaxDD'), vars = namesBest))

getVariant("single.earth.v20", earth)
getVariant("grow.earth.v40", earth)


# The Trading System
data <- tail(Tdata.train, 100)
results <- list()
for(name in namesBest) {
	sys <- getVariant(name, fullResults)
	results[[name]] <- runLearner(sys, Tform, data, Tdata.eval)
}

results <- t(as.data.frame(results))

results[, c("Ret", "RetOverBH", "MaxDD", "SharpeRatio", "NTrades", "PercProf")]

getVariant('grow.earth.v40', fullResults)

data <- tail(Tdata.train, 50)
results <- list()
for (name in namesBest) {
	sys <- getVariant(name, fullResults)
	results[[name]] <- runLearner(sys, data, Tdata.eval)
}
results <- t(as.data.frame(results))

results[, c("Ret", "RetOverBH", "MaxDD", "SharpeRatio", "NTrades", "PercProf")]


getVariant("grow.earth.v40", fullResults)

model <- learner("MC.nnetR", list(maxit = 750, linout = T, trace = F, size = 10,
				decay = 0.01))
preds <- growingWindowTest(model, Tform, data, Tdata.eval, relearn.step = 120)
signals <- factor(preds, levels = 1:3, labels = c("s", "h", "b"))
date <- rownames(Tdata.eval)[1]
market <- IBEX35[paste(date, "/", sep = '')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = "pol3")

plot(trade.res, market, theme = "white", name = "CABK - final test")


library(PerformanceAnalytics)
rets <- Return.calculate(as.xts(trade.res@trading$Equity))

chart.CumReturns(rets, main = "Cumlative returns of startegy", ylab = "returns")

yearlyReturn(as.xts(trade.res@trading$Equity))

plot(100 * yearlyReturn(as.xts(trade.res@trading$Equity)), 
		main = 'Yearly percentage returns of the trading system')
abline(h = 0, lty = 2)

table.CalendarReturns(R = rets)
table.AnnualizedReturns(rets)

table.DownsideRisk(rets)


