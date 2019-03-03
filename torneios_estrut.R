rm(list=ls());gc()

library(dplyr)
library(forecast)
library(quantmod)
library(QuantTools)
library(TTR)

vec <- c("AAPL","ABBV","ABT","ACN","AGN","AIG","ALL","AMGN","AMZN","AXP","BA","BAC",#"BIIB",
"BK","BKNG","BLK","BMY",#"BRK.B",
"C","CAT","CELG",#"CHTR",
"CL","CMCSA","COF","COP",
"COST","CSCO","CVS","CVX","DHR","DIS","DUK","DWDP","EMR","EXC","F","FB","FDX","FOX",
"FOXA","GD","GE",#"GILD",
"GM","GOOG","GOOGL","GS","HAL","HD","HON","IBM","INTC",
"JNJ","JPM","KHC","KMI","KO","LLY","LMT","LOW","MA","MCD","MDLZ","MDT","MET","MMM",
"MO","MRK","MS","MSFT","NEE","NFLX","NKE","NVDA","ORCL","OXY","PEP","PFE","PG",
"PM","PYPL","QCOM","RTN","SBUX","SLB","SO","SPG","T","TGT","TXN","UNH","UNP","UPS",
"USB","UTX","V","VZ","WBA","WFC","WMT","XOM") ## vetor com empresas

#BAIXA DADOS PARA CADA EMPRESA LISTADA # travou no 18
for (esse in 1:length(vec)){
dados <- getSymbols(as.character(vec[esse]),from = '2008-01-01',to = '2019-01-31')
QQQ <- get(as.character(vec[esse]))
}

#dados <- getSymbols('AAPL',from = '2000-01-01',to = '2019-01-31') 
lista <- list()

for (j in 1:length(vec)){ #vec[13, 21, 43] deu erro
QQQ <- get(vec[j])

fechamento <- QQQ[,4]
close <- QQQ[,4]
high <- QQQ[,2]
low <- QQQ[,3]
open <- QQQ[,1]
volume <- QQQ[,5]

#############################
YYY <- sign(diff(close))
YYY[which(YYY==0)] <- -1
#############################
N <- 10
AB_UP  <- TTR::SMA( QQQ[,3] * (1 - 4*(QQQ[,2]-QQQ[,3] ) / (QQQ[,2] + QQQ[,3]) ), 
                    order = N)
#############################
AB_DOWN  <- TTR::SMA( high * (1 + 4*(high-low ) / (high + low) ), 
                      order = N)
# Acceleration_Middle <- TTR::SMA( QQQ$QQQ.Close , order = N )
#############################
# Accumulative_Distribution <- cumsum(((QQQ$QQQ.Close - QQQ$QQQ.Low) - (QQQ$QQQ.High - QQQ$QQQ.Close)) / ((QQQ$QQQ.High - QQQ$QQQ.Low) * QQQ$QQQ.Volume))

AD <- TTR::chaikinAD(HLC = QQQ[,2:4],volume = QQQ[,5])
#############################
MFM = ((close - low) - (high - close)) / (high - low)
#############################
MFV = MFM * volume

ADL <- 0
for ( i in 2:dim(QQQ)[1] ) { ADL[i] <- ADL[i-1] + MFV[i]}

ADL <- as.xts(ADL,order.by = index(close))

#############################
ADX <- TTR::ADX(HLC = QQQ[,2:4], n = 14)[,4]
#############################
CHOSC <- TTR::EMA(ADL, n = 3) - TTR::EMA(ADL, n = 10)
#############################
ADO <- (high - diff(close))/(high - low)
#############################
n_fast = 10
n_slow = 20

APO <- TTR::EMA( close , n = n_fast ) - TTR::EMA( close , n = n_slow )
#############################
AR_POS <- TTR::aroon(HL = QQQ[,2:3],n = 25)[,1]
AR_NEG <- TTR::aroon(HL = QQQ[,2:3],n = 25)[,2]
AR_OSC <- TTR::aroon(HL = QQQ[,2:3],n = 25)[,3]
#############################
ATR <- TTR::ATR(HLC = QQQ[,2:4],n = 14,maType = TTR::SMA)[,2]
#############################
ATRP <- ATR/close*100
#############################
# Average Volume
AVOL = TTR::SMA(volume , N)
#############################
# BBANDS
BB_LOW <- TTR::BBands(HLC = QQQ[,2:4], n = 20, maType = TTR::SMA, sd = 2)[,1]
BB_UP <- TTR::BBands(HLC = QQQ[,2:4], n = 20, maType = TTR::SMA, sd = 2)[,3]
BB_BW <- (BB_UP-BB_LOW)/TTR::BBands(HLC = QQQ[,2:4], n = 20, maType = TTR::SMA, sd = 2)[,2]*100
#############################
bww <- function(close, n){
  res <- (close - TTR::SMA(close, n))^2
  res <- TTR::SMA(res,n)
  return(res)
}

BWW <- bww(close, n = 10)
#############################
volat <- function(close, n){
  res <- close
  for (i in (n):length(res)){
    res[i] <- (sd(close[(i-n+1):i]))
  }
  res[1:(n-1)] <- NA
  return(res)
}

VOLAT <- volat(close, n = 10)
#############################
perc_b <- function(close,n){
  res <- close
  for (i in (n):length(res)){
    res[i] <- (sd(close[(i-n+1):i]))
  }
  
  low <- TTR::SMA(close,n) - 2*volat(TTR::SMA(close,n), n)
  high <- TTR::SMA(close,n) + 2*volat(TTR::SMA(close,n), n)
  res <- (close-low)/(high-low)
  return(res)
}

PERC_B <- perc_b(close, n = 20)
#############################
CCI <- TTR::CCI(HLC = QQQ[,2:4], n = 20, maType = TTR::SMA, c = 0.015)
#############################
CMF <- TTR::CMF(HLC = QQQ[,2:4], volume = QQQ[,5], n = 20)
#############################
cvol <- function(high, low, n){
  prov <- TTR::EMA(high-low, n)
  res <- high
  for (i in 11:length(res)){
    res[i] <- (as.numeric(prov[i]) - as.numeric(prov[i-n]))/as.numeric(prov[i-10])
  }
  res[1:10] <- NA
  return(res)
}

CVOL <- cvol(high, low, n = 5)
#############################
CMO <- TTR::CMO(close, n = 20)
#############################
maxx <- function(close, n){
  res <- close
  for (i in (n+1):length(res)){
    res[i] <- max(close[(i-n):(i-1)])
  }
  res[1:n] <- NA
  return(res)
}

MAXX <- maxx(close, n = 10)
#############################
minn <- function(close, n){
  res <- close
  for (i in (n+1):length(res)){
    res[i] <- min(close[(i-n):(i-1)])
  }
  res[1:n] <- NA
  return(res)
}

MINN <- minn(close, n = 10)
#############################
CHAND_LONG <- maxx(close,n = 22) - 3*TTR::ATR(HLC = QQQ[,2:4],n = 22,maType = TTR::SMA)[,2]
CHAND_SHORT <- maxx(close,n = 22) + 3*TTR::ATR(HLC = QQQ[,2:4],n = 22,maType = TTR::SMA)[,2]
#############################
roc <- function(close, n){
  res <- close
  for (i in (n+1):length(res)){
    res[i] <- (as.numeric(close[i]))/(as.numeric(close[i-n]))*100
  }
  res[1:n] <- NA
  return(res)
}

ROC <- roc(close, n = 10)
#############################
COPP <- as.xts(TTR::WMA((roc(close, n = 14) + roc(close, n = 11)), n = 10),order.by = index(close))
#############################
DPO <- TTR::DPO(close, n = 20, maType = TTR::SMA)
#############################
PDM = diff(high)
NDM = c()
for(i in 1: (length(low) - 1) )  {
  NDM[i] = as.numeric( low[i] ) - as.numeric( low[i+1] )
}

WPDM = c(NA,NA)

for (i in 3:length(PDM) ){
  WPDM[i] = PDM[i-1] - mean(PDM[1:i-1],na.rm=TRUE) + as.numeric( PDM[i] )
}

WNDM = c(NA,NA)

for (i in 3:length(PDM) ){
  WNDM[i] = NDM[i-1] - mean(NDM[1:i-1],na.rm=TRUE) + as.numeric( NDM[i] )
}

close_low  <- c()
high_close <- c()
high_low   <- c()

for (i in 2:nrow(QQQ)) {
  high_low      <- high - low
  high_close[i] <- as.numeric( high[i] ) - as.numeric( close[i-1] )
  close_low[i]  <- as.numeric( close[i-1] ) - as.numeric( low[i] )
}

tr = data.frame(High_menos_Low     =  high_low,
                High_menos_Close_1 =  high_close,
                Close_1_menos_Low  =  close_low )


TR =   apply(tr, 1, max, na.rm = TRUE)

WTR = c(NA,NA)
for (i in 3:length(TR)){
  WTR[i] = TR[i-1] - mean(TR[1:i-1],na.rm=TRUE) + as.numeric( TR[i] )
}

PDI = ( WPDM / WTR ) * 100
NDI = ( WNDM / WTR ) * 100
DD = abs(PDI - NDI)
DMI = ( DD / (PDI + NDI)  ) * 100

DMI <- as.xts(DMI,order.by = index(close))
#############################
DONCHIAN <- TTR::DonchianChannel(QQQ[,2:3], n = 10)[,2]
#############################
DEMA = 2 * ( TTR::EMA(close , N) - TTR::EMA( TTR::EMA(close, N ), N   )  )
#############################
minimos = c()
maximos = c()
for (i in 1:length(low)) { 
  minimos[i] = min(low[1:i])
  maximos[i] = max(high[1:i])
}

close_menos_min = c()
high_menos_min = c()

for ( i in 1:length(low)){
  close_menos_min[i] = close[i] - minimos[i]
  high_menos_min[i]  = maximos[i] - minimos[i]
}

DSS = as.xts((TTR::EMA( TTR::EMA( close_menos_min)  ) / TTR::EMA( TTR::EMA( high_menos_min) ) ) * 100
             ,order.by = index(close))
#############################
hl_tm1 = NA
for (i in 2:length(high)) hl_tm1[i] = high[i-1] + low[i-1]

prov = ((high - low)/2 - hl_tm1/2 )  /  ((volume/100000000) / (high-low))

EMV = TTR::SMA(prov,n = 14)  
#############################
EMA <- TTR::EMA(close, n = 10)
#############################
FORCE = TTR::EMA(diff(close)*volume, n=13)
#############################
hull <- function(close, n){
  prov1 <- TTR::WMA(close, n = round(n/2))
  prov2 <- TTR::WMA(close, n = n)
  res <- as.xts((TTR::EMA( TTR::EMA( close_menos_min)  ) / TTR::EMA( TTR::EMA( high_menos_min) ) ) * 100
                ,order.by = index(close))
  return(res)
}
HULL <- hull(close, n = 20)
#############################
n1 = 10 ; nf = 2 ; ns = 30

change <- abs(diff(close, lag = 10))
vovovo <- diff(close)
ER <- close
for (i in 11:length(ER)){
  ER[i] <- as.numeric(change[i])/(sum(abs(vovovo[(i-9):i])))
}

SC <- (ER*(2/(nf+1) - 2/(ns+1)) + (2/(ns+1)))^2

KAMA <- TTR::SMA(close, n = 10)
for (i in 11:length(KAMA)){
  KAMA[i] <- as.numeric(KAMA[i-1]) + SC[i]*(as.numeric(close[i]) - as.numeric(KAMA[i-1]))
}
#############################
KC_M = TTR::EMA( high + low + close / 3 , n = 20)  # no paper n1 = 20

KC_L = KC_M - 2* (TTR::ATR (QQQ[,2:4], n = 10)$atr ) # no paper n = 10. Quem ??? o X ???
KC_U = KC_M + 2* (TTR::ATR (QQQ[,2:4], n = 10)$atr ) 
#############################
mqo <- function(close, n){
  prov <- close
  res1 <- close
  res2 <- close
  res3 <- close
  res4 <- close
  for (i in (n+1):length(prov)){
    base <- prov[(i-n):(i-1)]
    modell <- lm(formula = base~index(base))
    res1[i] <- modell$coefficients[1]
    res2[i] <- modell$coefficients[2]
    # res3[i] <- close[i]*modell$coefficients[2] + modell$coefficients[1]
    res3[i] <- predict(modell, close[i])[1]
    res4[i] <- summary(modell)$coefficients[2,2]
  }
  res1[1:n] <- res2[1:n] <- res3[1:n] <- res4[1:n] <- NA
  res <- cbind(res1, res2, res3, res4)
  return(res)
}

ppp <- mqo(close, 10)
# ppp %>% head(16)

MQO_ALPHA <- ppp[,1]
MQO_BETA <- ppp[,2]
MQO_PRED <- ppp[,3]
MQO_STD <- ppp[,4]
#############################
MACD <- TTR::MACD(close, nFast = 12, nSlow = 26, maType = TTR::EMA, nSig = 9)[,1]
#############################
MACDH <- TTR::MACD(close, nFast = 12, nSlow = 26, maType = TTR::EMA, nSig = 9)[,2]
#############################
MAE_UP = TTR::SMA(close) + TTR::SMA(close)/4
MAE_LOW = TTR::SMA(close) - TTR::SMA(close)/4
#############################
mass <- function(high, low, n){
  prov <- TTR::EMA(high-low, n) / TTR::EMA(TTR::EMA(high-low, n), n)
  res <- high
  for (i in (3*(n-1)+24):length(res)){
    res[i] <- sum(prov[(i-24):i])
  }
  res[1:(3*(n-1)+23)] <- NA
  return(res)
}

MASS <- mass(high, low, n = 9)
#############################
RMF <- (high+low+close)/3*volume
#############################
MFI <- TTR::MFI(HLC = QQQ[,2:4],volume = QQQ[,5],n = 14)
#############################
MIDPOINT <- (MAXX - MINN)/2
#############################
# midprice <- function(high, low, n){
#   res <- high
#   for (i in 1:length(res)){
#     res[i] <- (maxx(high, n = 10)[i] - minn(low, n = 10)[i])/2
#   }
#   return(res)
# }
# MIDPRICE <- midprice(high, low, n = 10)

MIDPRICE <- (maxx(high, n = 10) - minn(low, n = 10))/2
#############################
MOM <- momentum(close, n = 10)
#############################
nvi <- function(close, volume){
  res <- close
  res[1] <- 1000
  for (i in 2:length(res)){
    if (as.numeric(volume[i]) < as.numeric(volume[i-1])){
      res[i] <- as.numeric(res[i-1]) + (as.numeric(close[i]) - as.numeric(close[i-1]))/
        as.numeric(close[i-1]) * as.numeric(res[i-1])
    }
    else{
      res[i] <- res[i-1]
    }
  }
  return(res)
}

NVI <- nvi(close, volume)
#############################
# ATR <- TTR::ATR(HLC = QQQ[,2:4],n = 14,maType = TTR::SMA)[,2]
NATR <- ATR/close*100
#############################
OBV <- TTR::OBV(close, volume)
#############################
# parabolic SAR -- HOLD::
SAR <- TTR::SAR(HL = QQQ[,2:3],accel = c(0.02,0.2))
#############################
TP <- (high + low + close)/3
SS1 <- 2*TP - high
SS2 <- TP - (high - low)
SR1 <- 2*TP - low
SR2 <- TP + (high - low)
#############################
# TP <- (high + low + close)/3
FS1 <- TP - (0.382 * (high - low))
FS2 <- TP - (0.618 * (high - low))
FR1 <- TP + (0.382 * (high - low))
FR2 <- TP + (0.618 * (high - low))
#############################
ddd <- close
for (i in 1:length(ddd)){
  if (close[i] == open[i]){
    ddd[i] <- high[i] + low[i] + 2*close[i]
  }
  else if (close[i] > open[i]){
    ddd[i] <- 2*high[i] + low[i] + close[i]
  }
  else{
    ddd[i] <- high[i] + 2*low[i] + close[i]
  }
}

PD1 <- ddd/4
DS1 <- (ddd/2) - high
DR1 <- (ddd/2) - low
#############################
pc_up <- function(high, n){
  res <- high
  for (i in n:length(res)){
    res[i] <- max(high[(i-n+1):i])
  }
  res[1:(n-1)] <- NA
  return(res)
}

PC_UP <- pc_up(high, n = 20)
#############################
pc_down <- function(low, n){
  res <- low
  for (i in n:length(res)){
    res[i] <- min(low[(i-n+1):i])
  }
  res[1:(n-1)] <- NA
  return(res)
}

PC_DOWN <- pc_down(low, n = 20)
#############################
chopp <- function(high, low, n){
  prov1 <- TTR::ATR(HLC = QQQ[,2:4],n,maType = TTR::SMA)[,2]
  prov2 <- pc_up(high, n) - pc_down(low, n)
  
  res <- close
  res[1:(n-1)] <- NA
  for (i in n:length(res)){
    res[i] <- (log10(sum(prov1[(i-n+1):i])+n))/prov2[i]
  }
  return(res)
}

CHOPPINESS <- chopp(high, low, n = 14)
#############################
ppo <- function(close, nfast, nslow){
  res <- (EMA(close, nfast) - EMA(close, nslow))/EMA(close, nslow) * 100
  return(res)
}

PPO <- ppo(close, nfast = 12, nslow = 26)
#############################
PPOH <- PPO - EMA(PPO, n = 9)
#############################
pvo <- function(volume, nfast, nslow){
  res <- (EMA(volume, nfast) - EMA(volume, nslow))/EMA(volume, nslow) * 100
  return(res)
}

PVO <- pvo(volume, nfast = 12, nslow = 26)
#############################
PVOH <- PVO - EMA(PVO, n = 9)
#############################
pvi <- function(close, volume){
  res <- close
  res[1] <- 1000
  for (i in 2:length(res)){
    if (as.numeric(volume[i])>as.numeric(volume[i-1])){
      res[i] <- as.numeric(res[i-1]) + (as.numeric(close[i]) - as.numeric(close[i-1]))/
        as.numeric(close[i-1]) * as.numeric(res[i-1])
    }
    else{
      res[i] <- res[i-1]
    }
  }
  return(res)
}

PVI <- pvi(close, volume)
#############################
pvt <- function(close, volume){
  res <- close
  res[1] <- 0
  for (i in 2:length(res)){
    res[i] <- as.numeric(res[i-1]) + ((as.numeric(close[i]) - as.numeric(close[i-1]))/
                                        as.numeric(close[i-1]) * as.numeric(volume[i]))
  }
  return(res)
}

PVT <- pvt(close, volume)
#############################
KST <- TTR::SMA(roc(close, n = 10),n = 10) + 2*(TTR::SMA(roc(close, n = 15),n = 10)) + 
  3*(TTR::SMA(roc(close, n = 20),n = 10)) + 4*(TTR::SMA(roc(close, n = 30),n = 15))
#############################
PSK <- TTR::SMA(roc(close, n = 10),n = 10) + 2*(TTR::SMA(roc(close, n = 15),n = 10)) + 
  3*(TTR::SMA(roc(close, n = 20),n = 10)) + 4*(TTR::SMA(roc(close, n = 30),n = 15)) + 
  TTR::SMA(roc(close, n = 40),n = 50) + 2*(TTR::SMA(roc(close, n = 65),n = 65)) + 
  3*(TTR::SMA(roc(close, n = 75),n = 75)) + 4*(TTR::SMA(roc(close, n = 100),n = 100)) + 
  TTR::SMA(roc(close, n = 195),n = 130) + 2*(TTR::SMA(roc(close, n = 265),n = 130)) + 
  3*(TTR::SMA(roc(close, n = 390),n = 130)) + 4*(TTR::SMA(roc(close, n = 530),n = 195))
#############################
RSI <- RSI(close, n = 10, maType = TTR::SMA)
#############################
pp1 <- TTR::SMA(((close-open) + 2*(diff(close)-diff(open)) + 
                   2*(diff(close,lag = 2)-diff(open,lag = 2)) + (diff(close,lag = 3)-diff(open,lag = 3)))/6, n = 12)
pp2 <- TTR::SMA(((high-low) + 2*(diff(high)-diff(low)) + 
                   2*(diff(high,lag = 2)-diff(low,lag = 2)) + (diff(high,lag = 3)-diff(low,lag = 3)))/6, n = 12)

RVI <- pp1/pp2
#############################
SMA <- TTR::SMA(close, n = 10)
#############################
stochrsi <- function(close, n){
  prov <- RSI(close, n = 10, maType = TTR::SMA)
  res <- close
  res[1:n] <- NA
  for (i in (n+1):length(res)){
    res[i] <- (prov[i] - min(prov[(i-n):(i-1)]))/(max(prov[(i-n):(i-1)]) - min(prov[(i-n):(i-1)]))
  }
  return(res)
}

STRSI <- stochrsi(close, n = 10)
#############################
STOCH_K<- stoch(close, nFastK = 10, nSlowD = 3, nFastD = 3)[,1]
#############################
STOCH_D<- stoch(close, nFastK = 10, nSlowD = 3, nFastD = 3)[,2]
#############################
STOCH_D_SLOW<- stoch(close, nFastK = 10, nSlowD = 3, nFastD = 3)[,3]
#############################
TEMA <- (3*EMA(close, n = 10)) - 
  (3*EMA(EMA(close, n = 10), n = 10)) + 
  (3*EMA(EMA(EMA(close, n = 10), n = 10), n = 10))
#############################
TRIMA <- SMA(close, n = round((10+1)/2))
#############################
TRIX <- TRIX(close, n = 15, maType = TTR::EMA)[,1]
#############################
TSI <- (EMA(EMA(diff(close),n = 25),n = 13))/(EMA(EMA(abs(diff(close)),n = 25),n = 13))
#############################
TP <- (high + low + close)/3
#############################
ulcer <- function(close, n){
  prov <- close
  prov[1:n] <- NA
  for (i in (n+1):length(prov)){
    prov[i] <- (as.numeric(close[i]) - as.numeric(max(close[(i-n+1):i])))/
      as.numeric(max(close[(i-n+1):i])) * 100
  }
  res <- sqrt(SMA(prov^2,n))
  return(res)
}

ULCER <- ulcer(close, n = 14)
#############################
ULTOSC <- ultimateOscillator(QQQ[,2:4], n = c(7,14,28), wts = c(4,2,1))
#############################
vama <- function(close, volume, n){
  avol <- SMA(volume, n)
  prov <- (3*volume)/(2*avol)*close
  return(SMA(prov, n))
}

VAMA <- vama(close, volume, n = 10)
#############################
vwap <- function(volume, n){
  propro <- TP*volume
  res <- volume
  res[1:(n-1)] <- NA
  for (i in n:length(res)){
    res[i] <- (sum(propro[(i-n+1):i]))/(sum(volume[(i-n+1):i]))
  }
  return(res)
}

VWAP <- vwap(volume, n = 15)
#############################
vol_osc <- function(volume, n_fast, n_slow){
  return((SMA(volume, n_fast) - SMA(volume, n_slow))/SMA(volume, n_slow)*100)
}

VOOSC <- vol_osc(volume, n_fast = 14, n_slow = 28)
#############################
vpt <- function(volume, close){
  res <- volume
  res[1] <- 0
  for (i in 2:length(res)){
    res[i] <- as.numeric(res[i-1]) + as.numeric(volume[i])*
      ((as.numeric(close[i]) - as.numeric(close[i-1]))/as.numeric(close[i-1]))
  }
  res[1] <- NA
  return(res)
}

VPT <- vpt(volume, close)
#############################
pvoi <- function(high, low, n){
  prov <- abs(high - diff(low))
  tr <- TTR::ATR(HLC = QQQ[,2:4],n = 14, maType = TTR::SMA)[,1]
  res <- high
  for (i in (n+2):length(res)){
    res[i] <- (sum(prov[(i-n):(i-1)]))/(sum(tr[(i-n):(i-1)]))
  }
  res[1:(n+1)] <- NA
  return(res)
}

nvoi <- function(high, low, n){
  prov <- abs(low - diff(high))
  tr <- TTR::ATR(HLC = QQQ[,2:4],n = 14, maType = TTR::SMA)[,1]
  res <- high
  for (i in (n+2):length(res)){
    res[i] <- (sum(prov[(i-n):(i-1)]))/(sum(tr[(i-n):(i-1)]))
  }
  res[1:(n+1)] <- NA
  return(res)
}

PVOI <- pvoi(high, low, n = 14)
NVOI <- nvoi(high, low, n = 14)
#############################
WILL_R <- WPR(QQQ[,2:4],n = 10)
#############################
WMA <- WMA(close, n = 10, wts = 1:10)
#############################
wws <- function(close,n){
  res <- close
  res[1:(n-1)] <- NA
  res[n] <- SMA(close, n)[n]
  for (i in (n+1):length(res)){
    res[i] <- as.numeric(res[(i-1)]) + (close[i] - as.numeric(res[(i-1)]))/n
  }
  return(res)
}

WWS <- wws(close, n = 10)
#############################


#############################
#############################
#############################
#############################
#############################
########### BORIS ###########
#############################
#############################
#############################
#############################
#############################

#############################
DISP <- fechamento/(SMA(x = fechamento, n = 9))
#############################
n_fast <- 12
n_slow <- 24

OSCP <- (SMA(x = fechamento, n = n_fast)-SMA(x = fechamento, n = n_slow))/SMA(x = fechamento, n = n_fast)
#############################
indic_up <- function(x,n){
  indic <- x
  for (i in 2:length(indic)){
    indic[i] <- sign(as.numeric(x[i])-as.numeric(x[i-1]))
  }
  indic[1] <- NA
  
  res <- x
  for (i in (n+1):length(res)){
    res[i] <- length(which(indic[(i-n+1):(i)]=="1"))
  }
  res[1:n] <- NA
  
  return(res)
}
# indic_up(fechamento,n = 3)

PSY <- function(x,n){
  return((indic_up(x,n))/n*100)
}

PSY <- PSY(fechamento,n = 10)
#############################
DIU <- function(close,high,low,n){
  
  numm <- close
  for (i in (n+1):length(close)){
    numm[i] <- sum(diff(high)[(i-n+1):i])
  }
  numm[1:n] <- NA
  
  prov <- cbind(as.numeric(high - low),
                c(NA,as.numeric(high[-1]) - as.numeric(fechamento[-length(fechamento)])),
                c(NA,as.numeric(as.numeric(fechamento[-length(fechamento)]) - low[-1])))
  
  prov2 <- as.xts(apply(prov,1,max),order.by = index(close))
  
  res <- numm/prov2
  return(res*100)
}

DIU <- DIU(close, high, low, n=10)
#############################
DID <- function(close,high,low,n){
  
  numm <- close
  for (i in (n+1):length(close)){
    numm[i] <- sum(diff(low)[(i-n+1):i])
  }
  numm[1:n] <- NA
  
  prov <- cbind(as.numeric(high - low),
                c(NA,as.numeric(high[-1]) - as.numeric(fechamento[-length(fechamento)])),
                c(NA,as.numeric(as.numeric(fechamento[-length(fechamento)]) - low[-1])))
  
  prov2 <- as.xts(apply(prov,1,max),order.by = index(close))
  
  res <- numm/prov2
  return(res*100)
}

DID <- DID(close, high, low, n=10)
#############################
BIAS <- (close - SMA(close,n=5))/SMA(close,n=5)*100
#############################
vol_ratio <- function(close,volume,n){
  indic <- close
  for (i in 2:length(indic)){
    indic[i] <- sign(as.numeric(close[i])-as.numeric(close[i-1]))
  }
  indic[1] <- NA
  
  res1 <- vector(mode = "numeric",length = length(close))
  for (i in 2:length(res1)){
    if (indic[i]>0){
      res1[i] <- volume[i]
    }
    else{
      res1[i] <- 0
    }
  }
  res1[1] <- NA
  
  res2 <- vector(mode = "numeric",length = length(close))
  for (i in 2:length(res2)){
    if (indic[i]<=0){
      res2[i] <- volume[i]
    }
    else{
      res2[i] <- 0
    }
  }
  res2[1] <- NA
  
  res <- close
  
  for (i in (n+1):length(close)){
    if (as.numeric(sum(res2[(i-n+1):i]) - sum(volume[(i-n+1):i])) == 0){
      res[i] <- 0
    }
    else{
      res[i] <- as.numeric((sum(res1[(i-n+1):i]) - sum(volume[(i-n+1):i])))/
        as.numeric(sum(res2[(i-n+1):i]) - sum(volume[(i-n+1):i]))
    }
  }
  res[1:n] <- NA
  
  return(res)
}

VOLR <- vol_ratio(close,volume,n = 3)
#############################
a_ratio <- function(open, high, low, n){
  prov1 <- high-open
  prov2 <- open-low
  
  res <- low
  for (i in (n):length(res)){
    res[i] <- (sum(prov1[(i-n+1):i]))/(sum(prov2[(i-n+1):i]))
  }
  res[1:(n-1)] <- NA
  
  return(res)
}

ARATIO <- a_ratio(open, high, low, n = 10)
#############################
b_ratio <- function(close, high, low, n){
  prov1 <- high-close
  prov2 <- close-low
  
  res <- close
  for (i in (n):length(res)){
    res[i] <- (sum(prov1[(i-n+1):i]))/(sum(prov2[(i-n+1):i]))
  }
  res[1:(n-1)] <- NA
  
  return(res)
}

BRATIO <- b_ratio(close, high, low, n = 10)
#############################
REX <- SMA(3*close - (low + open + high), n=20)
#############################
hpr <- function(close,n){
  res <- close
  for (i in n:length(close)){
    res[i] <- res[i]/max(close[(i-n+1):i])
  }
  res[1:(n-1)] <- NA
  return(res)
}

HPR <- hpr(close,n=10)
#############################
lpr <- function(close,n){
  res <- close
  for (i in n:length(close)){
    res[i] <- min(close[(i-n+1):i])/res[i]
  }
  res[1:(n-1)] <- NA
  return(res)
}

LPR <- lpr(close,n=10)
#############################
vmom <- function(volume,n){
  res <- volume
  for (i in (n+1):length(res)){
    res[i] <- as.numeric(volume[i])-as.numeric(volume[i-n])
  }
  res[1:n] <- NA
  return(res)
}

VMOM <- vmom(volume, n = 10)
#############################
mpp <- function(close, n){
  res <- close
  for (i in n:length(close)){
    res[i] <- (close[i] - min(close[(i-n+1):i]))/(max(close[(i-n+1):i]) - min(close[(i-n+1):i]))
  }
  res[1:(n-1)] <- NA
  return(res)
}

MPP <- mpp(close, n = 10)
#############################
var_ratio <- function(close, n){
  res <- close
  for (i in (2*n):length(res)){
    res[i] <- ((sd(close[(i-n+1):i]))^2)/((sd(close[(i-n-n+1):(i-n)]))^2)
  }
  res[1:(2*n-1)] <- NA
  return(res)
}
VARR <- var_ratio(close, n = 10)
#############################


BASIS <- cbind(AB_UP,AB_DOWN, AD, MFM, ADL, ADX, CHOSC, ADO, APO, AR_POS, AR_NEG, AR_OSC, ATR, ATRP, AVOL, BB_UP, BB_LOW, BB_BW, BWW,
               VOLAT, PERC_B, CCI, CMF, CVOL, CMO, MAXX, MINN, CHAND_LONG, CHAND_SHORT, ROC, COPP, DPO, DMI, DONCHIAN, DEMA,
               DSS, EMV, FORCE, HULL, KAMA, KC_U, KC_M, KC_L, MQO_ALPHA, MQO_BETA, MQO_PRED, MQO_STD, MACD, MACDH,
               MAE_UP, MAE_LOW, MASS, RMF, MFI, MIDPOINT, MIDPRICE, MOM, NVI, NATR, OBV, SAR, TP, SS1, SS2, SR1, SR2, FS1,
               FS2, FR1, FR2, PD1, DS1, DR1, PC_UP, PC_DOWN, CHOPPINESS, PPO, PPOH, PVO, PVOH, PVI, PVT, KST, PSK, RSI, RVI,
               SMA, STRSI, STOCH_D, STOCH_K, STOCH_D_SLOW, TEMA, TRIMA, TRIX, TSI, ULCER, ULTOSC, VAMA, VWAP, VOOSC, VPT,
               PVOI, NVOI, WILL_R, WMA, WWS, DISP, OSCP, PSY, DIU, DID, BIAS, VOLR, ARATIO, BRATIO, REX, HPR, LPR, VMOM, MPP, VARR, YYY)



colnames(BASIS) <- c("AB_UP","AB_DOWN","AD","MFM","ADL","ADX","CHOSC","ADO","APO","AR_POS","AR_NEG","AR_OSC","ATR","ATRP","AVOL","BB_UP","BB_LOW","BB_BW","BWW",
                     "VOLAT","PERC_B","CCI","CMF","CVOL","CMO","MAXX","MINN","CHAND_LONG","CHAND_SHORT","ROC","COPP","DPO","DMI","DONCHIAN","DEMA",
                     "DSS","EMV","FORCE","HULL","KAMA","KC_U","KC_M","KC_L","MQO_ALPHA","MQO_BETA","MQO_PRED","MQO_STD","MACD","MACDH",
                     "MAE_UP","MAE_LOW","MASS","RMF","MFI","MIDPOINT","MIDPRICE","MOM","NVI","NATR","OBV","SAR","TP","SS1","SS2","SR1","SR2","FS1",
                     "FS2","FR1","FR2","PD1","DS1","DR1","PC_UP","PC_DOWN","CHOPPINESS","PPO","PPOH","PVO","PVOH","PVI","PVT","KST","PSK","RSI","RVI",
                     "SMA","STRSI","STOCH_D","STOCH_K","STOCH_D_SLOW","TEMA","TRIMA","TRIX","TSI","ULCER","ULTOSC","VAMA","VWAP","VOOSC","VPT",
                     "PVOI","NVOI","WILL_R","WMA","WWS","DISP","OSCP","PSY","DIU","DID","BIAS","VOLR","ARATIO","BRATIO","REX","HPR","LPR","VMOM","MPP","VARR","YYY")

  lista[[j]] <- BASIS %>% na.omit()
  print(j)
  write.csv2(BASIS, file = paste0(vec[j],".csv"))
}

base_completa <- do.call( rbind, lista )

# Filtrando
base_completa2 <- base_completa["2011/2019-01"]

save(base_completa2, file = "basecompleta.RData")
write.csv2(base_completa2, file = "basecompleta.csv")
