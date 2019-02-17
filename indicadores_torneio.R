library(dplyr)
library(forecast)
library(quantmod)
library(QuantTools)

dados <- getSymbols('QQQ') 


fechamento <- QQQ$QQQ.Close
close <- QQQ$QQQ.Close
high <- QQQ$QQQ.High
low <- QQQ$QQQ.Low
open <- QQQ$QQQ.Open
volume <- QQQ$QQQ.Volume

N <- 10

Acceleration_Lower  <- forecast::ma( QQQ[,3] * (1 - 4*(QQQ[,2]-QQQ[,3] ) / (QQQ[,2] + QQQ[,3]) ), 
                                     order = N)
Acceleration_Upper  <- forecast::ma( QQQ$QQQ.High * (1 + 4*(QQQ$QQQ.High-QQQ$QQQ.Low ) / (QQQ$QQQ.High + QQQ$QQQ.Low) ), 
                                     order = N)
Acceleration_Middle <- forecast::ma( QQQ$QQQ.Close , order = N )
Accumulative_Distribution <- cumsum(((QQQ$QQQ.Close - QQQ$QQQ.Low) - (QQQ$QQQ.High - QQQ$QQQ.Close)) / ((QQQ$QQQ.High - QQQ$QQQ.Low) * QQQ$QQQ.Volume))

MFM = ((QQQ$QQQ.Close - QQQ$QQQ.Low) - (QQQ$QQQ.High - QQQ$QQQ.Close)) / (QQQ$QQQ.High - QQQ$QQQ.Low)
MFV = MFM * QQQ$QQQ.Volume

ADL <- 0
for ( i in 2:dim(QQQ)[1] ) { ADL[i] <- ADL[i-1] + MFV[i]}

n_fast = 10
n_slow = 20
APO <- ema( QQQ$QQQ.Close , n = n_fast ) - ema( QQQ$QQQ.Close , n = n_slow )

desde_max <- 0
desde_min <- 0

for (i in 2:7) {
  
  desde_max[i] <- ifelse( i == which.max(teste[1:i]) , 1 , i - which.max(teste[1:i]) )
  desde_min[i] <- ifelse( i == which.min(teste[1:i]) , 1 , i - which.min(teste[1:i]) )
  
}

AR_h
AR_d
ARO  # ARO = AR_h - AR_d

close_low  <- c()
high_close <- c()
high_low   <- c()

for (i in 2:nrow(QQQ)) {
  high_low      <- QQQ$QQQ.High - QQQ$QQQ.Low
  high_close[i] <- as.numeric( QQQ$QQQ.High[i] ) - as.numeric( QQQ$QQQ.Close[i-1] )
  close_low[i]  <- as.numeric( QQQ$QQQ.Close[i-1] ) - as.numeric( QQQ$QQQ.Low[i] )
}

tr = data.frame(High_menos_Low     =  high_low,
                High_menos_Close_1 =  high_close,
                Close_1_menos_Low  =  close_low )


TR =   apply(tr, 1, max, na.rm = TRUE)

ATR_inicial <- forecast::ma(TR , order = N) # ATR[1] ?  ---- Confuso.. ----

for (i in 2:length(TR))  ATR[i] = ( (N - 1) * ATR[i-1] + TR[i] ) / length(ATR)

ATRP = (ATR / QQQ$QQQ.Close) * 100  # Nao feito



# Average Volume

avol = ma(QQQ$QQQ.Volume , N)


# BBANDS

BB_m = ma(QQQ$QQQ.Close , N)  # Usa o Close mesmo???

BW = sum( (QQQ$QQQ.Close - as.numeric( ma(QQQ$QQQ.Close) ))^2 ) / length(QQQ$QQQ.Close) ### dimensao diferente. Confuso.

# lower and upper band
BB_l = BB_m - 2 * (BW)
BB_l = BB_m + 2 * (BW)
BBW  =
  
  # ?? (Pagina 11)
  CCI = ...


# Money Flow Multiplier

MFM = ((QQQ$QQQ.Close - QQQ$QQQ.Low) - (QQQ$QQQ.High - QQQ$QQQ.Close)) / (QQQ$QQQ.High - QQQ$QQQ.Low)
MFV = MFM * QQQ$QQQ.Volume
CMF = sum(MFV[1:N]) / sum(QQQ$QQQ.Volume[1:N]) #verificar N

CHOSC = ema(ADL, n = 3) - ema(ADL, n = 10)
cvol_ema <- ema( QQQ$QQQ.High - QQQ$QQQ.Low , n = N)

CVOL =  (cvol_ema - cvol_ema[i-n]) / cvol_ema[i-10]     #Duvida Penge

Psum = sum( diff( QQQ$QQQ.Close ) )       #paraperodosonde?
Nsum = abs(sum( diff( QQQ$QQQ.Close ) ) ) #paraperodosonde?

CMO = (Psum - Nsum / (Psum + Nsum)) * 100


# Pagina 14

DPO = c()
sma_close = sma(QQQ$QQQ.Close,N) %>% na.omit()

for (k in 7:length(QQQ$QQQ.Close)) {
  DPO[k] <- QQQ$QQQ.Close [ k - round( N/2 + 1 ) ] - sma_close[k-6]
}

PDM = diff(QQQ$QQQ.High)
NDM = c()
for(i in 1: (length(QQQ$QQQ.Low) - 1) )  {
  NDM[i] = as.numeric( QQQ$QQQ.Low[i] ) - as.numeric( QQQ$QQQ.Low[i+1] )
}

WPDM = c(NA,NA)

for (i in 3:length(PDM) ){
  WPDM[i] = PDM[i-1] - mean(PDM[1:i-1],na.rm=TRUE) + as.numeric( PDM[i] )
}

WNDM = c(NA,NA)

for (i in 3:length(PDM) ){
  WNDM[i] = NDM[i-1] - mean(NDM[1:i-1],na.rm=TRUE) + as.numeric( NDM[i] )
}

WTR = c(NA,NA)
for (i in 3:length(TR)){
  WTR[i] = TR[i-1] - mean(TR[1:i-1],na.rm=TRUE) + as.numeric( TR[i] )
}

PDI = ( WPDM / WTR ) * 100
NDI = ( WNDM / WTR ) * 100
DD = abs(PDI - NDI)
DMI = ( DD / (PDI + NDI)  ) * 100


Donchian = DonchianChannel(QQQ[,2:3])


DEMA = 2 * ( TTR::EMA(QQQ$QQQ.Close , N) - TTR::EMA( TTR::EMA(QQQ$QQQ.Close, N ), N   )  )

minimos = c()
maximos = c()
for (i in 1:length(QQQ$QQQ.Low)) { 
  minimos[i] = min(QQQ$QQQ.Low[1:i])
  maximos[i] = max(QQQ$QQQ.High[1:i])
}

close_menos_min = c()
high_menos_min = c()

for ( i in 1:length(QQQ$QQQ.Low)){
  close_menos_min[i] = QQQ$QQQ.Close[i] - minimos[i]
  high_menos_min[i]  = maximos[i] - minimos[i]
}

DSS = (TTR::EMA( TTR::EMA( close_menos_min)  ) / TTR::EMA( TTR::EMA( high_menos_min) ) ) * 100

high = QQQ$QQQ.High
low  = QQQ$QQQ.Low
volume = QQQ$QQQ.Volume
close = QQQ$QQQ.Close

hl_tm1 = NA
for (i in 2:length(high)) hl_tm1[i] = high[i-1] + low[i-1]

prov = ((high - low)/2 - hl_tm1/2 )  /  ((volume/100000000) / (high-low))

EMV = TTR::SMA(prov,n = 14)  

## EMA???

wma_close = TTR::WMA( close[i] )

FORCE = TTR::EMA(diff(close)*volume)


HMA = WMA() ## nao entendi

## Pagina 18 
n1 = 10 ; nf = 2 ; ns = 30

for (i in 1:length(close)) {
  abs1 = abs(close)
}
ER = abs(diff(close,lag = n1)) / 
  
  KAMA ### Falta..

KC_M = TTR::EMA( high + low + close / 3 , n = 20)  # no paper n1 = 20
KC_L = KC_M - 2* ( ATR (QQQ[,2:4], n = 10)$atr ) # no paper n = 10. Quem é o X ???
KC_U = KC_M + 2* ( ATR (QQQ[,2:4], n = 10)$atr ) 


macd = MACD(volume)

macdh = macd$macd - TTR::EMA(macd$macd, n=9)


MAE_UP = SMA(close) + SMA(close)/4
MAE_LOW = SMA(close) - SMA(close)/4

high_menos_low = high - low
prov2 = TTR::EMA( high_menos_low, n = 9) / (TTR::EMA( TTR::EMA( high_menos_low, n = 9), n = 9 ))

MASS_ind = sum(prov2[1:25]) # ???? sum 1:25 ?????
































# BORIS PAROU AQUI
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
RSI <- RSI(close, n = 10, maType = TTR::SMA)
#############################
rvi <- FALTA
#############################
SMA <- SMA(close, n = 10)
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
sd do mqo <- FALTA
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
vortex <- FALTA
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
indic_up(fechamento,n = 3)

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
#############################
#############################
#############################
#############################
#############################

#############################

#############################

#############################

#############################

#############################

#############################


