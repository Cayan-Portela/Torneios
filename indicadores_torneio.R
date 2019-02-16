library(dplyr)
library(forecast)
library(quantmod)
library(QuantTools)

dados <- getSymbols('QQQ') 


fechamento <- QQQ$QQQ.Close

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


