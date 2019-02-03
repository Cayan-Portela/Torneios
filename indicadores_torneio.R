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
DPO = QQQ$QQQ.Close [ round( N/2 +1 ) ] - ma(QQQ$QQQ.Close , N)
