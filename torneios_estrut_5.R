rm(list=ls());gc()

library(dplyr)
library(forecast)
library(quantmod)
library(QuantTools)
library(TTR)

# US 97; UK 47, FRA 30; ALE 17; CHI 45; JAP 49; BRA 47

vec <- c("AAPL","ABBV","ABT","ACN","AGN","AIG","ALL","AMGN","AMZN","AXP","BA","BAC",#"BIIB",
         "BK","BKNG","BLK","BMY",#"BRK.B",
         "C","CAT","CELG",#"CHTR","COST",
         "CL","CMCSA","COF","COP",
         "CSCO","CVS","CVX","DHR","DIS","DUK","DWDP","EMR","EXC","F","FB","FDX","FOX",
         "FOXA","GD","GE",#"GILD",
         "GM","GOOG","GOOGL","GS","HAL","HD","HON","IBM","INTC",
         "JNJ","JPM","KHC","KMI","KO","LLY","LMT","LOW","MA","MCD","MDLZ","MDT","MET","MMM",
         "MO","MRK","MS","MSFT","NEE","NFLX","NKE","NVDA","ORCL","OXY","PEP","PFE","PG",
         "PM","PYPL","QCOM","RTN","SBUX","SLB","SO","SPG","T","TGT","TXN","UNH","UNP","UPS",
         "USB","UTX","V","VZ","WBA","WFC","WMT","XOM") ## vetor com empresas

ukk <- c("III","ADM","AAL",#"ANTO",
         "AHT","ABF","AZN","AUTO",#"AV",
         "BA",#"BARC","BDEV",
         "BKG","BHP","BP",#"BATS",
         #"BLND",#"BT-A","BNZL","BRBY","CCH","MKS",
         "CCL","CNA","CPG","CRH",#"CRDA","DGE","HIK","HSX","IMB","ITRK","ITV","LSE","LAND",
         "DCC","DLG","EZJ","EVR","GSK","HL",
         "INF","IHG","IAG",
         "JE","KGF","MRO",
         #"MRW","NG","RB","RMV","RSA","TUI",
         "MCRO","PPB","PSON","PSN","PRU","REL","RTO","RIO","RBS",
         #"EXPN","FERG","FRES","GLEN","HLMA","HSBA","JMAT","LGEN","LLOY","MNDI",
         #"NXT","NMC","OCDO","PHNX","RR","RDSA","SBRY","SMT","SGRO","SN","SMDS","SSE","STJ","TW","ULVR","UU","WTB",
         "SGE","SDR","SVT","SMIN","SKG","SPX","STAN","SLA","TSCO","VOD","WPP")

franssa <- c("AC","AI","AIR","MT","ATO","CS","BNP","EN","BN","EL",
             "KER","OR","LHN","LR","MC","ML","ORA","UG","PUB","SAF","SGO",
             "SAN","SU","GLE",#"SW","CA","ENGI","RNO","SOLB","VIE","CAP","ACA","RI","URW",
             "STM","FTI","FP","FR","DG","VIV")

alemanha <- c("ADS","ALV","BAS","BAYN","BEI","BMW","CON",
              #"1COV","DB1","EOAN","FRE","HEN3","MUV2","VOW3","DBK","LHA","FME","LIN","TKA","WDI"
              "DAI","DPW","DTE","HEI","IFX","MRK","RWE","SAP","SIE","VNA")

brasil <- c("ABEV3.SA","BTOW3.SA","B3SA3.SA","BBAS3.SA","BBDC4.SA","BRAP4.SA","BRKM5.SA","BRFS3.SA",
            "CCRO3.SA","CMIG4.SA","CIEL3.SA","CPLE6.SA","CSAN3.SA","CPFE3.SA","CSNA3.SA","CYRE3.SA",
            "ECOR3.SA","ENBR3.SA","ELET6.SA","EMBR3.SA","EGIE3.SA","ESTC3.SA",#"FIBR3.SA","KLBN11.SA","SMLE3.SA","SUZB5.SA","VVAR11.SA",
            "GGBR4.SA","GOLL4.SA","PCAR4.SA","HYPE3.SA","IGTA3.SA","ITUB4.SA","ITSA4.SA","JBSS3.SA",
            "KROT3.SA","RENT3.SA","LAME4.SA","LREN3.SA","GOAU4.SA","MRVE3.SA","NATU3.SA","PETR4.SA",
            "RAIL3.SA","SBSP3.SA","SANB11.SA","VIVT4.SA","TIMP3.SA","UGPA3.SA","USIM5.SA","VALE3.SA","WEGE3.SA")

china <- c("600000.SS","600016.SS","600028.SS","600029.SS","600030.SS","600036.SS","600048.SS",
           "600050.SS","600100.SS","600104.SS","600111.SS","600340.SS",#"600485.SS",
           "600518.SS","600519.SS","600547.SS","600606.SS","600837.SS","600887.SS",
           #"600919.SS","601229.SS","601881.SS","601989.SS"
           "600958.SS","600999.SS","601006.SS","601088.SS","601166.SS","601169.SS","601186.SS",
           "601198.SS","601211.SS","601288.SS","601318.SS","601328.SS","601336.SS",
           "601390.SS","601398.SS","601601.SS","601628.SS","601668.SS","601688.SS","601766.SS",
           "601788.SS","601800.SS","601818.SS","601857.SS","601901.SS","601985.SS","601988.SS")

japao <- c("7203.T","9984.T","6861.T","9437.T","8306.T","4502.T","9433.T","9432.T",
           #"9434.T",
           "6758.T","9983.T","7182.T","7267.T","8316.T","6098.T","9022.T","8058.T","4661.T",
           "7974.T","8411.T","7751.T","4452.T","4519.T","6594.T","9020.T","4063.T","8766.T",
           "6954.T","7201.T","6367.T","6981.T","3382.T","2914.T","4568.T","6501.T","6902.T",
           "4503.T","5108.T","4911.T","8001.T","6503.T","8031.T","6273.T","8802.T","7741.T",
           "8801.T","8035.T","4901.T","4543.T","6178.T")

setwd("C:/Users/b05652877465/Desktop/CAYAN TORNEIOS/CROSS-TESIS/coleta_global_identif")

vec <- japao

#BAIXA DADOS PARA CADA EMPRESA LISTADA # travou no 18
for (esse in 1:length(vec)){
  dados <- getSymbols(as.character(vec[esse]),from = '2008-01-01',to = '2019-03-01')
  QQQ <- get(as.character(vec[esse]))
}

# getSymbols("CIEL3",from = '2008-01-01',to = '2019-03-01')

#dados <- getSymbols('AAPL',from = '2000-01-01',to = '2019-01-31') 
lista <- list()

for (j in 1:length(vec)){ #vec[13, 21, 43] deu erro
  QQQ <- get(vec[j])
  QQQ <- QQQ %>% na.omit()
  
  # QQQ[which(QQQ[,1] %>% is.na),1] <- 0.0001
  # QQQ[which(QQQ[,2] %>% is.na),2] <- 0.0002
  # QQQ[which(QQQ[,3] %>% is.na),3] <- 0.0003
  # QQQ[which(QQQ[,4] %>% is.na),4] <- 0.0004
  # QQQ[which(QQQ[,5] %>% is.na),5] <- 0.0005
  
  fechamento <- QQQ[,4]
  close <- QQQ[,4]
  high <- QQQ[,2]
  low <- QQQ[,3]
  open <- QQQ[,1]
  volume <- QQQ[,5]
  
  # volume[volume==0] <- mean(volume[volume!=0])
  volume[volume==0] <- mean(volume)
  
  # fechamento[which(fechamento %>% is.na())] <- 0.00004
  # close[which(close %>% is.na())] <- 0.00006
  # high[which(high %>% is.na())] <- 0.00002
  # low[which(low %>% is.na())] <- 0.00003
  # open[which(open %>% is.na())] <- 0.00001
  # volume[which(volume %>% is.na())] <- 0.00005
  
  #############################
  # YYY <- sign(diff(close))
  # YYY[which(YYY==0)] <- -1
  
  # pp <- c(1,2,3,4,5)
  # 
  # c(diff(pp),NA)/pp
  # 
  # c(diff(prov),NA)/prov
  # 
  # c(diff(as.vector(close)),NA)/close
  # 
  # vari_abs <- c(diff(as.vector(close)),NA)
  # vari_perc <- c(diff(as.vector(close)),NA)/close
  # prov <- as.xts(lead(c(diff(as.vector(close)),NA)/close,order.by = index(close))
  
  YYY <- as.xts(lead(as.vector(sign(diff(close)))),order.by = index(close))
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
  MFM = ((close - low) - (high - close)) / ((high - low)+0.0001)
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
  prov[which(prov %>% is.na())] <- 0
  prov[which(prov %>% is.infinite())] <- max(volume)
  
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
  ############################
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
  EMA <- TTR::EMA(close, n = 10)
  #############################
  # ATIVO <- rep(vec[j], nrow(QQQ))
  ATIVO <- j
  # ATIVO <- as.xts(rep(vec[j], nrow(QQQ)),order.by = index(close))
  #############################
  
  
  BASIS <- cbind(ATIVO, AB_UP,AB_DOWN, AD, MFM, ADL, ADX, CHOSC, ADO, APO, AR_POS, AR_NEG, AR_OSC, ATR, ATRP, AVOL, BB_UP, BB_LOW, BB_BW, BWW,
                 VOLAT, PERC_B, CCI, CMF, CVOL, CMO, MAXX, MINN, CHAND_LONG, CHAND_SHORT, ROC, COPP, DPO, DMI, DONCHIAN, DEMA,
                 DSS, EMA, EMV, FORCE, HULL, KAMA, KC_U, KC_M, KC_L, MQO_ALPHA, MQO_BETA, MQO_PRED, MQO_STD, MACD, MACDH, open, close, volume,
                 MAE_UP, MAE_LOW, MASS, RMF, MFI, MIDPOINT, MIDPRICE, MOM, NVI, NATR, OBV, SAR, TP, SS1, SS2, SR1, SR2, FS1,
                 FS2, FR1, FR2, PD1, DS1, DR1, PC_UP, PC_DOWN, CHOPPINESS, PPO, PPOH, PVO, PVOH, PVI, PVT, KST, PSK, RSI, RVI,
                 SMA, STRSI, STOCH_D, STOCH_K, STOCH_D_SLOW, TEMA, TRIMA, TRIX, TSI, ULCER, ULTOSC, VAMA, VWAP, VOOSC, VPT,
                 PVOI, NVOI, WILL_R, WMA, WWS, DISP, OSCP, PSY, DIU, DID, BIAS, VOLR, ARATIO, BRATIO, REX, HPR, LPR, VMOM, MPP, VARR, YYY)
  
  
  
  colnames(BASIS) <- c("ATIVO", "AB_UP","AB_DOWN","AD","MFM","ADL","ADX","CHOSC","ADO","APO","AR_POS","AR_NEG","AR_OSC","ATR","ATRP","AVOL","BB_UP","BB_LOW","BB_BW","BWW",
                       "VOLAT","PERC_B","CCI","CMF","CVOL","CMO","MAXX","MINN","CHAND_LONG","CHAND_SHORT","ROC","COPP","DPO","DMI","DONCHIAN","DEMA",
                       "DSS","EMA","EMV","FORCE","HULL","KAMA","KC_U","KC_M","KC_L","MQO_ALPHA","MQO_BETA","MQO_PRED","MQO_STD","MACD","MACDH", "OPEN","CLOSE","VOLUME",
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

save(base_completa2, file = "basecompleta_JAP.RData")
write.csv2(base_completa2, file = "basecompleta_JAP.csv")