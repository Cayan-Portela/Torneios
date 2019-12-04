library(dplyr)

paises <- c('ALE', 'BRA', 'CHN', 'FRA', 'JAP', 'UK', 'USA')

k = 7

base_completa2 <- read.csv2(paste0("bases_finais_sem_col_zoadas/nao_bugadas_", paises[k], ".csv"),
                            header = T)

# TORNEIOS TUDO

###### Definindo os grupos #######

prov <- base_completa2[1:round(0.75*nrow(base_completa2)),]
ncol(prov)
prov$YYY <- (prov$YYY+1)/2

# ROUND 1: 100/125

grupo <- list()



coluna_Y <- which(colnames(prov) == "YYY")
coluna_Ativos <- which(colnames(prov) == "ATIVO")
# Para o sorteio so entram indices das variaveis independentes
preditores <- (1:coluna_Y)[-c(coluna_Y, coluna_Ativos) ]

set.seed(96500)
grupo[[1]] <- sample( preditores , 21)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff( preditores ,unlist(grupo)),
                       size = 24)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 15){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos,coluna_Y)] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

# ROUND 2: 75/100

grupo <- list()
set.seed(96500)
grupo[[1]] <- sample( sobraron , 15 )

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(sobraron, unlist(grupo)),
                       size = 15)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 10){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos, coluna_Y )] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

# ROUND 3: 50/75

#prov <- prov[,c(sobraron,ncol(prov))]

grupo <- list()
grupo[[1]] <- sample( sobraron , 10)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(sobraron ,unlist(grupo)),
                       size = 10)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 5){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos, coluna_Y )] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

## ROUND 4: 15/25

grupo <- list()
grupo[[1]] <- sample( sobraron , 5)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(sobraron ,unlist(grupo)),
                       size = 5)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 3){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos, coluna_Y )] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

lista_final_tudo <- colnames(prov[ , sobraron])

# Padronizar como 'i' dos paises

write.csv( lista_final_tudo,
           paste0( "Feature_Selection/torneios_tudo_",paises[k],".csv"),
           row.names = FALSE )

#### Ate aqui ok! ####

# TORNEIOS T1

###### Definindo os grupos #######

prov <- base_completa2[1:round(0.75*nrow(base_completa2)),c("SMA","WMA","EMA","MOM","STOCH_K","STOCH_D","STOCH_D_SLOW",
                                                            "RSI","MACD","WILL_R","CCI","ROC","DISP","OSCP","PSY",
                                                            "BIAS","VOLR","ATR","BB_UP",
                                                            "BB_LOW","DMI","KC_U","KC_L","TRIMA","MAE_UP","MAE_LOW",
                                                            "REX","NVI","PVI","VAMA","HPR","LPR","MAXX","MINN","VMOM",
                                                            "MPP","PPO","SAR","OBV","VOLAT","MFI","MQO_BETA",
                                                            "OPEN","CLOSE","VOLUME","YYY")]
ncol(prov)
prov$YYY <- (prov$YYY+1)/2

# ROUND 1: 40/51

grupo <- list()
coluna_Y <- which(colnames(prov) == "YYY")

set.seed(96500)
# Para o sorteio so entram indices das variaveis independentes
preditores <- (1:coluna_Y)[-coluna_Y]

grupo[[1]] <- sample( preditores , 9)
for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(preditores ,unlist(grupo)),
                       size = 9)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 5){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos,ncol(prov))] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

# ROUND 2: 30/40

grupo <- list()
grupo[[1]] <- sample( sobraron , 5)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(sobraron ,unlist(grupo)),
                       size = 5)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 2){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos,ncol(prov))] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

lista_final_t1 <- colnames(prov[,sobraron])

# Padronizar como 'i' dos paises

write.csv( lista_final_t1,
           paste0( "Feature_Selection/torneios_tab1_",paises[k],".csv"),
           row.names = FALSE )


