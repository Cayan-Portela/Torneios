library(dplyr)

paises <- c('ALE', 'BRA', 'CHN', 'FRA', 'JAP', 'UK', 'USA')

i = 7



  base_completa2 <- read.csv2(paste0("bases_finais_sem_col_zoadas/nao_bugadas_", paises[i], ".csv"),
                              header = T)


################ ICI!!!!!!!!!!!!!!!!!!!
base_completa2 <- base_completa2[ , -which(colnames(base_completa2) == "ATIVO") ]

YYY <- which(colnames(base_completa2)=="YYY")

base_completa2[,-YYY] <- apply( base_completa2[ , -YYY], 2,
                                function(x){(x-mean(x))/sd(x)} )

base_completa2$YYY <- ifelse(base_completa2$YYY == 1, 1, 0)
gc()
set.seed(96500)

model <- glm( YYY ~ .,
              family = binomial(link = "logit"),
              data = base_completa2 )

n <- nrow(base_completa2)

model_step <- step(model, direction = "both", k = log(n))


write.csv( rownames( summary(model_step)$coefficients )[-1],
           paste0( "Feature_Selection/stepwise_tudo_",paises[i],".csv"),
           row.names = FALSE )

### Tab 2 


rm(model,model_step)
gc()

model <- glm( YYY ~ SMA+WMA+EMA+MOM+STOCH_K+STOCH_D+
                STOCH_D_SLOW+RSI+MACD+WILL_R+
                CCI+ROC+DISP+OSCP+PSY+BIAS+
                VOLR+ATR+BB_UP+BB_LOW+
                DMI+KC_U+KC_L+TRIMA+MAE_UP+MAE_LOW+
                REX+NVI+PVI+VAMA+HPR+LPR+MAXX+MINN+
                VMOM+MPP+PPO+SAR+OBV+VOLAT+MFI+
                MQO_BETA+OPEN+CLOSE+VOLUME,
                family = binomial(link = "logit"),
                data = base_completa2 )

n <- nrow(base_completa2)

model_step <- step(model, direction = "both", k = log(n))

write.csv( rownames( summary(model_step)$coefficients )[-1],
           paste0( "Feature_Selection/stepwise_tab1_",paises[i],".csv"),
           row.names = FALSE )

rm(model, model_step)
gc()


