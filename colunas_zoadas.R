

# Script colunas zoadas

paises <- c('ALE', 'BRA', 'CHN', 'FRA', 'JAP', 'UK', 'USA')

ativos_bug <-  list()
n_ativos <- list()
bugadas <- list()

for( i in 1: length(paises) ) {

  load( paste0('bases_finais/basecompleta_', paises[i],'.RData') )
  
  bugadas[[i]] <- apply (  base_completa2, 2, 
                      function(x)  sum( is.infinite(x) ) > 0  )
  
  bugadas[[i]] <- which(bugadas[[i]] > 0)
  print(bugadas[[i]])
  
  lista_bugados <- list()
  
  for ( k in 1:length(bugadas) ) {
    # linhas bugadas
    teste <- which( is.infinite(base_completa2[ , names(bugadas)[k] ] ) )
    
    lista_bugados[[k]] <-  base_completa2$ATIVO[ teste ] %>% unique()
  }
  
  ativos_bug[[i]] <- lista_bugados %>% unlist() %>% unique()
  n_ativos[[i]] <- length(  unique( base_completa2$ATIVO ) )
  rm(base_completa2)
}

colunas_zoadas <- unique( names( unlist(bugadas) ) )

write.csv2(colunas_zoadas, file = "colunas_zoadas.csv",
           row.names = F)




################  Ler cada Base e Salvar sem colunas zoadas. #################

col_zoadas <- read.csv2('colunas_zoadas.csv', header = T)

paises <- c('ALE', 'BRA', 'CHN', 'FRA', 'JAP', 'UK', 'USA')

  for( i in 1: length(paises) ) {
      load( paste0('bases_finais/basecompleta_', paises[i],'.RData') )
      base_final <- base_completa2[ , !(colnames(base_completa2) %in% col_zoadas$x) ]
      write.csv2(base_final, 
                 file = paste0( "nao_bugadas_",paises[i],'.csv'), 
                 row.names = F)
  }

teste <- read.csv2('bases_finais_sem_col_zoadas/nao_bugadas_USA.csv', header = T)
