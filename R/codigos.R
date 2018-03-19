#' Cria labels de pico, vale e nivel
#'
#' Essa funcao classifica a serie temporal em pico, vale e nivel.
#'
#' @param y Vector. Serie temporal.
#' @param lag Numeric. Tamanho da janela desejada para media movel
#' @param threshold Numeric. Numero de desvios padroes.
#' @param influence Numeric. Influence = 0 recalcula o valor do threshold.
#' @return list com os seguintes objetos: signals, avgFilter and stdFilter.
#' @export
#' @references https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data
#' @examples
#' set.seed(412)
#' df = tibble(serie = rnorm(100, 10, 1))
#' plot(df$serie, type = 'l')
#' df$sinal <- timeSeries::classificadorPico(df$serie, lag = 6, threshold = 2, influence = 0.1)$signals
#' head(df)


classificadorPico <- function(y,lag,threshold,influence) {
  signals <- rep(2,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag])
  stdFilter[lag] <- sd(y[0:lag])
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 3;
      } else {
        signals[i] <- 1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 2
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i])
    stdFilter[i] <- sd(filteredY[(i-lag):i])
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
}



#' Ultima vez que apareceu algum determinado sinal no lag 1
#'
#' Ultima vez que apareceu sinal (pico, vale ou  nivel) baseado na coluna de sinais no
#' lag 1.
#' @param df dataframe.
#' @param defasagem Numeric.
#' @return dataframe.


ultimoSinal <- function(df, defasagem = 1){

  # if(df$sinal[defasagem] == 3){
  #
  #   df$last_pico <- NA
  #
  #   for(j in 1:(defasagem)){
  #     df$last_pico[j] = 1
  #   }
  #
  #   df$last_nivel <- NA
  #
  #   for(j in 1:defasagem){
  #     df$last_nivel[j] = j
  #   }
  #
  #   df$last_vale <- NA
  #
  #   for(j in 1:defasagem){
  #     df$last_vale[j] = j
  #   }
  #
  # }else if(df$sinal[defasagem] == 2){
  #
  #   df$last_pico <- NA
  #
  #   for(j in 1:defasagem){
  #     df$last_pico[j] = j
  #   }
  #
  #   df$last_nivel <- NA
  #
  #   for(j in 1:defasagem){
  #     df$last_nivel[j] = 1
  #   }
  #
  #   df$last_vale <- NA
  #
  #   for(j in 1:defasagem){
  #     df$last_vale[j] = j
  #   }
  #
  # }else{
  #
  #   df$last_pico <- NA
  #
  #   for(j in 1:defasagem){
  #     df$last_pico[j] = j
  #   }
  #
  #   df$last_nivel <- NA
  #
  #   for(j in 1:defasagem){
  #     df$last_nivel[j] = j
  #   }
  #
  #   df$last_vale <- NA
  #
  #   for(j in 1:(defasagem)){
  #     df$last_vale[j] = 1
  #   }
  #
  # }

  df$last_vale <- NA
  df$last_pico <- NA
  df$last_nivel <- NA

  for(i in (1+defasagem):(nrow(df)+defasagem)){
    if(i <= nrow(df)){
      if(df$sinal[i-defasagem] == 3){
        df$last_pico[i] = 1
      }
      if(df$sinal[i-defasagem] != 3){
        df$last_pico[i] = df$last_pico[i-1] + 1
      }
    }else{
      df$last_pico[i-defasagem] = df$last_pico[i-1-defasagem] + 1
    }
  }




  for(i in (1+defasagem):(nrow(df)+defasagem)){
    if(i <= nrow(df)){
      if(df$sinal[i-defasagem] == 2){
        df$last_nivel[i] = 1
      }
      if(df$sinal[i-defasagem] != 2){
        df$last_nivel[i] = df$last_nivel[i-1] + 1
      }
    }else{
      df$last_nivel[i-defasagem] = df$last_nivel[i-1-defasagem] + 1
    }
  }



  for(i in (1+defasagem):(nrow(df)+defasagem)){
    if(i <= nrow(df)){
      if(df$sinal[i-defasagem] == 1){
        df$last_vale[i] = 1
      }
      if(df$sinal[i-defasagem] != 1){
        df$last_vale[i] = df$last_vale[i-1] + 1
      }
    }else{
      df$last_vale[i-defasagem] = df$last_vale[i-1-defasagem] + 1
    }
  }

  return(df)
}


#' Frequency Signal Detection
#'
#' Calcula a frequencia das colunas de last_sinal de acordo com uma janela rolante.
#' @param df Dataframe.
#' @param janela Numeric. Janela rolante que por default 6.
#' @return Dataframe



freqSignal <- function(df, janela = 6){

  tamanho_df = length(df$sinal)

  if(length(df$sinal) <= janela){
    stop("O tamanho da janela rolante é igual ou maior que a série temporal")
  }else{

    df$freq_nivel = NA
    df$freq_pico = NA
    df$freq_vale = NA

    for(w in 1:(tamanho_df - (janela - 1) ) ){ # - 1 para olhar no lag 1. Nao utilizando info do periodo atual.

      sinal_janela = df$sinal[ w:(w + (janela - 1) ) ]

      df[( (janela - 1) + w) , 'freq_nivel'] = length(which(sinal_janela == 2)) / janela
      df[( (janela - 1) + w) , 'freq_pico'] = length(which(sinal_janela == 3)) / janela
      df[( (janela - 1) + w) , 'freq_vale'] = length(which(sinal_janela == 1)) / janela

    }
  }

  return(df)
}





#' Intervalo maximo de aparicao de um sinal
#'
#' Dado uma janela rolante esta funcao retorna o valor maximo da aparicao de um
#' determinado sinal (pico, vale ou nivel).
#'
#' @param df Dataframe.
#' @param janela Numeric. Tamanho da janela rolante. Por default 6.
#' @return Dataframe.


maxIntervalSignal <- function(df, janela = 6){

  tamanho_df = length(df$sinal)

  if(length(df$sinal) <= janela){
    stop("O tamanho da janela rolante é igual ou maior que a série temporal")
  }else{

    df$intervalo_max_last_nivel = NA
    df$intervalo_max_last_pico = NA
    df$intervalo_max_last_vale = NA

    for(w in 1:(tamanho_df - (janela - 1) ) ){ # - 1 para olhar no lag 1. Nao utilizando info do periodo atual.

      last_nivel_janela = df$last_nivel[ w:(w + (janela - 1) ) ]
      last_pico_janela = df$last_pico[ w:(w + (janela - 1) ) ]
      last_vale_janela = df$last_vale[ w:(w + (janela - 1) ) ]



      df[( (janela - 1) + w) , 'intervalo_max_last_nivel'] = max(last_nivel_janela)
      df[( (janela - 1) + w) , 'intervalo_max_last_pico'] = max(last_pico_janela)
      df[( (janela - 1) + w) , 'intervalo_max_last_vale'] = max(last_vale_janela)

    }
  }

  return(df)
}



#' Intervalo medio de aparicao de um sinal
#'
#' Dado uma janela rolante esta funcao retorna quantidade de tempo media da aparicao de um
#' determinado sinal (pico, vale ou nivel).
#'
#' @param df Dataframe.
#' @param janela Numeric. Tamanho da janela rolante. Por default 6.
#' @return Dataframe.


avgIntervalSignal <- function(df, janela = 6){


  tamanho_df = length(df$sinal)

  if(length(df$sinal) <= janela){
    stop("O tamanho da janela rolante é igual ou maior que a série temporal")
  }else{

    df$intervalo_medio_last_nivel = NA
    df$intervalo_medio_last_pico = NA
    df$intervalo_medio_last_vale = NA

    for(w in 1:(tamanho_df - (janela - 1) ) ){ # - 1 para olhar no lag 1. Nao utilizando info do periodo atual.

      last_nivel_janela = df$last_nivel[ w:(w + (janela - 1) ) ]
      last_pico_janela = df$last_pico[ w:(w + (janela - 1) ) ]
      last_vale_janela = df$last_vale[ w:(w + (janela - 1) ) ]



      df[( (janela - 1) + w) , 'intervalo_medio_last_nivel'] = mean(last_nivel_janela)
      df[( (janela - 1) + w) , 'intervalo_medio_last_pico'] = mean(last_pico_janela)
      df[( (janela - 1) + w) , 'intervalo_medio_last_vale'] = mean(last_vale_janela)

    }
  }

  return(df)
}



#'  Features de sinais
#'
#'  Cria todas as features de sinais para o modelo de classificacao.
#'
#'  @param df dataframe
#'  @param defasagem Numeric. Por default utiliza o lag 1 da coluna de sinais.
#'  @param janela Numeric. Tamanho da janela rolante. Por default 6.
#'  @return dataframe
#'  @export
#'  @examples
#' set.seed(412)
#' df = tibble(serie = rnorm(100, 10, 1))
#' df$sinal <- timeSeries::classificadorPico(df$serie, lag = 6, threshold = 2, influence = 0.1)$signals
#' timeSeries::featuresSinais(df, janela = 3) %>% View

featuresSinais <- function(df, defasagem = 1, janela = 6) {

  condicao = 'sinal' %in% colnames(df)

  if(condicao == FALSE){
    stop("A coluna de sinais deve ser renomeada como sinal no dataframe.")
  }

  df2 = ultimoSinal(df, defasagem)
  df3 =  freqSignal(df2, janela)
  df4 = maxIntervalSignal(df3, janela)
  df5 = avgIntervalSignal(df4, janela)

  return(df5)
}





