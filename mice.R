library(dplyr)
library(mice)
library(readxl)

# Carrega dados
dados <- read_excel("Ranking-dos-Estados-2023.xlsx", 
                        sheet = "Valores")
dados_aux <- dados[,3:ncol(dados)]



falhas <- flux(dados_aux)
write.csv(falhas, "falhas.csv")

# pobs = Proportion observed, influx = Influx
# outflux = Outflux ainb = Average inbound statistic aout = Average outbound statistic 
# fico = Fraction of incomplete cases among cases with Yj observed

fluxplot(dados_aux)
md.pattern(dados_aux, rotate.names = TRUE)


preenche_falhas <- function(dados) {
  # Contagem de valores ausentes por coluna
  valores_ausentes <- sapply(dados, function(x) sum(is.na(x)))
  
  # Total de linhas no dataframe
  total_linhas <- nrow(dados)
  
  # Obtendo o total de colunas
  total_colunas <- ncol(dados)
  
  # Percentual de valores ausentes por coluna
  percentual_ausentes <- valores_ausentes / total_linhas * 100
  
  # Exibir percentual de valores ausentes por coluna
  print(percentual_ausentes)
  
  # Calculo do percentual total de valores ausentes
  total_valores_ausentes <- sum(valores_ausentes)
  total_valores_dataframe <- total_linhas * total_colunas
  percentual_total_ausentes <- total_valores_ausentes / total_valores_dataframe * 100
  
  # Imprimir o percentual total de valores ausentes
  print(paste0("Percentual total de valores ausentes: ", percentual_total_ausentes, "%"))
  
  imp <- mice(dados)
  return(complete(imp))  
}

dados_prenchidos <- preenche_falhas(dados_aux)
print(dados_prenchidos)

# Salvar o dataframe em um arquivo CSV
write.csv(dados_prenchidos, "dados_com_medias_2023b.csv", row.names = FALSE)