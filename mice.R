library(dplyr)
library(FuzzyR)


# Carrega dados
dados_aux <- read_excel("Ranking-dos-Estados-2022.xlsx", 
                        sheet = "Valores")
dados_aux <- dados_aux[,4:50]


preenche_falhas <- function(dados) {
  # Contagem de valores ausentes por coluna
  valores_ausentes <- sapply(dados, function(x) sum(is.na(x)))
  
  # Total de linhas no dataframe
  total_linhas <- nrow(dados)
  
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
  
  # Preencher falhas
  it <- 25
  dados_imputados <- mice(dados, 
                          m = it, 
                          maxit = 500, 
                          method = "pmm", 
                          seed = 42)
  
  
  # Extrair cada conjunto de dados imputado completo e armazená-los em uma lista
  conjuntos_imputados <- lapply(1:it, function(i) complete(dados_imputados, i))
  
  # Calcular a média dos valores imputados em cada coluna
  medias_imputadas <- sapply(conjuntos_imputados, colMeans)
  
  # Criar um dataframe com a mesma estrutura que 'dados' e preencher com as médias dos valores imputados
  dados_com_medias <- dados
  for (i in 1:total_colunas) {
    dados_com_medias[is.na(dados[, i]), i] <- medias_imputadas[i]
  }
  return(dados_com_medias)
}

dados_prenchidos <- preenche_falhas(dados_aux)
print(dados_prenchidos)

# Salvar o dataframe em um arquivo CSV
write.csv(dados_prenchidos, "dados_com_medias.csv", row.names = FALSE)