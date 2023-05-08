# Carregar pacotes necessários
library(readr)
library(MASS)

# Ler o arquivo CSV
dados_a <- read_csv("dados_com_medias.csv")

an_mds <- function(dados, k) {
  # Calcular a matriz de distâncias
  matriz_distancias <- dist(dados)
  
  # Realizar o MDS não métrico
  mds_resultado <- isoMDS(matriz_distancias, k=k)
  
  # Função para calcular o stress do MDS não métrico
  calc_stress <- function(mds_coords, matriz_distancias) {
    dist_mds <- dist(mds_coords)
    stress <- sum((matriz_distancias - dist_mds)^2) / sum(matriz_distancias^2)
    return(stress)
  }
  
  # Executar o teste de permutação para o MDS não métrico
  n_permutacoes <- 999
  set.seed(123)
  stress_obs <- calc_stress(mds_resultado$points, matriz_distancias)
  stress_perm <- numeric(n_permutacoes)
  
  for (i in 1:n_permutacoes) {
    dados_permutados <- dados[sample(nrow(dados)), ]
    matriz_distancias_permutada <- dist(dados_permutados[, 1:11])
    mds_resultado_permutado <- isoMDS(matriz_distancias_permutada, maxit = 50, trace = FALSE)
    stress_perm[i] <- calc_stress(mds_resultado_permutado$points, matriz_distancias_permutada)
  }
  
  # Calcular o p-valor
  p_valor <- sum(stress_perm <= stress_obs) / n_permutacoes
  cat("P-valor:", p_valor, "\n")
  
  # Avaliando as variáveis
  # Calcular as correlações entre as variáveis e as coordenadas do MDS
  correlacoes <- cor(dados, mds_resultado$points)
  
  # Estabelecer um limite de correlação
  limite_correlacao <- 0.3
  
  # Encontrar as variáveis cujas correlações em ambas as dimensões estão abaixo do limite de correlação
  variaveis_excluir <- apply(abs(correlacoes) < limite_correlacao, 1, all)
  
  # Excluir as variáveis com base no critério estabelecido
  dados_reduzidos <- dados[, !variaveis_excluir]

  
  # Determinar o número máximo de dimensões a serem testadas
  max_dimensoes <- 7
  
  # Calcular o stress para cada número de dimensões
  stress_valores <- numeric(max_dimensoes)
  
  matriz_distancias <- dist(dados_reduzidos)
  for (k in 1:max_dimensoes) {
    mds_resultado <- isoMDS(matriz_distancias, k = k)
    stress_valores[k] <- calc_stress(mds_resultado$points, matriz_distancias)
  }
  
  # Criar o gráfico scree
  plot(1:max_dimensoes, 
       stress_valores, 
       type = "b", 
       main = "Gráfico Scree", 
       xlab = "Número de Dimensões", 
       ylab = "Stress")
  
  # Identificar o ponto de cotovelo
  cotovelo <- which.max(diff(diff(stress_valores)))
  points(cotovelo, stress_valores[cotovelo], col = "red", pch = 19)
  
    
  return(dados_reduzidos)
}

dados_b <- an_mds(dados_a, 3)
dados_c <- an_mds(dados_b, 3)

