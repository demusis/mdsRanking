# Carregar pacotes necessários
library(readr)
library(MASS)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(dplyr)



# Ler o arquivo CSV
# dados <- read_excel("Ranking-dos-Estados-2022.xlsx", 
#                     sheet = "Valores")
dados_a <- read_csv("dados_com_medias_2023b.csv")

# Função para calcular o stress do MDS não métrico (Kruskal's Non-metric Multidimensional Scaling)
calc_stress <- function(mds_coords, matriz_distancias) {
  dist_mds <- dist(mds_coords)
  stress <- sum((matriz_distancias - dist_mds)^2) / sum(matriz_distancias^2)
  return(stress)
}

# Calcula o p associado a dimensao fornecida
p_mds <- function(dados, k) {
  # Calcular a matriz de distâncias
  matriz_distancias <- dist(dados)
  
  # Realizar o MDS não métrico
  mds_resultado <- isoMDS(matriz_distancias, k=k)
  
  # Executar o teste de permutação para o MDS não métrico
  n_permutacoes <- 25
  set.seed(123)
  stress_obs <- calc_stress(mds_resultado$points, matriz_distancias)
  stress_perm <- numeric(n_permutacoes)
  
  for (i in 1:n_permutacoes) {
    dados_permutados <- dados[sample(nrow(dados)), ]
    matriz_distancias_permutada <- dist(dados_permutados[, 1:n_permutacoes])
    mds_resultado_permutado <- isoMDS(matriz_distancias_permutada, maxit = 50, trace = FALSE)
    stress_perm[i] <- calc_stress(mds_resultado_permutado$points, matriz_distancias_permutada)
  }
  
  # Calcular o p-valor
  p_valor <- sum(stress_perm <= stress_obs) / n_permutacoes
  cat("P-valor:", p_valor, "\n")
  
  return(p_valor)
 }  

p_mds(dados_a, 1)
p_mds(dados_a, 2)
p_mds(dados_a, 3)  
p_mds(dados_a, 4) # <-  
p_mds(dados_a, 5)

# ------------------------------------------------------------------------------

# Calcular as correlações entre as variáveis e as coordenadas do MDS
mds_resultado <- isoMDS(dist(dados_a), k=4)
correlacoes <- rcorr(as.matrix(dados_a), 
                     as.matrix(mds_resultado$points))

coef_correlacoes <- as.data.frame(correlacoes$r)[103:106]
colnames(coef_correlacoes) <- c('mds_1', 'mds_2', 'mds_3', 'mds_4')
p_correlacoes <- as.data.frame(correlacoes$P)[103:106]
colnames(p_correlacoes) <- c('mds_1', 'mds_2', 'mds_3', 'mds_4')

dados <- read_excel("Ranking-dos-Estados-2023.xlsx", 
                    sheet = "Valores")

mds_res <- cbind(dados[, c(1, 2)], mds_resultado$points, dados_a)
colnames(mds_res)[3:6] <- c('mds_1', 'mds_2', 'mds_3', 'mds_4')

# Salvar o dataframe em um arquivo CSV
write.csv(coef_correlacoes, "coef_correlações.csv", row.names = TRUE)
write.csv(p_correlacoes, "p_correlações.csv", row.names = TRUE)
write.csv(mds_res, "res_dados.csv", row.names = FALSE)

# ------------------------------------------------------------------------------

# Transformacao linear dos valores de uma coluna de modo que o menor 
# corresponda a zero e o maior a 100.
normalize <- function(x) {
  return (100*(x - min(x)) / (max(x) - min(x)))
}

# ------------------------------------------------------------------------------

c_mds <- function(dados) {
  # Determinar o número máximo de dimensões a serem testadas
  max_dimensoes <- 7
  
  # Calcular o stress para cada número de dimensões
  stress_valores <- numeric(max_dimensoes)
  
  matriz_distancias <- dist(dados) # Distância Eclidiana
  for (nd in 1:max_dimensoes) {
    mds_resultados <- isoMDS(matriz_distancias, k = nd)
    stress_valores[nd] <- calc_stress(mds_resultados$points, matriz_distancias)
  }
  
  # Criar o gráfico scree
  plot(1:max_dimensoes, 
       stress_valores, 
       type = "b", 
       main = "Gráfico Scree", 
       xlab = "Número de Dimensões", 
       ylab = "Stress")
  
  # Identificar o ponto de cotovelo
  cotovelo <- which.max(diff(diff(stress_valores))) + 1
  points(cotovelo, stress_valores[cotovelo], col = "red", pch = 19)
  points(cotovelo+1, stress_valores[cotovelo+1], col = "red", pch = 19)
  points(cotovelo+2, stress_valores[cotovelo+2], col = "red", pch = 19)
  
  aux <- list(cotovelo, mds_resultados[cotovelo])
  return(aux)
}

aux <- c_mds(dados_a+1)
aux[2]