# Carregar pacotes necessários
library(readr)
library(MASS)
library(ggplot2)
library(gridExtra)


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
  
  return(p_valor)
 }  

p_mds(dados_a, 1)
p_mds(dados_a, 2)
p_mds(dados_a, 3) # <-  
p_mds(dados_a, 4) # <-  


# ------------------------------------------------------------------------------


# Seleciona variaveis a parter do coeficiente de correlacao
sv_mds <- function(dados, k) {
  # Calcular a matriz de distâncias
  matriz_distancias <- dist(dados)
  
  # Realizar o MDS não métrico
  mds_resultado <- isoMDS(matriz_distancias, k=k)
  
  # Avaliando as variáveis
  # Calcular as correlações entre as variáveis e as coordenadas do MDS
  correlacoes <- cor(dados, mds_resultado$points)
  
  # Estabelecer um limite de correlação
  limite_correlacao <- 0.0
  
  # Encontrar as variáveis cujas correlações em ambas as dimensões estão abaixo 
  # do limite de correlação
  variaveis_excluir <- apply(abs(correlacoes) < limite_correlacao, 1, all)
  
  # Excluir as variáveis com base no critério estabelecido
  dados_reduzidos <- dados[, !variaveis_excluir]
  
  aux <- list(correlacoes, 
              dados_reduzidos, 
              mds_resultado)
  
  return(aux)
}

dados_sv <- sv_mds(dados_a, 3)
corr <- dados_sv[[1]]
dados_red <- dados_sv[[2]]
mds_res <- dados_sv[[3]]$points


# ------------------------------------------------------------------------------


# Transformacao linear dos valores de uma coluna de modo que o menor 
# corresponda a zero e o maior a 100.
normalize <- function(x) {
  return (100*(x - min(x)) / (max(x) - min(x)))
}


# Salvar o dataframe em um arquivo CSV
write.csv(corr, "correlacoes.csv", row.names = FALSE)

dados_dr <- dados_sv[[2]]
dados_mds <- dados_sv[[3]]

pontos <- as.data.frame(dados_mds$points)
pontos_nm <- as.data.frame(lapply(pontos, normalize))

dados_v <- cbind(dados, pontos_nm)
plot(pontos_nm)

# Salvar o dataframe em um arquivo CSV
write.csv(dados_v, "dados_v.csv", row.names = FALSE)

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
  
  aux <- list(cotovelo, mds_resultados[cotovelo])
  return(aux)
}

aux <- c_mds(dados_a+1)
aux[2]

dados <- read_excel("Ranking-dos-Estados-2023.xlsx", 
                    sheet = "Valores")
dados_res <- cbind(dados[,1:3], mds_res, dados_a)
dados_res <- dados_res[, -3] 

colnames(dados_res)[3:5] <- c('mds_1', 'mds_2', 'mds_3')

dados_res$n_mds_1 <- normalize(dados_res$mds_1)
dados_res$n_mds_2 <- normalize(dados_res$mds_2)
dados_res$n_mds_3 <- normalize(dados_res$mds_3)

# Salvar o dataframe em um arquivo CSV
write.csv(dados_res, "res_2023.csv", row.names = FALSE)
