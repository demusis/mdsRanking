library(dplyr)
library(FuzzyR)
 library(FactoMineR)
 library(factoextra)
 library(GPArotation)
library(MASS)
library(mice)
library(readxl)
library(readr)
library(vegan)


# Carrega dados
dados <- read_excel("Ranking-dos-Estados-2022.xlsx", 
                     sheet = "Valores")
dados <- dados[,4:50]

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

# Verificar o novo dataframe 'dados_com_medias'
print(dados_com_medias)

# Salvar o dataframe 'dados_com_medias' em um arquivo CSV
write.csv(dados_com_medias, "dados_com_medias.csv", row.names = FALSE)

# -----------------------------------------------------------------------------

# Ler o arquivo CSV
dados_com_medias <- read_csv("dados_com_medias.csv")

# Calcular a matriz de distâncias
matriz_distancias <- dist(dados[, 1:11])

# Realizar o MDS não métrico
mds_resultado <- isoMDS(matriz_distancias)

# Executar o teste de permutação para o MDS não métrico
n_permutacoes <- 999
set.seed(123)
teste_permutacao <- permutest(mds_resultado, distance = "euclidean", nperm = n_permutacoes)

# Exibir os resultados do teste de permutação
cat("P-valor:", teste_permutacao$Pvals, "\n")

# Exibir os resultados do MDS não métrico
print(mds_resultado)

# Gráfico das coordenadas do MDS não métrico
plot(mds_resultado$points, main = "MDS não métrico", xlab = "Dimensão 1", ylab = "Dimensão 2", pch = 19)

# Calcular o stress
stress <- mds_resultado$stress
cat("Stress:", stress, "\n")

# Calcular o stress relativo (normalized stress)
stress_relativo <- stress / sum(matriz_distancias^2)
cat("Stress relativo:", stress_relativo, "\n")




# Gráfico para as variáveis
fviz_pca_var(pca_resultado, axes.title = c("Dimensão 1", "Dimensão 2"))

# Tabela com os percentuais de variância explicados por autovalor
percentuais_explicados <- pca_resultado$eig
percentuais_explicados <- data.frame(round(percentuais_explicados, 2))
colnames(percentuais_explicados) <- c("Autovalor", "% Variância Explicada", "% Acumulado")
print(percentuais_explicados)

# Tabela com os pesos para cada variável por componente principal
pesos_variaveis <- pca_resultado$var$coord
pesos_variaveis <- data.frame(round(pesos_variaveis, 4))
colnames(pesos_variaveis) <- c("Dimensão 1", "Dimensão 2", "Dimensão 3", "Dimensão 4", "Dimensão 5", "Dimensão 6", "Dimensão 7", "Dimensão 8", "Dimensão 9", "Dimensão 10", "Dimensão 11")
rownames(pesos_variaveis) <- colnames(dados[, 1:11])
print(pesos_variaveis)

# -----------------------------------------------------------------------------

# Criar um dataframe com valores aleatorios de 0 a 1
dados <- data.frame(coluna1 = runif(50), 
                    coluna2 = runif(50), 
                    coluna3 = runif(50), 
                    coluna4 = runif(50), 
                    coluna5 = runif(50))

# Definir as categorias das variaveis linguisticas
menor <- 0
maior <- 1000
categorias <- c("baixo", "medio", "alto")
categorias_num <- c(1, 2, 3)
variaveis_peso <- c(1, 1, 1, 1, 1)

# Trapezios
pontos_corte <- seq(from = menor, to = maior, length.out = 8)
pontos_corte <- round(pontos_corte, digits = 0)

# Cria sistema fuzzy.
# a <- newfis ('hyper', fisType = " mamdani ", defuzzMethod = "centroid " )
fis <<- newfis('C')

# Criar as variaveis linguisticas correspondentes a cada coluna
for(i in 1:ncol(dados)) {
  fis <<- FuzzyR::addvar(fis, 'input', names(dados)[i], c(menor, maior))
  
  # Definir as funcoes de pertinencia
  for(j in 1:length(categorias)) {
    fis <<- addmf(fis, 'input', i, categorias[j], 'trapmf', 
                  c(pontos_corte[(j-1)*2+1], 
                    pontos_corte[(j-1)*2+2], 
                    pontos_corte[(j-1)*2+3], 
                    pontos_corte[(j-1)*2+4]))
  }
}

# Definir variavel de saida
fis <- addvar(fis, 'output', 'escore', c(menor, maior))

# Definir funcoes de pertinencia
for(j in 1:length(categorias)) {
  fis <<- addmf(fis, 'output', 1, categorias[j], 'trapmf', 
                c(pontos_corte[(j-1)*2+1], 
                  pontos_corte[(j-1)*2+2], 
                  pontos_corte[(j-1)*2+3], 
                  pontos_corte[(j-1)*2+4]))
}

# Gerar todas as combinacoes possiveis entre as categorias linguisticas
comb_categorias <- expand.grid(categorias_num, 
                               categorias_num, 
                               categorias_num, 
                               categorias_num, 
                               categorias_num)

# Funcao para calcular a media ponderada
media_ponderada <- function(escores, pesos) {
  return(sum(escores * pesos) / sum(pesos))
}

regras <- data.frame(
  escore = numeric(),
  media = numeric(),
  peso = numeric(),
  operador = numeric()
)
regras <- cbind(dados[0,], regras)

for (i in 1:nrow(comb_categorias)) {
  escore <- comb_categorias[i, ]
  colnames(escore) <- colnames(dados)
  media <- round(media_ponderada(escore, variaveis_peso), digits = 0)
  escore <- cbind(escore, # Valores de entrada
                  media = media, # Valores de saida
                  peso = 1, # Peso
                  operador = 1) # Operador 'AND'
  
  regras <- rbind(regras, escore)
  escore <- as.matrix(escore)
  
  fis <<- addrule(fis, escore)
}
write.csv(regras, 'regras.csv')

texto_regras <- showrule(fis)

# Plotar as funcoes de pertinencia
for(i in 1:ncol(dados)) {
  plotmf(fis, 'input', i)
}

# Vetor de dados de exemplo
dados_exemplo <- c(100, 100, 100, 100, 100)

# Avaliar o sistema fuzzy com os dados de exemplo
resultado <- evalfis(dados_exemplo, fis, draw = FALSE)
cat("Resultado da funcao de saida:", resultado, "\n")


# Plotar a funcao de saaida
nome_var = fis$output[[1]]$name
plot(D_x, D_y, type = "l", col = "blue", lwd = 3,
     xlab=nome_var,
     ylab='PertinÃªncia')
abline(v = D_out, col="red", lwd=3, lty=2)
text(D_out, 0,  toString(D_out),
     cex=1, pos=4,col="red") 
