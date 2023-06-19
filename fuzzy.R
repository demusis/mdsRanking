library(dplyr)
library(FuzzyR)
# library(FactoMineR)
# library(factoextra)
# library(GPArotation)
#library(MASS)
#library(mice)
# library(readxl)
library(readr)
library(vegan)

# Ler o arquivo CSV
dados <- read_csv("dados_com_medias.csv")

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


# Plotar a funcao de saida
nome_var = fis$output[[1]]$name
plot(D_x, D_y, type = "l", col = "blue", lwd = 3,
     xlab=nome_var,
     ylab='Pertinência')
abline(v = D_out, col="red", lwd=3, lty=2)
text(D_out, 0,  toString(D_out),
     cex=1, pos=4,col="red") 
