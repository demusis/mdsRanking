# Carregar o pacote FuzzyR
library(FuzzyR)

# Criar um dataframe com valores aleatórios de 0 a 1
dados <- data.frame(coluna1 = runif(50), coluna2 = runif(50), coluna3 = runif(50), coluna4 = runif(50), coluna5 = runif(50))

# Definir as categorias das variáveis linguísticas
menor <- 0
maior <- 1000
categorias <- c("baixo", "medio", "alto")
categorias_num <- c(1, 2, 3)
variaveis_peso <- c(1, 1, 1, 1, 1)

# Trapézios
pontos_corte <- seq(from = menor, to = maior, length.out = 8)
pontos_corte <- round(pontos_corte, digits = 0)

# Cria sistema fuzzy.
# a <- newfis ('hyper', fisType = " mamdani ", defuzzMethod = "centroid " )
fis <<- newfis('C')

# Criar as variáveis linguísticas correspondentes a cada coluna
for(i in 1:ncol(dados)) {
  fis <<- FuzzyR::addvar(fis, 'input', names(dados)[i], c(menor, maior))
  
  # Definir as funções de pertinência
  for(j in 1:length(categorias)) {
    fis <<- addmf(fis, 'input', i, categorias[j], 'trapmf', 
                  c(pontos_corte[(j-1)*2+1], 
                    pontos_corte[(j-1)*2+2], 
                    pontos_corte[(j-1)*2+3], 
                    pontos_corte[(j-1)*2+4]))
  }
}

# Definir variável de saída
fis <- addvar(fis, 'output', 'escore', c(menor, maior))

# Definir funções de pertinência
for(j in 1:length(categorias)) {
  fis <<- addmf(fis, 'output', 1, categorias[j], 'trapmf', 
                c(pontos_corte[(j-1)*2+1], 
                  pontos_corte[(j-1)*2+2], 
                  pontos_corte[(j-1)*2+3], 
                  pontos_corte[(j-1)*2+4]))
}

# Gerar todas as combinações possíveis entre as categorias linguísticas
comb_categorias <- expand.grid(categorias_num, 
                               categorias_num, 
                               categorias_num, 
                               categorias_num, 
                               categorias_num)

# Função para calcular a média ponderada
media_ponderada <- function(escores, pesos) {
  return(sum(escores * pesos) / sum(pesos))
}

# Adicionar as regras ao sistema fuzzy
for (i in 1:nrow(comb_categorias)) {
  escores <- comb_categorias[i, ]
  media <- round(media_ponderada(escores, variaveis_peso), digits = 0)
  escores <- as.matrix(cbind(escores, # Valores de entrada
                             media, # Valores de saída
                             1, # Peso
                             1)) # Operador 'AND'
  fis <<- addrule(fis, escores)
}

texto_regras <- showrule(fis)

# Plotar as funções de pertinência
for(i in 1:ncol(dados)) {
  plotmf(fis, 'input', i)
}

# Vetor de dados de exemplo
dados_exemplo <- c(100, 100, 100, 100, 100)

# Avaliar o sistema fuzzy com os dados de exemplo
resultado <- evalfis(dados_exemplo, fis, draw = FALSE)
cat("Resultado da função de saída:", resultado, "\n")


# Plotar a função de saída
nome_var = fis$output[[1]]$name
plot(D_x, D_y, type = "l", col = "blue", lwd = 3,
     xlab=nome_var,
     ylab='Pertinência')
abline(v = D_out, col="red", lwd=3, lty=2)
text(D_out, 0,  toString(D_out),
     cex=1, pos=4,col="red") 
