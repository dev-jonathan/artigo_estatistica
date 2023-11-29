rm(list=ls()) #limpa a memoria do R
version
citation()

# Carregar pacotes necessários
#################### Carregando pacotes ####################
if(!require(xlsx))
  install.packages("xlsx")
library(xlsx)

if(!require(dplyr))
  install.packages("dplyr")
library(dplyr)

if(!require(psych))
  install.packages("psych")
library(psych)

if(!require(readxl))
  install.packages("readxl")
library(readxl)

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)


# Carregar pacotes necessários
library(xlsx)
library(dplyr)
library(psych)
library(readxl)

##############################################################

dados_brutos <- read.csv("Global_Education.csv")

# Filtrando as colunas de interesse e renomeando-as (removendo nulos/0)
dados <- select(dados_brutos, 
                Paises_Regioes = Countries_and_areas,
                TaxaConclusaoEF_Masculino = Completion_Rate_Primary_Male, 
                TaxaConclusaoEF_Feminino = Completion_Rate_Primary_Female, 
                TaxaAlfabetizacaoJovens_Masculino = Youth_15_24_Literacy_Rate_Male, 
                TaxaAlfabetizacaoJovens_Feminino = Youth_15_24_Literacy_Rate_Female, 
                TaxaNatalidade = Birth_Rate, 
                MatriculaBrutaEF = Gross_Primary_Education_Enrollment, 
                TaxaDesemprego = Unemployment_Rate) %>%
  filter(TaxaConclusaoEF_Masculino > 0 & TaxaConclusaoEF_Feminino > 0 &
           TaxaAlfabetizacaoJovens_Masculino > 0 & TaxaAlfabetizacaoJovens_Feminino > 0 &
           TaxaNatalidade > 0 & MatriculaBrutaEF > 0 & TaxaDesemprego > 0)


# Definindo uma função para calcular a moda
getmode <- function(v) {
  v <- na.omit(v)  # Remove NA
  v <- v[v > 0]    # Remove valores 0
  uniqv <- unique(v)
  if (length(uniqv) == 0) return(NA)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculando medidas de posição central e dispersão para cada coluna (exceto 'Paises_Regioes')
medidas_posicao_central <- lapply(dados[, -1], function(x) {
  list(Media = mean(x, na.rm = TRUE), 
       Mediana = median(x, na.rm = TRUE), 
       Moda = getmode(x))
})

medidas_dispersao <- lapply(dados[, -1], function(x) {
  list(DesvioPadrao = sd(x, na.rm = TRUE), 
       Variancia = var(x, na.rm = TRUE))
})

# Criando uma variável para cada coluna com as medidas calculadas (exceto para 'Paises_Regioes')
nomes_variaveis <- names(dados)[-1]

for (nome in nomes_variaveis) {
  assign(paste0("PosicaoCentral_", nome), medidas_posicao_central[[nome]])
  assign(paste0("Dispersao_", nome), medidas_dispersao[[nome]])
}

calculate_confidence_interval <- function(x) {
  result <- t.test(x, conf.level = 0.95)
  return(result$conf.int)
}

# Calcular o intervalo de confiança para cada coluna
confidence_intervals <- lapply(dados[, -1], calculate_confidence_interval)

# ============== PRINTS ==================

#printar todas as medidas para todas as colunas
for (nome in nomes_variaveis) {
  pos_central <- get(paste0("PosicaoCentral_", nome))
  dispersao <- get(paste0("Dispersao_", nome))
  ic <- confidence_intervals[[nome]]
  
  medidas_pos_central <- paste("Média:", pos_central$Media, 
                               "Mediana:", pos_central$Mediana, 
                               "Moda:", pos_central$Moda)
  medidas_dispersao <- paste("Desvio Padrão:", dispersao$DesvioPadrao, 
                             "Variância:", dispersao$Variancia)
  
  print(paste("Medidas para:", nome))
  print(medidas_pos_central)
  print(medidas_dispersao)
  print(paste("Intervalo de Confiança (95%):", ic[1], "-", ic[2]))
  cat("\n")
}

#exemplo de prints para cada coluna separada
# Taxa de Conclusão do Ensino Fundamental - Masculino
#print("Taxa de Conclusão do Ensino Fundamental - Masculino:")
#print(PosicaoCentral_TaxaConclusaoEF_Masculino)
#print(Dispersao_TaxaConclusaoEF_Masculino)

# Imprimir os intervalos de confiança para todas as colunas
# print(confidence_intervals)
 
## ================== grafico e teste hipotese para genero ensino fundamental
 
 # Calculando as médias para cada gênero
 media_masculino <- mean(dados$TaxaConclusaoEF_Masculino, na.rm = TRUE)
 media_feminino <- mean(dados$TaxaConclusaoEF_Feminino, na.rm = TRUE)
 
 # Criando um dataframe para o gráfico
 dados_grafico <- data.frame(
   Genero = c("Masculino", "Feminino"),
   TaxaConclusaoMedia = c(media_masculino, media_feminino)
 )
 
 # Criar o gráfico de barras
 ggplot(dados_grafico, aes(x = Genero, y = TaxaConclusaoMedia, fill = Genero)) +
   geom_bar(stat = "identity", position = position_dodge()) +
   ylab("Taxa Média de Conclusão do Ensino Fundamental") +
   xlab("Gênero") +
   ggtitle("Comparação da Taxa Média de Conclusão do Ensino Fundamental por Gênero") +
   theme_minimal()
 
 # Realizar o teste de hipótese (teste t para amostras independentes)
 teste_t <- t.test(dados$TaxaConclusaoEF_Masculino, dados$TaxaConclusaoEF_Feminino)
 
 # Imprimir os resultados do teste t
 print(teste_t)
 
 
 
 
 
 #================
 
 #Teste de Hipótese para Duas Proporções (Masculino e Feminino)

 p1 <- mean(dados$TaxaConclusaoEF_Masculino / 100)
 p2 <- mean(dados$TaxaConclusaoEF_Feminino / 100)
 n1 <- length(dados$TaxaConclusaoEF_Masculino)
 n2 <- length(dados$TaxaConclusaoEF_Feminino)
 p <- (p1 * n1 + p2 * n2) / (n1 + n2)
 Z <- (p1 - p2) / sqrt(p * (1 - p) * (1/n1 + 1/n2))
 p_value <- 2 * pnorm(-abs(Z)) # two-tailed test
 

 # Teste de Hipótese (Taxa de Natalidade: Brasil vs Mundo)
 
 mu_0 <- mean(dados$TaxaNatalidade, na.rm = TRUE)
 t_value <- (mean(dados$TaxaNatalidade) - mu_0) / (sd(dados$TaxaNatalidade) / sqrt(length(dados$TaxaNatalidade)))
 p_value <- 2 * pt(-abs(t_value), df=length(dados$TaxaNatalidade) - 1) # two-tailed test
 
 
# Teste de Hipótese para Taxa de Desemprego: Brasil vs Mundo

 mu_0 <- mean(dados$TaxaDesemprego, na.rm = TRUE)
 t_value <- (mean(dados$TaxaDesemprego) - mu_0) / (sd(dados$TaxaDesemprego) / sqrt(length(dados$TaxaDesemprego)))
 p_value <- 2 * pt(-abs(t_value), df=length(dados$TaxaDesemprego) - 1) # two-tailed test
 