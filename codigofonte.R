rm(list=ls()) #limpa a memoria do R
version
citation()

#################### pacotes ####################
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

if (!require(knitr)) install.packages("knitr")
library(knitr)

# Carregar pacotes necessários
library(xlsx)
library(dplyr)
library(psych)
library(readxl)
library(knitr)
library(ggplot2)

##############################################################

dados_brutos <- read.csv("Global_Education.csv")

# Filtrando as colunas de interesse e renomeando-as (removendo zeros)
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

dados
# função para calcular a moda
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

# Criando uma variável para cada coluna com as medidas calculadas 
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

#print("Taxa de Conclusão do Ensino Fundamental - Masculino:")
#print(PosicaoCentral_TaxaConclusaoEF_Masculino)
#print(Dispersao_TaxaConclusaoEF_Masculino)

# Imprimir os intervalos de confiança para todas as colunas
# print(confidence_intervals)
 
## ======Parte Inferencial - teste hipotese =============
 
 # teste de hipótese diferenca de media TaxaConclusaoEF feminino Vs masculino (teste t para amostras independentes)
 teste_t <- t.test(dados$TaxaConclusaoEF_Masculino, dados$TaxaConclusaoEF_Feminino)
 # Imprimir os resultados do teste t
 print(teste_t)
 

 #Teste de Hipótese para Duas Proporções (Masculino e Feminino)

 p1 <- mean(dados$TaxaConclusaoEF_Masculino / 100)
 p2 <- mean(dados$TaxaConclusaoEF_Feminino / 100)
 n1 <- length(dados$TaxaConclusaoEF_Masculino)
 n2 <- length(dados$TaxaConclusaoEF_Feminino)
 p <- (p1 * n1 + p2 * n2) / (n1 + n2)
 Z <- (p1 - p2) / sqrt(p * (1 - p) * (1/n1 + 1/n2))
 p_value <- 2 * pnorm(-abs(Z)) # two-tailed test

 print(paste("Valor Z para o teste de duas proporções:", Z))
 print(paste("Valor p para o teste de duas proporções:", p_value))
 
 # teste de hipótese diferenca de media TaxaAlfabetizacaoJovens feminino Vs masculino (teste t para amostras independentes)
 teste_t2 <- t.test(dados$TaxaAlfabetizacaoJovens_Masculino , dados$TaxaAlfabetizacaoJovens_Feminino)
 
 # Imprimir os resultados do teste t
 print(teste_t2)
 
 
 # Teste de Hipótese (Taxa de Natalidade: Brasil vs Mundo)
 taxa_natalidade_brasil <- 13.92 # substitua por a taxa de natalidade do Brasil

 mu_0 <- mean(dados$TaxaNatalidade, na.rm = TRUE)
 t_value <- (mu_0 - taxa_natalidade_brasil) / (sd(dados$TaxaNatalidade) / sqrt(length(dados$TaxaNatalidade)))
 p_value <- 2 * pt(-abs(t_value), df=length(dados$TaxaNatalidade) - 1) # two-tailed test
 
 print(paste("Valor t para o teste de taxa de natalidade:", t_value))
 print(paste("Valor p para o teste de taxa de natalidade:", p_value))
 
 # Teste de Hipótese para Taxa de Desemprego: Brasil vs Mundo
 taxa_desemprego_brasil <- 12.08 # substitua por a taxa de desemprego do Brasil

 mu_0 <- mean(dados$TaxaDesemprego, na.rm = TRUE)
 t_value <- (mu_0 - taxa_desemprego_brasil) / (sd(dados$TaxaDesemprego) / sqrt(length(dados$TaxaDesemprego)))
 p_value <- 2 * pt(-abs(t_value), df=length(dados$TaxaDesemprego) - 1) # two-tailed test
 
 print(paste("Valor t para o teste de taxa de desemprego:", t_value))
 print(paste("Valor p para o teste de taxa de desemprego:", p_value))
 
 
 #================= graficos =================
 media_masculino <- mean(dados$TaxaConclusaoEF_Masculino, na.rm = TRUE)
 media_feminino <- mean(dados$TaxaConclusaoEF_Feminino, na.rm = TRUE)
 
 # Criando um dataframe para o gráfico
 dados_grafico <- data.frame(
   Genero = c("Masculino", "Feminino"),
   TaxaConclusaoMedia = c(media_masculino, media_feminino)
 )
 
 # gráfico de barras Taxa Média de Conclusão do Ensino Fundamental por genero
 ggplot(dados_grafico, aes(x = Genero, y = TaxaConclusaoMedia, fill = Genero)) +
   geom_bar(stat = "identity", position = position_dodge()) +
   ylab("Taxa Média de Conclusão do Ensino Fundamental") +
   xlab("Gênero") +
   ggtitle("Comparação da Taxa Média de Conclusão do Ensino Fundamental por Gênero") +
   theme_minimal()
 
 
 # Calculando as médias para cada gênero na taxa de alfabetização
 media_alfabetizacao_masculino <- mean(dados$TaxaAlfabetizacaoJovens_Masculino, na.rm = TRUE)
 media_alfabetizacao_feminino <- mean(dados$TaxaAlfabetizacaoJovens_Feminino, na.rm = TRUE)
 
 # Criando um dataframe para o gráfico de alfabetização
 dados_alfabetizacao_grafico <- data.frame(
   Genero = c("Masculino", "Feminino"),
   TaxaAlfabetizacaoMedia = c(media_alfabetizacao_masculino, media_alfabetizacao_feminino)
 )
 
 # gráfico de barras para a taxa de alfabetização
 ggplot(dados_alfabetizacao_grafico, aes(x = Genero, y = TaxaAlfabetizacaoMedia, fill = Genero)) +
   geom_bar(stat = "identity", position = position_dodge()) +
   ylab("Taxa Média de Alfabetização de Jovens") +
   xlab("Gênero") +
   ggtitle("Comparação da Taxa Média de Alfabetização de Jovens por Gênero") +
   theme_minimal()
 
 # gráfico Taxa de Alfabetização vs. Taxa de Desemprego
 dados$TaxaAlfabetizacaoMedia <- (dados$TaxaAlfabetizacaoJovens_Masculino + dados$TaxaAlfabetizacaoJovens_Feminino) / 2
 ggplot(dados, aes(x = TaxaAlfabetizacaoMedia, y = TaxaDesemprego)) +
   geom_point() +
   xlab("Taxa Média de Alfabetização") +
   ylab("Taxa de Desemprego") +
   ggtitle("Relação entre Alfabetização e Desemprego")
 
 # gráfico Taxa de Natalidade vs. Taxa de Desemprego
 ggplot(dados, aes(x = TaxaNatalidade, y = TaxaDesemprego)) +
   geom_point() +
   xlab("Taxa de Natalidade") +
   ylab("Taxa de Desemprego") +
   ggtitle("Relação entre Natalidade e Desemprego")
 
 # ================= tabelas ==============
 
 dados_completo <- data.frame(
   Country = c("Albania", "China", "Afghanistan", "Brazil", "Global"),
   TaxaConclusaoEF_Masculino = round(c(94, 97, 67, 95, 77.9814814814815), 2),
   TaxaConclusaoEF_Feminino = round(c(96, 97, 40, 97, 78.5185185185185), 2),
   TaxaAlfabetizacaoJovens_Masculino = round(c(99, 100, 74, 99, 88.2037037037037), 2),
   TaxaAlfabetizacaoJovens_Feminino = round(c(100, 100, 56, 99, 85.4259259259259), 2),
   TaxaNatalidade = round(c(11.78, 10.9, 32.49, 13.92, 25.3766666666667), 2),
   MatriculaBrutaEF = round(c(107, 100.2, 104, 115.4, 105.214814814815), 2),
   TaxaDesemprego = round(c(12.33, 4.32, 11.12, 12.08, 6.77666666666667), 2)
 )
 
 # Criar um dataframe com os dados sumarizados
 sumario_dados <- data.frame(
   Variavel = c("TaxaConclusaoEF_Masculino", "TaxaConclusaoEF_Feminino",
                "TaxaAlfabetizacaoJovens_Masculino", "TaxaAlfabetizacaoJovens_Feminino",
                "TaxaNatalidade", "MatriculaBrutaEF", "TaxaDesemprego"),
   Media = c(77.98, 78.52, 88.20, 85.43, 25.38, 105.21, 6.78),
   Mediana = c(80.5, 88, 94.5, 96.5, 24.29, 104.6, 4.64),
   Moda = c(95, 97, 99, 99, 32.49, 104, 4.59),
   Desvio_Padrao = c(19.88, 23.28, 14.83, 20.02, 9.42, 14.91, 4.87),
   Variancia = c(395.04, 542.03, 219.94, 400.63, 88.82, 222.44, 23.72),
   IC_Inferior = c(72.56, 72.16, 84.16, 79.96, 22.80, 101.14, 5.45),
   IC_Superior = c(83.41, 84.87, 92.25, 90.89, 27.95, 109.29, 8.11)
 )
 
 # Criar a tabela de sumário
 kable(sumario_dados, caption = "Sumário Estatístico das Variáveis", align = 'c', 
       col.names = c("Variável", "Média", "Mediana", "Moda", "Desvio Padrão", "Variância", "IC 95% Inferior", "IC 95% Superior"))
 