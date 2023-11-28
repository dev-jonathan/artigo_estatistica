rm(list=ls())
version
citation()
#comentarios
## rm(list=ls()) #limpa a memoria do R

dados_brutos <- read.csv("Global_Education.csv")

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

##############################################################


# Supondo que seu dataframe original seja 'dados_brutos'

# Carregar pacotes necessários
library(xlsx)
library(dplyr)
library(psych)
library(readxl)

# Filtrando as colunas de interesse e renomeando-as
dados <- select(dados_brutos, 
                Paises_Regioes = Countries_and_areas,
                TaxaConclusaoEF_Masculino = Completion_Rate_Primary_Male, 
                TaxaConclusaoEF_Feminino = Completion_Rate_Primary_Female, 
                TaxaAlfabetizacaoJovens_Masculino = Youth_15_24_Literacy_Rate_Male, 
                TaxaAlfabetizacaoJovens_Feminino = Youth_15_24_Literacy_Rate_Female, 
                TaxaNatalidade = Birth_Rate, 
                MatriculaBrutaEF = Gross_Primary_Education_Enrollment, 
                TaxaDesemprego = Unemployment_Rate)

# Definindo uma função para calcular a moda
getmode <- function(v) {
  uniqv <- unique(v)
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

# Taxa de Conclusão do Ensino Fundamental - Masculino
print("Taxa de Conclusão do Ensino Fundamental - Masculino:")
print(PosicaoCentral_TaxaConclusaoEF_Masculino)
print(Dispersao_TaxaConclusaoEF_Masculino)



# Taxa de Conclusão do Ensino Fundamental - Feminino
print("Taxa de Conclusão do Ensino Fundamental - Feminino:")
print(PosicaoCentral_TaxaConclusaoEF_Feminino)
print(Dispersao_TaxaConclusaoEF_Feminino)


# Taxa de Alfabetização de Jovens - Masculino
print("Taxa de Alfabetização de Jovens - Masculino:")
print(PosicaoCentral_TaxaAlfabetizacaoJovens_Masculino)
print(Dispersao_TaxaAlfabetizacaoJovens_Masculino)


# Taxa de Alfabetização de Jovens - Feminino
print("Taxa de Alfabetização de Jovens - Feminino:")
print(PosicaoCentral_TaxaAlfabetizacaoJovens_Feminino)
print(Dispersao_TaxaAlfabetizacaoJovens_Feminino)

# Taxa de Natalidade
print("Taxa de Natalidade:")
print(PosicaoCentral_TaxaNatalidade)
print(Dispersao_TaxaNatalidade)


# Matrícula Bruta no Ensino Fundamental
print("Matrícula Bruta no Ensino Fundamental:")
print(PosicaoCentral_MatriculaBrutaEF)
print(Dispersao_MatriculaBrutaEF)

# Taxa de Desemprego
print("Taxa de Desemprego:")
print(PosicaoCentral_TaxaDesemprego)
print(Dispersao_TaxaDesemprego)





# Imprimir os intervalos de confiança para todas as colunas
 print(confidence_intervals)
 
 for (nome in nomes_variaveis) {
   pos_central <- get(paste0("PosicaoCentral_", nome))
   dispersao <- get(paste0("Dispersao_", nome))
   ic <- confidence_intervals[[nome]]
   
   # Formatação para exibir na mesma linha
   medidas_pos_central <- paste("Média:", pos_central$Media, 
                                "Mediana:", pos_central$Mediana, 
                                "Moda:", pos_central$Moda)
   medidas_dispersao <- paste("Desvio Padrão:", dispersao$DesvioPadrao, 
                              "Variância:", dispersao$Variancia)
   
   print(paste("Medidas para:", nome))
   print(medidas_pos_central)
   print(medidas_dispersao)
   print(paste("Intervalo de Confiança (95%):", ic[1], "-", ic[2]))
   cat("\n") # Adiciona uma linha em branco para separar as saídas
 }
