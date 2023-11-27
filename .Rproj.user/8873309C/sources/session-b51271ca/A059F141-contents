rm(list=ls())
version
citation()
#comentarios
##setwd("C:")
##dados<-read.csv("Pasta.csv", head=T, sep";",header=T)

##dados_brutos <- read.csv("Global_Education.csv")
## dados_brutos

dados <- read.csv("Global_Education.csv")

# Carregar pacotes necessários
library(dplyr)
library(psych)

# Calculando as estatísticas descritivas para cada variável
estatisticas_descritivas <- dados %>%
  select(Completion_Rate_Primary_Male, Completion_Rate_Primary_Female, 
         Youth_15_24_Literacy_Rate_Male, Youth_15_24_Literacy_Rate_Female, 
         Birth_Rate, Gross_Primary_Education_Enrollment, 
         Unemployment_Rate) %>%
  summarise_all(list(mean=mean, median=median, sd=sd, var=var)) 

# Mostrar as estatísticas descritivas
print(estatisticas_descritivas)

# Calculando a moda para cada variável
# Nota: A moda pode não ser única, e esta função retorna a moda mais baixa em caso de múltiplas modas
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda_values <- sapply(dados[, c("Completion_Rate_Primary_Male", "Completion_Rate_Primary_Female", 
                                "Youth_15_24_Literacy_Rate_Male", "Youth_15_24_Literacy_Rate_Female", 
                                "Birth_Rate", "Gross_Primary_Education_Enrollment", 
                                "Unemployment_Rate")], getmode)

# Mostrar valores de moda
print(moda_values)

# Calculando o intervalo de confiança de 95% para a média de cada variável
intervalos_confianca <- dados %>%
  select(Completion_Rate_Primary_Male, Completion_Rate_Primary_Female, 
         Youth_15_24_Literacy_Rate_Male, Youth_15_24_Literacy_Rate_Female, 
         Birth_Rate, Gross_Primary_Education_Enrollment, 
         Unemployment_Rate) %>%
  summarise_all(~ t.test(.)$conf.int[1:2])

# Mostrar os intervalos de confiança
print(intervalos_confianca)

