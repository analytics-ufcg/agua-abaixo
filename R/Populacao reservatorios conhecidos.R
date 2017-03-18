library(dplyr)

abastecimento_municipio <- read.csv("C:/Users/Celio/Desktop/EstudoTrabalho/analytics/agua-abaixo/olhonagua/abastecimento_municipio.csv", fileEncoding = "utf-8")

abastecimento_municipio$GEOCODIGO1 <- as.numeric(substr(abastecimento_municipio$GEOCODIGO,0,6))


source("Abastecimento e Habitantes.R", encoding = "utf-8")

abastecimento_populacao <-merge(abastecimento_municipio, populacao(), by.x ="GEOCODIGO1", by.y = "Geocodigo", all = FALSE)

dados <- abastecimento_populacao %>%
  group_by(GEOCODIGO1,Pop_total,Pop_urbana_total) %>%
  summarise(n_monitorados = sum(COD_ANA !="Não_monitorado"), 
            n_nao_monitorados = sum(COD_ANA =="Não_monitorado")) %>%
  filter(n_monitorados > 0, n_nao_monitorados == 0) %>%
  select(GEOCODIGO1,Pop_total,Pop_urbana_total)

#nao_monitorado_e_monitorado <- dados %>%
#  filter(n_monitorados > 0, n_nao_monitorados > 0)

#nao_monitorado <- dados %>%
#  filter(n_monitorados == 0, n_nao_monitorados > 0)

#monitorado <- dados %>%
#  filter(n_monitorados > 0, n_nao_monitorados == 0)
