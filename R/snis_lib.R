
load_snis <- function(dir = "snis/"){
    library(readr)
    library(dplyr)
    dados = read_csv(paste0(dir, "/info_geral.csv"), 
                     col_types = cols(`Código.do.Município` = col_character()))
    agua = read_csv(paste0(dir, "/info_agua.csv"), 
                    col_types = cols(`Código.do.Município` = col_character()))
    left_join(dados, agua) %>%
        select(codigo = `Código.do.Município`, 
               municipio = `Município`, 
               estado = `Estado`, 
               ano = `Ano.de.Referência`,
               consumo = `AG010...Volume.de.água.consumido..1000.m..ano.`, 
               populacao_abastecida = `AG026...População.urbana.atendida.com.abastecimento.de.água..Habitantes.`, 
               populacao_total_ibge = `POP_TOT...População.total.do.município.do.ano.de.referência..Fonte..IBGE....Habitantes.`, 
               populacao_urbana_ibge = `POP_URB...População.urbana.do.município.do.ano.de.referência..Fonte..IBGE....Habitantes.`) %>% 
        return()
}