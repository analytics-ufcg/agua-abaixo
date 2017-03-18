
load_snis <- function(dir = "snis/"){
    library(readr)
    dados = read_csv(paste0(dir, "/info_geral.csv"), 
                     col_types = cols(`Código.do.Município` = col_character()))
    agua = read_csv(paste0(dir, "/info_agua.csv"), 
                    col_types = cols(`Código.do.Município` = col_character()))
    return(dplyr::left_join(dados, agua))
}