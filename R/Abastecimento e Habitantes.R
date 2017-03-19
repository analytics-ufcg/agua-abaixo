populacao <- function(file_path_abastecimento_municipio, file_path_informacoes_gerais){
  library(dplyr, warn.conflicts = FALSE)
  abastecimento = read.csv(file_path_abastecimento_municipio, fileEncoding = "utf-8")
  info_gerais = read.csv(file_path_informacoes_gerais, fileEncoding = "utf-8")
  
  info_gerais_ultimas = filter(info_gerais, Ano.de.Referência == max(Ano.de.Referência))
  abastecimento$GEOCODIGO1 <- as.numeric(substr(abastecimento$GEOCODIGO,0,6))
  comparacao = right_join(info_gerais_ultimas, abastecimento, by = c("Código.do.Município" = "GEOCODIGO1"))
  k = rename(comparacao,
     Geocodigo = Código.do.Município,
     Municipio = Município,
     Estado = Estado,
     Pop_total = POP_TOT...População.total.do.município..Fonte..IBGE....Habitantes.,
     Pop_urbana_total = POP_URB...População.urbana.do.município..Fonte..IBGE...Habitantes.
  )
  
  result = distinct(k, Geocodigo, Municipio, Estado, Pop_total, Pop_urbana_total)
  return (result)
}
