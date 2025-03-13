# Trabalho de programação
library(dplyr)
library(tidyverse)
library(abjutils)
library(sf)
library(maps)
# Projeto Final - 4 mapas, 2 do mundo, 2 do Brasil

base = read.csv2('Programacao_FGV/trabalho_programacao.csv', encoding = 'UTF8')

# https://comexstat.mdic.gov.br/pt/municipio/105891

# Mapa por destino com o passar dos anos

mapa_producao = base %>% group_by(Ano, País) %>% 
  summarise(Quilograma.Líquido = sum(Quilograma.Líquido)) %>% 
  as.data.frame() %>% 
  separate(País, sep = '\n', into = c(NA, 'NO_PAIS'))

# https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta
codigo_pais = read.csv2('PAIS.csv', encoding = 'latin1') %>% select(NO_PAIS, CO_PAIS_ISOA3)

paises = inner_join(mapa_producao, codigo_pais) 
paises = paises %>% select(-NO_PAIS) %>% group_by(CO_PAIS_ISOA3) %>% 
  summarise(Quilograma.Líquido = sum(Quilograma.Líquido))

# Gerando o mapa
shp_mundo = read_sf('Programacao_FGV/MAPA/mundo.shp') %>% st_zm()
library('countrycode')

mundo = map_data('world')
mundo$code = countrycode(mundo$region, "country.name", "iso3c")

mundos = mundo %>% right_join(paises, by = c('code' = 'CO_PAIS_ISOA3'))

ggplot(mundos, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = Quilograma.Líquido)) + 
  theme(legend.position = 'top', legend.text = element_text(vjust = 2)) 
