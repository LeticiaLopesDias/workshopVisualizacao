###
# Workshop Visualização de Dados com R
# Letícia Lopes (leticialssdias@gmail.com) 
# novembro/2024
# PPG Ecologia e Evolução/ UFG
###


# Pacotes:
if (!require("pacman")) install.packages("pacman")

# função que detecta se pacote está instalado e, se não estiver, faz a instalação
# caso já esteja instalado, ela carrega o pacote. substitui os passos de 
# install.packages() e library()
pacman::p_load(
  tidyverse
  )

# Checar diretório: confira se script e banco de dados estão armazenados nos
# locais adequados
# Pode ser feito manualmente Session > Set working directory > Choose directory
# ou definindo um caminho com a função setwd(), exemplo:
# setwd("C:/Users/NOTE-Letícia Lopes/OneDrive/Documentos/GitHub/workshopVisualizacao/scripts")

# Carregar banco de dados:
# Usaremos os dados dos artigo https://www.science.org/doi/10.1126/sciadv.abh2932

dados <- read_csv("dados/sciadv.abh2932_Data_file_S1.csv", 
                  locale = locale(encoding = "latin1"))
dplyr::glimpse(dados)

# Arrumar colunas
dados <- dados |> 
  mutate(
    across(area_km2:loss_veg_control, as.double)
  ) |> 
  select(-...19)
glimpse(dados)

# Vamos entender o banco de dados:
view(dados)

# Vamos começar nosso primeiro gráfico
ggplot()

# Veremos, será que a perda de vegetação está associada ao tamanho da área?
dados |> 
  ggplot() +
  geom_point(aes(x = area_km2, y = loss_veg_treatment))

# Essa forma de escrita é o mesmo que:
# g1 <- 
  ggplot(dados) +
  geom_point(aes(x = area_km2, y = loss_veg_treatment))

# Ou:
ggplot(dados, aes(x = area_km2, y = loss_veg_treatment)) +
  geom_point()


#  Modificando o intervalo entre os valores do eixo (variável numérica)
g1

g1 +  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1,
                                                    decimal.mark = ",",
                                                    big.mark = ".")) +
  scale_x_continuous(n.breaks = 6)


# Posso definir manualmente os valores 

g1 + scale_x_continuous(breaks = seq(min(dados$area_km2, na.rm = T),
                                      max(dados$area_km2, na.rm = T),
                                      by = 10000))


## Formatando a unidade para "em milhões"

g1 + scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                       decimal.mark = ",",
                                                       scale = 1e-6,
                                                       suffix = " M"))

## Modificando os limites dos eixos
g1 + coord_cartesian(ylim = c(-1, 5000000000),
                     xlim = c(0, 90000))

g1 + scale_y_continuous(limits = c(-1, 5000000000))


## Variáveis categóricas

# g2 <- 
  dados |> 
  group_by(biome) |> 
  summarise(perda_media = mean(loss_veg_treatment, na.rm = T)) |> 
  ggplot() +
  geom_col(aes(x = biome, y = perda_media))


## Modificando os rótulos do eixo x (categórico)

g2 + scale_x_discrete(labels = c("Amazônia",
                                 "Mata Atlântica",
                                 "Caatinga",
                                 "Cerrado",
                                 "Pampa",
                                 "Pantanal"))



## Modificando as ordens das categorias do eixo x (categórico)
unique(dados$biome)

g2 + scale_x_discrete(limits = c("Cerrado",
                                 "Amazon",
                                 "Pantanal",
                                 "Atlantic Forest",
                                  "Pampa",
                                 "Caatinga"))


## Outra opção: usando o pacote forcats (tidyverse)

dados |> 
  group_by(biome) |> 
  summarise(perda_media = mean(loss_veg_treatment, na.rm = T)) |> 
  mutate(biome = fct_reorder(biome, perda_media)) |> 
  ggplot() +
  geom_col(aes(x = biome, y = perda_media)) +
  # Modificando o nome dos eixos:
  labs(x = element_blank(),
       y = "Diferença entre % \nde vegetação")
