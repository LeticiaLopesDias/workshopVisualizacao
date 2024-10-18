###
# Workshop Visualização de Dados com R
# Letícia Lopes (leticialssdias@gmail.com) 
# novembro/2024
# PPG Ecologia e Evolução/ UFG
###



# Pacotes:
if (!require("pacman")) install.packages("pacman")

# A função abaixo  detecta se pacote está instalado e, se não estiver, faz a instalação
# caso já esteja instalado, ela carrega o pacote. substitui os passos de 
# install.packages() e library()
pacman::p_load(
  tidyverse,
  viridis, # Paleta acessível para daltônicos
  ggsci, # Scientific journal color palettes
  extrafont,
  patchwork,
  geobr,
  sf
  )

# Definir diretório:
# Pode ser feito manualmente: Session > Set working directory > Choose directory
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


## Modificando o intervalo entre os valores do eixo (variável numérica)
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


g1 + scale_x_continuous(expand = expansion(mult = c(0, 0.05)))


## Variáveis categóricas

# g2 <- 
  dados |> 
  group_by(biome) |> 
  summarise(perda_media = mean(loss_veg_treatment, na.rm = T)) |> 
  ggplot() +
  geom_col(aes(x = biome, y = perda_media))


## Modificando os rótulos do eixo x (categórico)
# Abaixo mudamos apenas o rótulo, não a ordem
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
# podemos ordenar por uma variável numérica

dados |> 
  group_by(biome) |> 
  summarise(perda_media = mean(loss_veg_treatment, na.rm = T)) |> 
  mutate(biome = fct_reorder(biome, perda_media)) |> 
  ggplot() +
  geom_col(aes(x = biome, y = perda_media)) +
  # Modificando o nome dos eixos:
  labs(x = element_blank(),
       y = "Diferença entre % \nde vegetação")


## Modificando as cores

dados |> 
  ggplot(aes(x = area_km2, y = remaining_veg_treatment_initial)) +
  geom_point(color = "darkred") +
  geom_smooth(method = lm, color = "grey")

g3 <- dados |> 
  ggplot(aes(x = area_km2, y = remaining_veg_treatment_initial, 
             color = gov_type)) +
  geom_point() +
  geom_smooth(method = lm)

# Paleta de color
?viridis::viridis
viridis::viridis(4)

g3 + scale_color_manual(values = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"))

g3 + scale_color_viridis_d(option = "magma")

# Paletas do pacote ggsci
# Dá para usar como scale_color ou scale_fill
# scale_color_npg(): Nature Publishing Group color palettes
# scale_color_aaas(): American Association for the Advancement of Science color palettes
# scale_color_lancet(): Lancet journal color palettes
# scale_color_jco(): Journal of Clinical Oncology color palettes
# scale_color_tron(): This palette is inspired by the colors used in Tron Legacy. It is suitable for displaying data when using a dark theme.

g3 + scale_color_npg()

## Facet_wrap

g3 + facet_wrap(~biome)


## Modificando o tema

g3 + theme_bw()
g3 + theme_minimal()
g3 + theme_classic()


## Modificando as fontes
# Fontes instaladas no meu notebook:
extrafont::loadfonts(device = "win") 

g3 + theme(
  text = element_text(family = "Bookman Old Style")
)


## Modificando a legenda

g3 + 
  labs(color = "Tipo de área",
       x = "Área (km²)",
       y = "Vegetação natural inicial") +
  theme(legend.position = "bottom")


## Composição de figura com patchwork

g1 + g2

g1 / g3

(g1 + g2) / g3

## Salvando a figura final em alta resolução

ggsave("img/fig1.png", width = 10, height = 5, dpi = 300)



## Combinando o que aprendemos até aqui:

dados |> 
  ggplot(aes(x = log(area_km2), y = remaining_veg_treatment_initial, 
             color = gov_type)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, se = F) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1,
                                                    decimal.mark = ","),
                     limits = c(0,1)) +
  labs(x = "Log(Área em km²)",
       y = "% vegetação nativa original",
       color = "Tipo de Governança") +
  scale_color_npg() +
  facet_wrap(~ biome) +
  theme_classic() +
  theme(legend.position = "top")


# Caso os pontos estejam pixelados, rodar o código abaixo
trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)


## Extra: Introdução a mapas com ggplot2
