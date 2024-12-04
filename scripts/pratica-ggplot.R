###
# Workshop Visualização de Dados com R
# Letícia Lopes (leticialssdias@gmail.com) 
# dezembro/2024
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

# Vamos entender o banco de dados:
view(dados)
unique(dados$gov_type)
unique(dados$category)


# Vamos começar nosso primeiro gráfico
ggplot()


# Explorar se a perda de vegetação está associada ao tamanho da área
dados |> 
  ggplot() +
  geom_point(aes(x = area_km2, y = loss_veg_treatment))

# Essa forma de escrita é o mesmo que:
ggplot(dados) +
  geom_point(aes(x = area_km2, y = loss_veg_treatment))

# Ou:
ggplot(dados, aes(x = area_km2, y = loss_veg_treatment)) +
  geom_point()


## Ajustes estéticos 
dados |> 
  ggplot() +
  geom_point(aes(x = area_km2, y = loss_veg_treatment),
             size = 2,
             alpha = 0.5,
             color = "darkgreen") +
  scale_x_continuous(labels = scales::number_format(decimal.mark = ",",
                                                    big.mark = ".",
                                                    scale = 1e-6,
                                                    suffix = " M"),
                     breaks = seq(min(dados$area_km2, na.rm = T),
                                  max(dados$area_km2, na.rm = T),
                                  by = 10000),
                     expand = expansion(mult = c(0.05, 0.05))
                     
                     ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001,
                                                    decimal.mark = ",",
                                                    big.mark = "."),
                     n.breaks = 6,
                     limits = c(-1, 1)
                     ) +
  labs(
    x = "Área (km²)",
    y = "% perda de vegetação",
    title = "Área x Desmatamento"
  ) +
  theme_classic()



## Variáveis categóricas
dados |> 
  group_by(biome) |> 
  summarise(perda_media = mean(loss_veg_treatment, na.rm = T)) |> 
  ggplot() +
  geom_col(aes(x = biome, y = perda_media)) +
  # Abaixo, mudo apenas o rótulo
  # scale_x_discrete(labels = c("Amazônia",
  #                                "Mata Atlântica",
  #                                "Caatinga",
  #                                "Cerrado",
  #                                "Pampa",
  #                                "Pantanal")) +
  # Dessa forma, mudo a ordem:
  scale_x_discrete(limits = c("Cerrado",
                              "Amazon",
                              "Pantanal",
                              "Atlantic Forest",
                              "Pampa",
                              "Caatinga"))
  


## Outra opção: usando o pacote forcats (tidyverse)
# podemos ordenar por uma variável numérica

g1 <- 
  dados |> 
  group_by(biome) |> 
  summarise(perda_media = mean(loss_veg_treatment, na.rm = T)) |> 
  mutate(biome = fct_reorder(biome, perda_media)) |> 
  ggplot() +
  geom_col(aes(x = biome, y = perda_media),
           fill = "brown") +
  labs(x = element_blank(),
       y = "Diferença entre % \nde vegetação") +
  theme_classic()
  


## Modificando as cores

# Paleta de color
?viridis::viridis
viridis::viridis(4)

g2 <- 
  dados |> 
  ggplot(aes(x = area_km2, y = remaining_veg_treatment_initial, 
             color = gov_type)) +
  geom_point() +
  geom_smooth(method = lm) +
  # Inserindo valors manualmente:
  scale_color_manual(
    values = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")
    )
  # Usando uma função de paleta pronta:
  # scale_color_viridis_d(option = "magma")
  # scale_color_npg()


# Paletas do pacote ggsci
# Dá para usar como scale_color ou scale_fill
# scale_color_npg(): Nature Publishing Group color palettes
# scale_color_aaas(): American Association for the Advancement of Science color palettes
# scale_color_lancet(): Lancet journal color palettes
# scale_color_jco(): Journal of Clinical Oncology color palettes
# scale_color_tron(): This palette is inspired by the colors used in Tron Legacy. It is suitable for displaying data when using a dark theme.

  
## Facet_wrap

dados |> 
  ggplot(aes(x = area_km2, y = remaining_veg_treatment_initial, 
             color = gov_type)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  scale_color_npg() +
  facet_wrap(~biome) +
  labs(color = "Tipo de área",
       x = "Área (km²)",
       y = "Vegetação natural inicial") +
  # Modificando o tema 
  theme_bw() +
  # theme_minimal() +
  # theme_classic() +
  theme(legend.position = "bottom")


## Composição de figura com patchwork

g1 + g2

g1 / g2 + plot_annotation(tag_levels = 'A')

(g1 + g2) / g2



## Modificando as fontes

# Fontes instaladas no meu notebook:
extrafont::loadfonts(device = "win") 
# Importar fontes instaladas no sistema - pode levar algum tempo
font_import()
# Retorna vetor com os family names - usados no argumento de texto do ggplot
fonts()

loadfonts()

g1 + 
  theme(text = element_text(family = "Symbol"))


## Salvando a figura final em alta resolução

ggsave("img/fig1.png", width = 10, height = 5, dpi = 300)



## Agora, vamos reproduzir os histogramas da Fig 1 do artigo
dados |> 
  ggplot() +
  geom_histogram(aes(or), fill = "#2596be", bins = 15) +
  geom_vline(aes(xintercept = 1), col = "red", linetype = "longdash") +
  scale_x_continuous(transform = "log",
                     n.breaks = 5,
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  labs(x = "Odds ratio of vegetation loss",
       y = "Frequency") +
  facet_wrap(~biome, scales = "free") +
  theme_classic() +
  theme(strip.background = element_blank())



# Caso os pontos estejam pixelados, rodar o código abaixo
# trace(grDevices::png, quote({
#   if (missing(type) && missing(antialias)) {
#     type <- "cairo-png"
#     antialias <- "subpixel"
#   }
# }), print = FALSE)



## Extra: Mapas com ggplot2

biomas <- geobr::read_biomes()
biomas
plot(st_geometry(biomas))

ggplot(biomas) +
  geom_sf(aes(fill = name_biome),
          color = "white",
          linewidth = 0.3
          ) +
  geom_sf_text(aes(label = name_biome), 
               size = 4,
               color = "grey90") +
  scale_fill_viridis_d(option = "turbo") +
  labs(x = element_blank(),
       y = element_blank(),
       fill = "Bioma") +
  theme_bw() +
  ggspatial::annotation_scale(location = 'bl', width_hint = 0.2) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(1.5, "cm"), 
                                    pad_y = unit(0.7, "cm"),
                                    height = unit(0.6, "cm"),
                                    width = unit(0.6, "cm")
  ) +
  theme(
    legend.justification = "bottom",
    panel.grid = element_blank()
  )
