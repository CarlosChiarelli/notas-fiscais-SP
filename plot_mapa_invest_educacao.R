library(tidyverse)
library(descr)
library(abjutils)
library(brazilmaps)

df_receita18 <- read_csv2("receitas-2018.csv", locale = locale())

df_receita18 <- df_receita18 %>% 
  mutate_if(is.character, funs(toUTF8(.)))

glimpse(df_receita18)

df_receita18 %>% 
  select(ds_fonte_recurso,
         ds_poder,
         ds_cd_aplicacao_fixo) %>% 
  map(function(coluna) unique(coluna))

df_receita18 <- df_receita18 %>% 
  rename(local_invest = ds_cd_aplicacao_fixo) %>% 
  mutate(local_invest = str_sub(local_invest, 7) %>% 
           rm_accent() %>% 
           str_to_lower())

df_educa <- df_receita18 %>% 
  group_by(ds_municipio) %>% 
  mutate(total_arrecad = sum(vl_arrecadacao)) %>% 
  filter(local_invest %>% 
           str_detect("educacao")) %>% 
  mutate(educacao = (sum(vl_arrecadacao)/total_arrecad)*100) 

df_unir <- df_educa %>% 
  select(ds_municipio,
         educacao) %>% 
  distinct() %>% 
  mutate(municipio = str_to_upper(ds_municipio))
  
mapa <- get_brmap("City", geo.filter = list(State = 35)) %>% 
  left_join(df_unir, c("nome" = "municipio")) %>% 
  ggplot() +
  geom_sf(aes(fill=(educacao)), col="black") +
  scale_fill_viridis_c(option = 2,
                      #direction = -1
                      )+
  labs(title = "porcentagem educação cada município (2018)")+
  theme_minimal()

ggsave("mapa_investm_educa_2018.jpeg",
       width = 10,
       height = 7)


# para fazer grafico 3d
#remotes::install_github("tylermorganwall/rayshader")
library(rayshader)

#gg_nc = ggplot(nc) +
#  geom_sf(aes(fill = AREA)) +
#  scale_fill_viridis("Area") +
#  ggtitle("Area of counties in North Carolina") +
#  theme_bw()

# descricao site https://www.tylermw.com/3d-ggplots-with-rayshader/
plot_gg(mapa, multicore = TRUE, width = 6 ,height=2.7, fov = 70) # fov=70 (padrao)
render_depth(focallength=100,focus=.5)
render_camera(zoom=0.5,theta=-30,phi=30)
render_snapshot()

# não rodar!!! buga o sistema 
mapa %>% 
  plot_gg(
    multicore = T,
    width = 6*2,
    height = 2.7*2,
    fov = 70
  )
render_snapshot()



mapa <- get_brmap("City", geo.filter = list(State = 35))
glimpse(mapa)
