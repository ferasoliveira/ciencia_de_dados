pacotes <- c("tidyverse","sf","spdep","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra","gtools","grid")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

data <- read.csv("acidentes_causa.csv",sep=";",dec = ",")
estados_filtrar <- c("SC", "PR", "RS")
data_filter <- data[data$uf %in% estados_filtrar & data$idade < 120 & data$ano_fabricacao_veiculo > 1950 & data$km != "?" & data$km != 0 & data$estado_fisico != "Nao Informado" & data$latitude > -33 & data$latitude < -21 & data$longitude > -58 & data$longitude < -47,]
data_fil <- data_filter[, !(colnames(data_filter) %in% c("pesid", "marca" , "id_veiculo" , "regional" , "delegacia" , "uop"))]

data_fil <- data_fil[complete.cases(data_fil[c("latitude", "longitude", "km")]), ]

br_counts <- table(data_fil$br)
new_table <- data.frame(BR = names(br_counts), Count = as.vector(br_counts))
max_km <- aggregate(data$km, by = list(BR = data$br), FUN = max)

# Mesclar a informação de km máximo com a nova tabela
new_table <- merge(new_table, max_km, by = "BR", all.x = TRUE)

# Renomear a coluna de km máximo
colnames(new_table)[3] <- "Quilometros"

new_table$Acidentes_por_km <- new_table$Count / new_table$Quilometros

# Calcular a nova coluna ponderada
data_fil$Score <- data_fil$ilesos * 1 + 
  data_fil$feridos_leves * 3 +
  data_fil$feridos_graves * 7 +
  data_fil$mortos * 13

data_fil$Score <- data_fil$Score * 100 / 13

mean_nota <- aggregate(data_fil$Score, by = list(BR = data_fil$br), FUN = mean)

new_table <- merge(new_table, mean_nota, by = "BR", all.x = TRUE)

colnames(new_table)[5] <- "Media_Gravidade"

new_table_sorted <- arrange(new_table, desc(Count))

# Imprimir as 10 maiores BRs
top_6_brs <- head(new_table_sorted, 6)
print(top_6_brs)

data_fil <- na.omit(data_fil, cols = "km")

# 116
data_br116 <- data_fil[data_fil$br == 376, ]

data_br116$km_interval <- cut(data_br116$km, breaks = seq(0, max(data_br116$km) + 10, by = 10), right = FALSE)


# Criar a tabela de contagem das BRs e intervalos de quilometragem
br_km_counts <- table(data_br116$br, data_br116$km_interval)

# Converter a tabela de contagem em um novo dataframe
new_table <- as.data.frame.matrix(br_km_counts)

score2 <- aggregate(data_br116$Score, by = list(BR = data_br116$br, Interval = data_br116$km_interval), FUN = mean)

# Criar a nova tabela no mesmo formato da anterior
new_table <- reshape(score2, idvar = "BR", timevar = "Interval", direction = "wide")
new_table <- new_table[, !colnames(new_table) %in% "BR"]
transposed_table <- t(new_table)
trans2 <- as.data.frame.matrix(transposed_table)
colnames(transposed_table)[1] <- "BR 376"

contagem <- as.data.frame.matrix(br_km_counts)

trans <- t(contagem)
trans3 <- as.data.frame.matrix(trans)
colnames(trans3)[1] <- "Contagem"
write.csv(transposed_table, "score376.csv", row.names = TRUE)
write.csv(trans3, "count376.csv", row.names = TRUE)























print(summary(data$km))
acidentes <- st_as_sf(data_fil, 
                      coords = c("longitude", "latitude"), 
                      crs = 4326)


# Se eu quisesse filtrar só os acidentes que tem mortos:
#data_filter <- data_filter[data_filter$mortos >= 1,]


#o crs 4326 que utilizamos é o sistema de coordenadas geográficas não projetadas WGS84

# Plotando os pontos com ggplot:
acidentes %>%
  ggplot()+
  geom_sf()

# também é possível plotar com a projeção de Mercator, utilizando o st_transform:

acidentes %>% st_transform(3857) %>%
  ggplot() +
  geom_sf()

#outra forma de visualizar é com o tm_shape com o modo tmap, que vai utilizar um mapa interativo como base
# Adicionando uma camada de um mapa do Leafleet
tmap_mode("view")

tm_shape(shp = acidentes) + 
  tm_dots(col = "deepskyblue4", 
          border.col = "black", 
          size = 0.05, 
          alpha = 0.8)



#Outra forma de fazer é utilizar um arquivo ShapeFile com formas geométricas, polígonos
# Carregando um shapefile:
shp_sc <- readOGR(dsn = "SC_shapefile", 
                  layer = "SC_Municipios_2022",
                  encoding = "UTF-8", 
                  use_iconv = TRUE)

shp_pr <- readOGR(dsn = "PR_shapefile", 
                  layer = "PR_Municipios_2022",
                  encoding = "UTF-8", 
                  use_iconv = TRUE)

shp_rs <- readOGR(dsn = "RS_shapefile", 
                  layer = "RS_Municipios_2022",
                  encoding = "UTF-8", 
                  use_iconv = TRUE)

# Criando um objeto com todos os estados
all_states <- list(sc = shp_sc, pr = shp_pr, rs = shp_rs)

# Visualizando os shapefiles de SC, PR e RS
tm_shape(all_states$sc) +
  tm_borders() +
  tm_shape(all_states$pr) +
  tm_borders() +
  tm_shape(all_states$rs) +
  tm_borders() +
  tm_layout(main.title = "Mapa de Estados")

# Visualizando o shapefile carregado:
#tm_shape(shp = shp_sc) +
# tm_borders()

# o modo abaixo permite gerar os gráficos para salvar, mas não para interagir
# tmap_mode("plot")

#unindo as duas observações com ggplot:
cidades_sc <- st_read("SC_shapefile/SC_Municipios_2022.shp")
cidades_sc %>% st_transform(4326) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data=acidentes)

cidades_pr <- st_read("PR_shapefile/PR_Municipios_2022.shp")
cidades_pr %>% st_transform(4326) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data=acidentes)

cidades_rs <- st_read("RS_shapefile/RS_Municipios_2022.shp")
cidades_rs %>% st_transform(4326) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data=acidentes)


# Combinando os dados dos três estados
cidades <- rbind(cidades_sc, cidades_rs, cidades_pr)

#Utilizando o mapa interativo:
tm_shape(shp = cidades) +
  tm_borders()+
  tm_shape(shp = acidentes)+
  tm_dots()

cidades <- cidades %>% st_transform(4326)
acidentes <- acidentes %>% st_transform(4326)
cidades <- cidades %>% rename("municipio"="NM_MUNICIP")
cidades_acidentes <- cidades  %>% 
  st_join(acidentes) 
cidades_num_acidentes <-  cidades_acidentes %>% group_by(municipio.x) %>% 
  tally() 


cidades_num_acidentes %>%
  ggplot() +
  geom_sf(aes(fill=n))

#outra forma:


tm_shape(shp=acidentes)+
  tm_dots(size = 0.01,alpha=0.3)+
  tm_shape(shp=cidades_num_acidentes)+
  tm_fill(col="n",alpha=0.4)+
  tm_borders()


#Análise espacial estatística
# Estabelecendo uma vizinhança:
vizinhos_queen <- poly2nb(pl = cidades_num_acidentes,
                          queen = TRUE,
                          row.names = cidades_num_acidentes$municipio.x)

summary(vizinhos_queen)
# 
# shp_sc <- readOGR(dsn = "shapefile_sc", 
#                   layer = "sc_state",
#                   encoding = "UTF-8", 
#                   use_iconv = TRUE)
# 
# # Observando a vizinhança estabelecida:
# plot.new()
# plot(shp_sc, border = "lightgray")
# plot(vizinhos_queen, 
#      coordinates(shp_sc), 
#      add = TRUE, 
#      col = "#FDE725FF")


# Definindo uma matriz de vizinhanças com padronização em linha:
matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W")

# Observando a matriz de contiguidades, critério queen, padronizada em linha:
colnames(matrizW_queen_linha) <- rownames(matrizW_queen_linha)


# Antes de dar prosseguimento aos estudos das autocorrelações espaciais sobre
# a variável de número de acidentes, vamos observar alguns comportamentos:
tm_shape(shp = cidades_num_acidentes) +
  tm_fill(col = "n", 
          n=10,
          palette = "viridis",
          legend.hist = TRUE) +
  tm_borders() +
  tm_layout(legend.outside = TRUE)

# Salvando o gráfico da acidentes para uso subsequente:
acidentes_plot <- tm_shape(shp = cidades_num_acidentes) +
  tm_fill(col = "n", 
          n=10,
          palette = "viridis",
          legend.hist = TRUE) +
  tm_borders() +
  tm_layout(legend.outside = TRUE)


# Autocorrelação Global – a Estatística I de Moran ------------------------

# Para o cálculo da Estatística I de Moran, nosso algoritmo esperará como
# declaração um objeto de classe listw. Como exemplificação, voltaremos a 
# utilizar o objeto matrizW_queen:
listw_queen <- mat2listw(matrizW_queen_linha)

# Após isso, poderemos utilizar a função moran.test():
moran.test(x = cidades_num_acidentes$n, 
           listw = listw_queen)


###########################################################################

# A quais conclusões preliminares podemos chegar?


# O Diagrama da Estatística I de Moran ------------------------------------
moran.plot(x = cidades_num_acidentes$n, 
           listw = listw_queen, 
           #zero.policy = TRUE,
           xlab = "Óbitos", 
           ylab = "Óbitos epacialmente defasados"
)


# Autocorrelação Local – a Estatística Moran Local ------------------------

# Considerando a variável poverty do objeto shp_co, podemos aferir sua 
# Estatística Moran Local, com o uso da função localmoran(), como se segue:
moran_local <- localmoran(x = cidades_num_acidentes$n, 
                          listw = listw_queen)



# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_sp:
moran_local_mapa <- cbind(cidades_num_acidentes, moran_local)

# Plotando a Estatística Moran Local de forma espacial, sem problemas na escala
# de cores:
moran_local_mapa <- moran_local_mapa %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5))) 

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "faixa_quantis", 
          palette = "plasma",
          n=10) +
  tm_borders() +
  tm_borders() +
  tm_layout(legend.outside = TRUE)

# Salvando o gráfico moran local para comparação subsequente:
moran_local_plot <- tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "faixa_quantis", 
          palette = "plasma",
          n=10) +
  tm_borders() +
  tm_borders() +
  tm_layout(legend.outside = TRUE)

tmap_mode("plot")
# Criando um espaço no ambiente de visualização do R:
plot.new()
pushViewport(
  viewport(
    layout = grid.layout(1,2)
  )
)

# Comparando os dados da pobreza no Centro-Oeste brasileiro com os valores da
# Estatística Moran Local:
# Passo 4: Executar as plotagens
print(acidentes_plot, vp = viewport(layout.pos.col = 1, height = 5))
print(moran_local_plot, vp = viewport(layout.pos.col = 2, height = 5))
tmap_mode("view")
# Plotando as labels dadas pelas estatísticas de Moran (LL, LH, HL, HH):
moran_local_mapa <- cbind(moran_local_mapa, 
                          attr(x = moran_local, which = "quadr")[1])

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "mean", palette = "plasma", alpha=0.5) +
  tm_borders(col = "gray")+
  tm_shape(shp=acidentes)+
  tm_dots(size = 0.01,alpha=0.2)


# Estabelecendo uma Clusterização LISA ------------------------------------


# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes <- vector(mode = "numeric", length = nrow(moran_local))

quadrantes

# Criando um vetor que contenha o centro das observações da variável poverty ao 
# redor de sua média:
variacao_acidentes_cidades <- cidades_num_acidentes$n - mean(cidades_num_acidentes$n)

variacao_acidentes_cidades

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local <- moran_local[,1] - mean(moran_local[,1])

centro_moran_local

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes[variacao_acidentes_cidades > 0 & centro_moran_local > 0] <- "AA"
quadrantes[variacao_acidentes_cidades > 0 & centro_moran_local < 0] <- "AB"
quadrantes[variacao_acidentes_cidades < 0 & centro_moran_local > 0] <- "BA"
quadrantes[variacao_acidentes_cidades < 0 & centro_moran_local < 0] <- "BB"

quadrantes

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes[moran_local[,5] > sig] <- "Estatisticamente_não_significante"

quadrantes

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa["quadrantes"] <- factor(quadrantes)


# Versão do gráfico anterior para daltônicos:
tm_shape(shp = moran_local_mapa) +
  tm_polygons(col = "quadrantes", 
              pal = c(AA = "#FDE725FF",
                      AB = "#7AD151FF", 
                      BA = "#2A788EFF", 
                      BB = "#440154FF",
                      Estatisticamente_não_significante = "white")) +
  tm_borders() +
  tm_layout(legend.outside = TRUE)

# A que conclusões podemos chegar?

# Uma análise rápida sobre o Número de mortos

#avaliando mortos

cidades_num_mortos <- cidades_acidentes %>% group_by(municipio.x) %>% summarise(mortos = sum(mortos))  
cidades_num_mortos[is.na(cidades_num_mortos)] <-0

cidades_num_mortos %>%
  ggplot() +
  geom_sf(aes(fill=mortos))

tm_shape(shp=cidades_num_mortos)+
  tm_fill(col="mortos", alpha = 0.5)+
  tm_borders()+
  tm_shape(shp=acidentes[acidentes$mortos >= 1,])+
  tm_dots(size = 0.01,alpha=0.5)