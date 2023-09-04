# Installation of Packages

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

# Reading and Basic Data Preprocessing, here you can modify the document in the first line to handle other years as well

data <- read.csv("acidentes2019.csv",sep=";",dec = ",")
estados_filtrar <- c("SC", "PR", "RS")
data_filter <- data[data$uf %in% estados_filtrar & data$idade < 120 & data$ano_fabricacao_veiculo > 1950 & data$km != "?" & data$km != 0 & data$estado_fisico != "Nao Informado" & data$latitude > -33 & data$latitude < -21 & data$longitude > -58 & data$longitude < -47,]
data_fil <- data_filter[, !(colnames(data_filter) %in% c("pesid", "marca" , "id_veiculo" , "regional" , "delegacia" , "uop"))]
data_fil <- data_fil[complete.cases(data_fil[c("latitude", "longitude", "km")]), ]
br_counts <- table(data_fil$br)
new_table <- data.frame(BR = names(br_counts), Count = as.vector(br_counts))
max_km <- aggregate(data$km, by = list(BR = data$br), FUN = max)
new_table <- merge(new_table, max_km, by = "BR", all.x = TRUE)
colnames(new_table)[3] <- "Quilometros"
new_table$Acidentes_por_km <- new_table$Count / new_table$Quilometros

# Calculate the accident severity weights and normalize the data

data_fil$Score <- data_fil$ilesos * 1 + 
  data_fil$feridos_leves * 3 +
  data_fil$feridos_graves * 7 +
  data_fil$mortos * 13

data_fil$Score <- data_fil$Score * 100 / 13
mean_nota <- aggregate(data_fil$Score, by = list(BR = data_fil$br), FUN = mean)
new_table <- merge(new_table, mean_nota, by = "BR", all.x = TRUE)
colnames(new_table)[5] <- "Media_Gravidade"
new_table_sorted <- arrange(new_table, desc(Count))

# Print the top 6 highways with the highest number of accidents per kilometer

top_6_brs <- head(new_table_sorted, 6)
print(top_6_brs)

data_fil <- na.omit(data_fil, cols = "km")

# For each item in this list, manually change the value in the line below

num_br <- 101
data_brnum <- data_fil[data_fil$br == num_br, ]
data_brnum$km_interval <- cut(data_brnum$km, breaks = seq(0, max(data_brnum$km) + 10, by = 10), right = FALSE)

# Create a table with the count of highways (BRs) and their kilometer intervals

br_km_counts <- table(data_brnum$br, data_brnum$km_interval)
new_table <- as.data.frame.matrix(br_km_counts)
score2 <- aggregate(data_brnum$Score, by = list(BR = data_brnum$br, Interval = data_brnum$km_interval), FUN = mean)

# Create the new table in the same format as the previous one

new_table <- reshape(score2, idvar = "BR", timevar = "Interval", direction = "wide")
new_table <- new_table[, !colnames(new_table) %in% "BR"]
transposed_table <- t(new_table)
trans2 <- as.data.frame.matrix(transposed_table)
colnames(transposed_table)[1] <- paste("BR ", num_br)
contagem <- as.data.frame.matrix(br_km_counts)
trans <- t(contagem)
trans3 <- as.data.frame.matrix(trans)
colnames(trans3)[1] <- "Contagem"
write.csv(transposed_table, paste("score", num_br, ".csv"), row.names = TRUE)
write.csv(trans3, paste("count", num_br, ".csv"), row.names = TRUE)
