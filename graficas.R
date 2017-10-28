setwd("~/Desktop/1er_Semestre/Sistemas Opera/Proy_SOA")

library(tidyverse)

datos <- read_csv("~/Desktop/1er_Semestre/Sistemas Opera/Proy_SOA/datos2.csv")
#datos <- read_delim("~/Desktop/1er_Semestre/Sistemas Opera/Proy_SOA/datos.csv", "\t", escape_double = FALSE, trim_ws = TRUE)
#datos <- read_delim("~/Desktop/1er_Semestre/Sistemas Opera/Proy_SOA/datos2.csv", "\t", escape_double = FALSE, trim_ws = TRUE)
colnames(datos) <- c("corrida", "servidor", "tiempo_proces", "cliente", "time_stamp", "tasa", "polling")
datos <- datos %>% select(-servidor)
ndatos <- nrow(datos)
datos$time_stamp <- datos$time_stamp - min(datos$time_stamp, na.rm = TRUE)
datos <- datos %>% mutate(dif_tiempos = c(NA, datos$time_stamp[2:ndatos] - datos$time_stamp[1:(ndatos-1)]))
datos <- datos %>% mutate(dif_tiempos = ifelse(dif_tiempos >1000 | dif_tiempos < 0, NA, dif_tiempos))

throughput_df <- datos %>% group_by(polling, tasa) %>% summarise(tasa_out=max(corrida)/100)
throughput_df[nrow(throughput_df)+1,] <- c("Si",100000, 200)
throughput_df[nrow(throughput_df)+1,] <- c("No",100000, 0)
throughput_df[nrow(throughput_df)+1,] <- c("Óptimo/Fatal, resp.",100000, 0)
throughput_df$tasa <- as.double(throughput_df$tasa)
throughput_df$tasa_out <- as.double(throughput_df$tasa_out)
throughput_df <- throughput_df %>% mutate(tasainp = 101000/tasa )
throughput_df[13,3] <- 5110

throughput_df <- throughput_df %>% mutate(tasa_df = (tasainp - tasa_out))
throughput_df <- throughput_df %>% gather(metodo, valor, -tasainp, -tasa, -polling)
throughput_df <- throughput_df %>% mutate(metodo = ifelse(metodo = "tasa_out", "Tasa de sí recibidos", "Tasa no recibidos"))

g1 <- ggplot(data = throughput_df, aes(x = tasainp, y = valor, col = polling, shape = polling)) + geom_line(size = 0.3) +
  geom_point(size = 2) + labs(title = "Throughput", x = "Tasa de paquetes recibidos (pkts/seg)", y = "Tasa de paquetes procesados (pkts/seg)") + theme_bw() +
  ylim(0, 180) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + facet_wrap(~metodo) +
  geom_abline(slope = 1) + scale_color_manual(values=cbPalette) 
g1


prop1 <- sort(sapply(1:101, function(i) datos %>% filter(cliente==i) %>% nrow()))
prop1 <- 100*(prop1/sum(prop1))
prop2 <- sort(sapply(1:101, function(i) datos %>% filter(polling == "Si", cliente==i) %>% nrow()))
prop2 <- 100*(prop2/sum(prop2))
prop3 <- sort(sapply(1:101, function(i) datos %>% filter(polling == "No", cliente==i) %>% nrow()))
prop3 <- 100*(prop3/sum(prop3))

prop <- c(prop1, prop2 ,prop3)
Atención <-  factor(c(ifelse(abs(prop1-100/101) < .15, "Justo", "Injusto"), 
                      ifelse(abs(prop2-100/101) < .15, "Justo", "Injusto"),
                      ifelse(abs(prop3-100/101) < .15, "Justo", "Injusto")))

clientes_df <- data_frame(cliente = rep(1:101, 3), prop = prop, Atención = Atención, Experimento = rep(c("Ambas","Con Poleo","Sin Poleo"), each = 101))
cbPalette <- c("#a9a9a9", "#a9a9a9", "#a9a9a9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


g2 <- ggplot(clientes_df, aes(x = cliente, y = prop, fill = Atención)) +
  geom_bar(stat="identity", width = 1, col = 'white', size = 0.01 ) +
  scale_y_continuous(breaks = round(seq(0, (max(prop) + 0.02), by = 0.1), 2)) +
  scale_fill_manual(values=cbPalette) +
  labs(title = "Atención por Clientes") + xlab("Clientes") + ylab("Proporción de atencion por Clientes %") + 
  theme_bw() + facet_wrap(~Experimento)

g2

throughput_df <- datos %>% group_by(polling, tasa) %>% summarise(latencia=mean(dif_tiempos, na.rm = TRUE))
throughput_df <- throughput_df %>% mutate(tasainp = 10100000/tasa)

g3 <- ggplot(data = throughput_df, aes(x = tasainp/100, y = latencia/1000, col = polling, shape = polling)) + geom_line(size = 0.3) +
  geom_point(size = 3) + labs(title = "Latencia", x = "Tasa de paquetes recibidos (pkts/seg)", y = "Latencia promedio en seg de procesamiento") +
  theme_bw() + scale_color_manual(values=cbPalette)
g3

