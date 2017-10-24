datos <- read_delim("~/Desktop/1er_Semestre/Sistemas Opera/Proy_SOA/datos.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
colnames(datos) <- c("corrida", "servidor", "tiempo_proces", "cliente", "time_stamp", "tasa", "polling")
datos <- datos %>% select(-servidor)
datos$time_stamp <- datos$time_stamp - min(datos$time_stamp)
datos <- datos %>% mutate(dif_tiempos = c(NA, datos$time_stamp[2:66975] - datos$time_stamp[1:66974]))
datos <- datos %>% mutate(dif_tiempos = ifelse(dif_tiempos >1000 | dif_tiempos < 0, NA, dif_tiempos))

throughput_df <- datos %>% group_by(polling, tasa) %>% summarise(tasa_recibido=max(corrida))
throughput_df <- throughput_df %>% mutate(tasainp = 10100000/tasa )
throughput_df[13,3] <- 5110

g1 <- ggplot(data = throughput_df, aes(x = tasainp, y = tasa_recibido, col = polling)) + geom_line(size = 0.3) +
  geom_point() + labs(title = "Throughput", x = "Input Packet Rate (pkts/sec)", y = "Output Packet Rate (pkts/sec)") + theme_bw()  + ylim(1000, 18000)
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
g2 <- ggplot(clientes_df, aes(x = cliente, y = prop, fill = Atención)) +
  geom_bar(stat="identity", width = 1, alpha = 0.95, col = 'gray') +
  scale_y_continuous(breaks = round(seq(0, (max(prop) + 0.02), by = 0.1), 2)) +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red")) +
  labs(title = "Atención por Clientes") + xlab("Clientes") + ylab("Proporción de atencion por Clientes %") + 
  theme_bw() + facet_wrap(~Experimento)
g2

throughput_df <- datos %>% group_by(polling, tasa) %>% summarise(latencia=mean(dif_tiempos, na.rm = TRUE))
throughput_df <- throughput_df %>% mutate(tasainp = 10100000/tasa)

g3 <- ggplot(data = throughput_df, aes(x = tasainp, y = latencia, col = polling)) + geom_line(size = 0.3) +
  geom_point() + labs(title = "Latencia", x = "Input Packet Rate (pkts/sec)", y = "Latencia promedio por paquete recibido") + theme_bw()  
g3