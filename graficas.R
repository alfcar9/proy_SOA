library(tidyverse)
library(knitr)
datos <- read_delim("~/Desktop/1er_Semestre/Sistemas Opera/Proyecto/proy.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
datos %>% filter(Polling == "No") %>% nrow()
colnames(datos) <- c("corrida", "servidor", "tiempo_proces", "cliente", "time_stamp", "tasa", "polling")
datos <- datos %>% select(-servidor)
datos <- datos %>% mutate(dif_tiempos = c(NA, abs(datos$time_stamp[2:66975] - datos$time_stamp[1:66974])))
datos <- datos %>% mutate(recibido = ifelse(tiempo_proces < 10, TRUE, FALSE))
y <- sort(sapply(1:101, function(i) datos %>% filter(cliente==i) %>% nrow()))
y <- 100*(y/sum(y))
clientes_df <- data_frame(x = 1:101, y = y, Atención = factor(ifelse(abs(x-50) < 25, "Justo", "Injusto")))
g1 <- ggplot(clientes_df, aes(x = x, y = y, fill = Atención)) +
  geom_segment(x = 0, xend = 102, y = quantile(y, 0.48), yend = quantile(y, 0.48), col = 'ivory', size = 25, alpha = 0.1) +
  geom_segment(x = 0, xend = 102, y = quantile(y, .95), yend = quantile(y, .95), col = 'ivory', size = 26, alpha = 0.1) +
  geom_segment(x = 0, xend = 102, y = quantile(y, .05), yend = quantile(y, .05), col = 'ivory', size = 28, alpha = 0.1) +
  geom_bar(stat="identity", width = 1, alpha = 0.95, col = 'gray') +
  scale_y_continuous(breaks = round(seq(0, (max(y) + 0.02), by = 0.1), 2)) +
  scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red")) +
  labs(title = "Atención por Clientes") + xlab("Clientes") + ylab("Proporción de atencion por Clientes %")

g1

g2 <- ggplot() + geom_histogram(aes(x = datos$dif_tiempos, y = ..density.., is.na = FALSE), col = 'black', alpha = 0.2, fill='blue') + 
    labs(title = "Latencia", x = "Nanosegundos", y = " ") + xlim(0,60)
g2

summary_df <- datos %>% group_by(polling, tasa) %>% summarise(tasa_recibido=mean(recibido), tiempo=mean(tiempo_proces))

g3 <- ggplot(data = (summary_df), aes(x = tasa, y = tasa*tasa_recibido, col = polling)) + geom_line(size = 0.3) +
  geom_point() + labs(title = "Latencia", x = "Tasa", y = "Nanosegundos")
g3

g4 <- ggplot(data = summary_df %>% filter(!is.na(tiempo)), aes(x = tasa, y = tiempo, col = polling)) + geom_line() + geom_point() +
  labs(title = "Latencia", x = "Tasa", y = "Nanosegundos")
g4

# Numero de datos
num_df <- cbind(sapply(unique(datos$tasa), function(i) datos %>% filter(tasa == i, polling == "Si") %>% nrow()), sapply(unique(datos$tasa), function(i) datos %>% filter(tasa == i, polling == "No") %>% nrow()))
