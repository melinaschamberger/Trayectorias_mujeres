#Porcentaje de puestos ocupados por mujeres, por sector de actividad
# https://datos.gob.ar/dataset/produccion-porcentaje-puestos-ocupados-por-mujeres-por-sector-actividad

#Librerías
library(readr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(plotly)

#Defino la fuente
windowsFonts(A = windowsFont("Roboto"))
#Formato
t <- list(
  family = "Roboto",
  size = 12,
  color = '#50535C')

## 1. Cargo datos
#Sector productivo: valores anuales
df_juntos <- read.csv("https://raw.githubusercontent.com/melinaschamberger/Trayectorias_mujeres/main/Datos/Mujeres_sector_privado/Sector_privado_final.csv", 
                      encoding = "Latin1")

#Sector productivo: dos digitos
sp_dos <- read.csv("https://raw.githubusercontent.com/melinaschamberger/Trayectorias_mujeres/main/Datos/Mujeres_sector_privado/sector_privado_dosdigitos.csv",
                   encoding = "Latin1")

## Gráficos de línea
### Filtrados por reactive
filtrado_uno <- df_juntos %>% 
  filter(clae2_desc == "Todos") %>% 
  group_by(anio) %>% 
  summarise(porc_promedio = round(mean(porc_medio),2))

#grafico
graf_anual <- plot_ly(filtrado_uno, 
                      x = ~anio, y = ~porc_promedio, name = 'Año', type = 'scatter', mode = 'ines+markers',
                      line = list(color = '#F2BBC5', width = 3), 
                      marker = list(color = '#8C0368', size = 8)) %>% 
  layout(title = '',
         xaxis = list(title = "Año"),
         yaxis = list(title = 'Promedio anual (%)')) %>% 
  layout(font = t)

### Filtrados por reactive
filtrado <- df_juntos %>% filter(anio == 2007 & clae2 == 0) %>% arrange(mes)
filtrado$mes_cuali <- factor(filtrado$mes_cuali, levels = filtrado[["mes_cuali"]])

valor_inicial <- list(
  xref = 'paper',
  x = 0.05,
  y = filtrado$porc_medio[1],
  xanchor = 'right',
  yanchor = 'middle',
  text = paste(filtrado$porc_medio[1], '%'),
  font = list(family = 'Roboto',
              size = 12,
              color = '#50535C'),
  showarrow = FALSE)
valor_final <- list(
  xref = 'paper',
  x = 0.95,
  y = filtrado$porc_medio[12],
  xanchor = 'left',
  yanchor = 'middle',
  text = paste(filtrado$porc_medio[12], '%'),
  font = list(family = 'Roboto',
              size = 12,
              color = '#50535C'),
  showarrow = FALSE)


fig <- plot_ly(filtrado, x = ~mes_cuali, y = ~porc_medio, name = 'trace 0', type = 'scatter', mode = 'ines+markers',
               line = list(color = '#BC91D9', width = 3), 
               marker = list(color = '#362840', size = 8)) %>% 
  layout(title = "",
         xaxis = list(title = "Mes"), 
         yaxis = list(title = " "))  %>% layout(annotations = valor_inicial)%>% 
  layout(annotations = valor_final, font = t)

## Gráficos de barra
### Filtrados por reactive
sp_dos_A <- sp_dos %>%  filter(anio == 2017) %>% arrange(-porc_medio) 
sp_dos_A <- sp_dos_A[1:20,]  
sp_dos_B <- sp_dos %>%  filter(anio == 2017) %>% arrange(porc_medio) 
sp_dos_B <- sp_dos_B[1:20,]  

#Gráfico de mejores
graf_dos <- ggplot(sp_dos_A, aes(x=reorder(clae2_desc, porc_medio), 
                                 y=porc_medio, 
                                 fill = porc_medio)) +  
  geom_bar(stat="identity", 
           width = 0.4) + 
  coord_flip() + 
  geom_text(aes(label = porc_medio), 
            colour = "black", 
            fontface = "italic", 
            family = "A",
            size = 2.5, 
            hjust = -0.10) + 
  xlab("Sector") + 
  ylab(" ") + 
  labs(
    title = "Mujeres ocupadas por sector de actividad (%)",
    caption = "Fuente: Elaboración propia en base a datos provistos por el Ministerio de Desarrollo Productivo (2021).",
    subtitle = "20 sectores con mayor participación femenina."
  )  +
  theme_minimal() + 
  theme(legend.position = "null", 
        text=element_text(size=12, 
                          family = "A", 
                          colour = "#50535C"),
        plot.caption = element_text(hjust = 0.85)) + 
  scale_fill_gradientn(colours = c("#FAAE7B", "#CC8B79","#9F6976","#714674","#432371", "#3d2067"),
                       values = scales::rescale(c(45,50,60,70,80, 88))) +
  scale_y_continuous(limits = c(0, 90),
                     breaks = seq(0, 90, by = 10), 
                     labels = sprintf("%.f%%", seq(0,90, by = 10)))

#Gráfico de peores
graf_tres <- ggplot(sp_dos_B, aes(x=reorder(clae2_desc, -porc_medio), 
                                  y=porc_medio, 
                                  fill = porc_medio)) +  
  geom_bar(stat="identity", 
           width = 0.4) + 
  coord_flip() + 
  geom_text(aes(label = porc_medio), 
            colour = "black", 
            fontface = "italic", 
            family = "A",
            size = 2.5, 
            hjust = -0.10) + 
  xlab("Sector") + 
  ylab(" ") + 
  labs(
    title = "Mujeres ocupadas por sector de actividad (%)",
    subtitle = "20 sectores con menor participación femenina.",
    caption = "Fuente: Elaboración propia en base a datos provistos por el Ministerio de Desarrollo Productivo (2021)."
  )  +
  theme_minimal() + 
  theme(legend.position = "null", 
        text=element_text(size=12, 
                          family = "A", 
                          colour = "#50535C"),
        plot.caption = element_text(hjust = 0.85)) + 
  scale_fill_gradientn(colours = c( "#3D2067", "#714674","#B67A78","#D8947A", "#FAAE7B","#FAB587"),
                       values = scales::rescale(c(11,8,5,2,0))) +
  scale_y_continuous(limits = c(0, 90),
                     breaks = seq(0, 90, by = 5), 
                     labels = sprintf("%.f%%", seq(0,90, by = 5)))
