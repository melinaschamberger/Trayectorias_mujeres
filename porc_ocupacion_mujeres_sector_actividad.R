#Porcentaje de puestos ocupados por mujeres, por sector de actividad
# https://datos.gob.ar/dataset/produccion-porcentaje-puestos-ocupados-por-mujeres-por-sector-actividad

#Librerías
library(readr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(plotly)

## 1. Cargo datos

#Porcentaje de mujeres en el sector privado, por sector de actividad
dos_digitos <- read_csv("https://raw.githubusercontent.com/melinaschamberger/proyecto_shiny/main/Datos/Mujeres_sector_privado/share_mujer_privado_mensual_por_clae2_06122021.csv")
dos_digitos <- dos_digitos %>% rename( "por_mu_sector_privado_clae2" = share_mujer)
tres_digitos <- read_csv("https://raw.githubusercontent.com/melinaschamberger/proyecto_shiny/main/Datos/Mujeres_sector_privado/share_mujer_privado_mensual_por_clae3_06122021.csv")
tres_digitos <- tres_digitos %>% rename( "por_mu_sector_privado_clae3" = share_mujer)
seis_digitos <- read_csv("https://raw.githubusercontent.com/melinaschamberger/proyecto_shiny/main/Datos/Mujeres_sector_privado/share_mujer_privado_mensual_por_clae6_06122021.csv")
seis_digitos <- seis_digitos %>% rename( "por_mu_sector_privado_clae6" = share_mujer)
letras <- read_csv("https://raw.githubusercontent.com/melinaschamberger/proyecto_shiny/main/Datos/Mujeres_sector_privado/share_mujer_privado_mensual_por_letra_06122021.csv")
letras <- letras %>% rename( "por_mu_sector_privado_letra" = share_mujer)

#Porcentaje de mujeres en el sector privado (total)
privado_total <- read_csv("https://raw.githubusercontent.com/melinaschamberger/proyecto_shiny/main/Datos/Mujeres_sector_privado/share_mujer_privado_total_06122021.csv")
privado_total <- privado_total %>% rename( "por_mu_sector_privado_total" = share_mujer)

#Diccionario de clases: porcentaje de ocupación femenina por sector privado y sector de actividad
datos <- read_csv("https://datos.produccion.gob.ar/dataset/162dc5a6-7a95-4f19-9d85-063c9be23208/resource/145dd127-630e-40a5-9a31-239e34a01d36/download/clae_diccionario.csv")

## 2. Analizo participación femenina en sector privado (clasificacion de dos digitos)
colnames(datos)
sp_dos <- datos %>%  select(5:6)
sp_dos <- unique(sp_dos)
sp_dos <- left_join(dos_digitos, sp_dos)
sp_dos <- sp_dos %>% mutate(anio = year(fecha))
#promedio anual en clasificación de dos digitos
sp_dos <- sp_dos %>% 
  group_by(anio, clae2_desc, clae2) %>% 
  summarise(porc_medio = round(mean(por_mu_sector_privado_clae2)*100,2))

#Guardo archivo
write.csv(sp_dos, "sector_privado_dosdigitos.csv", row.names = F)
                  
## 3. Analizo participación femenina en sector privado (clasificacion de tres digitos)
colnames(datos)
sp_tres.a<- datos %>%  select(3:6)
sp_tres <- unique(sp_tres.a)
sp_tres <- left_join(tres_digitos, sp_tres)
sp_tres <- sp_tres %>% mutate(anio = year(fecha))
#promedio anual en clasificación de tres digitos
sp_tres <- sp_tres %>% 
  group_by(anio, clae3_desc, clae3) %>% 
  summarise(porc_medio = round(mean(por_mu_sector_privado_clae3)*100,2))
sp_tres <- left_join(sp_tres.a, sp_tres)
sp_tres <- unique(sp_tres)

## 4. Analizo participación femenina en sector privado (clasificacion de seis digitos)
colnames(datos)
sp_seis.a<- datos %>%  select(1:6)
sp_seis <- unique(sp_seis.a)
sp_seis <- left_join(seis_digitos, sp_seis)
sp_seis <- sp_seis %>% mutate(anio = year(fecha))
#promedio anual en clasificación de tres digitos
sp_seis <- sp_seis %>% 
  group_by(anio, clae6_desc, clae6) %>% 
  summarise(porc_medio = round(mean(por_mu_sector_privado_clae6)*100,2))
sp_seis <- left_join(sp_seis.a, sp_seis)
sp_seis <- unique(sp_seis)

#-------------------------------------------------------------------------------
## 5. Analizo en un gráficos los datos del 2

#ejemplo: año 2017
sp_dos_A <- sp_dos %>%  filter(anio == 2017) %>% arrange(-porc_medio) 
sp_dos_A <- sp_dos_A[1:20,]  

sp_dos_B <- sp_dos %>%  filter(anio == 2017) %>% arrange(porc_medio) 
sp_dos_B <- sp_dos_B[1:20,]  

#Defino la fuente
windowsFonts(A = windowsFont("Roboto"))

#Gráfico de mejores del 2017
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

ggsave("graf_dos.png", width = 40, height = 20, units = "cm")

#Gráfico de peores del 2017
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
ggsave("graf_tres.png", width = 40, height = 20, units = "cm")

#-------------------------------------------------------------------------------
## 6. Analizo participación femenina en empresas
colnames(datos)

#Junto los datos según clasificación de dos digitos
nuevo_sector_dos <- datos %>%  select(5:6)
nuevo_sector_dos <- unique(nuevo_sector_dos)
nuevo_sector_dos <- left_join(dos_digitos, nuevo_sector_dos)
nuevo_sector_dos <- nuevo_sector_dos %>% mutate(anio = year(fecha),
                                                    mes = month(fecha))
#promedio anual en clasificación de dos digitos
nuevo_sector_dos <- nuevo_sector_dos %>% 
  group_by(anio, mes, clae2_desc, clae2) %>% 
  summarise(porc_medio = round(mean(por_mu_sector_privado_clae2)*100,2))

##se espera poder filtrar por año y por sector de empresas

#gráfico inicial con "promedio de todos los sectores
promedio_mensual_gral <- nuevo_sector_dos %>% 
  group_by(anio, mes) %>% 
  summarise(porc_medio = round(mean(porc_medio),2)) %>% 
  mutate(clae2_desc = "Todos",
         clae2 = 0)
#acomodo los datos para unirlos
p1 <- promedio_mensual_gral %>% select(1,2,4,5)
p2 <- promedio_mensual_gral %>% select(3)
p3 <- cbind(p1,p2)
rm(p1,p2)
p3 <- p3 %>% select (1:4,6)
colnames(p3)
p3 <- p3 %>% rename("anio" = "anio...1")

#incorporo al df uno variable que resuma los datos de todas las categorías para cada año
df_juntos <- rbind(nuevo_sector_dos, p3)
df_juntos <- df_juntos %>% mutate(mes_cuali = case_when(
                                    mes == 1 ~ "Enero",
                                    mes == 2 ~ "Febrero",
                                    mes == 3 ~ "Marzo",
                                    mes == 4 ~ "Abril",
                                    mes == 5 ~ "Mayo",
                                    mes == 6 ~ "Junio",
                                    mes == 7 ~ "Julio",
                                    mes == 8 ~ "Agosto",
                                    mes == 9 ~ "Septiembre",
                                    mes == 10 ~ "Octubre",
                                    mes == 11 ~ "Noviembre",
                                    mes == 12 ~ "Diciembre"))

#Guardo el archivo para tomar los datos de acá
write.csv(df_juntos, "Sector_privado_final.csv", row.names = F)

#pruebo aplicando ambos filtros: 

df_juntos <- read.csv("https://raw.githubusercontent.com/melinaschamberger/Trayectorias_mujeres/main/Datos/Mujeres_sector_privado/Sector_privado_final.csv", 
                      encoding = "Latin1")

filtrado <- df_juntos %>% filter(anio == 2007 & clae2 == 0) %>% arrange(mes)
filtrado$mes_cuali <- factor(filtrado$mes_cuali, levels = filtrado[["mes_cuali"]])

#ahora debo realizar el gráfico para mostrar la evolución para cada año de la inserción femenina en empresas privadas
#antes debe ir uno con variación interanual por sector. solo se filtra el sector
filtrado_uno <- df_juntos %>% 
                  filter(clae2_desc == "Todos") %>% 
                  group_by(anio) %>% 
                  summarise(porc_promedio = round(mean(porc_medio),2))

#formato
t <- list(
  family = "Roboto",
  size = 12,
  color = '#50535C')


#grafico
graf_anual <- plot_ly(filtrado_uno, 
                      x = ~anio, y = ~porc_promedio, name = 'Año', type = 'scatter', mode = 'ines+markers',
               line = list(color = '#F2BBC5', width = 3), 
               marker = list(color = '#8C0368', size = 8)) %>% 
  #layout(annotations = valor_inicial)%>% 
  #layout(annotations = valor_final, font = t)
               layout(title = '',
                      xaxis = list(title = "Año"),
                      yaxis = list(title = 'Promedio anual (%)')) %>% 
               layout(font = t)

htmlwidgets::saveWidget(graf_anual, "graf_cuatro.html", selfcontained = T)

#este es para mostrar segundo: más específico por sector (reactive del gráfico anterior) y año.  
valor_inicial <- list(
  xref = 'paper',
  x = 0.05,
  y = porc_medio[1],
  xanchor = 'right',
  yanchor = 'middle',
  text = paste(porc_medio[1], '%'),
  font = list(family = 'Roboto',
              size = 12,
              color = '#50535C'),
  showarrow = FALSE)
valor_final <- list(
  xref = 'paper',
  x = 0.95,
  y = porc_medio[12],
  xanchor = 'left',
  yanchor = 'middle',
  text = paste(porc_medio[12], '%'),
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

fig

htmlwidgets::saveWidget(fig, "graf_cinco.html", selfcontained = T)
