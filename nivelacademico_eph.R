#Activo librerías
library(tidyverse)

#Importo base de datos
eph2021<-read.csv("https://raw.githubusercontent.com/melinaschamberger/Trayectorias_mujeres/main/Datos/trayectoria_ecucativa/usu_individual_T221.csv", sep=";", encoding = "UTF-8")

#Exploro base
colnames(eph2021)

#Limpio base
eph2021<-rename(eph2021, REGION = X.U.FEFF.REGION)
eph2021<-rename(eph2021, ESTADO = FINALIZADO)
eph2021$AGLOMERADO <- NULL

#Modifico valores de acuerdo al índice metodológico publicado por INDEC.

eph2021<-mutate(eph2021, REGION = case_when(REGION == 01 ~ 'GBA',
                                            REGION == 40 ~ 'NOA',
                                            REGION == 41 ~ 'NEA',
                                            REGION == 42 ~ 'CUYO',
                                            REGION == 43 ~ 'PAMPEANA',
                                            REGION == 44 ~ 'PATAGONIA'))
                
eph2021<-mutate(eph2021, SEXO = case_when(SEXO == 1 ~ 'VARÓN',
                                          SEXO == 2 ~ 'MUJER'))

eph2021<-mutate(eph2021, FINALIZADO = case_when(ESTADO == 1 ~ 'COMPLETO',
                                                ESTADO == 2 ~ 'INCOMPLETO',
                                                ESTADO == 3 ~ 'NS/NC'))

eph2021<-mutate(eph2021, MAX.NIVEL.CURSADO = case_when(MAX.NIVEL.CURSADO == 1 ~ 'JARDÍN/PREESCOLAR',
                                                       MAX.NIVEL.CURSADO == 2 ~ 'PRIMARIO',
                                                       MAX.NIVEL.CURSADO == 3 ~ 'EGB',
                                                       MAX.NIVEL.CURSADO == 4 ~ 'SECUNDARIO',
                                                       MAX.NIVEL.CURSADO == 5 ~ 'POLIMODAL',
                                                       MAX.NIVEL.CURSADO == 6 ~ 'TERCIARIO',
                                                       MAX.NIVEL.CURSADO == 7 ~ 'UNIVERSITARIO',
                                                       MAX.NIVEL.CURSADO == 8 ~ 'POSGRADO UNIVERSITARIO',
                                                       MAX.NIVEL.CURSADO == 9 ~ 'EDUCACIÓN ESPECIAL'))

#Creo nueva variable 'NIVEL.EDUCATIVO' combinando MAX.NIVEL.CURSADO y ESTADO

eph2021<-mutate(eph2021, NIVEL.EDUCATIVO = case_when(MAX.NIVEL.CURSADO == 'JARDÍN/PREESCOLAR' & ESTADO == 'COMPLETO'~ 'JARDÍN/PREESCOLAR COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'JARDÍN/PREESCOLAR' & ESTADO == 'INCOMPLETO'~ 'JARDÍN/PREESCOLAR INCOMPLETO',
                                                     MAX.NIVEL.CURSADO == 'JARDÍN/PREESCOLAR' & ESTADO == 'NS/NC'~ 'JARDÍN/PREESCOLAR SIN ESPECIFICAR',
                                                     MAX.NIVEL.CURSADO == 'PRIMARIO' & ESTADO == 'COMPLETO'~ 'PRIMARIO COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'PRIMARIO' & ESTADO == 'INCOMPLETO'~ 'PRIMARIO INCOMPLETO',
                                                     MAX.NIVEL.CURSADO == 'PRIMARIO' & ESTADO == 'NS/NC'~ 'PRIMARIO SIN ESPECIFICAR',
                                                     MAX.NIVEL.CURSADO == 'EGB' & ESTADO == 'COMPLETO'~ 'EGB COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'EGB' & ESTADO == 'INCOMPLETO'~ 'EGB INCOMPLETO',
                                                     MAX.NIVEL.CURSADO == 'EGB' & ESTADO == 'NS/NC'~ 'EGB SIN ESPECIFICAR',
                                                     MAX.NIVEL.CURSADO == 'SECUNDARIO' & ESTADO == 'COMPLETO'~ 'SECUNDARIO COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'SECUNDARIO' & ESTADO == 'INCOMPLETO'~ 'SECUNDARIO INCOMPLETO',
                                                     MAX.NIVEL.CURSADO == 'SECUNDARIO' & ESTADO == 'NS/NC'~ 'SIN ESPECIFICAR',
                                                     MAX.NIVEL.CURSADO == 'POLIMODAL' & ESTADO == 'COMPLETO'~ 'POLIMODAL COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'POLIMODAL' & ESTADO == 'INCOMPLETO'~ 'POLIMODAL INCOMPLETO',
                                                     MAX.NIVEL.CURSADO == 'POLIMODAL' & ESTADO == 'NS/NC'~ 'POLIMODAL SIN ESPECIFICAR',
                                                     MAX.NIVEL.CURSADO == 'TERCIARIO' & ESTADO == 'COMPLETO'~ 'TERCIARIO COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'TERCIARIO' & ESTADO == 'INCOMPLETO'~ 'TERCIARIO COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'TERCIARIO' & ESTADO == 'NS/NC'~ 'TERCIARIO SIN ESPECIFICAR',
                                                     MAX.NIVEL.CURSADO == 'UNIVERSITARIO' & ESTADO == 'COMPLETO'~ 'UNIVERSITARIO COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'UNIVERSITARIO' & ESTADO == 'INCOMPLETO'~ 'UNIVERSITARIO INCOMPLETO',
                                                     MAX.NIVEL.CURSADO == 'UNIVERSITARIO' & ESTADO == 'NS/NC'~ 'UNIVERSITARIO SIN ESPECIFICAR',
                                                     MAX.NIVEL.CURSADO == 'POSGRADO UNIVERSITARIO' & ESTADO == 'COMPLETO'~ 'POSGRADO UNIVERSITARIO COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'POSGRADO UNIVERSITARIO' & ESTADO == 'INCOMPLETO'~ 'POSGRADO UNIVERSITARIO INCOMPLETO',
                                                     MAX.NIVEL.CURSADO == 'POSGRADO UNIVERSITARIO' & ESTADO == 'NS/NC'~ 'POSGRADO UNIVERSITARIO SIN ESPECIFICAR',
                                                     MAX.NIVEL.CURSADO == 'EDUCACIÓN ESPECIAL' & ESTADO == 'COMPLETO'~ 'EDUCACIÓN ESPECIAL COMPLETO',
                                                     MAX.NIVEL.CURSADO == 'EDUCACIÓN ESPECIAL' & ESTADO == 'INCOMPLETO'~ 'EDUCACIÓN ESPECIAL INCOMPLETO',
                                                     MAX.NIVEL.CURSADO == 'EDUCACIÓN ESPECIAL' & ESTADO == 'NS/NC'~ 'EDUCACIÓN ESPECIAL SIN ESPECIFICAR')) 

                                                