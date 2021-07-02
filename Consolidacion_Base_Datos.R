library(readr)
library(tidyverse)
library(genero)
library(eeptools)

#Cargar datos
IMC <- read_csv("Datos/Base de datos empleados/IMC-Table 1.csv", 
                     col_types = cols(`AÑO NACIMIENTO` = col_integer(), 
                     ESTATURA = col_number(), PESO = col_number(), 
                     IMC = col_number()))
HABITOS <- read_csv("Datos/Base de datos empleados/HÁBITOS-Table 1.csv", 
                     col_types = cols(`AÑO NACIMIENTO` = col_integer()))
PADECIMIENTOS <- read_csv("Datos/Base de datos empleados/PADECIMIENTOS-Table 1.csv", 
                     col_types = cols(`AÑO DE NACIMIENTO` = col_integer()))
#---------------------------------------NUEVA BASE DE DATOS--------------------------------#
        #Nombre
PERSONAS <- as.data.frame(IMC$NOMBRES)
colnames(PERSONAS) <- "NOMBRES"
        #Apellido        
PERSONAS$APELLIDOS <- NA
PERSONAS$APELLIDOS <- IMC$APELLIDOS
        #Celular
PERSONAS$CELULAR <- NA
PERSONAS$CELULAR <- IMC$CELULAR
        #Sexo
PERSONAS$SEXO <- genero(PERSONAS$NOMBRES,
                        lang = "es")
PERSONAS$SEXO[PERSONAS$SEXO=="male"] <- "HOMBRE"
PERSONAS$SEXO[PERSONAS$SEXO=="female"] <- "MUJER"
Nombres_sin_sexo_MasVida_corregido <- read_csv("Datos/Nombres_sin_sexo_MasVida_corregido.csv", 
                                               col_types = cols(X5 = col_skip()))
PERSONAS$SEXO[match(Nombres_sin_sexo_MasVida_corregido$`NUMERO ID`,
      PERSONAS$CELULAR)] <- Nombres_sin_sexo_MasVida_corregido$SEXO
PERSONAS$SEXO[PERSONAS$SEXO=="M"] <- "HOMBRE"
PERSONAS$SEXO[PERSONAS$SEXO=="F"] <- "MUJER"
PERSONAS$SEXO <- as.factor(PERSONAS$SEXO)
rm(Nombres_sin_sexo_MasVida_corregido)
        #Año de nacimiento
PERSONAS$NACIMIENTO <- NA
PERSONAS$NACIMIENTO <- IMC$`AÑO NACIMIENTO`
        #Edad aproximada
PERSONAS$EDAD_APROX <- NA
PERSONAS$EDAD_APROX<- (2021-PERSONAS$NACIMIENTO)
                #Corregir la edad aproximada para eliminar los valores mayores de 75 años
                #El 75 fue elegido al azar pero se consideró que la edad de retiro es 60
                #Sin embargo el 12% de la muestra es mayor que 60=>75 años
                #Tambien se eliminaron valores <19 años de edad.
        PERSONAS$EDAD_APROX <- as.integer(PERSONAS$EDAD_APROX)
        PERSONAS$EDAD_APROX[PERSONAS$EDAD_APROX>75] <- NA
        PERSONAS$EDAD_APROX[PERSONAS$EDAD_APROX<19] <- NA       
        #Indice de masa corporal
PERSONAS$IMC <- NA
PERSONAS$IMC <- IMC$IMC
PERSONAS$IMC[PERSONAS$IMC==0] <- NA
        #Alimentacion
PERSONAS$ALIMENTACION_BALANCEADA <- NA
PERSONAS$ALIMENTACION_BALANCEADA <- HABITOS$`ALIMENTACIÓN BALANCEADA`
PERSONAS$ALIMENTACION[PERSONAS$ALIMENTACION_BALANCEADA=="No"] <- "A VECES"
PERSONAS$ALIMENTACION_BALANCEADA <- as.factor(PERSONAS$ALIMENTACION_BALANCEADA)
        #Actividad fisica
PERSONAS$ACTIVIDAD_FISICA <- NA
PERSONAS$ACTIVIDAD_FISICA <- HABITOS$`ACTIVIDAD FÍSICA`
PERSONAS$ACTIVIDAD_FISICA[is.na(PERSONAS$ACTIVIDAD_FISICA)] <- "NO"
PERSONAS$ACTIVIDAD_FISICA <- as.factor(PERSONAS$ACTIVIDAD_FISICA)
        #Frecuencia AF
PERSONAS$FRECUENCIA_AF <- NA
PERSONAS$FRECUENCIA_AF <- HABITOS$`FRECUENCIA DE ACTIVIDAD FÍSICA`
PERSONAS$FRECUENCIA_AF <- as.factor(PERSONAS$FRECUENCIA_AF)
        #Frecuenca con la que fuma
PERSONAS$FRECUENCIA_FUMA <- NA
PERSONAS$FRECUENCIA_FUMA <- HABITOS$FUMA
PERSONAS$FRECUENCIA_FUMA <- as.factor(PERSONAS$FRECUENCIA_FUMA)
        #Frecuencia con la que bebe alcohol
PERSONAS$FRECUENCIA_ALCOHOL <- NA
PERSONAS$FRECUENCIA_ALCOHOL <- HABITOS$`BEBE ALCOHOL`
PERSONAS$FRECUENCIA_ALCOHOL <- as.factor(PERSONAS$FRECUENCIA_ALCOHOL)
#-----------------------------------------------------------------------------------------#
#---------------------------NUEVAS VARIABLES----------------------------------------------#
        #Fuma
PERSONAS$FUMA <- NA
PERSONAS$FUMA <- PERSONAS$FRECUENCIA_FUMA
PERSONAS$FUMA <- as.character(PERSONAS$FUMA)
PERSONAS$FUMA[PERSONAS$FUMA=="MUCHO"] <- "SI"
PERSONAS$FUMA[PERSONAS$FUMA=="POCO"] <- "SI"
PERSONAS$FUMA <- as.factor(PERSONAS$FUMA)
        #Bebe alcohol
PERSONAS$ALCOHOL <- NA
PERSONAS$ALCOHOL <- PERSONAS$FRECUENCIA_ALCOHOL
PERSONAS$ALCOHOL <- as.character(PERSONAS$ALCOHOL)
PERSONAS$ALCOHOL[PERSONAS$ALCOHOL=="-"] <- NA
PERSONAS$ALCOHOL[PERSONAS$ALCOHOL=="1-NO"] <- "NO"
PERSONAS$ALCOHOL[PERSONAS$ALCOHOL=="2-SI SOCIALMENTE"] <- "SI"
PERSONAS$ALCOHOL[PERSONAS$ALCOHOL=="3-SI, 2..3 Vces x smna"] <- "SI"
PERSONAS$ALCOHOL[PERSONAS$ALCOHOL=="4-Si, Casi siempre"] <- "SI"
PERSONAS$ALCOHOL <- as.factor(PERSONAS$ALCOHOL)
        #Grupo de edad
PERSONAS$GRUPO_EDAD <- NA
PERSONAS$GRUPO_EDAD[PERSONAS$EDAD_APROX<60] <- "ADULTO"
PERSONAS$GRUPO_EDAD[PERSONAS$EDAD_APROX>59] <- "ADULTO MAYOR"
PERSONAS$GRUPO_EDAD <- as.factor(PERSONAS$GRUPO_EDAD)
        #Estado nutricional
PERSONAS$ESTADO_NUTRI <- NA
                #Adultos
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC<18)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO"] <- "DESNUTRICION"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=18 & PERSONAS$IMC<21)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO"] <- "DELGADO"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=21 & PERSONAS$IMC<25)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO"] <- "NORMAL"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=25 & PERSONAS$IMC<30)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO"] <- "SOBREPESO"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=30 & PERSONAS$IMC<40)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO"] <- "OBESIDAD"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=40)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO"] <- "OBESIDAD MORBIDA"
                #Adultos mayores
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC<19)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO MAYOR"] <- "DESNUTRICION"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=19 & PERSONAS$IMC<23)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO MAYOR"] <- "DELGADO"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=23 & PERSONAS$IMC<28)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO MAYOR"] <- "NORMAL"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=28 & PERSONAS$IMC<32)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO MAYOR"] <- "SOBREPESO"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=32 & PERSONAS$IMC<40)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO MAYOR"] <- "OBESIDAD"
        PERSONAS$ESTADO_NUTRI[(PERSONAS$IMC>=40)&
                                      PERSONAS$GRUPO_EDAD=="ADULTO MAYOR"] <- "OBESIDAD MORBIDA"
PERSONAS$ESTADO_NUTRI <- as.factor(PERSONAS$ESTADO_NUTRI)
PERSONAS$ESTADO_NUTRI <- factor(PERSONAS$ESTADO_NUTRI,
                                levels = c("DESNUTRICION","DELGADO","NORMAL",
                                           "SOBREPESO","OBESIDAD","OBESIDAD MORBIDA"))
        #Agregar jóvenes como grupo de edad
PERSONAS$GRUPO_EDAD <- as.character(PERSONAS$GRUPO_EDAD)
PERSONAS$GRUPO_EDAD[PERSONAS$EDAD_APROX<30] <- "JOVEN"
PERSONAS$GRUPO_EDAD <- as.factor(PERSONAS$GRUPO_EDAD)
PERSONAS$GRUPO_EDAD <- factor(PERSONAS$GRUPO_EDAD,
                              levels = c("JOVEN","ADULTO","ADULTO MAYOR"))
        #Padecimientos
PERSONAS$ENFERMEDADES <- NA
PERSONAS$ENFERMEDADES <- 
        PADECIMIENTOS$`TIPO DE ENFERMEDAD`[match(PERSONAS$CELULAR,PADECIMIENTOS$CELULAR)]
        #Categoria de reporte de enfermedad
PERSONAS$REPORTA_ENFERMEDAD <- NA
PERSONAS$REPORTA_ENFERMEDAD[!is.na(PERSONAS$ENFERMEDADES)] <- "SI"
PERSONAS$REPORTA_ENFERMEDAD[is.na(PERSONAS$ENFERMEDADES)]<- "NO"

#-----------------------------------------------------------------------------------------#
