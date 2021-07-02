library(readr)
library(tidyverse)

#----------------------------------GRAFICOS----------------------------------------------#
#Estado nutricional por sexo y grupo de edad
PERSONAS %>%
        filter(!is.na(PERSONAS$SEXO)&!is.na(PERSONAS$GRUPO_EDAD)&
                       !is.na(PERSONAS$ESTADO_NUTRI)) %>%
        ggplot(aes(ESTADO_NUTRI,fill=SEXO))+
        theme_bw()+
        geom_bar()+
        facet_wrap(~GRUPO_EDAD)+
        theme(axis.text.x = element_text(angle = 45,hjust=1))+
        xlab(label = "Estado Nutricional")+
        ylab(label = "Empleados municipales")+
        ggtitle(label = "Estado Nutricional por Grupo de Edad")

#Estado nutricional por sexo y grupo de edad en proporcion
PERSONAS %>%
        filter(!is.na(PERSONAS$SEXO)&!is.na(PERSONAS$GRUPO_EDAD)&
                       !is.na(PERSONAS$ESTADO_NUTRI)) %>%
        ggplot(aes(GRUPO_EDAD,fill=SEXO))+
        theme_bw()+
        geom_bar(position = "fill")+
        facet_wrap(~ESTADO_NUTRI)+
        theme(axis.text.x = element_text(angle = 45,hjust=1))+
        xlab(label = "Grupo de edad")+
        ylab(label = "Proporción (%)")+
        ggtitle(label = "Estado Nutricional por Grupo de Edad")

#Estado nutricional por sexo y grupo de edad (lado a lado)
PERSONAS %>%
        filter(!is.na(PERSONAS$SEXO)&!is.na(PERSONAS$GRUPO_EDAD)&
                       !is.na(PERSONAS$ESTADO_NUTRI)) %>%
        ggplot(aes(ESTADO_NUTRI,fill=SEXO))+
        theme_bw()+
        geom_bar(position = "dodge")+
        facet_wrap(~GRUPO_EDAD)+
        theme(axis.text.x = element_text(angle = 45,hjust=1))+
        xlab(label = "Estado Nutricional")+
        ylab(label = "Empleados municipales")+
        ggtitle(label = "Estado Nutricional por Grupo de Edad")

#Reporta enfermedad (lado a lado)
PERSONAS %>%
        filter(!is.na(PERSONAS$SEXO)&!is.na(PERSONAS$GRUPO_EDAD)&
                       !is.na(PERSONAS$REPORTA_ENFERMEDAD)) %>%
        ggplot(aes(REPORTA_ENFERMEDAD,fill=SEXO))+
        theme_bw()+
        geom_bar(position = "dodge")+
        facet_wrap(~GRUPO_EDAD)+
        xlab(label = "¿Reporta enfermedad?")+
        ylab(label = "Empleados municipales")+
        ggtitle(label = "Reporte de Enfermedad por Grupo de Edad")

#Reporta enfermedad
PERSONAS %>%
        filter(!is.na(PERSONAS$SEXO)&!is.na(PERSONAS$GRUPO_EDAD)&
                       !is.na(PERSONAS$REPORTA_ENFERMEDAD)) %>%
        ggplot(aes(REPORTA_ENFERMEDAD,fill=SEXO))+
        theme_bw()+
        geom_bar()+
        facet_wrap(~GRUPO_EDAD)+
        xlab(label = "¿Reporta enfermedad?")+
        ylab(label = "Empleados municipales")+
        ggtitle(label = "Reporte de Enfermedad por Grupo de Edad")

#Por grupo de edad y estado nutricional
PERSONAS %>%
        filter(!is.na(PERSONAS$SEXO)&!is.na(PERSONAS$GRUPO_EDAD)&
                       !is.na(PERSONAS$REPORTA_ENFERMEDAD)&!is.na(PERSONAS$ESTADO_NUTRI)&
                       !is.na(PERSONAS$FUMA)&!is.na(PERSONAS$ACTIVIDAD_FISICA)) %>%
        ggplot(aes(GRUPO_EDAD,ESTADO_NUTRI,color=REPORTA_ENFERMEDAD))+
        theme_bw()+
        geom_jitter()+
        xlab("Grupo de edad")+
        ylab("Estado nutricional")+
        ggtitle("Empleados por Grupo de Edad y Estado Nutricional")+
        guides(color=guide_legend(title="¿Reporta enfermedad?"))

#Por sexo y si reporta enfermedad
PERSONAS %>%
        filter(!is.na(SEXO)&!is.na(REPORTA_ENFERMEDAD)) %>%
        ggplot(aes(SEXO,fill=REPORTA_ENFERMEDAD))+
        theme_bw()+
        geom_bar()+
        guides(fill=guide_legend(title="¿Reporta enfermedad?"))+
        xlab("Sexo")+
        ylab("Cantidad de empleados")+
        ggtitle("Empleados por Sexo y Reporte de Enfermedad")

#Por sexo y si reporta enfermedad en porcentaje
PERSONAS %>%
        filter(!is.na(SEXO)&!is.na(REPORTA_ENFERMEDAD)) %>%
        ggplot(aes(SEXO,fill=REPORTA_ENFERMEDAD))+
        theme_bw()+
        geom_bar(position = "fill")+
        guides(fill=guide_legend(title="¿Reporta enfermedad?"))+
        xlab("Sexo")+
        ylab("Porcentaje (%)")+
        ggtitle("Proporción(%) Empleados por Sexo y Reporte de Enfermedad")

#Grupo de edad y reporte de enfermedad
PERSONAS %>%
        filter(!is.na(GRUPO_EDAD)&!is.na(REPORTA_ENFERMEDAD)) %>%
        ggplot(aes(GRUPO_EDAD,fill=REPORTA_ENFERMEDAD))+
        theme_bw()+
        geom_bar(position = "fill")+
        guides(fill=guide_legend(title="¿Reporta enfermedad?"))+
        xlab("Grupo de edad")+
        ylab("Porcentaje (%)")+
        ggtitle("Proporción(%) Empleados por Grupo de Edad y Reporte de Enfermedad")

#Actividad fisica y reporte de enfermedad proporción
ggplot(PERSONAS,
       aes(ACTIVIDAD_FISICA,fill=REPORTA_ENFERMEDAD))+
        theme_bw()+
        geom_bar()+
        guides(fill=guide_legend(title="¿Reporta enfermedad?"))+
        xlab("Actividad física")+
        ylab("Empleados")+
        ggtitle("Empleados por Actividad Física y Reporte de Enfermedad")

#Actividad fisica y reporte de enfermedad proporción
ggplot(PERSONAS,
       aes(ACTIVIDAD_FISICA,fill=REPORTA_ENFERMEDAD))+
        theme_bw()+
        geom_bar(position = "fill")+
        guides(fill=guide_legend(title="¿Reporta enfermedad?"))+
        xlab("Actividad física")+
        ylab("Porcentaje (%)")+
        ggtitle("Proporción(%) Empleados por Actividad Física y Reporte de Enfermedad")

        
#-------------------------------------TABLAS----------------------------------------------#
#Tabla de personas que reportan enfermedades por sexo y grupo de edad
x <- PERSONAS %>%
        filter(!is.na(PERSONAS$SEXO)&!is.na(PERSONAS$GRUPO_EDAD)&
                       !is.na(PERSONAS$REPORTA_ENFERMEDAD)) %>%
        group_by(SEXO,GRUPO_EDAD,REPORTA_ENFERMEDAD) %>%
        summarise(Personas=n(),Proporcion=Personas/5964)
write.csv(x,
      file = "Tabla de personas que reportan enfermedades por sexo y grupo de edad")
rm(x)
#Tabla y analisis de chisquared de relacion sexo y enfermedad
chisq.test(table(PERSONAS$SEXO,PERSONAS$REPORTA_ENFERMEDAD))
summary(chisq.test(table(PERSONAS$SEXO,PERSONAS$REPORTA_ENFERMEDAD)))

#Grupo de edad y padecimientos de las personas
x <- PERSONAS %>%
        filter(!is.na(GRUPO_EDAD)&!is.na(REPORTA_ENFERMEDAD)) %>%
        group_by(GRUPO_EDAD,REPORTA_ENFERMEDAD) %>%
        summarise(Empleados_totales=n())
write.csv(x,
          file = "Tabla de proporcion de empleados por grupo edad y reporte de enfermedad")
chisq.test(table(PERSONAS$GRUPO_EDAD,PERSONAS$REPORTA_ENFERMEDAD))
rm(x)

#Empleados por Actividad Física y Reporte de Enfermedad
table(PERSONAS$ACTIVIDAD_FISICA,PERSONAS$REPORTA_ENFERMEDAD)
chisq.test(table(PERSONAS$ACTIVIDAD_FISICA,PERSONAS$REPORTA_ENFERMEDAD))

#Hombres y mujeres
table(PERSONAS$SEXO, useNA = "ifany")

#Grupos de edad
table(PERSONAS$GRUPO_EDAD, PERSONAS$SEXO, useNA = "ifany")

#-------------------------------------REGRESIONES-----------------------------------------#
lm(PERSONAS$IMC~PERSONAS$ALIMENTACION_BALANCEADA)
summary(lm(PERSONAS$IMC~PERSONAS$ALIMENTACION_BALANCEADA))

lm(PERSONAS$IMC~PERSONAS$SEXO)
summary(lm(PERSONAS$IMC~PERSONAS$SEXO))

lm(PERSONAS$IMC~PERSONAS$SEXO+PERSONAS$FRECUENCIA_AF)
summary(lm(PERSONAS$IMC~PERSONAS$SEXO+PERSONAS$FRECUENCIA_AF))

lm(PERSONAS$IMC~PERSONAS$FRECUENCIA_AF+PERSONAS$ALIMENTACION_BALANCEADA)
summary(lm(PERSONAS$IMC~PERSONAS$FRECUENCIA_AF+PERSONAS$ALIMENTACION_BALANCEADA))

lm(PERSONAS$IMC~PERSONAS$ACTIVIDAD_FISICA)
summary(lm(PERSONAS$IMC~PERSONAS$ACTIVIDAD_FISICA))

lm(PERSONAS$IMC~PERSONAS$ALIMENTACION_BALANCEADA+PERSONAS$FRECUENCIA_AF+PERSONAS$FUMA+
           PERSONAS$ALCOHOL)
summary(lm(PERSONAS$IMC~PERSONAS$ALIMENTACION_BALANCEADA+PERSONAS$FRECUENCIA_AF+
                   PERSONAS$FUMA+PERSONAS$ALCOHOL))
