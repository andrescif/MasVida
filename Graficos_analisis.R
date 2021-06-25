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





