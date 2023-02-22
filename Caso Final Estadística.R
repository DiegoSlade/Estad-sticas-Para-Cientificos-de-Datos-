# UNIVERSIDAD HEMISFERIOS IMF SMART EDUCATION
#````MAESTRIA SISTEMAS DE INFORMACION MENCION EN DATA SCIENCE´´´´

#TIULO . Caso Práctico Estadistica para el Científco de DATOSi
 
# Estudiantes: Digner Jimenez, Pamela Yugsi, Diego Ureta


#Instalaci?n de paquetes y librer?as
rm(list)

install.packages("summarytools")
install.packages("rlang")
install.packages("ggplot2")
install.packages ("tidyverse")
install.packages ("gmodels")
install.packages ("ggmosaic")
install.packages ("corrplot")
install.packages ("gmodels")
install.packages ("dplyr")
install.packages ("SamplingUtil")
install.packages ("magrittr")
install.packages ("carData")
install.packages ("MASS")
install.packages ("ISLR")
install.packages ("car")
install.packages ("openintro")
install.packages("PerformanceAnalytics")
install.packages ("nortest")
install.packages ("doBY")




#Cargamos las Librerias
library(dplyr)
library(tidyr) 
library(readxl)
library (summarytools)
library(rlang)
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(gmodels)
library(ggmosaic)
library(corrplot)
library(PerformanceAnalytics)
library(datasets)
library(ggthemes)
library(extrafont)
library(grid)
library(RColorBrewer)
library(nortest)
library(car)
library(doBy)
library(plyr)
file.choose()


#Creacion de la base de datos de trabajo


datos <- read_excel("C:\\Users\\yea\\Documents\\estadistica\\BaseAAA.xlsx")
datos
View(datos)

dim (datos)# Dimensiones de la base de datos 


#Exploración para Limpieza del nuevo dataframe (datos)

glimpse(datos)
sum(is.na(datos))
apply(is.na(datos), 2, sum)
names(datos) #Conocer el nombre de las variables
datos$Estrato_Bosque # Conocer el nombre de los elementos de la variable  
str(datos) # Estructura de datos
summary(datos)


#transformación de variables categoricas
datos$Estrato_Bosque <- as.factor(datos$Estrato_Bosque)
datos$`Conglomerado actual` <- as.factor(datos$`Conglomerado actual`)
datos$Parcela <-as.factor(datos$Parcela)
datos$Especie<-as.factor(datos$Especie)
datos$Vivo<-as.factor(datos$Vivo)
datos$estado_arbol<-as.factor(datos$estado_arbol)

summary(datos)

#***********Tablas de frecuencia*************

#Tabla de frecuencia de dos vias

Estratos <-as.data.frame(table(Estrato_bosque=datos$Estrato_Bosque))
Especie<-as.data.frame(table(Especie=datos$Especie))
Estado <- as.data.frame(table(estado=datos$estado_arbol))

EstratoEStado <-as.data.frame(table(Estrato_bosque=datos$Estrato_Bosque,
                                    estado=datos$estado_arbol))

# Muestra la tabla de frecuencia completa redondeando las frecuencias relativas a 3 decimales.
TablaEstrato <-transform(Estratos,
          FreqAc=cumsum(Freq),
          Rel= round(prop.table(Freq), 2),
          RelAc= round(cumsum(prop.table(Freq)), 2))

TabEstadoEstrato<- transform(EstratoEStado,
          FreqAc=cumsum(Freq),
          Rel= round(prop.table(Freq), 2),
          RelAc= round(cumsum(prop.table(Freq)), 2))


### Exportamos las tablas creadas

write.csv ( x=TablaEstrato, file="Estratos.Csv",row.names=F)
write.csv ( x=Especie, file="Especie.Csv",row.names=F)
write.csv ( x=TabEstadoEstrato, file="Estado arbol.Csv",row.names=F)


# otra forma de Agrupa los valores de la Variabes Estado

EstratosVM<-as.data.frame(table(Estrato1=datos$Estrato_Bosque,Vivo=datos$Vivo))

# Muestra la tabla de frencuencia con datos agrupados, redondeando a 3 decimales las frecuencias relativas
Estratovm1<-transform(EstratosVM,
          FreqAc=cumsum(Freq),
          Rel= round(prop.table(Freq), 3),
          RelAc= round(cumsum(prop.table(Freq)), 3))

#Creacion de tabla de frecuencia Unidimensional

Tablacontingecia<-table(datos$Estrato_Bosque,datos$estado_arbol)
view(Tablacontingecia)
addmargins(Tablacontingecia)#Sumatoria


pruebat2<-as.data.frame(Tablacontingecia)
tablaFR<-prop.table(Tablacontingecia)
prop.table(Tablacontingecia)
tablaFR<-prop.table(Tablacontingecia)
view(tablaFR)
addmargins(tablaFR)


croostab<-ctable (datos$Estrato_Bosque,datos$estado_arbol)


#Exploracion de Datos/ correlaciones

cor(datos$Biomasa,datos$Diametro, use="everything",
    method=c("pearson"))
pairs(datos$Biomasa ~ datos$Diametro)

cor(datos$Biomasa,datos$Dendiam, use="everything",
    method=c("pearson"))
pairs(datos$Biomasa ~ datos$Dendiam)

cor(datos$Biomasa,datos$Densidad, use="everything",
    method=c("pearson"))
pairs(datos$Biomasa ~ datos$Densidad)

DatosCor<-select(datos,Biomasa,Diametro,Densidad,AreaCorregida,Altura_total,Altura_Comercial,Dendiam)
Cortotal<-cor(DatosCor)
Cortotal

corrplot(Cortotal)
chart.Correlation(DatosCor)

BiomasaDiametro <- data.frame(datos$Biomasa,datos$Diametro)
BiomasaDiametro 
chart.Correlation(BiomasaDiametro)

BiomasHT <- data.frame(datos$Biomasa,datos$Altura_total)
BiomasHT 
chart.Correlation(BiomasHT)

#Construcción de gráficas 

barplot(table(datos$Estrato_Bosque), main = "Estratos de bosque",
        xlab = "ESTRATOS", 
        ylab = "numero_individuos", 
        col = "lightgreen", 
        border = "darkgreen")

ggplot(datos,aes(Estrato_Bosque,fill=Vivo))+geom_bar(position="dodge")

ggplot() + geom_point(datos, mapping=aes(x=Biomasa, y = Diametro))

ggplot(data=datos, aes(x=Biomasa,y=Diametro, fill=Estrato_Bosque,color=Estrato_Bosque,))+geom_point()
ggplot(data=datos, aes(x=Biomasa,y=Diametro, fill=Vivo,color=Vivo,))+geom_point()

boxplot(datos$Biomasa~datos$Estrato_Bosque,col=datos$Biomasa,main="Biomasa_Estrato")+ 


  ## Exploración de distribution de datos (pruebas de Normalidad)
  ## Histogramas con curvas de normalidad
  
  NormBiomasa <- ggplot(datos, aes(x=Biomasa,fill=Estrato_Bosque))+
  geom_histogram(aes(y=..density..))+
  facet_wrap('~Estrato_Bosque',ncol=2)+
  stat_function(fun = dnorm,colour="red",
                args=list(mean=mean(datos$Biomasa,na.rm=TRUE),
                          sd=sd(datos$Biomasa,na.rm=TRUE)))
  NormBiomasa
  
  qqnorm(datos$Biomasa)
  qqline(datos$Biomasa)
  
  NormDiametro <- ggplot(datos, aes(x=Diametro,fill=Estrato_Bosque))+
    geom_histogram(aes(y=..density..))+
    facet_wrap('~Estrato_Bosque',ncol=2)+
    stat_function(fun = dnorm,colour="red",
                  args=list(mean=mean(datos$Diametro,na.rm=TRUE),
                            sd=sd(datos$Diametro,na.rm=TRUE)))
  NormDiametro
  
  qqnorm(datos$Diametro)
  qqline(datos$Diametro)
  
  NormDensidad <- ggplot(datos, aes(x=Densidad,fill=Estrato_Bosque))+
    geom_histogram(aes(y=..density..))+
    facet_wrap('~Estrato_Bosque',ncol=2)+
    stat_function(fun = dnorm,colour="red",
                  args=list(mean=mean(datos$Densidad,na.rm=TRUE),
                            sd=sd(datos$Densidad,na.rm=TRUE)))
  NormDensidad
  qqnorm(datos$Densidad)
  qqline(datos$Densidad)
  
  NormAltura <- ggplot(datos, aes(x=Altura_total,fill=Estrato_Bosque))+
    geom_histogram(aes(y=..density..))+
    facet_wrap('~Estrato_Bosque',ncol=2)+
    stat_function(fun = dnorm,colour="red",
                  args=list(mean=mean(datos$Altura_total,na.rm=TRUE),
                            sd=sd(datos$Altura_total,na.rm=TRUE)))
  NormAltura
  qqnorm(datos$Altura_total)
  qqline(datos$Altura_total)
  
  
  #Dividimos la Biomasa en Estratos
  Biomas <- split(datos$Biomasa, datos$Estrato_Bosque)
  Biomas # Para ver los elementos de la lista Estratos
  
  
  plot(density(Biomas$`Andino de Ceja Andina`), lwd=3, col='blue',
       xlim=c(0, 25), main='', las=1,
       xlab='Peso (kg)', ylab='Densidad')
  
  lines(density(Biomas$`Andino Montano`), lwd=1, col='deeppink')
  lines(density(Biomas$`Seco Andino`), lwd=1, col='red')
  lines(density(Biomas$`Seco Pluvioestacional`), lwd=1, col ='green' )
  legend('topright', legend=c('Andino Ceja Andina' ,'Andino Montano','Seco Andino','Seco Puvioestacional' ),
         lwd=3, col=c('blue', 'deeppink', 'red' , 'green' ), bty='n')
  
  
  ### test de Normalidad
  
  lillie.test(datos$Biomasa)
 shapiro.test(datos$Biomasa)
 
 lillie.test(datos$Diametro)
 shapiro.test(datos$Diametro)
  
 lillie.test(datos$Densidad)
 shapiro.test(datos$Densidad)
 
 lillie.test(datos$Altura_total)
 shapiro.test(datos$Altura_total)
 
 ## Exploramos normalidad en la variable de interes a nivel de estrato
 
 lillie.test(Biomas$`Andino de Ceja Andina`)
 lillie.test(Biomas$`Andino Montano`)
 lillie.test(Biomas$`Seco Andino`)
 lillie.test(Biomas$`Seco Pluvioestacional`)
 
 ### Calculo del modelo de regresion Lineal
 
 Diam <- split(datos$Diametro,datos$Estrato_Bosque)
 HT <- split(datos$Altura_total,datos$Estrato_Bosque)
 
 #Biomasa y Diametro
 
 ggplot(data = datos, mapping = aes(x = Diametro, y = Biomasa)) +
   geom_point(color = "firebrick", size = 2) +
   labs(title  =  'Biomasa', x  =  'Diametro') +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))
 cor.test(x = datos$Diametro, y = datos$Biomasa, method = 'spearman')
 
 
 ggplot(data = datos, mapping = aes(x = Diametro, y = Biomasa)) +
   geom_point(color = "firebrick", size = 2) +
   facet_wrap('~Estrato_Bosque',ncol=2)+
   labs(title  =  'Biomasa', x  =  'Diametro') +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))
 cor.test(x = datos$Diametro, y = datos$Biomasa, method = 'spearman')
 cor.test(x = Diam$`Andino de Ceja Andina`, y = Biomas$`Andino de Ceja Andina`, method = 'spearman')
 cor.test(x = Diam$`Andino Montano`, y = Biomas$`Andino Montano`, method = 'spearman')
 cor.test(x = Diam$`Seco Pluvioestacional`, y = Biomas$`Seco Pluvioestacional`, method = 'spearman')
 cor.test(x = Diam$`Seco Andino`, y = Biomas$`Seco Andino`, method = 'spearman')
 
 #Biomasa y Altura total
 ggplot(data = datos, mapping = aes(x = Altura_total, y = Biomasa)) +
   geom_point(color = "firebrick", size = 2) +
   labs(title  =  'Biomasa', x  =  'AlturaTotal') +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 ggplot(data = datos, mapping = aes(x =Altura_total , y = Biomasa)) +
   geom_point(color = "firebrick", size = 2) +
   facet_wrap('~Estrato_Bosque',ncol=2)+
   labs(title  =  'Biomasa/Altura', x  =  'Altura_total') +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))
 cor.test(x = datos$Altura_total, y = datos$Biomasa, method = 'spearman')
 cor.test(x = HT$`Andino de Ceja Andina`, y = Biomas$`Andino de Ceja Andina`, method = 'spearman')
 cor.test(x = HT$`Andino Montano`, y = Biomas$`Andino Montano`, method = 'spearman')
 cor.test(x = HT$`Seco Pluvioestacional`, y = Biomas$`Seco Pluvioestacional`, method = 'spearman')
 cor.test(x = HT$`Seco Andino`, y = Biomas$`Seco Andino`, method = 'spearman')
 
 # DIAMETRO Y ALTURA correlacion y R2
 
 ggplot(data = datos, mapping = aes(x = Altura_total, y = Diametro)) +
   geom_point(color = "firebrick", size = 2) +
   labs(title  =  'Relación Diametro-Altura', x  =  'AlturaTotal') +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 ggplot(data = datos, mapping = aes(x = Altura_total, y = Diametro)) +
   geom_point(color = "firebrick", size = 2) +
   facet_wrap('~Estrato_Bosque',ncol=2)+
   labs(title  =  'Relación Diametro-Altura', x  =  'Altura_total') +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 cor.test(x = datos$Altura_total, y = datos$Diametro, method = 'spearman')
 cor.test(x = HT$`Andino de Ceja Andina`, y = Diam$`Andino de Ceja Andina`, method = 'spearman')
 cor.test(x = HT$`Andino Montano`, y = Diam$`Andino Montano`, method = 'spearman')
 cor.test(x = HT$`Seco Pluvioestacional`, y = Diam$`Seco Pluvioestacional`, method = 'spearman')
 cor.test(x = HT$`Seco Andino`, y = Diam$`Seco Andino`, method = 'spearman')
 
 
 
## Generación de modelos de regresión 
 modelBiomDiam <- lm(Biomasa ~ Diametro, datos)
 summary(modelBiomDiam)
 
 modelBiomHt <- lm(Biomasa~ Altura_total, datos)
 summary(modelBiomHt)
 
 modelBiomDens <- lm(Biomasa~ Densidad, datos)
 summary( modelBiomDens)
 
 modelDiamHT <- lm(Diametro~ Altura_total, datos)
 summary(modelDiamHT)
 
## Grafico de Predichos y Residuos
 
 modelBiomDiam$coefficients
 
 predichosBiomDiam <- modelBiomDiam$fitted.values
 residuosBiomDiam <- modelBiomDiam$residuals
 distanciasreBiomDiam <- cooks.distance(modelBiomDiam)
 
 a <- plot (modelBiomDiam)
 b <- plot(predichosBiomDiam,distanciasreBiomDiam)
 c <- hist( residuosBiomDiam)
 a
 
  ## Analisis de varianza##
#Comprobamos Homocestacidad de varianzas
leveneTest(datos$Biomasa ~ datos$Estrato_Bosque)
leveneTest(datos$Biomasa ~ datos$Estrato_Bosque, data = datos, center = "median")
# Se elige una prueba no paramétrica para la comparación de medias entre grupos dado
#que se rechaza la hipotesis nula de igualdad de varianza (levene test)


#Analisis Kruskall.Wallis (Analisis de Varianza no parametrica)

kruskall<-kruskal.test( datos$Biomasa ~ datos$Estrato_Bosque, data = datos)
plot(kruskall)

#comparacion de medias Mann Whitney
pairwise.wilcox.test(x = datos$Biomasa, g = datos$Estrato_Bosque, p.adjust.method = "holm" )



##Resumenes por Estrato.
summaryBy(Biomasa + Diametro + Densidad + Atura_total ~ Estrato_Bosque, data=datos, FUN=mean)
ddply(datos, "Estrato_Bosque", colwise(mean)) 
