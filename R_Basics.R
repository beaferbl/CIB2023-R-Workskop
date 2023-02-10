########################### Exploramos RStudio  ################################
#### R COMO CALCULADORA ####
# Operadores matemáticos: Operaciones basicas 

3+2
3-2
3*2
3/2
3**2

#### Operadores lógicos: Devuelve TRUE/FALSE ####
# IMPORTANTE ESCRIBIR DOS VECES "="
1==2 ## 1 es igual a 2?
1!=2 ## 1 es diferente de 2?
1<0 ## 1 es menor que 0?
1>0 ## 1 es mayor que 0?

#### Variables ####
a=2 # Los valores se pueden asignar con un =
b<-3 # O con <- (forma preferida por convención)
b < -3  # OJO! Los espacios cuentan
c <- "Hola"
e <- "Adios"

#### Clases de objetos ####
c="4"
class(a)
class(b)
class(c)

#### Operaciones sencillas ####
d<-a+b
sum(a,b) # Funcion suma
sum(a,b,6)
class(sum)

d<-a+c # No permite sumar nº y letras
d<-c+e # No permite sumar caracteres
d<-paste(c,e, sep= "-") # Podemos concatenar variables de caracteres

# En el ejemplo anterior, imaginad que queremos calcular la media
# de las variables a y b
media_valores<-(a+b)/2
mean(a,b)

#### Vectores ####
# Se definen con c() y separando los elementos con una coma
numeros <- c(1,2,3,4,5)

saludos<-c("Hola","Buenos días", "Buenas tardes")
length(saludos) # Obtener el número de elementos de un vector
saludos[2] # Acceder a los elementos de un vector
saludos<-saludos[c(3,1,2)] # Reordenar un vector y sobreescribirlo

#### Conjuntos de datos ####
meses_dias<-data.frame(meses=c("enero","febrero","marzo","abril","mayo","junio",
                               "julio","agosto","septiembre","octubre",
                               "noviembre","diciembre"),
                       dias=c(31,28,31,30,31,30,31,31,30,31,30,31))

#### Funciones ####
log(10)
sum(a,b)
help(sum)

sum(meses_dias$dias) # Sumatorio
mean(meses_dias$dias) # Media
median(meses_dias$dias) # Mediana
sd(meses_dias$dias) # Desviación estandar

#### Importancia de conocer la clase de objeto ####
c="4"
# a+c # Esto nos da error

##################### Análisis de un conjunto de datos #########################
# Descargamos los datos de cbioportal y los importamos a Posit Cloud

#### IMPORTAR CONJUNTOS DE DATOS A PARTIR DE FICHEROS ####
library(readr)
library(readxl)
library(openxlsx)
install.packages("readxl")

# Dos opciones igual de válidas:
tmb<-read.delim("tmb_mskcc_2018_clinical_data.tsv")
# Con read.table obligatorio poner el separador
tmb<-read.table("tmb_mskcc_2018_clinical_data.tsv",header=TRUE,sep="\t")
# ¿Y si fuera un excel? readxl: read_excel(),read_xls(),read_xlsx()


#### Exploración del conjunto de datos ####
class(tmb) # vemos que R lo reconoce como data.frame
str(tmb) # estructura del conjunto de datos
tmb$Cancer.Type # acceder a las variables
class(tmb$tmb$Cancer.Type)
dim(tmb) # Número de filas y columnas
colnames(tmb) # Nombres de las columnas
rownames(tmb) # Nombres de las filas

#### Filtrar una variable [] ####
# Supongamos que me interesa conocer cuál ha sido la media de supervivencia de los pacientes vivos y los exitus.

mean(tmb$Overall.Survival..Months.)
mean(tmb$Overall.Survival..Months.[tmb$Overall.Survival.Status=="1:DECEASED"])
mean(tmb$Overall.Survival..Months.[tmb$Overall.Survival.Status=="0:LIVING"])


#### Función summary() ####
summary(tmb$Overall.Survival..Months.)
summary(tmb$Overall.Survival..Months.[tmb$Overall.Survival.Status=="1:DECEASED"])
summary(tmb$Overall.Survival..Months.[tmb$Overall.Survival.Status=="0:LIVING"])

#### Función range ####

#### Gráficos de frecuencia: funciones table() y barplot() ####
# Sexo
sex.freq<-table(tmb$Sex)
barplot(sex.freq) # Lo más sencillo
sex.bar<-barplot(sex.freq,
                 col=c("blue","red"),
                 xlab="Sexo",
                 ylab="Número de pacientes",
                 ylim = c(0,max(sex.freq)+200)
                 ) # Un poco más bonito
text(x=sex.bar, y=sex.freq+20,
     labels=paste0(round(sex.freq/sum(sex.freq)*100),"%") ,cex=1)

# Edad
age.freq<-table(tmb$Age.Group.at.Diagnosis.in.Years)[c(1,3,4,5,2)]
age.bar<-barplot(age.freq,
                 col=brewer.pal(5, "Set3"),
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)
text(age.bar, age.freq+20, paste0(round(age.freq/sum(age.freq)*100), "%") ,cex=1)

##### PALETAS DE COLORES ####
# Tambien acepta sistema HEX and RGB
age.bar<-barplot(age.freq,
                 col=c("#FF99FF","#0066FF","#00FF4D","#FF9900", "#AA4371"), # 1 color por grupo, 5 grupos
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)

## Colores predeterminados
heat.colors
terrain.colors
topo.colors

age.bar<-barplot(age.freq,
                 col=heat.colors(5), ## Indicamos el nº de colores que necesitamos
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)

## Paletas predefinidas
install.packages("RColorBrewer")
library(RColorBrewer)

# Otras opciones de colores dentro de BrewerColors:
"Set1"
"Set2"
"Paired"
"Dark2"
"RdYlBu"
"BrBG"

display.brewer.all(colorblindFriendly = TRUE)

# Edad
age.freq<-table(tmb$Age.Group.at.Diagnosis.in.Years)[c(1,3,4,5,2)]
age.bar<-barplot(age.freq,
                 col=brewer.pal(5, "Set3"),
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)
text(age.bar, age.freq+20, paste0(round(age.freq/sum(age.freq)*100), "%") ,cex=1)

#### Gráfico de cajas y bigotes: función boxplot() #####
boxplot(tmb$TMB..nonsynonymous.~tmb$Overall.Survival.Status)


#### Gráfico de cajas y bigotes: solución con ggplot2 ####
# Este apartado nos sirve para introducir el paquete ggplot2, que nos ofrece muchas posibilidades para crear gráficos.

# Esta plantilla te explica de forma resumida las principales funciones de ggplot:
# https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf


# Boxplot: geom_boxplot
# Barplot: geom_bar
# Dotplot: geom_point

# 1) Instalar la libreria  y cargarla
install.packages("ggplot2")
library(ggplot2)
# Google: ggplot cheat sheet (https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf)
## GGPLOT2: BOXPLOT
ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.))+
  geom_boxplot()

ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.))+
  geom_boxplot() +
  labs(y="Supervivencia global (meses)",x=" ") +
  theme(text=element_text(size=10))

ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.))+
  geom_boxplot(outlier.shape = NA) +
  labs(y="Supervivencia global (meses)",x=" ") +
  theme(text=element_text(size=10))

ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.),
       fill=factor(Overall.Survival.Status))+
  geom_boxplot()+
  geom_jitter(aes(colour=Overall.Survival.Status))+
  theme(text=element_text(size=10))+
  labs(y="Supervivencia global (meses)",x=" ")

ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.),
       fill=factor(Overall.Survival.Status))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(colour=Overall.Survival.Status))+
  theme(text=element_text(size=10))+
  labs(y="Supervivencia global (meses)",x=" ")
## GGPLOT2: DOTPLOT

## Mostrar los datos filtrados:
ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  theme(text=element_text(size=10))+
  labs(y="Cobertura",x="Pureza Tumoral")

ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  theme(text=element_text(size=10), legend.position = "bottom")+
  labs(y="Cobertura",x="Pureza Tumoral")

ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  theme(text=element_text(size=10), legend.position = "top")+
  labs(y="Cobertura",x="Pureza Tumoral")

cols <- c("#55AD89", "#EF6F6A") ## cambiamos el color
ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  scale_color_manual(values = cols) +
  theme(text=element_text(size=10), legend.position = c(0.1, 0.9))+
  labs(y="Cobertura",x="Pureza Tumoral")

## GGPLOT2: BARPLOT

ggplot(data=tmb,
       aes(x=Cancer.Type, fill=Cancer.Type)) +
  geom_bar() 

# The value of hjust (horizontal) and vjust (vertical) are only defined between 0 and 1:

# 0 means left-justified
# 1 means right-justified

ggplot(data=tmb,
       aes(x=Cancer.Type, fill=Cancer.Type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=tmb,
       aes(x=Cancer.Type, fill=Cancer.Type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 50, hjust=1)) +
  scale_fill_manual(values = topo.colors(11)) 

ggplot(data=tmb,
       aes(x=Cancer.Type, fill=Cancer.Type)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  xlab("Tipo de Cancer") +
  ylab("Nº de Muestras") +
  ggtitle("Grafico de Barras", subtitle= "Taller de R CIB 2023")

## GGSAVE
Formatos: "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
Tamaños: width, height, units	
Plot size in units ("in", "cm", "mm", or "px")
Resolución: dpi  
```{r}
help(ggsave)
ruta_file_name = ""
ggsave(ruta_file_name,
       plot=last_plot(),
       device = "png")

#### Gráficos apilados ####
# Vemos que, excepto en el caso de cáncer de mama, que solo se da en mujeres, y en los tumores primarios de origen desconocido, cáncer colorectal y cáncer de pulmón de célula no pequeña, la mayoría de los casos se dan en hombres, lo que es especialmente notable en el caso del cáncer de vejiga, renal, esofagogástrico y cáncer de piel no melanoma.
sex.freq.tumor<-table(tmb$Sex,tmb$Cancer.Type)
sex.tumor.bar<-barplot(sex.freq.tumor,
                       beside=TRUE,
                       legend=rownames(sex.freq.tumor),
                       col=c("blue","red"),
                       xlab="Sexo por tipo de tumor",
                       ylab="Número de pacientes",
                       ylim = c(0,max(sex.freq.tumor)+20),
                       xaxt="n",
                       cex.names=0.5)
text(sex.tumor.bar, sex.freq.tumor+5 ,
     paste0(apply(sex.freq.tumor,2,function(x){round(x/sum(x)*100)}), "%") ,cex=0.6) 
tumor.labels <- c("Bladder","Breast","Unknown\nprimary","Colorectal",
                  "Esophagogastric","Glioma","Head and Neck","Melanoma",
                  "Non-Small\nCell Lung","Renal","Skin")
text(cex=0.8, x=colMeans(sex.tumor.bar)-.25, y=-20,
     tumor.labels,xpd=TRUE, srt=20)

# En cuanto a la edad, llama la atención que en el cáncer colorectal hay un porcentaje aparentemente mayor de pacientes de 31-50 años en comparación con los otros tipos.

age.freq.tumor<-table(tmb$Age.Group.at.Diagnosis.in.Years,
                      tmb$Cancer.Type)[c(1,3,4,5,2),]
age.tumor.bar<-barplot(age.freq.tumor,
                       beside=TRUE,
                       legend=rownames(age.freq.tumor),
                       col=brewer.pal(5, "Set3"),
                       xlab="Edad al diagnóstico (años)",
                       ylim = c(0,max(age.freq.tumor)+20),
                       xaxt="n",
                       cex.names=0.5)
text(age.tumor.bar, age.freq.tumor+5,
     paste0(apply(age.freq.tumor,2,function(x){round(x/sum(x)*100)}), "%"),
     cex=0.6) 

text(cex=0.8, x=colMeans(age.tumor.bar)-.25, y=-15,
     tumor.labels,
     xpd=TRUE, srt=10)

#### Gráficos de correlación ####
# Nos hemos entretenido mirando cada una de las variables, pero lo importante  de este artículo es ver si hay una relación entre carga mutacional y eficacia del tratamiento. La eficacia la podemos evaluar en términos de supervivencia global. Exploramos si la carga mutacional tiene relación con la supervivencia, ya que esto determinaría si los inhibidores de los puntos de control autoinmunes funcionan mejor en función de la carga mutacional. Sin embargo, vemos que no hay correlación entre estas dos variables, ni en la cohorte en general, ni en ningún tipo de cáncer en concreto, ya que los coeficientes de correlación son muy bajos.
cor(tmb$TMB,tmb$Overall.Survival..Months.)
cor.tmb.cancer<-sapply(unique(tmb$Cancer.Type),function(x){
  cor(tmb$TMB..nonsynonymous.[tmb$Cancer.Type==x],
      tmb$Overall.Survival..Months.[tmb$Cancer.Type==x])
})
plot(tmb$TMB..nonsynonymous.,tmb$Overall.Survival..Months.)
plot(tmb$Age.at.Which.Sequencing.was.Reported..Days.,tmb$TMB..nonsynonymous.)

##################### swirl: aprender R dentro de R ############################
install.packages("swirl")
library(swirl)