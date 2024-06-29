library(dplyr)
library(haven)
library(labelled)
library(readxl)
library(lubridate)
library(mice)
library(ExPanDaR)
library(GGally)
library(corrplot)
library(ggplot2)
library(explor)
library(cluster) # medoides
library(Kmedians) # medianas
library(factoextra) # visual
library(flexclust)



##CARGA DE LA DATA
#csv
cart<-read.csv("Trabajo final/_data/CARTERA CREDITOS PERSONAS.csv", sep = ",")


##PREPARACI?N

##CREACI?N DE VARIABLES
cart$FECHA_NACIMIENTO <- as.Date(cart$FECHA_NACIMIENTO, format="%d/%m/%Y")
cart$FECHA_DIAS_COBERTURA <- as.Date(cart$FECHA_DIAS_COBERTURA, format="%d/%m/%Y")
cart$FECHA_VENCIMIENTO <- as.Date(cart$FECHA_VENCIMIENTO, format="%d/%m/%Y")

cart$edad <- as.numeric(floor(interval(cart$FECHA_NACIMIENTO, Sys.Date()) / years(1)))

summary(cart$edad)

cart<-cart %>% mutate(gedad=cut(edad, c(21,38,45,54,81) ))

cart$diascob <- as.numeric(cart$FECHA_VENCIMIENTO - cart$FECHA_DIAS_COBERTURA)

##Selecci?n (columnas/variables) y filtrado (filas/unidades)

BD<-cart %>% select(-SUCURSAL, -CODIGO_AGENCIA, -NUMERO_TRAMITE, - COD_GARANTIA,
                    -NUMERO_POLIZA, -FECHA_DIAS_COBERTURA, -FECHA_VENCIMIENTO,
                    -FECHA_MOVIMIENTO, -TIPO_ABONO,-NUMERO_FACTURA, -NUMERO_AUTORIZACION,
                    -CODIGO_CONTROL, -CODIGO_DOSIFICACION, -MONEDA, -NUMERO_ASIENTO,
                    -NOMBRE, -DES_GARANTIA, -FECHA_NACIMIENTO, -FECHA_DESEMBOLSO,
                    -ESTADO, -NRO_DECLARACION_SALUD, -PERIODO, -DIRECCION_DOMICILIO,
                    -DECLARACION_SALUD, -TIPO_EXCLUSION, -NRO_DOC_ID,-LINEA_CREDITO)

BD$MONEDA_OPERACION<-factor(as.numeric(BD$MONEDA_OPERACION), 1:2, c("BS", "USD"))

BD$MONTO <- as.numeric(gsub(",", "", BD$MONTO))
BD$CODIGO_PERSONA <- as.character(cart$CODIGO_PERSONA)
BD$VALOR_ASEGURADO <- as.numeric(gsub(",", "", BD$VALOR_ASEGURADO))
BD$MONTO_DESEMBOLSADO <- as.numeric(gsub(",", "", BD$MONTO_DESEMBOLSADO))
BD$diascob<-as.numeric(BD$diascob)
BD$TASA_PRIMA_INTERES <- as.factor(cart$TASA_PRIMA_INTERES)
BD$PRODUCTO <- factor(BD$PRODUCTO, levels = c("SEGUROS GENERALES", 
                                                  "SEGURO PERSONAS"))

BD <- BD %>%
  rename(
    OCUPACION = ACT_ECO_OCUPACION,
    cod_pers = CODIGO_PERSONA,
    SUCURSAL = DESC_SUCURSAL,
    AGENCIA = DESCRIPCION_AGENCIA,
    EXT = EXT_DOC_ID,
    MONEDA = MONEDA_OPERACION,
    PRIMA = MONTO,
    CREDITO = MONTO_DESEMBOLSADO,
    NAC = NACIONALIDAD,
    TIPOCRED = OPERACION_CREDITICIA, 
    SEGURO = ?..PRODUCTO,
    TASASEG = TASA_PRIMA_INTERES,
    TIPOID = TIPO_DOC_ID,
    SALDO = VALOR_ASEGURADO,
    diascred = PLAZO_CREDITO.DIAS.
    )


##cantidad de datos 
par(cex.axis = 0.7)
md.pattern(BD, plot = T)
flux(BD)

#listwise: DATOS COMPLETOS
BDO<-na.omit(BD) #SIN VALORES PERDIDOS
md.pattern(BDO, plot = T)
flux(BDO)

BD_mis<-mice::ic(BD) #BD DE VALORES PERDIDOS
md.pattern(BDO)
na.action(BDO) 
naprint(na.action(BDO))

######DATOS ATIPICOS
##################
q1 <- quantile(BDO$MONTO_DESEMBOLSADO, 0.95)
BDO_a <- which(BDO$MONTO_DESEMBOLSADO > q1)

iqr <- IQR(BDO$MONTO_DESEMBOLSADO)
up <- quantile(BDO$MONTO_DESEMBOLSADO, 0.95 )+ 1.5*iqr
BDO_A <- subset(BDO, BDO$MONTO_DESEMBOLSADO < up)


q2 <- quantile(BDO_A$VALOR_ASEGURADO, 0.95)
BDO_a <- which(BDO_A$VALOR_ASEGURADO > q2)

iqr <- IQR(BDO_A$VALOR_ASEGURADO)
up <- quantile(BDO_A$VALOR_ASEGURADO, 0.95)+ 1.5*iqr
BDO_A <- subset(BDO_A, BDO_A$VALOR_ASEGURADO < up)

boxplot(select(BDO_A, where(is.numeric)))
######################

# PAIRWISE DATOS PARCIALES
var(BD[,],na.rm = T)#listwise
var(BD[,],na.rm = T, use ="pairwise.complete.obs" )#pairwise

#EXPLORACION DE DATOS
str(BDO_A)
ExPanD(BDO_A)
ggpairs(BDO_A %>% select(SALDO, PRIMA))
names(BDO_A)

#muestreo
set.seed(1245)
BDO_MUESTRA<-BDO_A %>% sample_n(20000)#MAS
BDO_MUESTRA_10 <- BDO_A %>% sample_n(10000)#MAS
BDO_MUESTRA %>% summarise(mean(VALOR_ASEGURADO))
BDO_A %>% summarise(mean(VALOR_ASEGURADO))
ggplot(BDO_A,aes(VALOR_ASEGURADO))+geom_density()+ggtitle("DATOS COMPLETOS")
ggplot(BDO_MUESTRA,aes(VALOR_ASEGURADO))+geom_density()+ggtitle("Muestra n=20000")

##ANALISIS DE COMPONENTES

mcor <- BDO_A %>% select(where(is.numeric)) %>% cor()
corrplot(mcor)

# Calcular los eigenvalores y eigen vectores
m1<-eigen(mcor)
a1<-m1$vectors %*% diag(m1$values) %*% t(m1$vectors)

sum(m1$values)
sum(diag(mcor))

m<-4
a2<-m1$vectors[,1:m] %*% diag(m1$values)[1:m,1:m] %*% t(m1$vectors[,1:m])
round(a2-mcor,5)

# elegir el n?mero de componentes a retener
m1$values

plot(1:length(m1$values),m1$values, type = "b")
pesoL<-(m1$values/sum(m1$values))*100
pesoL
cumsum(pesoL)# 80% o 90%


############################################
# Comandos ACP
#matriz covarianza
m2<-prcomp(BDO_A %>% select_if(is.numeric), center = F ,scale. = F )
m2$sdev^2
m2$rotation
m2$center
m2$scale
m2$x
acp<-data.frame(m2$x)
cor(acp)
corrplot(cor(acp))
names(acp)
#matriz correlaci?n
m3<-prcomp(BDO_A %>% select_if(is.numeric), scale. = T)#correlacion
summary(m3)
acp<-data.frame(m3$x)
cor(acp)
corrplot(cor(acp))
plot(m3)
abline(h=1,col="red")

#################################
m5<-princomp(BDO_A %>% select_if(is.numeric), cor = T)
summary(m5)
plot(m5)
m5$sdev
m5$loadings
m5$scores
corrplot(cor(m5$scores))
dim(m5$scores)
BDO_c<-BDO_A %>% bind_cols(data.frame(m5$scores))
corrplot(
  cor(BDO_c %>% select_if(is.numeric))
)
# EXPLOR
#dashboard 
explor(m5)

ggplot(BDO_c, aes(Comp.1,Comp.2, col=DESC_SUCURSAL)) + geom_point(alpha=0.5) + geom_vline(xintercept =  0,linetype="dotted") +
  geom_hline(yintercept = 0,linetype="dotted")


###########3variables seleccionadas correlaciones
m6<-princomp(BD_s %>% select_if(is.numeric), cor = T)
summary(m6)
plot(m6)
m6$sdev
m6$loadings
m6$scores
corrplot(cor(m6$scores))
dim(m6$scores)
BDO_c<-BD_s %>% bind_cols(data.frame(m6$scores))
corrplot(
  cor(BDO_c %>% select_if(is.numeric))
)
# EXPLOR
#dashboard 
explor(m6)


#################CLUSTER K MEANS
#########6 variables
BD_num <- BDO_A %>% select_if(is.numeric)
BDOS<-scale(BD_num)
set.seed(1245)#controlar la aleatoriedad
n1<-kmeans(BDOS,3)# comando de origen
fviz_cluster(n1, data=BDOS)

set.seed(1245)#controlar la aleatoriedad
n2<-kmeans(BDOS,2)# comando de origen
gp <- fviz_cluster(n2, data=BDOS, geom= "point") + theme_minimal()
gp

summary(n1)
summary(n2)

#########2 variables
BD_num_2 <- BDO_A %>% select(VALOR_ASEGURADO,MONTO)
BDOS<-scale(BD_num_2)
set.seed(1245)#controlar la aleatoriedad
n1<-kmeans(BDOS,3)# comando de origen
fviz_cluster(n1, data=BDOS, geom= "point") + theme_minimal()

set.seed(1245)#controlar la aleatoriedad
n2<-kmeans(BDOS,2)# comando de origen
fviz_cluster(n2, data=BDOS, geom= "point") + theme_minimal()





#####################
##### Muestra
BD_num <- BDO_MUESTRA_10%>% select_if(is.numeric)
BDOS<-scale(BD_num)
set.seed(1245)#controlar la aleatoriedad
n2<-kmeans(BDOS,2)# comando de origen
gm <- fviz_cluster(n2, data=BDOS, geom= "point") + theme_minimal() 
gm

set.seed(1245)#controlar la aleatoriedad
n1<-kmeans(BDOS,3)# comando de origen
fviz_cluster(n1, data=BDOS)

####visualización 
gp ####### poblacion
gm ##### muestra


#####cluster con variables mixtas
###Informe avanzado- borrador
library(cluster)
library(vegan)

BD_p <- BDO_MUESTRA %>%select(PRODUCTO,
                              VALOR_ASEGURADO,
                              MONTO,
                              MONTO_DESEMBOLSADO,
                              PLAZO_CREDITO.DIAS.,
                              edad,
                              diascob)
#BD_p$PRODUCTO <- factor(BD_p$PRODUCTO, levels = c("SEGUROS GENERALES", "SEGURO PERSONAS"))
BD_p$PRODUCTO <- as.numeric(BD_p$PRODUCTO)


#dd<-daisy(BD_p, metric = "gower") ##########mucho tiempo 20 min
#as.matrix(dd) #########mucho tiempo
#
library(vegan)
###vv <- vegdist(BD_p, method="gower")###########3mucho tiempo 20min



