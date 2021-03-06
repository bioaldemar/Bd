## BD 
## Read packages



```
library(sp)
library(rgdal)
library(raster)
library(rgdal)
library(dismo)
library(rasterVis)
library(xtable)
```


### Read Data


# Path to working directory
```
path <- "C:/Users/diego.lizcano/Documents/GitHub/Bd_N_Santander/data"

BD1<-readOGR(dsn=path, layer="BD")

###cargando variables ambientales
files <- list.files(path=path, pattern='tif', full.names=TRUE )

# View(files)
# Agrupar las variables ambientales en un objeto stack
predictors <- stack(files)
# names(predictors)
plot(predictors$Bio1)
plot(BD1, pch=18, add=TRUE, col="blue", main="Batrachochytrium dendrobatidis")

levelplot(predictors)
```



#Extraer la informacion de variables ambientales por ptos. con presencia de BD
```
# head(BD)
BD <- BD1[,-1] # quito la primera columna
head(BD)
names(BD)[1]<-paste("x")
names(BD)[2]<-paste("y") # re nombro encabezados de  columnas
# head(BD)

presvals <- (BD)

###presvals <- extract(predictors, BD)

set.seed(2000)
backg <- randomPoints(predictors, n=500, extf = 1.25)


plot(predictors$DEM30)
points(backg, pch=18, col="red")
plot(BD1, pch=18, add=TRUE, col="blue", main="Batrachochytrium dendrobatidis")
```

#generando datos de prueba y entrenamiento con los datos de presencia y pseudo-ausencia con particion 80-20%
```
indexes_pres = sample(1:nrow(presvals), size=0.2*nrow(presvals))
indexes_backg = sample(1:nrow(backg), size=0.2*nrow(backg))
```

###Presencias
#identifica el 20% de los datos como datos prueba/test
```
pres_test = presvals[indexes_pres,]
dim(pres_test)
```

#identifica el 80% de los datos como datos entrenamiento/train
```
pres_train = presvals[-indexes_pres,]
dim(pres_train)
```
# View(pres_train)

###pseudo-ausencia
#identifica el 20% de los datos como datos prueba/test
```
backg_test = backg[indexes_backg,]
dim(backg_test)
```
#identifica el 80% de los datos como datos entrenamiento/train
```
backg_train = backg[-indexes_backg,]
dim(backg_train)
```

# mapeando los datos de entrenamiento y prueba de presencias y backg
```
r = raster(predictors, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE, main="datos de entrenamiento y prueba ")
points(backg_train, pch='-', cex=0.5, col='red')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')
```
# Train points
```
trainpres <- data.frame( extract(predictors, pres_train) )
trainbackg <- data.frame( extract(predictors, backg_train) )
train <- rbind(trainpres, trainbackg)
pb_train <- c(rep(1, nrow(trainpres)), rep(0, nrow(trainbackg)))
envtrain <- data.frame( cbind(pa=pb_train, train) )
```

#### extract from stack
```
testpres <- data.frame( extract(predictors, pres_test) )
testbackg <- data.frame( extract(predictors, backg_test) )
# head(envtrain)
```

#Ajuste del modelo GLM y predicc??n 
# GLM binomial 
```
gm1 <- glm(pa ~ Bio1 + Bio2 + Bio3 + Bio4 + Bio5 + Bio6 + Bio7 + Bio8 + Bio9 + Bio10 + Bio11 + Bio12 + Bio13 + Bio14 + Bio15 + Bio16 + Bio17 + Bio18 + Bio19 + DEM30,family = binomial(link = "logit"), data=envtrain)
summary(gm1)
coef(gm1)

# GLM gausiano 
gm2 <- glm(pa ~ Bio1 + Bio2 + Bio3 + Bio4 + Bio5 + Bio6 + Bio7 + Bio8 + Bio9 + Bio10 + Bio11 + Bio12 + Bio13 + Bio14 + Bio15 + Bio16 + Bio17 + Bio18 + Bio19 + DEM30,family = gaussian(link = "identity"), data=envtrain)
summary(gm2)
coef(gm2)

```

## evaluacion
```
ge1 <- evaluate(testpres, testbackg, gm1)
ge1

ge2 <- evaluate(testpres, testbackg, gm2)
ge2

par(mfrow=c(1,2))
plot(ge1, 'ROC', main="ge1")
plot(ge2, 'ROC', main="ge2")
```

##mapeo del modelo
```
pg <- predict(predictors, gm2, ext=predictors)
par(mfrow=c(1,2))
plot(pg, main='GLM/gaussian, BD')
plot(BD, add=TRUE, border='dark grey')
tr <- threshold(ge2, 'spec_sens')
plot(pg>tr, main='presence/backgence')
plot(BD, add=TRUE, border='dark grey')
points(pres_train, pch='+')

dev.off()
```

# Comentarios
1. En terminos generales el codigo esta bien mis comentarios son mas a los datos y la seleccion del modelo que predice.

2. Creo que es mejor si usa los datos de presencia ausencia reales en lugar de pseudo ausencias al azar. Si tiene y puede recuperar los datos de las coordenadas de los bichos con deteccion negativa es es mucho mejor.

3. En mi opinion es mejor que haga una preseleccion de variables para trabajar con las que no estan correlacionadas. En teoria la altitud y la temperatura lo estan, asi que indicarian lo mismo y al predecir esto introduce ruido de sobreprediccion.

4. Es raro que haga un modelo gausiano (normal) con datos de presencia ausencia. Es mejor uno binomial. En el binomial vale la pena probar con combinaciones cuadraticas y exponenciales de alguna variables ya que no siempre la relacion es lineal.

5. Si el ciriterio de seleccion del mejor modelo es el AUC hay que seleccionar el mayor AUC para predecir.

6. En mi opinion la ventana de prediccion es demasiado grande comparado con la extension de los puntos de presencia. 



