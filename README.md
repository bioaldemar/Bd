# Bd

library(sp)
library(rgdal)
library(raster)
library(rgdal)
library(dismo)


BD<-readOGR(dsn=".", layer="BD")



###cargando variables ambientales

files <- list.files(path=".", pattern='tif', full.names=TRUE )

View(files)


# Agrupar las variables ambientales en un objeto stack
predictors <- stack(files)
names(predictors)
plot(predictors$Bio1)
plot(BD, pch=18, add=TRUE, col="blue", main="Batrachochytrium dendrobatidis")

plot(predictors)

#Extraer la informacion de variables ambientales por ptos. con presencia de BD

head(BD)
BD <- BD[,-1] # quito la primera columna
head(BD)
names(BD)[1]<-paste("x")
names(BD)[2]<-paste("y") # re nombro encabezados de  columnas
head(BD)

presvals <- (BD)

###presvals <- extract(predictors, BD)

set.seed(2000)
backg <- randomPoints(predictors, n=500, extf = 1.25)


#generando datos de prueba y entrenamiento con los datos de presencia y pseudo-ausencia con particion 80-20%
indexes_pres = sample(1:nrow(presvals), size=0.2*nrow(presvals))
indexes_backg = sample(1:nrow(backg), size=0.2*nrow(backg))

###Presencias
#identifica el 20% de los datos como datos prueba/test
pres_test = presvals[indexes_pres,]
dim(pres_test)
#identifica el 80% de los datos como datos entrenamiento/train
pres_train = presvals[-indexes_pres,]
dim(pres_train)

View(pres_train)


###pseudo-ausencia
#identifica el 20% de los datos como datos prueba/test
backg_test = backg[indexes_backg,]
dim(backg_test)
#identifica el 80% de los datos como datos entrenamiento/train
backg_train = backg[-indexes_backg,]
dim(backg_train)


# mapeando los datos de entrenamiento y prueba de presencias y backg
r = raster(predictors, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE, main="datos de entrenamiento y prueba ")
points(backg_train, pch='-', cex=0.5, col='red')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')


trainpres <- data.frame( extract(predictors, pres_train) )
trainbackg <- data.frame( extract(predictors, backg_train) )
train <- rbind(trainpres, trainbackg)
pb_train <- c(rep(1, nrow(trainpres)), rep(0, nrow(trainbackg)))
envtrain <- data.frame( cbind(pa=pb_train, train) )

testpres <- data.frame( extract(predictors, pres_test) )
testbackg <- data.frame( extract(predictors, backg_test) )


head(envtrain)


#Ajuste del modelo GLM y predicc??n 
# GLM binomial 
gm1 <- glm(pa ~ Bio1 + Bio2 + Bio3 + Bio4 + Bio5 + Bio6 + Bio7 + Bio8 + Bio9 + Bio10 + Bio11 + Bio12 + Bio13 + Bio14 + Bio15 + Bio16 + Bio17 + Bio18 + Bio19 + DEM30,family = binomial(link = "logit"), data=envtrain)
summary(gm1)
coef(gm1)

# GLM gausiano 
gm2 <- glm(pa ~ Bio1 + Bio2 + Bio3 + Bio4 + Bio5 + Bio6 + Bio7 + Bio8 + Bio9 + Bio10 + Bio11 + Bio12 + Bio13 + Bio14 + Bio15 + Bio16 + Bio17 + Bio18 + Bio19 + DEM30,family = gaussian(link = "identity"), data=envtrain)
summary(gm2)
coef(gm2)

## evaluacion
ge1 <- evaluate(testpres, testbackg, gm1)
ge1

ge2 <- evaluate(testpres, testbackg, gm2)
ge2

##mapeo del modelo
pg <- predict(predictors, gm2, ext=predictors)
par(mfrow=c(1,2))
plot(pg, main='GLM/gaussian, BD')
plot(BD, add=TRUE, border='dark grey')
tr <- threshold(ge2, 'spec_sens')
plot(pg>tr, main='presence/backgence')
plot(BD, add=TRUE, border='dark grey')
points(pres_train, pch='+')
