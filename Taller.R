


library(readr)
###extrar la data_clasificada
data <- read_csv("data_clasificada.csv")
View(data)

View(novenoIntento)
library(readxl)
library("tidyverse")
library("cluster")
library("factoextra")
library("NbClust")
library("vegan")
library("textshape")
library("tibble")
library("tidyverse")
library(Rilostat)

#pasamos a dataframe y le damos un alias
df<-as.data.frame(data)
df<-as.data.frame(dataset_ocupaciones)


#Eliminamos las columnas que no necesitamos 
df<- subset(df,select = -c(Column1))
view(df)

#Eliminamos los elementos repetidos
df<-df[!duplicated(df$OCUPACIONES),]

#pasamos las columnas a ocupaciones para que sean el nombre de las variables
df<-textshape::column_to_rownames(df,loc = 1)

#probamos la concordancia de los datos, cuanta relacion tienen 
m.distancia <- get_dist(df, method = "euclidian")
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))


#probamos los diferentes metodos para calculas el K
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat")

k3 <- kmeans(df, centers = 2, nstart = 25)
k3
#clasifica las ocupaciones en clusteres (consola)
#str(k3)


#graficamos los kmeans
fviz_cluster(k3, data = df)
fviz_cluster(k3, data = df, ellipse.type = "euclid",repel = FALSE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k3, data = df, ellipse.type = "norm")
fviz_cluster(k3, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

#jer�rquico con 2 clusters
res2 <- hcut(df, k = 2, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))

#con 3 clusters
res4 <- hcut(df, k = 3, stand = TRUE)
fviz_dend(res4, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green"))


#a�adimos el  numero de cluster al la tabla dataframe
df$clus<-as.factor(k3$cluster)
df

