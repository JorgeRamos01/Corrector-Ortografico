rm(list=ls())
#La parte del codigo comentado se puede descomentar y comentar la linea anterior a cada linea descomentada para
#poder hacer uso del otro diccionario de frecuencias de la RAE

library(readr)
library(stringdist)
library(tokenizers)
frecuencias <- read_table2("~/Documentos/Datos complejos/Tareas-20180904/freq_es.txt", col_names = FALSE)
#frecuencias2 <- read_table2("~/Documentos/Datos complejos/Tareas-20180904/10000.txt", locale = locale(encoding = "ISO-8859-1"))
frecuencias[,3]<-frecuencias$X2/sum(frecuencias$X2)
#frecuencias2[,5]<-frecuencias2$Frec.normalizada/sum(frecuencias2$Frec.normalizada)

#Leemos nuestras rese;as de prueba
setwd("~/Documentos/Datos complejos/Tareas-20180904/Prueba1")
filelist = list.files(pattern = ".*.txt")
datalist = lapply(filelist, function(x)read_file(x, locale = locale(encoding = "ISO-8859-1")))

for (i in 1:length(datalist)){ #Limpieza y tokenizado de los textos
  a<-datalist[i][[1]]
  b<-gsub('[[:punct:] ]',' ',a) #Removemos signos de puntuacion
  b<-gsub("[0-9]+", "", b) #Removemos numeros
  c<-unlist(tokenize_words(b))
  for (j in 1:length(c)){
    word<-c[j]
    #Buscamos las palabras en el diccionario con longitud de mas menos 2 de la palabra a corregir y que coincida con la primera
    #leta de la palabra
    temp<-nchar(frecuencias$X1)<=(nchar(word)+2) & nchar(frecuencias$X1)>=(nchar(word)-2) & substr(frecuencias$X1,1,1)==substr(word,1,1)
    #temp<-nchar(frecuencias2$Frec.absoluta)<=(nchar(word)+2) & nchar(frecuencias2$Frec.absoluta)>=(nchar(word)-2) & substr(frecuencias2$Frec.absoluta,1,1)==substr(word,1,1)
    newDict<-frecuencias$X1[temp]
    #newDict<-frecuencias2$Frec.absoluta[temp]
    newFreq<-frecuencias$V3[temp]
    #newFreq<-frecuencias2$V5[temp]
    
    #Generamos las edit-distances
    w <- c(1,1,1) ##insert,delet,subst
    dist <- stringdist(word,newDict,method="lv",
                       weight=w/max(w))

    
    for (k in 1:length(dist)){ #Asignacion de probabilidades correspondientes a la edit-distance
      if (dist[k]==0) {dist[k]=0.90}
      else if (dist[k]==1){dist[k]=0.09}
      else if (dist[k]==2){dist[k]=0.01}
      else {dist[k]=0}
    }
    argMax<-which.max(dist*newFreq) #Calculamos el argumento maximo
    wordNew<-newDict[argMax] #Conseguimos la palabra que usaremos para reemplazar
    a<-sub(word,wordNew,a)
  }
  temp2<-paste0("~/Documentos/Datos complejos/Tareas-20180904/resultados/Corregido",filelist[i])
  fileConn<-file(temp2)
  writeLines(a, fileConn)
  close(fileConn)
}











