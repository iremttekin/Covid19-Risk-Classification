library(cluster)
library(factoextra)
library(caret)
library(tidyverse)
library(NbClust)
library(mice)
mydata<-covid19-risk-data
View(mydata)
View(mydata[is.na(mydata)])
mydata[is.na(mydata$testing_rate),"testing_rate"]<-mean(mydata$testing_rate, na.rm=TRUE)
mydata[is.na(mydata$vaccination_rate),"vaccination_rate"]<-mean(mydata$vaccination_rate, na.rm=TRUE)
mydata[is.na(mydata$goverment_response_index),"goverment_response_index"]<-mean(mydata$goverment_response_index, na.rm=TRUE)
mydata[is.na(mydata$hospital_bed_capacity),"hospital_bed_capacity"]<-mean(mydata$hospital_bed_capacity, na.rm=TRUE)
mydata[is.na(mydata$face_mask_wearing_rate),"face_mask_wearing_rate"]<-mean(mydata$face_mask_wearing_rate, na.rm=TRUE)
mydata[is.na(mydata$health_worker_rate),"health_worker_rate"]<-mean(mydata$health_worker_rate, na.rm=TRUE)
View(mydata)

Data_selected<-mydata[,2:10]
View(Data_selected)

scaledData<-preProcess(Data_selected,method = c("center", "scale"))
scaledData_predicted<-predict(scaledData,Data_selected)

fviz_nbclust(scaledData_predicted,kmeans, method ="wss")
fviz_nbclust(scaledData_predicted,kmeans, method ="silhouette")
fviz_nbclust(scaledData_predicted,kmeans, method ="gap_stat")

nb<-NbClust(scaledData_predicted,distance = "euclidean", min.nc = 2,max.nc = 10,method = "centroid")

cluster1<-kmeans(scaledData_predicted, centers =2 , iter.max = 10,nstart = 10)
cluster1

cluster1$withinss
cluster1$tot.withinss
scaledData_predicted$cluster<-cluster1$cluster
View(mydata)
View(scaledData_predicted)
fviz_cluster(cluster1, data = scaledData_predicted)
boxplot(testing_rate ~ cluster, data=scaledData_predicted)
View(scaledData_predicted)

countries<-mydata$countries
clusters<-scaledData_predicted$cluster
df<-data.frame(countries,clusters)
view(df)
