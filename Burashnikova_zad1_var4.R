##Устанавливаем нужную директорию 

setwd("C:/121Group/Burashnikova/MathMod/Mathmode")
##Устанавливаем необхождимые пакеты

library(tidyverse)
library(rnoaa)
##Скачиваем и сохраняем на диск список метеостанций

station_data=ghcnd_stations()
write.csv(station_data,"station_data2020.csv")
#После получения списка всех станций, выбираем из него список станций ближайших к столице нашего региона,Тамбов,создав таблицу с именем региона и координатами его столицы

tambov = data.frame(id = "TAMBOV", latitude = 52.73169,longitude = 41.44326)
#Ознакомимся со справкой команды meteo_nearby_stations
 meteo_nearby_stations
#Необходимо выбрать конечное число метеостанций ,которые имеют необходимые данные
meteo_nearby_stations
tambov_around=meteo_nearby_stations(lat_lon_df = tambov,station_data = station_data,limit=10,var=c("PRCP","TAVG"),year_min=2020,year_max=2020)
tambov_around
#вспомним, как работать со списками
#1)очевидно что первым элементом таблицы будет
#идентификатор метеостанции Тамбова, его то мы и попытаемся получить
tambov_id = tambov_around[["TAMBOV"]][["id"]]
summary (tambov_id)
#2)чтобы получить таблицу всех метеостанций вокруг Тамбова нужно выбрать целиком первый объект из списка
tambov_table=tambov_around[[1]]  
summary(tambov_table)
#в таблице tфьищм_table оказалось 10 объектов, ранжированных по расстоянию от Тамбова
tambov_stations=tambov_table
#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит
str(tambov_table)
tambov_stations$id

##Скачивание погодных данных для выбранных метеостанций
#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используем следующую команду
all_tambov_data = meteo_tidy_ghcnd(stationid = tambov_id)
