##Бурашникова Екатерина-для региона 68 рассчитайте урожайность пшеницы в 2006 году, взяв для рассчета средние суммы активных температур за текущий год ,
##с 10 ближайших метеостанций но убирая из рассчёта активных температур дни с температурой выше 25 градусов
#Устанавливаем нужную директорию 

setwd("C:/121Group/Burashnikova/MathMod/Mathmode")
##Устанавливаем необхождимые пакеты

library(tidyverse)
library(rnoaa)
##Скачиваем и сохраняем на диск список метеостанций

station_data=ghcnd_stations()
write.csv(station_data,"station_data2020.csv")


#Считывание списка метеостанций с диска
station_data = read.csv("stations_data2020.csv") 
#После получения списка всех станций, выбираем из него список станций ближайших к столице нашего региона,Тамбов,создав таблицу с именем региона и координатами его столицы

tambov = data.frame(id = "TAMBOV", latitude = 52.73169,longitude = 41.44326)
#Ознакомимся со справкой команды meteo_nearby_stations
 meteo_nearby_stations
#Необходимо выбрать конечное число метеостанций ,которые имеют необходимые данные
meteo_nearby_stations
tambov_around=meteo_nearby_stations(lat_lon_df = tambov,station_data = station_data,limit=10,var=c("PRCP","TAVG"),year_min=2006,year_max=2006)
tambov_around
#вспомним, как работать со списками
#1)очевидно что первым элементом таблицы будет
#идентификатор метеостанции Тамбова, его то мы и попытаемся получить

tambov_id = tambov_around[["TAMBOV"]][["id"]] [1]

summary (tambov_id)
#2)чтобы получить таблицу всех метеостанций вокруг Тамбова нужно выбрать целиком первый объект из списка
tambov_table=tambov_around[[1]]  
summary(tambov_table)
#в таблице tambov_table оказалось 10 объектов, ранжированных по расстоянию от Тамбова

tambov_stations=tambov_table

#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит

str(tambov_table)
tambov_stations$id

##Скачивание погодных данных для выбранных метеостанций
#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используем следующую команду

all_tambov_data = meteo_tidy_ghcnd(stationid = tambov_id)
#Подумаем, какие из этих данных нам нужны

###Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
#Создадим объект, куда скачаем все данные всех метеостанций
all_tambov_meteodata = data.frame()
#Цикл для всех метеостанций

for(i in 1:10) 
{ data=meteo_tidy_ghcnd(stationid = tambov_id,var="TAVG", date_min = "2006-01-01", date_max = "2006-12-31")
  all_tambov_meteodata=rbind(all_tambov_meteodata,all_i)
  all_i= bind_rows(all_i,data) }  

#Запись полученных данных в файл
write.csv (all_tambov_meteodata,"all_tambov_meteodata.csv")
all_tambov_meteodata
#2 часть
# считываем данные из файла all_tambov_meteodata.csv
all_tambov_meteodata = read.csv("all_tambov_meteodata.csv")
#посмотрим на данные
str(all_tambov_meteodata)
#видим, что дата записана в формате "2006-01-01"
#ищем библиотеку из tidyverse, которая может помочь с датой
library(lubridate)
# вытащить год
#проверим, что работает
y = year(all_tambov_meteodata$date); y
#добавим месяц
all_tambov_meteodata [,"month"]= month(all_tambov_meteodata$date) 
#вытащить день от начала года
all_tambov_meteodata [,"day_of_the_year"]= yday(all_tambov_meteodata$date) 
#проверим результат
str(all_tambov_meteodata)   

