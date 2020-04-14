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
station_data = read.csv("station_data2020.csv") 
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

for(i in 10) 
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
all_tambov_meteodata[, "year"] =  year(all_tambov_meteodata$ date)
#добавим месяц
all_tambov_meteodata [,"month"]= month(all_tambov_meteodata$date) 
#вытащить день от начала года
all_tambov_meteodata [,"day_of_the_year"]= yday(all_tambov_meteodata$date) 
#проверим результат
str(all_tambov_meteodata)   
##Приведение средней суммы температур в подходящую форму, при помощи деления на 10
all_tambov_meteodata[,"tavg"] = all_tambov_meteodata$tavg/10
all_tambov_meteodata
#Превращение всех NA и tavg <5 в нули 
all_tambov_meteodata[is.na(all_tambov_meteodata$tavg),"tavg"] = 0
all_tambov_meteodata[all_tambov_meteodata$tavg<5, "tavg"] = 0
all_tambov_meteodata[all_tambov_meteodata$tavg>25,"tavg"]=0
summary (all_tambov_meteodata)
##Cуммарная температура за месяц за год для всех станций
#Группировка по метеостанциям, годам и месяцам при помощи функции group_by
alldays = group_by(all_tambov_meteodata, id, year, month)
alldays
#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_Tambov = summarize(alldays, tsum = sum(tavg))

sumT_alldays_Tambov


summary(sumT_alldays_Tambov)
# Сгруппируем данные по месяцам  
groups_Tambov_months = group_by(sumT_alldays_Tambov,month)
# найдем для всех метеостанций среднее по месяцам
sumT_months = summarize(groups_Tambov_months, St = mean(tsum))
sumT_months
## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000) 
# константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000) 
# константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) 
# отношение числа дней i-го месяца, 
#входящих в период вегетации культуры, к общему 
#числу дней в месяце,константа по табл. 1.
y = 1.0 
# Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf = 300
# Коэффициент использования ФАР посевом 
Qj = 1600
# калорийность урожая культуры 
Lj = 2.2 
# сумма частей основной и побочной продукции 
Ej = 25 
# стандартная влажность культуры 
# Рассчитаем Fi по месяца
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
##  Расчитываем урожай как сумму по месяцам ##
Yield = sum(sumT_months$Yi);  Yield

