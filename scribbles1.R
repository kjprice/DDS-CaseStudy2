## problem 1
city = read.csv('data/CityTemp.csv')
str(city)
head(city)
length(unique(city$Country))


temp = read.csv('data/Temp.csv')
tempWithValues = temp[!is.na(temp$Monthly.AverageTemp),]
str(temp)
dim(city)

diff.range = function(temps) {
  range.numbers = range(temps)
  range.numbers[2] - range.numbers[1]
}

tempWithDates = tempWithValues[!is.na(tempWithValues$Date),]


#tempWithDates$date = as.Date(tempWithDates$Date)
tempWithDates$date = as.character(tempWithDates$Date)
tempWithDates$year = gsub('[0-9]{1,2}/[0-9]{1,2}/', '', tempWithDates$date)
tempWithDates$year = gsub('-[0-9]{1,2}-[0-9]{1,2}', '', tempWithDates$year)

temp.since.1900 = tempWithDates[tempWithDates$year > "1900",]

#plot(temp.since.1900$Monthly.AverageTemp)

country.temperature.ranges = tapply(temp.since.1900$Monthly.AverageTemp, temp.since.1900$Country, diff.range)

most.varying.temps = sort(country.temperature.ranges, TRUE)[1:20]

plot(most.varying.temps, type="o", col="red", xlab = "Country", ylab = "Temperature")
legend("topright", pch=1, col=c('red'), c('Temp by country'), lty = 10, text.width = 4)


# Part 2 - UStemp
library(NISTunits)
temps.after.1990 = tempWithDates[tempWithDates$year >= "1990",]
UStemp = temps.after.1990[temps.after.1990$Country == 'United States',]
UStemp$farenheit = NISTdegCtOdegF(UStemp$Monthly.AverageTemp)


avg.temperatures = tapply(UStemp$farenheit, UStemp$year, mean)
x.values = 1990 + 0:(length(avg.temperatures)-1)
plot(x.values, avg.temperatures, xlab = 'Year', ylab='Temperature')

# c)	Calculate the one year difference of average land temperature by year and provide the maximum difference (value) with corresponding two years

one.year.difference = (c(0, avg.temperatures) - c(avg.temperatures, 0))[2:length(avg.temperatures)]

#part 3: a.	Find the difference between the maximum and the minimum temperatures for each major city

cityWithValues = city[!is.na(city$Monthly.AverageTemp),]
cityWithDates = cityWithValues[!is.na(cityWithValues$Date),]

#cityWithDates$date = as.Date(cityWithDates$Date)
cityWithDates$date = as.character(cityWithDates$Date)
cityWithDates$year = gsub('[0-9]{1,2}/[0-9]{1,2}/', '', cityWithDates$date)
cityWithDates$year = gsub('-[0-9]{1,2}-[0-9]{1,2}', '', cityWithDates$year)

city.since.1900 = cityWithDates[cityWithDates$year > "1900",]

city.temperature.ranges = tapply(city.since.1900$Monthly.AverageTemp, city.since.1900$City, diff.range)

most.varying.city = sort(city.temperature.ranges, TRUE)[1:20]

plot(most.varying.temps, type="o", col = 'blue', xlab = 'Year', ylab='Temperature')
legend("topright", pch=1, col=c('blue'), c('Temp by city'), text.width = 6)

plot(most.varying.city, type = "o", col='red', xlab = 'Year', ylab='Temperature')
lines(most.varying.temps, type="o", col = 'blue')
legend("topright", pch=1, col=c('red', 'blue'), c('Temp by country', 'Temp by city'), text.width = 5)


