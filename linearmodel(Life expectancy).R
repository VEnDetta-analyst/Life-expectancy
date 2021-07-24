data = read.csv("https://ibm.box.com/shared/static/q0gt7rsj6z5p3fld163n70i65id3awz3.csv")
head(data)
#Visualising the dataset 
plot(my_data$Access_to_Sanitation,my_data$Life_Expectancy,xlab="Access to sanitation(% of population)",ylab="Life expentency (years)",col='red',lwd=1)
#We'll just make simple barplot to see bottom 20 countries for sanitation
my_data=my_data[order(my_data["Access_to_Sanitation"]),]
barplot(my_data[c(1:20),"Access_to_Sanitation"],
        names.arg = as.vector(my_data[c(1:20),"Country"]),
        col = "red", las = 2,
        ylab = "Access to Sanintation (% of Population)")

my_data=my_data[order(my_data["Life_Expectancy"]),]
barplot(my_data[c(1:20),"Life_Expectancy"],
        names.arg = as.vector(my_data[c(1:20),"Country"]),
        col="blue",las=2,
        ylab="Life expentancy(years)")
#Building model
sanitation=as.vector(my_data$Access_to_Sanitation)
model=lm(Life_Expectancy~sanitation, data=my_data)
summary(model)
model
#Interpretation: Adj r square is 0.7147. So, our model is 71.47% accurate.
#pvalue is less than 2.2e-16 therefore we can say there is relationship between both variables
plot(model)
#plotting fitted line
plot(my_data$Access_to_Sanitation,my_data$Life_Expectancy,col="blue")
abline(model,col="red")
#Prediction
points_to_predict=data.frame(sanitation=c(10,42))
prediction=predict(model,points_to_predict,interval = "prediction")
prediction
#Population in a country having 10% sanitation will have life expectancy 47-65 years.