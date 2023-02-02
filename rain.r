df <- read.csv("Wind.csv")
dim(df)

df <- na.omit(df)
dim(df)

# View(df)
colnames(df)
head(df$RISK_MM)

# install.packages("ggplot2")
# library(ggplot2)
#ggplot(df, aes(x=RISK_MM, y=RainTomorrow)) + geom_bar(stat="identity")

plot(x=df$Temp9am, y=df$Evaporation,col="red",xlab="Temperature at 9am",ylab="Evaporation", main="Temperature vs Evaporation")
abline(lm(df$Evaporation ~ df$Temp9am,col="green"))


df$Range = df$MaxTemp - df$MinTemp;
plot(y=df$Range, x=df$WindGustSpeed, col="red",xlab="Wind Speed",ylab="Temperature Range", main="Temperature Range vs Wind Speed")
abline(lm(df$Range~ df$WindGustSpeed ,col="green"))

plot(y=df$, x=df$WindGustSpeed, col="red",xlab="Temperature at 9am",ylab="Evaporation", main="")
abline(lm(df$RISK_MM ~ df$Evaporation,col="green"))

barplot(table(df$RainToday), xlab = "Class", ylab = "Frequency", main ="Bar-Chart",col=c("blue","red"))
barplot(table(df$RainTomorrow), xlab = "X-axis", ylab = "Y-axis", main ="Bar-Chart",col=c("brown","green"))


df$Risk_Class = df$RISK_MM
df$Risk_Class[df$RISK_MM <= 5.5] = 0
df$Risk_Class[df$RISK_MM > 5.5] = 1
head(df$Risk_Class)
# barplot(table(df$Risk_Class), xlab = "Risk Class", ylab = "Frequency", main ="Bar-Chart",col=c("violet","yellow"))


plot(y=df$Risk_Class, x=df$Evaporation+df$Rainfall, col="red",xlab="Rainfall+Evap",ylab="Rain Tomorrow chances", main="Rain Tomorrow probability")
abline(glm(df$Risk_Class ~ df$Evaporation + df$Rainfall))
abline(h=0.5, col=("blue"))



v = df$Rainfall[df$Rainfall != 0]
p = df$Pressure9am[df$Rainfall != 0]
plot(y=p, x=v, col="red", xlab="Rainfall", ylab="Pressure", main="Pressure vs Rainfall")
abline(lm(p ~ v))
