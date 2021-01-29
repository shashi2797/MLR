############TOYOTA COROLLA##############################
corolla <- read.csv ("corolla.csv")
View(corolla)
str(corolla)
corolla<-corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
summary(corolla)
#write.csv(summary(corolla),"outputs/summary_corolla.csv")
boxplot(corolla,col = 1:9)
boxplot(scale(corolla),ylim=c(-5,5))
# Checking out the correlations and the Pair plot
cor(corolla)
# write.csv(cor(corolla),'outputs/cor_corolla.csv')
# no pair of variables except Age and Price is highly correlated
pairs(corolla)



model <- lm(Price ~ ., data = corolla)
summary(model)
pred1 <- predict(model,interval="predict")
pred1
cor(pred1,corolla$Price)

##for cc and doors individual############
model.1cc <- lm(Price ~ cc , data = corolla)
summary(model.1cc) # Its significant to output

model.1Doors <- lm(Price ~ Doors , data = corolla)
summary(model.1Doors) # Its significant to output

##with both##
model.1<- lm(Price ~ cc + Doors)
summary(model.1) # Its significant to output
##qqplot##
plot(model.1)# Residual Plots, QQ-Plots, Std. Residuals vs Fitted, Cook's distance

model.1 <- lm(Price ~ ., data=corolla[-81,])
summary(model.1)

avPlots(model.1)

vif(model.1)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.

finalmodel.1 <- lm(Price ~ Age_08_04 +KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = corolla[-c(81),])
summary(finalmodel.1)                 
plot(finalmodel.1)

########50_STARTUPS##############
startups <- read.csv ("50_startups.csv")
View(startups)
str(startups)
summary(startups)
attach(startups)
head(startups)
boxplot(startups[,-4])
pairs(startups[,-4])

cor(startups[,-4])
attach(startups)

model <- lm(Profit ~ ., data = startups)
summary(model)
pred1 <- predict(model,interval="predict")
pred1
cor(pred1,startups$Profit)

plot(model)
vif(model)
avPlots(model)

model.1 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend)
summary(model.1)
plot(model.1)
vif(model.1)
avPlots(model.1)

summary(lm(Profit~R.D.Spend))
summary(lm(Profit~Administration))
summary(lm(Profit~Marketing.Spend))
summary(lm(Profit~State))
##removing state##
model.2 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend)
summary(model.2)
plot(model.2)
vif(model.2)
avPlots(model.2)
pred2 <- predict(model.2,interval="predict")
pred2
cor(pred2,startups$Profit)
