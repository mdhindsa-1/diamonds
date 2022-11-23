library(MASS) 
library(dplyr)
#EDA for project

diamonds= read.csv("diamonds4.csv")
diamonds = data.frame(diamonds)
head(diamonds)
#5 columns 2 quantitiative 3 categorical

diamonds$clarity = factor(diamonds$clarity)
diamonds$cut= factor(diamonds$cut)
diamonds$color =factor(diamonds$color)

levels(diamonds$clarity)
levels(diamonds$cut)
levels(diamonds$color)

#counts and summary stats of variables
count(diamonds,c(cut))
count(diamonds,c(clarity))
count(diamonds,c(color))

summary(diamonds$price)
summary(diamonds$carat)

#basic view of price vs carat
model = lm(price~carat, data = diamonds)
par(mfrow = c(1,1))
plot(model, main = "Price~Carat")
plot(diamonds$carat, (diamonds$price)^(1))
abline(model, col = 'red')
grid()
plot(model$fitted.values,model$residuals)
abline(h=0, col = 'red')
grid()

#################################### CUT PLOT ###########################################

par(mfrow=c(1,1))

a1=subset(diamonds, cut == "Very Good")
a2=subset(diamonds, cut == "Good")
a3=subset(diamonds, cut == "Ideal")
a4=subset(diamonds, cut== 'Astor Ideal')

reg1<-lm(price~carat,data=a1)
reg2<-lm(price~carat,data=a2)
reg3<-lm(price~carat,data=a3)
reg4<-lm(price~carat,data=a4)

count(diamonds,c(cut))
count(diamonds,c(clarity))
count(diamonds,c(color))
par(mfrow=c(1,1))

plot(a1$carat, a1$price, main="Price against Carats, by cut score", xlab = 'Carat',
     ylab = 'Price')
points(a2$carat, a2$price, pch=2, col='red')
points(a3$carat, a3$price, pch=3, col='blue')
points(a4$carat, a4$price, pch=4, col='green')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')

legend("topleft", c("Very Good","Good","Ideal", 'Astor Ideal'), lty=c(1,2,3,4), pch=c(1,2,3,4), col=c("black","red","blue", 'green')) 


################################ Clarity Plot ###########################################
unique(diamonds$clarity)

b1=subset(diamonds, clarity == 'VVS2')
b2=subset(diamonds, clarity == 'VS2')
b3=subset(diamonds, clarity == 'IF')
b4=subset(diamonds, clarity == 'VVS1')
b5=subset(diamonds, clarity == 'VS1')
b6=subset(diamonds, clarity == 'SI1')
b7=subset(diamonds, clarity == 'SI2')
b8=subset(diamonds, clarity == 'FL')

reg1<-lm(price~carat,data=b1)
reg2<-lm(price~carat,data=b2)
reg3<-lm(price~carat,data=b3)
reg4<-lm(price~carat,data=b4)
reg5<-lm(price~carat,data=b5)
reg6<-lm(price~carat,data=b6)
reg7<-lm(price~carat,data=b7)
reg8<-lm(price~carat,data=b8)

plot(b1$carat, b1$price, main="Price against Carats, by clarity score", xlab = 'Carat',
     ylab = 'Price')
points(b2$carat, b2$price, pch=2, col='red')
points(b3$carat, b3$price, pch=3, col='blue')
points(b4$carat, b4$price, pch=4, col='green')
points(b5$carat, b5$price, pch=5, col='orange')
points(b6$carat, b6$price, pch=6, col='purple')
points(b7$carat, b7$price, pch=7, col='gray')
points(b8$carat, b8$price, pch=8, col='pink')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')
abline(reg5,lty=5, col="orange") 
abline(reg6,lty=6, col="purple")
abline(reg7,lty=7, col='gray')
abline(reg8,lty=8, col="pink") 

legend("topleft", 
       c("VVS2","Vs2","IF", 'VVS1','VS1','SI1','SI2','FL'),
       lty=c(1,2,3,4,5,6,7,8),
       pch=c(1,2,3,4,5,6,7,8), 
       col=c("black","red","blue", 'green', 'orange', 'purple', 'gray','pink')) 


############################## color plot ##############################################
unique(diamonds$color)

c1=subset(diamonds, color == 'G')
c2=subset(diamonds, color == 'H')
c3=subset(diamonds, color == 'F')
c4=subset(diamonds, color == 'J')
c5=subset(diamonds, color == 'E')
c6=subset(diamonds, color == 'D')
c7=subset(diamonds, color == 'I')

reg1<-lm(price~carat,data=c1)
reg2<-lm(price~carat,data=c2)
reg3<-lm(price~carat,data=c3)
reg4<-lm(price~carat,data=c4)
reg5<-lm(price~carat,data=c5)
reg6<-lm(price~carat,data=c6)
reg7<-lm(price~carat,data=c7)

plot(c1$carat, c1$price, main="Price against Carats, by color", xlab = 'Carat',
     ylab = 'Price')
points(c2$carat, c2$price, pch=2, col='red')
points(c3$carat, c3$price, pch=3, col='blue')
points(c4$carat, c4$price, pch=4, col='green')
points(c5$carat, c5$price, pch=5, col='orange')
points(c6$carat, c6$price, pch=6, col='purple')
points(c7$carat, c7$price, pch=7, col='gray')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')
abline(reg5,lty=5, col="orange") 
abline(reg6,lty=6, col="purple")
abline(reg7,lty=7, col='gray')

legend("topleft", 
       c("G","H","F", 'J','E','D','I'),
       lty=c(1,2,3,4,5,6,7),
       pch=c(1,2,3,4,5,6,7), 
       col=c("black","red","blue", 'green', 'orange', 'purple', 'gray')) 

#box cox is close to 0 so we will use a log transformation for ease of interpretation
par(mfrow = c(1,1))
boxcox(model, seq(-.1,.4, by=.1), main = "Box-Cox Plot of Price~Carat") 

#out put is still not solving the constant variance so we add a transformation to the predictor
model2 = lm(log(price)~carat, data =diamonds)
boxcox(model2, seq(-5,5, by =1))
par(mfrow = c(2,2))
plot(model2, main = 'log(price)~carat')

#output now looks like it will pass linear regresion assumptions, we will check ACF and boxcox after adding categoricals
model3 = lm(log(price)~log(carat+.1), data=diamonds)
par(mfrow=c(2,2))                                                              
plot(model3, main = 'log(price)~log(carat + 0.1)')




############################## Quantitatives have now been transformed #################################3
diamonds$price = log(diamonds$price)
diamonds$carat = log(diamonds$carat +0.1)


################################Used these to decide groupings to follow#################################3

########################color plot transformed quantitatives ############################################
par(mfrow=c(1,1))
c1=subset(diamonds, color == 'G')
c2=subset(diamonds, color == 'H')
c3=subset(diamonds, color == 'F')
c4=subset(diamonds, color == 'J')
c5=subset(diamonds, color == 'E')
c6=subset(diamonds, color == 'D')
c7=subset(diamonds, color == 'I')

reg1<-lm(price~carat,data=c1)
reg2<-lm(price~carat,data=c2)
reg3<-lm(price~carat,data=c3)
reg4<-lm(price~carat,data=c4)
reg5<-lm(price~carat,data=c5)
reg6<-lm(price~carat,data=c6)
reg7<-lm(price~carat,data=c7)

plot(c1$carat, c1$price, main="Transformed Price against Carats, by color",xlab = 'log(carat+0.2)',
     ylab = 'log(Price)')
points(c2$carat, c2$price, pch=2, col='red')
points(c3$carat, c3$price, pch=3, col='blue')
points(c4$carat, c4$price, pch=4, col='green')
points(c5$carat, c5$price, pch=5, col='orange')
points(c6$carat, c6$price, pch=6, col='purple')
points(c7$carat, c7$price, pch=7, col='gray')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')
abline(reg5,lty=5, col="orange") 
abline(reg6,lty=6, col="purple")
abline(reg7,lty=7, col='gray')

legend("topleft", 
       c("G","H","F", 'J','E','D','I'),
       lty=c(1,2,3,4,5,6,7),
       pch=c(1,2,3,4,5,6,7), 
       col=c("black","red","blue", 'green', 'orange', 'purple', 'gray')) 
################################### We used this plot to determine color groupings further down ###################33

###################################cut plot after transformation##########################################
par(mfrow=c(1,1))

a1=subset(diamonds, cut == "Very Good")
a2=subset(diamonds, cut == "Good")
a3=subset(diamonds, cut == "Ideal")
a4=subset(diamonds, cut== 'Astor Ideal')

reg1<-lm(price~carat,data=a1)
reg2<-lm(price~carat,data=a2)
reg3<-lm(price~carat,data=a3)
reg4<-lm(price~carat,data=a4)

count(diamonds,c(cut))
count(diamonds,c(clarity))
count(diamonds,c(color))
par(mfrow=c(1,1))

plot(a1$carat, a1$price, main="Price against Carats, by cut score")
points(a2$carat, a2$price, pch=2, col='red')
points(a3$carat, a3$price, pch=3, col='blue')
points(a4$carat, a4$price, pch=4, col='green')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')

legend("topleft", c("Very Good","Good","Ideal", 'Astor Ideal'), lty=c(1,2,3,4), pch=c(1,2,3,4), col=c("black","red","blue", 'green')) 

#we used this plot to make the cut groupings further down in the code

################################### clarity plot post transformation #######################################3
b1=subset(diamonds, clarity == 'VVS2')
b2=subset(diamonds, clarity == 'VS2')
b3=subset(diamonds, clarity == 'IF')
b4=subset(diamonds, clarity == 'VVS1')
b5=subset(diamonds, clarity == 'VS1')
b6=subset(diamonds, clarity == 'SI1')
b7=subset(diamonds, clarity == 'SI2')
b8=subset(diamonds, clarity == 'FL')

reg1<-lm(price~carat,data=b1)
reg2<-lm(price~carat,data=b2)
reg3<-lm(price~carat,data=b3)
reg4<-lm(price~carat,data=b4)
reg5<-lm(price~carat,data=b5)
reg6<-lm(price~carat,data=b6)
reg7<-lm(price~carat,data=b7)
reg8<-lm(price~carat,data=b8)

plot(b1$carat, b1$price, main="Price against Carats, by clarity score")
points(b2$carat, b2$price, pch=2, col='red')
points(b3$carat, b3$price, pch=3, col='blue')
points(b4$carat, b4$price, pch=4, col='green')
points(b5$carat, b5$price, pch=5, col='orange')
points(b6$carat, b6$price, pch=6, col='purple')
points(b7$carat, b7$price, pch=7, col='gray')
points(b8$carat, b8$price, pch=8, col='pink')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')
abline(reg5,lty=5, col="orange") 
abline(reg6,lty=6, col="purple")
abline(reg7,lty=7, col='gray')
abline(reg8,lty=8, col="pink") 

legend("topleft", 
       c("VVS2","Vs2","IF", 'VVS1','VS1','SI1','SI2','FL'),
       lty=c(1,2,3,4,5,6,7,8),
       pch=c(1,2,3,4,5,6,7,8), 
       col=c("black","red","blue", 'green', 'orange', 'purple', 'gray','pink')) 

############################we used this plot to relevel the levels of clarity below ########################3

################################ Setting new levels for categoricals ##########################################
levels(diamonds$color) <- list("colorgroup1" = c("D"),
                               "colorgroup2"   = c("E","F","G","H"),
                               "colorgroup3"   = c("I","J"))

diamonds$color = relevel(diamonds$color, ref = "colorgroup3")
contrasts(diamonds$color)

levels(diamonds$cut) <- list("cutgroup1" = c("Astor Ideal"),
                             "cutgroup2"   = c("Ideal"),
                             "cutgroup3"   = c("Good","Very Good"))

diamonds$cut = relevel(diamonds$cut, ref = "cutgroup3")
contrasts(diamonds$cut)

levels(diamonds$clarity) <- list("claritygroup1" = c("FL","IF"),
                                 "claritygroup2"   = c("VS1","VVS1","VS2","VVS2"),
                                 "claritygroup3"   = c("SI1","SI2"))

diamonds$clarity = relevel(diamonds$clarity, ref = "claritygroup3")

############################## Creating the grouped plots ################################


par(mfrow=c(1,1))
cg1=subset(diamonds, color == 'colorgroup1')
cg2=subset(diamonds, color == 'colorgroup2')
cg3=subset(diamonds, color == 'colorgroup3')
reg1c<-lm(price~carat,data=cg1)
reg2c<-lm(price~carat,data=cg2)
reg3c<-lm(price~carat,data=cg3)
plot(cg1$carat, cg1$price, main="Price against Carats, by color", xlab = 'log(carat+0.1)', ylab='log(price)')
points(cg2$carat, cg2$price, pch=2, col='red')
points(cg3$carat, cg3$price, pch=3, col='blue')
abline(reg1c,lty=1)
abline(reg2c,lty=2, col="red")
abline(reg3c,lty=3, col="blue")

legend("topleft", c("Color Group 1","Color Group 2","Color Group 3"), lty=c(1,2,3), pch=c(1,2,3), col=c("black","red","blue"))

par(mfrow=c(1,1))
cg1=subset(diamonds, clarity == 'claritygroup1')
cg2=subset(diamonds, clarity == 'claritygroup2')
cg3=subset(diamonds, clarity == 'claritygroup3')
reg1c<-lm(price~carat,data=cg1)
reg2c<-lm(price~carat,data=cg2)
reg3c<-lm(price~carat,data=cg3)
plot(cg1$carat, cg1$price, main="Price against Carats, by clarity", xlab = 'log(carat+0.1)', ylab='log(price)')
points(cg2$carat, cg2$price, pch=2, col='red')
points(cg3$carat, cg3$price, pch=3, col='blue')
abline(reg1c,lty=1)
abline(reg2c,lty=2, col="red")
abline(reg3c,lty=3, col="blue")

legend("topleft", c("Clarity Group 1","Clarity Group 2","Clarity Group 3"), lty=c(1,2,3), pch=c(1,2,3), col=c("black","red","blue"))

par(mfrow=c(1,1))
cg1=subset(diamonds, cut == 'cutgroup1')
cg2=subset(diamonds, cut == 'cutgroup2')
cg3=subset(diamonds, cut == 'cutgroup3')
reg1c<-lm(price~carat,data=cg1)
reg2c<-lm(price~carat,data=cg2)
reg3c<-lm(price~carat,data=cg3)
plot(cg1$carat, cg1$price, main="Price against Carats, by cut", xlab = 'log(carat+0.1)', ylab='log(price)')
points(cg2$carat, cg2$price, pch=2, col='red')
points(cg3$carat, cg3$price, pch=3, col='blue')
abline(reg1c,lty=1)
abline(reg2c,lty=2, col="red")
abline(reg3c,lty=3, col="blue")
legend("topleft", c("Cut Group 1","Cut Group 2","Cut Group 3"), lty=c(1,2,3), pch=c(1,2,3), col=c("black","red","blue"))


###### Model Selection Time #########

#model with just price only predicted by carat
small=lm(price~carat, data=diamonds)
summary(small)

#model with all categoricals 
full_no_int = lm(price~., data = diamonds)
summary(full_no_int)

#Forward selection first
step(small, scope=list(lower=small, upper=full_no_int), direction="forward")


#Backward selection
step(full_no_int, scope=list(lower=small, upper=full_no_int), direction="backward")

#Both ways consider having all 3 variables based on aic however clarity is most important followed by color by cut in that order
#based on lowest aics 

model_select = lm(price~carat + clarity+color+cut, data = diamonds)
summary(model_select)

full_int = lm(price~carat*.,data = diamonds) #model with all interactions
summary(full_int) #some not very siginifcant interactions here

anova(model_select, full_int) #### F test suggest keeping the interactions, however with the parallel nature we decided to remove them


##################### Since our previous plots show similar slopes we decided to ignore interactions #################
summary(model_select)
par(mfrow=c(2,2))
plot(model_select, main = "Selected Model")
par(mfrow=c(1,2))
acf(model_select$residuals, main ='Selected Model ACF Plot')   ### potentially go with this for the lags further from significant
boxcox(model_select)

pairwise_col<-glht(full_no_int, linfct = mcp(color="Tukey"))
pairwise_cut<-glht(full_no_int, linfct = mcp(cut="Tukey"))
pairwise_clar<-glht(full_no_int, linfct = mcp(clarity="Tukey"))


summary(pairwise_clar)
summary(pairwise_col)
summary(pairwise_cut)

