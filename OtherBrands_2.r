---
title: "Stats Diamonds Project Notebook"
output:
  html_document:
    df_print: paged
  html_notebook: default
  pdf_document: default
---
8:15 Wednesday section, Group 6
Manpreet, Tulsi, TJ, Jonathan

This code's purpose is to focus on external data sources for 
diamond pricing, to see if they are compatible with the project dataset and the model we have developed.

The code will:
Read in original data plus external brand data
Then combine into single dataframe "diamonds" 
Add a new column "vendor"
The original data has value "vendor" == "Original"

```{r}
orig_diamonds= read.csv("diamonds4.csv")
orig_diamonds = data.frame(orig_diamonds)
orig_diamonds$vendor = "Original"

extra_diamonds= read.csv("extra_diamonds.csv")
extra_diamonds = data.frame(extra_diamonds)
keep_columns = c("carat","clarity","color","cut","price","vendor")
extra_diamonds = extra_diamonds[keep_columns]

# Merge the external data and original 
# So that transformations occur in the same way to both types
diamonds <- rbind(extra_diamonds, orig_diamonds)

# preserve original values for price and carat, pre-transform
# So that we can continue to refer to them in terms that
# make sense to the customer
diamonds$o_price <- diamonds$price
diamonds$o_carat <- diamonds$carat
```

Reduce categories for model simplicity.  The justification 
for these is in the EDA section of our project document.

```{r}
library(MASS) 
library(dplyr)
#EDA for project

diamonds$clarity = factor(diamonds$clarity)
diamonds$cut= factor(diamonds$cut)
diamonds$color =factor(diamonds$color)

#adjusting the data frame variables to the transformed versions
diamonds$price = log(diamonds$price)
diamonds$carat = log(diamonds$carat + .1)

##### setting new levels and reference levels
levels(diamonds$color) <- list("colorgroup1" = c("D"),
                      "colorgroup2"   = c("E","F","G","H"),
                      "colorgroup3"   = c("I","J","K"))

diamonds$color = relevel(diamonds$color, ref = "colorgroup3")
#contrasts(diamonds$color)

levels(diamonds$cut) <- list("cutgroup1" = c("Astor Ideal"),
                             "cutgroup2"   = c("Ideal", "Excellent"),
                             "cutgroup3"   = c("Good","Very Good"))

diamonds$cut = relevel(diamonds$cut, ref = "cutgroup3")
#contrasts(diamonds$cut)

levels(diamonds$clarity) <- list("claritygroup1" = c("FL","IF"),
                                 "claritygroup2"   = c("VS1","VVS1","VS2","VVS2"),
                                 "claritygroup3"   = c("SI1","SI2"))

diamonds$clarity = relevel(diamonds$clarity, ref = "claritygroup3")

```

Set aside the external vendors' diamonds into dataframe "subExtra"
Then remove them from our main "diamonds" dataframe
They will not influence our model and can be used for testing

After this code snippet, "diamonds" will contain the same data as the original (except it has the new column "vendor")

```{r}
subCostco<-subset(diamonds,vendor=="Costco")
subCostco$subcolor = "coral3"
subCostco$subpch = 2
subBlueNile<-subset(diamonds,vendor=="BlueNile")
subBlueNile$subcolor = "blue"
subBlueNile$subpch = 12
subDanielWilliams<-subset(diamonds,vendor=="DanielWilliams")
subDanielWilliams$subcolor = "green"
subDanielWilliams$subpch = 8
subExtras <- rbind(subBlueNile, subCostco, subDanielWilliams)
diamonds<-subset(diamonds,vendor=="Original")
```

See how Vendors' diamond data compares to original
Temporarily remove diamonds over 4.5 carats to better focus on
the Vendors' diamonds 

```{r}
diamonds_focused<-subset(diamonds,carat<4.5)
plot(diamonds_focused$carat,
     diamonds_focused$price, 
     col = "grey",
     main="Price against carat",
     ylab="Log(Price in dollars)",
     xlab="Log(Carats)")
points(subExtras$carat,subExtras$price, 
       pch=subExtras$subpch, 
       col=subExtras$subcolor)
legend("topleft", c("Original Data","Costco","Blue Nile","Daniel William"), lty=c(1,2,3,4),
       pch=c(1,2,12,8), 
       cex = 0.85,
       col=c("grey","coral3","blue","green"))
```

Create model (without any interactions)

```{r}
#model with all additive terms and condensed #classes
test_lm = lm(price~carat+cut+clarity+color, data=diamonds)
summary(test_lm)

```

Initial look to see if predictions based on original dataset are compatible with externally sourced diamonds.


```{r}

pExtras <- data.frame(predict(test_lm, newdata = subExtras, interval = "prediction"))
subExtras$predicted <- exp(pExtras$fit)

subExtras

```

Graph of prediction intervals compared to external data points

```{r}
pred.int <- predict(test_lm, newdata = subExtras, interval = "prediction")

ggdata <- cbind(subExtras,pred.int)
library("ggplot2")

p <- ggplot(ggdata, aes(carat, price)) +
  geom_point(pch=subExtras$subpch, 
             col=subExtras$subcolor) 
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_line(aes(y = fit), color = "blue")
#  stat_smooth(method = "loess")  # "gam" and "lm" are other options

#NB I can't figure out how to make a legend :-(
```


Bar chart showing what percentages of external-sample data are under/over the prediction interval of our model.

```{r}
subExtras$upr <- exp(pExtras$upr)
subExtras$lwr <- exp(pExtras$lwr)

# Define 5 categories for prediction accuracy:
# --Below and above PI
# --Top- and bottom- half within PI
# --Within 5% of model-predicted mean price
subExtras$PAccuracy[subExtras$o_price < subExtras$lwr] = "Below PI"
subExtras$PAccuracy[subExtras$o_price >= subExtras$lwr] = "Low"
subExtras$PAccuracy[subExtras$o_price > (subExtras$predicted * 0.95)] = "Accurate"
subExtras$PAccuracy[subExtras$o_price >= (subExtras$predicted * 1.05)] = "High"
subExtras$PAccuracy[subExtras$o_price >= subExtras$upr] = "Above PI"

M <- c("Below PI", "Low", "Accurate", "High", "Above PI")
subExtras$PAccuracy <- factor(subExtras$PAccuracy, levels = M)
counts <- table(subExtras$PAccuracy, subExtras$vendor)
counts2 <- prop.table(counts, 2) # change counts to %
par(mfrow=c(1, 1), mar=c(5, 5, 4, 8))
barplot(counts2, main="Diamond Prices compared to Predicted Price",
  xlab="Diamond Seller",
  ylab="Sample Frequency",
  col=c("green","blue","yellow","orange","red"),
  names.arg=c("Blue\nNile","Costco\n","Daniel\nWilliam"),
  legend = rownames(counts),
  args.legend = list(x = "topright", bty = "n", inset=c(-0.5, 0)))
```

