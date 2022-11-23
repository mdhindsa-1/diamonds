# diamonds

Diamond pricing can be complex, taking into consideration many different factors
including the “4 C’s”: carat weight, cut, color and clarity. This project uses 1,212 observations of
diamonds from the online diamond retailer, Blue Nile, to build a statistically based model that,
when provided with the 4C’s, can estimate diamond prices. The project aims to lay the
groundwork for a tool that would help consumers be better informed diamond shoppers.
Though a simpler model using fewer inputs would have been sufficient, we want to create
a model where customers can toggle the predictive variables to reach a desired price point. The
below equation shows that when there is an increase of x% in the carat weight, the predicted
price is increased by a factor of (1 + 𝑥/100) while the categorical variables are held 2.232326
constant. Our model also shows promise for helping consumers who purchase from other
diamond retailers.
𝑙𝑜𝑔(𝑃𝑟𝑖𝑐𝑒) = 𝑙𝑜𝑔(𝐶𝑎𝑟𝑎𝑡 + 0. 1) + 𝐶𝑙𝑎𝑟𝑖𝑡𝑦 + 𝐶𝑜𝑙𝑜𝑟 + 𝐶𝑢𝑡.
The following project will first assess relationships between the 4C predictor variables and
whether they affect the response variable, price. Next, we outline the steps taken to transform the
predictors and build a multiple linear regression model. Finally, we will discuss whether the
model is useful enough to estimate diamond pricing from other retailers such as Costco and
Daniel William
