library(arules)
library('arulesViz')

TransFood <- read.csv('https://xiaoruizhu.github.io/Data-Mining-R/data/food_4_association.csv')
TransFood <- TransFood[, -1]
# Find out elements that are not equal to 0 or 1 and change them to 1.
Others <- which(!(as.matrix(TransFood) ==1 | as.matrix(TransFood) ==0), arr.ind=T )
TransFood[Others] <- 1
TransFood <- as(as.matrix(TransFood), "transactions")


#run summary report
summary(TransFood)


#itemFrequencyPlot() shows the frequency for items
itemFrequencyPlot(TransFood, support = 0.1, cex.names=0.8)

# Run the apriori algorithm
basket_rules <- apriori(TransFood,parameter = list(sup = 0.005, conf = 0.95,target="rules"))
summary(basket_rules)
inspect(head(basket_rules))

#Basket rules of size greater than 4
inspect(subset(basket_rules, size(basket_rules)>4))

inspect(subset(basket_rules, lift>5))

plot(basket_rules)
plot(head(sort(basket_rules, by="lift"), 10), method = "graph")
plot(basket_rules, method="grouped")
