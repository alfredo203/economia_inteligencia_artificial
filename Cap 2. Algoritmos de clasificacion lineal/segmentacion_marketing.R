a <- read.csv("Online Retail.csv", stringsAsFactors = F)

f<- a %>% filter(Country == "Poland" | Country == "Japan")

plot(f$Quantity, f$UnitPrice, col = as.factor(f$Country))
plot(log(f$Quantity), log(f$UnitPrice), col = as.factor(f$Country))
