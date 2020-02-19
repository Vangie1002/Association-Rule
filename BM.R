# load library
library(dplyr)
library(arules)
library(arulesViz)
library(stringr)
library(ggplot2)
library(RColorBrewer)
# Read file

data <- read.csv('combined.csv',sep=',',na.strings = " ")
str(data)
# Check that all orders in line_item are present in our orders dataset. 
# Exclude from line_item any rows that do not meet that condition.
summary(data)
# We don't need product_id because its all 0 
data$state <- NULL 
summary(data)

colnames(data) <- c('order','sku')

#data$sku = as.character(data$sku)

#data = as(data, "transactions")

# write cv 
write.csv(data,file="data.csv",row.names = FALSE)


#creating transition matrix

tran <- read.transactions('data.csv', format = c('single'), sep=',', cols = c("order", "sku"), header = TRUE)
tran
summary(tran)
inspect(tran)
# number of transactions
length(tran)
# Number of items per transaction 
size(tran)
# Lists the transactions by conversion (LIST must be capitalized)
LIST(tran)
# To see the item labels 
itemLabels(tran)
# Visualize
image(sample(tran))

itemFrequencyPlot(tran,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")


# Calculate rules use Apriori algorithm and specifying support and confidency 
rules=apriori(tran,parameter=list(support=0.00005,confidence=0.00006,maxlen=3))

length(rules) # 5005 rules
subset.Rules<-rules[quality(rules)$confidence>0.6]
length(subset.Rules)  # 53

# Inspect the top 10 rules in term of confidence 
inspect(rules)
inspect(head(sort(rules,by='lift'),10))
inspect(subset.Rules)
inspect(head(sort(subset.Rules,by='lift'),10))
top10subRules <- head(subset.Rules, n = 10, by = "confidence")
inspect(top10subRules)
# Visulize the rules 
plot(rules,jitter=0)
plot(subset.Rules,method="grouped matrix")
plot(top10subRules, method = "graph",  engine = "htmlwidget")
plot(subset.Rules,method="matrix")
plot(head(sort(rules,by='lift'),20),method="graph")
plot(head(sort(top10subRules,by='confidence'),10),method="graph")
plot(head(sort(subset.Rules,by='lift'),5),method="grouped matrix")

saveAsGraph(head(subset.Rules, n = 100, by = "lift"), file = "rules.graphml")
# Filter top 20 rules with highest lift
subRules2<-head(subset.Rules, n=20, by="lift")
plot(subRules2, method="paracoord")

