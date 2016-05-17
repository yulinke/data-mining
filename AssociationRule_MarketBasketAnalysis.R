#----------------------------------------------------------------------------------------------
#-- Association Rule- Market Basket Analysis --------------------------------------------------
#-- That is exactly what the Groceries Data Set contains: a collection of receipts 
#-- with each line representing 1 receipt and the items purchased. Each line is called 
#-- a transaction and each column in a row represents an item. You can see the Groceries 
#-- data set (groceries.csv). Use association rule” to find the potential patterns which 
#-- satisfy the following criterion: 
#-- 1. Set the minimum support to 0.001 
#-- 2. Set the minimum confidence of 0.15 
#---                                  
#-- Source: Salem Marafi                                                                             
#-- http://www.salemmarafi.com/wp-content/uploads/2014/03/groceries.csv      
#---
#-- install.packages("arules", "arulesViz")
#-- May. 14, 2016 --- N97041081 柯佑霖 -------------------------------------------------------


library(arules)     #關聯式分析套件
library(arulesViz)  #視覺化的套件arulesViz

#--- 讀取.csv檔，利用.transactions(購物籃)的方式讀取
TrData <- read.transactions("./Q1/groceries.csv", format = "basket", sep = ",")
#inspect(TrData)

#--- Create an item frequency plot for the top 20 items
data(TrData)
itemFrequencyPlot(TrData,topN=20,type="absolute")

#--- 針對TrData裡的資料做關聯式分析
#--- Set the minimum support to 0.001
#--- Set the minimum confidence of 0.15
rule <- apriori(TrData, parameter = list(supp = 0.001, conf =0.15))
summary(rule)
#--- 根據lift做排列，並取出前10名
Top10Rule <- head(sort(rule,by="lift"),10)
inspect(Top10Rule)
summary(Top10Rule)

#--- 也可以使用下列的方式取出Top10
#inspect(head(sort(rule,by="lift"),10)[1:10])

#--- 刪除多於規則(Redundancies)
subset.matrix <- is.subset(x=Top10Rule, y=Top10Rule)
#--- 把這個矩陣的下三角去除，只留上三角的資訊
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
#--- 計算每個column中TRUE的個數，若有一個以上的TRUE，代表此column是多餘的
redundant <- colSums(subset.matrix, na.rm=T) >= 1
#--- 移除多餘的規則
DeleteRule <- Top10Rule[!redundant]
inspect(DeleteRule)
#--- 根據lift做排列，並取出前5名
Top5Rule <- head(sort(DeleteRule,by="lift"),5)
inspect(Top5Rule)

#--- Visualization
#pdf("Scatter plot for all rules.pdf")
plot(rule)
#dev.off()

#pdf("Graph for Top5 rules.pdf")
plot(Top5Rule, method="graph", control = list(type='items'))
#dev.off()

#pdf("Grouped matrix for Top5 rules.pdf")
plot(Top5Rule, method="grouped")
#dev.off()
