# TODO: Add comment
# 
# http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR/
#
###############################################################################

load("sales.RData")

summary(sales)

length(which(is.na(sales$Quant) & is.na(sales$Val)))

sum(is.na(sales$Quant) & is.na(sales$Val))

table(sales$Insp)/nrow(sales) * 100

topS <- table(sales$ID)
topP <- table(sales$Prod)
barplot(topS, main = "Transacrions per salespeople", names.arg = "",
		xlab = "Salespeople", ylab = "Amount")
barplot(topP, main = "Transacrions per product", names.arg = "",
		xlab = "Products", ylab = "Amount")

sales$Uprice <- sales$Val/sales$Quant
summary(sales$Uprice)

attach(sales)
upp <- aggregate(Uprice, list(Prod), median, na.rm = T)
topP <- sapply(c(T,F), function(o)
			upp[order(upp[,2], decreasing = o)[1:5],1])
colnames(topP) <- c('Expensive','Cheap')
topP

tops <- sales[Prod %in% topP[1,], c("Prod", "Uprice")]
tops$Prod <- factor(tops$Prod)
boxplot(Uprice ~ Prod, data = tops, ylab = "Uprice", log = "y")

vs <- aggregate(Val, list(ID), sum, na.rm = T)
scoresSs <- sapply(c(T, F), function(o)
			vs[order(vs$x, decreasing = o)[1:5], 1])
colnames(scoresSs) <- c('Most', 'Least')
scoresSs

#Top saler
sum(vs[order(vs$x, decreasing = T)[1:100], 2])/sum(Val, na.rm = T) * 100

#Bottom saler
sum(vs[order(vs$x, decreasing = F)[1:2000], 2])/sum(Val, na.rm = T) * 100

qs <- aggregate(Quant, list(Prod), sum, na.rm = T)
scoresPs <- sapply(c(T, F), function(o)
			qs[order(qs$x, decreasing = o)[1:5], 1])
colnames(scoresPs) <- c('Most', 'Least')
scoresPs

#Top quantity saler
sum(as.double(qs[order(qs$x, decreasing = T)[1:100], 2]))/sum(as.double(Quant), na.rm = T) * 100

#Bottom quantity saler
sum(as.double(qs[order(qs$x, decreasing = F)[1:4000], 2]))/sum(as.double(Quant), na.rm = T) * 100

out <- tapply(Uprice, list(Prod = Prod),
		function(x) length(boxplot.stats(x)$out))

out[order(out, decreasing = T) [1:10]]

sum(out)/nrow(sales) * 100

totS <- table(ID)
totP <- table(Prod)

nas <- sales[which(is.na(Quant) & is.na(Val)), c("ID", "Prod")]

propS <- 100 * table(nas$ID)/totS
propS[order(propS, decreasing = T)[1:10]] 


propP <- 100 * table(nas$Prod)/totP
propP[order(propP, decreasing = T)[1:10]] 

detach(sales)
sales <- sales[-which(is.na(sales$Quant) & is.na(sales$Val)),]

nnasQp <- tapply(sales$Quant, list(sales$Prod), function(x) sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp,decreasing = T)[1:10]]

sales <- sales[!sales$Prod %in% c("p2442", "p2443")]

nlevels(sales$Prod)



nnasQp <- tapply(sales$Quant, list(sales$Prod), function(x) sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp,decreasing = T)[1:10]]

