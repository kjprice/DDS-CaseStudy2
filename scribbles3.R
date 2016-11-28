head(Orange)
tapply(Orange$circumference, Orange$Tree, mean)
Circumference = Orange$circumference
Age = Orange$age

plot(Circumference, Age, pch=c(24, 25, 21, 22, 23), col=c('red', 'blue', 'green', 'cyan', 'black'))
legend(30, 1500, pch=c(24, 25, 21, 22, 23), legend = paste("size", c(1, 2, 3, 4, 5)), title="Tree Sizes", col=c('red', 'blue', 'green', 'cyan', 'black'), bty='n', cex=.8, xpd = FALSE)

sortedData = Orange[order(Orange$circumference),]
boxplot(sortedData$circumference ~ sortedData$Tree)
