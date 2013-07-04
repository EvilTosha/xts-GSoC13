## just see it works

num.matrix <- matrix(rnorm(40), ncol = 4)
colnames(num.matrix) <- c("num1", "num2", "num3", "num4")
num.xts <- as.xts(num.matrix, Sys.Date() + 1:10)

char.matrix <- matrix(rep("string", 40), ncol = 4)
colnames(char.matrix) <- c("char1", "char2", "char3", "char4")
char.xts <- as.xts(char.matrix, Sys.Date() + 1:10)

column.classes <- rep(c("character", "double"), 4)

xdfn <- xtsdfn(num.xts, char.xts, column.classes = column.classes)
print(xdfn)

print(xdfn[1:2, 3:5])


## time test
set.seed(21)
numrow <- 1e3
numcol <- 1000
num.matrix <- matrix(rnorm(numrow * numcol), nrow = numrow)
char.matrix <- matrix(as.character(rnorm(numrow * numcol)), nrow = numrow)
column.classes <- c(rep("character", numcol), rep("double", numcol))
xdfn <- xtsdfn(as.xts(num.matrix, Sys.Date() + 1:numrow),
               as.xts(char.matrix, Sys.Date() + 1:numrow), column.classes = column.classes)

d <- cbind(data.frame(num.matrix), data.frame(char.matrix))
rownames(d) <- Sys.Date() + 1:numrow
xdf <- as.xtsdf(d)
i <- 1:5e2
j <- 1:1000

library(rbenchmark)

benchmark(xdfn[i, j], xdf[i, j])
