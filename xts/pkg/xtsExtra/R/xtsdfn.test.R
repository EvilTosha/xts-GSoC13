## just see it works

num.matrix <- matrix(rnorm(40), ncol = 4)
colnames(num.matrix) <- c("num1", "num2", "num3", "num4")
num.xts <- as.xts(num.matrix, Sys.Date() + 1:10)

char.matrix <- matrix(rep("string", 40), ncol = 4)
colnames(char.matrix) <- c("char1", "char2", "char3", "char4")
char.xts <- as.xts(char.matrix, Sys.Date() + 1:10)

index <- rep(c(TRUE, FALSE), 4)

xdfn <- as.xtsdfn(num.xts, char.xts, index)
print(xdfn)

print(xdfn[1:2, 3:5])


## time test
set.seed(21)
numrow <- 1e3
numcol <- 1000
num.matrix <- matrix(rnorm(numrow * numcol), nrow = numrow)
char.matrix <- matrix(as.character(rnorm(numrow * numcol)), nrow = numrow)
index <- c(rep(TRUE, numcol), rep(FALSE, numcol))
xdfn <- as.xtsdfn(as.xts(num.matrix, Sys.Date() + 1:numrow),
                  as.xts(char.matrix, Sys.Date() + 1:numrow), index)

d <- cbind(data.frame(num.matrix), data.frame(char.matrix))
rownames(d) <- Sys.Date() + 1:numrow
xdf <- as.xtsdf(d)
i <- 1:5e2
j <- 1:1000

library(rbenchmark)

benchmark(xdfn[i, j], xdf[i, j])
