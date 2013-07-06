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
numcol <- 100
num.matrix <- matrix(rnorm(numrow * numcol), nrow = numrow)
char.matrix <- matrix(as.character(rnorm(numrow * numcol)), nrow = numrow)
column.classes <- c(rep("character", numcol), rep("double", numcol))
xdfn <- xtsdfn(as.xts(num.matrix, Sys.Date() + 1:numrow),
               as.xts(char.matrix, Sys.Date() + 1:numrow), column.classes = column.classes)

d <- cbind(data.frame(num.matrix), data.frame(char.matrix))
rownames(d) <- Sys.Date() + 1:numrow
xdf <- as.xtsdf(d)
i <- 1:5e2
j <- 1:100

library(rbenchmark)

benchmark(xdfn[i, j], xdf[i, j])

## multiple class data frame

## http://braverock.com/brian/order_book.rda
load("../../../../data/order_book.rda")

order.book.df <- as.data.frame(order_book)

## some ugly transformations
order.book    <- transform(order.book.df,
                           Order.Qty        = as.character(Order.Qty),
                           Order.Price      = as.numeric(as.character(Order.Price)),
                           Order.Type       = as.character(Order.Type),
                           Order.Side       = as.character(Order.Side),
                           Order.Threshold  = as.numeric(as.character(Order.Threshold)),
                           Order.Status     = as.character(Order.Status),
                           Order.StatusTime = as.character(Order.StatusTime),
                           Prefer           = as.character(Prefer),
                           Order.Set        = as.character(Order.Set),
                           Txn.Fees         = as.character(Txn.Fees),
                           Rule             = as.character(Rule))

## original index has non-unique values
rownames(order.book) <- Sys.Date() + 1:nrow(order.book)

ob.xtsdfn <- as.xtsdfn.data.frame(order.book)
ob.xtsdf <- as.xtsdf(order.book)

i <- 400:12000
j <- c(3, 4, 7, 9)

benchmark(ob.xtsdf[i, j], ob.xtsdfn[i, j])
