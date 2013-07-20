require(xts)

## tests with one class matrix just to ensure correctness and back-compatibility
data(sample_matrix)

xts.obj <- as.xts(sample_matrix, dateFormat='Date')
xtsdfn.obj <- as.xtsdfn(xts)

print(identical(index(xts.obj), index(xtsdfn.obj)))
print(identical(colnames(xts.obj), colnames(xtsdfn.obj)))
print(identical(names(xts.obj), names(xtsdfn.obj)))

## basic subsetting tests
i <- 10:120
j <- c(1, 3)

print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))
print(identitcal(xts.obj[i, ], as.xts(xtsdfn.obj[i, ])))
print(identitcal(xts.obj[, j], as.xts(xtsdfn.obj[, j])))

i <- as.POSIXct("2007-01-03", "GMT")
print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

i <- "2007-01-03/2007-02-01"
print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

i <- '2007-03'
print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

i <- '/2007-03-13'
print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

j <- c("Open", "Low")
print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

j <- c(TRUE, FALSE, FALSE, TRUE)
print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

## TODO: probably add some subsettig with first/last?

## cbind
print(identical(xtsdfn.obj, cbind(xtsdfn.obj[, 1:2], xtsdfn.obj[, 3:4])))

## rbind
print(identical(xtsdfn.obj, rbind(xtsdfn.obj[1:30, ], xtsdfn.obj[31:nrow(xtsdfn.obj)])))

## merge
set.seed(123)
N <- 10
df1 <- data.frame(x = sample(N,N), y1 = rnorm(N), row.names = as.Date(1990-01-01) + 1:N)
df2 <- data.frame(x = sample(N,N), y2 = rnorm(N), row.names = as.Date(1990-01-01) + 1:N)

xts1 <- as.xts(df1)
xts2 <- as.xts(df2)
xtsdfn1 <- as.xtsdfn(df1)
xtsdfn2 <- as.xtsdfn(df2)

print(identical(merge(xts1, xts2), as.xts(merge(xtsdfn1, xtsdfn2))))

## following tests should work fine

print(identical(cumsum(xts.obj), as.xts(cumsum(xtsdfn.obj))))
print(identical(cumprod(xts.obj), as.xts(cumprod(xtsdfn.obj))))
print(identical(cummin(xts.obj), as.xts(cummin(xtsdfn.obj))))
print(identical(cummax(xts.obj), as.xts(cummax(xtsdfn.obj))))
print(identical(mean(xts.obj), mean(xtsdfn.obj)))

## multiple class columns data.frame
## merge test
df <- data.frame(row.names=as.Date("1990-01-01") + 0:9)
df[, "numeric"] <- 1:10
df[, "character"] <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
df[, "POSIXct"] <- as.POSIXct("2000-01-01") + 1:10

df2 <- data.frame(row.names=as.Date("1990-01-01") + 0:9)
df2[, "numeric"] <- 21:30
df2[, "character2"] <- c("one2", "two2", "three2", "four2", "five2", "six2", "seven2", "eight2", "nine2", "ten2")

## result of xts-like merge of df and df2
df.res <- data.frame(row.names=as.Date("1990-01-01") + 0:9)
df.res[, "numeric"] <- df[, "numeric"]
df.res[, "character"] <- df[, "character"]
df.res[, "POSIXct"] <- df[, "POSIXct"]
df.res[, "numeric.1"] <- df2[, "numeric"]
df.res[, "character2"] <- df2[, "character2"]

xtsdfn.obj <- as.xtsdfn(df)
xtsdfn.obj2 <- as.xtsdfn(df2)

print(identical(df.res, as.data.frame(merge(xtsdfn.obj, xtsdfn.obj2))))

print(identical(dim(df), dim(xtsdfn.obj)))

## following tests should fail on non-numeric columns
print(identical(cumsum(xts.obj), as.xts(cumsum(xtsdfn.obj))))
print(identical(cumprod(xts.obj), as.xts(cumprod(xtsdfn.obj))))
print(identical(cummin(xts.obj), as.xts(cummin(xtsdfn.obj))))
print(identical(cummax(xts.obj), as.xts(cummax(xtsdfn.obj))))
print(identical(mean(xts.obj), mean(xtsdfn.obj)))

## subsetting assignment tests
xtsdfn.obj[, "numeric"] <- 101:110
df2 <- df
df2[, "numeric"] <- 101:110
print(identical(df2, as.data.frame(xtsdfn.obj)))

xtsdfn.obj[4, "character"] <- "forty"
df2[4, "character"] <- "forty"
print(identical(df2, as.data.frame(xtsdfn.obj)))

## other subsetting tests
colnames(xtsdfn.obj) <- c("numeric_new", "character_new", "POSIXct_new")

## quantstrat order book tests
demo("bbands", package="quantstrat")
order.book <- getOrderBook("bbands")$bbands$IBM

## constructor parameters may change during development
order.book.xtsdfn <- as.xtsdfn(order.book, column.classes = c("numeric", "numeric", "factor", "factor", "numeric", "factor", "POSIXct", "factor", "character", "factor", "character"))

## TODO: get examples of order book operations from quantstrat package

## add new order
new.order.time <- Sys.Date()
new.order <- xtsdfn(t(c(100, 96.3, "market", "long", NA, "closed", as.POSIXct("2008-01-01"), "", NA, 0, "ruleSignal.rule")),
                    order.by = new.order.time,
                    dimnames=list(NULL, c("Order.Qty", "Order.Price", "Order.Type",
                      "Order.Side", "Order.Threshold", "Order.Status", "Order.StatusTime",
                      "Prefer", "Order.Set", "Txn.Fees", "Rule")))

order.book.xtsdfn <- rbind(order.book.xtsdfn, new.order)
print(identical(order.book.xtsdfn[Sys.Date, ], new.order))
