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
df <- data.frame(row.names=as.Date("1990-01-01") + 0:9)
df[, 1] <- 1:10
df[, 2] <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
df[, 3] <- as.POSIXct("2000-01-01") + 1:10

colnames(df) <- c("numeric", "character", "POSIXct")

xtsdfn.obj <- as.xtsdfn(df)

print(identical(dim(df), din(xtsdfn.obj)))

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
