## loading of order book example

load(url("http://braverock.com/brian/order_book.rda"))
#loads an order_book character mode xts object

## such conversion produces non-unique indexes
order.book.df <- as.data.frame(order_book, stringsAsFactors = FALSE)

order.book.df$Order.Qty        <- as.factor(order.book.df$Order.Qty)
order.book.df$Order.Price      <- as.double(order.book.df$Order.Price)
order.book.df$Order.Type       <- as.factor(order.book.df$Order.Type)
order.book.df$Order.Side       <- as.factor(order.book.df$Order.Side)
order.book.df$Order.Threshold  <- as.double(order.book.df$Order.Threshold)
order.book.df$Order.Status     <- as.factor(order.book.df$Order.Status)
## this operation drops times, but for demonstration purposes it's okay
order.book.df$Order.StatusTime <- as.POSIXct(order.book.df$Order.StatusTime)
order.book.df$Prefer           <- as.factor(order.book.df$Prefer)
order.book.df$Order.Set        <- as.character(order.book.df$Order.Set)
order.book.df$Txn.Fees         <- as.factor(order.book.df$Txn.Fees)
order.book.df$Rule             <- as.character(order.book.df$Rule)


order.book.xtsdfn <- as.xtsdfn(order.book.df)

## tests with one class matrix just to ensure correctness and back-compatibility
data(sample_matrix)

xts.obj <- as.xts(sample_matrix, dateFormat='Date')
xtsdfn.obj <- as.xtsdfn(xts.obj)

print(identical(index(xts.obj), index(xtsdfn.obj)))
print(identical(colnames(xts.obj), colnames(xtsdfn.obj)))

## basic subsetting tests
i <- 10:120
j <- c(1, 3)

print(identical(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))
print(identical(xts.obj[i, ], as.xts(xtsdfn.obj[i, ])))
print(identical(xts.obj[, j], as.xts(xtsdfn.obj[, j])))

i <- as.POSIXct("2007-01-03", "GMT")
print(identical(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

i <- "2007-01-03/2007-02-01"
print(identical(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

i <- '2007-03'
print(identical(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

i <- '/2007-03-13'
print(identical(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

j <- c("Open", "Low")
print(identical(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

j <- c(TRUE, FALSE, FALSE, TRUE)
print(identical(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

## TODO: probably add some subsettig with first/last?

## cbind
print(identical(xtsdfn.obj, cbind(xtsdfn.obj[, 1:2], xtsdfn.obj[, 3:4])))
print(identical(xtsdfn.obj, cbind(xtsdfn.obj[, "Open"], xtsdfn.obj[, c(FALSE, TRUE, TRUE, TRUE)])))
print(identical(cbind(xts.obj[1:2, ], xts.obj[, 3:4]), as.xts(cbind(xtsdfn.obj[1:2, ], xtsdfn.obj[, 3:4]))))

## rbind
print(identical(xtsdfn.obj, rbind(xtsdfn.obj[1:30, ], xtsdfn.obj[31:nrow(xtsdfn.obj)])))
print(identical(xtsdfn.obj, rbind(xtsdfn.obj["2007-03-14/", ], xtsdfn.obj["/2007-03-13", ])))
## following test should fail
print(identical(rbind(xts.obj[1:2, ], xts.obj[, 3:4]), as.xts(rbind(xtsdfn.obj[1:2, ], xtsdfn.obj[, 3:4]))))


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
