require(xts)

## tests with one class matrix just to ensure correctness and back-compatibility
data(sample_matrix)

xts.obj <- as.xts(sample_matrix, dateFormat='Date')
xtsdfn.obj <- as.xtsdfn(xts)

print(identical(index(xts.obj), index(xtsdfn.obj)))

## basic subsetting tests
i <- 10:120
j <- c(1, 3)

print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

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

## TODO: add merge tests

## following tests should work fine

print(identical(cumsum(xts.obj), as.xts(cumsum(xtsdfn.obj))))
print(identical(cumprod(xts.obj), as.xts(cumprod(xtsdfn.obj))))
print(identical(cummin(xts.obj), as.xts(cummin(xtsdfn.obj))))
print(identical(cummax(xts.obj), as.xts(cummax(xtsdfn.obj))))
print(identical(mean(xts.obj), mean(xtsdfn.obj)))


df <- data.frame(row.names=as.Date("1990-01-01") + 0:9)
df[, 1] <- 1:10
df[, 2] <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
df[, 3] <- as.POSIXct("2000-01-01") + 1:10

colnames(df) <- c("numeric", "character", "POSIXct")

xtsdfn.obj2 <- as.xtsdfn(df)

## following tests should fail on non-numeric columns
print(identical(cumsum(xts.obj), as.xts(cumsum(xtsdfn.obj))))
print(identical(cumprod(xts.obj), as.xts(cumprod(xtsdfn.obj))))
print(identical(cummin(xts.obj), as.xts(cummin(xtsdfn.obj))))
print(identical(cummax(xts.obj), as.xts(cummax(xtsdfn.obj))))
print(identical(mean(xts.obj), mean(xtsdfn.obj)))

## subsetting assignment tests
xtsdfn.obj2[, "numeric"] <- 101:110
df2 <- df
df2[, "numeric"] <- 101:110
print(identical(df2, as.data.frame(xtsdfn.obj2)))

xtsdfn.obj2[4, "character"] <- "forty"
df2[4, "character"] <- "forty"
print(identical(df2, as.data.frame(xtsdfn.obj2)))
