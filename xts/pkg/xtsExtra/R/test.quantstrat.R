require(xts)

data(sample_matrix)

xts.obj <- as.xts(sample_matrix, dateFormat='Date')
xtsdfn.obj <- as.xtsdfn(xts)

i <- 10:120
j <- c(1, 3)

print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

i <- "2007-01-03/2007-02-01"
print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

j <- c("Open", "Low")
print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

j <- c(TRUE, FALSE, FALSE, TRUE)
print(identitcal(xts.obj[i, j], as.xts(xtsdfn.obj[i, j])))

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


df <- ## multiple class columns data.frame

xtsdfn.obj2 <- as.xtsdfn(df)

## following tests should fail on non-numeric columns
print(identical(cumsum(xts.obj), as.xts(cumsum(xtsdfn.obj))))
print(identical(cumprod(xts.obj), as.xts(cumprod(xtsdfn.obj))))
print(identical(cummin(xts.obj), as.xts(cummin(xtsdfn.obj))))
print(identical(cummax(xts.obj), as.xts(cummax(xtsdfn.obj))))
print(identical(mean(xts.obj), mean(xtsdfn.obj)))
