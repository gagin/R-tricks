## Statistics is counterintuitive. Benford's law is especially so.
## It's an observation that for real data from unregulared processes
## number of entries starting with lower digits is much higher than
## ones starting with higher ones. Numbers starting with 1s are about 30%
## while less than 5% start with 9.
## See https://en.wikipedia.org/wiki/Benford%27s_law
## It's intuitive for processes with exponential growth or decay like
## growing bacteria or gaining votes. But in reality it also applies to
## a lot of cases where intuitive result is quite the opposite. Take
## incomes, for example. One could assume that incomes should be grouped
## somewhere around average income. Supposedly there should be a lot more
## people with incomes starting with 4s and 5s than 1s, right?
## Let's check.

dev      <- FALSE
devlines <- 1000

library(dplyr)
library(data.table)
income <- "PINCP"

if (dev) lines <- c(devlines,0) else lines <- c(-1L,-1L)
d <- rbind(
        fread("../input/pums/ss13pusa.csv", select=income, nrows=lines[1] ),
        fread("../input/pums/ss13pusb.csv", select=income, nrows=lines[2] )
)

inc <- as.numeric(d$PINCP)

# What's the actual average for this group?
summary(inc)
# With median this low there should be a lot of 1s, but still, let's do it.
# First, let's look at basic histogram for under 100k incomes
hist(inc[inc>0 & inc < 100000 & !is.na(inc)]/1000,
    main = "Incomes split by bins for US positive incomes under US$100k",
    xlab = "Thousand $US",
    col=rev(terrain.colors(20))
)
# Now let's switch to first digits distribution.

# This function to extract the first non-zero digit has a great potential for improvement.
vFirstDigit <- function(numbers.vector) {
        sapply(numbers.vector, function(x) {
                if (x > 0) {
                        while (x < 1) x <- x*10
                        x %>% as.character %>% substr(1,1) %>% as.integer -> x
                } else {
                        x <- 0
                }
                return(x)
        })
}

inc[!is.na(inc)] %>% vFirstDigit -> first.digits

# What distribution of first non-zero digits looks like?

nonzero <- first.digits[first.digits != 0]
nonzero %>% table
nonzero %>% hist(breaks=0:9,
                                          xaxt="n",
                                          main="Benford's law application to US incomes",
                                          xlab="First digit of positive incomes",
                                          col=rev(terrain.colors(9)))
axis(1, at=0:9, labels=c("",1:9), hadj=6)

# Summary doesn't presume so, but intuitively there should be many people with income in 100k-200k range,
# is it possible that they skew the data? What if we look at sub-100k earners?

inc[inc < 100000 & !is.na(inc)] %>% vFirstDigit -> first.digits.under100k

under100 <- first.digits.under100k[first.digits.under100k != 0]
under100 %>% table
under100 %>% hist(breaks=0:9,
                                          xaxt="n",
                                          main="Benford's law application to US incomes under 100k",
                                          xlab="First digit of positive incomes",
                                          col=rev(terrain.colors(9)))
axis(1, at=0:9,  labels=c("",1:9), hadj=6)

# Well, 1s are now lower but the Benford's law still works. Interestingly, with a smaller debug sample
# I had 2s and 3s very almost at the same level, which was weird. But on a bigger sample, it all leveled out,
# as it should have. Well, except 7s and 8s :)
