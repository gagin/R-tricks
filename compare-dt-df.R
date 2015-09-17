source("https://raw.githubusercontent.com/gagin/R-tricks/master/progress.R")
library(data.table)

kIterations <- 10000
kSteps <- 1000

tick<-progress(kSteps)
system.time({
        a <- data.frame(co=1)
        for(i in 1:kIterations) {
                tick()
                a[1,"co"]<-i
        }
})

tick<-progress(kSteps)
system.time({
        a <- data.table(co=1)
        for(i in 1:kIterations) {
                tick()
                a[1,co := i]
        }
})

# Amazingly, DT 10-15 times slower when assigning, even by reference.
# Is it perhaps because it's evaluating expression?