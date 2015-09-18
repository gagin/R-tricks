source("https://raw.githubusercontent.com/gagin/R-tricks/master/progress.R")
library(data.table)
library(dplyr)

kSteps <- 10000L

its <- c(1000L,10000L,100000L,1000000L)
ways <- list(
        c("LI <- list",'LI[[1]][1] <- i'),
        c("LI <- list",'LI[["co"]][1] <- i'),
        c("LI <- list",'LI[[1L]][1L] <- i'),
        c("LI <- list",'LI[["co"]][1L] <- i'),
        c("LI <- list",'LI[[1]] <- i'),
        c("LI <- list",'LI[["co"]] <- i'),
        c("LI <- list",'LI[[1L]] <- i') )#,
                vain<-list(
                c("DT <- data.table",'DT[["co"]][1] <- i'))
                c("DT <- data.table",'DT[1, co := i]'),
                c("DT <- data.table",'DT[[1]][1] <- i'),
                c("DT <- data.table",'DT[["co"]][1] <- i'),
                c("DF <- data.frame",'DF[1, 1] <- i'),
                c("DF <- data.frame",'DF[1, "co"] <- i'),
                c("DF <- data.frame",'DF[[1]][1] <- i'),
                c("DF <- data.frame",'DF[["co"]][1] <- i'),
        c("DT <- data.table",'DT[1L, co := i]'),
        c("DT <- data.table",'DT[[1L]][1L] <- i'),
        c("DT <- data.table",'DT[["co"]][1L] <- i'),
        c("DF <- data.frame",'DF[1L, 1L] <- i'),
        c("DF <- data.frame",'DF[1L, "co"] <- i'),
        c("DF <- data.frame",'DF[[1L]][1L] <- i'),
        c("DF <- data.frame",'DF[["co"]][1L] <- i')
)

benchmarks <- data.frame(matrix(ncol=3L,nrow=length(its)*length(ways)))
colnames(benchmarks) <- c("Method","Iterations","Time")
for(itn in 1L:length(its)) {
        it <- its[itn]
        for (wayn in 1L:length(ways)) {
                way <- ways[[wayn]]
                tick<-progress(kSteps)
                tt <- system.time({
                        eval(parse(text=paste0(way[1L],"(co=1L)")))
                        for(i in 1L:it) {
                                tick(paste0("Processing ", way,
                                            " for ", it,
                                            ": step "))
                                eval(parse(text=way[2L]))
                        }
                })
                benchmarks[wayn+(itn-1L)*length(ways), ] <- list(way[2L],
                                                                it,
                                                                tt[3L])        
        }
}

### Ways to assign from slowest to fastest
# 1. Slowest - DT-style assigning:
# df[1, co := i]
# Using explicit integer notation with "L" doesn't seem to make any difference.

# 2. DF's cell index is 40 times faster:
# df[1, 1] <- i 

# 3-4. List-style notation is marginally faster, and DF is better at it.
# For this item and above, referencing column by name looks bit slower,
# but I'm not sure if the difference isn't random.

# 5. The fastest is direct list assignment, and name-reference is somehow
# better there.
# Of course, assignment to the root level of the nested list is faster.
# In order to be able to assign to a arbitrary position, multi-dimentional
# list should be initialized first.
list.template<-function(nrow=1,ncol=1) rep(list(rep(NA,nrow)),ncol)
# And then convert to frame or table for further processing
list.template(2,4) %>% as.data.table


# dput(benchmarks,"benchmarks4.dput")
# benchmarks <- rbind(benchmarks,get("benchmarks3.dput"))

# DT-style results aggregation
as.data.table(benchmarks)[
        , rate := 1e+6*Time/Iterations
        ][
        , .(Seconds.per.million=round(mean(rate))), by=Method
        ][
        order(Seconds.per.million)
        ]

# Mixed-style aggregation slower by 30% (sample of 1 attempt)
as.data.table(benchmarks)[
        , rate := 1e+6*Time/Iterations
        ][
        , .(Seconds.per.million=rate %>% mean %>% round), by=Method
        ][
        order(Seconds.per.million)
        ] %>%
        print(row.names=FALSE) # Isn't style-related, wasn't in testing

# Please notice that other operations in benchmarking script take time too,
# and they are distributed in code differently, so will probably affect
# shorter iterations more significantly.
# For example, counter reset in progress() happens more in longer loops,
# and so will increase average cycle time for higher iterations.

# Ideally test should be run multiple times, and then means should be used
# with confidence intervals.
# Also same script should be run without assignment part, so assignment's
# net cost will be then calculated by subtraction.
# Could be another exercise for statistical inference.
