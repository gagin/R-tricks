progress <-
        function(add="", prefix="Processing row", init=FALSE, tick.init=100) {
# default parameter meant to say something meaningful about the loop content
# Function name potentially conflicts with httr::progress
                if(init) {
                        progress.store<<-c(counter=0,
                                           ticks=0,
                                           tick=tick.init)
                }
                progress.store[1] <<- progress.store[1] + 1
                if(progress.store[1] == progress.store[3]) {
                        progress.store[2] <<- progress.store[2] + 1
                        if(add != "") add <- paste(":",add)
                        cat(paste0(prefix,
                                   " ",
                                   progress.store[2]*progress.store[3],
                                   add,
                                   "\n"))
                        progress.store[1] <<- 0
                }
        }