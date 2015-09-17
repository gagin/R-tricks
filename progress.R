# Function to indicate progress in loop
# Initialiaze before loop with my_progress<-progress()
# Then call my_progress() in every loop
progress <- function(tick.step=100) {
        # Function name potentially conflicts with httr::progress
        counter <- 0
        ticks   <- 0
        tick    <- tick.step
        function(add="", prefix="Processing row") {
                # default arg meant to say something meaningful about the loop content        
                counter <<- counter + 1
                if(counter == tick) {
                        ticks <<- ticks + 1
                        if(add != "") add <- paste(":",add)
                        cat(paste0(prefix,
                                   " ",
                                   as.character(ticks*tick),
                                   add,
                                   "\n"))
                        counter <<- 0
                }
        }
}