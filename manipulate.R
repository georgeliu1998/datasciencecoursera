library(manipulate)
myhist <- function(sample.size, sample.freq) {
    # Generate random exponentials to form the population distribution
    pop.size <- 10000
    exp.dist <- rexp(pop.size)
    # Repeat sample.freq times, each time, take a sample from the population 
    # and calculate the mean, store in sampling.means.  
    sampling.means <- c()
    for (i in 1:sample.freq){
        exp.sample <- sample(exp.dist, size=sample.size)   
        sample.mean <- mean(exp.sample)
        sampling.means <- c(sampling.means, sample.mean)
    }
    # Plot the population and sampling distributions
    par(mfrow = c(1, 2))
    hist(exp.dist, main = "Population Distribution", 
         xlab = "Random Exponentials")
    hist(sampling.means, main = "Sampling Mean Distribution", 
         xlab = "Sampling Means")
}
# Use manipulate function to adjust sample size and frequency
manipulate(myhist(sample.size, sample.freq), 
           sample.size=slider(1, 100, step = 5), 
           sample.freq=slider(100, 1000, step = 100))
