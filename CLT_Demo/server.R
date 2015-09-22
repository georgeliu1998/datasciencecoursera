# Generate random exponentials to form the population distribution
pop.size <- 10000
exp.dist <- rexp(pop.size)

shinyServer(function(input, output) {
    output$size <- renderText({
        paste("Sample Size = ", input$sample.size)
    })
    
    output$frequency <- renderText({
        paste("Sample Frequency = ", input$sample.freq)
    })    
    
    output$myhist <- renderPlot({
        # Plot the population and sampling distributions
        sample.size <- input$sample.size
        sample.freq <- input$sample.freq
        # Repeat sample.freq times, each time take a sample from the 
        # population and calculate the mean, store in sampling.means.  
        sampling.means <- c()
        for (i in 1:input$sample.freq){
            exp.sample <- sample(exp.dist, size=input$sample.size)   
            sample.mean <- mean(exp.sample)
            sampling.means <- c(sampling.means, sample.mean)
        }
        par(mfrow = c(1, 2))
        hist(exp.dist, main = "Population Distribution", 
            xlab = "Random Exponentials")
        hist(sampling.means, main = "Sampling Mean Distribution", 
            xlab = "Sampling Means")
    })
})

