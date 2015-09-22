shinyUI(fluidPage(
    titlePanel("Central Limit Theorem Demo"),
    
    sidebarLayout(
        position = "right",
        
        sidebarPanel( 
            p("This app allows you to see the Central Limit Theorem (CLT) in 
                action."),
            p("In a nutshell, CLT says 'The means calculated from 
                large enough samples will be normally distributed, even if 
                the population from which the samples are drawn may not be
                normally distributed'. That's exactly our situation. As 
                you can see on the left, we have an exponential distribution
                as our population."),
            p("As you adjust the sample size and sample frequeny, you'll
                notice the normal pattern in our sampling distribution."),
            sliderInput(inputId = "sample.size", 
                        label = "Sample Size",
                        min = 0, max = 100, value = 1, step = 5),
            sliderInput(inputId = "sample.freq", 
                        label = "Sample Frequency",
                        min = 0, max = 1000, value = 100, step = 100)
        ),
        
        mainPanel(strong("Current Selection:"),
                  textOutput("size"),
                  textOutput("frequency"),
                  plotOutput(outputId = "myhist"),
                  br(),
                  div("Developed & Maintained by George Liu. 
                    All rights reserved.", style = "color:blue")
                  
        )
    )
))