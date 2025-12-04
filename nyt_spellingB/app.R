
library(renv)
library(shiny)
library(lexicon)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MAIN TITLE"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
        ),

        # Main panel
        mainPanel(
           #plotOutput("distPlot")
        )
    )
)

# Define server logic 
server <- function(input, output) {
  
  var_dict <- lexicon::grady_augmented %>%
    .[!grepl(pattern = "[[:punct:]]", 
                 x = ., 
                 ignore.case = T)] %>%
    .[nchar(.) > 3]
  
  req.ltr  <- "h"
  opt.ltrs <- "ampuct"
  not.ltrs <- letters[!letters %in% 
                        c(req.ltr,
                          unlist(strsplit(opt.ltrs,split="")))]
  
  # must have
  # can have
  # cannot have
  temp.words <- var_dict[!var_dict %in% grep(pattern = paste(not.ltrs,sep = "|",collapse="|"), 
       x = var_dict, ignore.case = T, value = T)] %>%
    grep("h", ., value = T)
  
  #temp.pangrams
  
  temp.ul <- strsplit(temp.words,"") %>%
    lapply(., unique) %>%
    lapply(., length) %>%
    unlist()
  
  temp.pangrams <- temp.words[temp.ul == 7]
  
  table(temp.ul)
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
