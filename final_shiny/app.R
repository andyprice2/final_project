library(shiny)
library(tidyverse)
library(fs)
library(markdown)
library(gganimate)
library(caTools)

# Create the UI, which controls the look of the page.

ui <- fluidPage(
  
  # Application title
  
  titlePanel("MLB Pitcher Raw Stuff"),
  
  # Create a navigation bar (with a title called "Menu") and give it tabs:
  # Four-Seam Fastball and About. The first loads the image created in the setup. The section
  # gives some background.
  
  navbarPage(
    "Menu",
    tabPanel(
      "Four-Seam Fastball",
      sidebarPanel(
        selectInput("fastball_four_seam_pitcher", "Pitcher",
                    c("Justin Verlander" = "Justin Verlander",
                      "Reynaldo Lopez" = "Reynaldo Lopez",
                      "Sean Newcomb" = "Sean Newcomb",
                      "Kevin Gausman" = "Kevin Gausman"))
      ),
      mainPanel(
        tableOutput("test")
      )
    ),
    tabPanel(
      "About",
      mainPanel(
        "This project examines a Major League Baseball pitcher’s “stuff”: stuff, in baseball, refers to the raw power of a pitcher’s repertoire. It doesn’t consider his command or sequencing, but rather how fast his pitches are, how much they move (in every direction), and how many different types of pitches he can throw (fastball, curveball, slider, etc). “Stuff”, for nearly its entire existence, has been a buzzword thrown around by announcers with very little quantification. Now, though, with the introduction of Statcast technology to every MLB stadium, we have three-dimensional measurements of every pitch thrown, and can begin to quantify this historically vague concept of “stuff”. This project explores how much of an effect a pitcher’s velocity and movement affect the traditional results-oriented statistics foundational to sabermetrics. 

        Statcast data, which I pull using Bill Petti’s BaseballR and Carson Sievert’s PitchRx packages in R, has been most accurate the past two years (before that, the data has a large “park factor”, meaning that because of improper calibration or something similar, it was hard to make comparisons between pitches recorded by Statcast at two different stadiums). This was discovered by Jared Cross, founder of Steamer Projections, who helped provide background information for this project. Many thanks to Cross, Petti, and Sievert. 
        
        Each row of this data, which is directly scraped from Statcast’s official website (https://baseballsavant.mlb.com/) using tools in the BaseballR package, corresponds to a pitch thrown in the last two years, and contains variables for a pitch’s horizontal and vertical movement (measured in feet), velocity (measured in miles per hour), and result (this could be, for example, a swinging strike or a ball in play hit at 80 mph, as Statcast also stores data about the exit velocity of batted balls)."
      )
    )
  )
)

# Now create the server, where the shiny app loads in the graphic that I created
# in map.Rmd. Note that it's important to render as an image as opposed to a
# plot (which I initially tried).

server <- function(input, output) {
 
  fastball_four_seam <- readRDS("fastball_four_seam.rds")
  
   output$test <- renderTable({
     fastball_four_seam %>%
       filter(player_name == input$fastball_four_seam_pitcher)
   }, rownames = TRUE)
    
  
}

# Run the application
shinyApp(ui = ui, server = server)
