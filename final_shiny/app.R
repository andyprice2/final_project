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
      "About",
      
   # Add an about page that uses html tags to style. P means paragraph, h means
   # header, and the header numbers control size!
   
      mainPanel(
        tags$h1("About This Project"),
        
        tags$h2("Background"),
        
        tags$p("This project aims to project pitch outcomes based on pitcher 'stuff'. Stuff, in baseball, 
               refers to the raw power of a pitcher’s repertoire. It doesn’t consider his command or sequencing, 
               but rather how fast his pitches are, how much they move (in every direction), and how many 
               different types of pitches he can throw (fastball, curveball, slider, etc). “Stuff”, for nearly 
               its entire existence, has been a buzzword thrown around by announcers with very little quantification. 
               Now, though, with the introduction of Statcast technology to every MLB stadium, we have three-dimensional 
               measurements of every pitch thrown, and can begin to quantify this historically vague concept of “stuff”. 
               This project explores how much of an effect a pitcher’s velocity and movement affect the traditional 
               results-oriented statistics foundational to sabermetrics."),
        
        tags$h2("Methodology"),
        
        tags$p("I start by scraping individual pitch-level data directly from baseball.savant.com. To
               do this I use Bill Petti’s BaseballR package. Baseball Savant stores Statcast data; 
               Statcast records the velocity and movement in both the x and y axes of every pitch, 
               among many other measurements. I built a statcast database for 2018, starting here 
               because Statcast has been most accurate the past two years (before that, the data has a large “park factor”,
               meaning that because of improper calibration or something of the kind, it was hard to make 
               comparisons between pitches recorded by Statcast at two different stadiums). This was 
               discovered by Jared Cross, founder of Steamer Projections, who helped provide background 
               information for this project. Many thanks to Cross and Petti."),
        
        tags$p("Then, after cleaning the data and splitting into training and testing sets, 
               I create a new, nested data frame out of this initial Statcast data frame, 
               in which there's a single row for each pitch type (for which there were more than
               10,000 pitches thrown in 2018, so no eephus pitch). This data frame has a list column with all the pitches 
               of a given type, which will be used for modeling. This is an important step in the modeling process:
               instead of making one model, there will be one for every type of pitch (so we aren't comparing the movement 
               of a curveball against that of a fastball). This allows for tailoring to each pitch type."),
        
        tags$p("Now that the data is cleaned and segmented by pitch type, I run a random forest regression on 
               each pitch type, creating probability trees. Unlike the typical tree often used in a random forest analysis, 
               which either predicts of classifies, this produces a probability distribution that takes a pitch's velocity,
               vertical movement, and horizontal movement and compares it to similar pitches, showing the likelihood that a 
               given pitch will result in any given of outcomes (say, a called strike or a double). Go to the fastball or
               changeup tabs and you can select a pitcher (one of the four pitchers who threw the most of the given pitch in
               2018, and it will show you the predicted outcomes for their average pitch (a pitch with their average velocity
               and movement in both directions)."),
        
        tags$h2("Rough-ness"),
        
        tags$p("This is a rough draft, which means it's almost where I want it to be, but needs some tweaks. These won't
               be hard to add, since now I have a fully automated workflow that travels from scraping the data to building
               the models to building the shiny app. This has been the bulk of my focus up until now, and now that it's 
               complete I can focus on everything else. For example, right now this only uses 2018 data, because that was
               already enough to make my computer run incredibly slowly. For the final draft, I'll simply change my database
               building functions to include 2019 data. Additionally, I have the code for building all 9 models, but there 
               are only 2 in the shiny because I wanted to focus on finishing the workflow, which will make adding the
               next 7 quite easy (only a matter of debugging). The models themselves need a little work, which will mostly
               consist of changing their parameters/what they predict. Right now I'm using the statcast event descriptions (
               although I've modified them to count walks as balls and strikeouts as strikes, since the ball-strike count 
               should have no effect on the models). But there are still lots of parameters that are noise, not signal, like 
               events that have little to do with pitcher stuff, like stolen bases, catcher's interferences, etcetera.
               Adding more data (2019) and getting rid of these sources of noise will be pretty simple tasks but will drastically
               improve the models' brier scores. This is my main focus, but I will also make everything look a little prettier
               after its in its final final form (color will go a long way toward making this look nicer).")
        
               )
               ),
   
   # Call the changeup plot, which is determined by the user input -- they're
   # given the options of all pitchers, which is determined by changeup_choices
   # (created further below).
   
    tabPanel(
      "Changeup",
      sidebarPanel(
        selectInput(
          "changeup_pitcher", "Pitcher", changeup_choices
        )
      ),
      mainPanel(
        plotOutput("changeup")
      )
    ),

  # Do the same exact thing for fastball.
  
    tabPanel(
      "Four-Seam Fastball",
      sidebarPanel(
        selectInput(
          "fastball_four_seam_pitcher", "Pitcher", fastball_choices
        )
      ),
      mainPanel(
        plotOutput("fastball_four_seam")
      )
    )
  )
)

# Here I read in the RDS created in the randomforest.R script, and create
# variables that store the names of all the pitchers.

fastball_four_seam <- readRDS("fastball_four_seam.rds")
changeup <- readRDS("changeup.RDS")

fastball_choices <- setNames(fastball_four_seam$player_name, fastball_four_seam$player_name)
changeup_choices <- setNames(changeup$player_name, changeup$player_name)

# Now create the server, where the shiny app creates the plots. Should
# eventually switch to pivot_wider. Coord flip because the names on the x-axis
# were overlapping.

server <- function(input, output) {
  
# First, make fastball plot.
    
  output$fastball_four_seam <- renderPlot({
    fastball_four_seam %>%
      filter(player_name == input$fastball_four_seam_pitcher) %>%
      .[, 7:ncol(fastball_four_seam)] %>%
      gather(key = "outcome", value = "likelihood") %>%
      filter(likelihood > 0.005) %>%
      ggplot(aes(x = reorder(outcome, likelihood), y = likelihood)) +
      geom_col() +
      labs(
        x = "Outcome of Pitch",
        y = "Likelihood of Outcome",
        title = "Likelihood of Outcomes for Pitcher's Average Fastball",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      coord_flip()
  })
  

  # Do the same thing for changeup.

  output$changeup <- renderPlot({
    changeup %>%
      filter(player_name == input$changeup_pitcher) %>%
      .[, 7:ncol(changeup)] %>%
      gather(key = "outcome", value = "likelihood") %>%
      filter(likelihood > 0.005) %>%
      ggplot(aes(x = reorder(outcome, likelihood), y = likelihood)) +
      geom_col() +
      labs(
        x = "Outcome of Pitch",
        y = "Likelihood of Outcome",
        title = "Likelihood of Outcomes for Pitcher's Average Fastball",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      coord_flip()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
