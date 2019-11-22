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
    ),

    tabPanel(
      "About",
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
               each pitch type, creating probability trees. Unlike the typical tree used in a random forest analysis, 
               which "),


        tags$h2("Rough-ness")
      )
    )
  )
)



# Here I read in the RDS created in the randomforest.R script, and create variables that store the names of all the pitchers mention

fastball_four_seam <- readRDS("fastball_four_seam.rds")
changeup <- readRDS("changeup.RDS")

fastball_choices <- setNames(fastball_four_seam$player_name, fastball_four_seam$player_name)
changeup_choices <- setNames(changeup$player_name, changeup$player_name)

# Now create the server, where the shiny app loads in the graphic that I created
# in map.Rmd. Note that it's important to render as an image as opposed to a
# plot (which I initially tried).

server <- function(input, output) {
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
