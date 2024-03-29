
# Libraries ---------------------------------------------------------------

library(shiny)
library(tidyverse)
library(fs)
library(markdown)
library(gganimate)
library(caTools)

# Read in RDSs ------------------------------------------------------------

# Here I read in the RDS created in the randomforest.R script, and create
# variables that store the names of all the pitchers.

fastball_four_seam <- readRDS("fastball_four_seam.rds")
changeup <- readRDS("changeup.rds")
curveball <- readRDS("curveball.rds")
cutter <- readRDS("cutter.rds")
fastball_two_seam <- readRDS("fastball_two_seam.rds")
knuckle_curve <- readRDS("knuckle_curve.rds")
sinker <- readRDS("sinker.rds")
slider <- readRDS("slider.rds")
split_finger <- readRDS("split_finger.rds")


# Start of UI and navbar -------------------------------------------------------------

# Create the UI, which controls the look of the page.

ui <- fluidPage(

  # Application title

  titlePanel("MLB Pitcher Raw Stuff"),

  # Create a navigation bar (with a title called "Menu") and give it tabs:
  # Four-Seam Fastball and About. The first loads the image created in the setup. The section
  # gives some background.

  navbarPage(
    "",

    # About Page --------------------------------------------------------------

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
               among many other measurements. I built a statcast database for 2018 and 2019, starting here 
               because Statcast has been most accurate the past two years (before that, the data has a large “park factor”,
               meaning that because of improper calibration or something of the kind, it was hard to make 
               comparisons between pitches recorded by Statcast at two different stadiums). This was 
               discovered by Jared Cross, founder of Steamer Projections, who helped provide background 
               information for this project. Many thanks to Cross and Petti."),

        tags$p("Then, after cleaning the data and splitting into training and testing sets, 
               I create a new, nested data frame out of this initial Statcast data frame, 
               in which there's a single row for each pitch type (for which there were more than
               10,000 pitches thrown in 2018 and 2019, so no eephus pitch). This data frame has a list column with all the pitches 
               of a given type, which will be used for modeling. This is an important step in the modeling process:
               instead of making one model, there will be one for every type of pitch (so we aren't comparing the movement 
               of a curveball against that of a fastball). This allows for tailoring to each pitch type."),

        tags$p("Now that the data is cleaned and segmented by pitch type, I run a random forest regression on 
               each pitch type, creating probability trees. Unlike the typical tree often used in a random forest analysis, 
               which either predicts of classifies, this produces a probability distribution that takes a pitch's velocity,
               vertical movement, and horizontal movement and compares it to similar pitches, showing the likelihood that a 
               given pitch will result in any given of outcomes (say, a called strike or a double). Go to the pitch tabs and
               you can select a pitcher (one of the 25 pitchers who threw the most of the given pitch in 2019, and it will show you the predicted outcomes for their average pitch (a pitch with their average velocity
               and movement in both directions)."),
        
        # Add in the links, which are explained in the comments in the server section.
        
       uiOutput("github"),
       
       uiOutput("pdf"),
       
       uiOutput("video")
        
        
        
      )
    ),


    # Changeup Output ---------------------------------------------------------

    # Call the changeup plot, which is determined by the user input -- they're
    # given the options of all four pitchers. This will soon be automated instead
    # of entered by hand, which will be infeasible when I expand beyond four
    # players. I have the code, but it was having trouble publishing because some
    # of it was only in my console -- have to go through and figure that problem
    # out.

    tabPanel(
      "Changeup",
      sidebarPanel(
        selectInput(
          "changeup_pitcher", "Pitcher",
          choices = unique(changeup$player_name)
        )
      ),
      mainPanel(
        plotOutput("changeup")
      )
    ),

    # Fastball Output ---------------------------------------------------------

    # Do the same exact thing for fastball.

    tabPanel(
      "Four-Seam Fastball",
      sidebarPanel(
        selectInput(
          "fastball_four_seam_pitcher", "Pitcher",
          choices = unique(fastball_four_seam$player_name)
        )
      ),
      mainPanel(
        plotOutput("fastball_four_seam")
      )
    ),

    # Curveball Output --------------------------------------------------------

    tabPanel(
      "Curveball",
      sidebarPanel(
        selectInput(
          "curveball_pitcher", "Pitcher",
          choices = unique(curveball$player_name)
        )
      ),
      mainPanel(
        plotOutput("curveball")
      )
    ),


    # Cutter Output -----------------------------------------------------------

    tabPanel(
      "Cutter",
      sidebarPanel(
        selectInput(
          "cutter_pitcher", "Pitcher",
          choices = unique(cutter$player_name)
        )
      ),
      mainPanel(
        plotOutput("cutter")
      )
    ),

    # Two-Seam Fastball Output ------------------------------------------------

    tabPanel(
      "Two-Seam Fastball",
      sidebarPanel(
        selectInput(
          "fastball_two_seam_pitcher", "Pitcher",
          choices = unique(fastball_two_seam$player_name)
        )
      ),
      mainPanel(
        plotOutput("fastball_two_seam")
      )
    ),


    # Knuckle Curve Output ----------------------------------------------------

    tabPanel(
      "Knuckle Curve",
      sidebarPanel(
        selectInput(
          "knuckle_curve_pitcher", "Pitcher",
          choices = unique(knuckle_curve$player_name)
        )
      ),
      mainPanel(
        plotOutput("knuckle_curve")
      )
    ),

    # Sinker Output -----------------------------------------------------------

    tabPanel(
      "Sinker",
      sidebarPanel(
        selectInput(
          "sinker_pitcher", "Pitcher",
          choices = unique(sinker$player_name)
        )
      ),
      mainPanel(
        plotOutput("sinker")
      )
    ),

    # Slider Output -----------------------------------------------------------

    tabPanel(
      "Slider",
      sidebarPanel(
        selectInput(
          "slider_pitcher", "Pitcher",
          choices = unique(slider$player_name)
        )
      ),
      mainPanel(
        plotOutput("slider")
      )
    ),

    # Split-finger Output -----------------------------------------------------

    tabPanel(
      "Split-Finger",
      sidebarPanel(
        selectInput(
          "split_finger_pitcher", "Pitcher",
          choices = unique(split_finger$player_name)
        )
      ),
      mainPanel(
        plotOutput("split_finger")
      )
    )
  )
)

# Server ---------------------------------------------------------

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
        title = "Likelihood of Outcomes for Pitcher's Average Four-Seamer",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      scale_y_continuous(labels = scales::percent) +
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
        title = "Likelihood of Outcomes for Pitcher's Average Changeup",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()
  })
  
  # Curveball
  
  output$curveball <- renderPlot({
    curveball %>%
      filter(player_name == input$curveball_pitcher) %>%
      .[, 7:ncol(curveball)] %>%
      gather(key = "outcome", value = "likelihood") %>%
      filter(likelihood > 0.005) %>%
      ggplot(aes(x = reorder(outcome, likelihood), y = likelihood)) +
      geom_col() +
      labs(
        x = "Outcome of Pitch",
        y = "Likelihood of Outcome",
        title = "Likelihood of Outcomes for Pitcher's Average Curveball",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()
  })
  
  # Cutter
  
  output$cutter <- renderPlot({
    cutter %>%
      filter(player_name == input$cutter_pitcher) %>%
      .[, 7:ncol(cutter)] %>%
      gather(key = "outcome", value = "likelihood") %>%
      filter(likelihood > 0.005) %>%
      ggplot(aes(x = reorder(outcome, likelihood), y = likelihood)) +
      geom_col() +
      labs(
        x = "Outcome of Pitch",
        y = "Likelihood of Outcome",
        title = "Likelihood of Outcomes for Pitcher's Average Cutter",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()
  })
  
  # Fastball Two-Seam
  
  output$fastball_two_seam <- renderPlot({
    fastball_two_seam %>%
      filter(player_name == input$fastball_two_seam_pitcher) %>%
      .[, 7:ncol(fastball_two_seam)] %>%
      gather(key = "outcome", value = "likelihood") %>%
      filter(likelihood > 0.005) %>%
      ggplot(aes(x = reorder(outcome, likelihood), y = likelihood)) +
      geom_col() +
      labs(
        x = "Outcome of Pitch",
        y = "Likelihood of Outcome",
        title = "Likelihood of Outcomes for Pitcher's Average Two-Seamer",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()
  })
  
  # Knuckle Curve 
  
  output$knuckle_curve <- renderPlot({
    knuckle_curve %>%
      filter(player_name == input$knuckle_curve_pitcher) %>%
      .[, 7:ncol(knuckle_curve)] %>%
      gather(key = "outcome", value = "likelihood") %>%
      filter(likelihood > 0.005) %>%
      ggplot(aes(x = reorder(outcome, likelihood), y = likelihood)) +
      geom_col() +
      labs(
        x = "Outcome of Pitch",
        y = "Likelihood of Outcome",
        title = "Likelihood of Outcomes for Pitcher's Average Knuckle Curve",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()
  })
  
  # Sinker
  
  output$sinker <- renderPlot({
    sinker %>%
      filter(player_name == input$sinker_pitcher) %>%
      .[, 7:ncol(sinker)] %>%
      gather(key = "outcome", value = "likelihood") %>%
      filter(likelihood > 0.005) %>%
      ggplot(aes(x = reorder(outcome, likelihood), y = likelihood)) +
      geom_col() +
      labs(
        x = "Outcome of Pitch",
        y = "Likelihood of Outcome",
        title = "Likelihood of Outcomes for Pitcher's Average Sinker",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()
  })
  
  # Slider
  
  output$slider <- renderPlot({
    slider %>%
      filter(player_name == input$slider_pitcher) %>%
      .[, 7:ncol(slider)] %>%
      gather(key = "outcome", value = "likelihood") %>%
      filter(likelihood > 0.005) %>%
      ggplot(aes(x = reorder(outcome, likelihood), y = likelihood)) +
      geom_col() +
      labs(
        x = "Outcome of Pitch",
        y = "Likelihood of Outcome",
        title = "Likelihood of Outcomes for Pitcher's Average Slider",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()
  })
  
  # Split Finger 
  
  output$split_finger <- renderPlot({
    split_finger %>%
      filter(player_name == input$split_finger_pitcher) %>%
      .[, 7:ncol(split_finger)] %>%
      gather(key = "outcome", value = "likelihood") %>%
      filter(likelihood > 0.005) %>%
      ggplot(aes(x = reorder(outcome, likelihood), y = likelihood)) +
      geom_col() +
      labs(
        x = "Outcome of Pitch",
        y = "Likelihood of Outcome",
        title = "Likelihood of Outcomes for Pitcher's Average Splitter",
        subtitle = "Only outcomes shown are those with > 0.5 % chance of occuring"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()
  })
  
  # Here I post a hyperlink to the github repo so people can check out the
  # underlying code.
  
  urlGithub <- a("Github Repo", href="https://github.com/andyprice2/final_project")
  output$github <- renderUI({
    tagList("Link to ", urlGithub)
  })
    
  # And here I give the hyperlink to download the PDF. I preferred not to have
  # it appear directly on the about page because the two are semi-repetitive.
  # This way if the viewer is especially interested, though, they can read it.
    
  urlPDF <- a("USCLAP Submission PDF", href="https://github.com/andyprice2/final-project-pdf/raw/master/USCLAP.pdf")
  output$pdf <- renderUI({
    tagList("Download ", urlPDF)
  })
  
  # Link to video overview.
  
  urlVideo <- a("Video Overview", href="https://www.youtube.com/watch?v=mfU_9pugIfM")
  output$video <- renderUI({
    tagList("Link to ", urlVideo)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
