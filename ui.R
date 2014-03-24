library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Gaussian Process example"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("noise", 
                "Observation noise:", 
                min = 0.01, max = 1, value = 1, step= 0.01),
    sliderInput("length_scale", 
                "Length scale:", 
                min = 0.1, max = 15, value = 0.5, step= 0.01),
    sliderInput("sigma_mag", 
                "Magnitude:", 
                min = 0.1, max = 10, value = 0.5, step= 0.01),
    sliderInput("n_samples", 
                "Number of functions to draw:", 
                min=1, max=100, value=1),
    sliderInput("n_obs", 
                "Number of observed samples", 
                min=1, max=100, value=10)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("GaussianProcess"),
    plotOutput("covariance")
  )
))
