library(shiny)
library(shinyjs)
library(markdown)
library(googleAuthR)
library(googleID)

fluidPage(
  useShinyjs(),

  titlePanel("Predict Cholera"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      if(glogin){googleAuthUI("gauth_login")} else {NULL},
      br(),
      textOutput("display_username"),
      disabled(actionButton("btn", "Next"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      div(id="Intro",
        includeMarkdown("intro.md")
      ),
      hidden(plotOutput("plot_avg", click = "click_avg", dblclick = "dblclick_avg")),
      hidden(plotOutput("plot_iso1", click = "click_iso1", dblclick = "dblclick_iso1")),
      hidden(plotOutput("plot_iso2", click = "click_iso2", dblclick = "dblclick_iso2")),
      hidden(plotOutput("plot_iso3", click = "click_iso3", dblclick = "dblclick_iso3")),
      hidden(div(id="End",
          includeMarkdown("end.md")
      ))
    )
  )
)
