library(shiny)
library(readr)
library(magrittr)
library(lubridate)
library(purrr)
library(dplyr)
library(googleAuthR)
library(googleID)

double_days <- function(start, end){
  c(start, end + days(end - start))
}

delete_last_row <- function(df){
  df[1:(nrow(df) - 1),]
}

get_time <- function(){
  gsub(" ", "T", as.character(Sys.time()))
}

date_ <- function(){
  as.Date(integer(0), origin = "1970-01-01")
}

function(input, output, session) {
  #shinyjs::disable("btn")
  
  outbreaks <- read_csv("outbreaks.csv")
  out_avgs <- read_csv("outbreak-avgs.csv")
  isos <- outbreaks$ISO_A2_L1 %>% unique() %>% sample(3)
  outbreaks <- map(isos, ~ outbreaks %>%
                             filter(ISO_A2_L1 == .x) %>%
                             select(TL, cases))
  names(outbreaks) <- isos
  id <- paste(sample(c(letters, LETTERS, 0:9), size = 20, replace = TRUE), collapse = "")
  id <- paste0(id, "?", gsub(" ", "T", as.character(Sys.time())))
  
  script <- c("Intro", "plot_avg", "plot_iso1", "plot_iso2", "plot_iso3", "End")
  state <- 1
  
  result <- map(script, ~ data_frame(Id = character(), Group = character(), Pred_Level = double(),
                            Pred_Date = date_(), Sys_Time = character())
  )
  names(result) <- script
  
  rv = reactiveValues(
    login = FALSE
  )  
  if (glogin) {
    print("in glogin")
    ## Authentication
    accessToken <- callModule(googleAuth, "gauth_login",
                              login_class = "btn btn-primary",
                              logout_class = "btn btn-primary")
    userDetails <- reactive({
      validate(
        need(accessToken(), 
             "You are not logged in.  You must log in to save projections")
      )
      rv$login <- TRUE
      cat("john")
      toggleState("btn")
      
      with_shiny(get_user_info, shiny_access_token = accessToken())
    })
    
    ## Display user's Google display name after successful login
    output$display_username <- renderText({
      validate(
        need(userDetails(), "getting user details")
      )
      paste0("You are logged in as ", userDetails()$displayName)
      # userDetails()$displayName
    })
    
    ## Workaround to avoid shinyaps.io URL problems
    observe({
      if (rv$login) {
        shinyjs::onclick(
          "gauth_login-googleAuthUi",
          shinyjs::runjs(
            "window.location.href = 'https://jmuschelli.shinyapps.io/epi_click';"))
      }
    })
  }
  
  output$plot_avg <- renderPlot({
    if(!is.null(input$click_avg) && input$click_avg$x > max(out_avgs$TL)){
      result$plot_avg <<- add_row(result$plot_avg,
                         Id = id,
                         Group = "avg",
                         Pred_Level = input$click_avg$y,
                         Pred_Date = as.Date(input$click_avg$x, origin = "1970-01-01"),
                         Sys_Time = get_time()
                         )
    }
    
    if(!is.null(input$dblclick_avg)){
      result$plot_avg <<- delete_last_row(result$plot_avg)
    }
    
    result$plot_avg %>%
      select(Pred_Date, Pred_Level) %>%
      rename(TL = Pred_Date, avg = Pred_Level) %>%
      bind_rows(out_avgs) %$%
      plot(TL, avg, xlim = double_days(min(out_avgs$TL), max(out_avgs$TL)))
    abline(v = max(out_avgs$TL), lty = 2)
    result$plot_avg %>%
      select(Pred_Date, Pred_Level) %>%
      rename(TL = Pred_Date, avg = Pred_Level) %>%
      bind_rows(out_avgs) %$%
      lines(lowess(avg ~ as.numeric(TL)), col = 2)
  })
  
  output$plot_iso1 <- renderPlot({
    if(!is.null(input$click_iso1) && input$click_iso1$x > max(outbreaks[[1]]$TL)){
      result$plot_iso1 <<- add_row(result$plot_iso1,
                                  Id = id,
                                  Group = names(outbreaks)[1],
                                  Pred_Level = input$click_iso1$y,
                                  Pred_Date = as.Date(input$click_iso1$x, origin = "1970-01-01"),
                                  Sys_Time = get_time()
      )
    }
    
    if(!is.null(input$dblclick_iso1)){
      result$plot_iso1 <<- delete_last_row(result$plot_iso1)
    }
    
    result$plot_iso1 %>%
      select(Pred_Date, Pred_Level) %>%
      rename(TL = Pred_Date, cases = Pred_Level) %>%
      bind_rows(outbreaks[[1]]) %$%
      plot(TL, cases, xlim = double_days(min(outbreaks[[1]]$TL), max(outbreaks[[1]]$TL)))
    abline(v = max(outbreaks[[1]]$TL), lty = 2)
    result$plot_iso1 %>%
      select(Pred_Date, Pred_Level) %>%
      rename(TL = Pred_Date, cases = Pred_Level) %>%
      bind_rows(outbreaks[[1]]) %$%
      lines(lowess(cases ~ as.numeric(TL)), col = 2)
  })
  
  output$plot_iso2 <- renderPlot({
    if(!is.null(input$click_iso2) && input$click_iso2$x > max(outbreaks[[2]]$TL)){
      result$plot_iso2 <<- add_row(result$plot_iso2,
                                   Id = id,
                                   Group = names(outbreaks)[2],
                                   Pred_Level = input$click_iso2$y,
                                   Pred_Date = as.Date(input$click_iso1$x, origin = "1970-01-01"),
                                   Sys_Time = get_time()
      )
    }
    
    if(!is.null(input$dblclick_iso2)){
      result$plot_iso2 <<- delete_last_row(result$plot_iso2)
    }
    
    result$plot_iso2 %>%
      select(Pred_Date, Pred_Level) %>%
      rename(TL = Pred_Date, cases = Pred_Level) %>%
      bind_rows(outbreaks[[2]]) %$%
      plot(TL, cases, xlim = double_days(min(outbreaks[[2]]$TL), max(outbreaks[[2]]$TL)))
    abline(v = max(outbreaks[[2]]$TL), lty = 2)
    result$plot_iso2 %>%
      select(Pred_Date, Pred_Level) %>%
      rename(TL = Pred_Date, cases = Pred_Level) %>%
      bind_rows(outbreaks[[2]]) %$%
      lines(lowess(cases ~ as.numeric(TL)), col = 2)
  })
  
  output$plot_iso3 <- renderPlot({
    if(!is.null(input$click_iso3) && input$click_iso3$x > max(outbreaks[[3]]$TL)){
      result$plot_iso3 <<- add_row(result$plot_iso3,
                                   Id = id,
                                   Group = names(outbreaks)[3],
                                   Pred_Level = input$click_iso2$y,
                                   Pred_Date = as.Date(input$click_iso1$x, origin = "1970-01-01"),
                                   Sys_Time = get_time()
      )
    }
    
    if(!is.null(input$dblclick_iso3)){
      result$plot_iso3 <<- delete_last_row(result$plot_iso3)
    }
    
    result$plot_iso3 %>%
      select(Pred_Date, Pred_Level) %>%
      rename(TL = Pred_Date, cases = Pred_Level) %>%
      bind_rows(outbreaks[[3]]) %$%
      plot(TL, cases, xlim = double_days(min(outbreaks[[3]]$TL), max(outbreaks[[3]]$TL)))
    abline(v = max(outbreaks[[3]]$TL), lty = 2)
    result$plot_iso3 %>%
      select(Pred_Date, Pred_Level) %>%
      rename(TL = Pred_Date, cases = Pred_Level) %>%
      bind_rows(outbreaks[[3]]) %$%
      lines(lowess(cases ~ as.numeric(TL)), col = 2)
  })
  
  observeEvent(input$btn, {
    if(state < length(script)){
      toggle(script[state])
      toggle(script[state + 1])
    }
    state <<- state + 1
  })
  
}
