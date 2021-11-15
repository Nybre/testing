Dashboard_file_UI <- function(id) {
  ns <- NS(id)
  tagList( 
    
    fluidRow(
      value_box("",value = HTML("<b> Frost Calculator </b>"),color = "orange", 
                width = 16, size = "tiny")
    ), 
    column(width = 6, 
           box(title="Frost Probability", color = "blue", width = 6,    
               withSpinner(plotlyOutput(ns("probability_plot")) )
           ) 
    ),
    column(width = 6, 
           box(title="Temperature Seasonality", color = "green", width = 6,    
               withSpinner(plotlyOutput(ns("history_plot")) ) )
    ),
    
    column(width = 4,
           box(title = "Control Panel", color = "purple", width =4,
               collapsible = T,   
               tags$div(tags$div(HTML("<b><h3>Latitude :</h3></b> ")),
                        numeric_input(ns('latitude_id'),label = '',value = 50,style = "width: 100%;")),
               br(),
               tags$div(tags$div(HTML("<b><h3>Longitude :</h3></b> ")),
                        numeric_input(ns('longitude_id'), label = '',6,style = "width: 100%;")),br(),
               value_box_output(ns("infomessage")),
               tags$div(tags$div(
                 br(),
                 hr(),
                 HTML("<h4>Some explaination</h4> "), 
                 HTML("<p>Please provide latitude and longitude of the location you want to analyse. </p>
                      <p>Data request will be put in the queue. The treatmeent can take up to 15 min but will be cached</p>
                      <p>So you can initial a request and come back later with teh same request to see the result !</p>
                      <p>If you have no idea what to put, try latitude 50, longitude 6 (this is north of Luxembourg, near Clervaux)</p>")
                 
                )
              )
         )  
    ) 
  )
}

Dashboard_file <- function(input, output, session, pool) { 
  #source user inputs
  latitude = reactive(input$latitude_id)
  longitude = reactive(input$longitude_id)
   
  
  infomessage <- reactiveVal("")
  
  output$infomessage <- renderValueBox({infomessage()})
  
  # Probability
  output$probability_plot <- renderPlotly({
    latitude = latitude()
    longitude = longitude()
    
    if (is.na(latitude)) {
      infomessage("Please provide latitude")
    } else if (latitude > 90) {
      infomessage("Latitude should be lower than 90")
    } else if (latitude < -90) {
      infomessage("Latitude should be higer than -90")
    } else {
      if (is.na(longitude)) {
        infomessage("Please provide longitude")
      } else if (longitude > 180) {
        infomessage("Longitude should be lower than 180")
      } else if (longitude < -180) {
        infomessage("Longitude should be higer than -180")
      } else {
        infomessage("Values are OK ! Computation in progress")
        
        
        tryCatch(
          { 
            probalilbities_with_date = probability(latitude, longitude)
            
            infomessage("Computation finished !")
            
            dates = probalilbities_with_date$date
            probability = probalilbities_with_date$freeze_probability*100
            plot_output_1 = qplot(dates, probability, colour = probability,
                                  main = "Frost probability",
                                  ylab = "Probability of frost before last date (in %)",
                                  xlab = "Dates")
            
            ggplotly(plot_output_1)
            
         
          },
          error=function(cond) {
            infomessage("There was an error. Please retry later")
            print("There was an error")
            print(cond)
            # Choose a return value in case of error
            return(NA)
          },
          warning=function(cond) {
            print("There was an warinig")
            print(cond)
            # Choose a return value in case of warning
            return(NULL)
          })
        
        
      }
    }
  })
  
  
  # History
  output$history_plot <- renderPlotly({
    latitude = latitude()
    longitude = longitude()
    
    if (is.na(latitude)) {
      #infomessage("Please provide latitude")
    } else if (latitude > 90) {
      #infomessage("Latitude should be lower than 90")
    } else if (latitude < -90) {
      #infomessage("Latitude should be higer than -90")
    } else {
      if (is.na(longitude)) {
        #infomessage("Please provide longitude")
      } else if (longitude > 180) {
        #infomessage("Longitude should be lower than 180")
      } else if (longitude < -180) {
        #infomessage("Longitude should be higer than -180")
      } else {
        #infomessage("Values are OK ! Computation in progress")
        
        
        tryCatch(
          { 
            data_periode = history(latitude, longitude)
            
            #infomessage("Computation finished !")
            
            ymin = min(data_periode$median, data_periode$quantile1, data_periode$quantile2, na.rm=TRUE)
            ymax = max(data_periode$median, data_periode$quantile1, data_periode$quantile2, na.rm=TRUE)
            
            period = data_periode$period
            median = data_periode$median
            quantile1 = data_periode$quantile1
            quantile2 = data_periode$quantile2
            freeze_temperature = 1
            plot_output_2 = qplot(period, median, colour =  I("red"),
                                  main = "Temperature seasonality",
                                  ylab = "2m temperature",
                                  xlab = "Dates", ylim = c(ymin,ymax), geom ='line')+ geom_point(colour ="blue")+
              geom_line(aes(period, quantile1),linetype = 'dotted', colour = 'brown')+
              geom_line(aes(period, quantile2), linetype = 'dotted', colour = 'brown') +
              geom_hline(yintercept = freeze_temperature)
            ggplotly(plot_output_2)
            
          },
          error=function(cond) {
            infomessage("There was an error. Please retry later")
            print("There was an error")
            print(cond)
            # Choose a return value in case of error
            return(NA)
          },
          warning=function(cond) {
            print("There was an warinig")
            print(cond)
            # Choose a return value in case of warning
            return(NULL)
          })
        
        
      }
    }
  })
  
  output$title_text <- renderText({ "Frost calculation" })
  
} 