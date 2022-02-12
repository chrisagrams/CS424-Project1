library(shiny)
library(lubridate)
library(ggplot2)

halsted <- read.csv("halsted.csv", header=TRUE)
halsted$date <- ymd(halsted$date)
ohare <- read.csv("ohare.csv", header=TRUE)
ohare$date <- ymd(ohare$date)
year_options <- rep(2001:2021)
year_options <- append(year_options, "2001-2021")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    html {
                      height:100%;
                    }
                    body {
                      height: 100%;
                    }
                    "))
  ),
  style="height: 100%;",
  titlePanel("CS 424 Project 1"),
  actionButton("about", "About"),
  
  fluidRow(
    style="height: 100%;",
    column(6,
      style="border-radius: 16px;
      background-color: #f5f5f5;
      filter: drop-shadow(3px 1px 4px lightgrey);
      padding: 25px;
      margin: 25px;
      width: calc(50% - 50px);
      display: flex;
      min-height: calc(100% - 100px);",
      column(4,
        style="margin: auto;",
        h1("UIC-Halsted"),
        hr(style="border-color: grey;"),
        selectInput("uicYear", "Year:", 
                    year_options,
                    selected="2001-2021"),
        radioButtons("uicRange", "Range:",
                     c("Day", "Month", "Week", "Year"), selected = "Year")
  
      ),
  
      column(8,
        plotOutput(outputId = "uicPlot"),
        conditionalPanel(
          condition = "input.uicRange == 'Day'",
          dateRangeInput("uicDateRange", "Date range to view:", start="2001-01-01", end="2001-12-31")
        ),
        tableOutput(outputId = "uicTab"),
  
      )
    ),
    column(6,
        style="border-radius: 16px;
        background-color: #f5f5f5;
        filter: drop-shadow(-3px 1px 4px lightgrey);
        padding: 25px;
        margin: 25px;
        width: calc(50% - 50px);
        display: flex;
        min-height: calc(100% - 100px);",
        column(8,
              plotOutput(outputId = "oharePlot"),
              conditionalPanel(
                condition = "input.ohareRange == 'Day'",
                dateRangeInput("ohareDateRange", "Date range to view:", start="2001-01-01", end="2001-12-31")
              ),
              tableOutput(outputId = "ohareTab"),
        ),
       
        column(4, 
              style="margin: auto;",
              
              h1("O'Hare Airport"),
              hr(style="border-color: grey;"),
              selectInput("ohareYear", "Year:", 
                          year_options,
                          selected="2001-2021"),
              radioButtons("ohareRange", "Range:",
                           c("Day", "Month", "Week", "Year"), selected = "Year")
        )
    )
  )
)

server <- function(input, output, session) {

  output$uicPlot <- renderPlot({
    data <- halsted
    if(input$uicRange == "Year") {
      year_sum <- setNames(aggregate(x=data$rides, by=list(year(data$date)), FUN=sum), c("Year", "Riders"))
      ggplot(year_sum, aes(x=Year, y=Riders/1000000)) +
        geom_bar(stat="identity", fill="steelblue") +
        labs(
          title= paste("UIC-Halsted yearly ridership since 2001"),
          x ="Year",
          y = "Riders (in millions)") +
        theme(plot.background = element_rect(fill = "#f5f5f5"), text = element_text(size = 20))
    }
    else if (input$uicRange == "Month") {
      data_sub <- subset(data, year(data$date) == input$uicYear)
      month_sum <- setNames(aggregate(x=data_sub$rides, by=list(month(data_sub$date, label = TRUE)), FUN=sum), c("Month", "Riders"))
      ggplot(month_sum, aes(x=Month, y=Riders/1000)) +
        geom_bar(stat="identity", fill="steelblue") +
        labs(
          title= paste("UIC-Halsted monthly ridership in", input$uicYear),
          x ="Month",
          y = "Riders (in thousands)") +
        theme(plot.background = element_rect(fill = "#f5f5f5"), text = element_text(size = 20))
    }
    else if (input$uicRange == "Week") {
      data_sub <- subset(data, year(data$date) == input$uicYear)
      day_sum <- setNames(aggregate(x=data_sub$rides, by=list(wday(data_sub$date, label = TRUE)), FUN=sum), c("Day", "Riders"))
      ggplot(day_sum, aes(x=Day, y=Riders/1000)) +
        geom_bar(stat="identity", fill="steelblue") +
        labs(
          title=paste("UIC-Halsted day of the week ridership in", input$uicYear),
          x ="Day of week",
          y = "Riders (in thousands)") +
        theme(plot.background = element_rect(fill = "#f5f5f5"), text = element_text(size = 20))
    }
    else if (input$uicRange == "Day") {
      data_sub <- subset(data, year(data$date) == input$uicYear)
      ggplot(data_sub, aes(x=date, y=rides)) +
        geom_bar(stat="identity", fill="steelblue") +
        labs(
          title=paste("UIC-Halsted daily ridership in", input$uicYear),
          x ="Date",
          y = "Riders") +
        scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0)) +
        theme(plot.background = element_rect(fill = "#f5f5f5"), text = element_text(size = 20))
    }
    })
    output$oharePlot <- renderPlot({
      data <- ohare
      if(input$ohareRange == "Year") {
        year_sum <- setNames(aggregate(x=data$rides, by=list(year(data$date)), FUN=sum), c("Year", "Riders"))
        ggplot(year_sum, aes(x=Year, y=Riders/1000000)) +
          geom_bar(stat="identity", fill="steelblue") +
          labs(
            title= paste("O'Hare yearly ridership since 2001"),
            x ="Year",
            y = "Riders (in millions)") +
          theme(plot.background = element_rect(fill = "#f5f5f5"), text = element_text(size = 20))
      }
      else if (input$ohareRange == "Month") {
        data_sub <- subset(data, year(data$date) == input$ohareYear)
        month_sum <- setNames(aggregate(x=data_sub$rides, by=list(month(data_sub$date, label = TRUE)), FUN=sum), c("Month", "Riders"))
        ggplot(month_sum, aes(x=Month, y=Riders/1000)) +
          geom_bar(stat="identity", fill="steelblue") +
          labs(
            title= paste("O'Hare monthly ridership in", input$ohareYear),
            x ="Month",
            y = "Riders (in thousands)") +
          theme(plot.background = element_rect(fill = "#f5f5f5"), text = element_text(size = 20))
      }
      else if (input$ohareRange == "Week") {
        data_sub <- subset(data, year(data$date) == input$ohareYear)
        day_sum <- setNames(aggregate(x=data_sub$rides, by=list(wday(data_sub$date, label = TRUE)), FUN=sum), c("Day", "Riders"))
        ggplot(day_sum, aes(x=Day, y=Riders/1000)) +
          geom_bar(stat="identity", fill="steelblue") +
          labs(
            title=paste("O'Hare day of the week ridership in", input$ohareYear),
            x ="Day of week",
            y = "Riders (in thousands)") +
          theme(plot.background = element_rect(fill = "#f5f5f5"), text = element_text(size = 20))
      }
      else if (input$ohareRange == "Day") {
        data_sub <- subset(data, year(data$date) == input$ohareYear)
        ggplot(data_sub, aes(x=date, y=rides)) +
          geom_bar(stat="identity", fill="steelblue") +
          labs(
            title=paste("O'Hare daily ridership in", input$ohareYear),
            x ="Date",
            y = "Riders") +
          scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0)) +
          theme(plot.background = element_rect(fill = "#f5f5f5"), text = element_text(size = 20))
      }
    })
    output$uicTab <- renderTable({
      data <- halsted
      if(input$uicRange == "Year") 
      {
        data$date <- ymd(data$date)
        year_sum <- setNames(aggregate(x=data$rides, by=list(year(data$date)), FUN=sum), c("Year", "Riders"))
        
        year_sum_copy <- year_sum
        year_sum_copy$Riders <- prettyNum(year_sum$Riders, big.mark=",")
        year_sum_copy$Year <- as.integer(year_sum$Year)
        year_sum_copy
      }
      else if(input$uicRange == "Month")
      {
        data_sub <- subset(data, year(data$date) == input$uicYear)
        month_sum <- setNames(aggregate(x=data_sub$rides, by=list(month(data_sub$date, label = TRUE)), FUN=sum), c("Month", "Riders"))
        month_sum_copy <- month_sum
        month_sum_copy$Riders <- prettyNum(month_sum$Riders, big.mark=",")
        month_sum_copy
      }
      else if(input$uicRange == "Week")
      {
        data_sub <- subset(data, year(data$date) == input$uicYear)
        day_sum <- setNames(aggregate(x=data_sub$rides, by=list(wday(data_sub$date, label = TRUE)), FUN=sum), c("Day", "Riders"))
        day_sum_copy <- day_sum
        day_sum_copy$Riders <- prettyNum(day_sum$Riders, big.mark = ",")
        day_sum_copy
      }
      else if(input$uicRange == "Day")
      {
        data_sub <- subset(data, year(data$date) == input$uicYear)
        data_sub <- data_sub[, c('date', 'rides')]
        data_sub_sub <- data_sub[data_sub$date %in% input$uicDateRange[1]:input$uicDateRange[2], ]
        data_sub_sub <- setNames(data_sub_sub, c("Date", "Riders"))
        data_sub_sub <- data_sub_sub[order(data_sub_sub$Date),]
        data_sub_sub$Date <- format(data_sub_sub$Date,'%Y-%m-%d')
        data_sub_sub$Riders <- prettyNum(data_sub_sub$Riders, big.mark=",")
        data_sub_sub
      }
    })
    
    output$ohareTab <- renderTable({
      data <- ohare
      if(input$ohareRange == "Year") 
      {
        data$date <- ymd(data$date)
        year_sum <- setNames(aggregate(x=data$rides, by=list(year(data$date)), FUN=sum), c("Year", "Riders"))
        
        year_sum_copy <- year_sum
        year_sum_copy$Riders <- prettyNum(year_sum$Riders, big.mark=",")
        year_sum_copy$Year <- as.integer(year_sum$Year)
        year_sum_copy
      }
      else if(input$ohareRange == "Month")
      {
        data_sub <- subset(data, year(data$date) == input$ohareYear)
        month_sum <- setNames(aggregate(x=data_sub$rides, by=list(month(data_sub$date, label = TRUE)), FUN=sum), c("Month", "Riders"))
        month_sum_copy <- month_sum
        month_sum_copy$Riders <- prettyNum(month_sum$Riders, big.mark=",")
        month_sum_copy
      }
      else if(input$ohareRange == "Week")
      {
        data_sub <- subset(data, year(data$date) == input$ohareYear)
        day_sum <- setNames(aggregate(x=data_sub$rides, by=list(wday(data_sub$date, label = TRUE)), FUN=sum), c("Day", "Riders"))
        day_sum_copy <- day_sum
        day_sum_copy$Riders <- prettyNum(day_sum$Riders, big.mark = ",")
        day_sum_copy
      }
      else if(input$ohareRange == "Day")
      {
        data_sub <- subset(data, year(data$date) == input$ohareYear)
        data_sub <- data_sub[, c('date', 'rides')]
        data_sub_sub <- data_sub[data_sub$date %in% input$ohareDateRange[1]:input$ohareDateRange[2], ]
        data_sub_sub <- setNames(data_sub_sub, c("Date", "Riders"))
        data_sub_sub <- data_sub_sub[order(data_sub_sub$Date),]
        data_sub_sub$Date <- format(data_sub_sub$Date,'%Y-%m-%d')
        data_sub_sub$Riders <- prettyNum(data_sub_sub$Riders, big.mark=",")
        data_sub_sub
      }
    })
  
    observeEvent(input$uicYear, {
      input_selected <- input$uicRange
      uic_labels <- if(input$uicYear == "2001-2021"){
         c("Year")
      } else {
         c("Day", "Month", "Week", "Year")
      }
      updateRadioButtons(session, "uicRange", choices=uic_labels, selected=input_selected)
    
      
      if(input$uicYear != "2001-2021")
      {
        updateDateRangeInput(session, "uicDateRange", start = ymd(paste(input$uicYear, "-01-01")),
                                                      end = ymd(paste(input$uicYear, "-01-31")),
                                                      min = ymd(paste(input$uicYear, "-01-01")),
                                                      max = ymd(paste(input$uicYear, "-12-31"))
                             )
      }
      
    })
    observeEvent(input$ohareYear, {
      input_selected <- input$ohareRange
      ohare_labels <- if(input$ohareYear == "2001-2021"){
        c("Year")
      } else {
        c("Day", "Month", "Week", "Year")
      }
      updateRadioButtons(session, "ohareRange", choices=ohare_labels, selected=input_selected)
      
      if(input$ohareYear != "2001-2021")
      {
        updateDateRangeInput(session, "ohareDateRange", start = ymd(paste(input$ohareYear, "-01-01")),
                             end = ymd(paste(input$ohareYear, "-01-31")),
                             min = ymd(paste(input$ohareYear, "-01-01")),
                             max = ymd(paste(input$ohareYear, "-12-31"))
        )
      }
      
    })
    observeEvent(input$about, {
      showModal(modalDialog(
        title = "About this app",
        h4("Application written by Chris Grams for CS 424."),
        p("This application was designed to quickly visualize and compare trends on the UIC-Halsted, O'Hare Airport, and Forest Park CTA Blue Line stops. Data is imported and processed through R using lubridate and ggplot2 packages. Interface is designed and served through Shiny server."),
        a("Data provided by Chicago Transit Authority:", href = "https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
        tags$blockquote(cite="Chicago Transit Authority", "On the rail system, a customer is counted as an \"entry\" each time he or she passes through a turnstile to enter a station.  Customers are not counted as \"entries\" when they make a \"cross-platform\" transfer from one rail line to another, since they don't pass through a turnstile. Where the number given for rail is in \"boardings,\" what's presented is a statistically valid estimate of the actual number of boardings onto the rail system. "),
        easyClose = TRUE
      ))
    })

}

shinyApp(ui = ui, server = server)

