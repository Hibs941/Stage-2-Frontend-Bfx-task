library(shiny)

library(ggplot2)

library(plotly)

ui <- fluidPage(

 

  tags$style(HTML("

        .btn-custom {

            background-color: #007bff; /\* Blue color \*/

            color: white;

            border: none;

            padding: 10px 20px;

            font-size: 16px;

            border-radius: 4px;

            cursor: pointer;

        }

        .btn-custom:hover {

            background-color: #0056b3; /\* Darker blue color on hover \*/

        }body {

                background-color: #F3E5F5;

            }

            .shiny-output-error {

                color: red;

            }

            .title-box {

                background-color: #4A148C;

                padding: 20px;

                text-align: center;

                border-radius: 10px;

                color: white;

                margin-bottom: 20px;

            }

            .title-box h1 {

                margin: 0;

                font-size: 32px;

            }

    ")),

  

  # Title in Header Box

  div(class = "title-box",

      h1("Laboratory Reagent Prepation Calculators")), 

  sidebarLayout(

    sidebarPanel(

      # Tabs for different calculators

      tabsetPanel(

        id = 'tabs',

        tabPanel("Serial Dilution",

                 numericInput("init\_conc", "Initial Concentration (C1)", value = NULL),

                 numericInput("final\_conc", "Final Concentration (C2)", value = NULL),

                 numericInput("dilution\_factor", "Dilution Factor", value = 2),

                 numericInput("total\_volume", "Total Volume", value = NULL),

                 selectInput("volume\_unit", "Volume Unit", 

                             choices = c("mL" = "mL", "µL" = "µL", "L" = "L"), selected = "mL"),

                 actionButton("calc\_serial", "Calculate Serial Dilution", class = "btn-custom"),

                 actionButton("clear\_serial", "Clear", class = "btn-custom")

        ),

        tabPanel("Stock Dilution",

                 numericInput("stock\_conc", "Stock Concentration", value = NULL),

                 numericInput("final\_conc\_stock", "Final Concentration", value = NULL),

                 numericInput("final\_volume\_stock", "Final Volume", value = NULL),

                 selectInput("volume\_unit\_stock", "Volume Unit", 

                             choices = c("mL" = "mL", "µL" = "µL", "L" = "L"), selected = "mL"),

                 actionButton("calc\_stock", "Calculate Stock Dilution", class = "btn-custom"),

                 actionButton("clear\_stock", "Clear", class = "btn-custom")

        ),

        tabPanel("Molarity",

                 numericInput("solutes\_molarity", "Molarity (M)", value = NULL),

                 numericInput("solutes\_volume", "Volume (L)", value = NULL),

                 numericInput("solutes\_mass", "Molecular Weight (g/mol)", value = NULL),

                 actionButton("calc\_molarity", "Calculate Mass Required", class = "btn-custom"),

                 actionButton("clear\_molarity", "Clear", class = "btn-custom")

        ),

        tabPanel("Buffer Preparation",

                 numericInput("buffer\_conc", "Desired Buffer Concentration (M)", value = NULL),

                 numericInput("buffer\_volume", "Desired Buffer Volume (L)", value = NULL),

                 numericInput("buffer\_stock\_conc", "Stock Solution Concentration (M)", value = NULL),

                 actionButton("calc\_buffer", "Calculate Volume of Stock Solution", class = "btn-custom"),

                 actionButton("clear\_buffer", "Clear", class = "btn-custom")

        )

      )

    ),

    mainPanel(

      # Output area for selected calculator

      textOutput("serial\_dilution\_output"),

      plotlyOutput("serial\_dilution\_schematic"),

      textOutput("stock\_dilution\_output"),

      textOutput("molarity\_output"),

      textOutput("buffer\_output")

    )

  )

)

server <- function(input, output, session) {

  observeEvent(input$calc\_serial, {

    C1 <- input$init\_conc

    C2 <- input$final\_conc

    dilution\_factor <- input$dilution\_factor

    V2 <- input$total\_volume

    

    volume\_unit <- input$volume\_unit

    conversion\_factor <- switch(volume\_unit,

                                "mL" = 1,

                                "µL" = 1000,

                                "L" = 0.001)

    V2 <- V2 \* conversion\_factor

    

    num\_steps <- log10(C1 / C2) / log10(dilution\_factor)

    volumes <- numeric(num\_steps)

    concentrations <- numeric(num\_steps)

    

    for (i in 1:num\_steps) {

      C\_current <- C1 / (dilution\_factor ^ i)

      V1 <- (C\_current \* V2) / C1

      volumes\[i] <- V1

      concentrations\[i] <- C\_current

    }

    volumes <- volumes / conversion\_factor

    

    output$serial\_dilution\_output <- renderText({

      paste("Number of steps: ", round(num\_steps),

            "\nVolumes to transfer (in", volume\_unit, "): ", paste(round(volumes, 2), collapse = ", "))

    })

    

    output$serial\_dilution\_schematic <- renderPlotly({

      df <- data.frame(

        step = 1:num\_steps,

        concentration = concentrations,

        volume = volumes

      )

      

      rect\_width <- 0.8

      rect\_height <- 0.5

      

      p <- ggplot(df, aes(x = step, fill = concentration)) +

        geom\_rect(aes(xmin = step - rect\_width / 2, xmax = step + rect\_width / 2,

                      ymin = 0 - rect\_height / 2, ymax = 0 + rect\_height / 2,

                      text = paste("Step:", step, "\<br>Concentration:", round(concentration, 2), 

                                   "\<br>Volume:", round(volume, 2), volume\_unit)), 

                  color = "black") +

        geom\_segment(aes(x = step + rect\_width / 2, xend = step + 1 - rect\_width / 2,

                         y = 0, yend = 0), 

                     arrow = arrow(length = unit(0.3, "cm")), 

                     size = 1, color = "blue", data = df\[df$step < num\_steps, ]) +

        scale\_fill\_gradient(low = "#D8BFD8", high = "#800080", name = "Concentration") +

        theme\_void() +

        labs(title = "Serial Dilution Schematic", subtitle = "Test Tubes and Transfers") +

        theme(

          plot.title = element\_text(hjust = 0.5, size = 18), 

          plot.subtitle = element\_text(hjust = 0.5),

          panel.grid = element\_blank()

        )

      

      ggplotly(p, tooltip = "text")

    })

  })

  

  observeEvent(input$calc\_stock, {

    stock\_conc <- input$stock\_conc

    final\_conc <- input$final\_conc\_stock

    final\_volume <- input$final\_volume\_stock

    

    volume\_unit\_stock <- input$volume\_unit\_stock

    conversion\_factor\_stock <- switch(volume\_unit\_stock,

                                      "mL" = 1,

                                      "µL" = 1000,

                                      "L" = 0.001)

    final\_volume <- final\_volume \* conversion\_factor\_stock

    

    volume\_stock\_needed <- (final\_conc \* final\_volume) / stock\_conc

    volume\_stock\_needed <- volume\_stock\_needed / conversion\_factor\_stock

    

    output$stock\_dilution\_output <- renderText({

      paste("Volume of stock solution required (in", volume\_unit\_stock, "):", round(volume\_stock\_needed, 2))

    })

  })

  

  observeEvent(input$calc\_molarity, {

    molarity <- input$solutes\_molarity

    volume <- input$solutes\_volume

    molecular\_weight <- input$solutes\_mass

    

    mass\_required <- molarity \* volume \* molecular\_weight

    

    output$molarity\_output <- renderText({

      paste("Mass of solute required (in grams):", round(mass\_required, 2))

    })

  })

  

  observeEvent(input$calc\_buffer, {

    buffer\_conc <- input$buffer\_conc

    buffer\_volume <- input$buffer\_volume

    stock\_conc <- input$buffer\_stock\_conc

    

    volume\_stock\_needed <- (buffer\_conc \* buffer\_volume) / stock\_conc

    

    output$buffer\_output <- renderText({

      paste("Volume of stock solution required (in L):", round(volume\_stock\_needed, 2))

    })

  })

  

  observeEvent(input$clear\_serial, {

    updateNumericInput(session, "init\_conc", value = NULL)

    updateNumericInput(session, "final\_conc", value = NULL)

    updateNumericInput(session, "dilution\_factor", value = 2)

    updateNumericInput(session, "total\_volume", value = NULL)

    updateSelectInput(session, "volume\_unit", selected = "mL")

    output$serial\_dilution\_output <- renderText({ NULL })

    output$serial\_dilution\_schematic <- renderPlotly({ NULL })

  })

  

  observeEvent(input$clear\_stock, {

    updateNumericInput(session, "stock\_conc", value = NULL)

    updateNumericInput(session, "final\_conc\_stock", value = NULL)

    updateNumericInput(session, "final\_volume\_stock", value = NULL)

    updateSelectInput(session, "volume\_unit\_stock", selected = "mL")

    output$stock\_dilution\_output <- renderText({ NULL })

  })

  

  observeEvent(input$clear\_molarity, {

    updateNumericInput(session, "solutes\_molarity", value = NULL)

    updateNumericInput(session, "solutes\_volume", value = NULL)

    updateNumericInput(session, "solutes\_mass", value = NULL)

    output$molarity\_output <- renderText({ NULL })

  })

  

  observeEvent(input$clear\_buffer, {

    updateNumericInput(session, "buffer\_conc", value = NULL)

    updateNumericInput(session, "buffer\_volume", value = NULL)

    updateNumericInput(session, "buffer\_stock\_conc", value = NULL)

    output$buffer\_output <- renderText({ NULL })

  })

}

shinyApp(ui = ui, server = server)
