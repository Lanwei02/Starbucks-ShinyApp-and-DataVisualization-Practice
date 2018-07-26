ui <- navbarPage("Starbucks",
           tabPanel("Worldwide Distribution",highchartOutput('MAP1', height = "800px")),
           tabPanel("Worldwide Distribution (Top 5 Countries)",h4('Please selecting the reb button:'),
                    leafletOutput("map", height="500px"),
                    tabPanel("plot", DT::dataTableOutput("plot"))),
           tabPanel("Ownership",h4('Worldwide:'),
                    tabPanel("ownership", DT::dataTableOutput("ownership_dt")),
                    plotlyOutput("ownership_bar", width = "100%")),
           tabPanel('Top N countries', sliderInput("n", "Number of countries shown: ", min = 5, max = 73, value = 10, step = 1),
                    plotlyOutput("topCountry", width = "100%"),
                    tabPanel('topCountryBar', DT::dataTableOutput("topCountryBar")))
)