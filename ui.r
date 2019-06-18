#Libraries Deployed
library(shinycssloaders)
library(shiny)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(plotly)
library(plyr)
library(dplyr)
library(DT)
library(data.table)
library(lubridate)
library(shinydashboard)
library(shinythemes)
library(tidyr)
library(shinyWidgets)
library(rsconnect)

#Reading in the Basetable
basetable <- read.csv("./basetable.csv", stringsAsFactors = FALSE)

#Start of UI.

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Betting Analytics"),
                    
                    dashboardSidebar(width = 350, collapsed = FALSE,
                                     tags$head(
                                       tags$style(HTML("
                                                       .sidebar { height: 90vh; overflow-y: auto; }
                                                       
                                                       " ))),
                                     
                                     sidebarMenu(
                                       #menuItem("Introduction", tabName = "intro", icon = icon("th")),
                                       # menuItem("Data", tabName = "data", icon = icon("dashboard")),
                                       menuItem("Data Table", tabName = "DT", icon = icon("indent-right", lib = 'glyphicon')),
                                       menuItem("Visualising Demographics", tabName = "DEM", icon = icon("screenshot", lib = 'glyphicon')),
                                       menuItem("Visualising Product Spread", tabName = "PTY", icon = icon("align-left", lib = "glyphicon")),
                                       menuItem("Visualisations Based on Product Types", tabName = "PRD", icon = icon("align-left", lib = "glyphicon"),
                                       menuItem("Visualisations of SportsBook Data", tabName = "SBK", icon = icon("list-alt", lib = 'glyphicon')),
                                       menuItem("Visualisations of Poker Data", tabName = "PKR", icon = icon("list-alt", lib = 'glyphicon')),
                                       menuItem("Visualisations of Casino Data", tabName = "CSN", icon = icon("list-alt", lib = 'glyphicon')),
                                       menuItem("Visualisations of Games Data", tabName = "GMS", icon = icon("list-alt", lib = 'glyphicon'))),
                                       
                                       tags$div(
                                         
                                         
                                         prettyRadioButtons("SelectG","Gender :",inline = T,
                                                                c("Both"="Both","Male"="Male","Female"="Female"))
                                         
                                       ))
                                     
                                     
                                       ),
                    
                    # Building the Dashboard Body
                    
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName="DT",
                                
                                
                                tabsetPanel(
                                           tabPanel("Data",
                                                      wellPanel(div(style = 'overflow-x: scroll', DT::dataTableOutput('table'))
                                                      ))
                                             
                                                                 )),
                                                                 
                        #  Demographics Tab Content
                        
                        tabItem(tabName = "DEM",
                                # Different Tab Panels for Demographics
                                         tabPanel("Continent Chart",
                                                  wellPanel(fluidRow(plotlyOutput("plotcontinent")))
                                                  ),
                                          tabPanel("Selection Options",
                                                   wellPanel(fluidRow(
                                                     
                                                     pickerInput('continent', 'Select Continents:', choices = c("Africa", "Asia",
                                                                                                                "Europe", "Latin America and Caribbean",
                                                                                                                "North America" , "Oceania"), selected = c("Africa", "Asia",
                                                                                                                                                           "Europe", "Latin America and Caribbean",
                                                                                                                                                           "North America" , "Oceania"),
                                                                 options = list(`actions-box` = TRUE),
                                                                 multiple= TRUE),
                                                     
                                               # Select which types of gender to plot
                                                     prettyCheckboxGroup(inputId = "selected_gender",
                                                                        label = "Select gender:",
                                                                        choices = c("M", "F"),
                                                                        fill = TRUE,
                                                                        shape = 'round',
                                                                        icon = icon('users'),
                                                                        animation = 'pulse',
                                                                        selected = c("M","F") ),
                                                     
                                                          hr(),           # Horizontal line for visual separation
                                                     
                                                     
                                                     sliderInput("xbins", "Range: ", 
                                                                 min = 10, max = 100, value = c(18,75), width = 250)
                                                     
                                                     
                                                     
                                                     
                                                     
                                                   ))
                                                  ),
                                          tabPanel("Age Histogram",
                                                  wellPanel(fluidRow(plotlyOutput("agehist")))
                                          )
                        ),
                        
                        # Products Tab
                        tabItem(tabName = "PTY",
                                
                                #Different Panels under Products Tab
                                tabPanel("Number Plot",
                                         wellPanel(fluidRow(plotlyOutput("plotnumber")))
                                         ),
                                tabPanel("Selection Options",
                                         wellPanel(fluidRow(
                                           selectInput('xall', 'Select variable for X axis:', choices = c("Bets" = "total_bets" ,"Stakes"= "total_stakes", "Winnings" = "total_winnings" ), selected = "total_stakes"),
                                           selectInput('yall', 'Select variable for Y axis:', choices = c("Bets" ="total_bets" ,"Stakes"= "total_stakes", "Winnings" = "total_winnings" ), selected = "total_winnings"),
                                           
                                           hr(),                # Horizontal line for visual separation
                                           
                                           
                                           # Select which types of gender to plot
                                           prettyCheckboxGroup(inputId = "selected_number",
                                                              label = "Select number of products:",
                                                              choices = c("1" , "2", "3", "4"),
                                                              selected = c("1" , "2", "3", "4") ),
                                           
                                           hr(),                # Horizontal line for visual separation
                                           
                                           pickerInput(inputId = "selected_app",
                                                       label = "Select Application:",
                                                       choices = c("BALLS OF FIRE" ,"BETANDWIN CASINO", "BETANDWIN POKER",
                                                                   "BETANDWIN.DE" ,  "BETEUROPE GAMES"  , "BETEUROPE POKER" ,"BETEUROPE.COM" ,"BETOTO CASINO",
                                                                   "BETOTO POKER" ,"BETOTO.COM" ,"BOF.BETEUROPE.COM" ,"BOF.PLAYIT.COM", "CASINO.BETEUROPE.COM",
                                                                   "CASINO.PLAYIT.COM",   "LOTTERY.BETOTO.COM", "PLAYIT GAMES" , "PLAYIT POKER" , "PLAYIT.COM",
                                                                   "WAP.BETANDWIN.COM",  "WAP.BETANDWIN.DE"),
                                                       selected = c("BALLS OF FIRE" ,"BETANDWIN CASINO", "BETANDWIN POKER",
                                                                    "BETANDWIN.DE" ,  "BETEUROPE GAMES"  , "BETEUROPE POKER" ,"BETEUROPE.COM" ,"BETOTO CASINO",
                                                                    "BETOTO POKER" ,"BETOTO.COM" ,"BOF.BETEUROPE.COM" ,"BOF.PLAYIT.COM", "CASINO.BETEUROPE.COM",
                                                                    "CASINO.PLAYIT.COM",   "LOTTERY.BETOTO.COM", "PLAYIT GAMES" , "PLAYIT POKER" , "PLAYIT.COM",
                                                                    "WAP.BETANDWIN.COM",  "WAP.BETANDWIN.DE"),
                                                       options = list(`actions-box` = TRUE),
                                                       multiple= TRUE)
                                           
                                         )
                                         
                                         )
                                ),
                                tabPanel("Application Plot",
                                         wellPanel(fluidRow(plotlyOutput("plotapp")))
                                ),
                                tabPanel("All Products Plot",
                                         wellPanel(fluidRow(plotlyOutput("allP")))
                                )
                        ),
                       
                         # Sports Book Tab
                        tabItem(tabName = "SBK",
                                # Different Panels for Sports Book Tab
                                
                                tabPanel("SBK Plots",
                                         wellPanel(fluidRow(plotlyOutput("SPPlot")))
                                ),
                                tabPanel("Selection Options",
                                         wellPanel(fluidRow(
                                           
                                           
                                           
                                           h4("Winnings, bets and stakes relationship"),      # Third level header: Plotting
                                           
                                           
                                           sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(basetable),
                                                       value = 1000, step = 500, round = 0),
                                           selectInput('x', 'Select variable for X axis:', choices = c("Bets" = "sum_bets_SportsBook" ,"Stakes"= "sum_stakes_SportsBook", "Winnings" = "sum_winnings_SportsBook" ), selected = "sum_bets_SportsBook"),
                                           selectInput('y', 'Select variable for Y axis:', choices = c("Bets" ="sum_bets_SportsBook" ,"Stakes"= "sum_stakes_SportsBook", "Winnings" = "sum_winnings_SportsBook" ), selected = "sum_winnings_SportsBook"),
                                           
                                           
                                           hr(),                # Horizontal line for visual separation
                                           hr(),                # Horizontal line for visual separation
                                           h4("Bets and monetary value"),   
                                           selectInput('x3', 'Select variable for X axis:', choices = c( "Bets" ="sum_bets_SportsBook"), selected = "sum_bets_SportsBook"),
                                           selectInput('y3', 'Select variable for Y axis:', choices = c("Lenght of subscription Monetary" = "MValLOS_SportsBook", 
                                                                                                        "Stakes monetary value for Stakes " ="MVStakesLOS_SportsBook",
                                                                                                        "Winnings monetary value" = "MVWinnigsLOS_SportsBook"), selected = "MVWinnigsLOS_SportsBook")
                                         ))
                                ),
                                tabPanel("SBK Monetary Values Plot",
                                         wellPanel(fluidRow(plotlyOutput("SPMval")))
                                )
                        ),
                        
                        #Casino Tab
                        
                        tabItem(tabName = "CSN",
                                # Different Panels for Casino Tab
                                
                                tabPanel("CSN Plot",
                                         wellPanel(fluidRow(plotlyOutput("CSPlot")))
                                ),
                                tabPanel("Selection Options",
                                         wellPanel(fluidRow(
                                           
                                           h4("Winnings, bets and stakes relationship"),      # Third level header: Plotting
                                           
                                           selectInput('xc', 'Select variable for X axis:', choices = c("Bets" = "sum_bets_Casino" ,"Stakes"= "sum_stakes_Casino", "Winnings" = "sum_winnings_Casino" ), selected = "sum_bets_Casino"),
                                           selectInput('yc', 'Select variable for Y axis:', choices = c("Bets" ="sum_bets_Casino" ,"Stakes"= "sum_stakes_Casino", "Winnings" = "sum_winnings_Casino" ), selected = "sum_winnings_Casino"),
                                           
                                           
                                           hr(),                # Horizontal line for visual separation
                                           hr(),                # Horizontal line for visual separation
                                           h4("Bets and monetary value"),   
                                           selectInput('x3c', 'Select variable for X axis:', choices = c( "Bets" ="sum_bets_Casino"), selected = "sum_bets_Casino"),
                                           selectInput('y3c', 'Select variable for Y axis:', choices = c("Lenght of subscription Monetary" = "MValLOS_casino", 
                                                                                                        "Stakes monetary value" ="MVStakesLOS_casino",
                                                                                                        "Winnings monetary value" = "MVWinnigsLOS_casino"), selected = "MVWinnigsLOS_casino")
                                           
                                           
                                         ))
                                ),
                                tabPanel("Casino Monetary Value Plot",
                                         wellPanel(fluidRow(plotlyOutput("CSMVal")))
                        )),
                        
                        # Poker Tab
                        
                        tabItem(tabName = "PKR",
                                  # Different Panels for Poker Tab
                                  
                                  tabPanel("Poker Plot",
                                           wellPanel(fluidRow(plotlyOutput("PKPlot")))
                                  ),
                                  tabPanel("Selection Options",
                                           wellPanel(fluidRow(
                                             
                                             h4("Buy and Sell relationship"),      # Third level header: Plotting
                                             
                                             selectInput('xp', 'Select variable for X axis:', choices = c( "Buy amount " = "sum_Amount_Buy" , "Profit"= "MValProfit_Poker" , "Sell amount" = "sum_Amount_Sell"), selected = "MValProfit_Poker"),
                                             selectInput('yp', 'Select variable for Y axis:', choices = c("Buy amount " = "sum_Amount_Buy" , "Profit"= "MValProfit_Poker" , "Sell amount" = "sum_Amount_Sell" ), selected = "sum_Amount_Buy"),
                                             
                                             
                                             hr(),                # Horizontal line for visual separation
                                             
                                             hr()               # Horizontal line for visual separation
                                             
                                             
                                           ))
                                  )
                                  
                        ),
                        
                        # Games Tab
                        
                        tabItem(tabName = "GMS",
                                
                                # Different Panels for Games Tab
                                
                                tabPanel("Games Plot",
                                         wellPanel(fluidRow(plotlyOutput("GMPlot")))
                                ),
                                tabPanel("Selection Options",
                                         wellPanel(fluidRow(
                                           
                                           
                                           h4("Winnings, bets and stakes relationship"),      # Third level header: Plotting
                                           
                                           selectInput('xg', 'Select variable for X axis:', choices = c("Bets" = "sum_bets_Games" ,"Stakes"= "sum_stakes_Games", "Winnings" = "sum_winnings_Games" ), selected = "sum_bets_Games"),
                                           selectInput('yg', 'Select variable for Y axis:', choices = c("Bets" ="sum_bets_Games" ,"Stakes"= "sum_stakes_Games", "Winnings" = "sum_winnings_Games" ), selected = "sum_winnings_Games"),
                                           
                                           
                                           hr(),                # Horizontal line for visual separation
                                           
                                           hr(),                # Horizontal line for visual separation
                                           h4("Bets and monetary value"),   
                                           selectInput('x3g', 'Select variable for X axis:', choices = c( "Bets" ="sum_bets_Games"), selected = "sum_bets_Games"),
                                           selectInput('y3g', 'Select variable for Y axis:', choices = c("Lenght of subscription Monetary" = "MValLOS_Games", 
                                                                                                        "Stakes monetary value" ="MVStakesLOS_Games",
                                                                                                        "Winnings monetary value" = "MVWinnigsLOS_Games"), selected = "MVWinnigsLOS_Games")
                                           
                                           
                                           
                                         ))
                                ),
                                tabPanel("Games Monetary Value",
                                         wellPanel(fluidRow(plotlyOutput("GMMVal")))
                        )
                        ))
                                  ))





# End of UI.