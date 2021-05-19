library(shiny)
library(shinydashboard)
library(tidyverse)
library(dashboardthemes)
library(ggpubr)


# create data
# df1 <- data.frame(scenario = c("Baseline", "Scenario A", "Scenario B"), PI = c(2.57, 0.15, 0.9), Erosion = c(1.6, 0.03, 0.5), 
#                   Yield = c(5.4, 4.065, 4.8), Cost = c(98.33, 28.00, 42), Return = c(91.02, 300, 225), Milk = c(1.278, 1.048, 1.1))
df1 <- read_csv("data/wholeFarm.csv")
df2 <- read_csv("data/plotData.csv")
# data for comparision chart
#1. compare all values to the greatest value in each category so that everything is on the same axis
compare_df <- df1 %>%
    mutate(PI_ratio = PI/max(PI),
           Erosion_ratio = Erosion/max(Erosion),
           Yield_ratio = Yield/max(Yield),
           Cost_ratio = Cost/max(Cost),
           Return_ratio = Return/max(Return),
           Milk_ratio = Milk/max(Milk)) %>%
    select(-c(PI:Milk)) %>%
    rename(Return = Return_ratio,
           Cost = Cost_ratio,
           Milk = Milk_ratio,
           Yield = Yield_ratio,
           PI = PI_ratio,
           Erosion = Erosion_ratio) %>%
    pivot_longer(!scenario, names_to = "indicators", values_to = "ratio")


color_table <- tibble(
    #scenario = c("baseline", "Scenario A", "Scenario B"),
    Color = c("#334856", "#8C9C47", "#C5D54F")
)
# darkslategrey, darkseagreen4, darkseagreen3
#khaki4, khaki3, khaki2
#darkolivegreen2, darkolivegreen3, darkolivegreen4

color_table2 <- tibble(
    #scenario = c("baseline", "Scenario A", "Scenario B"),
    Color = c("#334856", "#225B81", "#056DB3",
              "#8C9C47", "#B1D025", "#C8F014",
              "#C5D54F", "#D9EA57", "#D2DB8A")
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Page header
    dashboardHeader(
        title = "GrazeScape Scenario Output",
        titleWidth = 450
    ),
    
    dashboardSidebar(
        sidebarMenu(id = "sidebarmenu",
            menuItem("Economic Indicators", icon = icon("bar-chart-o"),
                     menuSubItem('Whole Farm Analysis',
                                 tabName = "wholeEcon"),
                     menuSubItem('Field Analysis',
                                 tabName = "plotEcon")),
            menuItem("Soil Indicators", icon = icon("bar-chart-o"),
                     menuSubItem('Whole Farm Analysis',
                                 tabName = "wholeSoil"),
                     menuSubItem('Field Analysis',
                                 tabName = "plotSoil")),
            menuItem("Biodiversity Indicators", tabName = "bio", icon = icon("bar-chart-o")),
            menuItem("Compare Across", tabName = "ecoServ", icon = icon("bar-chart-0"))
        )
    ),
    
    dashboardBody(
        shinyDashboardThemes(
            theme = "poor_mans_flatly"
        ),
        tabItems(
            tabItem(tabName = "wholeEcon",
                    fluidRow(
                      column(6,
                               plotOutput("econ1")),
                      column(6, 
                             plotOutput("econ2"))),
                    br(),
                    br(),
                    fluidRow(
                        column(6,
                               plotOutput("econ3")),
                        column(6, 
                               plotOutput("econ4")))
            ),
            tabItem(tabName = "plotEcon",
                    fluidRow(
                        column(6,
                               plotOutput("plotEcon1")),
                        column(6, 
                               plotOutput("plotEcon2"))),
                    br(),
                    br(),
                    fluidRow(
                        column(6,
                               plotOutput("plotEcon3")),
                        column(6, 
                               plotOutput("plotEcon4")))
            ),
            tabItem(tabName = "wholeSoil",
                    fluidRow(
                        column(6,
                               plotOutput("soil1")),
                        column(6, 
                               plotOutput("soil2")))
            ),
            tabItem(tabName = "plotSoil",
                    fluidRow(
                        column(6,
                               plotOutput("plotSoil1")),
                        column(6, 
                               plotOutput("plotSoil2")))
            ),
            tabItem(tabName = "bio"),
            tabItem(tabName = "ecoServ",
                    plotOutput("compare"))
        ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$econ1 <- renderPlot({
        ggplot(df1, aes(x = scenario, y = Cost, fill = scenario)) + 
        geom_col(width = 0.4)  +
        # geom_col(data=df1[(df1$scenario=="A"),],        # filter
        #          aes(scenario), alpha=0, size=1, lty = "dotdash", color="black", width = 0.4) +
        scale_fill_manual(values = color_table$Color) +
        ylab("Dollars ($)")+
        ggtitle("Cost per Dry Matter Ton") +
        geom_hline(yintercept = 0)+
        theme(
            text = element_text(size = 18),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            axis.line.y = element_line(color = "lightgrey"),
            panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5))
    })
    output$econ2 <- renderPlot({
        ggplot(df1, aes(x = scenario, y = Return, fill = scenario)) + 
            geom_col(width = 0.4)  +
            # geom_col(data=df1[(df1$scenario=="A"),],        # filter
            #          aes(scenario), alpha=0, size=1, lty = "dotdash", color="black", width = 0.4) +
            scale_fill_manual(values = color_table$Color) +
            ylab(" ")+
            geom_hline(yintercept = 0)+
            ggtitle("Net Return per Acre") +
            theme(
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                panel.background = element_rect(fill = "white"),
                legend.position = "none",
                axis.line.y = element_line(color = "lightgrey"),
                plot.title = element_text(hjust = 0.5))
    })
    output$econ3 <- renderPlot({
        ggplot(df1, aes(x = scenario, y = Yield, fill = scenario)) + 
            geom_col(width = 0.4)  +
            # geom_col(data=df1[(df1$scenario=="Scenario A"),],        # filter
            #          aes(scenario), alpha=0, size=1, lty = "dotdash", color="black", width = 0.4) +
            geom_hline(yintercept = 0)+
            scale_fill_manual(values = color_table$Color) +
            ggtitle("Dry Matter") +
            ylab("Tons per Acre")+
            theme(
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.y = element_line(color = "lightgrey"),
                panel.background = element_rect(fill = "white"),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
    }) 
    output$econ4 <- renderPlot({
        ggplot(df1, aes(x = scenario, y = Milk, fill = scenario)) + 
            geom_col(width = 0.4)  +
            # geom_col(data=df1[(df1$scenario=="Scenario A"),],        # filter
            #          aes(scenario), alpha=0, size=1, lty = "dotdash", color="black", width = 0.4) +
            scale_fill_manual(values = color_table$Color) +
            ylab("Pounds (millions)")+
            geom_hline(yintercept = 0)+
            ggtitle("Annual Milk Production") +
            #ylim(0,15)+
            theme(
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.y = element_line(color = "lightgrey"),
                panel.background = element_rect(fill = "white"),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)))
    })
    output$plotEcon1 <- renderPlot({
        ggplot(df2, aes(x = scenario, y = Cost)) +   
            geom_bar(aes(fill = field), position = "dodge", stat="identity") +
            scale_fill_manual(values = color_table$Color) +
            ylab("Dollars ($)")+
            ggtitle("Cost per Dry Matter Ton") +
            geom_hline(yintercept = 0)+
            theme(
                text = element_text(size = 18),
                legend.position = "None",
                axis.title.x = element_blank(),
                axis.line.y = element_line(color = "lightgrey"),
                panel.background = element_rect(fill = "white"),
                plot.title = element_text(hjust = 0.5))
    })
    output$plotEcon2 <- renderPlot({
        ggplot(df2, aes(x = scenario, y = Return)) + 
            geom_bar(aes(fill = field), position = "dodge", stat="identity") +
            scale_fill_manual(values = color_table$Color) +
            ylab(" ")+
            geom_hline(yintercept = 0)+
            ggtitle("Net Return per Acre") +
            theme(
                text = element_text(size = 18),
                legend.position = c(0.85,0.9),
                axis.title.x = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.line.y = element_line(color = "lightgrey"),
                plot.title = element_text(hjust = 0.5))
    })
    output$plotEcon3 <- renderPlot({
        ggplot(df2, aes(x = scenario, y = Yield)) + 
            geom_bar(aes(fill = field), position = "dodge", stat="identity") +
            geom_hline(yintercept = 0)+
            scale_fill_manual(values = color_table$Color) +
            ggtitle("Dry Matter") +
            ylab("Tons per Acre")+
            theme(
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                axis.line.y = element_line(color = "lightgrey"),
                panel.background = element_rect(fill = "white"),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
    }) 
    output$plotEcon4 <- renderPlot({
        ggplot(df2, aes(x = scenario, y = Milk)) + 
            geom_bar(aes(fill = field), position = "dodge", stat="identity") +
            scale_fill_manual(values = color_table$Color) +
            ylab("Pounds (millions)")+
            geom_hline(yintercept = 0)+
            ggtitle("Annual Milk Production") +
            #ylim(0,15)+
            theme(
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.y = element_line(color = "lightgrey"),
                panel.background = element_rect(fill = "white"),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)))
    })
    output$soil1 <- renderPlot({
        ggplot(df1, aes(x = scenario, y = PI, fill = scenario)) +
            geom_col(width = 0.4) +
            # geom_col(data=df1[(df1$scenario=="Scenario A"),],        # filter
            #          aes(scenario), alpha=0, size=1, lty = "dotdash", color="black", width = 0.4) +
            geom_hline(yintercept = 0)+
            scale_fill_manual(values = color_table$Color) +
            ggtitle("Phosphorous loss")+
            ylab("Pounds per Acre")+
            theme(
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.y = element_line(color = "lightgrey"),
                panel.background = element_rect(fill = "white"),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5))
    })
    output$soil2 <- renderPlot({
        ggplot(df1, aes(x = scenario, y = Erosion, fill = scenario)) +
            geom_col(width = 0.4) +
            # geom_col(data=df1[(df1$scenario=="Scenario A"),],        # filter
            #          aes(scenario), alpha=0, size=1, lty = "dotdash", color="black", width = 0.4) +
            geom_hline(yintercept = 0)+
            scale_fill_manual(values = color_table$Color) +
            ggtitle("Soil Loss") +
            ylab("Tons per Acre")+
            theme(
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.y = element_line(color = "lightgrey"),
                panel.background = element_rect(fill = "white"),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5))
    })
    output$plotSoil1 <- renderPlot({
        ggplot(df2, aes(x = scenario, y = PI)) +
            geom_bar(aes(fill = field), position = "dodge", stat="identity") +
            geom_hline(yintercept = 0)+
            scale_fill_manual(values = color_table$Color) +
            ggtitle("Phosphorous loss")+
            ylab("Pounds per Acre")+
            theme(
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.y = element_line(color = "lightgrey"),
                panel.background = element_rect(fill = "white"),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5))
    })
    output$plotSoil2 <- renderPlot({
        ggplot(df2, aes(x = scenario, y = Erosion)) +
            geom_bar(aes(fill = field), position = "dodge", stat="identity") +
            geom_hline(yintercept = 0)+
            scale_fill_manual(values = color_table$Color) +
            ggtitle("Soil Loss") +
            ylab("Tons per Acre")+
            theme(
                text = element_text(size = 18),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.y = element_line(color = "lightgrey"),
                panel.background = element_rect(fill = "white"),
                legend.position = c(0.85,0.9),
                plot.title = element_text(hjust = 0.5))
    })
    
    output$compare <- renderPlot({
        
        ggplot(compare_df, aes(x = indicators, y = ratio, fill = scenario)) +
            geom_col(position = "dodge") +
            scale_fill_manual(values = color_table$Color) +
            #coord_flip() +
            theme(
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.text = element_text(size=22),
                axis.title.y = element_text(size = 18),
                legend.title = element_blank(),
                legend.text = element_text(size = 16)
            ) +
            xlab("") +
            ylab("Ratio")
    })
    
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
