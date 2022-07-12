library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidymodels)
library(tidyverse)
library(stringr)
library(plotly)
library(ranger)
library(caret)


model_rate <- readRDS("model_forest_rate.rds")
model_rate_activism <- readRDS("model_forest_rate_activism.rds")

# function to predict the probability
predict_probability <- function(model, dat){
    stats::predict(model, dat, type = "prob") %>%
        tidyr::gather() %>%
        dplyr::mutate(value = as.numeric(value))
}

# Define UI for application that draws a histogram
# Define UI for app that draws a histogram ----
ui <- fluidPage(

    # App title ----
    titlePanel("Auswertung der Umfrageergebnisse zur Abstimmung des CO2-Gesetz vom 13.06.21"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            selectInput("pol_party", label = "Welche Partei entspricht in den Zielen und Forderungen am ehesten Ihren eigenen
                        Ansichten und Wünschen?",
                        choices = c("SVP (Schweizerische Volkspartei)",
                                    "SP (Sozialdemokratische Partei)",
                                    "FDP.Die Liberalen (Freisinnig Demokratische Partei)",
                                    "CVP (Christlichdemokratische Volkspartei)",
                                    "GPS (Grüne Partei Schweiz)",
                                    "GLP (Grünliberale Partei)",
                                    "BDP (Bürgerlich Demokratische Partei)",
                                    "EVP (Evangelische Volkspartei der Schweiz)",
                                    "Lega dei Ticinesi",
                                    "PdA (Partei der Arbeit Schweiz)",
                                    "MCG (Mouvement Citoyens Genevois)",
                                    "CSP (Christlichsoziale Partei Schweiz)",
                                    "EDU (Eidgenössisch-Demokratische Union)",
                                    "Sol. (SolidaritéS)",
                                    "Andere:",
                                    "Keine",
                                    "Weiss nicht / keine Antwort")
            ),

        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Histogram ----
            plotOutput(outputId = "Plot1", height = "200px")
            # ,plotOutput(outputId = "Plot2", height = "200px")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {



    output$Plot1 <- renderPlot({


        dat <- tibble(
           "civi_stat" = 1,
            "fin_cond" = 1,
            "pol_party" = if(input$pol_party == "SVP (Schweizerische Volkspartei)") {1}
            else if (input$pol_party == "SP (Sozialdemokratische Partei)") {2}
            else if (input$pol_party == "FDP.Die Liberalen (Freisinnig Demokratische Partei)") {3}
            else if (input$pol_party == "CVP (Christlichdemokratische Volkspartei)") {4}
            else if (input$pol_party == "GPS (Grüne Partei Schweiz)") {5}
            else if (input$pol_party == "GLP (Grünliberale Partei)") {6}
            else if (input$pol_party == "BDP (Bürgerlich Demokratische Partei)") {7}
            else if (input$pol_party == "EVP (Evangelische Volkspartei der Schweiz)") {8}
            else if (input$pol_party == "Lega dei Ticinesi") {9}
            else if (input$pol_party == "PdA (Partei der Arbeit Schweiz)") {10}
            else if (input$pol_party == "MCG (Mouvement Citoyens Genevois)") {11}
            else if (input$pol_party == "CSP (Christlichsoziale Partei Schweiz)") {12}
            else if (input$pol_party == "EDU (Eidgenössisch-Demokratische Union)") {13}
            else if (input$pol_party == "Sol. (SolidaritéS)") {14}
            else if (input$pol_party == "Andere:") {15}
            else if (input$pol_party == "Keine") {16}
            else if (input$pol_party == "Weiss nicht / keine Antwort") {17},
            "renew_heating" = 1,
            "left_right" = 1,
            "prior_benefit" = 1,
            "ren_driver" = 1,
            "home_owner" = 1,
            "educ" = 1,
            "empl_sect" = 1,
            "empl_stat" = 2,
            "gender" = 1,
            "region" = 1,
            "know_targ" = 1,
            "know_build" = 1,
            "know_trans" = 1,
            "know_food" = 1,
            "know_avia" = 1,
            "know_wast" = 1,
            "efficiency" = 3,
            "effectiveness" = 3,
            "competitiveness" = 3,
            "justice" = 3,
            "transformation" = 3

        )

            predict_probability(model_rate, dat) %>%
             dplyr::mutate(
              value = ifelse(key == ".pred_2", value*(-1), value),
              value = ifelse(key == ".pred_1", value*(-1), value),
              value = ifelse(key == ".pred_3", value/2, value),
              dv = ""
            ) %>%
            dplyr::bind_rows(.[.$key == ".pred_3",] %>% dplyr::mutate(value = value *(-1))) %>%
            dplyr::mutate(key = factor(key, levels = c(".pred_3", ".pred_2", ".pred_1", ".pred_4", ".pred_5"))) %>%
            ggplot2::ggplot(.) +
            ggplot2::geom_bar(aes(x = dv, y = value, fill = key), stat = "identity", position = position_stack(reverse = TRUE)) +
            ggplot2::theme_minimal() +
            ggplot2::coord_flip() +
            ggplot2::ylim(-1,1) +
            ggplot2::labs(
            title = "Public Support",
            x = "",
            y = "Probability"
        ) +
            ggplot2::scale_fill_manual(name = "", labels =c("Viel Aufwand zur Unterstützung", "Etwas Aufwand zur Unterstützung", "Viel Aufwand zur Verhinderung", "Etwas Aufwand zur Verhinderung", "Weder noch"), limits = rev, values = c("darkgreen", "lightgreen", "red4", "red3",  "grey")) +
        ggplot2::theme(plot.title = element_text(margin = ggplot2::margin(30,30,30,30)), legend.position = "bottom") +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))

    })

}

# Run the application
shinyApp(ui = ui, server = server)
