library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(googlesheets4)

gsheet_matches <- "https://docs.google.com/spreadsheets/d/1UMfZPGZCMBTQ8GOhjct0umpJiZp1kvOmKLN7m4SyXGU"
gsheet_scores <- "https://docs.google.com/spreadsheets/d/14eKVVBPUZ86xEqRtrPLIvzIax3kz_ww5mr53QwiqDhw"

matches <- read_sheet(gsheet_matches)
scores <- read_sheet(gsheet_scores)

# ui <- fluidPage(
#     titlePanel("Desamigos Fantasy"),

#     sidebarPanel(
#         selectInput(
#             "match_id",
#             "ID da partida",
#             choices = matches$MatchID
#         ),
#         p(paste0("Partida", input$match_id))
#     ),

#     mainPanel(
#         dataTableOutput("tabela")
#     )
# )

ui <- dashboardPage(
    dashboardHeader(
        title = "Desamigos Fantasy"
    ),
    dashboardSidebar(
        selectInput(
            "match_id",
            "ID da partida",
            choices = matches$MatchID
        ),
        htmlOutput("match_text")
    ),
    dashboardBody(
        fluidRow(
            dataTableOutput("tabela")
        )
    )
)

server <- function(input, output) {
    scores_output <- reactive({
        scores %>%
            filter(match_id == input$match_id) %>%
            mutate(
                Jogador = Name,
                Hero = hero,
                Side = ifelse(isRadiant, "Radiant", "Dire"),
                Kills = kills,
                Deaths = deaths,
                LHs = last_hits,
                GPM = gold_per_min,
                Towers = towers_killed,
                Roshans = roshans_killed,
                `TFs (%)` = teamfight_participation,
                `Obs. Wards placed` = obs_placed,
                Stacks = camps_stacked,
                Runes = rune_pickups,
                FBs = firstblood_claimed,
                `Stuns (s)` = stuns,
                Score = total,
                .keep = "none"
            )
    })

    output$tabela <- renderDataTable({
        scores_output() %>%
            datatable(
                extensions = c("FixedColumns", "Select"),
                selection = "none",
                style = "default",
                options = list(
                    scrollX = TRUE,
                    paging = FALSE,
                    bFilter = FALSE,
                    fixedColumns = list(leftColumns = 4, rightColumns = 1),
                    buttons = c("csv")
                )
            ) %>%
            formatStyle(c(3), borderRight = "1px solid gray") %>%
            formatStyle(ncol(scores_output()), borderLeft = "1px solid gray")
    })

    output$match_text <- renderUI({
        line1 <- h3(paste0(matches[which(input$match_id == matches$MatchID),"Title"]))
        line2 <- p(paste0("Partida ", input$match_id))
        line3 <- p(matches[which(input$match_id == matches$MatchID),"Description"])

        HTML(paste(line1, line2, line3, sep = "<br />"))
    })
}

shinyApp(ui, server)
