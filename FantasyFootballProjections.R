
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(DT)
library(ffanalytics)
# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidPage(theme = shinytheme("sandstone")),
    # Application title
    titlePanel("Your Fantasy Football Projection"),
    #theme = shinytheme("darkly"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        titlePanel("Input League Point System for Each Position"),
        actionButton("generate", "Generate New Table"),
        p("Click the button to update the table after inputing your scoring system. This should take a minute or two."),
        #passing
        tabsetPanel(
            tabPanel("Passing", numericInput("pass_att", "Points per Pass Attempt", 0), 
                     numericInput("pass_att", "Points per Pass Attempt", 0), 
                     numericInput("pass_comp", "Points per Pass Completion", .1),
                     numericInput("pass_yds", "Points per Pass Yard", 0.04), 
                     numericInput("pass_tds", "Points per Passing Touchdown", 6),
                     numericInput("pass_int", "Points per Interception Thrown", -1),
                     numericInput("pass_inc", "Points per Incompletion", 0),
                     numericInput("pass_40_yds", "Points per 40 Yard Pass", 2),
                     numericInput("pass_300_yds", "300 Passing Yard Bonus", 0),
                     numericInput("pass_350_yds", "350 Passing Yard Bonus", 0),
                     numericInput("pass_400_yds", "400 Passing Yard Bonus", 0)),
        tabPanel("Rushing", numericInput("rush_yds", "Points per Rushing Yard", .1),
                 numericInput("rush_att", "Points per Rushing Attempt", 0),
                 numericInput("rush_tds", "Points per Rushing Touchdown", 6),
                 numericInput("rush_40_yds", "Points per 40 Yard Rush", 2),
                 numericInput("rush_100_yds", "100 Rushing Yard Bonus", 0),
                 numericInput("rush_150_yds", "150 Rushing Yard Bonus", 0),
                 numericInput("rush_200_yds", "200 Rushing Yard Bonus", 0)),
        tabPanel("Recieving", numericInput("rec", "Points per Reception", 0),
                 numericInput("rec_yds", "Points per Recieving Yard", 0),
                 numericInput("rec_tds", "Points per Recieving Touchdown", 6),
                 numericInput("rec_40_yds", "Points per 40 Yard Reception", 2),
                 numericInput("rec_100_yds", "100 Recieving Yard Bonus", 0),
                 numericInput("rec_150_yds", "150 Recieving Yard Bonus", 0),
                 numericInput("rec_200_yds", "200 Recieving Yard Bonus", 0)),
        tabPanel("Kicking", numericInput("xp", "Extra Point", 1),
                 numericInput("fg_0019", "Field Goal from 0-19", 3),
                 numericInput("fg_2029", "Field Goal from 20-29", 3),
                 numericInput("fg_3039", "Field Goal from 30-39", 3),
                 numericInput("fg_4049", "Field Goal from 40-49", 4),
                 numericInput("fg_50", "Field Goal from 50+", 5),
                 numericInput("fg_miss", "Field Goal Miss", 0)),
        tabPanel("Defense", numericInput("dst_fum_rec", "Point per Fumble Recovery", 2),
                 numericInput("dst_int", "Point per Interception", 2),
                 numericInput("dst_safety", "Point per Safety", 2),
                 numericInput("dst_sacks", "Point per Sack", 1),
                 numericInput("dst_td", "Point per Defensive Touchdown", 6),
                 numericInput("dst_blk", "Point per Blocked Kick", 2),
                 numericInput("dst_ret_yds", "Point per Return Yard", 0),
                 numericInput("dst_pts_allowed", "Point per Allowed Point", 0)),
        tabPanel("Misc", numericInput("fumbles_lost", "Fumbles Lost", -1),
                 numericInput("fumbles_total", "Total Fumbles", 0),
                 numericInput("sacks", "Points per Sack", 0),
                 numericInput("two_pts", "2 Point Conversion", 2),   
                 numericInput("return_tds", "Points per Return Touchdown", 6),
                 numericInput("return_yds", "Points per Return Yard", 0))
        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
        titlePanel("Player Projections"),
        numericInput("season", "Season", 2020),
           DT::DTOutput('Player_Projections')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    ntext <- eventReactive(input$generate, {
        my_scrape = scrape_data(src = c("CBS", "ESPN", "FantasyData", "FantasyPros", "FFToday",
                                        "FleaFlicker", "NumberFire", "Yahoo", "FantasyFootballNerd", "NFL", "RTSports",
                                        "Walterfootball"), pos = c("QB", "RB", "WR", "TE", "K" ,"DST"),
                                season = input$season, week = 0)
        my_settings = my_settings = list(
            pass = list(
                pass_att = input$pass_att, pass_comp = input$pass_comp, pass_inc = input$pass_inc, pass_yds = input$pass_yds, 
                pass_tds = input$pass_tds,
                pass_int = input$pass_int, pass_40_yds = input$pass_40_yds,  pass_300_yds = input$pass_300_yds, pass_350_yds = input$pass_350_yds,
                pass_400_yds = input$pass_400_yds),
            rush = list(
                all_pos = TRUE,
                rush_yds = input$rush_yds,  rush_att = input$rush_att, rush_40_yds = input$rush_40_yds, rush_tds = input$rush_tds,
                rush_100_yds = input$rush_100_yds, rush_150_yds = input$rush_150_yds, rush_200_yds = input$rush_200_yds),
            rec = list(
                all_pos = TRUE,
                rec = input$rec, rec_yds = input$rec_yds, rec_tds = input$rec_tds, rec_40_yds = input$rec_40_yds, rec_100_yds = input$rec_100_yds,
                rec_150_yds = input$rec_150_yds, rec_200_yds = input$rec_200_yds),
            misc = list(
                all_pos = TRUE,
                fumbles_lost = input$fumbles_lost, fumbles_total = input$fumbles_total,
                sacks = input$sacks, two_pts = input$two_pts),
            kick = list(
                xp = input$xp, fg_0019 = input$fg_0019,  fg_2029 = input$fg_2029, fg_3039 = input$fg_3039, fg_4049 = input$fg_4049,
                fg_50 = input$fg_50,  fg_miss = input$fg_miss),
            ret = list(
                all_pos = TRUE,
                return_tds = input$return_tds, return_yds = input$return_yds),
            idp = list(
                all_pos = TRUE,
                idp_solo = 1, idp_asst = 0.5, idp_sack = 2, idp_int = 3,  idp_fum_force = 3,
                idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2),
            dst = list(
                dst_fum_rec = input$dst_fum_rec,  dst_int = input$dst_int, dst_safety = input$dst_safety, dst_sacks = input$dst_sacks, dst_td = input$dst_td,
                dst_blk = input$dst_blk, dst_ret_yds = input$dst_ret_yds, dst_pts_allowed = input$dst_pts_allowed),
            pts_bracket = list(
                list(threshold = 0, points = 10),
                list(threshold = 6, points = 7),
                list(threshold = 13, points = 5),
                list(threshold = 17, points = 3),
                list(threshold = 21, points = 1)))
        my_projections =  projections_table(my_scrape, scoring_rules = my_settings)
        my_projections = my_projections %>% add_player_info()
        a=subset(my_projections, avg_type == "weighted") 
        b = a %>% 
            select(first_name, last_name, team, position, age, points, sd_pts, floor, ceiling, tier, rank) %>%
            filter(tier < 15) %>%
            arrange(tier, desc(points), sd_pts)
        c = datatable(b,  filter = "top")
    })

    output$Player_Projections <- DT::renderDT({
        ntext()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
