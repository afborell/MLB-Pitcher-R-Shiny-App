library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(baseballr)
library(tidyverse)

chadwick <- read_delim('chadwick.txt')
statcast_pitchers_2021 <- read_csv('statcast_pitchers_2021')
statcast_pitchers_2021 <- statcast_pitchers_2021 %>% 
    rename(Pitch = pitch_name)
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="double"] <- "Double"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="field_error"] <- "Error"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="field_out"] <- "Out"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="fielders_choice"] <- "Fielders Choice"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="fielders_choice_out"] <- "Fielders Choice"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="force_out"] <- "Out"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="grounded_into_double_play"] <- "Out"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="hit_by_pitch"] <- "HBP"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="home_run"] <- "Home Run"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="sac_fly"] <- "Out"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="single"] <- "Single"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="triple"] <- "Triple"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="walk"] <- "Walk"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="strikeout"] <- "Strikeout"
statcast_pitchers_2021$events[statcast_pitchers_2021$events=="sac_bunt"] <- "Bunt"
statcast_pitchers_2021$type[statcast_pitchers_2021$type=="S"] <- "Strike"
statcast_pitchers_2021$type[statcast_pitchers_2021$type=="B"] <- "Ball"
statcast_pitchers_2021$type[statcast_pitchers_2021$type=="X"] <- "Hit Into Play"

ui <- fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    column(4, wellPanel(
        h3(id="big-heading", "2021 Pitcher Visualizations"),
        tags$style(HTML("#big-heading{color: black;}")),
        textInput("name", "Pitcher:",
                  value = ""),
        tableOutput("table"),
        checkboxGroupInput("Pitch", "Pitch:",
                           c("4-Seam Fastball", "Sinker", "Changeup", "Slider",
                             "Curveball", "Cutter",
                             "Knuckle Curve", "Split-Finger", "Fastball", "Screwball", "Eephus", "Knuckleball"),
                           selected = "4-Seam Fastball",
                           inline = TRUE),
        checkboxGroupInput("count", "Count:",
                           c("0 - 0", "1 - 0", "0 - 1", "2 - 0",
                             "1 - 1", "0 - 2", "3 - 0", "2 - 1", "1 - 2",
                             "3 - 1", "2 - 2", "3 - 2"),
                           selected = "0 - 0",
                           inline = TRUE),
        checkboxGroupInput("stand", 'Batter',
                           c('R', 'L'),
                           selected = c('R', 'L'),
                           inline = TRUE)
    )),
    column(8,
           plotOutput("plot1",
                      height = '540px')
    ),
    column(8,
           plotOutput("plot2",
                      height = '250px')
    ),
    column(12,
           plotOutput("plot3",
                      height = '450px')
))

server <- function(input, output, session) {
    
    output$table <- renderTable({
        get_id <- function(st){
            names <- str_to_lower(unlist(str_split(str_squish(st), " ")))
            if(length(names) == 3){
                names <- c(paste(names[1], names[2]), names[3])
            }
            chadwick %>%
                mutate(fname = str_to_lower(name_first),
                       lname = str_to_lower(name_last),
                       Name = paste(name_first,
                                    name_last)) %>%
                filter(fname == names[1],
                       lname == names[2]) %>%
                select(key_mlbam, Name)
        }
        nice_table <- function(d){
            d %>%
                group_by(Pitch) %>%
                summarize(Total = n()) %>%
                filter(is.na(Pitch) == FALSE)
        }
        pid <- get_id(input$name)$key_mlbam
        req(length(pid) > 0)
        nice_table(filter(statcast_pitchers_2021,
                          pitcher == pid))
    })
    output$plot1 <- renderPlot({
        req(length(input$count) > 0)
        get_id <- function(st){
            names <- str_to_lower(unlist(str_split(str_squish(st), " ")))
            if(length(names) == 3){
                names <- c(paste(names[1], names[2]), names[3])
            }
            chadwick %>%
                mutate(fname = str_to_lower(name_first),
                       lname = str_to_lower(name_last),
                       Name = paste(name_first,
                                    name_last)) %>%
                filter(fname == names[1],
                       lname == names[2]) %>%
                select(key_mlbam, Name)
        }
        add_zone <- function(color = "black"){
            topKzone <- 3.5
            botKzone <- 1.6
            inKzone <- -0.85
            outKzone <- 0.85
            kZone <- data.frame(
                x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
                y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
            )
            geom_path(aes(.data$x, .data$y),
                      data=kZone, lwd = 1, color = color)
        }
        centertitle <- function(){
            theme(plot.title = element_text(
                colour = "white", size = 14,
                hjust = 0.5, vjust = 0.8, angle = 0),
                plot.subtitle = element_text(
                    colour = "white", size = 14,
                    hjust = 0.5, vjust = 0.8, angle = 0))
        }
        pid <- get_id(input$name)$key_mlbam
        req(length(pid) > 0)
        
        th1 <- theme(plot.background =
                         element_rect(fill = "black"),
                     axis.text = element_text(colour = "white"),
                     axis.title = element_text(colour = "white"),
                     plot.title = element_text(hjust = 0.5, face = 'bold', size = 16))
        
        sc_new <- filter(statcast_pitchers_2021,
                         Count %in% input$count,
                         Pitch %in% input$Pitch,
                         stand %in% input$stand,
                         pitcher == get_id(input$name)$key_mlbam)
        
        ggplot() +
            geom_point(data = sc_new,
                       aes(plate_x, plate_z, color = type),
                       size = 0.8) +
            add_zone() +
            centertitle() + theme_classic() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
            labs(
                x = 'Horizontal location (ft.)',
                y = 'Vertalical location (ft.)'
            ) +
            coord_equal() +
            xlim(-2.5, 2.5) +
            ylim(0, 5) +
            labs(title = paste(get_id(input$name)$Name))
    }, res = 96)
    output$plot2 <- renderPlot({
        req(length(input$count) > 0)
        get_id <- function(st){
            names <- str_to_lower(unlist(str_split(str_squish(st), " ")))
            if(length(names) == 3){
                names <- c(paste(names[1], names[2]), names[3])
            }
            chadwick %>%
                mutate(fname = str_to_lower(name_first),
                       lname = str_to_lower(name_last),
                       Name = paste(name_first,
                                    name_last)) %>%
                filter(fname == names[1],
                       lname == names[2]) %>%
                select(key_mlbam, Name)
        }
        centertitle <- function(){
            theme(plot.title = element_text(
                colour = "white", size = 14,
                hjust = 0.5, vjust = 0.8, angle = 0),
                plot.subtitle = element_text(
                    colour = "white", size = 14,
                    hjust = 0.5, vjust = 0.8, angle = 0))
        }
        pid <- get_id(input$name)$key_mlbam
        req(length(pid) > 0)
        
        th1 <- theme(plot.background =
                         element_rect(fill = "black"),
                     axis.text = element_text(colour = "white"),
                     axis.title = element_text(colour = "white"))
        
        sc_new <- filter(statcast_pitchers_2021,
                         Count %in% input$count,
                         Pitch %in% input$Pitch,
                         stand %in% input$stand,
                         pitcher == get_id(input$name)$key_mlbam)
        
        ggplot(data = sc_new, aes(x = release_speed, fill = Pitch)) +
            geom_density(alpha = 0.6) + theme_classic() + labs(
                             x = "Release Speed (MPH)",
                             y = "Density"
                         ) + theme(legend.title = element_blank())
    }, res = 96)
    output$plot3 <- renderPlot({
        req(length(input$count) > 0)
        get_id <- function(st){
            names <- str_to_lower(unlist(str_split(str_squish(st), " ")))
            if(length(names) == 3){
                names <- c(paste(names[1], names[2]), names[3])
            }
            chadwick %>%
                mutate(fname = str_to_lower(name_first),
                       lname = str_to_lower(name_last),
                       Name = paste(name_first,
                                    name_last)) %>%
                filter(fname == names[1],
                       lname == names[2]) %>%
                select(key_mlbam, Name)
        }
        centertitle <- function(){
            theme(plot.title = element_text(
                colour = "white", size = 14,
                hjust = 0.5, vjust = 0.8, angle = 0),
                plot.subtitle = element_text(
                    colour = "white", size = 14,
                    hjust = 0.5, vjust = 0.8, angle = 0))
        }
        pid <- get_id(input$name)$key_mlbam
        req(length(pid) > 0)
        
        th1 <- theme(plot.background =
                         element_rect(fill = "black"),
                     axis.text = element_text(colour = "white"),
                     axis.title = element_text(colour = "white"))
        
        sc_new <- filter(statcast_pitchers_2021,
                         Count %in% input$count,
                         Pitch %in% input$Pitch,
                         stand %in% input$stand,
                         pitcher == get_id(input$name)$key_mlbam)
        
        ggplot(data = sc_new, aes(x = hc_x, y = -hc_y)) +
            geom_segment(x = 128, xend = 33, y = -208, yend = -100, size = 0.7, lineend = 'round', color = 'grey66') +
            geom_segment(x = 128, xend = 223, y = -208, yend = -100, size = 0.7, lineend = 'round', color = 'grey66') +
            geom_curve(x = 33, xend = 223, y = -100, yend = -100,
                       curvature = -0.65, color = 'grey66') +
            coord_fixed() +
            geom_point(aes(color = events), alpha = 0.6, size = 2) +
            scale_x_continuous(limits = c(25, 225)) +
            scale_y_continuous(limits = c(-225, -25)) +
            labs(title = "Spray Chart",
                 color = "Hit Result") +
            theme_classic() +
            th1 +
            theme(plot.background = element_rect(fill = "white"),
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                  plot.title = element_text(hjust = 0.5),
                  axis.line.x = element_blank(),
                  axis.line.y = element_blank(),
                  axis.ticks = element_blank()) +
            scale_colour_discrete(na.translate = F)
    }, res = 96)
}

shinyApp(ui = ui, server = server)
