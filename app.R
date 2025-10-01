library(shiny)
library(readr)
library(tidyverse)
library(viridis)
library(ggpubr)
library(bslib)
library(ggridges)
library(reactable)
library(htmlwidgets)
library(webshot)

# Reading the data that I made from scraping in python
data <- read_csv('osu_pitch_data.csv', show_col_types = FALSE) |>
  
  # Removing pitches with no description, as they would cause issues
  filter(!is.na(pitch_description)) |>
  
  # Filtering the data to just be OSU pitchers
  filter(pitcher_team_id == 3111) |>
  
  # Turning the date into a lubridate date, and changing coordinates from feet to inches
  mutate(game_date = mdy(game_date),         
         plate_x = -12*px,
         plate_z = 12*pz)

# Reading data I made that has relevant bio information for each pitcher
playerinfo <- read_csv('osu_pitcher_info.csv', show_col_types = FALSE)

# Defining a colormap for each pitch, that will stay consistent regardless of user input.
pitch_types <- unique(data$pitch_description)
pitch_types_sorted <- pitch_types[order(nchar(pitch_types))]
pitch_colors <- setNames(viridis(length(pitch_types), option = "C"), pitch_types_sorted)


# The UI
ui <- page_sidebar(
  title = "OSU Pitching Dashboards 2025",
  
  # Making a sidebar
  sidebar = sidebar(
    
    # Allowing the user to select the pitcher of interest
    selectInput(inputId = "pname",
                label = "Pitcher Name",
                choices = unique(data$pitcher_name),
                selected = "Wyatt Queen"),
    
    # Allowing the user to select the dates they are interested in, defaulting to all
    # Details are in the server as it reacts to the selected pitcher
    checkboxGroupInput("games",
                       "Select Games:")
  ),
  
  # Creating the tabs that will be in the graphic. 
  # There's a lot of shenanigans with graphics and divs and styles, all because I wanted everything to scale based on window size.
  # Its a pain. Which is why all the cards and layouts are wrapped in divs.
  # There's probably a better way, but this is what I got to work, so it is what it is.
  tabsetPanel(id = "all_tabs",
              
              # The first tab, which contains everything I will show
              tabPanel("Full Graphic",
                       div(
                         style = "
                           aspect-ratio: 1/1;
                           height: 90vh;
                           max-width: 90vh;
                           margin: auto;",
                         
                         # I did everything in nested cards, for aesthetics, so this is the main card
                         card(
                           div(
                             
                             # Setting the height of the first row
                             style = "height: 15vh;",
                             
                             # This is the first row of visuals
                             layout_columns(
                               
                               # Calling the reactive headshot image
                               card(uiOutput("headshot", style = "height: 100%;")),
                               
                               # Calling the reactive bio information
                               card(uiOutput("info_card", style = "height: 100%;")),
                               
                               # Showing the beaver logo, which stays the same
                               card(
                                 div(
                                   style = "height: 100%; display: flex; align-items: center; justify-content: center;",
                                   img(src = "https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/oregonstate.sidearmsports.com/images/logos/site/site.png", 
                                       style = "height: 100%; width: auto; object-fit: contain;")
                                 ),
                                 style = "height: 100%;"
                               ),
                               
                               # Defining how wide each section in the row should be
                               col_widths = c(3, 6, 3),
                               style = "height: 100%"
                             )
                           ),
                           div(
                             
                             # Setting the height of the second row, the plots
                             style = "height: 30vh;",
                             
                             # Creating the second row
                             layout_columns(
                               
                               # Calling the velocity ridge plot, which can be clicked
                               card("Velocity",
                                    plotOutput("velocityPlot", height = "100%", click = "velocity_click"), 
                                    style = "height: 100%;"),
                               
                               # Calling the breakplot object, which can be clicked
                               card("Movement",
                                    plotOutput("breakPlot", height = "100%", click = "movement_click"), 
                                    style = "height: 100%;"),
                               
                               # Calling the location plot, which can be clicked
                               card("Location",
                                    plotOutput("locationPlot", height = "100%", click = "location_click"), 
                                    style = "height: 100%;"),
                               
                               # Defining the width of each plot, all equal and (roughly) square
                               col_widths = c(4, 4, 4),
                               style = "height: 100%"
                             )
                           ),
                           div(
                             
                             # Setting the height of the third row, the data table
                             style = "height: 20vh; width: 100%; max-width: 100vw;",
                             
                             # Formatting the data table, and calling the reactive object. Can be clicked!
                             card(
                               div(
                                 style = "
                                   width: 100%;
                                   display: flex;
                                   flex-direction: column;
                                   justify-content: flex-start;
                                   overflow-y: auto;
                                   max-height: 20vh;",
                                 
                                 # Fun note: I had to use random so it calls a new unique click each time, otherwise it only works one time. 
                                 # It's not like that for the plots, as each click calls a unique coordinate, so those are always different
                                 onclick = "Shiny.setInputValue('pitch_table_click', Math.random())",
                                 reactableOutput("pitch_table")
                               )
                             ),
                             style = "height: 100%; width: 100%;"
                           )
                         )
                       )
              ),
              
              # Adding a second tab, this one showing the velocity ridge plot in more detail
              tabPanel("Velocity",
                       div(
                         style = "padding: 2em; max-width: 1000px; margin: auto;",
                         
                         # Calling the bigger ridge plot
                         plotOutput("velocityPlot_large", height = "500px"),
                         
                         # Making a download button for the plot
                         downloadButton("download_velocity", "Download Plot", class = "btn-primary", style = "margin-bottom: 1em;"),
                         
                         # Adding descriptions to the plot
                         div(
                           style = "margin-top: 1em; font-size: 1.1em; line-height: 1.6;",
                           p("This plot shows the distribution in pitch velocity (in mph), sorted by pitch type."),
                           p("Each of the different ridges, and different colors, represents a different pitch type.
                            This can be used to see how consistent the pitcher is at consistently throwing at the same
                              speed, and the difference in speed between multiple of their pitches.")
                         )
                       )
              ),
              
              # Adding another tab, this one for the break plot
              tabPanel("Short Form Movement",
                       div(
                         style = "padding: 2em; max-width: 1000px; margin: auto;",
                         
                         # Calling the reactive object
                         plotOutput("breakPlot_large", height = "500px"),
                         
                         # Making download button
                         downloadButton("download_movement", "Download Plot", class = "btn-primary", style = "margin-bottom: 1em;"),
                         
                         # Adding descriptions
                         div(
                           style = "margin-top: 1em; font-size: 1.1em; line-height: 1.6;",
                           p("This plot shows the movement of each individual pitch, relevant to gravity. 
                             In other words, a pitch that appears at (0,0) on this graph would be one that follows 
                             standard projectile motion, where all ball movement is purely created by gravity. 
                             When throwing a baseball, the pitcher will impart spin on the ball, which will affect 
                             the flight of the ball through the air, through things like the Magnus force and seam effects."),
                           p("The x-axis on this plot is Horizontal Break (HB), and the y-axis is Induced Vertical Break (IVB),
                             each of which are averaged on the pitch metric table with further descriptions there."),
                           p("This is why many of the points are far from the origin of the plot, as baseball pitchers 
                             are often trying to make the ball move in unique ways. This plot is a way to 'see' movement 
                             of pitches without watching the actual flight of the ball, or graphing the 3D trajectory."),
                           p("A use case for this can be seeing how pitcher's movement changes between games. For example, 
                             if Wyatt Queen is selected, you can compare the points in yellow (fastballs) between 2/22 and 3/1, 
                             by using the checkboxes on the left. It can be seen that on 2/22, the fastballs are further to the right, 
                             indicating more horizontal movement, as compared to 3/1. This could be due to a number of factors, including temperature 
                             and a miscalibrated radar, but it could also indicate that the pitcher was trying to make the baseball move in 
                             different ways. By seeing which day returned better results, coaches could determine which shape is more desirable.")
                           
                         )
                       )
              ),
              
              # Another tab!
              tabPanel("Pitch Location",
                       div(
                         style = "padding: 2em; max-width: 1000px; margin: auto;",
                         
                         # Another call to the reactive objects!
                         plotOutput("locationPlot_large", height = "500px"),
                         
                         # Another download button
                         downloadButton("download_location", "Download Plot", class = "btn-primary", style = "margin-bottom: 1em;"),
                         
                         
                         # Another description!
                         div(
                           style = "margin-top: 1em; font-size: 1.1em; line-height: 1.6;",
                           p("This plot shows the location of the baseball when it crosses home plate, represented by a single point. 
                             The strike zone in baseball is defined by coordinates of the ball when it crosses the plate, and those bounds 
                             are represented by the black box in this plot."),
                           p("This plot can be used to show how accurate a pitcher is at throwing within/around the strike zone, 
                             and where they tend to throw a certain pitch type, as that is still stored in the color of the dot. 
                             For example, if the pitcher selected is Wyatt Queen, it can be observed that the yellow dots, representing fastballs, 
                             are generally concentrated near the top of the strike zone. However, the reddish dots that represent changeups are 
                             concentrated near the bottom of the zone. This indicates that he likely intends to throw each different type of 
                             pitch in a different area. From a baseball standpoint, this also makes sense, as changeups generally 
                             perform better when they are near the bottom of the zone, so Wyatt is doing a good job of locating that pitch.")
                           
                         )
                       )
              ),
              
              # Shocker! Another tab!
              tabPanel("Pitch Metric Table",
                       div(
                         style = "padding: 2em; max-width: 1000px; margin: auto;",
                         
                         # And another call to the server
                         reactableOutput("pitch_table_large"),
                         
                         # Another download button
                         downloadButton("download_table", "Download Table as Image", class = "btn-primary", style = "margin-bottom: 1em;"),
                         
                         
                         # And one more description, even bigger this time
                         div(
                           style = "margin-top: 1em; font-size: 1.1em; line-height: 1.6;",
                           p("This table takes some key metrics that are tracked on each pitch, and then summarizes them across the entire pitch type. 
                             Pitches are intended to be similar to each other, so we look at the average in each group to see what is expected of each 
                             kind of pitch."),
                           p("Explanation for the columns:"),
                           p("- Count: The number of each pitch type thrown, during the selected days."),
                           p("- Pitch%: The percentage of all pitches thrown that were that pitch type."),
                           p("- Velo: The pitch type's average velocity. This can be estimated from the ridge plot, but it's nice to have the actual number."),
                           p("- IVB: Induced Vertical Break (inches), by pitch type. This describes the vertical movement of the ball after being thrown, compared to what would be expected by gravity. Gravity will make any projectile, like a baseball, drop. If the baseball drops less than expected by gravity, it would have a positive IVB, and if it drops more, then it would be negative."),
                           p("- HB: Horizontal Break (inches), by pitch type. The horizontal break of the ball is the movement of the ball compared to the initial velocity vector, in the horizontal direction. If the ball essentially takes a curving left turn during the motion, it would return a negative value, and a right turn would return a positive value."),
                           p("- Spin: The rate in which the ball is spinning, in RPMs. When a baseball is thrown, it will have some amount of spin on it by the way we throw objects, and this is measured in RPMs. Certain pitch types will often have more or less spin, and some people are better or worse at spinning the ball.")
                           
                         )
                       )
              ),
              
              # ONE MORE TAB
              tabPanel("Info",
                       div(
                         style = "padding: 2em; max-width: 1000px; margin: auto;",
                         div(
                           style = "margin-top: 1em; font-size: 1.1em; line-height: 1.6;",
                           h4("Summary"),
                           p("This project was to recreate an app I had previously made in python using streamlit, but to use that idea to explore R and shiny data visualization tools.
                             Previously, in python, I had used matplotlib to make the plots, while here I was able to explore ggplot further to see if some of the same plotting methods existed.
                             ggplot ended up being a lot smoother and easier to use, and did a really good job of recreating what I had previously done.
                             I wanted to make the main page have a simple and sleek look to it, and then allow the viewer to expand on the plot or details that they were interested in learning more about, which shiny's cards functionality did quite well."),
                           h4("Data"),
                           p("This data was scraped using python from MLB-Stats API, and I then saved the information in python and downloaded it as a CSV, which I used here.
                             OSU Played 10 games in stadiums that were equipped with Statcast, which is MLB's data tracking platform, which published this data publicly.
                             Headshots, bio information (height/weight), and the beaver logo were scraped from the OSU baseball roster site, in python, and then stored as a CSV."),
                           h4("Data Cleaning"),
                           p("A quick note about data cleaning: The data was in a pretty easy to use format after scraping it, so there wasn't much manipulation needed.
                             However, the pitch classification into different pitch types (four-seam fastball, curveball, etc.) in the MLB-Stats API was originally done using an MLB proprietary machine learning model, which isn't always accurate, as it isn't trained on college baseball data, which is what this is.
                             With this, I manually changed the pitch classification into what I know each pitch should be, based on my specific knowledge of baseball and my knowledge of the players themselves.
                             This was done on a pitch-by-pitch basis, using a combination of factors like velocity, movement, spin rate, and spin axes, which I wasn't able to do through code. 
                             Thus, this isn't represented anywhere in the code, as it was done in the CSV itself. 
                             This is standard practice in baseball analytics, as for OSU baseball currently, we manually input the pitch type for each pitch based on these characteristics. 
                             None of the actual measurements were altered, just the groupings, which had been automatically assigned in the first place anyways, so I don't feel this is an issue, but I wanted to make it clear."),
                           h4("Variables/Information"),
                           p("All the variables and information are described in the Pitch Metric Table tab, or on the relevant plot tabs.")
                         )
                       )
              )
  )
)


# This is the server (wow)
# I had to include the session, in addition to the usual input/output, so that I could make clicking something jump around in the app
server <- function(input, output, session) {
  
  # Reactively filtering our data by the pitcher, so we can find what dates they pitched
  pitch_selected <- reactive({
    data |>
      
      # Filtering by selected pitcher
      filter(pitcher_name == input$pname) |>
      
      # Releveling the pitch_type and pitch_description by which pitch appears most often
      mutate(pitch_type = fct_rev(fct_infreq(pitch_type)),
             pitch_description = fct_rev(fct_infreq(pitch_description)))
  })
  
  # Updating the checkboxes to have the dates that the pitcher played in
  observe({
    dates = sort(unique(pitch_selected()$game_date))
    updateCheckboxGroupInput(session, 
                             "games",
                             label = NULL,
                             choices = dates,
                             selected = dates)
  })
  
  # Counting the number of games that are selected
  game_count <- reactive({
    length(input$games)
  })
  
  # Making the selections into a list
  game_dates <- reactive({
    input$games
  })
  
  # Filtering our data by the selected games
  filt <- reactive({
    pitch_selected() |>
      filter(game_date %in% game_dates())
  })
  
  # Making the output for the headshot image, and making it link to the player's OSU roster link
  output$headshot <- renderUI({
    
    # Filtering the info data by our pitcher selected
    selected_row <- playerinfo |>
      filter(pitcher_name == input$pname)
    
    # Defining the relevant links, the picture to show and the roster to link to
    img_link <- selected_row$pic_link
    profile_link <- selected_row$link
    
    # Formatting, then calling the link that was stored in the data
    div(
      style = "height: 100%; display: flex; align-items: center; justify-content: center;",
      img(src = selected_row$pic_link, 
          style = "height: 100%; width: auto; object-fit: contain;")
    )
  })
  
  # Making the output for the bio
  output$info_card <- renderUI({
    
    # Again selecting the row of the selected pitcher
    selected_row <- playerinfo |>
      filter(pitcher_name == input$pname)
    
    # Defining all the variables we're interested in
    throws <- selected_row$handedness
    height <- selected_row$height
    weight <- selected_row$weight
    
    
    # Formatting and making the output how we want it
    div(
      style = "height: 15vh; display: flex; flex-direction: column; justify-content: space-between; align-items: center;",
      tagList(
        
        # The top row is just the selected name, but bold and big
        div(input$pname,
            style = "font-weight: bold; font-size: clamp(16px, 3vh, 24px); line-height: 1; margin-bottom: 0.5vh;"),
        
        # Combining how they throw, the height, and the weight
        div(paste(throws, " | ", height, " | ", weight, " lbs"),
            style = "font-size: clamp(12px, 2vh, 18px); line-height: 1; margin-bottom: 0.4vh;"),
        
        # Showing the selected dates
        div(paste0(game_count(), ifelse(game_count() == 1, " game", " games")),
            style = "font-size: clamp(12px, 2vh, 18px); line-height: 1; margin-bottom: 0.4vh;"),
        
        # Adding a note for the reader that they can click for more info
        div("Click on a plot/table to learn more about it.",
            style = "font-style: italic; color: #666; font-size: clamp(10px, 1.5vh, 16px); line-height: 1; margin-bottom: 0;")
      )
    )
  })
  
  # Making the small velocity ridge plot
  output$velocityPlot <- renderPlot({
    
    # Standard ggplot
    ggplot(filt(), aes(x = start_speed, y = pitch_type)) +
      
      # Making a ridge plot, with the ridge going 1.5x its allotted height at max, and cutting off long trails where no values occur
      geom_density_ridges(aes(fill = pitch_description), scale = 1.5, rel_min_height = 0.01) +
      
      # Using our color mapping
      scale_fill_manual(values = pitch_colors) +
      
      # Clearing the background, setting a base font size
      theme_minimal(base_size = 12) +
      
      # Removing the legend, making the title blank (since we have a title in the card), and making the plot a square
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        aspect.ratio = 1
      )
  })
  
  # Making the small movement plot
  output$breakPlot <- renderPlot({
    
    # standard ggplot
    ggplot(filt(), aes(x = hb, y = ivb)) +
      
      # Adding axis lines at x=0 and y=0, to show quadrants easily
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = 0, color = "gray") +
      
      # Plotting the points, with a black outline and filled inside for clarity, and making them bigger and more visible
      geom_point(aes(fill = pitch_description), color = "black", shape = 21, size = 2.5) +
      
      # Colormap
      scale_fill_manual(values = pitch_colors) +
      
      # Setting coordinates
      coord_fixed(ratio = 1, xlim = c(-25, 25), ylim = c(-25, 25)) +
      
      # again minimal theme, removing unnecessary parts
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        axis.title = element_blank()
      )
  })
  
  # Making the small pitch location plot
  output$locationPlot <- renderPlot({
    
    # Standard ggplot
    ggplot(filt(), aes(x = plate_x, y = plate_z)) +
      
      # Making a rectangle for the strike zone, I found these to be accurate bounds in my previous python project
      geom_rect(xmin = -8.5, xmax = 8.5, ymin = 19.10, ymax = 40.62, 
                color = "black", fill = NA, linewidth = 1) +
      
      # Plotting points, again black outline and filled
      geom_point(aes(fill = pitch_description), color = "black", shape = 21, size = 2.5) +
      
      # Again color map
      scale_fill_manual(values = pitch_colors) +
      
      # setting coords
      coord_fixed(ratio = 1, xlim = c(-30, 30), ylim = c(0, 60)) +
      
      # theming, removing legend
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        axis.title = element_blank()
      )
  })
  
  # Creating events for each click of a plot, calling the current session and moving to the selected tab
  observeEvent(input$velocity_click, {
    updateTabsetPanel(session, inputId = "all_tabs", selected = "Velocity")
  })
  observeEvent(input$movement_click, {
    updateTabsetPanel(session, inputId = "all_tabs", selected = "Short Form Movement")
  })
  observeEvent(input$location_click, {
    updateTabsetPanel(session, inputId = "all_tabs", selected = "Pitch Location")
  })
  observeEvent(input$pitch_table_click, {
    updateTabsetPanel(session, inputId = "all_tabs", selected = "Pitch Metric Table")
  })
  
  # Making the summary based on our pitcher
  pitch_summary <- reactive({
    
    # Grouping our pitches by pitch type, and finding mean/count of values we are intersted in
    df <- filt() |>
      group_by(pitch_description) |>
      summarize(
        Count = n(),
        Velo = round(mean(start_speed, na.rm = TRUE), 1),
        IVB = round(mean(ivb, na.rm = TRUE), 1),
        HB = round(mean(hb, na.rm = TRUE), 1),
        Spin = round(mean(spin_rate, na.rm = TRUE)),
        .groups = "drop"
      )
    
    # Finding total pitches thrown for calculating percentage
    total_pitches <- sum(df$Count)
    
    # adding new columns/renaming, then ordering by count
    df |>
      mutate(
        `Pitch %` = round(Count / total_pitches * 100, 1),
        `Pitch Name` = pitch_description
      ) |>
      select(`Pitch Name`, Count, `Pitch %`, Velo, IVB, HB, Spin) |>
      arrange(desc(Count))
  })
  
  # Making the pitch table, with the reactable package. 
  # It's generally meant for interactive tables, but I felt it looked nice, even if I didn't need/want it to be interactable
  
  data_table_large <- reactive({reactable(
    
    # Passing the table our data
    pitch_summary(),
    
    # Making it compact, works better on smaller screens
    compact = TRUE,
    
    # Since we click on the table to see it but bigger, we don't want/need it to be sortable here
    sortable = FALSE,
    
    # Formatting, making a minimum width and height of each column, and aligning it
    defaultColDef = colDef(
      minWidth = 50,
      align = "center",
      style = list(minHeight = "1.5em")
    ),
    
    # Making the columns
    columns = list(
      
      # Making the pitch name column bigger, so it can show the whole description
      `Pitch Name` = colDef(
        minWidth = 150,
        
        # Mapping the color of each pitch type to the background color, and then adding a white outline around text for readability
        style = function(value) {
          pitch_key <- as.character(value)
          color <- pitch_colors[pitch_key]
          style_str <- paste0(
            "background-color: ", color, ";",
            "color: black;",
            "font-weight: bold;",
            "padding: 6px;",
            "border-radius: 4px;",
            "text-align: center;",
            "text-shadow: -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff;"
          )
          style_str
        }
      )
    ),
    
    # The table is just one page, so we don't need multiple
    pagination = FALSE,
    
    # Scaling the column to be at its full width
    fullWidth = TRUE
  )
  })
  
  output$pitch_table <- renderReactable({
    data_table_large()
  })
  
  # For the next bigger versions of the plots, I'm making them reactive objects first, then calling the render plot on them, so I can reuse the reactive object in the download button
  
  # Making the bigger version of the velocity ridge plot: all is the same, except different y labels, and then actual axes labels, and a bigger plot
  velocityPlot_large <- reactive({
    ggplot(filt(), aes(x = start_speed, y = pitch_description)) +
      geom_density_ridges(aes(fill = pitch_description), scale = 1.5, rel_min_height = 0.01) +
      scale_fill_manual(values = pitch_colors) +
      labs(x = "Velocity (mph)", y = "Pitch Type", title = "Pitch Velocity Distribution") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none")
  })
  
  # Making the break plot, but bigger: Also similar, but bigger, and with a legend, and axes labels
  breakPlot_large <- reactive({
    ggplot(filt(), aes(x = hb, y = ivb)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_point(aes(fill = pitch_description), color = "black", shape = 21, size = 3) +
      scale_fill_manual(values = pitch_colors, name = "Pitch Type", breaks = levels(filt()$pitch_description)) +
      coord_fixed(ratio = 1, xlim = c(-25, 25), ylim = c(-25, 25)) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        x = "Horizontal Break (inches)",
        y = "Induced Vertical Break (inches)",
        title = "Pitch Movement"
      )
  })
  
  # Same as the last, bigger location plot, with legend and axes labels
  locationPlot_large <- reactive({
    ggplot(filt(), aes(x = plate_x, y = plate_z)) +
      geom_rect(xmin = -8.5, xmax = 8.5, ymin = 19.10, ymax = 40.62,
                color = "black", linewidth = 1, alpha = 0) +
      geom_point(aes(fill = pitch_description), color = "black", shape = 21, size = 3) +
      scale_fill_manual(values = pitch_colors, name = "Pitch Type", breaks = levels(filt()$pitch_description)) +
      coord_fixed(ratio = 1, xlim = c(-30, 30), ylim = c(0, 60)) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        x = "Horizontal Location (inches)",
        y = "Vertical Location (inches)",
        title = "Pitch Location"
      )
  })
  
  # Rendering the big velocity plot
  output$velocityPlot_large <- renderPlot({
    velocityPlot_large()
  })
  
  # Rendering the break plot
  output$breakPlot_large <- renderPlot({
    breakPlot_large()
  })
  
  # Rendering the location plot
  output$locationPlot_large <- renderPlot({
    locationPlot_large()
  })
  
  # Creating a function that will make downloading easier, since I want the files to be named consistently
  make_filename <- function(plot_type) {
    # Finding the pitcher's last name
    last_name <- tolower(strsplit(input$pname, " ")[[1]][2])
    
    # Creating the title, given plot type
    paste0(plot_type, "_plot_", last_name, ".png")
  }
  
  # Downloading the Velocity plot
  output$download_velocity <- downloadHandler(
    filename = function() {
      make_filename("velocity")
    },
    content = function(file) {
      ggsave(file, plot = velocityPlot_large(), width = 8, height = 6, dpi = 300)
    }
  )
  
  # Downloading movement plot
  output$download_movement <- downloadHandler(
    filename = function() {
      make_filename("movement")
    },
    content = function(file) {
      ggsave(file, plot = breakPlot_large(), width = 8, height = 6, dpi = 300)
    }
  )
  
  # Downloading location plot
  output$download_location <- downloadHandler(
    filename = function() {
      make_filename("location")
    },
    content = function(file) {
      ggsave(file, plot = locationPlot_large(), width = 8, height = 6, dpi = 300)
    }
  )
  
  # Downloading table
  output$download_table <- downloadHandler(
    filename = function() {
      make_filename("metric_table")
    },
    content = function(file) {
      # Temporary file paths
      tmp_html <- tempfile(fileext = ".html")
      tmp_png <- tempfile(fileext = ".png")
      
      # Save reactable as HTML widget
      saveWidget(data_table_large(), tmp_html, selfcontained = TRUE)
      
      # Take snapshot using webshot
      webshot(tmp_html, tmp_png)
      
      # Copy the PNG to the file path shiny expects for download
      file.copy(tmp_png, file)
    }
  )
  
  # Making a bigger metric table, with a few visual changes
  output$pitch_table_large <- renderReactable({
    reactable(
      pitch_summary(),
      
      # It can be uncompact and big now!
      compact = FALSE,
      
      # Making each cell bordered
      bordered = TRUE,
      
      # Making each row striped grey and light grey
      striped = TRUE,
      
      # Making the rows highlightable
      highlight = TRUE,
      
      # Now it can be sortable, for user interaction
      sortable = TRUE,
      
      # Again, bigger, but setting all column sizes
      defaultColDef = colDef(
        minWidth = 80,
        align = "center",
        style = list(minHeight = "1.5em", fontSize = "1em")
      ),
      
      # Like before, making the name column big, and mapping colors to it
      columns = list(
        `Pitch Name` = colDef(
          minWidth = 200,
          style = function(value) {
            pitch_key <- as.character(value)
            color <- pitch_colors[pitch_key]
            paste0(
              "background-color: ", color, ";",
              "color: black;",
              "font-weight: bold;",
              "padding: 8px;",
              "border-radius: 4px;",
              "text-align: center;",
              "text-shadow: -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff;"
            )
          }
        )
      ),
      
      # Still just one page, and still want it wiiiiiiide
      pagination = FALSE,
      fullWidth = TRUE
    )
  })
}

# Thats it! Wow! I hope the code was easy to follow, and explained well for the confusing parts.
# The div parts for the formatting of the cards is still not the most clear to me, I mainly trusted what stackoverflow and html resources had to say.
# Scaling the table was incredibly frustrating, but I'm happy with how it came out overall, it gets the job done, and a ctrl - to zoom out makes it work fine. 
shinyApp(ui = ui, server = server)



