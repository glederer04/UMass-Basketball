#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gt)
library(gtExtras)
urls <- c('https://images.sidearmdev.com/crop?url=https://ss-dummy-data.s3.us-east-1.amazonaws.com/images/person-default.png&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2023/5/18/Cohen_Josh.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2022/9/26/Cronin_Jackson.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2022/9/26/Cross_Matt.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://ss-dummy-data.s3.us-east-1.amazonaws.com/images/person-default.png&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2023/5/18/Davis_Robert.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2022/9/26/Diggins_Rahsool.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://ss-dummy-data.s3.us-east-1.amazonaws.com/images/person-default.png&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://ss-dummy-data.s3.us-east-1.amazonaws.com/images/person-default.png&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://ss-dummy-data.s3.us-east-1.amazonaws.com/images/person-default.png&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2022/9/26/Marcus_Ryan.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2023/5/18/Mayhugh_Sawyer.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2023/6/5/Ndjigue_Jayden.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2022/9/26/Thompson_Gianni.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2022/9/26/Thompson_Keon.jpg&width=100&height=100&gravity=north&type=webp',
          'https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/2023/5/18/Worthy_Marqui.jpg&width=100&height=100&gravity=north&type=webp',
          'https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/UMass_Amherst_athletics_logo.svg/1200px-UMass_Amherst_athletics_logo.svg.png')
url_df <- data.frame(Column1 = urls)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {background-color: #7A1717;}
        .custom-title-panel {color: #FFFFFF;}
        .custom-sidebar-panel {background-color: #E3E3E3;}
        .custom-sidebar-text {color: #000000;}
        .my-tabpanel {
          font-size: 20px;
          height: 800px;
        }
        .navbar {background-color: #E3E3E3;}
        .navbar .nav > li > a {
          color: #000000;
          font-size: 20px;
          font-weight: bold;
        }")
    )
  ),
  navbarPage(
    title = "",
    tabPanel(
      "UMass MBB Practice",
      class = "my-tabpanel",
      mainPanel(
        img(src = 'https://pbs.twimg.com/media/FyNTeg8XwAA0hIW?format=jpg&name=medium', align = 'left')
      )
    ),
    tabPanel(
      "Daily Tables",
      class = "my-tabpanel",
      sidebarLayout(
        sidebarPanel(
          class = "custom-sidebar-panel",
          div(
            dateInput("selectedDate", "Select Date:", value = Sys.Date()),
            class = "custom-sidebar-text",
            p('Complete Totals and Basic Totals below')
          )
        ),
        mainPanel(
          gt_output("playerStatsTable"),
          gt_output("playerStatsTable2")
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$selectedDate, {
    selected_date <- input$selectedDate
    file_name <- paste0(format(selected_date, "%Y-%m-%d"), ' | Complete Practice Totals.rds')
    file_name2 <- paste0(format(selected_date, "%Y-%m-%d"), ' | Basic Practice Totals.rds')
    
    if (file.exists(file_name)) {
      tbl <- data.frame(readRDS(file = file_name))
      output$playerStatsTable <- render_gt({
        tbl$Rim_Per <- suppressWarnings(as.numeric(tbl$Rim_Per))
        tbl$Mid_Per <- suppressWarnings(as.numeric(tbl$Mid_Per))
        tbl$Three_Per <- suppressWarnings(as.numeric(tbl$Three_Per))
        tbl$url <- url_df$Column1
        #tbl$url <- full_practice_data$url
        column_averages_rim <- sapply(tbl[, c("Rim_Per")], mean)
        column_averages_mid <- sapply(tbl[, c("Mid_Per")], mean)
        column_averages_three <- sapply(tbl[, c("Three_Per")], mean)
        
        tbl %>%
          gt() %>%
          gt_theme_538() %>%
          tab_style(style = list(cell_fill(color = "gray80")), locations = cells_body(rows = seq(1, nrow(tbl), by = 2))) %>%
          tab_style(style = list(cell_fill(color = "gray70")), locations = cells_body(rows = nrow(tbl))) %>%
          cols_label(url = '', Player = md('**Player**'), Pts = md('**Pts**'), Rim = md('**Rim**'), Rim_Per = md('**Rim%**'), Mid = md('**Mid**'), Mid_Per = md('**Mid%**'), Three = md('**3pt.**'), Three_Per = md('**3pt%**'), Off_Reb = md('**OReb**'), Def_Reb = md('**DReb**'), Reb = md('**Reb**'), Ast = md('**Ast**'), Stl = md('**Stl**'), Blk = md('**Blk**'), Tov = md('**Tov**'), Charges = md('**Charges**'), Fouls = md('**Fouls**')) %>%
          tab_header(title = md('**Practice Stat Sheet**'), subtitle = paste0(format(selected_date, "%A, %B %d, %Y"), ' | Complete Totals')) %>%
          text_transform(locations = cells_body(vars(url)), fn = function(x) {web_image(url = x, height = px(22.5))}) %>%
          data_color(columns = c(Rim_Per), colors = scales::col_numeric(palette = paletteer::paletteer_d(palette = "ggsci::green_material") %>% as.character(), domain = column_averages_rim)) %>%
          data_color(columns = c(Mid_Per), colors = scales::col_numeric(palette = paletteer::paletteer_d(palette = "ggsci::green_material") %>% as.character(), domain = column_averages_mid)) %>%
          data_color(columns = c(Three_Per), colors = scales::col_numeric(palette = paletteer::paletteer_d(palette = "ggsci::green_material") %>% as.character(), domain = column_averages_three)) %>%
          tab_options(table.background.color = 'gray89', heading.background.color = 'gray89', column_labels.background.color = 'gray89') %>%
          tab_options(heading.title.font.size = 30, heading.subtitle.font.size = 15) %>%
          gt_add_divider(Ast) %>%
          gt_add_divider(Rim_Per) %>%
          gt_add_divider(Mid_Per) %>%
          gt_add_divider(Three_Per) %>%
          tab_options(column_labels.font.size = 13) %>%
          cols_align(align = c('center'), columns = c(Pts, Reb, Ast, Rim, Rim_Per, Mid, Mid_Per, Three, Three_Per, Off_Reb, Def_Reb, Stl, Blk, Tov, Charges, Fouls))
      })
      
    } else {
      output$playerStatsTable <- render_gt({
        empty_tbl <- data.frame(Placeholder = character(0))
        gt(empty_tbl) %>%
          cols_hide(columns = vars(everything())) %>%
          tab_header(title = md('**Practice Stat Sheet**'), subtitle = "No stats available for the selected date.") %>%
          tab_options(heading.title.font.size = 30, heading.subtitle.font.size = 15) %>%
          tab_options(table.background.color = '#7A1717', heading.background.color = '#7A1717', column_labels.background.color = '#7A1717')
      })
    }
    
    if (file.exists(file_name2)) {
      tbl2 <- data.frame(readRDS(file = file_name2))
      output$playerStatsTable2 <- render_gt({
        tbl2$url <- url_df$Column1
        #tbl2$url <- full_practice_data$url
        tbl2 %>%
          gt() %>%
          gt_theme_538() %>%
          tab_style(style = list(cell_fill(color = "gray80")), locations = cells_body(rows = seq(1, nrow(tbl), by = 2))) %>%
          tab_style(style = list(cell_fill(color = "gray70")), locations = cells_body(rows = nrow(tbl))) %>%
          cols_label(url = '', Player = md('**Player**'), Pts = md('**Pts**'), Two = md('**2pt.**'), Two_Per = md('**2pt%**'), Three = md('**3pt.**'), Three_Per = md('**3pt%**'), Off_Reb = md('**OReb**'), Def_Reb = md('**DReb**'), Reb = md('**Reb**'), Ast = md('**Ast**'), Stl = md('**Stl**'), Blk = md('**Blk**'), Tov = md('**Tov**'), Fouls = md('**Fouls**')) %>%
          tab_header(title = md('**Practice Stat Sheet**'), subtitle = paste0(format(selected_date, "%A, %B %d, %Y"), ' | Basic Totals')) %>%
          text_transform(locations = cells_body(vars(url)), fn = function(x) {web_image(url = x, height = px(22.5))}) %>%
          tab_options(table.background.color = 'gray89', heading.background.color = 'gray89', column_labels.background.color = 'gray89') %>%
          tab_options(heading.title.font.size = 30, heading.subtitle.font.size = 15) %>%
          cols_align(align = c('center'), columns = vars(Pts, Two, Two_Per, Three, Three_Per, Off_Reb, Def_Reb, Reb, Ast, Stl, Blk, Tov, Fouls)) %>%
          gt_add_divider(Ast) %>%
          gt_add_divider(Two_Per) %>%
          gt_add_divider(Three_Per)
      })
    } else {
      output$playerStatsTable2 <- render_gt({
        empty_tbl <- data.frame(Placeholder = character(0))
        gt(empty_tbl) %>%
          cols_hide(columns = vars(everything()))
        # tab_header(title = md('**Practice Stat Sheet**'), subtitle = "No stats available for the selected date.") %>%
        # tab_options(heading.title.font.size = 30, heading.subtitle.font.size = 15)
      })
    }
  })
}

shinyApp(ui = ui, server = server)
