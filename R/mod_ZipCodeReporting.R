#' ZipCodeReporting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
library(sf)
mod_ZipCodeReporting_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      selectInput(ns("TimeFrame"),
                  label = "Select a Time Frame",
                  choices = c("Last 90 Days",
                              "Last 60 Days",
                              "Last 30 Days")), # End Timeframe select
      selectInput(ns('ZipCode'),
                  label = "Select a Zip Code",
                  choices  = Shapefile |>
                    dplyr::select(ZCTA5CE20) |>
                    dplyr::arrange(dplyr::desc(ZCTA5CE20)) |>
                    dplyr::mutate(ZCTA5CE20 = as.character(ZCTA5CE20)) |>
                    sf::st_drop_geometry()) , # End Zipcode Select
    width = 2), # End sidebarPanel
    mainPanel(
      fluidRow(uiOutput(ns("Title_Text"))),
      fluidRow(
        column(6,
        shinycssloaders::withSpinner(leaflet::leafletOutput(ns("CrimeMap"),
                                                            width = "100%"))), # End column
        column(6,
               shinycssloaders::withSpinner(plotly::plotlyOutput(ns("BarGraph"),
                                                                 width = "100%"))), # End column
      ),# End fluidRow
      fluidRow(
        shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("TimeSeries_Plot")))
     ), # End fluidRow
      width = 10
    )# End mainPanel
  )
}

#' ZipCodeReporting Server Functions
#'
#' @noRd
mod_ZipCodeReporting_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    DATA_Reactive <- reactive({

      Time_Filter <- switch(input$TimeFrame,
                            "Last 30 Days" = {"c(\"30 Days\")"},
                            "Last 60 Days" = {"c(\"30 Days\",
                                                 \"60 Days\")"},
                            "Last 90 Days" = {"c(\"30 Days\",
                                                 \"60 Days\",
                                                 \"90 Days\")"})

      CrimeData <- CrimeData |>
        dplyr::mutate(Time_Frame = dplyr::case_when(dplyr::between(DATEFILTER,
                                                                                      max(CrimeData$DATEFILTER, na.rm = TRUE)-30,
                                                                                      max(CrimeData$DATEFILTER, na.rm = TRUE))    ~ "30 Days",
                                                                       dplyr::between(DATEFILTER,
                                                                                      max(CrimeData$DATEFILTER, na.rm = TRUE)-60,
                                                                                      max(CrimeData$DATEFILTER, na.rm = TRUE)-31) ~ "60 Days",
                                                                       dplyr::between(DATEFILTER,
                                                                                      max(CrimeData$DATEFILTER, na.rm = TRUE)-90,
                                                                                      max(CrimeData$DATEFILTER, na.rm = TRUE)-61) ~ "90 Days",
                                                                       TRUE ~ "Wiggly_Pugs"
      )) |>
        dplyr::filter((Time_Frame %in% eval(parse(text = Time_Filter))) |>
                        tidyr::replace_na(TRUE))


      CrimeData <-  CoordConvert(CrimeData = CrimeData,
                                 Shapefile = Shapefile,
                                 Zipcode   = input$ZipCode)

      print(nrow(CrimeData))

      CrimeData

    })

    ZIPMAP_Reactive <- reactive({

      Shapefile <-  Shapefile |>
        dplyr::filter(ZCTA5CE20 == input$ZipCode)

      Shapefile


    })


    # output$BarGraph <- plotly::renderPlotly({
    #
    #
    #   DATA_Reactive |>
    #     sf::st_drop_geometry()
    #
    # })


    output$Title_Text <- renderUI({

      Title_Text <- paste0("<h2><b>",
                           "Crime Statistics for the Zip Code ",
                           input$ZipCode,
                           "</h2></b>")

      HTML(Title_Text)

    })

    output$CrimeMap <- leaflet::renderLeaflet({

      PlotData <- DATA_Reactive()


      pal <- leaflet::colorFactor(c("#6B95FF",
                                    "#AC9BF8",
                                    "#EC88B8",
                                    "#FF9A5C",
                                    "#FFC800",
                                    "#FFC565"),
                                  domain = NULL,
                                  na.color = "#808080")

      popup <- paste0("Crime Description: ",
                      as.character(PlotData$DESCRIPTION),
                      " <br> ",
                      "Date-Time of Offense: ",
                      as.character(PlotData$DATETIME))


      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::addCircles(data = PlotData,
                            color = NULL,
                            fillColor    = ~pal(Offense_Category),
                            fillOpacity  = 1,
                            weight       = 0,
                            popup        = popup

        ) |>
        leaflet::addPolygons(data         = ZIPMAP_Reactive(),
                             fillColor    = "grey70",
                             fillOpacity  = .1) |>
        leaflet::addLegend(pal    = pal,
                           values = PlotData$Offense_Category)


    })


    output$BarGraph <- plotly::renderPlotly({

      OutData <- DATA_Reactive() |>
        sf::st_drop_geometry() |>
        dplyr::select(Offense_Category,
                      FIREARM) |>
        dplyr::mutate(FIREARM = tidyr::replace_na(FIREARM,
                                                  "No Firearm Involved")) |>
        dplyr::group_by(Offense_Category,
                        FIREARM) |>
        dplyr::summarise(Count = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::group_by(Offense_Category) |>
        dplyr::mutate(FIREARM = factor(FIREARM,
                                       levels = c("No Firearm Involved",
                                                  "Firearm Involved"))) |>
        dplyr::group_by(Offense_Category) |>
        dplyr::arrange(FIREARM, .by_group = TRUE) |>
        dplyr::ungroup() |>
        dplyr::group_by(Offense_Category) |>
        dplyr::mutate(Group_Max = max(Count)) |>
        dplyr::arrange(Group_Max) |>
        dplyr::select(-c(Group_Max))


      Levels <- dplyr::pull(OutData |>
                              dplyr::select(Offense_Category) |>
                              dplyr::distinct(),
                            Offense_Category)

      OutData <- OutData |>
        dplyr::mutate(Offense_Category = factor(Offense_Category,
                                                levels = Levels))

      OutPlot <- OutData |>
        ggplot2::ggplot(mapping = ggplot2::aes(x = Offense_Category,
                                               y = Count,
                                               fill = FIREARM)) +
        ggplot2::geom_bar(stat = "identity",
                          position = ggplot2::position_dodge2(preserve = "single"))+
        ggplot2::theme_light() +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Distribution of Offenses by Category",
                      y     = "Frequency",
                      x     = NULL) +
        ggplot2::theme(plot.title   = ggplot2::element_text(hjust    = 0)        ,
                       axis.line    = ggplot2::element_line(size     = .2)       ,
                       panel.grid   = ggplot2::element_line(linetype = 2,
                                                            color    = "#6d6e71"),
                       panel.border = ggplot2::element_blank()                   ,
                       plot.caption = ggplot2::element_text(color    = "#6d6e71",
                                                            hjust    = 0)        ,
                       axis.text.x  = ggplot2::element_text(vjust    = 1,
                                                            hjust    = 1)        ,
                       legend.title = ggplot2::element_blank())

      plotly::ggplotly(OutPlot,
                       tooltip = c("text", "y")) |>
        plotly::layout(margin = list(l=0,
                                     r=0,
                                     b=10,
                                     t=90,
                                     pad=30)) %>%
        plotly::layout(legend = list(orientation = "h",   # show entries horizontally
                                     xanchor = "center",  # use center of legend as anchor
                                     x = 1,
                                     y = .15,
                                     title=list(text='')))
        # plotly::layout(title = list(text = paste0(title,
        #                                           '<br>',
        #                                           '<sup>',
        #                                           subtitle,
        #                                           '</sup>'),
        #                             font=list(size = 20)))


    })

    output$TimeSeries_Plot <- echarts4r::renderEcharts4r({

      Hours_Options <- data.frame(Hour = c("1am" ,
                                           "2am" ,
                                           "3am" ,
                                           "4am" ,
                                           "5am" ,
                                           "6am" ,
                                           "7am" ,
                                           "8am" ,
                                           "9am" ,
                                           "10am",
                                           "11am",
                                           "12pm",
                                           "1pm" ,
                                           "2pm" ,
                                           "3pm" ,
                                           "4pm" ,
                                           "5pm" ,
                                           "6pm" ,
                                           "7pm" ,
                                           "8pm" ,
                                           "9pm" ,
                                           "10pm",
                                           "11pm",
                                           "12am"))

      PlotData <- DATA_Reactive() |>
        sf::st_drop_geometry() |>
        dplyr::select(DATETIME) |>
        dplyr::mutate(DATETIME = as.numeric(DATETIME)) |>
        dplyr::mutate(DATETIME = as.POSIXct(DATETIME,
                                            origin = "1970-01-01",
                                            tz     = "America/Los_Angeles")) |>
        dplyr::mutate(Hour = as.numeric(format(DATETIME, "%H"))) |>
        dplyr::select(Hour) |>
        dplyr::group_by(Hour) |>
        dplyr::summarise(Count = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(Hour = dplyr::recode(Hour,
                                           "1"  = "1am" ,
                                           "2"  = "2am" ,
                                           "3"  = "3am" ,
                                           "4"  = "4am" ,
                                           "5"  = "5am" ,
                                           "6"  = "6am" ,
                                           "7"  = "7am" ,
                                           "8"  = "8am" ,
                                           "9"  = "9am" ,
                                           "10" = "10am",
                                           "11" = "11am",
                                           "12" = "12pm",
                                           "13" = "1pm" ,
                                           "14" = "2pm" ,
                                           "15" = "3pm" ,
                                           "16" = "4pm" ,
                                           "17" = "5pm" ,
                                           "18" = "6pm" ,
                                           "19" = "7pm" ,
                                           "20" = "8pm" ,
                                           "21" = "9pm" ,
                                           "22" = "10pm",
                                           "23" = "11pm",
                                           "0"  = "12am")) |>
        dplyr::full_join(Hours_Options,
                         by = "Hour") |>
        dplyr::mutate(Count = as.numeric(tidyr::replace_na(as.character(Count),
                                                           "0"))) |>
        dplyr::mutate(Hour = factor(Hour,
                                    levels = c("1am" ,
                                               "2am" ,
                                               "3am" ,
                                               "4am" ,
                                               "5am" ,
                                               "6am" ,
                                               "7am" ,
                                               "8am" ,
                                               "9am" ,
                                               "10am",
                                               "11am",
                                               "12pm",
                                               "1pm" ,
                                               "2pm" ,
                                               "3pm" ,
                                               "4pm" ,
                                               "5pm" ,
                                               "6pm" ,
                                               "7pm" ,
                                               "8pm" ,
                                               "9pm" ,
                                               "10pm",
                                               "11pm",
                                               "12am"))) |>
        dplyr::arrange(Hour)

      spline_int <- as.data.frame(spline(PlotData$Hour,
                                         PlotData$Count))

      PlotData |>
        echarts4r::e_charts(Hour) |>
        echarts4r::e_line(Count, smooth = TRUE) |>
        echarts4r::e_axis_labels(x = "Hour",
                                 y = "Frequency") |> # axis labels
        echarts4r::e_title("Number of Offense Commited by Hour of Day") |>  # move legend to the bottom
        echarts4r::e_tooltip(trigger = "axis"
        ) |>
        echarts4r::e_legend(right = 0)


    })



  })
}

## To be copied in the UI
# mod_ZipCodeReporting_ui("ZipCodeReporting_1")

## To be copied in the server
# mod_ZipCodeReporting_server("ZipCodeReporting_1")
