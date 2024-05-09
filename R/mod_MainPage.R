#' MainPage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MainPage_ui <- function(id){
  ns <- NS(id)
  tagList(


      sidebarPanel(
        fluidRow(selectInput(inputId = ns("CrimeCategory"),
                               label   = "View by Category of Offense",
                               choices = c("All Offenses"     ,
                                           "Criminal Traffic" ,
                                           "Drug"             ,
                                           "DUI Offense"      ,
                                           "Property"         ,
                                           "Public Order"     ,
                                           "Violent",
                                           "Other"))),
        fluidRow(
          column(6,
                 uiOutput(ns("MinDateUI"))),
          column(6,
                 dateInput(inputId = ns("MaxDate"),
                           label = "End Date",
                           value = max(CrimeData$DATEFILTER, na.rm = TRUE),
                           min = min(CrimeData$DATEFILTER, na.rm = TRUE)  ,
                           max = max(CrimeData$DATEFILTER, na.rm = TRUE)))),
        fluidRow(shinyWidgets::materialSwitch(inputId = ns("Firearms"),
                                              label   = "Firearms Involved Crime",
                                              status  = "danger")),
                   width = 4),
      mainPanel(
        fluidRow(
          htmlOutput(ns("TitleText")),
        ),
        br(),
        br(),
        fluidRow(
          shinycssloaders::withSpinner(leaflet::leafletOutput(ns("CrimeMap"))),
          width = "100%"),
        br(),
        br(),
        fluidRow(
          shinycssloaders::withSpinner(echarts4r::echarts4rOutput(ns("TimeSeries_Plot"))),
          width = "100%"
        ),
        width = 8)



  )
}

#' MainPage Server Functions
#'
#' @noRd
mod_MainPage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    MINDATE_Reactive <- reactive({

      if(input$Firearms == TRUE){

        data <- CrimeData |>
          dplyr::filter(!is.na(FIREARM))

        data

      }else{

        data <- CrimeData

        data
      }

      FilterDate <- if(is.null(input$MaxDate)){

        max(data$DATEFILTER,
            na.rm = TRUE)

      }else{

        input$MaxDate

      }

      # print(FilterDate)

      # Max_Date <- max(CrimeData |>
      #                   dplyr::filter(DATEFILTER <= FilterDate),
      #                 na.rm = TRUE)


    })


    output$MinDateUI <- renderUI({

      if(input$Firearms == TRUE){

        data <- CrimeData |>
          dplyr::filter(!is.na(FIREARM))

        data

      }else{

        data <- CrimeData

        data
      }

      tagList(
        dateInput(inputId = ns("MinDate"),
                  label = "Start Date",
                  value = min(data$DATEFILTER, na.rm = TRUE),
                  min = min(data$DATEFILTER  , na.rm = TRUE),
                  max = MINDATE_Reactive())
      )

    })


    output$TitleText <- renderUI({

      req(!is.null(input$MinDate))
      req(!is.null(input$MaxDate))

      Number_Crimes <- if(input$CrimeCategory == "All Offenses"){

        OutNumber <- nrow(DATA_Reactive() |>
          dplyr::filter(Offense_Category %in% c("Criminal Traffic" ,
                                                "Drug"             ,
                                                "DUI Offense"      ,
                                                "Property"         ,
                                                "Public Order"     ,
                                                "Violent",
                                                "Other")) |>
          dplyr::filter(dplyr::between(DATEFILTER,
                                       input$MinDate,
                                       input$MaxDate)))

        OutNumber


      }else{

       input$CrimeCategory

        OutNumber <- nrow(DATA_Reactive() |>
                            dplyr::filter(stringr::str_detect(Offense_Category, input$CrimeCategory)) |>
                            dplyr::filter(dplyr::between(DATEFILTER,
                                                         input$MinDate,
                                                         input$MaxDate)))

        OutNumber

      }


      Count_Text <- if(input$CrimeCategory == "All Offenses"){

        OutText <- paste0(format(Number_Crimes, big.mark = ",", scientific = FALSE),
                          " Total Offenses")

        OutText

      }else{

        OutText <-      paste0(format(Number_Crimes, big.mark = ",", scientific = FALSE),
                               " ",
                               input$CrimeCategory,
                               " Offenses")

        OutText <- gsub("DUI Offense",
                        "DUI",
                        OutText)

        OutText

      }

      if(input$Firearms == TRUE){

        Count_Text <- paste0(Count_Text, " Involving Firearms")

      }else{

        Count_Text

      }

      out_text <- paste0("<b><font size = \"+2\"> Oakland 90 Day Crime Statistics </b></font><br>",
                         "<font size = \"+0\"><b>Selected Date Range:</b> ",
                         as.character(input$MinDate),
                         " to ",
                         as.character(input$MaxDate),
                         "<br>",
                         Count_Text)


      HTML(out_text)

    })

    DATA_Reactive <- reactive({


      if(input$Firearms == TRUE){

        data <- CrimeData |>
          dplyr::filter(!is.na(FIREARM))

        data

      }else{

        data <- CrimeData

        data
      }

      req(!is.null(input$MinDate))
      req(!is.null(input$MaxDate))

      if(input$CrimeCategory == "All Offenses"){

         filter_vec <- c("Criminal Traffic" ,
                         "Drug"             ,
                         "DUI Offense"      ,
                         "Property"         ,
                         "Public Order"     ,
                         "Violent",
                         "Other")

        outData <- data |>
          dplyr::filter(Offense_Category %in% filter_vec) |>
          dplyr::filter(dplyr::between(DATEFILTER,
                                       input$MinDate,
                                       input$MaxDate))

        outData

      }else{

        outData <- data |>
          dplyr::filter(stringr::str_detect(Offense_Category, input$CrimeCategory)) |>
          dplyr::filter(dplyr::between(DATEFILTER,
                                       input$MinDate,
                                       input$MaxDate))

        outData

      }



    })

    TIMESERIES_Reactive <- reactive({

      req(!is.null(input$MinDate))
      req(!is.null(input$MaxDate))

      # Firearms Crimes

      if(input$Firearms == TRUE){

        data <- CrimeData |>
          dplyr::filter(!is.na(FIREARM))

        data

      }else{

        data <- CrimeData

        data
      }

      # Filter Vector

      filter_vec <- if(input$CrimeCategory == "All Offenses"){

        c("Criminal Traffic" ,
          "Drug"             ,
          "DUI Offense"      ,
          "Property"         ,
          "Public Order"     ,
          "Violent",
          "Other")

      }else{

        input$CrimeCategory

      }

      # Data Processing

      if(input$CrimeCategory == "All Offenses"){

        OutData <- data |>
        dplyr::filter(Offense_Category %in% filter_vec) |>
        dplyr::filter(dplyr::between(DATEFILTER,
                                     input$MinDate,
                                     input$MaxDate)) |>
        dplyr::select(Offense_Category,
                      DATEFILTER) |>
        dplyr::group_by(Offense_Category,
                        DATEFILTER) |>
        dplyr::summarise(Frequency = dplyr::n()) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = "Offense_Category",
                           values_from = "Frequency",
                           id_cols = DATEFILTER) |>
        dplyr::mutate_at(dplyr::vars(dplyr::any_of(filter_vec)),
                         function(x){

                           x = as.character(x)

                           x = tidyr::replace_na(x, "0")

                           x = as.numeric(x)

                         }) |>
          dplyr::arrange(DATEFILTER) |>
          dplyr::mutate(DATEFILTER = as.Date(DATEFILTER,
                                              format = "%Y-%m-%d"))

        # Handle Cases when certain crime types are not present in data

        if(ncol(OutData) == 8){

          OutData

        }else{

          Current_Names <- names(OutData |>
                                   dplyr::select(-c(DATEFILTER)))

          Needed_Vars <- filter_vec[!filter_vec %in% Current_Names]

          for (col_name in Needed_Vars) {
            OutData[col_name] <- 0
          }


          testthat::test_that("All Crimes Time Series Cols",{

            testthat::expect_equal(ncol(OutData), 8)

          })

          OutData <- OutData |>
            dplyr::mutate_at(dplyr::vars(dplyr::all_of(filter_vec)),
                             function(x){

                               x = as.numeric(x)

                             })

          # print(ncol(OutD))

        }



      }else{

        OutData <- data |>
          dplyr::filter(stringr::str_detect(Offense_Category, input$CrimeCategory)) |>
          dplyr::filter(dplyr::between(DATEFILTER,
                                       input$MinDate,
                                       input$MaxDate)) |>
          dplyr::select(Offense_Category,
                 DATEFILTER) |>
          dplyr::group_by(Offense_Category,
                   DATEFILTER) |>
          dplyr::summarise(Frequency = dplyr::n()) |>
          dplyr::ungroup() |>
          dplyr::arrange(DATEFILTER) |>
          dplyr::mutate(DATEFILTER = as.Date(DATEFILTER,
                                              format = "%Y-%m-%d"))

        OutData

        # print(str(OutData))

      }

    })



    output$CrimeMap <- leaflet::renderLeaflet({

      PlotData <- sf::st_as_sf(DATA_Reactive(), coords = c("Lon", "Lat"), crs = 4326, agr = "constant")

      pal <- leaflet::colorFactor(c("#6B95FF",
                                    "#AC9BF8",
                                    "#EC88B8",
                                    "#FF9A5C",
                                    "#FFC800",
                                    "#F98C18",
                                    "#DF330E"),
                                  domain = c("Criminal Traffic",
                                             "Drug"            ,
                                             "DUI Offense"     ,
                                             "Other"           ,
                                             "Property"        ,
                                             "Public Order"    ,
                                             "Violent")        ,
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
        leaflet::addLegend(pal    = pal,
                           values = PlotData$Offense_Category)


    })


    output$TimeSeries_Plot <- echarts4r::renderEcharts4r({

      if(input$CrimeCategory == "All Offenses"){

        TIMESERIES_Reactive() |>
        dplyr::mutate(DATEFILTER = factor(DATEFILTER)) |>
        echarts4r::e_charts(DATEFILTER) |>
        echarts4r::e_line(`Criminal Traffic`, smooth = TRUE, color = "#6B95FF") |>
        echarts4r::e_line(`Drug`            , smooth = TRUE, color = "#AC9BF8") |>
        echarts4r::e_line(`DUI Offense`     , smooth = TRUE, color = "#EC88B8") |>
        echarts4r::e_line(`Other`           , smooth = TRUE, color = "#FF9A5C") |>
        echarts4r::e_line(`Property`        , smooth = TRUE, color = "#FFC800") |>
        echarts4r::e_line(`Public Order`    , smooth = TRUE, color = "#F98C18") |>
        echarts4r::e_line(`Violent`         , smooth = TRUE, color = "#DF330E") |>
        echarts4r::e_axis_labels(x = "Date",
                                 y = "Frequency") |> # axis labels
        echarts4r::e_title("Crimes per Day by Type") |>  # move legend to the bottom
        echarts4r::e_tooltip(trigger = "axis"
                             # formatter = htmlwidgets::JS("
                             #      function(params){
                             #        return( ''
                             #                'params.name +
                             #                '<br />wt: ' + params.value[0] +
                             #                '<br />mpg: ' + params.value[1])
                             #                }
                             #    ")
        ) |>
        echarts4r::e_legend(left = 1,
                            bottom = 1)

      }else{


        Ind_Color <- switch(input$CrimeCategory,
                            "Criminal Traffic" = {"#6B95FF"},
                            "Drug"             = {"#AC9BF8"},
                            "DUI Offense"      = {"#EC88B8"},
                            "Other"            = {"#FF9A5C"},
                            "Property"         = {"#FFC800"},
                            "Public Order"     = {"#F98C18"},
                            "Violent"          = {"#DF330E"})

        TIMESERIES_Reactive() |>
          dplyr::mutate(DATEFILTER = factor(DATEFILTER)) |>
          echarts4r::e_charts(DATEFILTER) |>
          echarts4r::e_line(Frequency, smooth = TRUE) |>
          echarts4r::e_axis_labels(x = "Date",
                                   y = "Frequency") |> # axis labels
          echarts4r::e_title("Crimes per Day by Type") |>  # move legend to the bottom
          echarts4r::e_tooltip(trigger = "axis"
                               # formatter = htmlwidgets::JS("
                               #      function(params){
                               #        return( ''
                               #                'params.name +
                               #                '<br />wt: ' + params.value[0] +
                               #                '<br />mpg: ' + params.value[1])
                               #                }
                               #    ")
          ) |>
          echarts4r::e_color(color = Ind_Color) |>
          echarts4r::e_legend(left = 1,
                              bottom = 1)


      }

    })

  })
}

## To be copied in the UI
# mod_MainPage_ui("MainPage_1")

## To be copied in the server
# mod_MainPage_server("MainPage_1")
