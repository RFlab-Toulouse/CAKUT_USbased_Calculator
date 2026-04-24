#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# usePackage <- function(p)
# {
#   if (!is.element(p, installed.packages()[,1]))
#     install.packages(p, dep = TRUE, force = TRUE)#, repos = "http://cran.us.r-project.org")
#   require(p, character.only = TRUE)
# }
#
#
# usePackage("shiny")
# usePackage("shinyjs")
# usePackage("shinythemes")
# usePackage("shinyWidgets")

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyWidgets)
library(cowplot)
library(grid)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(


  includeCSS("www/myStyle.css"),
  shinyjs::useShinyjs(), ## Include shinyjs

    # Application title
  # Application title
  fluidRow(br()),

  div(id = "titleId",
      p("This calculator estimates",
        span(style ='font-style:italic', "in utero" ),
        "This calculator estimates in utero postnatal kidney survival at the age of 2 years, in fetuses with bilateral CAKUT, based on standardized ultrasound readings described by Buffin-Meyer and al., NDT, 2024. Please cite this paper in your publications."),
      p(style = "font-style:italic",
        "Disclamer, this web site is provided for educational and informational purposes only and does not constitute providing medical advice or professional services."
        )

  ),
    titlePanel(

      title = NULL,

      windowTitle = "Prediction of renal postnatal outcome"

    ),

    # Sidebar with a slider input for number of bins


  fixedRow(
    column(5,

           #hr(),
           div(
             class = "well",
             h4("Ultrasound features"),
             hr(),


             radioGroupButtons(
               inputId = "var1",
               label = list("Reduced amniotic fluid",div(class = "info", actionButton(inputId = "click_info_var1",
                                                                                      #class = "btn-primary",
                                                                                      style="padding: 0px 3px 0px 3px;
                                                                                      border: none;
                                                                                      background-color:hsla(0, 20%, 98%, 0.815);
                                                                                      margin: 0px;
                                                                                      color:lightcyan;",
                                                                                      icon("question-circle",
                                                                                           style="color:black",
                                                                                           #class = "myIcoInfo",
                                                                                           title = "The volume of amniotic fluid is considered as reduced when the amniotic fluid index, assessed by sonography, is <5.")))),
               choices = c("No" = 0, "Yes" = 1),
               justified = TRUE,
               checkIcon = list(
                 yes = icon("ok",
                            lib = "glyphicon")),
               individual = TRUE,
             ),

             radioGroupButtons(
               inputId = "var2",
               label = list("Kidney dysplasia",div(class = "info", actionButton(inputId = "click_info_var2",
                                                                                #class = "btn-primary",
                                                                                style="padding: 0px 3px 0px 3px;
                                                                                      border: none;
                                                                                      background-color:hsla(0, 20%, 98%, 0.815);
                                                                                      margin: 0px;
                                                                                      color:lightcyan;",
                                                                                icon("question-circle",
                                                                                     style="color:black",
                                                                                     title = "Here, the clinical diagnosis of kidney dysplasia is only based on sonography; it corresponds to absent or reduced cortico-medullary differentiation in at least one kidney.")))),
               choices = c("No" = 0, "Yes" = 1),
               justified = TRUE,
               checkIcon = list(
                 yes = icon("ok",
                            lib = "glyphicon")),
               individual = TRUE
             ),

             radioGroupButtons(
               inputId = "var3",
               label = list("Kidney cysts",div(class = "info", actionButton(inputId = "click_info_var3",
                                                                            #class = "btn-primary",
                                                                            style="padding: 0px 3px 0px 3px;
                                                                                      border: none;
                                                                                      background-color:hsla(0, 20%, 98%, 0.815);
                                                                                      margin: 0px;
                                                                                      color:lightcyan;",
                                                                            icon("question-circle",
                                                                                 style="color:black",
                                                                                 title = "Here, the clinical diagnosis of kidney cysts is established when microcysts or macrocysts or hyperechonegicity* are detected in at least one kidney during sonographic examination. *Hyperechognicity is defined by a right kidney brighter than the liver and a left kidney brighter than the spleen.")))),
               choices = c("No" = 0, "Yes" = 1),
               justified = TRUE,
               checkIcon = list(
                 yes = icon("ok",
                            lib = "glyphicon")),
               individual = TRUE
             ),

           ),
           hr(),

           fixedRow(
             column(6,
                    div(
                      class="pull-left",
                      style="display:inline-block",
                      #disabled(
                      actionButton(
                        inputId="predict",
                        label="Calculate ",
                        class="btn-primary",
                        width = "200px",
                        style="font-weight:bold;font-size: 1.5em"
                      )
                      #)

                    )
             )

           ),
           fluidRow(br()),
           fluidRow(br()),
           fluidRow(br()),

           div(id = "logo",
             tags$img(src="rflabxx.png",width="70%")
           )

           ),

    column(7,

           fixedRow(column(12,
                           fluidRow(br()),
                           HTML(paste(' <ol style = "list-style-type: none;font-weight:bold;color: #760001;font-size: 16px">
                                The predicted PROBABILITY that your patient has a SEVERE postnatal renal outcome is:
                                ',fluidRow(br()),'
                                <li style = "font-weight:bold;font-size: 1em; color:black">',
                                tags$b(textOutput("ProbYes")),'</li>
                                 </ol>'))


           )),
           hr(),

           plotOutput("GraphPrediction",
                      height = "500px")

           )
           )



)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  prediction_model<-reactiveValues(pred = NULL)


  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$click_info_var1
    },

    handlerExpr = {

      showModal(modalDialog(
        title = "Helper",
        p(column(12,
                 p("The volume of amniotic fluid is considered as reduced when the amniotic fluid index,
                       assessed by sonography, is <5.")
        ),
        br()),
        size = "l", easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
      ))
    })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$click_info_var2
    },

    handlerExpr = {

      showModal(modalDialog(
        title = "Helper",
        p(column(12,
                 p("
        Here, the clinical diagnosis of kidney dysplasia is only based on sonography;
                       it corresponds to absent or reduced cortico-medullary differentiation in at least one kidney.")
        ),
        br()),
        size = "l", easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
      ))
    })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$click_info_var3
    },

    handlerExpr = {

      showModal(modalDialog(
        title = "Helper",
        p(column(12,
                 p("Here, the clinical diagnosis of kidney cysts is established when microcysts or macrocysts or hyperechonegicity* are detected in at least one kidney during sonographic examination.
                   *Hyperechognicity is defined by a right kidney brighter than the liver and a left kidney brighter than the spleen."),
        ),
        br()),
        size = "l", easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
      ))
    })



  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$predict
    },
    handlerExpr = {

      prediction_model$pred<-1/(1+exp(-(-6.635+4.039*as.numeric(input$var1)+2.983*as.numeric(input$var2)+1.987*as.numeric(input$var3))))


      output$ProbYes<-renderText({
        paste(round(prediction_model$pred,3)*100,"%")
      })




      output$GraphPrediction<-renderPlot({

        if(!is.null(prediction_model$pred)){
          # if(prediction_model$pred*100<=25){
          #   position_pred<-0.12
          # } else if(prediction_model$pred*100>25 & prediction_model$pred*100<=50){
          #   position_pred<-0.37
          # } else if(prediction_model$pred*100>50 & prediction_model$pred*100<=75){
          #   position_pred<-0.63
          # } else if(prediction_model$pred*100>75){
          #   position_pred<-0.88
          # }
          
          position_pred <-  prediction_model$pred

          data_legend<-data.frame(a = seq(0,100, by=10), b = seq(0,100, by=10), I = seq(0,100, by=10))

          labels<-paste0(c("Low risk - 0",25,50,75,"High risk - 100"),"%")

          ggplot_legend<-ggplot(data_legend) +
            aes(x = a, y = b, colour = I) +
            geom_point(size =100) +
            # scale_color_viridis_c(option = "turbo", direction = 1) +
            scale_color_gradient(low = "#32CD32", high = "red",
                                 guide = guide_colorbar(nbin = 120,
                                                        label.position = "left",
                                                        title.position = "right",
                                                        title.vjust = position_pred,

                                                        #ticks.linewidth = 0.2,
                                                        ticks = FALSE
                                                        ),
                                 labels = labels
            )+
            theme_gray() +

            labs(colour ="<---- Prediction of your patient")+
            theme(
              legend.title = element_text(size = rel(1.65), face = "bold", hjust = 0.5, color = "#760001"),
              legend.text = element_text(size = rel(1.65), face = "bold"),
              legend.key = element_rect(fill = "transparent", colour = NA),
              legend.key.size = unit(3, "cm"),
              legend.key.width = unit(2,"cm"),
              legend.background = element_rect(fill = "transparent", colour = NA),
              legend.text.align = 1,
              legend.spacing.x = unit(0.12,"cm")
            )




          legend <- get_legend(ggplot_legend)
          grid.newpage()
          grid.draw(legend)
        }

      })

    })


}

# Run the application
shinyApp(ui = ui, server = server)
