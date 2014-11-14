
shinyUI(navbarPage("Quality of Govenment data", id="nav",
                   
                   tabPanel("Time-series",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  # Hide the red error messages!!!
                                  tags$style(type="text/css",
                                            ".shiny-output-error { visibility: hidden; }",
                                            ".shiny-output-error:before { visibility: hidden; }"
                                  )
                                  
                                ),
                                
                                #plotOutput("distPlot", width="100%", height = "100%"),
                                plotOutput("plot_big", width="100%", height = "100%"),
                                
                                absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                                              top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 350, height = "auto",
                                              
                                              h3("Customize the analysis"),
                                              #submitButton("Refresh! - Always Press TWICE!", icon("refresh")),
                                              
                                              radioButtons("dataset", h5("Select data"), 
                                                           choices=c("Basic"="basic",
                                                                     "Standard"="standard",
                                                                     "Social Policy"="social_policy")),
                                              
                                              uiOutput("ui_indicator"),
                                              #                                               radioButtons("plots", h5("Look at"), choices=c("Absolute Time-series"="line",
                                              #                                                                                              "Relative Time-series"="rela")),
                                              uiOutput("ui_timespan"),
                                              #uiOutput("ui_year_rel"),
                                              #                                               radioButtons("maps", h5("Show"), choices=c("Only line-plot"= "No",
                                              #                                                                                          "Map with line-plot"="Yes")),
                                              #uiOutput("ui_year_map")
                                              radioButtons("subset_region", h5("Select countries"), 
                                                           choices=c("Country groups"="cgoups",
                                                                     "Individual countries"="cindiv")),
                                              uiOutput("ui_region")

                                              )


                                )
                            ),

tabPanel("Scatterplots",
         
         
         div(class="outer",
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css")
               
               
             ),
             
             tabsetPanel(type= "tabs", position= "above",
                         tabPanel("Single year plot", plotOutput("plot_asso", width="100%", height = "750px")),
                         tabPanel("All years plot", plotOutput("plot_asso_all", width="100%", height = "750px"))
             ),
             
             absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                           top = 60, left = "auto", right = 20, bottom = "auto",
                           width = 550, height = "auto",
                           
                           h3("Define the variables"),
                           
                           radioButtons("dataset_asso", h5("Select data"), 
                                        choices=c("Basic"="basic",
                                                  "Standard"="standard",
                                                  "Social Policy"="social_policy")),
                           
                           uiOutput("ui_indicator_x"),
                           uiOutput("ui_indicator_y"),
                           uiOutput("ui_year_asso"),
                           uiOutput("ui_adjust_asso_plot"),
                           radioButtons("subset_region_asso", h5("Select countries"), 
                                        choices=c("Country groups"="cgoups",
                                                  "Individual countries"="cindiv")),
                           uiOutput("ui_region_asso")
             )
             

         )),

tabPanel("Parallel coordinates",
         
         
         div(class="outer",
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css")
               
               
             ),
             
             tabsetPanel(type= "tabs", position= "above",
                         tabPanel("Single year plot", plotOutput("plot_para", width="100%", height = "750px")),
                         tabPanel("All years plot", plotOutput("plot_para_all", width="100%", height = "750px"))
             ),
             
             
             
             absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                           top = 60, left = "auto", right = 20, bottom = "auto",
                           width = 300, height = "auto",
                           
                           h3("Define the variables"),
                           
                           radioButtons("dataset_para", h5("Select data"), 
                                        choices=c("Basic"="basic",
                                                  "Standard"="standard",
                                                  "Social Policy"="social_policy")),
                           
                           uiOutput("ui_indicator_var1"),
                           uiOutput("ui_indicator_var2"),
                           uiOutput("ui_indicator_var3"),
                           uiOutput("ui_indicator_var4"),
                           uiOutput("ui_year_para"),
                           checkboxInput("inverse_scale1", h6("Inverse scale1:"), value = FALSE),
                           checkboxInput("inverse_scale2", h6("Inverse scale2:"), value = FALSE),
                           checkboxInput("inverse_scale3", h6("Inverse scale3:"), value = FALSE),
                           checkboxInput("inverse_scale4", h6("Inverse scale4:"), value = FALSE),
                           radioButtons("subset_region_para", h5("Select countries"), 
                                        choices=c("Country groups"="cgoups",
                                                  "Individual countries"="cindiv")),
                           uiOutput("ui_region_para")
                           
             )#,
             
#              absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
#                            top = 160, left = 350, right = "auto", bottom = "auto",
#                            width = 250, height = "auto",
#                            
#                            h3("Subset regions"),
#                            radioButtons("subset_region_para", h5("Scheme for subsetting regions"), 
#                                         choices=c("Economic Regions"="economic_regions",
#                                                   "Federal Districts"="federal_district",
#                                                   "Type of Federal Subject"="type_of_subject",
#                                                   "Regions"="region")),
#                            uiOutput("ui_region_para")
#                            
#              )
             
         )
         
         
),

tabPanel("About",
         
         
         
         h3("Created by"),
         tags$a(href="http://markuskainu.fi","Markus Kainu"),
         tags$br(),
         tags$a(href="http://twitter.com/muuankarski","@muuankarski"),
         
         h3("Documentation"),
         tags$a("For further details of underlying data see",href="http://ropengov.github.io/rqog/vignettes/rqog_tutorial.html","rqog-packages vignette."),
         tags$br(),
         tags$a("rqog-package is being developed within",href="http://ropengov.github.io","rOpenGov-project."),

         h3("Source code"),
         tags$a(href="https://github.com/muuankarski/regionapp-qog", "Source code available at Github"),
         
         h3("Licencing"),
         
         tags$a(href="http://creativecommons.org/licenses/by/4.0/","This work is licensed under a"),
         tags$img(src="https://i.creativecommons.org/l/by/4.0/88x31.png"),
         tags$a(href="http://creativecommons.org/licenses/by/4.0/","Creative Commons Attribution 4.0 International License")
         
         #tagsimg alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />
         
         
         
)



                   )
)