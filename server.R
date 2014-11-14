
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape2)

shinyServer(function(input, output) {


  datInput_att <- reactive({
    
    if (input$dataset == "basic") {
      load("data/basic.rda")
      dfA <- basic
    }
    if (input$dataset == "standard") {
      load("data/standard.rda")
      dfA <- standard
    }
    if (input$dataset == "social_policy") {
      load("data/social_policy.rda")
      dfA <- social_policy
    }
    
    
    dfA
  })
  

  

  
  
  datInput_map <- reactive({
    
    load("data/map_adm1.rda")
    map
    
  })
  
#   output$ui_class <- renderUI({
#     dfA <- datInput_att()
#     
#     dfA$class <- factor(dfA$class)
#     levelit1 <- levels(dfA$class)          
#     ind1 <- selectInput("class", h5("Pick a class"),choices = levelit1, selected=levelit1[1], width = "300px")
#     list(ind1)
#   })
  
  
  output$ui_indicator <- renderUI({
    dfA <- datInput_att()
    dfA <- dfA[!is.na(dfA$value),]
    
    dfA$variable <- factor(dfA$variable)
    levelit1 <- levels(dfA$variable)          
    ind1 <- selectInput("variable", h5("Pick an indicator"),choices = levelit1, selected="wdi_gdpc", width = "300px")
    list(ind1)
  })
  
  output$ui_timespan <- renderUI({
    dfA <- datInput_att()
    dfA <- dfA[!is.na(dfA$value),]
    
    dfA <- dfA[dfA$variable == as.character(input$variable),]
    
    maxyear <- max(dfA$year)
    minyear <- min(dfA$year)
    
    ip1 <- sliderInput('timespan', h5('Time span for time-series'), min=minyear, max=maxyear, value=c(minyear,maxyear), step = 1)
    
    list(ip1)
  })
  
  output$ui_year_rel <- renderUI({
    dfA <- datInput_att()
    
    dfA <- dfA[dfA$variable == as.character(input$variable),]
    
    maxyear <- max(dfA$year)
    minyear <- min(dfA$year)
    
    if (input$plots == "line") {
      ip2 <- ""
    }
    if (input$plots == "rela") {
      ip2 <- sliderInput('year_rel', h5('Year when all regions 100'), min=minyear, max=maxyear, value=minyear+2)
    }
    list(ip2)
  })
  
#   output$ui_year_map <- renderUI({
#     dfA <- datInput_att()
#     
#     dfA <- dfA[dfA$indicator_en == as.character(input$indicator_en),]
#     
#     maxyear <- max(dfA$variable)
#     minyear <- min(dfA$variable)
#     
#     if (input$maps == "Yes") {
#       ip3 <- sliderInput('year_map', h5('Year to map'), min=minyear, max=maxyear, value=maxyear)
#     }
#     if (input$maps == "No") {
#       ip3 <- ""
#     }
#     list(ip3)
#   })
  
  
  
  output$ui_adjust_axis <- renderUI({
    dfA <- datInput_att()
    dfA$indicator_en <- as.character(dfA$indicator_en)
    dfA <- dfA[dfA$indicator_en == as.character(input$indicator_en),]
    dfA$indicator_en <- factor(dfA$indicator_en)
    
    maxvalue <- max(dfA$value)
    minvalue <- min(dfA$value)
    
    if (input$plots == "line" | input$plots == "rela") {
      ip0 <- sliderInput('adjust_yaxis', h5('Adjust Y-axis'), min=0, max=maxvalue, value=c(0,maxvalue), step = 1)  
    }
    list(ip0)
  })
  
   output$ui_region <- renderUI({
     dfA <- datInput_att()
     if (input$subset_region == "cgoups") {
        ip1 <- checkboxGroupInput("subset_line", "Include:",
                           c("All" = "all",
                             "EU15" = "EU15",
                             "EU27" = "EU27",
                             "OECD" = "OECD",
                             "Nordic" = "nordic",
                             "Europe" = "europe",
                             "Asia" = "asia",
                             "Africa" = "africa",
                             "Oceania" = "oceania",
                             "South America" = "south_america",
                             "North America" = "north_america",
                             "Post-socialist" = "postsos"),
                           selected = "all")
      }
     
     if (input$subset_region == "cindiv") {
       levels_countries <- as.character(unique(dfA$cname))
       ip1 <- selectizeInput('subreg_cname', h6("Select individual countries:"), choices = levels_countries, multiple = TRUE)
     }
     list(ip1)
   })
  

  
  ### plot functions
  
  ## colors
  

  
#   output$small_plot <- renderUI({
#     if (input$maps == "Yes") {
#       absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
#                     top = 500, left = 350, right = "auto", bottom = "auto",
#                     width = 550, height = 450,
#                     
#                     plotOutput("plot_small", width = 550))
#     }
#     
#   }) 
#   
  
  
  
  
  
  plot_line <- function(x) {
    dfA <- datInput_att()
    dfA <- dfA[!is.na(dfA$value),]
    
    ## --------------------------------------------------------------------
    # aluesubsettaus
    if (input$subset_region == "cgoups") {
      
      df1 <- data.frame()
      df2 <- data.frame()
      df3 <- data.frame()
      df4 <- data.frame()
      df5 <- data.frame()
      df6 <- data.frame()
      df7 <- data.frame()
      df8 <- data.frame()
      df9 <- data.frame()
      df10 <- data.frame()
      df11 <- data.frame()
      df12 <- data.frame()
      
      
      if ("all"  %in% input$subset_line)    df1 <- dfA
      if ("EU15" %in% input$subset_line)    df2 <- dfA[dfA$EU15 == 1,]
      if ("EU27" %in%  input$subset_line)   df3 <- dfA[dfA$EU27 == 1,]
      if ("OECD" %in% input$subset_line)    df4 <- dfA[dfA$OECD == 1,]
      if ("nordic" %in% input$subset_line)  df5 <- dfA[dfA$nordic == 1,]
      if ("europe" %in% input$subset_line) df6 <- dfA[dfA$europe == 1,]
      if ("africa" %in% input$subset_line)  df7 <- dfA[dfA$africa == 1,]
      if ("south_america" %in% input$subset_line) df8 <- dfA[dfA$south_america == 1,]
      if ("north_america" %in% input$subset_line) df9<- dfA[dfA$north_america == 1,]
      if ("oceania" %in% input$subset_line)      df10 <- dfA[dfA$oceania == 1,]
      if ("asia" %in% input$subset_line)         df11 <- dfA[dfA$asia == 1,]
      if ("postsos" %in% input$subset_line)         df12 <- dfA[dfA$postsos == 1,]
      
      
      dfA <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
      dfA <- dfA[!duplicated(dfA[c("cname","year","variable")]),]  
      
    }
    if (input$subset_region == "cindiv") {
      dfA <- dfA[dfA$cname %in% input$subreg_cname,]
    }

    
    dfA <- dfA[dfA$variable == as.character(input$variable),]
    
    ## --------------------------------------------------------------------
    
    # relativise
    
#     if (input$plots == "rela") {
#       library(reshape2)
#       df.wide <- dcast(dfA, ID + indicator_en + region_en + federal_district + economic_regions ~ variable, value.var="value")
#       df.wide <- cbind(df.wide[1:5],df.wide[-1:-5] / eval(parse(text=paste0("df.wide$`",input$year_rel,"`"))) * 100)
#       dfA <- melt(df.wide, id.vars=c("ID","indicator_en","region_en","federal_district","economic_regions"))
#       dfA$variable <- as.numeric(levels(dfA$variable))[dfA$variable]
#     }
    
    #  if (input$plots == "line") {
    dfA <- dfA[dfA$year >= input$timespan[1],]
    dfA <- dfA[dfA$year <= input$timespan[2],]
    #  }  
    
    
#     if (input$subset_region == "economic_regions") dfA <- dfA[dfA$economic_regions %in% input$subreg_economic_regions,]
#     if (input$subset_region == "federal_district") dfA <- dfA[dfA$federal_district %in% input$subreg_federal_district,]
#     if (input$subset_region == "type_of_subject") dfA <- dfA[dfA$type_of_subject %in% input$subreg_subject,]
#     if (input$subset_region == "region") dfA <- dfA[dfA$region_en %in% input$subreg_region,]
#     
#     if (input$subset_region == "economic_regions") {
#       color_obj <- "factor(economic_regions)"
#       #
#       names(myColors) <- levels(dfA$economic_regions)
#       library(ggplot2)
#       colScale <- scale_colour_manual(name = "Economic Regions",
#                                       values = myColors)
#     }
#     if (input$subset_region == "federal_district") {
#       color_obj <- "factor(federal_district)"
#       #
#       names(myColors) <- levels(dfA$federal_district)
#       library(ggplot2)
#       colScale <- scale_colour_manual(name = "Federal Districts",
#                                       values = myColors)
#     }
#     if (input$subset_region == "type_of_subject") {
#       color_obj <- "factor(type_of_subject)"
#       #
#       names(myColors) <- levels(dfA$type_of_subject)
#       library(ggplot2)
#       colScale <- scale_colour_manual(name = "type_of_subject",
#                                       values = myColors)
#     }
#     if (input$subset_region == "region") {
#       color_obj <- "factor(region_en)"
#       #
#       library(ggplot2)
#       library(RColorBrewer)
#       colScale <- scale_colour_manual(name = "Regions",
#                                       values = myColors)
#     }
#     
#     library(ggplot2)
#     require(grid)
#     
#     if (input$plots == "rela") {
#       line_100 <- geom_hline(aes_string(yintercept = 100), colour="Black", linetype="dashed")
#       line_rel <- geom_vline(aes_string(xintercept = input$year_rel), colour="Black", linetype="dashed") #+
#       #annotate("text", x = input$year_rel , y=median(dfA$value, na.rm = TRUE)*2, label = "Year all 100", colour="Black", size=5)
#     }
#     if (input$plots != "rela") {
#       line_100 <- geom_blank()
#       line_rel <- geom_blank()
#     }
#     
#     if (input$maps == "Yes") {
#       line_map <- geom_vline(aes_string(xintercept = input$year_map), colour="Dim grey", linetype="dashed") #+
#       #     annotate("text",x = input$year_map ,y=median(dfA$value, na.rm = TRUE)*2,
#       #              label = "Map year",colour="Dim grey", size=5)
#     }
#     
#     if (input$maps == "No") {
#       line_map <- geom_blank()
#     }
    
    
    print(ggplot(dfA, aes_string(x="year",y="value",group="cname",color="cname")) +
            geom_point(size=3) + geom_line()  +
            labs(title = as.character(input$variable)) +
            theme_bw() +
            theme(text = element_text(family="Open Sans")) +
#             # map year annotation
#             line_map +
#             # ----------------------- #
#             # map relative annotation
#             line_100 +
#             line_rel +
#             
            theme(legend.position="none") +
            theme(legend.text=element_text(size=12)) +
            theme(legend.title = element_blank()) +
             guides(color=guide_legend(nrow=2)) +
            geom_text(data=merge(dfA, 
                                 aggregate(year ~ cname, dfA, max),
                                 by=c("year","cname"), all.y=TRUE),
                      aes(x=year, y = value, label=cname),
                      hjust=-0.1,vjust=-1,size=4) + 
            geom_text(data=merge(dfA, 
                                 aggregate(year ~ cname, dfA, min),
                                 by=c("year","cname"), all.y=TRUE),
                      aes(x=year, y = value, label=cname),
                      hjust=.5,vjust=-1,size=4) #+
            #coord_cartesian(xlim=c(min(dfA$year-1),max(dfA$year)+3),
            #                ylim=input$adjust_yaxis) +
            #colScale
    )
  }
  
output$plot_big <- renderPlot({
  
#   if (input$maps == "Yes") {
#     plot_map()
#   }
  
#   if (input$maps == "No") {
    plot_line()
#   }
})
  

## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##
##       Scatterplots
## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##


datInput_asso <- reactive({
  
  if (input$dataset_asso == "basic") {
    load("data/basic.rda")
    dfA <- basic
  }
  if (input$dataset_asso == "standard") {
    load("data/standard.rda")
    dfA <- standard
  }
  if (input$dataset_asso == "social_policy") {
    load("data/social_policy.rda")
    dfA <- social_policy
  }
  dfA
})


# X-var

# output$ui_class_x <- renderUI({
#   dfA <- datInput_att()
#   dfA$class <- factor(dfA$class)
#   levelit1 <- levels(dfA$class)          
#   ind1 <- selectInput("class_x", h5("Pick a class for X-var"),choices = levelit1, width = "300px")
#   list(ind1)
# })


output$ui_indicator_x <- renderUI({
  dfA <- datInput_asso()
  dfA <- dfA[!is.na(dfA$value),]
  
  dfA$variable <- factor(dfA$variable)
  levelit1 <- levels(dfA$variable)          
  ind1 <- selectInput("indicator_x", h5("Pick an indicator for X-var"),choices = levelit1, selected = "wdi_gdpc", width = "300px")
  list(ind1)
})

# Y-var

output$ui_indicator_y <- renderUI({
  dfA <- datInput_asso()
  dfA <- dfA[!is.na(dfA$value),]
  
  dfA$variable <- factor(dfA$variable)
  levelit1 <- levels(dfA$variable)          
  ind1 <- selectInput("indicator_y", h5("Pick an indicator for Y-var"),choices = levelit1, selected = "undp_hdi", width = "300px")
  list(ind1)
})


output$ui_year_asso <- renderUI({
  dfA <- datInput_asso()
  dfA <- dfA[!is.na(dfA$value),]
  
  dfA$cname <- as.character(dfA$cname)
  dfA$variable <- as.character(dfA$variable)
  
  #dfX <- dfA[dfA$variable == "undp_hdi",]
  #dfY <- dfA[dfA$variable == "wdi_gdpc",]
  
  dfX <- dfA[dfA$variable == as.character(input$indicator_x),]
  dfY <- dfA[dfA$variable == as.character(input$indicator_y),]
  
  dfZ <- merge(dfX,dfY,by=c("cname","year"))
  
  dfZ$value.x[dfZ$value.x == 0] <- NA
  dfZ$value.y[dfZ$value.y == 0] <- NA
  dfZ <- dfZ[!is.na(dfZ$value.x),]
  dfZ <- dfZ[!is.na(dfZ$value.y),]
    
  year_list <- unique(sort(dfZ$year, decreasing = TRUE))
  
  ip3 <- selectInput('year_asso', h5('Year'), choices= year_list,
                     selected = max(year_list))
  
  list(ip3)
})



# output$ui_region_asso <- renderUI({
#   dfA <- datInput_att()
#   levels_federal_district <- as.character(levels(dfA$federal_district))[-1]
#   levels_economic_regions <- as.character(levels(dfA$economic_regions))[-1]
#   levels_subject <- as.character(levels(dfA$type_of_subject))[-1]
#   levels_regions <- as.character(levels(dfA$region_en))[-1]
#   
#   ip0 <- h3("Subset the regions")
#   
#   if (input$subset_region_asso == "economic_regions") {
#     ip1 <- checkboxGroupInput("subreg_economic_regions_asso", h6("Select region:"),inline = TRUE, choices = levels_economic_regions, selected = levels_economic_regions)
#   }
#   if (input$subset_region_asso == "federal_district") {
#     ip1 <- checkboxGroupInput("subreg_federal_district_asso", h6("Select region:"),inline = TRUE, choices = levels_federal_district, selected = levels_federal_district)
#   }
#   if (input$subset_region_asso == "type_of_subject") {
#     ip1 <- checkboxGroupInput("subreg_subject_asso", h6("Select type of subject:"),inline = TRUE, choices = levels_subject, selected = levels_subject)
#   }
#   if (input$subset_region_asso == "region") {
#     ip1 <- selectizeInput('subreg_region_asso', h6("Select individual regions:"), choices = levels_regions, multiple = TRUE)
#   }
#   list(ip1)
# })


output$ui_adjust_asso_plot <- renderUI({
  
  ip2 <- h4("Adjust the plot ")
  ip3 <- selectInput('smooth_method', h5('Method for smoothing'), 
                     choices= c("no smoothing",
                                "loess",
                                "glm",
                                "lm"
                     ))
  
  list(ip2,ip3)
})


output$ui_region_asso <- renderUI({
  dfA <- datInput_asso()
  if (input$subset_region_asso == "cgoups") {
    ip1 <- checkboxGroupInput("subset_line_asso", "Include:",
                              c("All" = "all",
                                "EU15" = "EU15",
                                "EU27" = "EU27",
                                "OECD" = "OECD",
                                "Nordic" = "nordic",
                                "Europe" = "europe",
                                "Asia" = "asia",
                                "Africa" = "africa",
                                "Oceania" = "oceania",
                                "South America" = "south_america",
                                "North America" = "north_america",
                                "Post-socialist" = "postsos"),
                              selected = "all")
  }
  
  if (input$subset_region_asso == "cindiv") {
    levels_countries <- as.character(unique(dfA$cname))
    ip1 <- selectizeInput('subset_cname_asso', h6("Select individual countries:"), choices = levels_countries, multiple = TRUE)
  }
  list(ip1)
})


plot_asso <- function(x) {
  
  
  dfA <- datInput_asso()
  dfA <- dfA[!is.na(dfA$value),]
  
  ## --------------------------------------------------------------------
  # aluesubsettaus
  if (input$subset_region_asso == "cgoups") {
    
    df1 <- data.frame()
    df2 <- data.frame()
    df3 <- data.frame()
    df4 <- data.frame()
    df5 <- data.frame()
    df6 <- data.frame()
    df7 <- data.frame()
    df8 <- data.frame()
    df9 <- data.frame()
    df10 <- data.frame()
    df11 <- data.frame()
    df12 <- data.frame()
    
    
    if ("all"  %in% input$subset_line_asso)    df1 <- dfA
    if ("EU15" %in% input$subset_line_asso)    df2 <- dfA[dfA$EU15 == 1,]
    if ("EU27" %in%  input$subset_line_asso)   df3 <- dfA[dfA$EU27 == 1,]
    if ("OECD" %in% input$subset_line_asso)    df4 <- dfA[dfA$OECD == 1,]
    if ("nordic" %in% input$subset_line_asso)  df5 <- dfA[dfA$nordic == 1,]
    if ("europe" %in% input$subset_line_asso) df6 <- dfA[dfA$europe == 1,]
    if ("africa" %in% input$subset_line_asso)  df7 <- dfA[dfA$africa == 1,]
    if ("south_america" %in% input$subset_line_asso) df8 <- dfA[dfA$south_america == 1,]
    if ("north_america" %in% input$subset_line_asso) df9<- dfA[dfA$north_america == 1,]
    if ("oceania" %in% input$subset_line_asso)      df10 <- dfA[dfA$oceania == 1,]
    if ("asia" %in% input$subset_line_asso)         df11 <- dfA[dfA$asia == 1,]
    if ("postsos" %in% input$subset_line_asso)         df12 <- dfA[dfA$postsos == 1,]
    
    dfA <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
    dfA <- dfA[!duplicated(dfA[c("cname","year","variable")]),]  
    dfA <- na.omit(dfA) 
    
    
  }
  if (input$subset_region_asso == "cindiv") {
    dfA <- dfA[dfA$cname %in% input$subset_cname_asso,]
  }
  
  ## --------------------------------------------------------------------
  
  
  
  
  dfA$cname <- as.character(dfA$cname)
  dfA$variable <- as.character(dfA$variable)
  dfA$year <- as.numeric(dfA$year)
  
  dfX <- dfA[dfA$variable == "undp_hdi",]
  dfY <- dfA[dfA$variable == "wdi_gdpc",]
  
  dfX <- dfA[dfA$variable == as.character(input$indicator_x),]
  dfY <- dfA[dfA$variable == as.character(input$indicator_y),]
  
  dfZ <- merge(dfX,dfY,by=c("cname","year"))
  
  dfZ <- dfZ[dfZ$year == input$year_asso, ]
  
#   if (input$subset_region_asso == "economic_regions") dfZ <- dfZ[dfZ$economic_regions %in% input$subreg_economic_regions_asso,]
#   if (input$subset_region_asso == "federal_district") dfZ <- dfZ[dfZ$federal_district %in% input$subreg_federal_district_asso,]
#   if (input$subset_region_asso == "type_of_subject") dfZ <- dfZ[dfZ$type_of_subject %in% input$subreg_subject_asso,]
#   if (input$subset_region_asso == "region") dfZ <- dfZ[dfZ$region_en %in% input$subreg_region_asso,]
#   
#   
#   if (input$subset_region_asso == "economic_regions") {
#     color_obj <- "factor(economic_regions)"
#     #
#     names(myColors) <- levels(dfZ$economic_regions)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "Economic Regions",
#                                     values = myColors)
#   }
#   if (input$subset_region_asso == "federal_district") {
#     color_obj <- "factor(federal_district)"
#     #
#     names(myColors) <- levels(dfZ$federal_district)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "Federal Districts",
#                                     values = myColors)
#   }
#   if (input$subset_region_asso == "type_of_subject") {
#     color_obj <- "factor(type_of_subject)"
#     #
#     names(myColors) <- levels(dfZ$type_of_subject)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "type_of_subject",
#                                     values = myColors)
#   }
#   if (input$subset_region_asso == "region") {
#     color_obj <- "factor(region_en)"
#     #
#     library(ggplot2)
#     library(RColorBrewer)
#     colScale <- scale_colour_manual(name = "Regions",
#                                     values = myColors)
#   }
  
  
  
  if (input$smooth_method == "no smoothing") {
    smoothing <- geom_blank()
  }
  if (input$smooth_method == "loess") {
    smoothing <- geom_smooth(method = loess)
  }
  if (input$smooth_method == "glm") {
    smoothing <- geom_smooth(method = glm)
  }
  if (input$smooth_method == "lm") {
    smoothing <- geom_smooth(method = lm)
  }  
  
  
  print(ggplot(dfZ, aes_string(x="value.x",y="value.y",group="1",color="cname",label="cname")) + 
          geom_point(size=3) + geom_text(vjust=1, hjust=-.1,family = "Open Sans", size=3.5) +
          smoothing +
      #    coord_cartesian(xlim=input$axis_x,ylim=input$axis_y) +
       #   colScale +
          theme_bw() +
          labs(x=input$indicator_x,y=input$indicator_y) +
          theme(axis.title = element_text(face = "bold", size = 14)) +
          theme(legend.position =  "none")
  )
  
  
  
  
  
}

plot_asso_all <- function(x) {
  

  dfA <- datInput_asso()
  
  ## --------------------------------------------------------------------
  # aluesubsettaus
  if (input$subset_region_asso == "cgoups") {
    
    df1 <- data.frame()
    df2 <- data.frame()
    df3 <- data.frame()
    df4 <- data.frame()
    df5 <- data.frame()
    df6 <- data.frame()
    df7 <- data.frame()
    df8 <- data.frame()
    df9 <- data.frame()
    df10 <- data.frame()
    df11 <- data.frame()
    df12 <- data.frame()
    
    
    if ("all"  %in% input$subset_line_asso)    df1 <- dfA
    if ("EU15" %in% input$subset_line_asso)    df2 <- dfA[dfA$EU15 == 1,]
    if ("EU27" %in%  input$subset_line_asso)   df3 <- dfA[dfA$EU27 == 1,]
    if ("OECD" %in% input$subset_line_asso)    df4 <- dfA[dfA$OECD == 1,]
    if ("nordic" %in% input$subset_line_asso)  df5 <- dfA[dfA$nordic == 1,]
    if ("europe" %in% input$subset_line_asso) df6 <- dfA[dfA$europe == 1,]
    if ("africa" %in% input$subset_line_asso)  df7 <- dfA[dfA$africa == 1,]
    if ("south_america" %in% input$subset_line_asso) df8 <- dfA[dfA$south_america == 1,]
    if ("north_america" %in% input$subset_line_asso) df9<- dfA[dfA$north_america == 1,]
    if ("oceania" %in% input$subset_line_asso)      df10 <- dfA[dfA$oceania == 1,]
    if ("asia" %in% input$subset_line_asso)         df11 <- dfA[dfA$asia == 1,]
    if ("postsos" %in% input$subset_line_asso)         df12 <- dfA[dfA$postsos == 1,]
    
    dfA <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
    dfA <- dfA[!duplicated(dfA[c("cname","year","variable")]),]  
    dfA <- na.omit(dfA) 
    
    
  }
  if (input$subset_region_asso == "cindiv") {
    dfA <- dfA[dfA$cname %in% input$subset_cname_asso,]
  }
  
  ## --------------------------------------------------------------------

  
  dfA$cname <- as.character(dfA$cname)
  dfA$variable <- as.character(dfA$variable)
  dfA$year <- as.numeric(dfA$year)
  
  dfX <- dfA[dfA$variable == "undp_hdi",]
  dfY <- dfA[dfA$variable == "wdi_gdpc",]
  
  dfX <- dfA[dfA$variable == as.character(input$indicator_x),]
  dfY <- dfA[dfA$variable == as.character(input$indicator_y),]
  
  dfZ <- merge(dfX,dfY,by=c("cname","year"))
  
  #   dfZ$value.x[dfZ$value.x == 0] <- NA
  #   dfZ$value.y[dfZ$value.y == 0] <- NA
  dfZ <- dfZ[!is.na(dfZ$value.x),]
  dfZ <- dfZ[!is.na(dfZ$value.y),]
  
  
#   if (input$subset_region_asso == "economic_regions") dfZ <- dfZ[dfZ$economic_regions %in% input$subreg_economic_regions_asso,]
#   if (input$subset_region_asso == "federal_district") dfZ <- dfZ[dfZ$federal_district %in% input$subreg_federal_district_asso,]
#   if (input$subset_region_asso == "type_of_subject") dfZ <- dfZ[dfZ$type_of_subject %in% input$subreg_subject_asso,]
#   if (input$subset_region_asso == "region") dfZ <- dfZ[dfZ$region_en %in% input$subreg_region_asso,]
#   
#   
#   if (input$subset_region_asso == "economic_regions") {
#     color_obj <- "factor(economic_regions)"
#     #
#     names(myColors) <- levels(dfZ$economic_regions)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "Economic Regions",
#                                     values = myColors)
#   }
#   if (input$subset_region_asso == "federal_district") {
#     color_obj <- "factor(federal_district)"
#     #
#     names(myColors) <- levels(dfZ$federal_district)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "Federal Districts",
#                                     values = myColors)
#   }
#   if (input$subset_region_asso == "type_of_subject") {
#     color_obj <- "factor(type_of_subject)"
#     # 
#     names(myColors) <- levels(dfZ$type_of_subject)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "type_of_subject",
#                                     values = myColors)
#   }
#   if (input$subset_region_asso == "region") {
#     color_obj <- "factor(region_en)"
#     #
#     library(ggplot2)
#     library(RColorBrewer)
#     colScale <- scale_colour_manual(name = "Regions",
#                                     values = myColors)
#   }
  
  
  
  if (input$smooth_method == "no smoothing") {
    smoothing <- geom_blank()
  }
  if (input$smooth_method == "loess") {
    smoothing <- geom_smooth(method = loess)
  }
  if (input$smooth_method == "glm") {
    smoothing <- geom_smooth(method = glm)
  }
  if (input$smooth_method == "lm") {
    smoothing <- geom_smooth(method = lm)
  }  
  #aes_string(x="value.x",y="value.y",group="1",color=color_obj,label="region_en")
  
  plot <-  ggplot(dfZ, aes(x=value.x,y=value.y,group=1,label=cname,color=cname)) + 
    geom_point(size=3) + geom_text(vjust=1, hjust=-.1,family = "Open Sans", size=3.5) +
    smoothing +
    #coord_cartesian(xlim=input$axis_x,ylim=input$axis_y) +
    #colScale + # ei toimi nyt kun makroalueiden mukainen väritys ei päällä (ei toimi korrelaatiofunktion takia)
    theme_bw() +
    theme(legend.position =  "none") +
    labs(x=input$indicator_x,y=input$indicator_y) +
    theme(axis.title = element_text(face = "bold", size = 14)) +
    facet_wrap(~year, scale= "free")
  
  
  library(plyr)
  cors <- ddply(dfZ, .(year), summarise, cor = round(cor(value.x, value.y, "pairwise.complete.obs"), 2))
  print(  
    plot + geom_text(data=cors, aes(label=paste0("cor = ", cor, sep="")), 
                     x=-Inf,y=Inf,hjust=-.6,vjust=2,
                     family="Open Sans", size=6, color="Dim grey")
  )
  
}

output$plot_asso <- renderPlot({
  
  plot_asso()
  
})


output$plot_asso_all <- renderPlot({
  
  plot_asso_all()
  
})



## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##
##       Parallel Coordinates Plot
## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##

datInput_para <- reactive({
  
  if (input$dataset_para == "basic") {
    load("data/basic.rda")
    dfA <- basic
  }
  if (input$dataset_para == "standard") {
    load("data/standard.rda")
    dfA <- standard
  }
  if (input$dataset_para == "social_policy") {
    load("data/social_policy.rda")
    dfA <- social_policy
  }
  dfA
})



# 1-var

output$ui_indicator_var1 <- renderUI({
  dfA <- datInput_para()
    
  dfA$variable <- factor(dfA$variable)
  levelit1 <- levels(dfA$variable)          
  ind1 <- selectInput("indicator_var1", h5("Pick an indicator for variable 1"),choices = levelit1, selected = "undp_hdi", width = "300px")
  list(ind1)
})

# 2-var

output$ui_indicator_var2 <- renderUI({
  dfA <- datInput_para()
  
  dfA$variable <- factor(dfA$variable)
  levelit1 <- levels(dfA$variable)          
  ind1 <- selectInput("indicator_var2", h5("Pick an indicator for variable 2"),choices = levelit1, selected = "wdi_gdpc", width = "300px")
  list(ind1)
})


# 3-var

output$ui_indicator_var3 <- renderUI({
  dfA <- datInput_para()
  
  dfA$variable <- factor(dfA$variable)
  levelit1 <- levels(dfA$variable)          
  ind1 <- selectInput("indicator_var3", h5("Pick an indicator for variable 3"),choices = levelit1, selected = "wbgi_vae", width = "300px")
  list(ind1)
})


# 4-var

output$ui_indicator_var4 <- renderUI({
  dfA <- datInput_para()
  
  dfA$variable <- factor(dfA$variable)
  levelit1 <- levels(dfA$variable)          
  ind1 <- selectInput("indicator_var4", h5("Pick an indicator for variable 4"),choices = levelit1, selected = "solt_ginet", width = "300px")
  list(ind1)
})


output$ui_region_para <- renderUI({
  dfA <- datInput_para()
  if (input$subset_region_para == "cgoups") {
    ip1 <- checkboxGroupInput("subset_line_para", "Include:",
                              c("All" = "all",
                                "EU15" = "EU15",
                                "EU27" = "EU27",
                                "OECD" = "OECD",
                                "Nordic" = "nordic",
                                "Europe" = "europe",
                                "Asia" = "asia",
                                "Africa" = "africa",
                                "Oceania" = "oceania",
                                "South America" = "south_america",
                                "North America" = "north_america",
                                "Post-socialist" = "postsos"),
                              selected = "all")
  }
  
  if (input$subset_region_para == "cindiv") {
    levels_countries <- as.character(unique(dfA$cname))
    ip1 <- selectizeInput('subset_cname_para', h6("Select individual countries:"), choices = levels_countries, multiple = TRUE)
  }
  list(ip1)
})



output$ui_year_para <- renderUI({
  dfA <- datInput_para()
  
  
  
  df1 <- dfA[dfA$variable == as.character(input$indicator_var1),]
  df2 <- dfA[dfA$variable == as.character(input$indicator_var2),]
  df3 <- dfA[dfA$variable == as.character(input$indicator_var3),]
  df4 <- dfA[dfA$variable == as.character(input$indicator_var4),]
  
  dw1 <- dcast(df1, cname + year ~ variable, value.var="value")
  dw2 <- dcast(df2, cname + year ~ variable, value.var="value")
  dw3 <- dcast(df3, cname + year ~ variable, value.var="value")
  dw4 <- dcast(df4, cname + year ~ variable, value.var="value")
  
  dwx <- merge(dw1,dw2,by=c("cname","year"))
  dwx <- merge(dwx,dw3,by=c("cname","year"))
  dwx <- merge(dwx,dw4,by=c("cname","year"))
  
  dwx <- na.omit(dwx) 
  
  year_list <- sort(as.numeric(unique(dwx$year)))
  
  ip3 <- selectInput('year_para', h5('Year'), choices= year_list,
                     selected = max(year_list))
  
  list(ip3)
})


plot_para <- function(x) {
  
  
  dfA <- datInput_para()
  
  
  ## --------------------------------------------------------------------
  # aluesubsettaus
  if (input$subset_region_para == "cgoups") {
    
    df1 <- data.frame()
    df2 <- data.frame()
    df3 <- data.frame()
    df4 <- data.frame()
    df5 <- data.frame()
    df6 <- data.frame()
    df7 <- data.frame()
    df8 <- data.frame()
    df9 <- data.frame()
    df10 <- data.frame()
    df11 <- data.frame()
    df12 <- data.frame()
    
    
    if ("all"  %in% input$subset_line_para)    df1 <- dfA
    if ("EU15" %in% input$subset_line_para)    df2 <- dfA[dfA$EU15 == 1,]
    if ("EU27" %in%  input$subset_line_para)   df3 <- dfA[dfA$EU27 == 1,]
    if ("OECD" %in% input$subset_line_para)    df4 <- dfA[dfA$OECD == 1,]
    if ("nordic" %in% input$subset_line_para)  df5 <- dfA[dfA$nordic == 1,]
    if ("europe" %in% input$subset_line_para) df6 <- dfA[dfA$europe == 1,]
    if ("africa" %in% input$subset_line_para)  df7 <- dfA[dfA$africa == 1,]
    if ("south_america" %in% input$subset_line_para) df8 <- dfA[dfA$south_america == 1,]
    if ("north_america" %in% input$subset_line_para) df9<- dfA[dfA$north_america == 1,]
    if ("oceania" %in% input$subset_line_para)      df10 <- dfA[dfA$oceania == 1,]
    if ("asia" %in% input$subset_line_para)         df11 <- dfA[dfA$asia == 1,]
    if ("postsos" %in% input$subset_line_para)         df12 <- dfA[dfA$postsos == 1,]
    
    dfA <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
    dfA <- dfA[!duplicated(dfA[c("cname","year","variable")]),]  
    dfA <- na.omit(dfA) 
    
    
  }
  if (input$subset_region_para == "cindiv") {
    dfA <- dfA[dfA$cname %in% input$subset_cname_para,]
  }
  
  ## --------------------------------------------------------------------
  
  
  df1 <- dfA[dfA$variable == as.character(input$indicator_var1),]
  df2 <- dfA[dfA$variable == as.character(input$indicator_var2),]
  df3 <- dfA[dfA$variable == as.character(input$indicator_var3),]
  df4 <- dfA[dfA$variable == as.character(input$indicator_var4),]


#   df1 <- dfA[dfA$variable == "wdi_fe",]
#   df2 <- dfA[dfA$variable == "wdi_fr",]
#   df3 <- dfA[dfA$variable == "wdi_fw",]
#   df4 <- dfA[dfA$variable == "wdi_gdpc",]


# df1 <- df1[!duplicated(df1[c("cname","year","variable")]),]
# df2 <- df2[!duplicated(df2[c("cname","year","variable")]),]
# df3 <- df3[!duplicated(df3[c("cname","year","variable")]),]
# df4 <- df4[!duplicated(df4[c("cname","year","variable")]),]

  dw1 <- dcast(df1, cname + year ~ variable, value.var="value")
  dw2 <- dcast(df2, cname + year ~ variable, value.var="value")
  dw3 <- dcast(df3, cname + year ~ variable, value.var="value")
  dw4 <- dcast(df4, cname + year ~ variable, value.var="value")
  
  dwx <- merge(dw1,dw2,by=c("cname","year"))
  dwx <- merge(dwx,dw3,by=c("cname","year"))
  dwx <- merge(dwx,dw4,by=c("cname","year"))
  
  dwx <- na.omit(dwx) 
  
  
#   #dfZ <- dfZ[dfZ$variable == 2010,]
#   
#   if (input$subset_region_para == "economic_regions") dfZ <- dfZ[dfZ$economic_regions %in% input$subreg_economic_regions_para,]
#   if (input$subset_region_para == "federal_district") dfZ <- dfZ[dfZ$federal_district %in% input$subreg_federal_district_para,]
#   if (input$subset_region_para == "type_of_subject") dfZ <- dfZ[dfZ$type_of_subject %in% input$subreg_subject_para,]  
#   if (input$subset_region_para == "region") dfZ <- dfZ[dfZ$region_en %in% input$subreg_region_para,]
#   
#   if (input$subset_region_para == "economic_regions") {
#     color_obj <- "factor(economic_regions)"
#     #
#     names(myColors) <- levels(dfZ$economic_regions)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "Economic Regions",
#                                     values = myColors)
#   }
#   if (input$subset_region_para == "federal_district") {
#     color_obj <- "factor(federal_district)"
#     #
#     names(myColors) <- levels(dfZ$federal_district)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "Federal Districts",
#                                     values = myColors)
#   }
#   if (input$subset_region_para == "type_of_subject") {
#     color_obj <- "factor(type_of_subject)"
#     #
#     names(myColors) <- levels(dfZ$type_of_subject)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "type_of_subject",
#                                     values = myColors)
#   }
#   if (input$subset_region_para == "region") {
#     color_obj <- "factor(region_en)"
#     #
#     library(ggplot2)
#     library(RColorBrewer)
#     #myColors <- brewer.pal(5,"Set1")
#     colScale <- scale_colour_manual(name = "Regions",
#                                     values = myColors)
#   }
  
  
  dfZZ <- dwx
  
  dfZZ$ID <- 1:nrow(dfZZ)
  
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  
  dfZZ[3] <- range01(dfZZ[3])
  dfZZ[4] <- range01(dfZZ[4])
  dfZZ[5] <- range01(dfZZ[5])
  dfZZ[6] <- range01(dfZZ[6])
  
  if (input$inverse_scale1 == TRUE) dfZZ[3] <- 1- dfZZ[3] 
  if (input$inverse_scale2 == TRUE) dfZZ[4] <- 1- dfZZ[4] 
  if (input$inverse_scale3 == TRUE) dfZZ[5] <- 1- dfZZ[5] 
  if (input$inverse_scale4 == TRUE) dfZZ[6] <- 1- dfZZ[6] 
  
  df.x <- melt(dfZZ,id.vars=c("ID","cname","year"), 
               measure.vars = c(input$indicator_var1,
                                input$indicator_var2,
                                input$indicator_var3,
                                input$indicator_var4))
  
  df.x$variable <- factor(df.x$variable, levels=c(input$indicator_var1,
                                                  input$indicator_var2,
                                                  input$indicator_var3,
                                                  input$indicator_var4))
  
  df.x <- df.x[df.x$year == input$year_para,]
  
  enddata <- df.x[df.x$variable == input$indicator_var4,]
  thirddata <- df.x[df.x$variable == input$indicator_var3,]
  seconddata <- df.x[df.x$variable == input$indicator_var2,]
  begindata <- df.x[df.x$variable == input$indicator_var1,]
  
  
  
  p <- ggplot(df.x, aes_string(x="variable",y="value",group="ID",color="cname")) + 
    geom_line() +
    geom_text(data=enddata, 
              aes(x=4, y=value,label=cname),
              hjust=-.2,size=3.5,family="Open Sans") +
    geom_text(data=begindata, 
              aes(x=1, y=value,label=cname), 
              hjust=1,size=3.5,family="Open Sans") +
    geom_text(data=thirddata, 
              aes(x=3, y=value,label=cname), 
              hjust=.3,vjust=-.5,size=3.5,family="Open Sans") +
    geom_text(data=seconddata, 
              aes(x=2, y=value,label=cname), 
              hjust=.3,vjust=-.5,size=3.5,family="Open Sans") +
    theme_bw() +
    theme(text = element_text(family="Open Sans")) +
    theme(legend.position="none") +
    theme(legend.text=element_text(size=12)) +
    guides(color=guide_legend(nrow=2)) #+
    #colScale
  
  
  print(p)
  
  
  
}

plot_para_all <- function(x) {
  
  
  dfA <- datInput_para()
  
  
  ## --------------------------------------------------------------------
  # aluesubsettaus
  if (input$subset_region_para == "cgoups") {
    
    df1 <- data.frame()
    df2 <- data.frame()
    df3 <- data.frame()
    df4 <- data.frame()
    df5 <- data.frame()
    df6 <- data.frame()
    df7 <- data.frame()
    df8 <- data.frame()
    df9 <- data.frame()
    df10 <- data.frame()
    df11 <- data.frame()
    df12 <- data.frame()
    
    
    if ("all"  %in% input$subset_line_para)    df1 <- dfA
    if ("EU15" %in% input$subset_line_para)    df2 <- dfA[dfA$EU15 == 1,]
    if ("EU27" %in%  input$subset_line_para)   df3 <- dfA[dfA$EU27 == 1,]
    if ("OECD" %in% input$subset_line_para)    df4 <- dfA[dfA$OECD == 1,]
    if ("nordic" %in% input$subset_line_para)  df5 <- dfA[dfA$nordic == 1,]
    if ("europe" %in% input$subset_line_para) df6 <- dfA[dfA$europe == 1,]
    if ("africa" %in% input$subset_line_para)  df7 <- dfA[dfA$africa == 1,]
    if ("south_america" %in% input$subset_line_para) df8 <- dfA[dfA$south_america == 1,]
    if ("north_america" %in% input$subset_line_para) df9<- dfA[dfA$north_america == 1,]
    if ("oceania" %in% input$subset_line_para)      df10 <- dfA[dfA$oceania == 1,]
    if ("asia" %in% input$subset_line_para)         df11 <- dfA[dfA$asia == 1,]
    if ("postsos" %in% input$subset_line_para)         df12 <- dfA[dfA$postsos == 1,]
    
    dfA <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
    dfA <- dfA[!duplicated(dfA[c("cname","year","variable")]),]  
    dfA <- na.omit(dfA) 
    
    
  }
  if (input$subset_region_para == "cindiv") {
    dfA <- dfA[dfA$cname %in% input$subset_cname_para,]
  }
  
  ## --------------------------------------------------------------------
  
  
  
  
  
  df1 <- dfA[dfA$variable == as.character(input$indicator_var1),]
  df2 <- dfA[dfA$variable == as.character(input$indicator_var2),]
  df3 <- dfA[dfA$variable == as.character(input$indicator_var3),]
  df4 <- dfA[dfA$variable == as.character(input$indicator_var4),]
  
  #table(dfA$variable)
  
  #   df1 <- dfA[dfA$variable == "wdi_fe",]
  #   df2 <- dfA[dfA$variable == "wdi_fr",]
  #   df3 <- dfA[dfA$variable == "wdi_fw",]
  #   df4 <- dfA[dfA$variable == "wdi_gdpc",]
  
  dw1 <- dcast(df1, cname + year ~ variable, value.var="value")
  dw2 <- dcast(df2, cname + year ~ variable, value.var="value")
  dw3 <- dcast(df3, cname + year ~ variable, value.var="value")
  dw4 <- dcast(df4, cname + year ~ variable, value.var="value")
  
  dwx <- merge(dw1,dw2,by=c("cname","year"))
  dwx <- merge(dwx,dw3,by=c("cname","year"))
  dwx <- merge(dwx,dw4,by=c("cname","year"))
  
  dwx <- na.omit(dwx) 
  
  
  
#   #dfZ <- dfZ[dfZ$variable == 2010,]
#   
#   if (input$subset_region_para == "economic_regions") dfZ <- dfZ[dfZ$economic_regions %in% input$subreg_economic_regions_para,]
#   if (input$subset_region_para == "federal_district") dfZ <- dfZ[dfZ$federal_district %in% input$subreg_federal_district_para,]
#   if (input$subset_region_para == "type_of_subject") dfZ <- dfZ[dfZ$type_of_subject %in% input$subreg_subject_para,]  
#   if (input$subset_region_para == "region") dfZ <- dfZ[dfZ$region_en %in% input$subreg_region_para,]
#   
#   if (input$subset_region_para == "economic_regions") {
#     color_obj <- "factor(economic_regions)"
#     #
#     names(myColors) <- levels(dfZ$economic_regions)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "Economic Regions",
#                                     values = myColors)
#   }
#   if (input$subset_region_para == "federal_district") {
#     color_obj <- "factor(federal_district)"
#     #
#     names(myColors) <- levels(dfZ$federal_district)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "Federal Districts",
#                                     values = myColors)
#   }
#   if (input$subset_region_para == "type_of_subject") {
#     color_obj <- "factor(type_of_subject)"
#     #
#     names(myColors) <- levels(dfZ$type_of_subject)
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "type_of_subject",
#                                     values = myColors)
#   }
#   if (input$subset_region_para == "region") {
#     color_obj <- "factor(region_en)"
#     #
#     library(ggplot2)
#     colScale <- scale_colour_manual(name = "Regions",
#                                     values = myColors)
#   }
  
  
  dfZZ <- dwx

  dfZZ$ID <- 1:nrow(dfZZ)
  
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
dfZZ[3] <- range01(dfZZ[3])
dfZZ[4] <- range01(dfZZ[4])
dfZZ[5] <- range01(dfZZ[5])
dfZZ[6] <- range01(dfZZ[6])

if (input$inverse_scale1 == TRUE) dfZZ[3] <- 1- dfZZ[3] 
if (input$inverse_scale2 == TRUE) dfZZ[4] <- 1- dfZZ[4] 
if (input$inverse_scale3 == TRUE) dfZZ[5] <- 1- dfZZ[5] 
if (input$inverse_scale4 == TRUE) dfZZ[6] <- 1- dfZZ[6] 
  
df.x <- melt(dfZZ,id.vars=c("ID","cname","year"), 
             measure.vars = c(input$indicator_var1,
                              input$indicator_var2,
                              input$indicator_var3,
                              input$indicator_var4))

df.x$variable <- factor(df.x$variable, levels=c(input$indicator_var1,
                                                input$indicator_var2,
                                                input$indicator_var3,
                                                input$indicator_var4))
  
  #df.x <- df.x[df.x$year == input$year_para,]
  
  enddata <- df.x[df.x$variable == input$indicator_var4,]
  thirddata <- df.x[df.x$variable == input$indicator_var3,]
  seconddata <- df.x[df.x$variable == input$indicator_var2,]
  begindata <- df.x[df.x$variable == input$indicator_var1,]
  
  
  p <- ggplot(df.x, aes_string(x="variable",y="value",group="ID",color="cname")) +
    geom_line() + 
    geom_text(data=enddata, 
              aes(x=4, y=value,label=cname),
              hjust=-.2,size=3.5,family="Open Sans") +
    geom_text(data=begindata, 
              aes(x=1, y=value,label=cname), 
              hjust=1,size=3.5,family="Open Sans") +
    geom_text(data=thirddata, 
              aes(x=3, y=value,label=cname), 
              hjust=.3,vjust=-.5,size=3.5,family="Open Sans") +
    geom_text(data=seconddata, 
              aes(x=2, y=value,label=cname), 
              hjust=.3,vjust=-.5,size=3.5,family="Open Sans") +
    theme_bw() +
    theme(text = element_text(family="Open Sans")) +
    theme(legend.position="none") +
    theme(legend.text=element_text(size=12)) +
    guides(color=guide_legend(nrow=2)) +
    facet_wrap(~year) #+
    #colScale
  
  
  print(p)  
  
  
  
  
  
}

output$plot_para <- renderPlot({
  
  plot_para()  
  
})

output$plot_para_all <- renderPlot({
  
  plot_para_all()
  
  
})


  

})
