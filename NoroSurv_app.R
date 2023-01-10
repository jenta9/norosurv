#load libraries
pacman::p_load("dplyr", "ggplot2", "readxl", 
               "lubridate", "shiny", "bslib",
               "plotly", "tidytext", "rnaturalearth",
               "ggthemes")

# create "not in" function
'%notin%' <- Negate('%in%')

#dir.create('fonts')
# #file.copy("www/25381880192.ttf", "fonts")
# system('fc-cache -f fonts')
# #fonts to include
# #Avenir Next LT
# #Arial Nova
# #Sabon Next LT

NSUR <- read.csv("NoroSurv_for_reports_f.csv")


#Define the top 4 genotypes to highlight on the figure input$genotypes
#first indicate which genotypes should be included in "other categories"
#create new column specifying
cols <- c("Genogroup", "CtypeUser")
NSUR[cols] <- lapply(NSUR[cols], as.character) #convert to character
#create new column "genotype_other" 

NSUR <- NSUR %>%
  dplyr::mutate(genotype_other = 
                  case_when(Genogroup == "GII" & CtypeUser %notin% c("GII.4 Sydney", "GII.2", "GII.3", "GII.6") ~ "GII other", 
                            Genogroup == "GI" | Genogroup %in% c("GIX.1", "GIX") ~ "GI or GIX",
                            TRUE ~   "other"))
#next add the genotypes to highlight
NSUR <- NSUR %>%
  mutate(genotype_other =
           case_when(CtypeUser == "GII.4 Sydney" ~ "GII.4 Sydney",
                     CtypeUser == "GII.2" ~ "GII.2",
                     CtypeUser == "GII.3"~ "GII.3",
                     CtypeUser == "GII.6" ~"GII.6",
                     TRUE ~ genotype_other))


####### map data #########
#read in Country coordinates dataframe
CountryCoord <- read_excel("Countries_lat_long.xlsx")

#merge Country coordinates with NSUR by Country
NSUR$Country <- as.character(NSUR$Country) #convert first to character
NSUR <- NSUR %>%
  left_join(CountryCoord, by="Country")


#add global region column according to GPDS (differs from imported version in CountryCoord)
south_central_Am <- c("Nicaragua", "Chile", 
                      "Brazil", "Costa Rica")
north_Am <- c("Canada", "United States")
africa <- c("South Africa", "Zambia")
europe <- c("Spain", "Germany")
south_asia <- c("India", "Bangladesh")
east_asia <- c("Japan", "Taiwan", "Hong Kong")
southeast_asia <- c("Philippines", "Thailand")
West_pacific_oceania <- c("New Zealand", "Australia")


NSUR$Country <- as.character(NSUR$Country)
NSUR <- NSUR %>%
  dplyr::mutate(Region_GPDS = case_when(
    Country %in% south_central_Am ~ "South and Central America",
    Country %in% north_Am ~ "North America",
    Country %in% africa ~ "Africa",
    Country %in% europe ~ "Europe",
    Country %in% south_asia ~ "South Asia",
    Country %in% east_asia ~ "East Asia",
    Country %in% southeast_asia ~ "Southeast Asia",
    Country %in% West_pacific_oceania ~ "Oceania and West Pacific",
    TRUE ~ "check"
  ))

#hemispheres designation
southern <- c("Chile", "Brazil","South Africa", 
              "New Zealand", "Australia", "Zambia")
northern <- c("Nicaragua","Costa Rica","Canada", 
              "United States","Spain", "Germany", 
              "India", "Bangladesh",
              "Hong Kong", "Japan", "Taiwan", 
              "Philippines", "Thailand")
NSUR <- NSUR %>%
  dplyr::mutate(Hemisphere = case_when(
    Country %in% southern ~ "Southern",
    Country %in% northern ~ "Northern",
    TRUE ~ "check"
  ))

Country_order <- c("Chile", "Brazil","South Africa","Zambia", 
                   "New Zealand", "Australia", 
                   "Nicaragua","Costa Rica",
                   "Canada", "United States",
                   "Spain", "Germany",  
                   "India", "Bangladesh", 
                   "Hong Kong", "Japan", 
                   "Taiwan","Thailand","Philippines")

NSUR <- NSUR %>%
  dplyr::mutate(Age_cat = case_when(
    Age >=0 & Age <12 ~ "0-11",
    Age >=12 & Age <24  ~ "12-23",
    Age >=24 & Age <60  ~ "24-59",
    Age >=60  ~ ">=60",
    TRUE ~ "check"
  ))


Age_order <- c("0-11", "12-23", "24-59",  ">=60")

NSUR$Age_cat <- as.character(NSUR$Age_cat)



#genotypes that are not rare- to compare with those that are rare
not_rare <- c("GII.4 Sydney[P16]","GII.4 Sydney[P31]", "GII.4 Sydney[P4]", "GII.4 Sydney[P12]",
              "GII.4 Hong Kong[P31]", "GII.4 untypable[P4]", "GII.4 untypable[P31]", "GII.4[P31]",
              "GII.4[P16]", "GII.3[P12]", "GII.2[P16]", "GII.6[P7]",
              "GII.4[P4]", "GII.4[P1]", "GII.4[P12]",
              "GII.1[P1]","GII.1[P16]", "GII.1[P21]", "GII.1[P33]",
              "GII.2[P2]", "GII.2[P12]", "GII.2[P21]","GII.2[P31]", "GII.2[P30]",
              "GII.3[P16]", "GII.3[P21]","GII.3[P3]", "GII.3[P29]","GII.3[P33]",
              "GII.5[P5]", "GII.5[P22]", 
              "GII.6[P6]",
              "GII.7[P7]", "GII.8[P8]", "GII.9[P7]",
              "GII.10[P12]", "GII.10[P16]", "GII.10[P33]",
              "GII.12[P16]", "GII.12[P33]", "GII.12[P12]",
              "GII.13[P13]", "GII.13[P12]", "GII.13[P16]", "GII.13[P21]",
              "GII.14[P7]", "GII.16[P16]",
              "GII.17[P17]", "GII.17[P13]","GII.17[P31]", "GII.17[PNA]", "GII.17[P16]",
              "GIX.1[P15]", "GII.20[P7]",
              "GII.21[P21]","GII.22[P22]", "GII.23[P23]", "GII.24[P24]", "GII.NA[PNA]",
              "GI.1[P1]", "GI.2[P2]",
              "GI.3[P3]", "GI.3[P10]", "GI.3[P13]","GI.3[P14]",
              "GI.4[P4]", 
              "GI.5[P4]", "GI.5[P5]", "GI.5[P12]",
              "GI.6[P6]", "GI.6[P11]",
              "GI.7[P7]",
              "GI.8[P8]", "GI.9[P9]" 
)
rare_strains <- NSUR %>%
  filter(dual_type %notin% not_rare) %>%
  .$dual_type





#------------------------------------------
# User interface ----
ui <- 
  fluidPage(
    
    theme = bs_theme(version = 4, bootswatch = "lumen"),
    
    fluidRow(
      column(12,
             style = "margin:0px; border-top:30px solid; border-color:#3297C8; padding: 0px 0px 0px 0px;")),
    
    fluidRow(
      column(9, 
             offset = 0, 
             style = "font-size: 40px;font-face: bold; font-family: Sabon Next LT; padding: 0px 0px 0px 20px; margin:0px;",
             span("Noro", style =  "float:left; font-face: bold; color: #112B51"), span("Surv:", style = "color:#3297C8")),
      
      column(2, 
             offset = 1, 
             div(htmltools::img(src = knitr::image_uri(file.path("Main_page.png")), 
                                alt = 'Main_page',
                                style = 'position:absolute; top: 15px; bottom:15px; left:0px; right: 10px; margin: 0px; ',
                                width= 206,
                                height= 103)))
    ),
    
    fluidRow(
      column(12, 
             offset = 0, 
             style = "color: #335573; font-size: 22px; font-family: Avenir Next LT; padding: 0px 0px 0px 20px",
             "A global network for norovirus strain surveillance among children")),
    
    fluidRow(
      column(12, 
             offset = 0, 
             style = "color: #7D7D7D; font-size: 12px; font-family: Arial Nova;padding: 12px 0px 12px 20px",
             span("Update created:"), 
             span(format(Sys.time(), '%B %d, %Y'), style = "font-style:italic")
      )
    ),
    
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3, offset = 0,
        selectInput("data_set",
                    label = "Select data set:",
                    choices = c("All", unique(NSUR$dataset))),
        sliderInput(inputId = "date_range", 
                    label = "Choose Date Range:", 
                    min = min(as.Date(NSUR$Month_Year_floor)), 
                    max = max(as.Date(NSUR$Month_Year_floor)),
                    value=c(as.Date("2015-12-01"), 
                            max(as.Date(NSUR$Month_Year_floor))),
                    timeFormat="%b %Y"),
        sliderInput(inputId = "age_range", 
                    label = "Choose Age Range:", 
                    min = min(NSUR$Age), 
                    max = 71,
                    value=c(0, 59),
                    step = 3),
        selectInput("region", 
                    label= "Choose region:",
                    choices = c("All", unique(NSUR$Region_GPDS)),
                    selected = "All",
                    multiple = TRUE),
        selectInput("hemisphere", 
                    label= "Choose hemisphere:",
                    choices = c("All", unique(NSUR$Hemisphere)),
                    selected = "All"),
        selectInput("countries", 
                    label= "Choose countries:",
                    choices = c("All", levels(factor(Country_order))),
                    selected = "All",
                    multiple = TRUE),
        selectInput("age_cat", 
                    label= "Choose age group:",
                    choices = c("All", unique(NSUR$Age_cat)),
                    selected = "All",
                    multiple = TRUE),
        selectInput("symptoms",
                    label= "Select symptom status:",
                    choices =  c("All", unique(NSUR$Symptoms)),
                    selected = "Symptomatic"),
        selectInput("setting",
                    label = "Select setting:",
                    choices = c("All", unique(NSUR$Setting)),
                    selected = "All",
                    multiple = TRUE)
        
      ),
      
      
      mainPanel(
        width = 9, offset=0,
        tabsetPanel(
          tabPanel('Dashboard',
                   br(),
                   dashboardPage(
                     dashboardHeader(disable = TRUE),
                     dashboardSidebar(disable = TRUE),
                     dashboardBody(
                       fluidRow(
                         # Dynamic valueBoxes
                         valueBoxOutput("seq_ct"),
                         # valueBoxOutput("country_ct"),
                         valueBoxOutput("genotype_ct"),
                         valueBoxOutput("strain_ct"),
                       ),
                       fluidRow(
                         box(
                           title = "Genogroup Distribution",
                           width = 4,
                           background = "light-blue",
                           solidHeader = TRUE,
                           plotOutput("genogroup_donut", height = 300)
                         ),
                         box(
                           title = "Top Genotypes",
                           width = 4,
                           background = "light-blue",
                           solidHeader = TRUE,
                           plotOutput("genotype_bar", height = 300)
                         ),
                         box(
                           title = "Top Strains",
                           width = 4,
                           background = "light-blue",
                           solidHeader = TRUE,
                           plotOutput("strain_bar", height = 300)
                         )
                       )
                     )
                   ),
                   downloadButton("data", "Download Data"),
                   downloadButton("report", "Download Report")
          ),
          
          tabPanel('About', br(),
                   fluidPage(
                     theme = bs_theme(version = 4, bootswatch = "lumen"),
                     fluidRow(
                       column(9, 
                              offset = 0, 
                              style = "font-size: 40px;font-face: bold; font-family: Sabon Next LT; padding: 0px 0px 0px 20px; margin:0px;",
                              span("Welcome", style =  "float:left; font-face: bold; color: #112B51")
                       )
                     ),
                     fluidRow(
                       column(12, 
                              offset = 0, 
                              style = "color: #335573; font-size: 22px; font-family: Avenir Next LT; padding: 0px 0px 20px 20px",
                              span("NoroSurv provides data on global norovirus strain diversity in children younger than 5 years of age, capturing existing and emerging strains over time. Data will inform efforts to develop and adapt norovirus vaccine candidates; it will also aid in the evaluation of future vaccine efficacy by documenting baseline global strain diversity of noroviruses in children."
                              )
                       )
                     ),
                     fluidRow(
                       column(9, 
                              offset = 0, 
                              style = "font-size: 40px;font-face: bold; font-family: Sabon Next LT; padding: 0px 0px 0px 20px; margin:0px;",
                              span("Explore the data", style =  "float:left; color: #112B51")
                       )
                     ),
                     fluidRow(
                       column(12, 
                              offset = 0, 
                              style = "color: #335573; font-size: 22px; font-family: Avenir Next LT; font-face: bold; color: #112B51; padding: 0px 0px 20px 20px",
                              span("Explore the tabs to view the data. Make selections in the sidebar to filter data by date, child age, and geography."
                              )
                       )
                     ),
                     fluidRow(
                       column(9, 
                              offset = 0, 
                              style = "font-size: 40px;font-face: bold; font-family: Sabon Next LT; padding: 0px 0px 0px 20px; margin:0px;",
                              span("Participate", style =  "float:left; color: #112B51")
                       )
                     ),
                     fluidRow(
                       column(12, 
                              offset = 0, 
                              style = "color: #335573; font-size: 22px; font-family: Avenir Next LT; font-face: bold; color: #112B51; padding: 0px 0px 20px 20px",
                              span("Laboratories upload norovirus sequence data to the secure password-protected web-portal. Strains are typed internally according to the latest norovirus classification (PMID: 31483239). Aggregate data are visualized near realtime in this data exploration tool. See also our latest publication (PMID 33900173).")
                       )
                     )
                   )
          ),
          
          tabPanel('Map', plotlyOutput("map")),
          tabPanel('Genotypes', br(), plotlyOutput("genotypes")),
          tabPanel('GII.4 strains', br(), plotlyOutput("gii4")),
          tabPanel('All strains', br(), plotOutput("dualTypes"), height = "auto"),
          tabPanel('By region', br(), plotOutput("regional")),
          tabPanel('By country', br(), plotOutput("country_select")),
          tabPanel('By Age', br(), plotOutput("age_cat")),
          # tabPanel('Tables', br(), DT::DTOutput("dtable")),
          tabPanel('Acknowlegements', br(),
                   fluidPage(
                     theme = bs_theme(version = 4, bootswatch = "lumen"),
                     fluidRow(
                       column(9, 
                              offset = 0, 
                              style = "font-size: 40px;font-face: bold; font-family: Sabon Next LT; padding: 0px 0px 0px 20px; margin:0px;",
                              span("Contributors", style =  "float:left; font-face: bold; color: #112B51")
                       )
                     ),
                     fluidRow(
                       column(12, 
                              offset = 0, 
                              style = "color: #335573; font-size: 22px; font-family: Avenir Next LT; padding: 0px 0px 20px 20px",
                              span("Jan	Vinjé, Jennifer L.	Cannon,	Joseph	Bonifacio,	Filemon	Bucardo,	Javier	Buesa,	Leesa	Bruggink,	Martin	Chi-Wai	Chan, Tulio	M.	Fumian,	Sidhartha	Giri,	Mark	D.	Gonzalez,	Joanne	Hewitt,	Jih-Hui	Lin,	Janet	Mans,	Christian	Muñoz,	Chao-Yang	Pan,	Xiao-Li	Pang,	Corinna	Pietsch, Mustafiz	Rahman,	Naomi	Sakon,	Rangaraj	Selvarangan,	Hannah	Browne,	and Leslie	Barclay")
                       )
                     ),
                     fluidRow(
                       column(9, 
                              offset = 0, 
                              style = "font-size: 40px;font-face: bold; font-family: Sabon Next LT; padding: 0px 0px 0px 20px; margin:0px;",
                              span("Acknowledgments", style =  "float:left; font-face: bold; color: #112B51")
                       )
                     ),
                     fluidRow(
                       column(12, 
                              offset = 0, 
                              style = "color: #335573; font-size: 22px; font-family: Avenir Next LT; padding: 0px 0px 20px 20px",
                              span("We thank the NoroSurv international laboratory teams involved in sample collection, sequence analysis, and reporting, which includes Mary Ann Igoy, C. Eures Iyar Oasin, and Mayan Lumandas at the Research Institute for Tropical Medicine; Noemi Navarro-Lleó at the University of Valencia; Lin-yao Zhang at the Chinese University of Hong Kong; K. Maheswari at Christian Medical College; Margarita Lay at the Universidad de Antofagasta; Gary McAucliffe and Terri Swager at LabPLUS; Dawn Croucher at the Institute of Environmental Science and Research; Shu-Chun Chiu at the Taiwan Centers for Disease Control; Nicola Page at the South Africa National Institute for Communicable Diseases; Thalia Huynh, Tasha Padilla, Christine Morales, and Debra Wadford at the California Department of Public Health; Kanti Pabbaraju at Alberta Precision Laboratory; Mohammad Enayet Hossain at the International Centre for Diarrhoeal Disease Research, Bangladesh; and Ferdaus Hassan, Dithi Banerjee, Chris Harrison, and Mary Moffat at Children’s Mercy Hospitals and Clinics.")
                       )
                     ),
                     fluidRow(
                       column(12, 
                              offset = 0, 
                              style = "color: #335573; font-size: 22px; font-family: Avenir Next LT; padding: 0px 0px 20px 20px",
                              span("This data exploration tool was created by Jennifer Cannon.")
                       )
                     )
                     
                   )
          )
        )
      )
    )
  )


# shinyApp(ui, server)
# 

#------------------------------------------
# Server logic ----
server <- function(input, output, session) {
  
  rval_filters <- reactive({
    req(input$data_set)
    req(input$date_range)
    req(input$age_range)
    req(input$region)
    req(input$hemisphere)
    req(input$countries)
    req(input$age_cat)
    req(input$symptoms)
    req(input$setting)
    
    data <- NSUR
    
    #filter data set NoroSurv, GPDS, or All
    if (input$data_set != "All"){
      data <- data %>%
        filter(dataset %in% input$data_set)
    } else {
      data 
    }
    
    validate(need(nrow(data)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
    #filter based on date range
    data <- data %>%
      filter(Month_Year_floor >= input$date_range[1] & Month_Year_floor <= input$date_range[2])
    data
    
    
    #filter based on age range
    data <- data %>%
      filter(Age >= input$age_range[1] & Age <= input$age_range[2])
    data
    
    
    #filter country
    data <- data
    if (input$countries != "All") {
      data <- data %>%
        filter(Country %in% input$countries)
    } else {
      data 
    }
    
    validate(need(nrow(data)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
    #filter region
    data <- data
    if (input$region != "All") {
      data <- data %>%
        filter(Region_GPDS %in% input$region)
    } else {
      data 
    }
    
    validate(need(nrow(data)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
    
    #filter  hemisphere
    data <- data
    if (input$hemisphere != "All") {
      data <- data %>%
        filter(Hemisphere %in% input$hemisphere)
    } else {
      data 
    }
    
    validate(need(nrow(data)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
    #filter age cat
    data <- data
    if (input$age_cat != "All") {
      data <- data %>%
        filter(Age_cat %in% input$age_cat)
    } else {
      data 
    }
    
    validate(need(nrow(data)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
    
    #filter symptoms
    data <- data
    if (input$symptoms != "All") {
      data <- data %>%
        filter(Symptoms %in% input$symptoms)
    } else {
      data 
    }
    
    validate(need(nrow(data)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
    #filter setting
    data <- data
    if (input$setting != "All") {
      data <- data %>%
        filter(Setting %in% input$setting)
    } else {
      data 
    }
    
    validate(need(nrow(data)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    
    data
    
  })
  
  
  
  #I use this function for reactive height adjustment 
  height_var <- function(){
    height_calc <- rval_filters()
    return (30*length(unique(height_calc$dual_type)))}
  
  #I use this function for reactive height adjustment 
  height_var2 <- function(){
    height_calc <- rval_filters()
    return (60*length(unique(height_calc$dual_type)))}
  
  
  output$map <- renderPlotly({
    
    #summarize data by country
    by_country <- rval_filters() %>%
      group_by(Country)%>%
      dplyr::summarise(n = n())
    
    #add country coordinates to summarized dataframe
    by_country <- by_country %>%
      left_join(CountryCoord, by= "Country")
    
    #get world country polygons at specified scale using rnaturalearth package
    countries <- ne_countries(scale=110)
    
    
    #make breaks for circle size and alpha
    mybreaks <- c(50, 100, 200, 300)
    
    static_map <- ggplot() +
      suppressWarnings(geom_polygon(data=countries, aes(x=long, y=lat, group=group),color="white", fill="grey48", alpha = 0.6, lwd = .25))+
      suppressWarnings(geom_point(data = by_country, aes(x=long, y=lat, group = Country, size = n, 
                                                         text = paste('</br>', Country,'</br> sequences: ', n), color=n))) +
      coord_sf(expand=FALSE)+
      #scale_size_continuous(range=c(3,7), breaks=mybreaks) +
      #scale_alpha_continuous(range=c(1, 0.5), breaks=mybreaks) +
      
      
      theme_void()  +
      theme_map() +
      theme(
        plot.margin = margin(l=0, r=2, b=0, t=0, unit= "cm"),
        legend.position = "none")
    
    fig <- ggplotly(static_map, tooltip = "text", autosize = F, height = 700, width = 1400)
    fig 
  })
  
  
  
  output$genotypes <- renderPlotly({
    
    #rename data set with selected filters
    Genotype_4236 <- rval_filters()
    
    #create a summary table of the data
    Highlight_genotype_summary <- Genotype_4236 %>%
      group_by(Year, Month_Year_floor, genotype_other) %>%
      summarise(count = n())
    
    #duplicate 'genotype_other' column for graphing purposes
    Highlight_genotype_summary <- Highlight_genotype_summary %>%
      mutate(genotype_other_2 = genotype_other)
    
    
    # reorder the genotypes for the figure
    genotype_other_order <- c("GII.4 Sydney", "GII.3", "GII.2",  "GII.6", "GII other", "GI or GIX")
    Highlight_genotype_summary$genotype_other <-  factor(Highlight_genotype_summary$genotype_other, levels= genotype_other_order)
    
    # finally the graph
    highlight_figure <- Highlight_genotype_summary %>%
      ggplot(aes(x=as.Date(Month_Year_floor), y= count)) +
      geom_line(data=Highlight_genotype_summary %>% dplyr::select(-genotype_other), aes(group=genotype_other_2), color="grey", size=0.5, alpha=0.5) +
      geom_line(aes(group=genotype_other,color=genotype_other,
                    text = paste('</br>', genotype_other, '</br>', Month_Year_floor ,'</br> sequences: ', count)),
                size=1.2 ) +
      #geom_text(aes(group=genotype_other, label= genotype_other),x = Inf, y = 100, hjust = 0.5)+
      scale_color_manual(values=c("GII.4 Sydney" = "#4C7A99", "GII.2" = "#A40004", "GII.3" = "#EBBF2C",   "GII.6" = "#FF6C00" ,  "GII other" = "#6358A6", "GI or GIX" =  "#7CCC5F"))+
      #scale_x_date(date_breaks = "1 year", minor_breaks = "6 months", date_labels = "%Y") +
      scale_x_date(breaks = seq(as.Date("2016-01-01"), as.Date("2022-01-01"), by="6 months"), date_labels = "%b\n%y") +
      theme_minimal() +
      theme(
        legend.position="none",
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title.y = element_text(family = "Avenir Next LT", face = "bold", size = 16, color="#3297C8"),
        axis.title.x = element_blank(),
        axis.ticks.x= element_line(),
        axis.text.x = element_text(family = "Arial Nova", size = 8, color = "#112B51"),
        axis.text.y = element_text(family = "Arial Nova", size = 12, color = "#112B51"),
        strip.text = element_text(family = "Arial Nova", size = 16, face = "bold", color = "#112B51")
      ) +
      labs(y = "Count")+
      ylim(0,100) +
      facet_wrap( ~ genotype_other, ncol = 3)
    
    
    ggplotly(highlight_figure, tooltip = "text", height = 700)
    
    
    
  })
  
  
  
  output$gii4 <- renderPlotly({
    
    #Plot GII.4 variants by highlighting in a figure
    #filter out all genotypes except GII.4
    Genotype_4 <- rval_filters() %>%
      filter(Ctype4 == "GII.4") 
    
    #create a summary table of GII.4 data- includes all GII.4 variants and Ptypes
    Genotype_4_summary <- Genotype_4 %>%
      group_by(Year, Month_Year_floor, dual_type) %>%
      summarise(count = n())
    
    #would like to also graph the total values for GII.4 Sydney strains in the highlighted figure
    #for that, we will use the dataframe: Highlight_genotype_summary from the previous graph
    #recreated here 
    Highlight_genotype_summary <- rval_filters() %>%
      group_by(Year, Month_Year_floor, genotype_other) %>%
      summarise(count = n())
    
    # reorder the genotypes for the figure
    genotype_other_order <- c("GII.4 Sydney", "GII.3", "GII.2",  "GII.6", "GII other", "GI or GIX")
    Highlight_genotype_summary$genotype_other <-  factor(Highlight_genotype_summary$genotype_other, levels= genotype_other_order)
    ####
    
    # the graph
    highlight_figure_GII.4 <- ggplot(data = Highlight_genotype_summary %>% filter(genotype_other == "GII.4 Sydney")) +
      geom_line(aes(x=as.Date(Month_Year_floor), y= count, group = genotype_other, color = genotype_other,
                    text = paste('</br>', "All ", genotype_other, '</br>', Month_Year_floor ,'</br> sequences: ', count)),
                size=1.2) +
      #scale_x_date(date_breaks = "6 months", minor_breaks = "6 months", date_labels = "%b%y") +
      theme_minimal() +
      scale_x_date(breaks = seq(as.Date("2016-01-01"), as.Date("2022-01-01"), by="6 months"), date_labels = "%b\n%Y")
    theme(
      legend.position = "right",
      legend.text = element_text(family = "Avenir Next LT", size = 10, color = "#112B51"),
      panel.grid = element_blank(),
      axis.line = element_line(),
      axis.title.y = element_text(family = "Avenir Next LT", size = 14, color= "#3297C8", face = "bold"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(family = "Arial Nova", size = 10, color = "#112B51"),
      axis.text.y = element_text(family = "Arial Nova", size = 10, color = "#112B51")
    )
    
    # GII.4 Sydney highlight GII.4 P types
    highlight_figure_GII.4_Ptypes <- highlight_figure_GII.4 +
      suppressWarnings(geom_line(data = Genotype_4_summary, aes(x=as.Date(Month_Year_floor), y= count, group= dual_type, color = dual_type, text = paste('</br>',dual_type, '</br>', Month_Year_floor ,'</br> sequences: ', count)),
                                 size=0.7)) +
      scale_color_manual(values=c("GII.4 Sydney" ="#4C7A99","GII.4 Sydney[P16]" = "#63AFD0", "GII.4 Sydney[P31]" = "#E78AC3", "GII.4 Sydney[P4]"= "#FF6C00","GII.4 untypable[P4]" = "#7CCC5F", "GII.4 untypable[P31]" = "#6358A6", "GII.4 Hong Kong[P31]" = "#EBBF2C")) +
      #scale_x_date(date_breaks = "1 year", minor_breaks = "6 months", date_labels = "%Y") +
      theme(panel.grid = element_blank(),
            axis.ticks.x= element_line(),
            legend.title = element_blank()) +
      ylim(0,100) +
      labs(y = "Count", x= "Date")
    
    ggplotly(highlight_figure_GII.4_Ptypes, tooltip = "text", height = 700)
    
  })
  
  output$dualTypes <- renderPlot({
    
    sum_all_data_by_dualtype <- rval_filters() %>%
      dplyr::select(dual_type) %>%
      group_by(dual_type)%>%
      summarise(total= n())
    
    
    max_size <- max(sum_all_data_by_dualtype$total)
    min_size <- min(sum_all_data_by_dualtype$total)
    
    top5 <- c("GII.4 Sydney[P16]","GII.4 Sydney[P31]", "GII.3[P12]", "GII.2[P16]", "GII.6[P7]")
    other <- sum_all_data_by_dualtype %>% filter(dual_type %notin% c(top5)) %>% .$dual_type
    
    colors <- c(not_rare = "#4C7A99", rare_strains = "#E78AC3")
    
    
    ######
    
    plot_genotype_all <- ggplot(sum_all_data_by_dualtype, aes(x= total, y = reorder(dual_type, total), fill = dual_type))+
      geom_bar(stat= "identity") +
      geom_label(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (max_size*0.03), color= "white",label.padding = unit(0.55, "lines"), label.size = NA)+
      scale_fill_manual(values = c("GII.4 Sydney[P16]" = "#4C7A99", "GII.4 Sydney[P31]"= "#4C7A99", 
                                   "GII.3[P12]"= "#4C7A99", "GII.2[P16]" = "#4C7A99", "GII.6[P7]" ="#4C7A99", 
                                   "GII.3[P30]"= "#E78AC3", "GII.20[P20]"= "#E78AC3", "GII.13[PNA8]"= "#E78AC3",
                                   "GII.3[PNA3]"= "#E78AC3", "GII.3[P25]" = "#E78AC3",
                                   "GII.21[P16]" = "#E78AC3", "GII.5[P40]" = "#E78AC3", 
                                   "GII.27[PNA9]" = "#E78AC3", "GI.7[PNA2]"= "#E78AC3",
                                   "GII.22[PNA5]" = "#E78AC3", "GII.17[P12]" = "#E78AC3")) +
      #scale_fill_manual(values = colors)  +
      theme_minimal()+
      theme(
        axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family = "Sabon Next LT", face = "bold", size = 12, color = "#112B51"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Sabon Next LT", size = 12, color = "#112B51", face = "bold")
      ) +
      scale_x_continuous(expand= c(0,0),limits = c(0, max_size + max_size*0.06)) +
      labs(x = "Count")  # + facet_grid(dual_type ~., scales= "free")
    
    
    plot_genotype_all
    
  },
  height = height_var
  )
  
  
  
  
  output$regional <- renderPlot({
    
    sum_all_data_by_dualtype <- rval_filters() %>%
      dplyr::select(dual_type, Region_GPDS) %>%
      group_by(dual_type, Region_GPDS)%>%
      summarise(total= n())
    
    
    max_size <- max(sum_all_data_by_dualtype$total)
    min_size <- min(sum_all_data_by_dualtype$total)
    
    top5 <- c("GII.4 Sydney[P16]","GII.4 Sydney[P31]", "GII.3[P12]", "GII.2[P16]", "GII.6[P7]")
    other <- sum_all_data_by_dualtype %>% filter(dual_type %notin% c(top5)) %>% .$dual_type
    
    sum_all_data_by_dualtype$Region_GPDS <- factor(sum_all_data_by_dualtype$Region_GPDS, levels = c("East Asia", "Southeast Asia",
                                                                                                    "South Asia","Central Asia",
                                                                                                    "Oceania and West Pacific", "Europe",
                                                                                                    "South and Central America","North America", "Africa"))
    
    plot_genotype_all <- ggplot(sum_all_data_by_dualtype, aes(x= total, y = reorder(dual_type, total), fill = dual_type))+
      geom_bar(stat= "identity") +
      #geom_label(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (max_size*0.03), color= "white",label.padding = unit(0.45, "lines"), label.size = NA)+
      #geom_text(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (max_size*0.03), color= "darkslategrey")+
      geom_text(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (1 + max_size*0.03), color= "darkslategrey", size=4)+
      scale_fill_manual(values = c("GII.4 Sydney[P16]" = "#4C7A99", "GII.4 Sydney[P31]"= "#4C7A99", 
                                   "GII.3[P12]"= "#4C7A99", "GII.2[P16]" = "#4C7A99", "GII.6[P7]" ="#4C7A99", 
                                   "GII.3[P30]"= "#E78AC3", "GII.20[P20]"= "#E78AC3", "GII.13[PNA8]"= "#E78AC3",
                                   "GII.3[PNA3]"= "#E78AC3", "GII.3[P25]" = "#E78AC3",
                                   "GII.21[P16]" = "#E78AC3", "GII.5[P40]" = "#E78AC3", 
                                   "GII.27[PNA9]" = "#E78AC3", "GI.7[PNA2]"= "#E78AC3",
                                   "GII.22[PNA5]" = "#E78AC3", "GII.17[P12]" = "#E78AC3")) +
      #scale_fill_manual(values = c("GII.4 Sydney[P16]" = "#4C7A99", "GII.4 Sydney[P31]"= "#4C7A99", "GII.3[P12]"= "#4C7A99", "GII.2[P16]" = "#4C7A99", "GII.6[P7]" ="#4C7A99", "GIX.1[P15]" = "#E78AC3", "GII.3[P30]"= "#E78AC3", "GII.20[P7]"= "#E78AC3", "GII.20[P20]"= "#E78AC3","GII.3[PNA3]"= "#E78AC3"))+
      theme_minimal()+
      theme(
        axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family = "Sabon Next LT", face = "bold", size = 10, color = "#112B51"),
        strip.text = element_text(family = "Sabon Next LT", size = 12, color = "#112B51", face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Sabon Next LT", size = 10, color = "#112B51", face = "bold")
      ) +
      scale_x_continuous(expand= c(0,0),limits = c(0, max_size + max_size*0.06)) +
      labs(x = "Count")  +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
      facet_wrap(~Region_GPDS, ncol=2, scales = "free")
    
    plot_genotype_all
    
    
    
    
  },
  height = height_var2
  )
  
  
  output$country_select <- renderPlot({
    
    sum_all_data_by_dualtype <- rval_filters() %>%
      dplyr::select(dual_type, Country) %>%
      group_by(dual_type, Country)%>%
      summarise(total= n())
    
    
    max_size <- max(sum_all_data_by_dualtype$total)
    min_size <- min(sum_all_data_by_dualtype$total)
    
    top5 <- c("GII.4 Sydney[P16]","GII.4 Sydney[P31]", "GII.3[P12]", "GII.2[P16]", "GII.6[P7]")
    other <- sum_all_data_by_dualtype %>% filter(dual_type %notin% c(top5)) %>% .$dual_type
    
    sum_all_data_by_dualtype$Country <- factor(sum_all_data_by_dualtype$Country, levels = Country_order)
    
    plot_genotype_all <- ggplot(sum_all_data_by_dualtype, aes(x= total, y = reorder(dual_type, total), fill = dual_type))+
      geom_bar(stat= "identity") +
      #geom_label(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (max_size*0.03), color= "white",label.padding = unit(0.45, "lines"), label.size = NA)+
      #geom_text(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (max_size*0.03), color= "darkslategrey")+
      geom_text(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (1 + max_size*0.03), color= "darkslategrey", size=4)+
      scale_fill_manual(values = c("GII.4 Sydney[P16]" = "#4C7A99", "GII.4 Sydney[P31]"= "#4C7A99", 
                                   "GII.3[P12]"= "#4C7A99", "GII.2[P16]" = "#4C7A99", "GII.6[P7]" ="#4C7A99", 
                                   "GII.3[P30]"= "#E78AC3", "GII.20[P20]"= "#E78AC3", "GII.13[PNA8]"= "#E78AC3",
                                   "GII.3[PNA3]"= "#E78AC3", "GII.3[P25]" = "#E78AC3",
                                   "GII.21[P16]" = "#E78AC3", "GII.5[P40]" = "#E78AC3", 
                                   "GII.27[PNA9]" = "#E78AC3", "GI.7[PNA2]"= "#E78AC3",
                                   "GII.22[PNA5]" = "#E78AC3", "GII.17[P12]" = "#E78AC3")) +
      #scale_fill_manual(values = c("GII.4 Sydney[P16]" = "#4C7A99", "GII.4 Sydney[P31]"= "#4C7A99", "GII.3[P12]"= "#4C7A99", "GII.2[P16]" = "#4C7A99", "GII.6[P7]" ="#4C7A99", "GIX.1[P15]" = "#E78AC3", "GII.3[P30]"= "#E78AC3", "GII.20[P7]"= "#E78AC3", "GII.20[P20]"= "#E78AC3","GII.3[PNA3]"= "#E78AC3"))+
      theme_minimal()+
      theme(
        axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family = "Sabon Next LT", face = "bold", size = 10, color = "#112B51"),
        strip.text = element_text(family = "Sabon Next LT", size = 12, color = "#112B51", face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Sabon Next LT", size = 10, color = "#112B51", face = "bold")
      ) +
      scale_x_continuous(expand= c(0,0),limits = c(0, max_size + max_size*0.06)) +
      labs(x = "Count")  +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
      facet_wrap(~Country, ncol=2, scales = "free")
    
    plot_genotype_all
    
    
    
    
  },
  height = height_var2)
  
  
  
  
  output$age_cat <- renderPlot({
    
    sum_all_data_by_age_cat <- rval_filters() %>%
      dplyr::select(dual_type, Age_cat) %>%
      group_by(dual_type, Age_cat)%>%
      summarise(total= n())
    
    
    max_size <- max(sum_all_data_by_age_cat$total)
    min_size <- min(sum_all_data_by_age_cat$total)
    
    top5 <- c("GII.4 Sydney[P16]","GII.4 Sydney[P31]", "GII.3[P12]", "GII.2[P16]", "GII.6[P7]")
    other <- sum_all_data_by_age_cat %>% filter(Age_cat %notin% c(top5)) %>% .$Age_cat
    
    
    sum_all_data_by_age_cat$Age_cat = factor(sum_all_data_by_age_cat$Age_cat, levels = c("0-11", "12-23", "24-59", ">=60")) 
    
    plot_age_cat <- ggplot(sum_all_data_by_age_cat, aes(x= total, y = reorder(dual_type, total), fill = dual_type))+
      geom_bar(stat= "identity") +
      #geom_label(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (max_size*0.03), color= "white",label.padding = unit(0.45, "lines"), label.size = NA)+
      #geom_text(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (max_size*0.03), color= "darkslategrey")+
      geom_text(aes(x=total, y=reorder(dual_type, total), label = total), nudge_x = (1 + max_size*0.06), color= "darkslategrey", size=4)+
      scale_fill_manual(values = c("GII.4 Sydney[P16]" = "#4C7A99", "GII.4 Sydney[P31]"= "#4C7A99", 
                                   "GII.3[P12]"= "#4C7A99", "GII.2[P16]" = "#4C7A99", "GII.6[P7]" ="#4C7A99", 
                                   "GII.3[P30]"= "#E78AC3", "GII.20[P20]"= "#E78AC3", "GII.13[PNA8]"= "#E78AC3",
                                   "GII.3[PNA3]"= "#E78AC3", "GII.3[P25]" = "#E78AC3",
                                   "GII.21[P16]" = "#E78AC3", "GII.5[P40]" = "#E78AC3", 
                                   "GII.27[PNA9]" = "#E78AC3", "GI.7[PNA2]"= "#E78AC3",
                                   "GII.22[PNA5]" = "#E78AC3", "GII.17[P12]" = "#E78AC3")) +
      #scale_fill_manual(values = c("GII.4 Sydney[P16]" = "#4C7A99", "GII.4 Sydney[P31]"= "#4C7A99", "GII.3[P12]"= "#4C7A99", "GII.2[P16]" = "#4C7A99", "GII.6[P7]" ="#4C7A99", "GIX.1[P15]" = "#E78AC3", "GII.3[P30]"= "#E78AC3", "GII.20[P7]"= "#E78AC3", "GII.20[P20]"= "#E78AC3","GII.3[PNA3]"= "#E78AC3"))+
      theme_minimal()+
      theme(
        axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family = "Sabon Next LT", face = "bold", size = 10, color = "#112B51"),
        strip.text = element_text(family = "Sabon Next LT", size = 12, color = "#112B51", face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Sabon Next LT", size = 10, color = "#112B51", face = "bold")
      ) +
      scale_x_continuous(expand= c(0,0),limits = c(0, max_size + max_size*0.06)) +
      labs(x = "Count")  +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
      facet_wrap(~ factor(Age_cat, levels = c("0-11", "12-23","24-59", ">=60")), ncol=2, scales = "free")
    
    plot_age_cat
    
  }, height = height_var2)
  
  
  # library(DT)
  # 
  # output$dtable <- DT::renderDT({
  #     rval_filters() %>%
  #         dplyr::select(Month_Year_floor, Country, dual_type, Age, Setting, Symptoms)
  # })
  
  
  output$seq_ct <- renderValueBox({
    count <- rval_filters()
    valueBox(
      length(count$SampleID), 
      "Sequences uploaded", 
      color = "light-blue"
    )
  })
  
  output$genotype_ct <- renderValueBox({
    count <- rval_filters() 
    valueBox(
      length(unique(count$Ctype4)), 
      "Genotypes", 
      color = "light-blue"
    )
  })
  
  output$strain_ct <- renderValueBox({
    count <- rval_filters() 
    valueBox(
      length(unique(count$dual_type)), 
      "Strains", 
      color = "light-blue"
    )
  })
  
  
  output$genogroup_donut <- renderPlot({
    genogroup <- rval_filters()
    # total number of sequences
    total <- length(genogroup$SampleID)
    #summarize % of each genotype
    genogroup_summary <- genogroup %>%
      group_by(Genogroup) %>%
      summarise(count = n()) %>%
      mutate(percent.seq = round(count/total * 100, digits = 0)) %>%
      filter(Genogroup %in% c("GI", "GII"))
    # Compute percentages
    genogroup_summary$fraction <- genogroup_summary$count / sum(genogroup_summary$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    genogroup_summary$ymax <- cumsum(genogroup_summary$fraction)
    
    # Compute the bottom of each rectangle
    genogroup_summary$ymin <- c(0, head(genogroup_summary$ymax, n=-1))
    
    # Compute label position
    genogroup_summary$labelPosition <- (genogroup_summary$ymax + genogroup_summary$ymin) / 2
    
    # Compute a good label
    genogroup_summary$label <- paste0(genogroup_summary$percent.seq, "%")
    
    
    # Make the plot
    plot_donut <-  ggplot(genogroup_summary, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Genogroup)) +
      geom_rect() +
      geom_text(x=3.5, aes(y=labelPosition, label=label), color = "black", size=5, fontface = "bold") + # x here controls label position (inner / outer)
      scale_fill_brewer(palette=3) +
      #scale_color_brewer(palette=3) +
      coord_polar(theta="y", clip = "off") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(
        text = element_text(family = "Sabon Next LT", face = "bold", size = 12, color = "#112B51"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(family = "Sabon Next LT", face = "bold", size = 12)) 
    # ggtitle("Genogroup distribution")
    
    plot_donut
    
    
  })
  
  output$genotype_bar <- renderPlot({
    genotype <- rval_filters()
    
    # cal total for percent calculation
    total <- length(genotype$SampleID)
    
    #summarize % of each genotype
    genotype_summary <- genotype %>%
      group_by(CtypeUser) %>%
      summarise(n = n()) %>%
      mutate(percent.seq = round(n/total * 100, digits = 0)) %>%
      mutate(rank = dense_rank(dplyr::desc(percent.seq)))
    
    top_5 <- genotype_summary %>%
      filter(rank <= 5) %>%
      .$CtypeUser
    
    genotype_summary <- genotype_summary %>%
      filter(CtypeUser %in% top_5)
    
    max_size <- max(genotype_summary$percent.seq)
    min_size <- min(genotype_summary$percent.seq)
    
    
    ######
    
    plot_genotype_all <- ggplot(genotype_summary, aes(x= percent.seq, y = reorder(CtypeUser, percent.seq), fill = CtypeUser))+
      geom_bar(stat= "identity",fill = "#4C7A99") +
      geom_text(aes(x=percent.seq, y=reorder(CtypeUser, percent.seq), label = paste0(percent.seq, "%")),
                nudge_x = (1 + max_size*0.03), color= "darkslategrey", size=4) +
      theme_minimal()+
      theme(
        axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family = "Sabon Next LT", face = "bold", size = 12, color = "#112B51"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Sabon Next LT", size = 12, color = "#112B51", face = "bold")
      ) +
      scale_x_continuous(expand= c(0,0),limits = c(0, max_size + max_size*0.1)) +
      labs(x = "Count")  
    # ggtitle("Top genotypes")
    
    plot_genotype_all
    
  })
  
  output$strain_bar <- renderPlot({
    
    sum_all_data_by_dualtype <- rval_filters()
    
    # cal total for percent calculation
    total <- length(sum_all_data_by_dualtype$SampleID)
    
    sum_all_data_by_dualtype <- sum_all_data_by_dualtype%>%
      dplyr::select(dual_type) %>%
      group_by(dual_type)%>%
      summarise(n= n()) %>%
      mutate(percent.seq = round(n/total * 100, digits = 0)) %>%
      mutate(rank = dense_rank(dplyr::desc(percent.seq)))
    
    
    max_size <- max(sum_all_data_by_dualtype$percent.seq)
    min_size <- min(sum_all_data_by_dualtype$percent.seq)
    
    top_5 <- sum_all_data_by_dualtype %>%
      filter(rank <= 5) %>%
      .$dual_type
    
    # filter for only those in the top 5
    sum_all_data_by_dualtype <- sum_all_data_by_dualtype %>%
      filter(dual_type %in% top_5)
    
    
    ######
    
    plot_genotype_all <- ggplot(sum_all_data_by_dualtype, aes(x= percent.seq, y = reorder(dual_type, percent.seq), fill = dual_type))+
      geom_bar(stat= "identity", fill = "#4C7A99") +
      geom_text(aes(x=percent.seq, y=reorder(dual_type, percent.seq), label = paste0(percent.seq, "%")),
                nudge_x = (1 + max_size*0.03), color= "darkslategrey", size=4) +
      theme_minimal()+
      theme(
        axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family = "Sabon Next LT", face = "bold", size = 12, color = "#112B51"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Sabon Next LT", size = 12, color = "#112B51", face = "bold")
      ) +
      scale_x_continuous(expand= c(0,0),limits = c(0, max_size + max_size*0.1)) +
      labs(x = "Count")  # + facet_grid(dual_type ~., scales= "free")
    
    
    plot_genotype_all
    
  })
  
  #     strain_bar <- reactive({
  #       
  #       sum_all_data_by_dualtype <- rval_filters() 
  #       
  #       # cal total for percent calculation
  #       total <- length(sum_all_data_by_dualtype$SampleID)
  #       
  #       sum_all_data_by_dualtype <- sum_all_data_by_dualtype%>%
  #         dplyr::select(dual_type) %>%
  #         group_by(dual_type)%>%
  #         summarise(n= n()) %>%
  #         mutate(percent.seq = round(n/total * 100, digits = 0)) %>%
  #         mutate(rank = dense_rank(dplyr::desc(percent.seq)))
  #       
  #       
  #       max_size <- max(sum_all_data_by_dualtype$percent.seq)
  #       min_size <- min(sum_all_data_by_dualtype$percent.seq)
  #       
  #       top_5 <- sum_all_data_by_dualtype %>%
  #         filter(rank <= 5) %>%
  #         .$dual_type
  #       
  #       # filter for only those in the top 5
  #       sum_all_data_by_dualtype <- sum_all_data_by_dualtype %>%
  #         filter(dual_type %in% top_5)
  #       
  #       
  #       ######
  #       
  #       plot_genotype_all <- ggplot(sum_all_data_by_dualtype, aes(x= percent.seq, y = reorder(dual_type, percent.seq), fill = dual_type))+
  #         geom_bar(stat= "identity", fill = "#4C7A99") +
  #         geom_text(aes(x=percent.seq, y=reorder(dual_type, percent.seq), label = paste0(percent.seq, "%")),
  #                   nudge_x = (1 + max_size*0.03), color= "darkslategrey", size=4) +
  #         theme_minimal()+
  #         theme(
  #           axis.title.y=element_blank(),
  #           axis.title.x = element_blank(),
  #           legend.position = "none",
  #           panel.grid = element_blank(),
  #           text = element_text(family = "Sabon Next LT", face = "bold", size = 12, color = "#112B51"),
  #           axis.text.x = element_blank(),
  #           axis.text.y = element_text(family = "Sabon Next LT", size = 12, color = "#112B51", face = "bold")
  #         ) +
  #         scale_x_continuous(expand= c(0,0),limits = c(0, max_size + max_size*0.1)) +
  #         labs(x = "Count")  # + facet_grid(dual_type ~., scales= "free")
  #       
  #       
  #       plot_genotype_all
  #       
  #     })
  # 
  # renderPlot({
  #   strain_bar()
  # })
  #     
  #     
  
  
  # output$country_ct <- renderValueBox({
  #   count <- rval_filters()
  #   valueBox(
  #     length(unique(count$Country)),
  #     "Countries",
  #     icon = icon("list"),
  #     color = "light-blue"
  #   )
  # })
  
  # 
  #     output$data <- downloadHandler(
  #       filename = function(){
  #         paste0("NoroSurv_report", ".csv")
  #       },
  #       content = function(file){
  #         write.csv(rval_filters(), file)
  #       }
  #       )
  
  # output$report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "report.html",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "NoroSurv_report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  # 
  #     # Set up parameters to pass to Rmd document
  #     params <- list(n = input$data_set)
  # 
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  
  # output$report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "report.html",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "NoroSurv_report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(
  #       n = input$data_set,
  #       plot = strain_bar()
  #       )
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  
  # output$report <- downloadHandler(
  #   filename = "report.html",
  #   content = function(file) {
  #     # id <- showNotification(
  #     #   "Rendering report...", 
  #     #   duration = NULL, 
  #     #   closeButton = FALSE
  #     # )
  #     # on.exit(removeNotification(id), add = TRUE)
  #     
  #     rmarkdown::render("NoroSurv_report.Rmd", 
  #                       output_file = file,
  #                       params <- list(
  #                         n= input$age_range
  #                       ),
  #                       envir = new.env(),
  #                       intermediates_dir = tempdir()
  #                       )
  #     }
  #   )
  
  # output$report <- downloadHandler(
  #   filename = "report.html",
  #   content = function(file) {
  #     params <- list(
  #       n = input$data_set)
  #     src <- normalizePath("NoroSurv_report.Rmd")
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, "NoroSurv_report.Rmd", overwrite = TRUE)
  #     out <- render("NoroSurv_report.Rmd", html_document())
  #     file.rename(out, file)
  #   }
  # )
  
  
}

# Run app ----
shinyApp(ui, server)