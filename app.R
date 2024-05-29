#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(shinysurveys)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(lubridate)
#library(ggplot2)
library(plotly)

gs4_auth(path = "scientificrealismsurvey-2bcd35ce57a1.json")

df <- data.frame(
  question = c("1. Our most successful physics shows us what the world is really like.",
               "2. Physics uncovers what the universe is made of and how it works.",
               "3. Our most successful physics is useful in many ways, but physics does not reveal the true nature of the world.",
               "4. The imperceptible objects that are part of our most successful physics probably exist. (with 'imperceptible' we mean objects that cannot be perceived with our unaided senses, e.g. electrons, black holes, ...)",
               "5. The imperceptible objects postulated by physics are only useful fictions.",
               "6. Physicists discover imperceptible objects.",
               "7. Communities of physicists construct imperceptible objects.",
               "8. Our best physical theories are true or approximately true.",
               "9. Physical theories do not reveal hidden aspects of nature. Instead, they are instruments for the classification, manipulation and prediction of phenomena.",
               "10. The most important goal of physics is giving us true theories.",
               "11. If there was a highly advanced civilization in another galaxy, their scientists would discover the existence and properties of many of the imperceptible objects of our current physics.",
               "12. I expect the best current theories in physics to be largely refuted in the next centuries – in the same way that successful theories were largely refuted in the past.",
               "13. Electrons exist.",
               "14. Electrons, with all their properties, exist 'out there,' independently from our theories.",
               "15. Our theories are getting closer to the real nature of the electron.",
               "16. Electrons are postulated as real within our models; it does not make sense to ask whether they exist 'outside' or independently of the theory/model.",
               "17. There is something in the world that behaves like (what we would define as) an electron.",
               "18. Electrons are (at least) as real as toe-nails and volcanoes.",
               "19. Phonons exist.",
               "20. There really was a Big Bang.",
               "21. General relativity is a true theory",
               "22. General relativity teaches us about the nature of spacetime",
               "23. General relativity is not the revelation of an underlying order of nature. It is a tool that helps us make predictions and construct GPS, for example.",
               "24. Newtonian mechanics is a true theory",
               "25. If a phenomenon can be explained both by a classical model and by a quantum model, neither of the models is closer to the truth than the other.",
               "26. We should build a particle collider that is bigger than the LHC.",
               "27. A physical theory cannot tell us what the universe is really made of, but the mathematical structure of our best theories represents the structure of the world.",
               "28. Having mutually conflicting theories about the same phenomena is valuable for physics.",
               "29. Our scientific knowledge is the product of the prevailing cultural traditions and historical periods in which they were formulated",
               "30. Scientific theories and models are idealized structures that represent the world from particular and limited points of view."),
  option = NA,
  input_type = "slider",
  input_id =  paste0("S",seq(1,30)),
  dependence = NA,
  dependence_value = NA,
  required = TRUE
)  %>% union(data.frame(
  question = c("What is your age? (optional)"),
  option = c('0 - 10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '80+'),
  input_type = "select",             
  input_id='age',
  dependence = NA,
  dependence_value = NA,
  required=FALSE
))

#  "What is your age? (optional)", "If you have a university degree is it in the arts, science or mixed? (optional)", "Any comments (optional"

extendInputType(input_type = "slider", {
  shiny::sliderInput(
    inputId = surveyID(),
    label = surveyLabel(),
    min = 0,
    max = 100,
    value = 50
  ) 
})


ui <- fluidPage(
  
  surveyOutput(df = df,
               survey_title = "Scientific Realism Questionnaire",
               survey_description = tagList(
                                     tags$b("To what extent do you agree with each of these statements?"), "Choose between 0 (completely disagree) and 100 (completely agree). We'll use these to calculate your philsophical positiona dn show you the results.",
                                            tags$br(),tags$br(), "Questions from", tags$a(href="https://philsci-archive.pitt.edu/22931/", "“Physicists’ Views on Scientific Realism”"), "Céline Henne, Hannah Tomczyk and Christopher Sperber.", 
                                            ), theme='#19837E'))



calculateRealismScore  <- function(df) {
  realismStatements <- c('S1', 'S2', 'S4', 'S6', 'S8', 'S10', 'S11', 'S13', 'S14', 'S15', 'S17', 'S18', 'S21', 'S22')
  mean(as.integer(df[1, colnames(df) %in% realismStatements]), na.rm = TRUE)
}

calculateInstumentalismScore  <- function(df) {
  insturmentalismStatements <- c('S3', 'S5', 'S7', 'S9', 'S12', 'S16', 'S23', 'S25')
  mean(as.integer(df[1, colnames(df) %in% insturmentalismStatements]), na.rm = TRUE)
}
  

assignCluster <- function(dfRow) {
  
# Define the centroids data frame
centroids <- data.frame(
  Cluster = c(1, 2, 3, 4, 5),
  S1  = c(30.3, 84.6, 82.3, 58.6, 69.7),
  S2  = c(54.5, 92.8, 89.6, 79, 79.3),
  S3  = c(67.5, 18.9, 29.6, 32.8, 66.1),
  S4  = c(54.6, 92.6, 89.5, 77, 39.6),
  S5  = c(66.1, 9.2, 24.1, 26.8, 78),
  S6  = c(43.2, 86, 85.5, 66.1, 80.3),
  S7  = c(64.5, 24, 55.2, 40.7, 37.9),
  S8  = c(46.8, 88.7, 88.2, 63.6, 29.7),
  S9  = c(71.2, 18, 46.7, 35.9, 65.5),
  S10 = c(25.2, 65.9, 61.4, 36.9, 71.6),
  S11 = c(57.3, 92.8, 86.6, 68.1, 51.4),
  S12 = c(66.9, 38.4, 52.2, 49.8, 58),
  S13 = c(55.7, 96.4, 95, 86, 83.2),
  S14 = c(43.3, 95.3, 89.4, 76, 65.5),
  S15 = c(43.8, 87, 89.5, 72, 74),
  S16 = c(61, 22.2, 43.8, 45.6, 61.6),
  S17 = c(76.7, 92, 92.4, 81.9, 88.9),
  S18 = c(48.9, 94.4, 93.2, 76.5, 77.5),
  S19 = c(42.7, 82.8, 84.8, 73, 73.1),
  S20 = c(48.3, 81, 76.2, 64.4, 68),
  S21 = c(37.6, 83.6, 87.2, 59.2, 74.1),
  S22 = c(61, 90.5, 93.4, 82.4, 74.7),
  S23 = c(62.7, 17.8, 25.2, 32.7, 64.8),
  S24 = c(34.2, 71.3, 79.3, 47.3, 73.1),
  S25 = c(54.2, 22.2, 60.6, 34.7, 57.2),
  #S26 = c(45.5, 64.5, 53.6, 53.9, 52.8), ignore LHC question for assigning clusters
  S27 = c(58.8, 39.4, 61.9, 58.8, 55.3),
  S28 = c(83.1, 74.8, 79.7, 79.7, 81.6),
  S29 = c(66.5, 41.5, 56.4, 50.4, 73.6),
  S30 = c(81.6, 46.8, 77.5, 56, 81.4)
)

colnames(centroids) <- c("Cluster", paste0("S", c(1:25, 27:30)))
  
 determine_cluster <- function(df, centroids) {
   # Initialize a vector to store the cluster assignments
   
   # Calculate the Euclidean distance between the row and each cluster centroid
   distances <- apply(centroids[ , -1], 1, function(centroid) {
     sqrt(sum((as.integer(df[1, colnames(df) %in% colnames(centroids)]) - centroid)^2, na.rm = TRUE))
   })
   # Assign the cluster with the minimum distance
   print(distances)
   return(centroids$Cluster[which.min(distances)])
   
 }
 return(determine_cluster(dfRow, centroids))
}

getClusterName <- function(clusterNumber) {
  print(paste("cluster number",clusterNumber))
  clusterNames <-c("an intrumentalist", "a classic realist", "a perspectival/pluarlist realist", "a moderate realist", "hard to categorize")
  clusterNames[clusterNumber]
}


create_gauge_plots <- function(realism_score, instrumentalism_score) {
  
  df <- data.frame(matrix(nrow=2, ncol = 2))
  
  names(df) <- c("variable", "percentage")
  df$variable <- c("Realism Score","Instrumentalism Score")
  df$val <- c(realism_score, instrumentalism_score)
  
  
  ggplot(df, aes(ymax = val/100, ymin = 0, xmax = 2, xmin = 1)) +
    geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#07091C") +
    geom_rect(fill = "#19837E") + 
    geom_text(aes(x = 0, y = 0, label = paste0(round(val,0),'%')), colour='#19837E', size=6.5) +
    coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
    geom_text(aes(x=0.4, y=1.5, label=variable), size=6.2) + 
    theme_void() +
    facet_wrap(~variable) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank()) +
    guides(fill=FALSE) +
    guides(colour=FALSE)
}





server <- function(input, output, session) {
  renderSurvey()

  observeEvent(input$submit, {
  
    
    dfResults <- getSurveyData() %>% select(-question_type) %>% pivot_wider('subject_id', names_from='question_id', values_from='response')
    cluster <- assignCluster(dfResults %>% head(1))
    realismScore <- calculateRealismScore(dfResults)
    instrumentalismScore <- calculateInstumentalismScore(dfResults)

    output$gaugePlt <- renderPlot({create_gauge_plots(realismScore, instrumentalismScore)})

    showModal(modalDialog(
      title = paste("Your results") ,
      plotOutput("gaugePlt")
    ))

   

    
    #print(getSurveyData())
    print("\n")
    #print(dfResults)
    dfResults$time = now()
    dfResults$cluster_id = cluster
    dfResults$cluster_id = getClusterName(cluster)


    sheet_append(
      ss = "1nEdLbk-Po2d5yBso9tuxflmM3O4yo3D_UYWxuUQ3wII",
      data = dfResults,
      sheet = "Results from Shiny App"
    )
  })
}


# Run the application 
shinyApp(ui = ui, server = server, options = list(host = '0.0.0.0', port = 7905))

#runApp('0.0.0.0', 4306)

# touch app.log
# Rscript --verbose app.R >> app.log 2>&1 &
