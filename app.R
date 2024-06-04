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
library(fmsb)

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
)) %>% union(data.frame(
  question = c("In your highest level of study, which subjects did you specialize in? (optional)"),
  option = c('Arts', 'Sciences', 'Even mix'),
  input_type = "select",             
  input_id='subjects',
  dependence = NA,
  dependence_value = NA,
  required=FALSE
))  %>% union(data.frame(
  question = c("Are you a professional academic? (optional)"),
  option = c('Yes', 'No'),
  input_type = "select",             
  input_id='academic',
  dependence = NA,
  dependence_value = NA,
  required=FALSE
)) %>% union(data.frame(
  question = c("What is your field of research? (optional)"),
  option = c("Natural Sciences", "Life Sciences", "Physical Sciences", "Mathematics and Statistics", "Engineering and Technology", "Computer Science and Information Technology", "Health and Medicine", "Social Sciences", "Humanities", "Arts", "Education", "Business and Management", "Law and Legal Studies", "Agricultural and Environmental Sciences", "Interdisciplinary Studies", "Library and Information Science", "Media and Communication", "Other"),
  input_type = "select",             
  input_id='field',
  dependence = 'academic',
  dependence_value = 'Yes',
  required=FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Natural Sciences: (optional)"),
  option = c("Biology", "Chemistry", "Physics (experimental)","Physics (theoretical)", "Physics (generalist)", "Earth Sciences (Geology, Meteorology, Oceanography)", "Environmental Science", "Astronomy", "Other"),
  input_type = "select",
  input_id = 'subfield_natural_sciences',
  dependence = 'field',
  dependence_value = 'Natural Sciences',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Life Sciences: (optional)"),
  option = c("Biochemistry", "Biotechnology", "Genetics", "Neuroscience", "Microbiology", "Zoology", "Botany", "Ecology", "Other"),
  input_type = "select",
  input_id = 'subfield_life_sciences',
  dependence = 'field',
  dependence_value = 'Life Sciences',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Physical Sciences: (optional)"),
  option = c("Materials Science", "Nanotechnology", "Applied Physics", "Other"),
  input_type = "select",
  input_id = 'subfield_physical_sciences',
  dependence = 'field',
  dependence_value = 'Physical Sciences',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Mathematics and Statistics: (optional)"),
  option = c("Pure Mathematics", "Applied Mathematics", "Statistics", "Actuarial Science", "Other"),
  input_type = "select",
  input_id = 'subfield_math_stats',
  dependence = 'field',
  dependence_value = 'Mathematics and Statistics',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Engineering and Technology: (optional)"),
  option = c("Civil Engineering", "Mechanical Engineering", "Electrical Engineering", "Chemical Engineering", "Computer Engineering", "Software Engineering", "Biomedical Engineering", "Aerospace Engineering", "Industrial Engineering", "Other"),
  input_type = "select",
  input_id = 'subfield_engineering_tech',
  dependence = 'field',
  dependence_value = 'Engineering and Technology',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Computer Science and Information Technology: (optional)"),
  option = c("Computer Science", "Information Technology", "Data Science", "Cybersecurity", "Artificial Intelligence", "Robotics", "Other"),
  input_type = "select",
  input_id = 'subfield_cs_it',
  dependence = 'field',
  dependence_value = 'Computer Science and Information Technology',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Health and Medicine: (optional)"),
  option = c("Medicine", "Nursing", "Public Health", "Pharmacology", "Dentistry", "Veterinary Medicine", "Medical Research", "Other"),
  input_type = "select",
  input_id = 'subfield_health_medicine',
  dependence = 'field',
  dependence_value = 'Health and Medicine',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Social Sciences: (optional)"),
  option = c("Psychology", "Sociology", "Anthropology", "Political Science", "Economics", "Geography", "Social Work", "International Relations", "Other"),
  input_type = "select",
  input_id = 'subfield_social_sciences',
  dependence = 'field',
  dependence_value = 'Social Sciences',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Humanities: (optional)"),
  option = c("History", "Philosophy", "Literature", "Linguistics", "Religious Studies", "Art History", "Classics", "Cultural Studies", "Other"),
  input_type = "select",
  input_id = 'subfield_humanities',
  dependence = 'field',
  dependence_value = 'Humanities',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Arts: (optional)"),
  option = c("Visual Arts", "Performing Arts", "Music", "Film Studies", "Design", "Other"),
  input_type = "select",
  input_id = 'subfield_arts',
  dependence = 'field',
  dependence_value = 'Arts',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Business and Management: (optional)"),
  option = c("Business Administration", "Management", "Marketing", "Finance", "Accounting", "International Business", "Human Resource Management", "Entrepreneurship", "Other"),
  input_type = "select",
  input_id = 'subfield_business_management',
  dependence = 'field',
  dependence_value = 'Business and Management',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Law and Legal Studies: (optional)"),
  option = c("Law", "Criminology", "Legal Studies", "Other"),
  input_type = "select",
  input_id = 'subfield_law_legal',
  dependence = 'field',
  dependence_value = 'Law and Legal Studies',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Agricultural and Environmental Sciences: (optional)"),
  option = c("Agriculture", "Horticulture", "Forestry", "Environmental Management", "Agricultural Economics", "Other"),
  input_type = "select",
  input_id = 'subfield_agri_env',
  dependence = 'field',
  dependence_value = 'Agricultural and Environmental Sciences',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Interdisciplinary Studies: (optional)"),
  option = c("Cognitive Science", "Gender Studies", "Urban Studies", "Development Studies", "Sustainability Studies", "Other"),
  input_type = "select",
  input_id = 'subfield_interdisciplinary',
  dependence = 'field',
  dependence_value = 'Interdisciplinary Studies',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Library and Information Science: (optional)"),
  option = c("Library Science", "Information Science", "Other"),
  input_type = "select",
  input_id = 'subfield_library_info_science',
  dependence = 'field',
  dependence_value = 'Library and Information Science',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Media and Communication: (optional)"),
  option = c("Journalism", "Media Studies", "Communication", "Other"),
  input_type = "select",
  input_id = 'subfield_media_communication',
  dependence = 'field',
  dependence_value = 'Media and Communication',
  required = FALSE
))  %>% union(data.frame(
  question = c("What is your profession?"),
  option = c("Healthcare and Medical", "Education", "Information Technology", "Engineering and Architecture", "Business and Finance", "Legal", "Creative Arts and Media", "Skilled Trades", "Science and Research", "Public Service and Administration", "Hospitality and Tourism", "Retail and Customer Service", "Transportation and Logistics", "Agriculture and Environmental", "Other"),
  input_type = "select",             
  input_id = 'profession',
  dependence = 'academic',
  dependence_value = 'No',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Healthcare and Medical:"),
  option = c("Doctor", "Nurse", "Pharmacist", "Dentist", "Veterinarian", "Physical Therapist", "Medical Technician", "Healthcare Administrator", "Psychologist", "Paramedic", "Other"),
  input_type = "select",
  input_id = 'subfield_healthcare_medical',
  dependence = 'profession',
  dependence_value = 'Healthcare and Medical',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Education:"),
  option = c("Teacher", "Professor", "School Administrator", "Librarian", "Tutor", "Education Consultant", "School Counselor", "Special Education Teacher", "Researcher", "Other"),
  input_type = "select",
  input_id = 'subfield_education',
  dependence = 'profession',
  dependence_value = 'Education',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Information Technology:"),
  option = c("Software Developer", "IT Support Specialist", "Network Administrator", "Data Scientist", "Cybersecurity Analyst", "Systems Analyst", "Database Administrator", "Web Developer", "IT Project Manager", "Other"),
  input_type = "select",
  input_id = 'subfield_it',
  dependence = 'profession',
  dependence_value = 'Information Technology',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Engineering and Architecture:"),
  option = c("Civil Engineer", "Mechanical Engineer", "Electrical Engineer", "Chemical Engineer", "Environmental Engineer", "Architect", "Industrial Designer", "Aerospace Engineer", "Structural Engineer", "Other"),
  input_type = "select",
  input_id = 'subfield_engineering_architecture',
  dependence = 'profession',
  dependence_value = 'Engineering and Architecture',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Business and Finance:"),
  option = c("Accountant", "Financial Analyst", "Marketing Manager", "Human Resources Manager", "Sales Manager", "Business Consultant", "Entrepreneur", "Customer Service Representative", "Project Manager", "Other"),
  input_type = "select",
  input_id = 'subfield_business_finance',
  dependence = 'profession',
  dependence_value = 'Business and Finance',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Legal:"),
  option = c("Lawyer", "Paralegal", "Judge", "Legal Secretary", "Compliance Officer", "Court Reporter", "Legal Consultant", "Other"),
  input_type = "select",
  input_id = 'subfield_legal',
  dependence = 'profession',
  dependence_value = 'Legal',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Creative Arts and Media:"),
  option = c("Graphic Designer", "Writer/Author", "Journalist", "Photographer", "Musician", "Actor/Actress", "Film Director", "Video Editor", "Art Director", "Other"),
  input_type = "select",
  input_id = 'subfield_creative_arts_media',
  dependence = 'profession',
  dependence_value = 'Creative Arts and Media',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Skilled Trades:"),
  option = c("Carpenter", "Electrician", "Plumber", "Mechanic", "Welder", "HVAC Technician", "Construction Worker", "Machinist", "Other"),
  input_type = "select",
  input_id = 'subfield_skilled_trades',
  dependence = 'profession',
  dependence_value = 'Skilled Trades',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Science and Research:"),
  option = c("Biologist", "Chemist", "Physicist", "Environmental Scientist", "Geologist", "Research Scientist", "Laboratory Technician", "Other"),
  input_type = "select",
  input_id = 'subfield_science_research',
  dependence = 'profession',
  dependence_value = 'Science and Research',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Public Service and Administration:"),
  option = c("Police Officer", "Firefighter", "Social Worker", "Public Relations Specialist", "Urban Planner", "Politician", "Diplomat", "Military Personnel", "Other"),
  input_type = "select",
  input_id = 'subfield_public_service',
  dependence = 'profession',
  dependence_value = 'Public Service and Administration',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Hospitality and Tourism:"),
  option = c("Chef", "Hotel Manager", "Travel Agent", "Tour Guide", "Event Planner", "Restaurant Manager", "Other"),
  input_type = "select",
  input_id = 'subfield_hospitality_tourism',
  dependence = 'profession',
  dependence_value = 'Hospitality and Tourism',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Retail and Customer Service:"),
  option = c("Retail Manager", "Cashier", "Sales Associate", "Store Manager", "Customer Service Representative", "Other"),
  input_type = "select",
  input_id = 'subfield_retail_customer_service',
  dependence = 'profession',
  dependence_value = 'Retail and Customer Service',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Transportation and Logistics:"),
  option = c("Truck Driver", "Airline Pilot", "Logistics Manager", "Supply Chain Analyst", "Delivery Driver", "Warehouse Manager", "Other"),
  input_type = "select",
  input_id = 'subfield_transportation_logistics',
  dependence = 'profession',
  dependence_value = 'Transportation and Logistics',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Agriculture and Environmental:"),
  option = c("Farmer", "Agricultural Scientist", "Environmental Consultant", "Horticulturist", "Forester", "Other"),
  input_type = "select",
  input_id = 'subfield_agriculture_environmental',
  dependence = 'profession',
  dependence_value = 'Agriculture and Environmental',
  required = FALSE
)) %>% union(data.frame(
  question = c("Please specify your subfield in Other:"),
  option = c("Other"),
  input_type = "select",
  input_id = 'subfield_other',
  dependence = 'profession',
  dependence_value = 'Other',
  required = FALSE
)) %>% union(data.frame(
  question = c("How long have you worked in your field? (optional)"),
  option = c('less than 1 year', '1 - 2 years', '2 - 5 years', '5 - 10 years ', '10 - 20 years', 'over 20 years', 'N/a'),
  input_type = "select",             
  input_id='time_in_field',
  dependence = NA,
  dependence_value = NA,
  required=FALSE
))


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
                                     tags$b("To what extent do you agree with each of these statements?"),
                                     tags$br(),tags$br(),
                                       "Choose between 0 (completely disagree) and 100 (completely agree). We'll use these to calculate your philsophical position and show you the results.",
                                            tags$br(),tags$br(), "Questions from", tags$a(href="https://philsci-archive.pitt.edu/22931/", "“Physicists’ Views on Scientific Realism”"), "Céline Henne, Hannah Tomczyk and Christopher Sperber.", 
                                     tags$br(),tags$br(),
                                     "Listen to Céline discuss the study on the ", tags$a(href="https://www.multiverses.xyz/podcast/do-electrons-exist-celine-henne-physicists-views-on-scientific-realism-instrumentalism/", "Multiverses Podcast")
                                            ), theme='#19837E'),
  )



calculateRealismScore  <- function(df) {
  realismStatements <- c('S1', 'S2', 'S4', 'S6', 'S8', 'S10', 'S11', 'S13', 'S14', 'S15', 'S17', 'S18', 'S21', 'S22')
  mean(as.integer(df[1, colnames(df) %in% realismStatements]), na.rm = TRUE)
}

calculateInstumentalismScore  <- function(df) {
  insturmentalismStatements <- c('S3', 'S5', 'S7', 'S9', 'S12', 'S16', 'S23', 'S25')
  mean(as.integer(df[1, colnames(df) %in% insturmentalismStatements]), na.rm = TRUE)
}
  


getCentroidDistances <- function(df) {
  
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
  
  # Calculate the Euclidean distance between the row and each cluster centroid
  distances <- apply(centroids[ , -1], 1, function(centroid) {
    sqrt(sum((as.integer(df[1, colnames(df) %in% colnames(centroids)]) - centroid)^2, na.rm = TRUE))
  })
  return(distances)
}

assignCluster <- function(distances) {
   clusters <- c(1, 2, 3, 4, 5)
   return(clusters[which.min(distances)])
}

getClusterName <- function(clusterNumber) {
  print(paste("cluster number",clusterNumber))
  clusterNames <-c("intrumentalism", "classic realism", "a perspectival/pluarlist realism", "moderate realism", "with the hard to categorize group")
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
          strip.text.x = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = unit(c(0, 0, 0, 0), "cm") # Reduce whitespace around the plot
          ) +
    
    guides(fill=FALSE) +
    guides(colour=FALSE)
}


create_radar_chart <-function(distances) {
  data <- data.frame(
    `Instrumentalist` = c(340.9976, 0, 340.9976-distances[1]),
    `Classic\nRealist` = c(441.6561, 0,441.6561-distances[2]),
    `Realist And Pluralist` = c(422.0926, 0, 422.0926-distances[3]),
    `Moderate Realist` = c(365.1837, 0,  365.1837- distances[4]),
    `Tricky` = c(381.9979, 0, 381.9979- distances[5])
  )
  
  # Add row names
  rownames(data) <- c("Max", "Min", "Value")
  
  par(mar = c(9, 3, 3, 3))
  
  # Plot the radar chart
  radarchart(data, axistype = 1,
             # Custom polygon colors
             pcol = "#07091C",
             pfcol = rgb(0.09803922, 0.51372549, 0.49411765, 0.5),
             plwd = 2,
             # Custom grid colors
             cglcol = "grey",
             cglty = 1,
             axislabcol = "grey",
             caxislabels =  rep(" ",5) ,
             # Custom background and border
             vlcex = 0.8)
}

getCompatriots <- function(clusterNumber) {
  compatriots <- c("People who have held similiar positions include Ernst Mach, Henri Poincaré, Niels Bohr & Larry Laudan.", 
                   "People who have held similiar positions include Stathis Psillos, Alan Musgrave & Steven Weinberg",
                   "You might be interested in the work Michela Massimi who has pioneered the idea of Perspectival Realism",
                   "People who have held similiar positions include Ian Hacking, Anjan Chakravartty & Nancy Cartwright",
                   "")
  compatriots[clusterNumber]
}


server <- function(input, output, session) {
  renderSurvey()

  observeEvent(input$submit, {
  
    
    dfResults <- getSurveyData() %>% select(-question_type) %>% pivot_wider('subject_id', names_from='question_id', values_from='response')
    
    distances <- getCentroidDistances(dfResults)
    cluster <- assignCluster(distances)
    realismScore <- calculateRealismScore(dfResults)
    instrumentalismScore <- calculateInstumentalismScore(dfResults)

    output$gaugePlt <- renderPlot({create_gauge_plots(realismScore, instrumentalismScore)})
    output$radarPlt <- renderPlot({create_radar_chart(distances)})
    
    showModal(modalDialog(
      title = paste("Your results") ,
      tags$div(
        "Of the survey questions, 22 test inclination towards realism or instrumentalism, below are your average results for these two sets of questions.",
        tags$br(), tags$br(),
        "Note it is possible to have high score for both as they are tested by distinct subsets of questions.",
        style = "position: relative; z-index: 2; background-color: transparent;"
      ),
      tags$div(plotOutput("gaugePlt"), style ="margin: -70px 0;  background-color: transparent;" ),
      tags$div(
        paste0("The below chart shows how you compare to the 5 clusters identified among physicists. Your closest cluster is ",getClusterName(cluster),". ",getCompatriots(cluster)) ,
        tags$br(),tags$br(),
        "Listen to a discussion the study on the ", tags$a(href="https://www.multiverses.xyz/podcast/do-electrons-exist-celine-henne-physicists-views-on-scientific-realism-instrumentalism/", "Multiverses Podcast"),
        style = "position: relative; z-index: margin: -60px 0 10px 0;" # Adjust the margin here to reduce space
      ),
      plotOutput("radarPlt")

    ))

   

    
    #print(getSurveyData())
    print("\n")
    #print(dfResults)
    dfResults$time = now()
    dfResults$cluster_id = cluster
    dfResults$cluster_id = getClusterName(cluster)
    dfResults$realism_score = realismScore
    dfResults$instrumentalism_score = instrumentalismScore


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
