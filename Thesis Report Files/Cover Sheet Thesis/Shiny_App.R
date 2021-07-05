library(shiny)
library(tidyverse)
load("dataset.rdata")

ena_z<-ena%>%select(ID,z_cat_WHO)%>%distinct(ID,z_cat_WHO)%>%group_by(ID)%>%summarise(Count=n())
ena_z$Count<-as.factor(ena_z$Count)

# Left Join
ena_all<-left_join(ena, ena_z, by = c("ID"="ID"))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    selectInput('gender','Select the Gender:',choices = c('Not Known'='not','Boy'='boy','Girl'='girl')),
    selectInput('category','Select the number of BMI category changes from intial stage:',choices = c('Not Known'='not', 'No Category Change'='1', 
                                                                                                         'One Category Change'='2', 
                                                                                                         'Two Category Change'='3',
                                                                                                         'Three Category Change'='4')),
    uiOutput('hidden'),
    plotOutput('graph')
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    output$hidden<-renderUI({
        selectInput('ids','Select the ID',choices = unique(data()$ID),multiple = T)
    })
    
    data<-reactive({
        #ena%>%filter(gender==input$gender)
        if(input$gender=='not'){
            if(input$category=='not')
                ena_all
            else
                ena_all %>% filter((Count) %in% input$category)
        }
        else{
            if(input$category=='not')
                ena_all %>% filter((gender) %in% input$gender)
            else{            
                ena_all %>% filter((gender) %in% input$gender) %>% 
                    filter((Count) %in% input$category)}
        }
    })
    
    output$graph<-renderPlot({
        unique_z_07<-ena %>% filter((ID)%in%input$ids)
        
        ggplot(unique_z_07) +
            geom_line(aes(x=age_years, y= z_score_WHO, color = ID))+
            geom_hline(yintercept=0.99999, linetype="dashed", color = "green")+
            geom_hline(yintercept=-2, linetype="dashed", color = "green")+
            annotate("rect", xmin = -Inf, xmax = Inf, ymin = -2, ymax = 0.99999, fill="green", alpha=0.15, color = NA) + annotate(geom = "text", x=18, y=-0.5, label="Normal", color = " Black") +
            
            geom_hline(yintercept=-2.000001, linetype="dashed", color = "green")+
            geom_hline(yintercept=-3, linetype="dashed", color = "yellow")+
            annotate("rect", xmin = -Inf, xmax = Inf, ymin = -3, ymax = -1.99999, fill="yellow", alpha=0.3, color = NA) + annotate(geom = "text", x=18, y=-2.5, label="Thin", color = " Black") +
            
            geom_hline(yintercept=-3.000001, linetype="dashed", color = "yellow")+
            geom_hline(yintercept=-Inf, linetype="dashed", color = "red")+
            annotate("rect", xmin = -Inf, xmax = Inf, ymin=-3.000001, ymax=-Inf, fill="red", alpha=0.15, color = NA) + annotate(geom = "text", x=18, y=-3.3, label="Severely Thin", color = " Black") +
            
            geom_hline(yintercept=1, linetype="dashed", color = "green")+
            geom_hline(yintercept=1.99999, linetype="dashed", color = "yellow")+
            annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = 1.99999, fill="yellow", alpha=0.3, color = NA) + annotate(geom = "text", x=18, y=1.5, label="Overweight", color = " Black") +
            
            geom_hline(yintercept=2, linetype="dashed", color = "yellow")+
            geom_hline(yintercept=Inf, linetype="dashed", color = "red")+
            annotate("rect", xmin = -Inf, xmax = Inf, ymin=2, ymax=Inf, fill="red", alpha=0.15, color = NA) +
            annotate(geom = "text", x=18, y=2.5, label="Obese", color = " Black") +
            
            xlab("Age (Years)") +
            ylab("Z Score (WHO)") +
            ggtitle("Changes in BMI Categories with Age")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
