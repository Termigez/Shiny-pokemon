library(shiny)
library(tidyverse)

setwd("~/Rprace/pokemony/poki2")

dane <- read.csv("Pokemon.csv")

ui <- fluidPage(
  titlePanel("Pokemony wersja 2.0"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Wybierz wlasnego pokemona oraz generacje, z ktora
               go porownasz"),
      selectInput("Imie",
                  label = "Nazwa pokemona",
                  choices = as.character(dane$Name)),
      
      checkboxInput("legendy",
                  label = "Uwzglednij legendy",
                  value=FALSE),
      radioButtons("wybor", "Rodzaj wykresu:",
                   c("filtruj","wszystkie"))
    ),
    
    mainPanel(plotOutput("wykres1"),
              plotOutput("wykres2"))
  )
)

server <- function(input, output) {
  
  output$wykres1 <- renderPlot({
    dane %>% filter(Name==input$Imie) %>% summarise(HP1 = mean(HP),
                                                  Attack1 = mean(Attack),
                                                  Defense1 = mean(Defense),
                                                  SpecialA = mean(Sp..Atk),
                                                  SpecialD = mean(Sp..Def),
                                                  Speed1 = mean(Speed)) %>%
      rbind(c("HP","Attack","Defense","Sp.Att","Sp.Def","Speed")) %>%
      data.table::transpose() %>% mutate(V13 = as.numeric(V1), V14=as.factor(V2)) %>%
      select(-V1, -V2) %>% ggplot(aes(x=V14,y=V13)) + geom_col(aes(fill=V14),color="black") +
      theme_bw() + xlab("Statystyka") + ylab("Wartosc") + 
      ggtitle(paste(input$Imie)) + theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(label=round(V13,2)),vjust=2) +
      scale_y_continuous(breaks=seq(0,125,25),limits = c(0,125))
  })
  
  output$wykres2 <- renderPlot({
    if (input$wybor == "filtruj") {
      dane %>% filter(Type.1 == dane$Type.1[dane$Name==input$Imie]) %>% 
      {if (input$legendy) filter(., Legendary %in% c("False","True")) else filter(., Legendary=="False")} %>%
        summarise(HP1 = mean(HP),
                  Attack1 = mean(Attack),
                  Defense1 = mean(Defense),
                  SpecialA = mean(Sp..Atk),
                  SpecialD = mean(Sp..Def),
                  Speed1 = mean(Speed)) %>%
        rbind(c("HP","Attack","Defense","Sp.Att","Sp.Def","Speed")) %>%
        data.table::transpose() %>% mutate(V11 = as.numeric(V1), V12=as.factor(V2)) %>%
        select(-V1, -V2) %>% ggplot(aes(x=V12,y=V11)) + geom_col(aes(fill=V12),color="black") +
        theme_bw() + xlab("Statystyka") + ylab("Wartosc") +
        ggtitle(paste(dane$Type.1[dane$Name==input$Imie])) + theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(aes(label=round(V11,2)),vjust=2) +
        scale_y_continuous(breaks=seq(0,125,25),limits = c(0,125))
    } else if (input$wybor == "wszystkie"){
      dane %>%
      {if (input$legendy) filter(., Legendary %in% c("False","True")) else filter(., Legendary=="False")} %>%
        summarise(HP1 = mean(HP),
                  Attack1 = mean(Attack),
                  Defense1 = mean(Defense),
                  SpecialA = mean(Sp..Atk),
                  SpecialD = mean(Sp..Def),
                  Speed1 = mean(Speed)) %>%
        rbind(c("HP","Attack","Defense","Sp.Att","Sp.Def","Speed")) %>%
        data.table::transpose() %>% mutate(V11 = as.numeric(V1), V12=as.factor(V2)) %>%
        select(-V1, -V2) %>% ggplot(aes(x=V12,y=V11)) + geom_col(aes(fill=V12),color="black") +
        theme_bw() + xlab("Statystyka") + ylab("Wartosc") +
        ggtitle("Wszystkie") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(aes(label=round(V11,2)),vjust=2) +
        scale_y_continuous(breaks=seq(0,125,25),limits = c(0,125))
    }
      
      
    
  })
  
}

shinyApp(ui = ui, server = server)

