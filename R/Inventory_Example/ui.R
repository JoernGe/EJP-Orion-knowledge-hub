library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(readxl)
library(bib2df)



ui <- fluidPage(
#  tags$head(HTML("<title>Title</title>")),
  tags$html(lang="en"),
  titlePanel(
    fluidRow(
      column(8,align = "center", style = "margin-top: 5px;",
             HTML("<h1><b><font color=\"#13699f\">Title</b><font color=\"#000000\"></h1>")),
      column(2,align = "left",
             list(a(img(id = "ejp-logo",
                        src = "logos/onehealth-ejp.png",
                        alt = "EJP logo",
                        height = 100), 
                    href = "https://google.de", 
                    target = "_blank"))),
      column(2,
             align = "right",
             list(a(img(id = "fli-logo",
                          src = "logos/FLI-Logo_blau_2017.png",
                          height = 100, 
                          alt = "FLI logo"),
                      href = "https://www.google.de",
                      target = "_blank")))),
    windowTitle = "Title"
  ),

  tags$style(
    ".nav li a {
        font-size: 14px;
        color: black;
        font-weight: bold;
    }
    .nav li a:hover {
    border: 1px solid black;
    box-sizing: border-box;
    }
    "
  ),

  mainPanel(width = 12,
    tabsetPanel(
      tabPanel("About", htmlOutput("about1"), icon = icon("info-circle")),
      tabPanel("Instruction",  
               htmlOutput("help1"),
               icon = icon("info-circle")),
      tabPanel("Overview", DT::dataTableOutput("table_o"), icon = icon("list")),
      tabPanel("Public health", DT::dataTableOutput("table1"), icon = icon("male","female")),
      tabPanel("Animal health", DT::dataTableOutput("table"), icon = icon("dog")),
      tabPanel("Feed & food", DT::dataTableOutput("table2"), icon = icon("fish")),
      tabPanel("Literature", DT::dataTableOutput("lit"), icon = icon("book"))
      )
    ),
 div(
  class = "footer",
  includeHTML("footer1.html"))
)
