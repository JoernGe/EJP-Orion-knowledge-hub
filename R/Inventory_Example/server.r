server <- function(input, output, session){
  ######## Read Data ######
  ### Introduction files 
  output$about1 <- renderText({paste(readLines("about2.html"), collapse="\n")})
  output$help1 <- renderText({paste(readLines("Guide2.html"), collapse="\n")})
  
  ### Read and clean literature data 
  df <- bib2df("EJP Orion WP2Epi Data sources3.bib", separate_names = FALSE)
  df$YEAR <- as.numeric(df$YEAR)
  df$AUTHOR <- unlist(lapply(df$AUTHOR, paste, collapse = "; "))
  df <- df %>% arrange(AUTHOR)
  df1 <- df[,c("CATEGORY","AUTHOR","TITLE","JOURNAL","VOLUME","YEAR","BOOKTITLE","CHAPTER","CROSSREF","EDITION","EDITOR","ISSN","DOI","URL","ABSTRACT")]

  ### Read and clean AH data 
  data_AH <- data.frame(read_excel("Inventory on animal health surveillance.xlsx"))
  data_AH$Year_start <- as.numeric(format(data_AH$Year_start, format="%Y"))
  data_AH <- data_AH %>% 
    mutate(URL = gsub(pattern = "([\\w-]*) \\((report|database|map|report overview|Literature|website): (.*?)\\)", 
                      replacement = "<a href='\\3' target='_blank'>\\1</a> (\\2)",
                      URL,
                      perl = TRUE)) %>% 
    mutate(URL = gsub(",", ",<br>", URL))
  names(data_AH) <- c("Hazard", "Subtype","Disease","Year start","Year end","Country","Region","Disease status","Surveillance objective","EU legal classification",
                      "National legal classification","Sampling context","case definition","Sampling strategy", "Target species","Target unit",
                      "Sampling stage","Sampling unit","Sample type","Sampler","URL")
  vals_trich<-reactiveValues()
  vals_trich$Data<-data_AH
  
  ### Read and clean PH data 
  data_PH <- data.frame(read_excel("Inventory on public health surveillance.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "date", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text")))
  data_PH$Year.start <- as.numeric(format(data_PH$Year.start, format="%Y"))
  data_PH <- data_PH %>% 
    mutate(URL = gsub(pattern = "([\\w-]*) \\((report|database|map|report overview|Literature|website): (.*?)\\)", 
                      replacement = "<a href='\\3' target='_blank'>\\1</a> (\\2)",
                      URL,
                      perl = TRUE)) %>% 
    mutate(URL = gsub(",", ",<br>", URL))
  
  
  names(data_PH) <- c("Hazard", "Subtype","Disease","Year start","Year end","Country","Region","Disease status","Surveillance objective","EU legal classification",
                    "National legal classification","Sampling context","case definition","Sampling strategy", "Target species","Target unit",
                    "Sampling stage","Sampling unit","Sample type","Sampler","URL")
  
  ### Read and clean FF data   
  data_FF <- data.frame(read_excel("Inventory on food and feed surveillance.xlsx"))
  data_FF$Year.start <- as.numeric(format(data_FF$Year.start, format="%Y")) 
  data_FF <- data_FF %>% 
    mutate(URL = gsub(pattern = "([\\w-]*) \\((report|database|map|report overview|Literature|website): (.*?)\\)", 
                      replacement = "<a href='\\3' target='_blank'>\\1</a> (\\2)",
                      URL,
                      perl = TRUE)) %>% 
    mutate(URL = gsub(",", ",<br>", URL))
  names(data_FF) <- c("Hazard", "Subtype","Year start","Year end","Country","Region","Disease status","Surveillance objective","EU legal classification",
                    "National legal classification","Sampling context","case definition","Sampling strategy", "Target matrix","Target unit",
                    "Sampling stage","Sampling unit","Sample type","Sampler","URL")
  
  
  data_AH_sub <- data_AH[,1:6]
  data_AH_sub$sector <- "Animal health"
  data_PH_sub <- data_PH[,1:6]
  data_PH_sub$sector <- "Public health"
  data_FF_sub <- data_FF[,1:5]
  data_FF_sub$sector <- "Food & Feed"
  data_FF_sub$Disease <- ""
  data_FF_sub <- data_FF_sub[,c(1,2,7,3,4,5,6)]
  data_o <- rbind(data_AH_sub,data_PH_sub,data_FF_sub)
  data_o <- data_o[,c(7,1,2,3,4,5,6)]
  #data_o <- unique(data_o)
  
  
  ###### Create download links #####

  output$downloadGuide <- downloadHandler(
    filename <- function() {
      paste("Guide to answer the Surveillance Systems Tables.pdf")
    },
    content <- function(file) {
      file.copy("./documents/Guide to answer the Surveillance Systems Tables.pdf", file)
    },
    contentType = "pdf"
  )
  
  output$downloadExampleAH <- downloadHandler(
    filename <- function() {
      paste("./documents/Inventory on animal health surveillance.xlsx")
    },
    content <- function(file) {
      file.copy("./documents/Inventory on animal health surveillance.xlsx", file)
    },
    contentType = "Excel"
  )

  output$downloadExamplePH <- downloadHandler(
    filename <- function() {
      paste("./documents/Inventory on public health surveillance.xlsx")
    },
    content <- function(file) {
      file.copy("Inventory on public health surveillance.xlsx", file)
    },
    contentType = "Excel"
  )

  output$downloadExampleFF <- downloadHandler(
    filename <- function() {
      paste("Inventory on food and feed surveillance.xlsx")
    },
    content <- function(file) {
      file.copy("Inventory on food and feed surveillance.xlsx", file)
    },
    contentType = "Excel"
  )

  #### render DataTable part ####
  ### Literature
  ## Responsive is for + Button
  output$lit <-
    renderDataTable(datatable({data=df1 %>% mutate(URL = paste0("<a href='", URL,"' target='_blank'>", URL,"</a>"))
    data},editable = FALSE, selection = 'single', extensions = c('Responsive','Buttons', 'FixedHeader'),
    options = list(scrollX = TRUE,
                   dom = 'Blfrtip',
                   buttons = c('csv', 'excel'),
                   pageLength = 20,
                   lengthMenu = c(5,10,20,50,100,200),
                   AutoWidth = TRUE,
                   fixedHeader = FALSE),
    escape=F)
    )
  
  
  output$table_o <- renderDataTable(
    datatable(data=data_o , rownames = FALSE, editable = FALSE, 
              selection = 'single', filter = 'top',
              extensions = c('RowGroup','Responsive','Buttons', 'FixedHeader'),
              options = list(scrollX = TRUE,
                             dom = 'Blfrtip',
                             buttons = c('csv', 'excel'),
                             deferRender = TRUE,
#                             columnDefs = list(list(className = 'dt-center', targets = 5)),
                             pageLength = 20,
                             lengthMenu = c(5,10,20,50),
                             AutoWidth = TRUE,
                             fixedHeader = FALSE),
              escape = F)
  )
  
    
  output$table <- renderDataTable(
    datatable(data=vals_trich$Data ,editable = FALSE, selection = 'single',
              rownames = FALSE, 
    extensions = c('RowGroup','Responsive','Buttons', 'FixedHeader'),
    options = list(scrollX = TRUE,
                   dom = 'Blfrtip',
                   buttons = c('csv', 'excel'),
                   deferRender = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = 5)),
                   pageLength = 20,
                   lengthMenu = c(5,10,20,50),
                   AutoWidth = TRUE,
                   fixedHeader = FALSE),
    escape = F, callback = JS('table.page(3).draw(false);'))
  )

  
  output$table1 <- renderDataTable(
    datatable(data=data_PH,editable = FALSE, selection = 'single',
              rownames = FALSE, 
              extensions = c('RowGroup','Responsive','Buttons', 'FixedHeader'),
    options = list(scrollX = TRUE,
                   dom = 'Blfrtip',
                   buttons = c('csv', 'excel'),
                   deferRender = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = 5)),
                   pageLength = 20,
                   lengthMenu = c(5,10,20,50),
                   AutoWidth = TRUE,
 fixedHeader = TRUE),
    escape = F)
  )

  
  output$table2 <- renderDataTable(
    datatable(data=data_FF,editable = FALSE, selection = 'single',
              rownames = FALSE, 
              extensions = c('RowGroup','Responsive','Buttons', 'FixedHeader'),
              options = list(scrollX = TRUE,
                             dom = 'Blfrtip',
                             buttons = c('csv', 'excel'),
                             deferRender = TRUE,
                             columnDefs = list(list(className = 'dt-center', targets = 5)),
                             pageLength = 20,
                             lengthMenu = c(5,10,20,50),
                             AutoWidth = TRUE,
                             fixedHeader = FALSE),
    escape = F)
  )
}