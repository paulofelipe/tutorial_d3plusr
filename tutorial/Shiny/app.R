#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(D3plusR)
library(dplyr)
library(tidyr)
library(httr)

set_config(config(ssl_verifypeer = 0L))

regions <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
                    stringsAsFactors = FALSE)
regions <- regions %>%
  select(alpha.3, region) %>%
  rename(Partner.ISO = alpha.3)

string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- jsonlite::fromJSON('partners.json')$results
paises <- reporters$id
names(paises) <- reporters$text

get.Comtrade <- function(url="https://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )

  if(fmt == "csv") {
    raw.data<- content(GET(string), "parsed") %>% data.frame()
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- rjson::fromJSON(content(GET(string), "text"))
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(

  fluidRow(
    column(2),
    column(8,
           h1("Exports and Imports by Country", align = "center")
    )
  ),
  fluidRow(
    column(2),
    column(8,
           selectInput("country",
                       label = "Select a reporter:",
                       choices = paises,
                       selected = "76",
                       multiple = FALSE)
    )
  ),
  fluidRow(
    column(2),
    column(8,
           d3plusOutput('treemap', height = 600)
           )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   data_un <- reactive({
     existe_dados <- FALSE
     ano <- 2016
     dados <- data.frame()
     while(!existe_dados){
       dados <- get.Comtrade(r = input$country, p = "all", ps = ano, fmt = "csv")$data
       existe_dados <- nrow(dados) > 1
       Sys.sleep(1.1)
       ano <- ano - 1
     }

    dados <- dados %>%
      filter(Partner != "World", Trade.Flow %in% c("Import", "Export")) %>%
      select(Year, Trade.Flow, Reporter, Reporter.ISO, Partner, Partner.ISO,
             Trade.Value..US..)

    dados_exp <- dados %>% filter(Trade.Flow == "Export") %>%
      select(-Trade.Flow) %>%
      rename(Trade_Value_Exp = Trade.Value..US..)

    dados_imp <- dados %>% filter(Trade.Flow == "Import") %>%
      select(-Trade.Flow) %>%
      rename(Trade_Value_Imp = Trade.Value..US..)

    dados <- full_join(dados_exp, dados_imp) %>%
      replace_na(list(Trade_Value_Exp = 0,
                      Trade_Value_Imp = 0))

    dados <- dados %>%
      left_join(regions) %>%
      replace_na(list(region = "Not Specified")) %>%
      mutate(region = ifelse(region == "", "Not Specified", region))
   })


   output$treemap <- renderD3plus({

     toggle_button <- list(list(Exports = "Trade_Value_Exp"),
                           list(Imports = "Trade_Value_Imp"))

     ano <- max(data_un()$Year)

     title <- paste0("Trade by Partners - ",
                     reporters$text[reporters$id == input$country],
                     " - ",
                     ano)

     d3plus(data_un(), type = "tree_map",
            id = c("region", "Partner"),
            currency_var = c("Trade_Value_Exp",
                             "Trade_Value_Imp"),
            locale = "pt_BR") %>%
       d3plusSize("Trade_Value_Exp") %>%
       d3plusUi(list(method = "size", type = "toggle",
                     label = "Trade Flow: ",
                     value = toggle_button)) %>%
       d3plusColor("region") %>%
       d3plusDepth(1) %>%
       d3plusTitle(value = title, font = list(size = 22, weight = 900),
                   total = list(value = list(prefix = "Total: "),
                                font = list(size = 16, weight = 900))) %>%
       d3plusFooter(value = "Source: UN/Comtrade.",
                    font = list(align = "left"))
   })
}

# Run the application
shinyApp(ui = ui, server = server)

