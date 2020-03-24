library(shiny)
library(ggplot2)
#############################################################pacotes da leitura do mapa
library(tidyverse)
library(sf)
library(stringr)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(DT)
library(leafpop)
library(readxl)
library(rgeos) #https://statistique-et-logiciel-r.com/premiers-pas-en-cartographie-avec-r/
library(viridis)
library(rainbow)
library(httr)
library(curl)
library(abjutils)
library(shinydashboard)
library(plotly)
library(scales)

###########################################################################################################
###########################################################################
######## Bancos de dados
###########################################################################
###########################################################################################################
###########################################################################

###########################################################################
### leitura dados banco
###########################################################################
#mapa
mapa_rs_shp <- sf::st_read("43MUE250GC_SIR.shp", quiet = TRUE)

dplyr::glimpse(mapa_rs_shp)
mapa_rs_shp <- mapa_rs_shp %>% 
  mutate(municipio = rm_accent(str_to_lower(NM_MUNICIP))) # todas as cidades com letra minuscula  e tira o acento

centroids <- gCentroid(spgeom = methods::as( object = mapa_rs_shp, Class = "Spatial" ),byid = TRUE)
latitude=c(centroids$y)
longitude= c(centroids$x)
aux_centroids = data.frame(latitude= latitude, longitude= longitude, municipio = mapa_rs_shp$municipio)
mapa_rs_shp_centroids <- merge(aux_centroids, mapa_rs_shp, by.x = "municipio", by.y = "municipio") # une os dois 

#casos leitos equipamentos covid municipios
dados_rs_municipio = read_csv("covid_municipios_rs.csv")
dados_rs_municipio <- dados_rs_municipio %>% 
  mutate(municipio = rm_accent(str_to_lower(cidade))) 

leitos_rs <- read_excel("leitos_rs.xlsx")
names(leitos_rs)=c("hospital", "municipio", "CNES", "leitos_enfermaria_ampliados", "leitos_uti_ampliados", "total_leitos_enfermaria", "total_leitos_uti", "regiao")
leitos_rs2 <- leitos_rs %>% 
  mutate(municipio = rm_accent(str_to_lower(municipio))) %>% 
  group_by(municipio, regiao) %>%
  summarise(leitos_enfermaria_ampliados = sum(leitos_enfermaria_ampliados),
            leitos_uti_ampliados = sum(leitos_uti_ampliados),
            total_leitos_enfermaria = sum(total_leitos_enfermaria),
            total_leitos_uti = sum(total_leitos_uti))

equipamentos_manutencao_vida_rs1 <- read_excel("equipamentos_manutencao_vida_rs.xlsx", skip = 4)
names(equipamentos_manutencao_vida_rs1)= c("var")
equipamentos_manutencao_vida_rs = equipamentos_manutencao_vida_rs1 %>% 
  separate(var, into = c("cases", "equipamentos"), sep = ";") %>%
  separate(cases, into = c("codigo", "municipio"), sep = 7) %>%
  mutate(municipio=rm_accent(str_to_lower(municipio)) )
equipamentos_manutencao_vida_rs$manuntecao_vida_equipamentos=as.numeric(equipamentos_manutencao_vida_rs$equipamentos)


n_municipios=length(mapa_rs_shp_centroids$municipio)
aux_dados_todos =data.frame(municipio = mapa_rs_shp_centroids$municipio,  data = rep("na", n_municipios), 
                      casos = rep(0, n_municipios), regiao =rep("na", n_municipios), 
                      leitos_uti_ampliados = rep(0, n_municipios), leitos_enfermaria_ampliados = rep(0, n_municipios),
                      total_leitos_uti = rep(0, n_municipios), total_leitos_enfermaria = rep(0, n_municipios),
                      manuntecao_vida_equipamentos = rep(0, n_municipios))

aux_dados_todos2 = dplyr::bind_rows(dados_rs_municipio, leitos_rs2, equipamentos_manutencao_vida_rs) 
aux_dados_todos2 = aux_dados_todos2%>%
                      select(-codigo, -equipamentos, -cidade)%>%
                      replace_na(list(data = "na", casos = 0, regiao = "na", leitos_enfermaria_ampliados = 0,
                                      leitos_uti_ampliados = 0, total_leitos_enfermaria=0, 
                                      total_leitos_uti = 0, manuntecao_vida_equipamentos =0))

aux_dados_todos3=rbind(aux_dados_todos, aux_dados_todos2)
# ###########################################################################
# ######## unindo os bancos
# ###########################################################################
banco_covid_rs <- merge(aux_dados_todos3, mapa_rs_shp_centroids, by.x = "municipio", by.y = "municipio") 
dplyr::glimpse(banco_covid_rs) 
banco_covid_rs = st_as_sf(banco_covid_rs)
banco_covid_rs <- st_transform(banco_covid_rs, "+init=epsg:4326")

###########################################################################################################
###########################################################################
######## Aplicativo
###########################################################################
###########################################################################################################
###########################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Dados COVID19 no Rio Grande do Sul",
                  titleWidth = 500),
  dashboardSidebar
  (sidebarMenu(
    menuItem("Mapa casos de COVID19 por municipio", tabName = "mapa_porporcao"),
    menuItem("Mapa quantidade de leitos plano acao", tabName = "mapa_leitos"),
    menuItem("Mapa quantidade equipamentos mantedores da vida", tabName = "mapa_equipamentos"),
    menuItem("Serie Temp. casos", tabName = "serie_proporcao"),
    menuItem("Tabela casos confirmados covid19", tabName = "tabela"),
    menuItem("Tabela quantidade de leitos do plano de ação da Sec. Saúde", tabName = "tabela_leitos"),
    menuItem("Tabela quantidade de equipamentos mantendendores da vida", tabName = "tabela_equipamentos")
  )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("serie_proporcao",
              fluidPage(
                titlePanel("Serie Temporal do numero de casos do COVID19 por municipio"),
                
                fluidRow(
                  sidebarPanel(
                    #h3("Escolha a Cidade"),
                    selectizeInput("cidade1",
                                   label = "Escolha a Cidade",
                                   choices = unique(dados_rs_municipio$municipio),
                                   multiple = T,
                                   options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                                   selected = "porto alegre"),
                    sliderInput("limite1",
                                "Limites do eixo vertical",
                                min = (0),
                                max = round(max(dados_rs_municipio$casos)+50, digits = 4),
                                value = c(0,round(max(dados_rs_municipio$casos)+50, digits = 4)),
                                step = 1),
                    width = 12
                  )
                ),
                mainPanel(
                  plotlyOutput("grafico_serie_proporcao", height = "600px"),
                  width = 12
                )
              )
      ),
      
      
      tabItem("tabela",
              fluidPage(
                titlePanel("Tabela com casos por município no Rio Grande do Sul"),
                mainPanel(
                  DTOutput(outputId = "tabela")
                )
              )
      ),

      
      tabItem("tabela_leitos",
              fluidPage(
                titlePanel("Tabela leitos por município no Rio Grande do Sul"),
                mainPanel(
                  DTOutput(outputId = "tabela_leitos")
                )
              )
      ),
      
      tabItem("tabela_equipamentos",
              fluidPage(
                titlePanel("Numero de equipamentos mantededores da vida"),
                mainPanel(
                  DTOutput(outputId = "tabela_equipamentos")
                )
              )
      ),
      
      tabItem("mapa_porporcao",
              fluidPage(
                titlePanel("Mapa do número de casos confirmados de COVID19"),
                
                fluidRow(
                  column(7,
                         selectizeInput("y",
                                        label = "Escolha a data",
                                        choices = unique(dados_rs_municipio$data),
                                        selected = "21-3-2020")
                  ),
                  mainPanel(
                    leafletOutput("grafico_mapa_proporcao", height = "600px"),
                    width = 12
                  )
                )
              )
              
      ),
      
      tabItem("mapa_equipamentos",
              fluidPage(
                titlePanel("Mapa do número de equipamentos mantendedores da vida"),
                
                fluidRow(
                  column(7,
                         selectizeInput("y_data_equipamento",
                                        label = "Escolha a data",
                                        choices = unique(dados_rs_municipio$data),
                                        selected = "21-3-2020")
                  ),
                  mainPanel(
                    leafletOutput("grafico_mapa_equipamentos", height = "600px"),
                    width = 12
                  )
                )
              )
              
      ),
      
      tabItem("mapa_leitos",
              fluidPage(
                titlePanel("Mapa dos casos confirmados de COVID19 e do número de leitos no nível 3 do 
                           plano de ação da secretaria de saúde do RS"),
                
                fluidRow(
                         selectizeInput("tipo_leito",
                                        label = "Escolha o tipo de leito",
                                        choices = c("leitos_uti_ampliados", "leitos_enfermaria_ampliados",
                                                    "total_leitos_uti", "total_leitos_enfermaria"), 
                                        selected = "total_leitos_uti"),
                         selectizeInput("calendario",
                                        label = "Escolha a data",
                                        choices = unique(dados_rs_municipio$data),
                                        selected = "21-3-2020"),
                  mainPanel(
                    leafletOutput("grafico_mapa_leitos", height = "600px"),
                    width = 12
                  )
                )
              )
              
      )
      
      ##
      
    )
  )
)


server <- function(input, output) {
  
  output$grafico_serie_proporcao <- renderPlotly({
    
    dados_rs_municipio$data <- as.Date(dados_rs_municipio$data,format = "%d-%m-%Y") 
    
    serie2 <- dados_rs_municipio %>%
      filter(municipio %in% input$cidade1) 
    
    
    ggplotly(
      ggplot(serie2, aes(x = data , y = casos, colour = `municipio`)) +
        geom_line(size = 1) +
        geom_point(size = 2)+
        ylim(input$limite1[1],c(input$limite1[2]+50))+
        labs(x = "dia", y = "Numero de casos confirmados")+
        theme( axis.text.x = element_text(angle=90))+
        scale_x_date(labels = date_format("%d-%m"))
    )
    
  })
  
  
  output$grafico_mapa_proporcao <- renderLeaflet({
    
    banco_covid_rs_tudo = banco_covid_rs %>%  # aqui coloca o filtro 
      filter(data == input$y | data=="na") %>%
      group_by(municipio) %>%
      mutate(data==input$y)%>%
      summarise(casos=max(casos))
    
    ## banco filtrado pra fazer o grafico
    banco_covid_rs_filtro = banco_covid_rs %>%
                                  filter(data == input$y)
    
    ####################################################    
    y_quantidade=banco_covid_rs_tudo$casos
    bins <- c(0, 1, 2, 3, 10, 20, 30, 40, max(y_quantidade))
    #bins = c(1, 100)
    #pal <- colorBin("magma", domain = y, bins = bins)
    #pal <- colorBin("white", domain = y)
    pal <- colorBin("YlOrRd", domain = y_quantidade, bins = bins)
    
    
    leaflet(banco_covid_rs_tudo) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addCircleMarkers(data=banco_covid_rs_filtro, lng=banco_covid_rs_filtro$longitude, 
                       lat = banco_covid_rs_filtro$latitude, radius=~banco_covid_rs_filtro$casos, 
                       weight = 1, color = "red", fillOpacity = 0.5)%>%
      addPolygons(fillColor = ~pal(y_quantidade), 
                  weight = 1,
                  opacity = 0.5,
                  fillOpacity = 0.5,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s - numero de casos %s", banco_covid_rs_tudo$municipio, round(y_quantidade, 6)),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~y_quantidade, opacity = 0.7, title = "casos confirmados",
                labFormat = labelFormat(digits = 5),
                position = "bottomright") #%>%
   
    #popup=~banco_covid_rs_filtro$casos)# %>%
    #addLegendCustom(colors = c("red"), labels = c("casos confirmados"), sizes = c(5))
  })
  
  ######
  
  output$grafico_mapa_leitos <- renderLeaflet({
    
    # banco filtro leitos
    #aux_nome2 = input$tipo_leito
    #aux_aux_nome2 = which(ax_nome2==names(banco_covid_rs))
    #nomes_filtro= c(municipio,input$tipo_leito )
    #data[,names(data) %in% input$show_vars]
    
    #banco_covid_rs[, c(municipio,input$tipo_leito)]
    
    aux_nome_leito = which(input$tipo_leito ==names(banco_covid_rs))  
    
    banco_covid_rs_leitos = banco_covid_rs %>%  # aqui coloca o filtro
      group_by(municipio) %>%
      summarize_at(colnames(banco_covid_rs)[aux_nome_leito], funs(max))
      #summarize_at(vars(starts_with(input$tipo_leito)), leito_escolhido = funs(max))
    

    ## banco filtrado casos
    banco_covid_rs_calendario_filtro = banco_covid_rs %>%
        filter(data == input$calendario)
      
    ####################################################    
    y_quantidade_leitos=data.frame(banco_covid_rs_leitos)[,2]
    bins <- c(0, 10, 20, 30, 50, 100, 300)
    #bins = c(1, 100)
    #pal <- colorBin("magma", domain = y, bins = bins)
    #pal <- colorBin("white", domain = y)
    # pal <- colorBin("Blues", domain = y_quantidade_leitos, bins = 5)
    pal <- colorBin("Blues", domain = y_quantidade_leitos, bins = bins)                                                   

    leaflet(banco_covid_rs_leitos) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addCircleMarkers(banco_covid_rs_calendario_filtro, lng=  banco_covid_rs_calendario_filtro$longitude, 
                       lat =   banco_covid_rs_calendario_filtro$latitude, radius=~banco_covid_rs_calendario_filtro$casos, 
                       weight = 1, color = "red", fillOpacity = 2)%>%
       addLegend(colors=rep("red"), labels=c("casos confirmados"))%>%
      addPolygons(fillColor = ~pal(y_quantidade_leitos), 
                  weight = 1,
                  opacity = 0.6,
                  fillOpacity = 0.6,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s - numero leitos %s", banco_covid_rs_leitos$municipio, round(y_quantidade_leitos, 6)),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~y_quantidade_leitos, opacity = 0.9,
                title = input$tipo_leito,
                labFormat = labelFormat(digits = 5),
                position = "bottomright") 
  })
  
  output$grafico_mapa_equipamentos <- renderLeaflet({
    
    banco_covid_rs_equipamentos = banco_covid_rs %>%  # aqui coloca o filtro 
      filter(data == input$y_data_equipamento| data=="na") %>%
      group_by(municipio) %>%
      #mutate(data=="20-3-2020")
      mutate(data==input$y)%>%
      summarise(equipamentos=max(manuntecao_vida_equipamentos))
    
    ## banco filtrado pra fazer o grafico
    banco_covid_rs_filtro = banco_covid_rs %>%
      filter(data == input$y_data_equipamento)
    
    ####################################################    
    y_quantidade=banco_covid_rs_equipamentos$equipamentos
    bins <- c(0, 10, 50, 100, 1000, 2000, 3000, max(y_quantidade))
    #bins = c(1, 100)
    #pal <- colorBin("magma", domain = y, bins = bins)
    #pal <- colorBin("white", domain = y)
    pal <- colorBin("Blues", domain = y_quantidade, bins = bins)
    
    
    leaflet(banco_covid_rs_equipamentos) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addCircleMarkers(data=banco_covid_rs_filtro, lng=banco_covid_rs_filtro$longitude, 
                       lat = banco_covid_rs_filtro$latitude, radius=~(banco_covid_rs_filtro$casos)*2, 
                       weight = 1, color = "red", fillOpacity = 300)%>%
      addLegend(colors=rep("red"), labels=c("casos confirmados"))%>%
      addPolygons(fillColor = ~pal(y_quantidade), 
                  weight = 1,
                  opacity = 0.6,
                  fillOpacity = 0.6,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s - numero equipamentos %s", banco_covid_rs_equipamentos$municipio, round(y_quantidade, 6)),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~y_quantidade, opacity = 0.7, title = "quantidade de equipamentos",
                labFormat = labelFormat(digits = 5),
                position = "bottomright") #%>%
    
    #popup=~banco_covid_rs_filtro$casos)# %>%
    #addLegendCustom(colors = c("red"), labels = c("casos confirmados"), sizes = c(5))
  })
  
  
   
  output$tabela <- renderDT(dados_rs_municipio)
  
  output$tabela_leitos <- renderDT(leitos_rs)
  
  output$tabela_equipamentos <- renderDT(equipamentos_manutencao_vida_rs)
  
  
}

shinyApp(ui, server)



