#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(dplyr)
library(plyr)
library(ggplot2)
library(highcharter)
library(reshape2)




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$text <- renderText({ 
        # Create text output for Description tab
        "This application analyses US Mass shooting Dataset from Kaggle 
        (https://www.kaggle.com/zusmani/us-mass-shootings-last-50-years).
        The database includes mass shooting from 1966 to 2017 in America.
        This application focus on those data and allows the user to perform 
        geo-spatial analysis and other analysis on mass shooting in the US."
        
    })
    
    # Create map with leaflet
    output$map <- renderLeaflet({
        # read file
        mydf <- read.csv("https://gwu-workshop-song-2019.s3.amazonaws.com/Mass+Shootings+Dataset+Ver+5.csv")
        
        # extract rows without blank latitude, blank date and bland gender
        mydf <- mydf[!is.na(mydf$Latitude)& !is.na(mydf$Date)& !is.na(mydf$Gender),]
        
        mydf$Date <- as.Date(mydf$Date)
        target1 <- c(as.integer(input$Year))
        mydf1 <- mydf[0,]
        
        #create data frame according to the input date
        for (i in target1) {
            if (i != 5){
                date_target1 <- as.Date(paste0(as.character(1966+(i-1)*10-1),"-12-31"))
                date_target2 <- as.Date(paste0(as.character(1966+(i-1)*10+10),"-1-1"))
                mydf1 <- rbind(mydf1,mydf %>% filter(Date >= date_target1 &
                                                         Date <= date_target2))
            }else{
                date_target1 <- as.Date(paste0(as.character(1966+(i-1)*10-1),"-12-31"))
                date_target2 <- as.Date(paste0(as.character(1966+(i-1)*10+12),"-1-1"))
                mydf1 <- rbind(mydf1,mydf %>% filter(Date > date_target1 &
                                                         Date <= date_target2))
            }
        }
        
        #filter the data frame according to the input gender
        mydf1 <- mydf1 %>% filter(Gender %in% input$Gender)
        
        # Create a color vector for use in legend to be displayed on map
        color <- colorFactor(c("coral","sky blue","yellow","pink"), mydf1$Gender)
        
        # Create map according to Cluster or not Cluster
        if (input$Cluster == 0){
            #create map according to not Cluster
            leaflet(mydf1) %>% 
                # Add multiple map tiles for selected map from Wikimedia and CartoDB
                addProviderTiles("CartoDB",group = "Carto") %>%
                addProviderTiles("Esri.WorldStreetMap", group = "Esri") %>%
                addProviderTiles("CartoDB.DarkMatter",group="CartoDB") %>% 
                
                #Add map markers
                addCircleMarkers(lng = ~Longitude,lat = ~Latitude,radius=3,color= color(mydf1$Gender),
                                 label = ~Title,popup=~paste("<b>",Title,",",State,"</b>","<br/>",Date,"<br/>",
                                                             "Shooter:",Gender,",","Fatalities:",as.character(Fatalities),
                                                             ",","Injured:",as.character(Injured))) %>%  addResetMapButton() %>% 
                # Add legends for different gender
                addLegend("bottomright",pal=color,values=mydf1$Gender,opacity=0.5,title = "Gender") %>%
                addLayersControl(
                    baseGroups = c("Carto","Esri","CartoDB")
                )
        } else{
            #create map according to Cluster
            leaflet(mydf1) %>% 
                #add multiple Tiles
                addProviderTiles("CartoDB",group = "Carto") %>%
                addProviderTiles("Esri.WorldStreetMap", group = "Esri") %>%
                addProviderTiles("CartoDB.DarkMatter",group="CartoDB") %>%
                #add markers
                addCircleMarkers(lng = ~Longitude,lat = ~Latitude,radius=3,color= color(mydf1$Gender),
                                 label = ~Title,popup=~paste("<b>",Title,",",State,"</b>","<br/>",Date,"<br/>",
                                                             "Shooter:",Gender,",","Fatalities:",
                                                             as.character(Fatalities),",","Injured:",
                                                             as.character(Injured)),
                                 clusterOptions = markerClusterOptions()) %>%  addResetMapButton() %>% 
                #add legend
                addLegend("bottomright",pal=color,values=mydf1$Gender,opacity=0.5,title = "Gender") %>%
                #add Contrl of tile layers
                addLayersControl(
                    baseGroups = c("Carto","Esri","CartoDB")
                )
        }
        
    })
    output$Heat <- renderPlot({
        #read data
        mydf_h <- read.csv("https://gwu-workshop-song-2019.s3.amazonaws.com/Mass+Shootings+Dataset+Ver+5.csv")
        
        #extract columns: State, Date
        mydf_h1 <- mydf_h %>% select(c("State","Date"))
        
        #create a new column based on extracing year from Date
        mydf_h1$year <- sapply(as.character(mydf_h1$Date),function(x){strsplit(x,"-")[[1]][[1]]})
        
        #count times based on State and year
        mydf_h2 <- mydf_h1 %>% count(vars=c("State","year"))
        
        #draw heat map
        ggplot(mydf_h2,aes(x=year,y=State))+geom_tile(aes(fill=freq),color="skyblue")+
            scale_fill_gradient(low="white",high="steelblue",name="Frequency")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
            geom_text(aes(year, State, label = freq), color = "dark blue", size = 2)+
            labs(title="The No. of mass shootings based on States in different years",size=4)
        
        
    })
    output$Pyramid <- renderHighchart({
        #read data
        mydf_p <- read.csv("https://gwu-workshop-song-2019.s3.amazonaws.com/Mass+Shootings+Dataset+Ver+5.csv")
        
        #extract data
        mydf_p1 <- mydf_p %>% filter(Gender == "F" | Gender == "M") %>% select(c("Date","Gender"))
        
        #create new column through extracting year from Date
        mydf_p1$year <- sapply(as.character(mydf_p1$Date),function(x){strsplit(x,"-")[[1]][[1]]})
        
        #tidy data
        mydf_p2 <- mydf_p1 %>%  count(vars=c("Gender","year"))
        mydf_pf <- filter(mydf_p2,Gender == "F")
        mydf_pm <- filter(mydf_p2,Gender == "M")
        mydf_pm$freq_f <- ifelse(mydf_pm$year %in% mydf_pf$year,1,0)
        mydf_req <- select(mydf_pm, c("year","freq","freq_f"))
        colnames(mydf_req) <- c("year","freq_m","freq_f")
        
        #create highchart
        highchart() %>%
            #create y axis
            hc_xAxis(
                
                list(
                    categories = mydf_req$year,
                    #reverse axis for left
                    reversed = FALSE,
                    #set labels
                    labels = list(step = 2)),
                list(
                    #create another y axis for right
                    opposite = TRUE,
                    categories = mydf_req$year,
                    reversed = FALSE,
                    #link to the left axis
                    linkedTo = 0,
                    labels = list(step = 2))
            ) %>%
            hc_plotOptions(series = list(#set the width of border
                borderWidth = 0))%>%
            hc_yAxis(
                #set the format of axis
                labels = list(formatter = JS("function () { return (Math.abs(this.value));}")),
                #set the range of x
                min = -65,max = 65)%>%
            hc_tooltip(#set the format for pop-up
                formatter = JS("function () {
                                   return '<b>' + this.series.name + ', Year: ' + this.point.category + '</b><br/>' +
                                     'Frequency: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}"))%>%
            hc_title(text = "Pyramid graph for mass shooting based on shooters' Gender ",align="center")%>%
            hc_plotOptions(series= list(
                #set stacking so that it dispaly pyramid graph
                stacking = "normal")) %>%
            hc_add_series(name = "Male",
                          #make the data to be negative so that it shows a symmetrical graph
                          data = -mydf_req$freq_m, 
                          type = "bar") %>%
            hc_add_series(name = "Female",data = mydf_req$freq_f,type = "bar") %>%
            hc_add_theme(hc_theme_538())
    })
    output$plot1 <- renderPlot({
        #read data
        mydf_1plot1 <- read.csv("https://gwu-workshop-song-2019.s3.amazonaws.com/Mass+Shootings+Dataset+Ver+5.csv")
        
        #extrat desired data
        mydf_1plot2 <- select(mydf_1plot1,c("Date","Fatalities","Injured","Total.victims"))
        
        #creat a new column through extract year from date
        mydf_1plot2$year <- sapply(as.character(mydf_1plot2$Date),function(x){strsplit(x,"-")[[1]][[1]]})
        
        #tidy data
        mydf_1plot3 <- mydf_1plot2 %>% select(c("Fatalities","Injured","Total.victims","year")) %>%
            group_by(year) %>% dplyr::summarise(Fatalities=sum(Fatalities),Injured=sum(Injured),Total_victims=sum(Total.victims))
        
        #draw graph depending the input of ui
        if (input$victims==3){
            if (input$plotchoice == 1){
                ggplot(mydf_1plot3,aes(x=year,y=Total_victims))+geom_bar(stat = "identity",fill= "coral", color="coral")+
                    theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                    labs(title = "The victims of mass shooting")
            } else {
                ggplot(mydf_1plot3,aes(x=year,y=Total_victims))+geom_point(color="coral")+
                    theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                    labs(title = "The victims of mass shooting")
                
            }
            
        }else if(input$victims == "1"){
            if(input$plotchoice == 1){
                ggplot(mydf_1plot3,aes(x=year,y=Fatalities))+geom_bar(stat = "identity",fill="steelblue", color="steelblue")+
                    theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                    labs(title = "The victims of mass shooting")
            } else{
                ggplot(mydf_1plot3,aes(x=year,y=Fatalities))+geom_point(color="steelblue")+
                    theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                    labs(title = "The victims of mass shooting")
            }
            
        }else if(input$victims == "2"){
            if (input$plotchoice == 1){
                ggplot(mydf_1plot3,aes(x=year,y=Injured))+geom_bar(stat ="identity",fill="thistle3",color="thistle3")+
                    theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                    labs(title = "The victims of mass shooting")
            }else{
                ggplot(mydf_1plot3,aes(x=year,y=Injured))+geom_point(color="purple")+
                    theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                    labs(title = "The victims of mass shooting")
            }
            
        }else{
            if (input$plotchoice == 1){
                m <- select(mydf_1plot3,c("year","Fatalities","Injured"))
                n <-  melt(m,id.vars = "year")
                class(n)
                ggplot(data=n,aes(x=year,y=value,fill = variable))+ 
                    geom_bar(position ="stack",stat ="identity")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                    labs(ylab="Fatalities & Injured" ,title = "The victims of mass shooting")
            } else{
                ggplot(mydf_1plot3,aes(x=year,y=Number_of_people,color=Number_of_people))+geom_point(aes(y=Fatalities,col="Fatalities"))+
                    geom_point(aes(y=Injured,col="Injured"))+theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                    labs(ylab="Fatalities & Injured", title = "The victims of mass shooting")
            }
            
        }
        
        
    })
    output$plot2 <- renderPlot({
        #read data
        mydf_2plot1 <- read.csv("https://gwu-workshop-song-2019.s3.amazonaws.com/Mass+Shootings+Dataset+Ver+5.csv")
        
        #extract data
        mydf_2plot2 <- select(mydf_2plot1,c("Date"))
        
        #creat a new column through extract year from date
        mydf_2plot2$year <- sapply(as.character(mydf_2plot2$Date),function(x){strsplit(x,"-")[[1]][[1]]})
        
        #tidy data
        mydf_2plot3 <- mydf_2plot2 %>% group_by(year) %>% count(vars=c("year"))
        colnames(mydf_2plot3) <- c("year","Frequency")
        
        #display graph according to the input of ui
        if (input$plotchoice2 == 1){
            ggplot(mydf_2plot2,aes(x=year))+geom_bar(fill="lightpink",color="white")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                labs(ylab="Frequency",title = "The frequency of mass shooting in different year")
        } else{
            ggplot(mydf_2plot3,aes(x=year,y=Frequency))+geom_point(color="lightpink")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=18,hjust = 0.5))+
                labs(ylab="Frequency",title = "The frequency of mass shooting in different year")
        }
        
    })
    
    output$table <- renderDataTable({
        #read data
        mydf_t1 <- read.csv("https://gwu-workshop-song-2019.s3.amazonaws.com/Mass+Shootings+Dataset+Ver+5.csv")
        
        #extract data
        mydf_t2 <- select(mydf_t1,c("State","Date","Open.Close.Location","Fatalities","Injured","Total.victims","Mental.Health.Issues", "Gender"))
        
        #change columns' name
        colnames(mydf_t2) <- c("State","Date","O/C Location","Fatalities","Injured","T_victims","Mental_Issues", "Gender")
        mydf_t2$Date <- as.Date(mydf_t2$Date)
        
        #display table according to the input of ui
        if (is.null(input$state)){
            mydf_t3 <- mydf_t2 %>% dplyr::filter( Date >= input$date[1] & Date <= input$date[2])
            mydf_t3$Date <- as.character(mydf_t3$Date,format='%Y-%m-%d')
            DT::datatable(mydf_t3[,input$show_vars])
            DT::datatable(mydf_t3, options = list(orderClasses = TRUE))
            #data.table::data.table(mydf_t3[,input$show_vars])
        } else {
            mydf_t3 <- mydf_t2[mydf_t2$State %in% input$state,] %>% dplyr::filter(Date >= input$date[1] & Date <= input$date[2])
            mydf_t3$Date <- as.character(mydf_t3$Date,format='%Y-%m-%d')
            DT::datatable(mydf_t3[,input$show_vars])
            DT::datatable(mydf_t3, options = list(orderClasses = TRUE))
        }
        
    })
    
})
