##########################################################
## server
##########################################################

server = shinyServer(function(input, output, session) {  
  
  data <- reactive({ 
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    
    ###############################################################################
    # The key inputs to come from the app
    ###############################################################################
    
    is       <- input$cnt             # Country name
    inc.type <- input$inc.type        # Type of scale-up
    inc.val  <- input$inc.val         # Value of scale-up
    inter    <- pin.groups %>% filter(calc_ICER=="yes") %>% pull(Code) %>% unique() %>% sort() 
    sel.cse  <- cse_g %>% pull(cause_name) %>% unique()
    
    
    ###############################################################################
    # Outputs 
    ###############################################################################
    
    proj   = project_pop(is, inter, inc.val, "yes", sel.cse, inc.type, "no", "no")
    
    dalys  <- proj$dalys %>%
      select(-c(DALY.ave, n.inters)) %>%
      gather(group, val, -year_id, -location_name) %>%
      mutate(measure = "dalys")%>%
      filter(year_id %in% 2020:2030)
    
    births <- proj$births_df %>%
      mutate(val = Male + Female, Male = NULL, Female = NULL, measure = "births")%>%
      filter(year_id %in% 2020:2030)
    
    all.lt <- proj$lt_out_df %>%
      gather(measure, val, -year_id, -group, -location_name)%>%
      filter(year_id %in% 2020:2030)
    
    pop    <- rbind(data.table(year_id = 2019:2030, group = "Baseline", measure = "population",
                               location_name = is, val = apply(proj$P0, 2, sum)),
                    data.table(year_id = 2019:2030, group = "Adjusted", measure = "population",
                               location_name = is, val = apply(proj$P1, 2, sum))) %>%
      filter(year_id %in% 2020:2030)
    
    dths   <- rbind(data.table(year_id = 2019:2030, group = "Baseline", measure = "deaths",
                               location_name = is, val = apply(proj$D0, 2, sum)),
                    data.table(year_id = 2019:2030, group = "Adjusted", measure = "deaths",
                               location_name = is, val = apply(proj$D1, 2, sum)))%>%
      filter(year_id %in% 2020:2030)
    
    
    
    ###############################################################################
    # OBS WPP ##
    ###############################################################################
    
    wpp.obs   <- obs.wpp %>% 
      filter(iso3 == countrycode::countrycode(is, "country.name", "iso3c") & year_id %in% 2020:2030) %>%
      mutate(location_name = is, iso3 = NULL)
    
    wpp.obs.d <- wpp.obs %>% filter(measure == "deaths") %>%
      rename(val = value, group = source) 
    
    wpp.obs.b <- wpp.obs %>% filter(measure == "births") %>%
      rename(val = value, group = source) 
    
    wpp.obs.lt <- wpp.obs %>% 
      rename(val = value, group = source) %>%
      mutate(measure = ifelse(measure == "imr", "q1",
                              ifelse(measure == "u5mr", "q5", measure))) %>%
      filter(measure %in% c("e0","q1","q5"))
    
    # Bring it all together for viz
    
    p.plot  <- pop %>% mutate(val = round(val))
    d.plot  <- rbind(dths, wpp.obs.d) %>% mutate(val = round(val))
    b.plot  <- rbind(births, wpp.obs.b) %>% mutate(val = round(val))
    lt.plot <- rbind(all.lt, wpp.obs.lt) %>% mutate(val = round(val, 2))
    
    def.pyr <- cbind(proj$P0[1:86,12], proj$P0[87:172,12])
    adj.pyr <- cbind(proj$P1[1:86,12], proj$P1[87:172,12])
    
    list(p.plot = p.plot, d.plot = d.plot, def.pyr = def.pyr, adj.pyr = adj.pyr,
         b.plot = b.plot, lt.plot = lt.plot, loc = is, 
         default.female  = proj$P0[1:86,],
         default.male    = proj$P0[87:172,],
         adjusted.female = proj$P1[1:86,],
         adjusted.male   = proj$P1[87:172,])
  })
  
  output$linea <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y   <- values[["p.plot"]]; loc <- values[["loc"]]; metric = "Population"  
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year_id, 
                   y = val,
                   group = group)) %>%
      hc_title(text = paste(loc,"population")) %>% 
      hc_yAxis(title = list(text = metric), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$lineb <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y   <- values[["d.plot"]]; loc <- values[["loc"]]; metric = "Deaths (all-causes)"  
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year_id, 
                   y = val,
                   group = group)) %>%
      hc_title(text = paste(loc,"deaths")) %>% 
      hc_yAxis(title = list(text = metric), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  
  output$poppy0 <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y          = values[["def.pyr"]]; loc <- values[["loc"]]
    
    xd <- y[,2]/1000
    yd <- y[,1]/1000
    xm <- -1*xd
    
    xx <- -100*xd/sum(xd,yd); yy <- 100*yd/sum(xd,yd)
    xx <- round(xx, 1); yy <- round(yy,1)
    
    categories = 0:99
    
    highchart() %>%
      hc_chart(type= 'bar')%>%
      hc_title(text= paste0(loc," ",2030, ", N = ",formatC(1000*sum(xd,yd), format="f", big.mark = ",", digits=0))) %>%
      hc_subtitle(text="Default assumptions for interventions") %>%
      hc_xAxis(list(lineColor= 'transparent', tickLength= 0, categories=categories,reversed=FALSE,labels=list(step= 5)),
               list(lineColor= 'transparent', tickLength= 0, categories= categories,opposite= TRUE,reversed= FALSE,linkedTo= 0,labels=list(step= 5)))%>%
      hc_tooltip(backgroundColor = "transparent", shared = FALSE, 
                 formatter = JS("function () {return '<b>' + 'Age ' + '</b> ' + this.point.category + '<br/>' + 
'<b>' + this.series.name + '</b> ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")) %>%
      hc_yAxis(gridLineWidth= 0, minorGridLineWidth= 0, lineColor= 'transparent', tickLength= 0, 
               title= list(text= "Population in thousands" ), labels=list(formatter=JS("function () {return Math.abs(this.value);}")))%>%
      hc_plotOptions(series=list(stacking= 'normal')) %>%
      hc_series(list(name= 'Male',  data= xm), list(name= 'Female', data= yd)) %>% 
      hc_exporting(enabled = TRUE) %>% hc_colors(cols[2:4]) 
  })
  
  output$poppy1 <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y          = values[["adj.pyr"]]; loc <- values[["loc"]]
    
    xd <- y[,2]/1000
    yd <- y[,1]/1000
    xm <- -1*xd
    
    xx <- -100*xd/sum(xd,yd); yy <- 100*yd/sum(xd,yd)
    xx <- round(xx, 1); yy <- round(yy,1)
    
    categories = 0:99
    
    highchart() %>%
      hc_chart(type= 'bar')%>%
      hc_title(text= paste0(loc," ",2030, ", N = ",formatC(1000*sum(xd,yd), format="f", big.mark = ",", digits=0))) %>%
      hc_subtitle(text="Adjusted assumptions for interventions") %>%
      hc_xAxis(list(lineColor= 'transparent', tickLength= 0, categories=categories,reversed=FALSE,labels=list(step= 5)),
               list(lineColor= 'transparent', tickLength= 0, categories= categories,opposite= TRUE,reversed= FALSE,linkedTo= 0,labels=list(step= 5)))%>%
      hc_tooltip(backgroundColor = "transparent", shared = FALSE, 
                 formatter = JS("function () {return '<b>' + 'Age ' + '</b> ' + this.point.category + '<br/>' + 
'<b>' + this.series.name + '</b> ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")) %>%
      hc_yAxis(gridLineWidth= 0, minorGridLineWidth= 0, lineColor= 'transparent', tickLength= 0, 
               title= list(text= "Population in thousands" ), labels=list(formatter=JS("function () {return Math.abs(this.value);}")))%>%
      hc_plotOptions(series=list(stacking= 'normal')) %>%
      hc_series(list(name= 'Male',  data= xm), list(name= 'Female', data= yd)) %>% 
      hc_exporting(enabled = TRUE) %>% hc_colors(cols[2:4]) 
  })
  
  output$birthb <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y   <- values[["b.plot"]]; loc <- values[["loc"]]; metric = "Births"
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year_id, 
                   y = val,
                   group = group)) %>%
      hc_title(text = paste(loc,"births")) %>% 
      hc_yAxis(title = list(text = metric), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$e0plot <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y   <- values[["lt.plot"]] %>% filter(measure == "e0"); loc <- values[["loc"]]; metric = "e0" 
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year_id, 
                   y = val,
                   group = group)) %>%
      hc_title(text = paste(loc,"life-expectancy at birth (e0)")) %>% 
      hc_yAxis(title = list(text = "Years"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$lineq1 <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y   <- values[["lt.plot"]] %>% filter(measure == "q1"); loc <- values[["loc"]]; metric = "q1" 
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year_id, 
                   y = val,
                   group = group)) %>%
      hc_title(text = paste(loc,"infant-mortality (1q0)")) %>% 
      hc_yAxis(title = list(text = "Deaths per 1,000 live births"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$lineq5 <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y   <- values[["lt.plot"]] %>% filter(measure == "q5"); loc <- values[["loc"]]; metric = "q5" 
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year_id, 
                   y = val,
                   group = group)) %>%
      hc_title(text = paste(loc,"child-mortality (5q0)")) %>% 
      hc_yAxis(title = list(text = "Deaths per 1,000 live births"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$lineq15 <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y   <- values[["lt.plot"]] %>% filter(measure == "q15"); loc <- values[["loc"]]; metric = "45q15" 
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year_id, 
                   y = val,
                   group = group)) %>%
      hc_title(text = paste(loc,"adult-mortality (45q15)")) %>% 
      hc_yAxis(title = list(text = "Deaths per 1,000 population"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
  output$lineq30 <- renderHighchart ({
    if(is.null(input$cnt)|is.null(input$inc.type)|is.null(input$inc.val)) return(NULL)
    values <- data()
    
    y   <- values[["lt.plot"]] %>% filter(measure == "q30"); loc <- values[["loc"]]; metric = "40q30" 
    
    y %>% 
      hchart(., 
             type = "spline", 
             hcaes(x = year_id, 
                   y = val,
                   group = group)) %>%
      hc_title(text = paste(loc,"adult-mortality (40q30)")) %>% 
      hc_yAxis(title = list(text = "Deaths per 1,000 population"), softMin=0) %>% 
      hc_xAxis(type = "category", title = list(text = "Year")) %>%       
      hc_tooltip(crosshairs = TRUE, backgroundColor = "transparent", shared = T) %>% 
      hc_colors(c("#e5b13a", "#4AA942", "#4bd5ee")) %>% hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())
  })
  
})