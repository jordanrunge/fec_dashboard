

shinyServer(function(input, output) {


  ####################
  ### Total Amount ###
  ####################
  
  pete_total_amount = reactive({
    zip_choropleth(p_total_amount,
                   state_zoom = ifelse(input$pick_state == 'Florida', "florida",
                                ifelse(input$pick_state == 'Iowa', "iowa",
                                ifelse(input$pick_state == 'Michigan', "michigan",
                                ifelse(input$pick_state == 'Ohio', "ohio",
                                ifelse(input$pick_state == 'Pennsylvania', "pennsylvania", "wisconsin"))))),
                   num_colors = 9,
                   title      = "",
                   legend     = "Total Amount ($)")
  })
  
  pete_total_amount_bar = reactive({
    df %>%
      filter(state == input$pick_state) %>%
      group_by(zip) %>% 
      summarize(amount = sum(amount)) %>% 
      arrange(desc(amount)) %>% 
      top_n(10) %>%
      ggplot(aes(reorder(zip, -amount), amount)) +
      geom_col(aes(fill=amount), position='dodge', fill="#002649") +
      xlab('Zip Code') +
      ylab('Total Raised') +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(color="grey10", size=12, angle=45),
            axis.text.y = element_text(color="grey10", size=12),
            axis.title.x = element_text(color="grey10", size=14, face="bold"),
            axis.title.y = element_text(color="grey10", size=14, face="bold"),
            legend.position = 'none') +
      scale_y_continuous(labels = scales::dollar_format(prefix="$"))
      
  })
  
  monthly_amount_state = reactive({
    df %>%
      filter(state == input$pick_state) %>%
      group_by(candidate, month, month_name) %>%
      summarise(monthly_total = sum(amount)/1e3) %>%
      ggplot(aes(x=month_name, y=monthly_total, group=candidate)) +
      geom_line(aes(color=candidate), size=1.5) +
      xlab('Month') +
      ylab('Amount Raised') +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(color="grey10", size=12),
            axis.text.y = element_text(color="grey10", size=12),
            axis.title.x = element_text(color="grey10", size=14, face="bold"),
            axis.title.y = element_text(color="grey10", size=14, face="bold"),
            legend.position = "none") +
      scale_color_manual(name="Candidate", values=c("#002649", "#8E1600"), ) +
      scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
      scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix=("K")))
  })
  
  #####################
  ### Unique Donors ###
  #####################
  
  pete_total_donors = reactive({
    zip_choropleth(p_total_donors,
                   state_zoom = ifelse(input$pick_state == 'Florida', "florida",
                                ifelse(input$pick_state == 'Iowa', "iowa",
                                ifelse(input$pick_state == 'Michigan', "michigan",
                                ifelse(input$pick_state == 'Ohio', "ohio",
                                ifelse(input$pick_state == 'Pennsylvania', "pennsylvania", "wisconsin"))))),
                   num_colors = 9,
                   title      = "",
                   legend     = "Unique Donors")
  })

  pete_total_donors_bar = reactive({
    df %>%
      filter(state == input$pick_state) %>%
      group_by(zip) %>% 
      summarize(n_donors = n_distinct(donor)) %>% 
      arrange(desc(n_donors)) %>% 
      top_n(10) %>%
      ggplot(aes(reorder(zip, -n_donors), n_donors)) +
      geom_col(aes(fill=n_donors), position='dodge', fill="#002649") +
      xlab('Zip Code') +
      ylab('Unique Donors') +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(color="grey10", size=12, angle=45),
            axis.text.y = element_text(color="grey10", size=12),
            axis.title.x = element_text(color="grey10", size=14, face="bold"),
            axis.title.y = element_text(color="grey10", size=14, face="bold"),
            legend.position = 'none') +
      scale_y_continuous(labels = scales::label_comma())
  })
  
  monthly_donors_state = reactive({
    df %>%
      filter(state == input$pick_state) %>%
      group_by(candidate, month, month_name) %>%
      summarize(n_donors = n_distinct(donor)) %>%
      ggplot(aes(x=month_name, y=n_donors, group=candidate)) +
      geom_line(aes(color=candidate), size=1.5) +
      xlab('Month') +
      ylab('Unique Donors') +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(color="grey10", size=12),
            axis.text.y = element_text(color="grey10", size=12),
            axis.title.x = element_text(color="grey10", size=14, face="bold"),
            axis.title.y = element_text(color="grey10", size=14, face="bold"),
            legend.position = "none") +
      scale_color_manual(name="Candidate", values=c("#002649", "#8E1600"), ) +
      scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
      scale_y_continuous(labels = scales::label_comma())
  })
  
  
  #####################
  ### Avg per Donor ###
  #####################
  
  pete_avg_donor = reactive({
    zip_choropleth(p_avg_donor,
                   state_zoom = ifelse(input$pick_state == 'Florida', "florida",
                                ifelse(input$pick_state == 'Iowa', "iowa",
                                ifelse(input$pick_state == 'Michigan', "michigan",
                                ifelse(input$pick_state == 'Ohio', "ohio",
                                ifelse(input$pick_state == 'Pennsylvania', "pennsylvania", "wisconsin"))))),
                   num_colors = 9,
                   title      = "",
                   legend     = "Avg Amount per Donor ($)")
  })
  
  pete_avg_donor_bar = reactive({
    df %>%
      filter(state == input$pick_state) %>%
      group_by(zip) %>% 
      summarize(avg_donor = sum(amount)/n_distinct(donor)) %>%
      arrange(desc(avg_donor)) %>% 
      top_n(10) %>%
      ggplot(aes(reorder(zip, -avg_donor), avg_donor)) +
      geom_col(aes(fill=avg_donor), position='dodge', fill="#002649") +
      xlab('Zip Code') +
      ylab('Avg Amount per Donor') +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(color="grey10", size=12, angle=45),
            axis.text.y = element_text(color="grey10", size=12),
            axis.title.x = element_text(color="grey10", size=14, face="bold"),
            axis.title.y = element_text(color="grey10", size=14, face="bold"),
            legend.position = 'none') +
      scale_y_continuous(labels = scales::dollar_format(prefix="$"))
  })
  
  monthly_avg_donor_state = reactive({
    df %>%
      filter(state == input$pick_state) %>%
      group_by(candidate, month, month_name) %>%
      summarize(avg_donor = sum(amount)/n_distinct(donor)) %>%
      ggplot(aes(x=month_name, y=avg_donor, group=candidate)) +
      geom_line(aes(color=candidate), size=1.5) +
      xlab('Month') +
      ylab('Amount Raised') +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(color="grey10", size=12),
            axis.text.y = element_text(color="grey10", size=12),
            axis.title.x = element_text(color="grey10", size=14, face="bold"),
            axis.title.y = element_text(color="grey10", size=14, face="bold"),
            legend.position = "none") +
      scale_color_manual(name="Candidate", values=c("#002649", "#8E1600"), ) +
      scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
      scale_y_continuous(labels = scales::dollar_format(prefix="$"))
  })
  
  
  
##################################################################################################
##################################################################################################


  ####################
  ### Total Amount ###
  ####################

     output$p_map_total_amount = renderPlot(
       pete_total_amount()
     )

     output$p_total_amount_bar = renderPlot(
      pete_total_amount_bar()
     )
     
     output$state_total_amount = renderPlot(
       monthly_amount_state()
     )
     
     
     
   #####################
   ### Unique Donors ###
   #####################
     
     output$p_map_unique_donors = renderPlot(
       pete_total_donors()
     )
     
     output$p_unique_donors_bar = renderPlot(
       pete_total_donors_bar()
     )
     
     output$state_unique_donors = renderPlot(
       monthly_donors_state()
     )
     

     
   #####################
   ### Avg per Donor ###
   #####################
     
     output$p_map_avg_donor = renderPlot(
      pete_avg_donor()
     )
     
     output$p_avg_donor_bar = renderPlot(
       pete_avg_donor_bar()
     )
     
     output$state_avg_donor = renderPlot(
       monthly_avg_donor_state()
     )

     

##################
### Donor List ###
##################
    
    output$query = DT::renderDataTable({
      datatable(df[, c(7,3,5,2,4)], rownames=F,
                colnames = c('Donor', 'Amount', 'Date', 'State', 'Zip Code'),
                filter = 'top', options=list(order = list(list(4, 'asc'))))
    })
    
      
})#closes Server






