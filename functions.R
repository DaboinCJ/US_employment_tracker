NUM<-function(x, decimals=0){
  prettyNum(round(x,decimals), big.mark = ",")
}

set_1_no_yellow<-c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "black", "#A65628", "#F781BF", "#999999","green","gold")

# Vector to order legend from highest to lowest employment index to date
ces_plots<-function(data=df_1_clean, start_date="2019-12-01", title_text=""){
  
  order_legend<-data %>% 
    filter(date==max(date)) %>% 
    left_join(data %>% 
                select(date,value_100=value,seriesID), by=c("date","seriesID")) %>% 
    arrange(desc(value/value_100)) %>% 
    pull(industry_name)
  
  data$industry_name_2<-factor(str_wrap(data$industry_name,25), 
                               levels = str_wrap(order_legend,25))
 
  theme_set(theme_minimal())
  theme_update(axis.text = element_text(size = 12),
               axis.text.x = element_text(angle=60,hjust = 1),
               axis.title = element_text(size = 12), 
               legend.text = element_text(size=ifelse(length(order_legend)<6,12,6)),
               legend.title.align = 0.3,
               legend.title = element_text(size=15), 
               legend.key.size = unit(1.4, "cm"))
   
  # chart in levels
  plot_0<-data %>% 
    filter(date >= start_date) %>%
    ggplot(aes(x=date, y=value, group=industry_name,
               color=industry_name_2))+
    geom_line(linewidth=1)+
    geom_point(size=1)+
    ggrepel::geom_label_repel(aes(label=ifelse(date == max(data$date),
                                               paste0(NUM(value,0)),NA)),
                              show.legend = FALSE)+
    scale_y_continuous(labels=scales::comma)+
    scale_x_date(date_labels = "%b %y")+
    scale_color_manual(values=set_1_no_yellow)+
    labs(title = paste("Monthly Jobs by Sector"),
         color="Sector",
         y="Number of jobs",
         x="Date",
         subtitle = paste0("Nofarm employment, not seasonally adjusted"))
  
  # Chart in recovery terms
  plot_1<-data %>% 
    filter(date >= start_date) %>%
    left_join(data %>% 
                filter(date == start_date) %>% 
                select(value_100=value,seriesID), by="seriesID") %>% 
    ggplot(aes(x=date, y=value/value_100, group=industry_name_2,
               color=industry_name_2))+
    geom_line(linewidth=1)+
    geom_point(size=1)+
    ggrepel::geom_label_repel(aes(label=ifelse(date == max(data$date),
                                               paste0(round(value/value_100*100,0)),NA)),
                              show.legend = FALSE)+
    scale_y_continuous()+
    scale_x_date(date_breaks = "4 months",date_labels = "%b %y")+
    scale_color_manual(values=set_1_no_yellow)+
    labs(title = paste("Recent evolution of Nofarm employment", title_text),
         color="Sector",
         y="Jobs Index (Dec 2019 = 100)",
         x="Date",
         subtitle = paste0("Not seasonally adjusted"))
  # plot_1
  
  complement<-data %>% 
    filter(date >= start_date) %>%
    group_by(date) %>% 
    mutate(date_value=sum(value)) %>% 
    ggplot(aes(x=date, y=value/date_value, group=industry_name_2,
               fill=industry_name_2))+
    geom_area(size=1)+
    # ggrepel::geom_label_repel(aes(label=ifelse(date == max(df_clean$date),
    #                                            paste0(round(value/value_100*100,0)),NA)),
    #                           show.legend = FALSE)+
    scale_y_continuous()+
    scale_x_date(date_breaks = "4 months",date_labels = "%b %y")+
    scale_fill_manual(values=set_1_no_yellow)+
    theme(legend.position = "none")+
    labs(title = NULL,
         color="Sector",
         y="Share of employment",
         x="Date")
  
  
  # (plot_1+ 
  #     theme(axis.text.x = element_blank(),
  #           axis.title.x = element_blank()))+
  #   complement+plot_layout(ncol=1, heights = c(3,1), guides="collect")
  
  return(list(plot_0,plot_1,complement))
}