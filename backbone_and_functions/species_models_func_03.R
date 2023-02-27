#Created by Keith Post on 2/9/23
#Code for functions used in Species Models App for Rarefaction mini-app


#### Functions for Species Accumulation Models======================================================
### Estimates richness from subsample of data (frame) & converts to tibble
est_s_as_tib<-function(data){
  data %>%
    colSums() %>%
    .[.>0] %>%
    estimateR() %>%
    as.data.frame() %>%
    t() %>%
    as_tibble() %>%
    mutate(across(everything(),~round(.x,2))) %>%
    clean_names()
}


### Plots species-accumulation curve
plotly_specaccum<-function(data,nm){
  data %>%
      ggplot(aes(x=!!sym(nm),y=richness)) +
      ggtitle("Species-Accumulation Curve") +
      geom_point() +
      theme_bw() +
      theme(title=element_text(size=12)) -> p
    
    p %>%
      ggplotly()
}


#### Functions for Rarefaction======================================================================
### Plots rarecurve as a ggplotly 
plotly_rarefy<-function(data,n){
  data %>%
    ggplot(aes(x=individuals,y=species,color=site)) +
    geom_line() +
    geom_vline(xintercept=n) +
    scale_color_viridis_d(end=0.85) +
    theme_bw() +
    labs(color="site") +
    theme() -> p
  
  p %>%
    ggplotly() %>%
    layout(legend=list(orientation="h",xanchor="center",x=0.5,y=-0.2))
}








