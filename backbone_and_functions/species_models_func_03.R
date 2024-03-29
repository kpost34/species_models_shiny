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
      labs(x=paste("Number of",nm),
           y="Richness") +
      theme_bw() -> p
    
    p %>%
      ggplotly() %>%
      layout(font=list(family="Arial"))
}


#### Functions for Rarefaction======================================================================
### Plots rarecurve as a ggplotly 
plotly_rarefy<-function(data,n){
  data %>%
    ggplot(aes(x=individuals,y=species,color=site)) +
    ggtitle("Rarefaction Curve") +
    geom_line() +
    geom_vline(xintercept=n) +
    scale_color_viridis_d(end=0.85) +
    theme_bw() +
    labs(x="Number of individuals",
         y="Richness",
         color="site") -> p
  
  p %>%
    ggplotly() %>%
    layout(legend=list(orientation="h",xanchor="center",x=0.5,y=-0.2),
           font=list(family="Arial"))
}


### Find middle default value of a slider given a max value and step
find_value<-function(min,max,step){
  half_total <- (min + max)/2
  half_total - (half_total %% step)
}


### Wrangle intersection values of rarecurve into table
find_rarefac_intersect<-function(data, n){
  data %>%
    filter(individuals==n) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    row_to_names(1) 
}



