#Created by Keith Post on 2/9/23
#Code for functions used in Species Models App for Rarefaction mini-app


#### Create Functions for Species Accumulation Models===============================================
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





