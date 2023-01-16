#Created by Keith Post on 1/15/23
#Code for functions used in Species Models App for Species-Area Curves mini-app


#### Create Functions to Filter and Plot on Different Scales========================================
### Linear
plot_linear_sars<-function(data,col,reg=FALSE){
  data %>%
    ggplot() +
    geom_point(aes(x=a,y=s),color=col) +
    labs(x="Area (hectares)",
         y="No. of species") +
    theme_bw() -> p
  
  if(reg){
    p +
      geom_smooth(aes(x=log10(a),y=log10(s)),method="lm")
  }
}


### Log-log
plot_log_sars<-function(data,col,reg=FALSE){
  data %>%
    ggplot(aes(x=log10(a),y=log10(s)),) +
    geom_point(color=col) +
    labs(x="log10(Area (hectares))",
         y="log10(No. of species)") +
    theme_bw() -> p
  
  if(reg){
    p +
      geom_smooth(method="lm")
  }
}


### Semi-log
plot_semilog_sars<-function(data,col,reg=TRUE){
  data %>%
    ggplot(aes(x=log10(a),y=s)) +
    geom_point(color=col) +
    labs(x="log10(Area (hectares))",
         y="No. of species") +
    theme_bw()  -> p
  
  if(reg){
    p +
      # geom_smooth(aes(x=log10(a),y=s),method="lm")
      geom_smooth(method="lm")
  }
}



