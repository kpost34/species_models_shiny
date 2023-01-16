#Created by Keith Post on 1/15/23
#Code for functions used in Species Models App for Species-Area Curves mini-app



#### Create Functions to Draw Models================================================================
### Power law on linear scale
plot_power_mod<-function(data,c,z,col){
  data %>%
    ggplot(aes(x=a)) +
    ggtitle("Power Law: Linear Scale") +
    geom_function(fun=~c*.x^z,color=col) +
    scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
    scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
    labs(x="Area",
         y="No. of Species") +
    theme_bw()
}

### Power law on log-log scale


#### Create Functions to Filter and Plot on Different Scales========================================
### Single plots
## Linear
plot_linear_sars<-function(data,col,reg=FALSE){
  data %>%
    ggplot(aes(x=a,y=s)) +
    ggtitle("Power Law: Linear") +
    geom_point(color=col) +
    labs(x="Area (hectares)",
         y="No. of species") +
    theme_bw() -> p
  
  if(reg){
    p +
      stat_smooth(method="nls",
                  formula=y~c*x^z,
                  method.args=list(start=c(c=2.5,z=.35)),
                  se=FALSE)
  }
}


## Log-log
plot_log_sars<-function(data,col,reg=FALSE){
  data %>%
    ggplot(aes(x=log10(a),y=log10(s)),) +
    ggtitle("Power Law: Log") +
    geom_point(color=col) +
    labs(x="log10(Area (hectares))",
         y="log10(No. of species)") +
    theme_bw() -> p
  
  if(reg){
    p +
      geom_smooth(method="lm")
  }
}


## Semi-log
plot_semilog_sars<-function(data,col,reg=TRUE){
  data %>%
    ggplot(aes(x=log10(a),y=s)) +
    ggtitle("Semilog Model") +
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


### Multiple plots
plot_sars_grid<-function(data,col){
  data %>%
    plot_linear_sars(col=col,reg=TRUE) -> p1a
  
  data %>%
    plot_log_sars(col=col,reg=TRUE) -> p1b
  
  data %>%
    plot_semilog_sars(col=col,reg=TRUE) -> p2
  
  plot_grid(p1a,p1b,p2,nrow=2)
  
}







