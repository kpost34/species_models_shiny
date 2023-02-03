#Created by Keith Post on 1/15/23
#Code for functions used in Species Models App for Species-Area Curves mini-app



#### Create Functions to Plot SARS Models===========================================================
### Power law on linear scale
plot_power_mod<-function(data,c,z,col){
  data %>%
    ggplot(aes(x=a)) +
    ggtitle("Power Law: Linear Scale") +
    geom_function(fun=~c*.x^z,color=col,linewidth=1.5) +
    scale_x_continuous(expand=c(0,0),limits=c(0,NA)) +
    scale_y_continuous(expand=c(0,0),limits=c(0,NA)) +
    labs(x="Area",
         y="No. of Species") +
    theme_bw() +
    theme(plot.title=element_text(size=18,face="bold"),
          axis.title=element_text(size=15,face="bold"),
          axis.text=element_text(size=12),
          text=element_text(family="Helvetica"),
          plot.margin=margin(0,20,0,0)) 
}


### Power law on log-log scale
plot_powerlog_mod<-function(data,c,z,col){
  data %>%
  ggplot(aes(x=log_a)) +
  ggtitle("Power Law: Log-log scale") +
  geom_function(fun = ~log10(c) + z*.x,color=col,linewidth=1.5) +
  labs(x="log10(Area)",
       y="log10(No. of Species)") +
  theme_bw() +
  theme(plot.title=element_text(size=18,face="bold"),
        axis.title=element_text(size=15,face="bold"),
        axis.text=element_text(size=12),
        text=element_text(family="Helvetica")) 
}


### Semilog model
plot_semilog_mod<-function(data,c,z,col){
  data %>%
    ggplot(aes(x=log_a)) +
    ggtitle("Semi-log Model") +
    geom_function(fun=~log10(c) + z*.x,color=col,linewidth=1.5) +
    labs(x="log10(Area)",
         y="No. of Species") +
    theme_bw() +
    theme(plot.title=element_text(size=18,face="bold"),
          axis.title=element_text(size=15,face="bold"),
          axis.text=element_text(size=12),
          text=element_text(family="Helvetica")) 
}


#### Create Functions to Plot SARS Data=============================================================
### Single plots
## Linear
plot_power_sars<-function(data,col="black",reg=FALSE,mod,col_reg=NA){
  data %>%
    {if(reg==TRUE) 
    mutate(.,lwr=confint2(mod)[1,1]*a^confint2(mod)[2,1],
           upr=confint2(mod)[1,2]*a^confint2(mod)[2,2])
    else .} %>%
    ggplot(aes(x=a,y=s)) +
    ggtitle("Power Law: Linear Scale") +
    geom_point(color="black") +
    labs(x="Area",
         y="No. of species") +
    theme_bw() +
    theme(plot.title=element_text(size=18,face="bold"),
          axis.title=element_text(size=15,face="bold"),
          axis.text=element_text(size=12),
          text=element_text(family="Helvetica")) -> p

  
  if(reg){
    p +
      #model
      geom_function(fun=~coef(mod)[1]*.x^coef(mod)[2],linewidth=1.2,color=col_reg) +
      #CIs
      geom_ribbon(aes(ymin=lwr,ymax=upr),fill="gray50",alpha=0.2) -> p
  }
  return(p)
}


## Log-log
plot_powerlog_sars<-function(data,col="black",reg=FALSE,col_reg=NA){
  data %>%
    ggplot(aes(x=log10(a),y=log10(s),
               text=paste("log a =",round(a,2),
                          "\nlog s = ",round(s,2)))) +
    ggtitle("Power Law: Log-log scale") +
    geom_point(color=col) +
    labs(x="log10(Area)",
         y="log10(No. of species)") +
    theme_bw() +
    theme(plot.title=element_text(size=18,face="bold"),
          axis.title=element_text(size=15,face="bold"),
          axis.text=element_text(size=12),
          text=element_text(family="Helvetica")) -> p
  
  if(reg){
    p +
      geom_smooth(method="lm",color=col_reg) -> p
  }
  return(p)
}


## Semi-log
plot_semilog_sars<-function(data,col="black",reg=TRUE,col_reg=NA){
  data %>%
    ggplot(aes(x=log10(a),y=s)) +
    ggtitle("Semi-log Model") +
    geom_point(color=col) +
    labs(x="log10(Area)",
         y="No. of species") +
    theme_bw() +
    theme(plot.title=element_text(size=18,face="bold"),
          axis.title=element_text(size=15,face="bold"),
          axis.text=element_text(size=12),
          text=element_text(family="Helvetica")) -> p
  
  if(reg){
    p +
      geom_smooth(method="lm",color=col_reg) -> p
  }
  return(p)
}


### Multiple plots
plot_sars_grid<-function(data,mod,col_reg,reg=TRUE){
  data %>%
    plot_power_sars(reg=reg,mod=mod,col_reg=col_reg) -> p1a
  
  data %>%
    plot_powerlog_sars(reg=reg,col_reg=col_reg) -> p1b
  
  data %>%
    plot_semilog_sars(reg=reg,col_reg=col_reg) -> p2
  
  plot_grid(p1a,p1b,p2,nrow=2)
  
}


#### Create Function to Create Models of SARS Data===================================================
compare_sars_mods<-function(mod1,mod2,nm){
  list(mod1,mod2) %>%
    set_names(nm) %>%
    purrr::imap(function(x,y){
      x %>%
        glance() %>%
        select(statistic,p.value,r.squared,rse=sigma,AIC) %>%
        mutate(model=y,.before="statistic")
    }) %>%
    bind_rows() %>%
    mutate(across(!model,~signif(.x,3)))
}














