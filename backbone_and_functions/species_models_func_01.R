#Created by Keith Post on 12/18/22
#Code for functions used in Species Models App


#### Create Function to Build DF of s, C, and E=====================================================
build_rate_df<-function(island,s_len,c,p,phi,d,ep,a){
  tibble(island=rep(paste(island),s_len),
         s=seq(0,p,length.out=s_len),
         Colonization=c*(p-s)*exp(-phi*d),
         Extinction=s*exp(-ep*a)) %>%
    pivot_longer(Colonization:last_col(),names_to="rate",values_to="value") %>%
    mutate(across(c(s,value),~signif(.x,3))) 
}


#### Create Function to Build DF of s* and rate*====================================================
build_eq_df<-function(island,c,p,ep,a,phi,d){
  tibble(island=paste(island),
               s_eq=(c*p*exp(ep*a))/(c*exp(ep*a) + exp(phi*d)),
               rate_eq=s_eq*exp(-ep*a)) %>%
  mutate(across(!island,~signif(.x,3)))
}


#### Create Function to Build Rate Plot=============================================================
build_rate_plot<-function(data1a,data1b,sec_isle="no",data2a=NA,data2b=NA){
  data1a %>%
    ggplot(aes(x=s,y=value)) +
    geom_line(aes(group=rate,linetype=rate,color=island,
                  text=paste0(island,
                              "\n",rate,": ",value," spp/t",
                              "\n","Species: ",s)),
              color="red4") +
    scale_color_manual(name=NULL,values=c("1"="red4"),guide="none") +
    scale_linetype_manual(name=NULL,values=c("Colonization"="solid",
                                   "Extinction"="dashed")) +
    geom_point(data=data1b,
               aes(x=s_eq,y=rate_eq,
                   text=paste0(island,
                              "\n","rate*: ",rate_eq," spp/t",
                              "\n", "s*: ",s_eq, " species")),
               color="brown") +
    geom_point(data=data1b,
               aes(x=s_eq,y=0,
               text=paste0(island,
                          "\n", "s*: ",s_eq, " species")),
               size=1,color="brown") +
    geom_segment(data=data1b,
                 aes(x=s_eq,xend=s_eq,y=0,yend=rate_eq),
                 linetype="dotted",color="purple") +
    labs(x="Species richness of island",
         y="Colonization/extinction rate (spp/time)") +
    theme_bw() +
    theme(legend.position="bottom") -> rate1_p
  
if(sec_isle=="yes") {
  rate1_p +
    geom_line(data=data2a,
              aes(x=s,y=value,group=rate,linetype=rate,color=island,
                  text=paste0(island,
                  "\n",rate,": ",value," spp/t",
                  "\n","Species: ",s))) +
    scale_color_manual(name=NULL,values=c("1"="red4","2"="blue4")) +
    geom_point(data=data2b,
               aes(x=s_eq,y=rate_eq,
                   text=paste0(island,
                               "\n","rate*: ",rate_eq, " spp/t",
                               "\n", "s*: ",s_eq, " species")),
               color="black") +
    geom_point(data=data2b,
               aes(x=s_eq,y=0,
                   text=paste0(island,
                               "\n", "s*: ",s_eq, " species")),
                   size=1,color="black") +
    geom_segment(data=data2b,
                 aes(x=s_eq,xend=s_eq,y=0,yend=rate_eq),
                     linetype="dotted",color="darkgreen") -> rate12_p
    } else if(sec_isle=="no") {
        rate1_p -> rate12_p
    }

rate12_p %>%
  ggplotly(tooltip="text") %>%
    layout(margin=list(b=110),
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.35))

}



#### Create Function to Build Spp v Time DF=========================================================
build_svt_df<-function(isle, t, c, p, phi, d, ep, a) {
  s<-c(0,rep(NA,t-1))
    
  for(i in 1:t){
    s[i+1]<-s[i] + c*(p-s[i])*
    exp(-phi*d)-s[i]*exp(-ep*a)
  }
     
  tibble(island=rep(paste("Island",isle),t+1),
         t=0:t,
         s=signif(s,3))
}



#### Create Function to Build Spp v Time Plot=======================================================
build_svt_plot<-function(data1,sec_isle,data2){
  data1 %>%
    {if(sec_isle=="yes") bind_rows(.,data2) else .} %>%
    ggplot() +
    geom_point(aes(x=t,y=s,shape=island,color=island,
                   text=paste0(island,
                               "\nSpecies: ",s,
                               "\nTime: ",t))) +
    scale_color_manual(name=NULL,values=c("Island 1"="red4","Island 2"="blue4")) +
    scale_shape_manual(name=NULL,values=c("Island 1"=16,"Island 2"=18)) +
    labs(x="Time",
         y="Species richness of island") +
    theme_bw() +
    theme(legend.position="bottom")-> p
    
  p %>%
    ggplotly(tooltip="text") %>%
    layout(margin=list(b=110),
      #put legend below plot
      legend=list(orientation="h",xanchor="center",yanchor="bottom",
                  x=0.5,y=-0.35))
}












