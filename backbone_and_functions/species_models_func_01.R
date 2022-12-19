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
                  text=paste0("Island ",island,
                              "\n",rate,": ",value," spp/t",
                              "\n","Species: ",s)),
              color="red4") +
    scale_color_manual(values=c("1"="red4"),guide="none") +
    scale_linetype_manual(values=c("Colonization"="solid",
                                   "Extinction"="dashed")) +
    geom_point(data=data1b,
               aes(x=s_eq,y=rate_eq,
                   text=paste0("Island ",island,
                              "\n","rate*: ",rate_eq," spp/t",
                              "\n", "s*: ",s_eq, " species")),
               color="brown") +
    geom_point(data=data1b,
               aes(x=s_eq,y=0,
               text=paste0("Island ",island,
                          "\n", "s*: ",s_eq, " species")),
               size=1,color="brown") +
    geom_segment(data=data1b,
                 aes(x=s_eq,xend=s_eq,y=0,yend=rate_eq),
                 linetype="dotted",color="purple") +
    labs(x="Species richness of island",
         y="Colonization/extinction rate (spp/time)") +
    theme_bw() -> rate1_p
  
if(sec_isle=="yes") {
  rate1_p +
    geom_line(data=data2a,
              aes(x=s,y=value,group=rate,linetype=rate,color=island,
                  text=paste0("Island ",island,
                  "\n",rate,": ",value," spp/t",
                  "\n","Species: ",s))) +
    scale_color_manual(values=c("1"="red4","2"="blue4")) +
    geom_point(data=data2b,
               aes(x=s_eq,y=rate_eq,
                   text=paste0("Island ",island,
                               "\n","rate*: ",rate_eq, " spp/time",
                               "\n", "s*: ",s_eq, " species")),
               color="black") +
    geom_point(data=data2b,
               aes(x=s_eq,y=0,
                   text=paste0("Island ",island,
                               "\n", "s*: ",s_eq, " species")),
                   color="black") +
    geom_segment(data=data2b,
                 aes(x=s_eq,xend=s_eq,y=0,yend=rate_eq),
                     linetype="dotted",color="darkgreen") -> rate12_p
    } else if(sec_isle=="no") {
        rate1_p -> rate12_p
    }

rate12_p %>%
  ggplotly(tooltip="text") %>%
    layout(margin=list(b=120),
      #put legend below plot
      legend=list(orientation="h",xanchor="left",yanchor="bottom",
                  x=0,y=-0.5))

}
