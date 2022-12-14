#Created by Keith Post on 12/11/22







#--------------------------------------------------------------------------------------------------#
##### Define Server Function========================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  

  ##### Island Biogeography=ib)=====================================================================
  #### UI--------------------------------------------------------------------------------------------
  
  
  #### Back-end--------------------------------------------------------------------------------------
  ### Rate plot
  ## Create reactive object
  rateDF_ib<-reactive({
    tibble(s=seq(0,input$sld_p1_ib,length.out=150),
           C=input$sld_c1_ib*(input$sld_p1_ib-s)*exp(-input$num_phi1_ib*input$num_d1_ib),
           E=s*exp(-input$num_ep1_ib*input$num_a1_ib),
           diff=abs(C-E)) %>%
      mutate(across(everything(),~round(.x,2)),
             equal=diff==min(diff))
  })
  
  
  ## Render plots 
  output$plotly_rate_ib<-renderPlotly({
    rateDF_ib() %>%
      ggplot(aes(x=s,y=C)) +
      geom_line(color="darkblue") +
      geom_line(aes(y=E),color="darkred") +
      geom_segment(data=~filter(.x,equal),
                 aes(x=s,xend=s,y=0,yend=E),lineend="butt",color="purple") +
      labs(x="Species richness of island (s)",y="Colonization rate (species/time)") +
      theme_bw() -> p_rate_ib
    
    p_rate_ib %>%
      ggplotly()
  })
  # plotly_spp_ib<-plotlyOutput({
  #   
  # })
  #   ),
  

  
  
  ##### Create second navbarMenu (Species-Area Curves=sa)===========================================

  
  ##### Create third navbarMenu (Rarefaction=rf)====================================================
        
               
               
               



}




