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
    tibble(s=seq(0,input$sld_p1_ib,length.out=100),
           C=input$sld_c1_ib*(input$sld_p1_ib-s)*exp(-input$num_phi1_ib*input$num_d1_ib),
           E=s*exp(-input$num_ep1_ib*input$num_a1_ib)) %>%
      mutate(across(C:E,~signif(.x,3)))
  })
  
  
  ## Render plots 
  output$plotly_rate_ib<-renderPlotly({
    rateDF_ib() %>%
      ggplot(aes(x=s,y=C)) +
      geom_line(color="darkblue") +
      geom_line(aes(y=E),color="darkred") +
      labs(x="Species",y="Colonization rate") +
      theme_bw() 
  })
  # plotly_spp_ib<-plotlyOutput({
  #   
  # })
  #   ),
  

  
  
  ##### Create second navbarMenu (Species-Area Curves=sa)===========================================

  
  ##### Create third navbarMenu (Rarefaction=rf)====================================================
        
               
               
               



}




