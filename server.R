#Created by Keith Post on 12/11/22





#--------------------------------------------------------------------------------------------------#
##### Define Server Function========================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  

  ##### Island Biogeography=ib======================================================================
  #### UI--------------------------------------------------------------------------------------------
  ### Load IB mini-app with sidebar hidden
  hide("Sidebar")
  
  ### Show sidebar if custom or specific scenario is selected and hide otherwise
  observeEvent(input$rad_scenario1_ib,{
    if(input$rad_scenario1_ib=="custom"){
      show("Sidebar")
    }
  })
  
  observeEvent(input$rad_scenario2_ib,{
    show("Sidebar")
  })
  
  observeEvent(input$rad_scenario1_ib,{
    if(input$rad_scenario1_ib=="scenario"){
      hide("Sidebar")
    }
  })
  
  
  ### Dynamically display second set of radio buttons
  output$ui_rad_scenarios2_ib<-renderUI({
    req(input$rad_scenario1_ib=="scenario")
    radioButtons(inputId="rad_scenario2_ib",choices=scenarios2_ib,selected=character(0),
                 inline=TRUE,label="Select a scenario")
    })
  
  ### Dynamically display sidebar and main panel
  observeEvent(input$rad_scenario1_ib,{
    updateTabsetPanel(inputId="input_tabset_ib",selected=input$rad_scenario1_ib)
  })
  
  observeEvent(input$rad_scenario1_ib, {
    updateTabsetPanel(inputId="out_tabset_ib",selected=input$rad_scenario1_ib)
  })
  
  
  ### Reset button
  observeEvent(input$reset_ib, {
    reset("Sidebar")
  })
  
  ##### Back-end------------------------------------------------------------------------------------
  #### Scenarios
  ### Set up switches for text sidebar, rate DF, eq rate DF, & spp v time DF to create reactive objects
  ## Text sidebar
  sc1_4_text_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
      "lvs"=sc1_text_ib,
      "nvd"=sc2_text_ib,
      "lnvsd"=sc3_text_ib,
      "ldvsn"=sc4_text_ib)
  })
  
  
  ## Rate DF
  sc1_4_rateDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
           "lvs"=sc1_rateDF_ib,
           "nvd"=sc2_rateDF_ib,
           "lnvsd"=sc3_rateDF_ib,
           "ldvsn"=sc4_rateDF_ib)
  })
  
  
  ## Eq Rate DF
  sc1_4_rate_eqDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
           "lvs"=sc1_rate_eqDF_ib,
           "nvd"=sc2_rate_eqDF_ib,
           "lnvsd"=sc3_rate_eqDF_ib,
           "ldvsn"=sc4_rate_eqDF_ib)
  })
  
  
  ## Spp vs Time DF
  sc1_4_spptDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
           "lvs"=sc1_spptDF_ib,
           "nvd"=sc2_spptDF_ib,
           "lnvsd"=sc3_spptDF_ib,
           "ldvsn"=sc4_spptDF_ib)
  })
  
  
  ### Render functions to output text and plots
  ## Text
  output$text_sc1_4_text_ib<-renderText({
    sc1_4_text_ib()
  })
  
  ## Rate Plotly Plot
  output$plotly_sc1_4_rate_ib<-renderPlotly({
    req(input$rad_scenario2_ib)
    build_rate_static_plot(rate_data=sc1_4_rateDF_ib(),eq_data=sc1_4_rate_eqDF_ib())
  })
  
  
  ## Spp v Time Plotly Plot
  output$plotly_sc1_4_sppt_ib<-renderPlotly({
    req(input$rad_scenario2_ib)
    build_svt_static_plot(data=sc1_4_spptDF_ib())
  })
  


  #### Custom specifications
  ### Rate plot
  ## Island 1
  # Create reactive object of rate DF
  rate1DF_ib<-reactive({
    build_rate_df(island="Island 1",s_len=300,c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                  d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })


  # Create reactive object of eq rate DF
  rate_eq1DF_ib<-reactive({
    build_eq_df(island="Island 1",c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })



  ## Island 2
  # Create reactive object of rate DF
  rate2DF_ib<-reactive({
    build_rate_df(island="Island 2",s_len=300,c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })


  # Create reactive object of eq rate DF
  rate_eq2DF_ib<-reactive({
    build_eq_df(island="Island 2",c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })



  ## Render plotly plot
  output$plotly_cust_rate_ib<-renderPlotly({
    build_rate_plot(data1a=rate1DF_ib(),data1b=rate_eq1DF_ib(),sec_isle=input$rad_is2_ib,
                    data2a=rate2DF_ib(),data2b=rate_eq2DF_ib())
  })



  ### Species over time plot
  ## Island 1
  # Reactive object
  spp1tDF_ib<-reactive({
    build_svt_df(isle=1, t=input$sld_t_ib, c=input$sld_c1_ib, p=input$sld_p_ib,
                 phi=input$sld_phi1_ib, d=input$num_d1_ib, ep=input$sld_ep1_ib, a=input$num_a1_ib)
  })



  ## Island 2
  # Reactive object
  spp2tDF_ib<-reactive({
    build_svt_df(isle=2,t=input$sld_t_ib, c=input$sld_c2_ib, p=input$sld_p_ib,
                 phi=input$sld_phi2_ib, d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })


  ## Render plotly plot
  output$plotly_cust_sppt_ib<-renderPlotly({
    build_svt_plot(data1=spp1tDF_ib(), sec_isle=input$rad_is2_ib, data2=spp2tDF_ib())
  })
  
  

  
  
  
  ##### Create second navbarMenu (Species-Area Curves=sa)===========================================

  
  ##### Create third navbarMenu (Rarefaction=rf)====================================================
        
               
               
               



}




