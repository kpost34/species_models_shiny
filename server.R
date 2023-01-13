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
  #resets all values in sidebar (custom UI panel)
  observeEvent(input$reset_ib, {
    reset("Sidebar")
  })
  

  # show("plot_cust_schematic_ib")
  # show("plotly_cust_rate_ib")
  # show("plotly_cust_sppt_ib")
  
  ### Toggle outputs for custom settings
  # observeEvent({
  #   if(input$chkgrp_plotoutputs_ib=="schematic"){
  #     show("plot_cust_schematic_ib")
  #     hide("plotly_cust_rate_ib")
  #     hide("plotly_cust_sppt_ib")
  #   }
  #   if(input$chkgrp_plotoutputs_ib=="rate"){
  #     show("plotly_cust_rate_ib")
  #     hide("plot_cust_schematic_ib")
  #     hide("plotly_cust_sppt_ib")
  #   }
  #   if(input$chkgrp_plotoutputs_ib=="svt"){
  #     show("plotly_cust_sppt_ib")
  #     hide("plot_cust_schematic_ib")
  #     hide("plotly_cust_rate_ib")
  #   }
  #   if(is.na(input$chkgrp_plotoutputs_ib)){
  #     hide("plotly_cust_sppt_ib")
  #     hide("plot_cust_schematic_ib")
  #     hide("plotly_cust_rate_ib")
  #   }
  # })
  
  observeEvent(input$chkgrp_plotoutputs_ib,{
    if(input$chkgrp_plotoutputs_ib=="schematic"){
      show("plot_cust_schematic_ib")
    } else{
      hide("plot_cust_schematic_ib")
    }
    if(input$chkgrp_plotoutputs_ib=="rate"){
      show("plotly_cust_rate_ib")
    } else{
      hide("plotly_cust_rate_ib")
    }
    if(input$chkgrp_plotoutputs_ib=="svt"){
      show("plotly_cust_sppt_ib")
    } else{
      hide("plotly_cust_sppt_ib")
    }
  },ignoreInit=TRUE,ignoreNULL=FALSE)

  # observeEvent(input$chkgrp_plotoutputs_ib,{
  # observe({
  #   toggle("plot_cust_schematic_ib",
  #          condition=input$chkgrp_plotoutputs_ib=="schematic")
  #   toggle("plotly_cust_rate_ib",
  #          condition=input$chkgrp_plotoutputs_ib=="rate")
  #   toggle("plotly_cust_sppt_ib",
  #          condition=input$chkgrp_plotoutputs_ib=="svt")
  #   }
  # )

  


  
  ##### Back-end------------------------------------------------------------------------------------
  #### Scenarios
  ### Set up switches for text sidebar, schematic DF, rate DF, eq rate DF, & spp v time DF to create reactive objects
  ## Text sidebar
  #display different text in sidebar when sc1-4 is selected
  sc1_4_text_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
      "lvs"=sc1_text_ib,
      "nvd"=sc2_text_ib,
      "lnvsd"=sc3_text_ib,
      "ldvsn"=sc4_text_ib)
  })
  
  
  ## Schematic DF
  #build reactive DF for schematic plot depending on sc1-4
  sc1_4_schematicDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
      "lvs"=build_schematic_df(nm=c("large","small"),a1=5000,d1=1000,sec_isle="yes",a2=1000,d2=1000),
      "nvd"=build_schematic_df(nm=c("near","distant"),a1=1000,d1=1000,sec_isle="yes",a2=1000,d2=5000),
      "lnvsd"=build_schematic_df(nm=c("large, near","small, distant"),
                                 a1=5000,d1=1000,sec_isle="yes",a2=1000,d2=5000),
      "ldvsn"=build_schematic_df(nm=c("large, distant","small, near"),
                                 a1=5000,d=5000,sec_isle="yes",a2=1000,d2=1000)
    )
  })
  
  ## Rate DF
  #build reactive DF for rate plot depending on sc1-4 
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
  #same as directly above except for equilibrium points of rate plot
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
  #build spp v time DF reactive object for sc1-4 depending on selection
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
  
  
  ## Schematic Plot
  output$plot_sc1_4_schematic_ib<-renderPlot({
    req(input$rad_scenario2_ib)
    make_island_schematic(sc1_4_schematicDF_ib(),sec_isle="yes")
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
  ### Schematic plot
  ## Create reactive object of schematic island DF
  schematicDF_ib<-reactive({
    build_schematic_df(nm=c("island 1","island 2"),a1=input$num_a1_ib,d1=input$num_d1_ib,
                       sec_isle=input$rad_is2_ib,a2=input$num_a2_ib,d2=input$num_d2_ib)
  })
  
  
  ## Render plotly plot
  output$plot_cust_schematic_ib<-renderPlot({
    make_island_schematic(schematicDF_ib(),sec_isle=input$rad_is2_ib)
  })
  
  
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
  # Create separate reactive object of rate DF (for island 2)
  rate2DF_ib<-reactive({
    build_rate_df(island="Island 2",s_len=300,c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=input$num_d2_ib, ep=input$sld_ep2_ib, a=input$num_a2_ib)
  })


  # Create reactive object of eq rate DF (for island 2)
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




