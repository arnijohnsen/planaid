#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(stringr)
library(RSQLite)
library(DBI)

db <- dbConnect(SQLite(), dbname="saved_plans.sqlite")

render_blood_1 <- function(input){
  if(input %% 2 == 0){
    return()
  } else  {
    list(
      actionButton("in_blood_1_today", label = "Today", style = "margin-bottom: 3px;"),
      actionButton("in_blood_1_tomorrow", label = "Tomorrow", style = "margin-bottom: 3px;")
    )
  }
}
render_vitals_1 <- function(input){
  if(input %% 2 == 0){
    return()
  } else  {
    list(
      actionButton("in_vitals_1_vitals", label = "Vitals", style = "margin-bottom: 3px;"),
      actionButton("in_vitals_1_fluid", label = "Fluid balance", style = "margin-bottom: 3px;"), 
      actionButton("in_vitals_1_urine", label = "Urine output", style = "margin-bottom: 3px;"), 
      actionButton("in_vitals_1_weight", label = "Weight", style = "margin-bottom: 3px;")
    )
  }
}
render_vitals_2 <- function(input){
  if(length(input) == 0){
    return()
  }
  if(input %% 2 == 0){
    return()
  } else  {
    list(
      material_slider("in_vitals_2_vitals_time", label = "", initial_value = 4, min = 1, max = 24, color = "#E84033")
    )
  }
}
render_support_1 <- function(input){
  if(input %% 2 == 0){
    return()
  } else  {
    list(
      actionButton("in_support_1_fluid", label = "IV fluid", style = "margin-bottom: 3px;"),
      actionButton("in_support_1_oxygen", label = "Oxygen", style = "margin-bottom: 3px;")
    )
  }
}
render_medsin_1 <- function(input){
  if(input %% 2 == 0){
    return()
  } else  {
    list(
      actionButton("in_medsin_1_calcium", label = "Calcium", style = "margin-bottom: 3px;"),
      actionButton("in_medsin_1_salbutamol", label = "Salbutamol", style = "margin-bottom: 3px;"),
      actionButton("in_medsin_1_insulinglucose", label = "Insulin&Glucose", style = "margin-bottom: 3px;"),
      actionButton("in_medsin_1_resonium", label = "Resonium", style = "margin-bottom: 3px;"),
      actionButton("in_medsin_1_furosemide", label = "Furosemide", style = "margin-bottom: 3px;")
    )
  }
}
render_medsout_1 <- function(input){
  if(input %% 2 == 0){
    return()
  } else  {
    list(
      actionButton("in_medsout_1_enalapril", label = "Enalapril", style = "margin-bottom: 3px;")
    )
  }
}
render_consult_1 <- function(input){
  if(input %% 2 == 0){
    return()
  } else  {
    list(
      actionButton("in_consult_1_nephrology", label = "Nephrology", style = "margin-bottom: 3px;")
    )
  }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$out_blood_1   <- renderUI({render_blood_1(  input$in_blood_0  )})
  output$out_vitals_1  <- renderUI({render_vitals_1( input$in_vitals_0 )})
  output$out_vitals_2  <- renderUI({render_vitals_2( input$in_vitals_1_vitals)})
  
  output$out_support_1 <- renderUI({render_support_1(input$in_support_0)})
  output$out_medsin_1  <- renderUI({render_medsin_1( input$in_medsin_0 )})
  output$out_medsout_1 <- renderUI({render_medsout_1(input$in_medsout_0)})
  output$out_consult_1 <- renderUI({render_consult_1(input$in_consult_0)})
  
  
  dr_text_unrendered <- reactiveValues()
  pt_text_unrendered <- reactiveValues()
  
  # Condition event
  observeEvent({input$in_condition}, {
    if(input$in_condition == "aki"){
      pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>You are suffering from <b>Acute kidney injury (AKI)</b>. AKI is a condition where the amount of blood filtered by the kidneys is reduced for a short period of time and can result in waste products, electrolytes and fluids accumulating in your body."))
    } 
    if(input$in_condition == "dvt"){
      pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>You are suffering from <b>Deep veins thrombosis (DVT)</b>. DVT is a blood clot that develops within a deep vein in the body, usually in the leg."))
    } 
    if(input$in_condition == "bleed"){
      pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>You are suffering from a <b>Post-operative bleeding</b> from the surgical site."))
    } 
    if(input$in_condition == "resp_fail"){
      pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>You are suffering from <b>Respiratory failure</b>. Respiratory failure is a condition where the lungs don't recive enough air to fully oxygenate the blood. "))
    } 
    if(input$in_condition == "urinary_retention"){
      pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>You are suffering from <b>Urinary retention</b>. Urinary retention is a condition where your bladder cannot be fully expressed without external assistance."))
    } 
    if(input$in_condition == "uti"){
      pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>You are suffering from a <b>Urinary tract infection (UTI)</b>. UTIs are infections of the bladder, urethrea or kidneys. "))
    } 
    if(input$in_condition == "wound_inf"){
      pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>You are suffering from a <b>Surgical wound infection</b>. Surgical wound infections are infections contracted during or shortly after surgery and can affect any part of the surgical site.  "))
    } 
    
  })
  
  # Blood events
  observeEvent({input$in_blood_1_today}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>New blood tests today"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>Blood tests will be drawn later today to monitor your metabolic state so we can arrange your treatment according to the results."))
  })
  observeEvent({input$in_blood_1_tomorrow}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>New blood tests tomorrow morning"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>Blood tests will be drawn tomorrow morning to monitor your metabolic state so we can arrange your treatment according to the results."))
  })
  
  # Vitals events
  observeEvent({input$in_vitals_2_vitals_time}, {
    add_str_dr <- isolate(str_c("<br>Monitor vitals every ", input$in_vitals_2_vitals_time, " hours"))
    add_str_pt <- isolate(str_c("<br><br>We will monitor your vitals today so we can see how your body is reacting to the stress following your illness and so we can make appropriate preventions and provide you with the right support your body needs to maintain a functioning activity."))
    if(length(dr_text_unrendered$dList) == 0){
      dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), add_str_dr)
      pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), add_str_pt)
    } else {
      if(grepl("Monitor vitals every", tail(dr_text_unrendered$dList, 1))){
        dr_text_unrendered$dList <- c(isolate(head(dr_text_unrendered$dList, -1)), add_str_dr)
        pt_text_unrendered$dList <- c(isolate(head(pt_text_unrendered$dList, -1)), add_str_pt)
      } else {
        dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), add_str_dr)
        pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), add_str_pt)
      }
    }
  })
  observeEvent({input$in_vitals_1_fluid}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>Monitor fluid balance"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>We will monitor your fluid balance so we can see the status of your blood level and the functioning of your kidneys and heart."))
  })
  observeEvent({input$in_vitals_1_urine}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>Monitor urine output"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>We will monitor your urine output so we can see if your kidneys are adiquetly perfused and outputting urine."))
  })
  observeEvent({input$in_vitals_1_weight}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>Monitor fluid balance"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>We will monitor your weight so we can predict if we are administering to much fluids or if your body fluids are accumulating causing oedema or other ways fluids can remain in your body."))
  })
  # Supportive therapy events
  observeEvent({input$in_support_1_fluid}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>IV fluids"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>In order to reverse or prevent dehydration and maintaining a sufficient blood pressure we will give you fluids through a venous access. This means that we will have to maintain a venous access while you are receiving fluids."))
  })
  observeEvent({input$in_support_1_oxygen}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>Oxygen suppliment"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>In order to provide all the organs and tissues in the body with enough oxygen we will administer supplemental oxygen for you to breath in. Sometimes it’s necessary to use face mask so the oxygen gets efficiently down to your lungs."))
  })
  # Medications in events
  observeEvent({input$in_medsin_0}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>Add the following medications to treatment:"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>You will need to take the following medications due to your illness:"))
  })
  observeEvent({input$in_medsin_1_calcium}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>- Calcium gluconate"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><b>Calcium gluconate</b>, helps the heart maintaining a normal and functioning heartbeat if there are electrolyte disturbances occurring."))
  })
  observeEvent({input$in_medsin_1_salbutamol}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>- Salbutamol"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><b>Salbutamol</b>, increases the activity of receptors found in smooth and striated muscles and causes potassium to enter the cells and reducing the amount in the blood."))
  })
  observeEvent({input$in_medsin_1_insulinglucose}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>- Insulin and glucose"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><b>Insulin and glucose</b>, helps muscles and tissues taking up glucose from the blood, but additionally causes potassium to enter the cells and reducion the amount in the blood. Glucose is administered to prevent hypoglycemia due to insulin administration."))
  })
  observeEvent({input$in_medsin_1_resonium}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>- Resonium"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><b>Resonium</b>, can help the body getting rid of electrolytes like potassium through intestines."))
  })
  observeEvent({input$in_medsin_1_furosemide}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>- Furosemide"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><b>Furosemide</b>, is a diuretic helping the body get rid of excess fluids and potassium through the kidneys."))
  })
  # Medications out events
  observeEvent({input$in_medsout_0}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>Remove the following medications from treatment:"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>We have to stop the following medications due to your illness:"))
  })
  observeEvent({input$in_medsout_1_enalapril}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>- Enalapril"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><b>Enalpril</b>, used to lower blood pressure by prohibiting the production of angiotensin which is a hormone which causes vasoconstriction and increases blood pressure. A side effect is the loss of blood flow through the kidneys so in the case of an acute kidney injury the administration of Enalpril should be avoided."))
  })
  # Consultation events
  observeEvent({input$in_consult_1_nephrology}, {
    dr_text_unrendered$dList <- c(isolate(dr_text_unrendered$dList), isolate("<br>Consult with nephrology"))
    pt_text_unrendered$dList <- c(isolate(pt_text_unrendered$dList), isolate("<br><br>We will contact a nephrologist due to your illness to aquire his/hers expert opinion on your treatment."))
  })
  
  # Render text
  output$dr_text <- renderText({
    if(length(dr_text_unrendered$dList > 0)){
      dr_text_unrendered$dList[1] <- str_remove(dr_text_unrendered$dList[1], "<br>")
    }
    dr_text_unrendered$dList
  })
  output$pt_text <- renderText({
    if(length(pt_text_unrendered$dList > 0)){
      pt_text_unrendered$dList[1] <- str_remove(pt_text_unrendered$dList[1], "<br><br>")
    }
    pt_text_unrendered$dList
  })
  
  # Save to database
  observeEvent({input$action_app}, {
    plan_to_write <- data.frame(
      user = "patient", 
      text = str_c(isolate(pt_text_unrendered$dList), collapse = "")
    )
    dbWriteTable(db, "PLANS", plan_to_write, append = T, row.names = F)
    dbDisconnect(db)
  })
})
