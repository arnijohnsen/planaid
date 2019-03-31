#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(stringr)

# Define UI for application that draws a histogram
shinyUI(material_page(style = ".btn{margin: 20px !important;}",
  title = "PlanAID",
  tags$br(), 
  material_row(
    material_column(
      width = 6, 
      material_card(
        title = "Plan for doctor", 
        actionButton("action_copy", label = "Copy to clipboard", style = "position: absolute; top: 10px; right: 10px;"),
        htmlOutput("dr_text")
      ),
      material_card(
        title = "Plan for patient", 
        actionButton("action_app", label = "Send to app", style = "position: absolute; top: 10px; right: 10px;"),
        htmlOutput("pt_text", style = "font-size:12px")
      )
    ), 
    material_column(
      width = 6, 
      material_card(
        title = "Plan creator",
        material_dropdown(
          input_id = "in_condition", label = "", choices = c(
            "Patient condition" = "u",
            "Acute kidney injury" = "aki",
            "Deep vein thrombosis" = "dvt", 
            "Postoperative bleeding" = "bleed",
            "Respiratory failure" = "resp_fail", 
            "Surgical wound infection" = "wound_inf",
            "Urinary retention" = "urinary_retention", 
            "Urinary tract infection" = "uti"
          ),
          selected = "u"
        ),
        actionButton("in_blood_0", label = "Blood test", style = "margin-bottom: 3px;"),
        uiOutput("out_blood_1"), br(),
        actionButton("in_vitals_0", label = "Vitals & Monitoring", style = "margin-bottom: 3px;"), 
        uiOutput("out_vitals_1"),
        uiOutput("out_vitals_2"), br(),
        actionButton("in_support_0", label = "Supportive therapy", style = "margin-bottom: 3px;"), 
        uiOutput("out_support_1"), br(),
        actionButton("in_medsin_0", label = "Medications (init.)", style = "margin-bottom: 3px;"), 
        uiOutput("out_medsin_1"), br(),
        actionButton("in_medsout_0", label = "Medications (dicont.)", style = "margin-bottom: 3px;"), 
        uiOutput("out_medsout_1"), br(),
        actionButton("in_consult_0", label = "Consultation", style = "margin-bottom: 3px;"), 
        uiOutput("out_consult_1"), br()
      )
    )
  )
))