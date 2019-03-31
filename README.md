# PlanAID

PlanAID is an in-hospital software made to help doctors make plans for admitted patients  and at the same time sharing it with them. This early version of PlanAID was created at the Nordic Health Hackathon 2019 in Helsinki. 

# Current features

The current version of PlanAID includes a working demo of the doctors UI, which creates both a concise plan for the doctors and a clear summary for the patient. The "Plan for doctor" field has a button to copy its content to the users clipboard. The "Plan for patient" field has a button to send data to a smartphone app, which currently saves the text summary to an SQL database. 

# Installation

1. Install R with the following packages: shiny, shinymaterial, stringr, RSQLite, DBI.
2. Install SQLite
3. Clone this repository
4. Run `shiny::runApp("planaid")`

# About the authors

PlanAID was created by Árni Johnsen and Kjartan Þórsson, both medical students at the University of Iceland. 
