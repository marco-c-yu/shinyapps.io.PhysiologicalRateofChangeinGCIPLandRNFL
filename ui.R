###########
# Package: Physiological Rate of Change in GCIPL and RNFL
################################################################################
# Copyright (C) 2023  Marco Chak-Yan YU
# 
# This program is under the terms of the GNU General Public License
# as published by the Free Software Foundation, version 2 of the License.
# 
# Redistribution, add, delete or modify are NOT ALLOWED
# WITHOUT AUTHOR'S NOTIFICATION AND PERMISSION.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
################################################################################

library(shiny)

copyright <- {c(
  "Copyright (C) 2023  Marco Chak-Yan YU",
  "",
  "This program is under the terms of the GNU General Public License",
  "as published by the Free Software Foundation, version 2 of the License.",
  "",
  "Redistribution, add, delete or modify are NOT ALLOWED",
  "WITHOUT AUTHOR'S NOTIFICATION AND PERMISSION.",
  "",
  "This program is distributed in the hope that it will be useful,",
  "but WITHOUT ANY WARRANTY; without even the implied warranty of",
  "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.",
  "See the GNU General Public License for more details."
)}

shinyUI(pageWithSidebar(
  # header
  {headerPanel('Physiological Rate of Change in GCIPL and RNFL')},
  # sidebar
  {sidebarPanel(
    titlePanel('For batch processing'),
    downloadButton('download_template', 'download input data template (.csv)'),
    br(),br(),
    fileInput('infile','upload input data (.csv):', multiple = FALSE),
    checkboxGroupInput(
      'opt','Please tick the required rate of change estimation:',
      choiceNames = c("GCIPL Average Thickness (GCavg)",
                      "Superior RNFL Thickness (RNFLs)",
                      "Inferior RNFL Thickness (RNFLi)",
                      "Between-Subjects Variation Band"),
      choiceValues = c("GCavg","RNFLs","RNFLi","band"),
      selected = c("GCavg","RNFLs","RNFLi","band"),
      width='100%'),
    numericInput("var.band","between-subjects variation band (%):",.95,min=0,max=1,step=0.01,width='250px'),
    br(),
    actionButton('run_rate','estimate rate of change'),
    br(),br(),
    downloadButton('download_output', 'download estimation output (.csv)'),
    # br(),br(),br(),
    # HTML("<b><i>Reference:</i></b>"),br(),
    # HTML("Cite doi:<a href='https://doi.org/###'>###</a>")
    br()
  )},
  # main panel
  {mainPanel(
    tabsetPanel(
      # input tab
      {tabPanel('estimation input',
                br(),
                HTML("</br><b>Input data (editable):</b></br>"),
                HTML("(<b>Double click</b> on any cell to edit; hit <b>Ctrl+Enter</b> to finish editing; hit <b>Esc</b> to cancel.)</br>"),
                HTML("(Gender can be missing for RNFLs/RNFLi rate of change estimation.)</br>"),
                actionButton('data_empty','erase all data'),
                actionButton('data_reset','reset data'),
                br(),
                DT::dataTableOutput("input_table"),
                br(),
                actionButton('add_row','add row'),
                actionButton('duplicate_row','duplicate last row'),
                actionButton('delete_row','delete row'))},
      # output tab
      {tabPanel('estimation output',
                HTML("</br><b>Output data:</b></br></br>"),
                DT::dataTableOutput("output_table"))},
      # additional tab
      {tabPanel('variable definition',
                br(),
                HTML({"<style>#def th,td{border: 1px solid; padding: 5px; vertical-align: top}</style>
                     <table id=def>
                     <tr><th style='width:120px'>Input variable</th><th>Definition</th></tr>
                     <tr><td>Age</td><td>Age (year)</td></tr>
                     <tr><td>Female</td><td>Binary indicator with 0 for Male and 1 for Female</td></tr>
                     <tr><td>SE</td><td>Spherical Equivalent (D)</td></tr>
                     <tr><td>GCavg</td><td>Cirrus HD-OCT Ganglion Cell-Inner Plexiform Layer (GCIPL) Average Thickness (&#956;m)</td></tr>
                     <tr><td>RNFLs</td><td>Cirrus HD-OCT Retinal Nerve Fibre Layer (RNFL) Thickness in the Superior quadrant (&#956;m)</td></tr>
                     <tr><td>RNFLi</td><td>Cirrus HD-OCT Retinal Nerve Fibre Layer (RNFL) Thickness in the Inferior quadrant (&#956;m)</td></tr>
                     </table>"}),
                br(),
                HTML({"<style>#def th,td{border: 1px solid; padding: 5px; vertical-align: top}</style>
                     <table id=def>
                     <tr><th style='width:120px'>Output variable</th><th>Definition</th></tr>
                     <tr><td>GCavg_rate</td><td>Expected Rate of change of Cirrus HD-OCT GCIPL Average Thickness (&#956;m/year)</td></tr>
                     <tr><td>GCavg_rate_lb</td><td>Lower Bound of Rate of change of Cirrus HD-OCT GCIPL Average Thickness (&#956;m/year)</td></tr>
                     <tr><td>GCavg_rate_up</td><td>Upper Bound of Rate of change of Cirrus HD-OCT GCIPL Average Thickness (&#956;m/year)</td></tr>
                     <tr><td>RNFLs_rate</td><td>Expected Rate of change of Cirrus HD-OCT RNFL Thickness in the Superior quadrant (&#956;m/year)</td></tr>
                     <tr><td>RNFLs_rate_lb</td><td>Lower Bound of Rate of change of Cirrus HD-OCT RNFL Thickness in the Superior quadrant (&#956;m/year)</td></tr>
                     <tr><td>RNFLs_rate_ub</td><td>Upper Bound of Rate of change of Cirrus HD-OCT RNFL Thickness in the Superior quadrant (&#956;m/year)</td></tr>
                     <tr><td>RNFLi_rate</td><td>Expected Rate of change of Cirrus HD-OCT RNFL Thickness in the Inferior quadrant (&#956;m/year)</td></tr>
                     <tr><td>RNFLi_rate_lb</td><td>Lower Bound of Rate of change of Cirrus HD-OCT RNFL Thickness in the Inferior quadrant (&#956;m/year)</td></tr>
                     <tr><td>RNFLi_rate_ub</td><td>Upper Bound of Rate of change of Cirrus HD-OCT RNFL Thickness in the Inferior quadrant (&#956;m/year)</td></tr>
                     </table>"}))},
      # help tab
      {tabPanel('help',
                HTML("<script src='/pdf.js'></script>
                     </br>User Guide (<a href='user_guide.pdf'>pdf</a>):</br>"),
                tags$iframe(style='height:600px; width:100%', src='user_guide.pdf'))}
      # # copyright tab
      # {tabPanel("Copyright", br(), HTML(paste(copyright,collapse='</br>')))}
    )
  )}
  
))
