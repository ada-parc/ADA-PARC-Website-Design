# national_ui.R
sidebarLayout(
  sidebarPanel(
    h3("National Data Sidebar Panel"),
    p("This is the sidebar panel content for the National Data tab.")
  ),
  mainPanel(
    h3("Testing testing"),
    plotOutput("us_map"),
    p("Big test here")
  )
)
