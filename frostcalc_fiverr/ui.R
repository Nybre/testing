shinyUI(
  dashboardPage( 
    dashboard_header(show_menu_button = T  ,titleWidth = "wide",logo_align = "center" , inverted = F,
                     logo_path = "images/frost.jpg",
                     title= "Frost Calculator Tool"),  
    dashboardSidebar(side = "left",disable = TRUE,
                     sidebarMenu( 
                       menuItem("Dashboard", tabName = "dash", icon = icon("info",lib = "font-awesome")) 
                     )),  
    dashboardBody(    
      tabItems(   
        tabItem(tabName = "dash",   
                Dashboard_file_UI("Dashboard_file-module")))
    ) 
  ))
