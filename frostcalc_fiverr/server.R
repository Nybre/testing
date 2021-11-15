options(digits.secs = 3) # Include milliseconds in time display

shinyServer(function(input, output,session) {
  
  #----------------server modules------------------
  callModule(Dashboard_file, "Dashboard_file-module", pool) 
  
})