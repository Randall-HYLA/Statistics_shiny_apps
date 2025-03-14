These folders contain a collection of statistical applications built using R and Shiny. Each application demonstrates different statistical techniques (parametric & non-parametric), 
allowing users to input data, adjust parameters, and visualize results in real-time.

These Shiny applications provide an interactive way apply statistical techniques, making the folder a valuable resource for students, educators, and data analysts.

Note: These Shiny applications are regularly updated to improve functionality, add new features, and ensure compatibility with the latest statistical practices.

To run a Shiny app in R, first ensure the shiny package is installed using install.packages("shiny"), then load it with library(shiny). If the app is written as an R script (app.R), use runApp("path/to/app.R"), or if it's in a folder containing separate ui.R and server.R files, use runApp("path/to/folder"). Alternatively, you can define the UI and server components within an R script and launch the app using shinyApp(ui, server). Running these commands in the R console will start a local server, opening the app in a web browser.

To run a Shiny app in RStudio, first ensure the shiny package is installed with install.packages("shiny"), and then load it using library(shiny). If your app is in a single script (app.R), simply open the file in RStudio and click the Run App button in the upper-right corner of the script editor. If the app is split into ui.R and server.R files, make sure both are in the same folder, then click Run App from the RStudio interface. Alternatively, you can run the app directly from the console using runApp("path/to/app.R") or runApp("path/to/folder"). This will start the app in a local server and open it in a web browser.
