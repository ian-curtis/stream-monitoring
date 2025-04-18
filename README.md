# Stream Monitoring of Local Watersheds

This project aims to provide high school instructors a resource for engaging in an activity designed by Dr. Amanda Buday, GVSU. Students in FFA classes travel to loacl watersheds and collect data about various attributes of the water (such as temperature, pH, E. Coli levels, etc.). They then enter their data into a set of Google files generated by this tool with the ultimate goal of automatically creating interpretable plots. This allows students to focus on reading charts and digging into the deeper meaning of trends rather than having to waste class time drawing or building their own charts.

The code and Shiny in this repository copies template files and guides users through setting up their own instance of a stream monitoring project. Instructor guidelines and student worksheets are still in progress and are not yet included.

You will need to register and application and generate a client ID and client secret from Google in order to make this work as it relies on `{googlesheets4}` and `{googledrive}` which utilize Google APIs.