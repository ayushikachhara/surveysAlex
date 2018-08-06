library(shiny)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(grDevices)

pm <- c(10,8.4,14,22.4,13.8,18,11)
days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

pm.df <- data.table(days = days, pm = pm)

p <- plot_ly(
  x = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
  y = c(10,8.4,14,22.4,13.8,18,11),
  name = "What was air quality like in Alexandra last week?",
  type = "bar", color = I("royalblue")
) %>% 
  layout(title = "What was air quality like in Alexandra last week?",
             xaxis = list(title = "Day of the week"),
             yaxis = list(title = "Particles in the air"),
         paper_bgcolor='white',
         plot_bgcolor='honeydew',
         font = list(
           family = "Agency FB",
           size = 20),
         margin = 12)

p
