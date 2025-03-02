# Biblioteca necesara
library(shiny)
library(ggplot2)

# Interfata utilizatorului (UI)
ui <- fluidPage(
  titlePanel("Aplicație combinată - Distribuții și Funcții Personalizate"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("mod", "Selectați modul:", 
                  choices = c("Distribuții variabile aleatoare", 
                              "Funcții personalizate")),
      
      # Panou pentru distribuții
      conditionalPanel(
        condition = "input.mod == 'Distribuții variabile aleatoare'",
        selectInput(
          inputId = "distribution",
          label = "Selectați distribuția:",
          choices = c("Normală N(0,1)", "Normală N(μ,σ^2)", 
                      "Exponențială Exp(λ)", "Poisson Pois(λ)", 
                      "Binomială Binom(r, p)"),
          selected = "Normală N(0,1)"
        ),
        numericInput("n", "n (numărul de observații):", 100, min = 1),
        selectInput(
          inputId = "variable",
          label = "Selectați variabila:",
          choices = c("X", "3 - 2X", "2 + 5X", "3X - 2", "5X - 4", 
                      "X^2", "X^3", "Suma (ΣX)", "Suma pătratelor (ΣX^2)"),
          selected = "X"
        ),
        conditionalPanel(
          condition = "input.distribution == 'Normală N(μ,σ^2)'",
          numericInput("mu", "μ (media):", 0),
          numericInput("sigma", "σ (deviația standard):", 1, min = 0.1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Exponențială Exp(λ)'",
          numericInput("lambda", "λ (rata):", 1, min = 0.1)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Poisson Pois(λ)'",
          numericInput("lambda_pois", "λ (media):", 1, min = 0)
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomială Binom(r, p)'",
          numericInput("r", "r (număr de experimente):", 10, min = 1),
          numericInput("p", "p (probabilitatea de succes):", 0.5, min = 0, max = 1)
        )
      ),
      
      # Panou pentru funcții personalizate
      conditionalPanel(
        condition = "input.mod == 'Funcții personalizate'",
        selectInput("distributie", "Alege funcția personalizată:", 
                    choices = c("Funcție personalizată A", 
                                "Funcție personalizată B", 
                                "Funcție personalizată C", 
                                "Funcție personalizată D", 
                                "Funcție personalizată E", 
                                "Funcție personalizata F", 
                                "Funcție personalizata G")),
        
        conditionalPanel(
          condition = "input.distributie == 'Funcție personalizată A'",
          numericInput("c", "Coeficient c:", value = 1, min = 0),
          sliderInput("interval_a", "Interval [a, b]", min = 0, max = 10, value = c(0, 2))
        ),
        conditionalPanel(
          condition = "input.distributie == 'Funcție personalizată B'",
          numericInput("a", "Coeficient a:", value = 1),
          numericInput("b", "Coeficient b:", value = 1),
          sliderInput("interval_b", "Interval [a, b]", min = 0, max = 10, value = c(0, 1))
        ),
        conditionalPanel(
          condition = "input.distributie == 'Funcție personalizată C'",
          numericInput("x_c_min", "x (minim):", value = 1, min = 1),
          numericInput("x_c_max", "x (maxim):", value = 5, min = 1)
        ),
        conditionalPanel(
          condition = "input.distributie == 'Funcție personalizată D'",
          checkboxGroupInput("x_d_values", "Alege valorile discrete pentru x:",
                             choices = 1:9, selected = 1:9)
        ),
        conditionalPanel(
          condition = "input.distributie == 'Funcție personalizată E'",
          numericInput("theta", "Parametru θ:", value = 1, min = 0.1),
          sliderInput("interval_e", "Interval [a, b]:", min = 0, max = 10, value = c(0, 5))
        ),
        conditionalPanel(
          condition = "input.distributie == 'Funcție personalizata F'",
          sliderInput("interval_f", "Interval [a, b]:", min = -10, max = 10, value = c(-5, 5))
        ),
        conditionalPanel(
          condition = "input.distributie == 'Funcție personalizata G'",
          sliderInput("interval_g", "Interval [a, b]:", min = -10, max = 10, value = c(-5, 5))
        )
      )
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("summary")
    )
  )
)

# Logica serverului (Server)
server <- function(input, output) {
  output$distPlot <- renderPlot({
    if (input$mod == "Distribuții variabile aleatoare") {
      n <- input$n
      
      # Generăm distribuția aleatoare
      if (input$distribution == "Normală N(0,1)") {
        data <- rnorm(n, mean = 0, sd = 1)
      } else if (input$distribution == "Normală N(μ,σ^2)") {
        data <- rnorm(n, mean = input$mu, sd = input$sigma)
      } else if (input$distribution == "Exponențială Exp(λ)") {
        data <- rexp(n, rate = input$lambda)
      } else if (input$distribution == "Poisson Pois(λ)") {
        data <- rpois(n, lambda = input$lambda_pois)
      } else if (input$distribution == "Binomială Binom(r, p)") {
        data <- rbinom(n, size = input$r, prob = input$p)
      }
      
      # Calculăm variabila selectată
      variable_data <- switch(input$variable,
                              "X" = data,
                              "3 - 2X" = 3 - 2 * data,
                              "2 + 5X" = 2 + 5 * data,
                              "3X - 2" = 3 * data - 2,
                              "5X - 4" = 5 * data - 4, 
                              "X^2" = data^2,
                              "X^3" = data^3,
                              "Suma (ΣX)" = cumsum(data),  # Suma cumulativă
                              "Suma pătratelor (ΣX^2)" = cumsum(data^2)  # Suma pătratelor
      )
      
      # Creăm un data frame pentru grafic
      df <- data.frame(x = variable_data)
      
      # Reprezentăm ECDF (funcția de repartiție empirică)
      ggplot(df, aes(x)) +
        stat_ecdf(geom = "step", color = "blue") +
        labs(
          title = paste("Funcția de repartiție: ", input$distribution, " pentru ", input$variable),
          x = "Valori",
          y = "Funcția de repartiție (ECDF)"
        ) +
        theme_minimal()
    } else {
      if (input$distributie == "Funcție personalizată A") {
        a <- input$interval_a[1]
        b <- input$interval_a[2]
        c <- input$c
        curve(c * x^4, from = a, to = b, col = "orange", lwd = 2,
              main = "Funcție personalizată A: f(x) = c * x^4",
              xlab = "x", ylab = "f(x)")
      } else if (input$distributie == "Funcție personalizată B") {
        a <- input$interval_b[1]
        b <- input$interval_b[2]
        coef_a <- input$a
        coef_b <- input$b
        curve(coef_a * x + coef_b * x^2, from = a, to = b, col = "brown", lwd = 2,
              main = "Funcție personalizată B: f(x) = a * x + b * x^2",
              xlab = "x", ylab = "f(x)")
      } else if (input$distributie == "Funcție personalizată C") {
        x_vals <- seq(input$x_c_min, input$x_c_max, by = 1)
        f_vals <- 4 / (x_vals * (x_vals + 1) * (x_vals + 2))
        plot(x_vals, f_vals, type = "h", col = "pink", lwd = 2,
             main = "Funcție personalizată C: f(x) = 4 / (x * (x+1) * (x+2))",
             xlab = "x", ylab = "f(x)")
      } else if (input$distributie == "Funcție personalizată D") {
        x_vals <- as.numeric(input$x_d_values)
        f_vals <- log(x_vals / (x_vals + 1))
        plot(x_vals, f_vals, type = "h", col = "cyan", lwd = 2,
             main = "Funcție personalizată D: f(x) = log(x / (x+1))",
             xlab = "x", ylab = "f(x)")
      } else if (input$distributie == "Funcție personalizată E") {
        a <- input$interval_e[1]
        b <- input$interval_e[2]
        theta <- input$theta
        curve(((theta^2) / (1 + theta)) * (1 + x) * exp(-theta * x), from = a, to = b,
              col = "magenta", lwd = 2,
              main = "Funcție personalizată E: f(x)",
              xlab = "x", ylab = "f(x)")
      } else if (input$distributie == "Funcție personalizata F") {
        a <- input$interval_f[1]
        b <- input$interval_f[2]
        f <- function(x) {
          ifelse(x < 0, (1/3) * exp(x), 
                 ifelse(x >= 0 & x < 1, 1/3, 
                        (1/3) * exp(-(x - 1))))
        }
        curve(f, from = a, to = b, col = "blue", lwd = 2,
              main = "Funcție personalizata F",
              xlab = "x", ylab = "f(x)")
      } else if (input$distributie == "Funcție personalizata G") {
        a <- input$interval_g[1]
        b <- input$interval_g[2]
        curve(1 / (pi * (1 + x^2)), from = a, to = b, col = "red", lwd = 2,
              main = "Funcție personalizata G: f(x) = 1 / (π * (1 + x^2))",
              xlab = "x", ylab = "f(x)")
      }
    }
  })
}

shinyApp(ui = ui, server = server)