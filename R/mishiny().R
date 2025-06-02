#' Prueba de la primera función
#' Varias líenas de prueba
#' Escrbir 'mishiny()'
#'
mishiny <- function(){
  # Cargar bibliotecas requeridas
  if(!require(shiny))install.packages("shiny", quiet = TRUE)
  library(shiny)
  if(!require(ggplot2))install.packages("ggplot2", quiet = TRUE)
  library(ggplot2)
  if(!require(plotly))install.packages("plotly", quiet = TRUE)
  library(plotly)
  if(!require(reshape2))install.packages("reshape2", quiet = TRUE)
  library(reshape2)
  if(!require(viridisLite))install.packages("viridisLite", quiet = TRUE)
  library(viridisLite)
  if(!require(scales))install.packages("scales", quiet = TRUE)
  library(scales)
  if(!require(rhandsontable))install.packages("rhandsontable", quiet = TRUE)
  library(rhandsontable)
  if(!require(sf))install.packages("sf", quiet = TRUE)
  library(sf)

  # Definir la interfaz de usuario
  ui <- fluidPage(
    titlePanel("Generador de mapas de ruido"),
    sidebarLayout(
      sidebarPanel(
        numericInput("largo", "Largo de la nave (m):", 100),
        numericInput("ancho", "Ancho de la nave (m):", 50),
        numericInput("n_maquinas", "Número de máquinas:", 2),
        uiOutput("maquinas_ui"),

        checkboxInput("incluir_trabajadores", "¿Deseas incluir trabajadores?", FALSE),
        conditionalPanel("input.incluir_trabajadores == true",
                         numericInput("n_trabajadores", "Trabajadores:", 1),
                         uiOutput("trabajadores_ui")),

        checkboxInput("incluir_puerta", "¿Deseas indicar la puerta?", FALSE),
        conditionalPanel("input.incluir_puerta == true",
                         numericInput("puerta_x", "X:", 0),
                         numericInput("puerta_y", "Y:", 0)),

        checkboxInput("incluir_oficinas", "¿Deseas indicar oficinas?", FALSE),
        conditionalPanel("input.incluir_oficinas == true",
                         numericInput("ofi_xmin", "X desde:", 10),
                         numericInput("ofi_xmax", "X hasta:", 20),
                         numericInput("ofi_ymin", "Y desde:", 10),
                         numericInput("ofi_ymax", "Y hasta:", 20)),

        checkboxInput("usar_contorno", "¿Deseas definir contorno personalizado?", FALSE),
        conditionalPanel("input.usar_contorno == true",
                         helpText("Introduce los vértices (hasta 16 o más, cerrar manualmente):"),
                         rHandsontableOutput("tabla_contorno")),

        checkboxInput("usar_estimacion_Li", "¿Deseas estimar Lᵢ a partir de mediciones?", FALSE),
        conditionalPanel("input.usar_estimacion_Li == true",
                         helpText("Introduce coordenadas y nivel Lp medido:"),
                         rHandsontableOutput("tabla_mediciones"),
                         actionButton("estimar_Li", "Estimar niveles Lᵢ"),
                         verbatimTextOutput("resultado_Li")),

        checkboxInput("usar_epi", "¿Deseas aplicar protección auditiva (EPI)?", FALSE),
        conditionalPanel("input.usar_epi == true",
                         numericInput("snr_valor", "Reducción SNR [dB]:", value = 20, min = 0, max = 40)),

        checkboxInput("usar_cerramiento", "¿Deseas aplicar cerramientos a máquinas?", FALSE),
        conditionalPanel("input.usar_cerramiento == true",
                         uiOutput("ui_cerramientos")),

        checkboxInput("usar_reverberacion", "¿Incluir campo reverberante?", FALSE),
        conditionalPanel("input.usar_reverberacion == true",
                         selectInput("material_reverberacion", "Selecciona el material predominante:",
                                     choices = c(
                                       "Hormigón / Ladrillo / Azulejo" = 0.03,
                                       "Madera barnizada" = 0.08,
                                       "Pintura sobre cemento" = 0.05,
                                       "Moqueta / alfombra gruesa" = 0.5,
                                       "Panel acústico poroso" = 0.8,
                                       "Cortinas pesadas" = 0.6,
                                       "Techo acústico perforado" = 0.75,
                                       "Superficie mixta (estimado)" = 0.25
                                     ),
                                     selected = 0.05),
                         numericInput("altura_techo", "Altura del techo (m):", value = 5, min = 2, max = 20, step = 0.5)
        )
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Mapa 2D", plotOutput("mapa_2d")),
          tabPanel("Mapa con isófonas", plotOutput("mapa_isofonas")),
          tabPanel("Mapa 3D", plotlyOutput("mapa_3d"))
        )
      )
    )
  )

  # Definir la lógica del servidor
  server <- function(input, output, session) {

    output$maquinas_ui <- renderUI({
      lapply(1:input$n_maquinas, function(i) {
        tagList(
          textInput(paste0("nombre_", i), paste("Máquina", i), value = paste0("M", i)),
          numericInput(paste0("x_", i), "X:", value = i * 10),
          numericInput(paste0("y_", i), "Y:", value = i * 5),
          numericInput(paste0("Lp_", i), "Lp [dB(A)]:", value = 100)
        )
      })
    })

    output$trabajadores_ui <- renderUI({
      lapply(1:input$n_trabajadores, function(i) {
        tagList(
          textInput(paste0("trab_nombre_", i), paste("Trabajador", i), value = paste0("T", i)),
          numericInput(paste0("trab_x_", i), "X:", value = i * 5),
          numericInput(paste0("trab_y_", i), "Y:", value = i * 5)
        )
      })
    })

    output$tabla_contorno <- renderRHandsontable({
      if (is.null(input$tabla_contorno)) {
        df <- data.frame(
          x = c(0, 0, 20, 40, 60),
          y = c(0, 40, 50, 50, 40)
        )
        rhandsontable(df)
      } else {
        rhandsontable(hot_to_r(input$tabla_contorno))
      }
    })

    output$tabla_mediciones <- renderRHandsontable({
      if (is.null(input$tabla_mediciones)) {
        df <- data.frame(x = c(5, 20), y = c(5, 15), Lp = c(95, 99))
        rhandsontable(df)
      } else {
        rhandsontable(hot_to_r(input$tabla_mediciones))
      }
    })

    output$ui_cerramientos <- renderUI({
      req(input$usar_cerramiento)
      maquinas <- maquinas_df()
      lapply(1:nrow(maquinas), function(i) {
        numericInput(paste0("cerramiento_", i),
                     label = paste("Reducción para", maquinas$nombre[i], "[dB]:"),
                     value = 0, min = 0, max = 40)
      })
    })

    maquinas_df <- reactive({
      data.frame(
        nombre = sapply(1:input$n_maquinas, function(i) input[[paste0("nombre_", i)]]),
        x = sapply(1:input$n_maquinas, function(i) input[[paste0("x_", i)]]),
        y = sapply(1:input$n_maquinas, function(i) input[[paste0("y_", i)]]),
        Lp = sapply(1:input$n_maquinas, function(i) input[[paste0("Lp_", i)]]),
        stringsAsFactors = FALSE
      )
    })

    Li_ajustado <- reactive({
      Li <- setNames(maquinas_df()$Lp, maquinas_df()$nombre)
      if (input$usar_cerramiento) {
        for (i in 1:length(Li)) {
          Li[i] <- Li[i] - input[[paste0("cerramiento_", i)]]
        }
      }
      Li
    })

    superficie <- reactive({
      2 * (input$largo * input$altura_techo + input$ancho * input$altura_techo) + input$largo * input$ancho
    })

    volumen <- reactive({
      input$largo * input$ancho * input$altura_techo
    })

    area_absorcion <- reactive({
      superficie() * as.numeric(input$material_reverberacion)
    })





    calcular_Lp <- function(px, py, maquinas, Li) {
      suma_directo <- 0
      for (i in 1:nrow(maquinas)) {
        r <- sqrt((px - maquinas$x[i])^2 + (py - maquinas$y[i])^2)
        r <- ifelse(r < 1, 1, r)
        suma_directo <- suma_directo + 10^((Li[maquinas$nombre[i]] - 20 * log10(r)) / 10)
      }
      Lp_directo <- 10 * log10(suma_directo)

      if (isTRUE(input$usar_reverberacion)) {
        A <- area_absorcion()
        suma_reverb <- 0
        for (i in 1:nrow(maquinas)) {
          Lw <- Li[maquinas$nombre[i]]
          Lp_reverb <- Lw - 10 * log10(A) + 6
          suma_reverb <- suma_reverb + 10^(Lp_reverb / 10)
        }
        Lp_total <- 10 * log10(10^(Lp_directo / 10) + suma_reverb)
        return(Lp_total)
      } else {
        return(Lp_directo)
      }
    }


    observeEvent(input$estimar_Li, {
      req(input$usar_estimacion_Li)
      puntos <- hot_to_r(input$tabla_mediciones)
      maquinas <- maquinas_df()

      r <- outer(1:nrow(puntos), 1:nrow(maquinas), Vectorize(function(i, j) {
        sqrt((puntos$x[i] - maquinas$x[j])^2 + (puntos$y[i] - maquinas$y[j])^2)
      }))
      colnames(r) <- maquinas$nombre

      estimar_Lp <- function(Li) {
        sapply(1:nrow(r), function(j) {
          10 * log10(sum(10 ^ ((Li - 20 * log10(r[j, ])) / 10)))
        })
      }

      error <- function(Li) {
        sum((estimar_Lp(Li) - puntos$Lp)^2)
      }

      resultado <- optim(par = rep(95, nrow(maquinas)), fn = error,
                         lower = rep(80, nrow(maquinas)), upper = rep(130, nrow(maquinas)),
                         method = "L-BFGS-B")

      Li_estimado <- round(resultado$par, 2)
      names(Li_estimado) <- maquinas$nombre

      output$resultado_Li <- renderPrint({
        cat("Niveles estimados Lᵢ (a 1 m):\n")
        print(Li_estimado)
        cat("\n📉 Error cuadrático total:", round(resultado$value, 2))
      })

      lapply(1:nrow(maquinas), function(i) {
        updateNumericInput(session, paste0("Lp_", i), value = Li_estimado[i])
      })
    })
    trabajadores_df <- reactive({
      if (!input$incluir_trabajadores) return(NULL)
      data.frame(
        nombre = sapply(1:input$n_trabajadores, function(i) input[[paste0("trab_nombre_", i)]]),
        x = sapply(1:input$n_trabajadores, function(i) input[[paste0("trab_x_", i)]]),
        y = sapply(1:input$n_trabajadores, function(i) input[[paste0("trab_y_", i)]]),
        stringsAsFactors = FALSE
      )
    })

    puerta_df <- reactive({
      if (!input$incluir_puerta) return(NULL)
      data.frame(x = input$puerta_x, y = input$puerta_y)
    })

    oficinas_df <- reactive({
      if (!input$incluir_oficinas) return(NULL)
      data.frame(xmin = input$ofi_xmin, xmax = input$ofi_xmax,
                 ymin = input$ofi_ymin, ymax = input$ofi_ymax)
    })
    puntos_df <- reactive({
      resolucion <- 0.25
      grid_x <- seq(0, input$largo, by = resolucion)
      grid_y <- seq(0, input$ancho, by = resolucion)
      puntos <- expand.grid(x = grid_x, y = grid_y)

      if (input$usar_contorno && !is.null(input$tabla_contorno)) {
        coords <- hot_to_r(input$tabla_contorno)
        coords <- coords[complete.cases(coords), ]
        coords <- coords[!is.na(coords$x) & !is.na(coords$y), ]
        if (nrow(coords) >= 3) {
          coords <- rbind(coords, coords[1, ])
          poligono <- st_sfc(st_polygon(list(as.matrix(coords))), crs = 4326)
          puntos_sf <- st_as_sf(puntos, coords = c("x", "y"), crs = 4326)
          dentro <- st_within(puntos_sf, poligono, sparse = FALSE)[, 1]
          puntos <- puntos[dentro, , drop = FALSE]
        }
      }

      maquinas <- maquinas_df()
      Li <- Li_ajustado()
      puntos$Lp <- mapply(calcular_Lp, puntos$x, puntos$y,
                          MoreArgs = list(maquinas = maquinas, Li = Li))

      return(puntos)
    })

    colores_viridis <- plasma(256)
    limite_inferior <- 70
    limite_superior <- 120
    corte <- 80
    valores <- seq(limite_inferior, limite_superior, length.out = 256)
    valores_rescale <- ifelse(
      valores < corte,
      rescale(valores, from = c(limite_inferior, corte), to = c(0, 0.05)),
      rescale(valores, from = c(corte, limite_superior), to = c(0.05, 1))
    )

    leyenda_colorbar <- scale_fill_gradientn(
      colours = colores_viridis,
      values = valores_rescale,
      limits = c(limite_inferior, limite_superior),
      name = "Lp [dB(A)]",
      breaks = seq(70, 120, by = 5),
      na.value = colores_viridis[1],
      guide = guide_colorbar(ticks = TRUE, barheight = 20, barwidth = 1.5)
    )

    output$mapa_2d <- renderPlot({
      puntos <- puntos_df()
      maquinas <- maquinas_df()
      trabajadores <- trabajadores_df()
      puerta <- puerta_df()
      oficinas <- oficinas_df()

      maquinas$Lp_val <- mapply(calcular_Lp, maquinas$x, maquinas$y,
                                MoreArgs = list(maquinas = maquinas, Li = Li_ajustado()))
      maquinas$color_texto <- ifelse(maquinas$Lp_val >= 90, "black", "white")

      if (!is.null(trabajadores)) {
        trabajadores$Lp_val <- mapply(calcular_Lp, trabajadores$x, trabajadores$y,
                                      MoreArgs = list(maquinas = maquinas, Li = Li_ajustado()))
        if (input$usar_epi) {
          trabajadores$Lp_val <- pmax(0, trabajadores$Lp_val - input$snr_valor)
        }
        trabajadores$color_texto <- ifelse(trabajadores$Lp_val >= 90, "black", "white")
      }

      p <- ggplot() +
        geom_tile(data = puntos, aes(x = x, y = y, fill = Lp)) +
        geom_point(data = maquinas, aes(x = x, y = y), color = "white", size = 3) +
        geom_text(data = maquinas, aes(x = x, y = y, label = nombre, color = color_texto),
                  vjust = -1, fontface = "bold", size = 3.5, show.legend = FALSE)

      if (!is.null(trabajadores)) {
        p <- p +
          geom_text(data = trabajadores, aes(x = x, y = y), label = "👷", size = 6) +
          geom_text(data = trabajadores, aes(x = x, y = y, label = nombre, color = color_texto),
                    vjust = 2, fontface = "bold", size = 3.5, show.legend = FALSE)
      }

      if (!is.null(puerta)) {
        p <- p +
          annotate("rect", xmin = puerta$x - 0.5, xmax = puerta$x + 0.5,
                   ymin = puerta$y - 0.5, ymax = puerta$y + 0.5, fill = "darkgreen", alpha = 0.6) +
          annotate("text", x = puerta$x, y = puerta$y + 1, label = "Puerta", color = "darkgreen")
      }

      if (!is.null(oficinas)) {
        p <- p +
          annotate("rect", xmin = oficinas$xmin, xmax = oficinas$xmax,
                   ymin = oficinas$ymin, ymax = oficinas$ymax,
                   fill = "grey40", alpha = 0.3) +
          annotate("text", x = (oficinas$xmin + oficinas$xmax)/2,
                   y = (oficinas$ymin + oficinas$ymax)/2, label = "Oficinas", color = "black")
      }

      if (input$usar_contorno && !is.null(input$tabla_contorno)) {
        coords <- hot_to_r(input$tabla_contorno)
        coords <- coords[complete.cases(coords), ]
        coords <- coords[!is.na(coords$x) & !is.na(coords$y), ]
        if (nrow(coords) >= 3) {
          coords <- rbind(coords, coords[1, ])
          poligono <- st_sfc(st_polygon(list(as.matrix(coords))), crs = 4326)
          puntos_sf <- st_as_sf(puntos, coords = c("x", "y"), crs = 4326)
          dentro <- st_within(puntos_sf, poligono, sparse = FALSE)[, 1]
          puntos <- puntos[dentro, , drop = FALSE]
        }
      }

      p + leyenda_colorbar +
        coord_equal() +
        theme_minimal() +
        scale_color_identity() +
        theme(panel.background = element_rect(fill = "white"))
    })

    output$mapa_isofonas <- renderPlot({
      puntos <- puntos_df()
      maquinas <- maquinas_df()
      trabajadores <- trabajadores_df()
      puerta <- puerta_df()
      oficinas <- oficinas_df()

      # Asegurar que puntos se filtran también aquí por el contorno si está activo
      if (input$usar_contorno && !is.null(input$tabla_contorno)) {
        coords <- hot_to_r(input$tabla_contorno)
        coords <- coords[complete.cases(coords), ]
        coords <- coords[!is.na(coords$x) & !is.na(coords$y), ]
        if (nrow(coords) >= 3) {
          coords <- rbind(coords, coords[1, ])
          poligono <- st_sfc(st_polygon(list(as.matrix(coords))), crs = 4326)
          puntos_sf <- st_as_sf(puntos, coords = c("x", "y"), crs = 4326)
          dentro <- st_within(puntos_sf, poligono, sparse = FALSE)[, 1]
          puntos <- puntos[dentro, , drop = FALSE]
        }
      }

      if (nrow(maquinas) > 0) {
        maquinas$Lp_val <- mapply(calcular_Lp, maquinas$x, maquinas$y,
                                  MoreArgs = list(maquinas = maquinas, Li = setNames(maquinas$Lp, maquinas$nombre)))
        maquinas$color_texto <- ifelse(maquinas$Lp_val >= 90, "black", "white")
      }

      if (!is.null(trabajadores)) {
        trabajadores$Lp_val <- mapply(calcular_Lp, trabajadores$x, trabajadores$y,
                                      MoreArgs = list(maquinas = maquinas, Li = setNames(maquinas$Lp, maquinas$nombre)))
        trabajadores$color_texto <- ifelse(trabajadores$Lp_val >= 90, "black", "white")
      }

      p <- ggplot(puntos, aes(x = x, y = y)) +
        geom_tile(aes(fill = Lp)) +
        geom_contour(aes(z = Lp), color = "black", breaks = seq(70, 115, by = 5)) +
        geom_point(data = maquinas, aes(x = x, y = y), color = "white", size = 2) +
        geom_text(data = maquinas, aes(x = x, y = y, label = nombre, color = color_texto),
                  vjust = -1, fontface = "bold", size = 3.5, show.legend = FALSE)

      if (!is.null(trabajadores)) {
        p <- p +
          geom_text(data = trabajadores, aes(x = x, y = y), label = "👷", size = 6) +
          geom_text(data = trabajadores, aes(x = x, y = y, label = nombre, color = color_texto),
                    vjust = 2, fontface = "bold", size = 3.5, show.legend = FALSE)
      }

      if (!is.null(puerta)) {
        p <- p +
          annotate("rect", xmin = puerta$x - 0.5, xmax = puerta$x + 0.5,
                   ymin = puerta$y - 0.5, ymax = puerta$y + 0.5,
                   fill = "darkgreen", alpha = 0.6) +
          annotate("text", x = puerta$x, y = puerta$y + 1,
                   label = "Puerta", color = "darkgreen", fontface = "bold")
      }

      if (!is.null(oficinas)) {
        p <- p +
          annotate("rect", xmin = oficinas$xmin, xmax = oficinas$xmax,
                   ymin = oficinas$ymin, ymax = oficinas$ymax,
                   fill = "grey40", alpha = 0.3) +
          annotate("text", x = (oficinas$xmin + oficinas$xmax)/2,
                   y = (oficinas$ymin + oficinas$ymax)/2,
                   label = "Oficinas", color = "black", fontface = "bold")
      }

      if (input$usar_contorno && !is.null(input$tabla_contorno)) {
        coords <- hot_to_r(input$tabla_contorno)
        if (nrow(coords) >= 3) {
          coords <- rbind(coords, coords[1, ])
          p <- p + geom_path(data = coords, aes(x = x, y = y),
                             color = "black", linewidth = 1)
        }
      }

      p + leyenda_colorbar +
        coord_equal() +
        theme_minimal() +
        scale_color_identity() +
        theme(panel.background = element_rect(fill = "white"))
    })


    output$mapa_3d <- renderPlotly({
      puntos <- puntos_df()
      if (nrow(puntos) == 0) return(NULL)
      puntos_3d <- dcast(puntos, y ~ x, value.var = "Lp")
      Lp_matrix <- as.matrix(puntos_3d[,-1])
      x_vals <- as.numeric(colnames(puntos_3d)[-1])
      y_vals <- puntos_3d$y
      colorescale_plotly <- lapply(1:256, function(i) list(valores_rescale[i], colores_viridis[i]))

      plot_ly(
        x = x_vals, y = y_vals, z = Lp_matrix,
        type = "surface",
        colorscale = colorescale_plotly,
        cmin = 70,
        cmax = 120,
        showscale = TRUE
      ) %>%
        layout(scene = list(
          xaxis = list(title = "X (m)"),
          yaxis = list(title = "Y (m)"),
          zaxis = list(title = "Lp [dB(A)]")
        ))
    })
  }

  shinyApp(ui = ui, server = server)


}
