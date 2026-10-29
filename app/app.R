# Shiny dashboard -- run with shiny::runApp("app") from the project root
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(plotly)
  library(DT)
  library(scales)
})

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "player_id", "archetype", "career_quarter", "rank", "name",
      "pivot_rate", "lo", "hi", "n", "pivot_events", "x", "y", "lr", "q",
      "prob_pivot", "pivoted"
    )
  )
}

# ============================================================
# DATA
# ============================================================
out <- if (dir.exists("outputs")) "outputs" else "../outputs"
dash <- readRDS(file.path(out, "dashboard_data.rds"))

players <- arrange(dash$players, name) |>
  mutate(archetype = as.character(archetype))
traj <- dash$traj |> mutate(archetype = as.character(archetype))
medoids <- dash$medoids |>
  mutate(archetype = as.character(archetype)) |>
  arrange(archetype, career_quarter)
arch <- dash$archetype_summary |>
  mutate(
    archetype = as.character(archetype),
    lo = coalesce(lo, pivot_rate),
    hi = coalesce(hi, pivot_rate)
  ) |>
  arrange(desc(pivot_rate))

# ---- Consistent colour per archetype across every chart -------------------
arch_levels <- sort(unique(players$archetype))
base_cols <- c(
  "#4f46e5", "#06b6d4", "#f59e0b", "#ef4444",
  "#10b981", "#8b5cf6", "#ec4899", "#64748b"
)
pal <- setNames(
  if (length(arch_levels) <= length(base_cols)) {
    base_cols[seq_along(arch_levels)]
  } else {
    grDevices::colorRampPalette(base_cols)(length(arch_levels))
  },
  arch_levels
)
outcome_cols <- c("No pivot" = "#cbd5e1", "Pivoted to doubles" = "#f59e0b")

# ---- 2-D map of the clustering space (PCA of the trajectories) -----------
# K-means was fitted on standardised early-career trajectories, so a PCA of
# the same trajectories shows how the clusters sit relative to each other.
build_cluster_map <- function(traj, players) {
  wide <- traj |>
    transmute(player_id, q = career_quarter, lr = log(rank)) |>
    tidyr::pivot_wider(
      names_from = q, values_from = lr,
      names_prefix = "q", values_fn = mean
    )
  m <- as.matrix(wide[, -1, drop = FALSE])
  for (j in seq_len(ncol(m))) {
    miss <- is.na(m[, j])
    if (all(miss)) {
      m[, j] <- 0
    } else if (any(miss)) {
      m[miss, j] <- median(m[, j], na.rm = TRUE)
    }
  }
  m <- m[, apply(m, 2, sd) > 0, drop = FALSE]
  pc <- prcomp(m, scale. = TRUE)
  var_exp <- pc$sdev^2 / sum(pc$sdev^2)
  scores <- tibble::tibble(
    player_id = wide$player_id,
    PC1 = pc$x[, 1],
    PC2 = pc$x[, 2]
  )
  list(data = inner_join(players, scores, by = "player_id"), var = var_exp)
}
cmap <- build_cluster_map(traj, players)
map_data <- cmap$data

feat_choices <- c(
  "Best career rank" = "best_rank",
  "Early win rate" = "early_win_rate",
  "Early singles matches" = "early_singles",
  "Early doubles share" = "early_doubles_share",
  "Height" = "height"
)
feat_choices <- feat_choices[feat_choices %in% names(players)]

# ---- Headline numbers -----------------------------------------------------
overall_rate <- mean(players$pivoted == "yes", na.rm = TRUE)
top_arch <- arch[which.max(arch$pivot_rate), ]

# ============================================================
# HELPERS
# ============================================================
rank_ticks <- c(1, 10, 50, 100, 250, 500, 1000, 2000)
rank_axis <- list(
  type = "log", autorange = "reversed",
  tickvals = rank_ticks, ticktext = as.character(rank_ticks)
)

style_plot <- function(p, xtitle = NULL, ytitle = NULL,
                       xextra = list(), yextra = list(), legend_y = -0.2) {
  p |>
    layout(
      font = list(
        family = "Inter, system-ui, -apple-system, sans-serif",
        color = "#334155"
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = c(
        list(title = xtitle, gridcolor = "#eef0f6", zeroline = FALSE), xextra
      ),
      yaxis = c(
        list(title = ytitle, gridcolor = "#eef0f6", zeroline = FALSE), yextra
      ),
      legend = list(orientation = "h", y = legend_y, x = 0),
      margin = list(l = 55, r = 10, t = 10, b = 40),
      hoverlabel = list(font = list(family = "Inter, system-ui, sans-serif"))
    ) |>
    config(displayModeBar = FALSE)
}

fmt <- function(x, f = as.character) {
  if (length(x) == 0 || is.na(x)) "\u2014" else f(x)
}

# ============================================================
# UI
# ============================================================
app_css <- "
body { background: #f4f5fb; }
.navbar {
  background: linear-gradient(90deg, #312e81, #4f46e5) !important;
  box-shadow: 0 2px 14px rgba(49, 46, 129, .28);
}
.navbar .navbar-brand, .navbar .nav-link { color: #fff !important; }
.navbar .navbar-brand { font-weight: 700; letter-spacing: .2px; }
.navbar .nav-link { opacity: .72; font-weight: 500; }
.navbar .nav-link:hover, .navbar .nav-link.active { opacity: 1; }
.navbar .nav-link.active { border-bottom: 2px solid #fff; }
.card {
  border: none; border-radius: 16px; background: #fff;
  box-shadow: 0 1px 3px rgba(16,24,40,.06), 0 10px 28px rgba(16,24,40,.06);
}
.card-header {
  background: transparent; border-bottom: 1px solid #eef0f6;
  font-weight: 600; color: #1e293b;
}
.bslib-value-box { border-radius: 16px; border: none; }
.bslib-sidebar-layout > .sidebar { background: #fff; }
.hero {
  background: linear-gradient(135deg, #4f46e5 0%, #7c3aed 100%);
  color: #fff; border-radius: 18px; padding: 26px 30px; margin-bottom: 4px;
  box-shadow: 0 12px 30px rgba(79,70,229,.28);
}
.hero h2 { font-weight: 700; margin: 0 0 6px 0; font-size: 1.6rem; }
.hero p { margin: 0; opacity: .88; max-width: 760px; }
.stat-row {
  display: flex; justify-content: space-between; padding: 9px 0;
  border-bottom: 1px solid #f1f2f8; font-size: .95rem;
}
.stat-row:last-child { border-bottom: none; }
.stat-label { color: #64748b; }
.stat-value { font-weight: 600; color: #1e293b; }
.pill {
  display: inline-block; padding: 3px 12px; border-radius: 999px;
  font-size: .78rem; font-weight: 600; margin-bottom: 12px;
}
.pill-test { background: #dcfce7; color: #166534; }
.pill-train { background: #e0e7ff; color: #3730a3; }
.prob-wrap { margin: 4px 0 14px 0; }
.prob-bar { height: 10px; border-radius: 999px; background: #eef0f6; }
.prob-bar > div {
  height: 100%; border-radius: 999px;
  background: linear-gradient(90deg, #6366f1, #a855f7);
}
.prob-caption { font-size: .8rem; color: #64748b; margin-top: 5px; }
.hint { font-size: .82rem; color: #64748b; }
.step-num {
  width: 34px; height: 34px; border-radius: 50%; background: #e0e7ff;
  color: #4338ca; font-weight: 700; display: flex; align-items: center;
  justify-content: center; margin-bottom: 10px;
}
table.dataTable thead th { color: #475569; font-weight: 600; }
"

ui <- page_navbar(
  title = tags$span(icon("bullseye"), " Singles-to-Doubles Pivot Explorer"),
  window_title = "Pivot Explorer",
  theme = bs_theme(
    version = 5,
    bg = "#f4f5fb",
    fg = "#1e293b",
    primary = "#4f46e5",
    secondary = "#64748b",
    success = "#10b981",
    base_font = font_collection(
      font_google("Inter", local = FALSE), "system-ui", "sans-serif"
    )
  ),
  fillable = FALSE,
  header = tags$style(HTML(app_css)),

  # ---------------------------------------------------------- Overview ----
  nav_panel(
    "Overview",
    icon = icon("chart-pie"),
    div(
      class = "hero",
      h2("Do early-career rankings hint at a switch to doubles?"),
      p(
        "Players are grouped into archetypes by how their ATP singles rank",
        "evolved in the first years of their career. Explore how often each",
        "archetype later pivots to doubles."
      )
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),
      value_box(
        "Players analysed", scales::comma(nrow(players)),
        showcase = icon("users"), theme = "primary"
      ),
      value_box(
        "Career archetypes", length(arch_levels),
        showcase = icon("layer-group"), theme = "info"
      ),
      value_box(
        "Overall pivot rate", scales::percent(overall_rate, accuracy = 0.1),
        showcase = icon("chart-line"), theme = "success"
      ),
      value_box(
        "Highest-pivot archetype",
        top_arch$archetype,
        p(scales::percent(top_arch$pivot_rate, accuracy = 0.1), "pivot rate"),
        showcase = icon("trophy"), theme = "warning"
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Medoid trajectories by archetype"),
        plotlyOutput("medoid_plot", height = "360px")
      ),
      card(
        card_header("Pivot rate by archetype (95% CI)"),
        plotlyOutput("rate_plot", height = "360px")
      )
    ),
    card(
      card_header("Archetype summary"),
      DTOutput("arch_tbl")
    )
  ),

  # ------------------------------------------------------- Cluster map ----
  nav_panel(
    "Cluster map",
    icon = icon("braille"),
    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        radioButtons(
          "view", "Projection",
          choices = c(
            "Trajectory map (PCA)" = "pca",
            "Player features" = "features"
          )
        ),
        conditionalPanel(
          "input.view == 'features'",
          selectInput(
            "xvar", "X axis", feat_choices,
            selected = feat_choices[1]
          ),
          selectInput(
            "yvar", "Y axis", feat_choices,
            selected = feat_choices[min(2, length(feat_choices))]
          )
        ),
        radioButtons(
          "color_by", "Colour by",
          choices = c("Archetype" = "archetype", "Pivot outcome" = "outcome"),
          inline = TRUE
        ),
        checkboxGroupInput(
          "arch_filter", "Archetypes",
          choices = arch_levels, selected = arch_levels
        ),
        checkboxInput("show_centres", "Show cluster centres", TRUE),
        selectizeInput(
          "hl_player", "Highlight a player",
          choices = NULL,
          options = list(placeholder = "Type a name...")
        ),
        p(
          class = "hint",
          "Each dot is a player. Hover for details; drag to zoom, double-click",
          "to reset."
        )
      ),
      card(
        card_header("How the archetypes cluster"),
        plotlyOutput("cluster_plot", height = "620px"),
        card_footer(class = "hint", textOutput("cluster_note"))
      )
    )
  ),

  # ----------------------------------------------------- Player lookup ----
  nav_panel(
    "Player lookup",
    icon = icon("magnifying-glass"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectizeInput(
          "player", "Search player",
          choices = NULL,
          options = list(placeholder = "Type a name...")
        ),
        p(
          class = "hint",
          "Pick a player to see their archetype, model-estimated pivot",
          "probability and how their early ranking compares to the archetype",
          "medoid."
        )
      ),
      layout_columns(
        fill = FALSE,
        col_widths = c(4, 4, 4),
        value_box(
          "Early-career archetype", textOutput("vb_arch"),
          showcase = icon("layer-group"), theme = "primary"
        ),
        value_box(
          "Predicted pivot probability", textOutput("vb_prob"),
          showcase = icon("percent"), theme = "info"
        ),
        value_box(
          "Actual outcome", textOutput("vb_actual"),
          showcase = icon("bullseye"), theme = "success"
        )
      ),
      layout_columns(
        col_widths = c(4, 8),
        card(card_header("Player profile"), uiOutput("profile")),
        card(
          card_header("Early-career ranking trajectory vs archetype medoid"),
          plotlyOutput("traj_plot", height = "400px")
        )
      )
    )
  ),

  # -------------------------------------------------------------- About ----
  nav_panel(
    "About",
    icon = icon("circle-info"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        div(class = "step-num", "1"),
        h5("Cluster"),
        p(
          "Players are clustered by their first years of ATP singles rankings",
          "into career archetypes."
        )
      ),
      card(
        div(class = "step-num", "2"),
        h5("Predict"),
        p(
          "A model estimates the probability of a later shift to doubles",
          "for each player."
        )
      ),
      card(
        div(class = "step-num", "3"),
        h5("Explore"),
        p(
          "Use the cluster map, archetype overview and player lookup to",
          "interpret the results."
        )
      )
    ),
    card(
      card_header("Reading the results"),
      p(
        "Predictions for players in training data are in-sample;",
        "held-out test-set players are explicitly flagged."
      ),
      p(
        "The cluster map is a 2-D PCA projection of the same standardised",
        "trajectories used for clustering, so players that sit close together",
        "had similar early-career ranking paths."
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
# nolint start: object_usage_linter.
server <- function(input, output, session) {
  choices <- setNames(players$player_id, players$name)
  best <- which.max(players$prob_pivot)
  default_id <- players$player_id[if (length(best) == 0) 1 else best]

  updateSelectizeInput(
    session, "player",
    server = TRUE, choices = choices, selected = default_id
  )
  updateSelectizeInput(session, "hl_player", server = TRUE, choices = choices)

  sel <- reactive({
    req(input$player)
    filter(players, player_id == as.integer(input$player))
  })

  # ---- Player lookup ------------------------------------------------------
  output$vb_arch <- renderText(sel()$archetype)

  output$vb_prob <- renderText({
    p <- sel()$prob_pivot
    if (is.na(p)) "N/A" else sprintf("%.1f%%", 100 * p)
  })

  output$vb_actual <- renderText({
    if (sel()$pivoted == "yes") {
      "Pivoted to doubles"
    } else {
      "No pivot in follow-up"
    }
  })

  output$profile <- renderUI({
    p <- sel()
    prob <- p$prob_pivot
    arch_rate <- arch$pivot_rate[arch$archetype == p$archetype]
    row <- function(label, value) {
      div(
        class = "stat-row",
        span(class = "stat-label", label),
        span(class = "stat-value", value)
      )
    }
    tagList(
      if (isTRUE(p$in_test)) {
        span(class = "pill pill-test", "Held-out test player")
      } else {
        span(class = "pill pill-train", "Training data (in-sample)")
      },
      if (!is.na(prob)) {
        div(
          class = "prob-wrap",
          div(
            class = "prob-bar",
            div(style = paste0("width:", round(100 * prob), "%"))
          ),
          div(
            class = "prob-caption",
            paste0(
              "Model probability ", scales::percent(prob, accuracy = 0.1),
              if (length(arch_rate) == 1) {
                paste0(
                  " \u00b7 archetype average ",
                  scales::percent(arch_rate, accuracy = 0.1)
                )
              } else {
                ""
              }
            )
          )
        )
      },
      row("Born", fmt(p$dob, function(d) format(d))),
      row("Plays", fmt(p$hand, function(h) {
        h <- as.character(h)
        switch(h,
          R = "Right-handed",
          L = "Left-handed",
          h
        )
      })),
      row("Height", fmt(p$height, function(h) paste(round(h), "cm"))),
      row("Best rank", fmt(p$best_rank, function(r) paste0("#", round(r)))),
      row("Early singles matches", fmt(p$early_singles, function(v) round(v))),
      row(
        "Early win rate",
        fmt(p$early_win_rate, function(v) scales::percent(v, accuracy = 0.1))
      ),
      row(
        "Early doubles share",
        fmt(
          p$early_doubles_share,
          function(v) scales::percent(v, accuracy = 0.1)
        )
      )
    )
  })

  output$traj_plot <- renderPlotly({
    p <- sel()
    col <- unname(pal[p$archetype])
    pl <- filter(traj, player_id == p$player_id)
    md <- filter(medoids, archetype == p$archetype)
    plot_ly() |>
      add_trace(
        data = md, x = ~career_quarter, y = ~rank,
        type = "scatter", mode = "lines", name = "Archetype medoid",
        line = list(color = "#94a3b8", width = 3, dash = "dash")
      ) |>
      add_trace(
        data = pl, x = ~career_quarter, y = ~rank,
        type = "scatter", mode = "lines+markers", name = p$name,
        line = list(color = col, width = 3),
        marker = list(color = col, size = 8)
      ) |>
      style_plot(
        "Quarters since debut", "ATP rank (log scale, better = up)",
        yextra = rank_axis
      )
  })

  # ---- Overview -----------------------------------------------------------
  output$medoid_plot <- renderPlotly({
    plot_ly(
      medoids,
      x = ~career_quarter, y = ~rank, color = ~archetype, colors = pal,
      type = "scatter", mode = "lines", line = list(width = 3)
    ) |>
      style_plot(
        "Quarters since debut", "ATP rank (log scale, better = up)",
        yextra = rank_axis
      )
  })

  output$rate_plot <- renderPlotly({
    plot_ly(
      arch,
      x = ~archetype, y = ~pivot_rate, type = "bar",
      marker = list(color = unname(pal[arch$archetype])),
      error_y = ~ list(
        type = "data", symmetric = FALSE,
        array = hi - pivot_rate, arrayminus = pivot_rate - lo,
        color = "#475569", thickness = 1.5
      ),
      text = ~ paste0(
        "<b>", archetype, "</b><br>Pivot rate: ",
        scales::percent(pivot_rate, accuracy = 0.1),
        "<br>Players: ", scales::comma(n)
      ),
      hoverinfo = "text"
    ) |>
      style_plot(
        NULL, "Pivot rate",
        xextra = list(
          categoryorder = "array", categoryarray = arch$archetype,
          tickangle = -20
        ),
        yextra = list(tickformat = ".0%")
      )
  })

  output$arch_tbl <- renderDT({
    tbl <- arch |>
      select(archetype, n, pivot_events, pivot_rate, lo, hi)
    datatable(
      tbl,
      rownames = FALSE,
      class = "stripe hover",
      colnames = c(
        "Archetype", "Players", "Pivots", "Pivot rate",
        "95% CI low", "95% CI high"
      ),
      options = list(dom = "t", pageLength = 20)
    ) |>
      formatPercentage(c("pivot_rate", "lo", "hi"), digits = 1)
  })

  # ---- Cluster map --------------------------------------------------------
  scatter_data <- reactive({
    d <- map_data
    if (input$view == "features" && length(feat_choices) >= 2) {
      d$x <- d[[input$xvar]]
      d$y <- d[[input$yvar]]
    } else {
      d$x <- d$PC1
      d$y <- d$PC2
    }
    d <- d[d$archetype %in% input$arch_filter & !is.na(d$x) & !is.na(d$y), ]
    d$hover <- paste0(
      "<b>", d$name, "</b><br>Archetype: ", d$archetype,
      "<br>Pivot probability: ",
      ifelse(is.na(d$prob_pivot), "N/A",
        scales::percent(d$prob_pivot, accuracy = 0.1)
      ),
      "<br>Outcome: ", ifelse(d$pivoted == "yes", "Pivoted", "No pivot")
    )
    d
  })

  axis_labels <- reactive({
    if (input$view == "features" && length(feat_choices) >= 2) {
      list(
        x = names(feat_choices)[match(input$xvar, feat_choices)],
        y = names(feat_choices)[match(input$yvar, feat_choices)]
      )
    } else {
      list(
        x = sprintf("PC1 (%.0f%% of variance)", 100 * cmap$var[1]),
        y = sprintf("PC2 (%.0f%% of variance)", 100 * cmap$var[2])
      )
    }
  })

  output$cluster_plot <- renderPlotly({
    req(length(input$arch_filter) > 0)
    d <- scatter_data()
    req(nrow(d) > 0)

    if (input$color_by == "archetype") {
      d$grp <- factor(d$archetype, levels = arch_levels)
      cols <- pal
    } else {
      d$grp <- factor(
        ifelse(d$pivoted == "yes", "Pivoted to doubles", "No pivot"),
        levels = c("No pivot", "Pivoted to doubles")
      )
      cols <- outcome_cols
      d <- d[order(d$grp), ] # draw pivots on top
    }
    d$grp <- droplevels(d$grp)
    trace_type <- if (nrow(d) > 3000) "scattergl" else "scatter"

    p <- plot_ly(
      d,
      x = ~x, y = ~y, color = ~grp, colors = cols,
      type = trace_type, mode = "markers",
      text = ~hover, hoverinfo = "text",
      marker = list(size = 7, opacity = 0.7)
    )

    if (isTRUE(input$show_centres)) {
      ctr <- d |>
        group_by(archetype) |>
        summarise(x = mean(x), y = mean(y), .groups = "drop")
      p <- p |>
        add_trace(
          data = ctr, x = ~x, y = ~y, inherit = FALSE,
          type = trace_type, mode = "markers+text", # was "scatter"
          text = ~archetype, textposition = "top center",
          textfont = list(size = 12, color = "#111827"),
          marker = list(
            symbol = "diamond", size = 15, color = unname(pal[ctr$archetype]),
            line = list(color = "#111827", width = 2)
          ),
          hoverinfo = "skip", showlegend = FALSE
        )
    }

    hid <- input$hl_player
    if (!is.null(hid) && nzchar(hid)) {
      h <- d[d$player_id == as.integer(hid), ]
      if (nrow(h) == 1) {
        p <- p |>
          add_trace(
            data = h, x = ~x, y = ~y, inherit = FALSE,
            type = trace_type, mode = "markers", name = h$name, # was "scatter"
            text = ~hover, hoverinfo = "text",
            marker = list(
              symbol = "star", size = 22, color = "#facc15", # yellow so it pops
              line = list(color = "#111827", width = 2)
            )
          )
      }
    }

    lab <- axis_labels()
    xextra <- list()
    yextra <- list()
    if (input$view == "features") {
      if (identical(input$xvar, "best_rank")) xextra <- list(type = "log")
      if (identical(input$yvar, "best_rank")) yextra <- list(type = "log")
    }
    style_plot(p, lab$x, lab$y, xextra, yextra, legend_y = -0.12)
  })

  output$cluster_note <- renderText({
    d <- scatter_data()
    base <- paste0(scales::comma(nrow(d)), " players shown. ")
    if (input$view == "pca") {
      paste0(
        base,
        "PCA of the standardised early-career rank trajectories used by the ",
        "clustering; the first two components capture ",
        sprintf("%.0f%%", 100 * sum(cmap$var[1:2])), " of the variance."
      )
    } else {
      paste0(base, "Axes show raw player features, coloured by cluster.")
    }
  })
}
# nolint end: object_usage_linter.

shinyApp(ui, server)
