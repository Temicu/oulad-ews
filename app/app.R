library(shiny); library(bslib); library(mongolite); library(DT); library(ggplot2)

URL  <- "mongodb://localhost:27017"
pred <- mongo("predictions",   db = "oulad_ews", url = URL)
mets <- mongo("model_metrics", db = "oulad_ews", url = URL)
FIELDS <- '{"_id":0}'   # fetch every field except _id

BLUE <- "#1f4e79"; RED <- "#b3402a"; AMBER <- "#c98a00"; GREY <- "#9aa5b1"

# ---- helpers ---------------------------------------------------------------
num_col <- function(d, nm) if (nm %in% names(d)) as.numeric(d[[nm]]) else rep(NA_real_, nrow(d))
pct     <- function(x) if (length(x) == 0 || is.na(x)) "n/a" else sprintf("%.0f%%", 100 * x)
# show every reason (not just the first) so rows no longer look identical
first_reason <- function(r) {
  if (is.data.frame(r) && nrow(r) && "text" %in% names(r))
    paste0(tools::toTitleCase(as.character(r$theme)), ": ", as.character(r$text), collapse = "  |  ")
  else "No behavioural warning signs"
}
stat_box <- function(label, value)
  div(class = "text-center px-2", div(class = "fs-4 fw-semibold", value),
      div(class = "text-muted small", label))
th <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold", colour = BLUE))

# ---- UI --------------------------------------------------------------------
ui <- page_navbar(
  title = "Early-warning dashboard",
  theme = bs_theme(version = 5, primary = BLUE, danger = RED),
  # white text on the selected table row so the band stays readable
  header = tags$style(HTML("table.dataTable tbody tr.selected td{color:#fff !important;}")),
  fillable = FALSE,
  sidebar = sidebar(width = 270,
    selectInput("module", "Module", sort(pred$distinct("module"))),
    selectInput("pres", "Course run", NULL),
    radioButtons("cutoff", "Prediction made at",
                 c("Week 4 (day 28)" = 28, "Week 8 (day 56)" = 56, "Week 12 (day 84)" = 84)),
    selectInput("focus", "Show",
                c("All flagged students" = "flagged", "New this checkpoint" = "new",
                  "Flagged at every checkpoint" = "persist", "All students" = "all")),
    checkboxInput("custom_thr", "Adjust flag threshold", FALSE),
    conditionalPanel("input.custom_thr",
      sliderInput("thr", "Flag if risk is at least", min = 0.05, max = 0.95, value = 0.38, step = 0.01)),
    checkboxInput("test_only", "Held-out test students only", FALSE),
    downloadButton("dl", "Download list (CSV)", class = "btn-sm btn-outline-primary w-100"),
    p(class = "text-muted small mt-3",
      "Reasons shown are behaviour only (activity and assessments). 'Actual outcome' is known
       only because this is historical data."),
    p(class = "text-muted small",
      "New = flagged now but not at any earlier checkpoint (at week 4, every flagged student is new).
       Persistent = flagged at every earlier checkpoint too.")),

  nav_panel("Students",
    uiOutput("cards"),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Ranked students (highest risk first)"), DTOutput("tbl")),
      uiOutput("detail"))),

  nav_panel("Overview",
    layout_columns(col_widths = c(6, 6),
      card(card_header("How risk scores split"), plotOutput("dist", height = 340)),
      card(card_header("Early warning by week"), plotOutput("weeks", height = 340)))),

  nav_panel("Fairness",
    uiOutput("fair_txt"),
    card(card_header("Flagged students: behaviour push vs background push"),
         plotOutput("fair_plot", height = 400)),
    p(class = "text-muted small",
      "Points above the diagonal are flagged mostly because of background, not behaviour.
       These flags deserve extra caution.")),

  nav_panel("Model performance",
    layout_columns(col_widths = c(7, 5),
      card(card_header("AUC by cutoff day"), plotOutput("perf", height = 320)),
      card(card_header("Metrics"), tableOutput("perf_tbl"))))
)

# ---- server ----------------------------------------------------------------
server <- function(input, output, session) {

  pres_choices <- reactive({
    req(input$module)
    sort(pred$distinct("presentation", sprintf('{"module":"%s"}', input$module)))
  })
  observeEvent(pres_choices(), updateSelectInput(session, "pres", choices = pres_choices()))

  # all cutoffs for the chosen run: one Mongo query, reused by every chart
  raw_d <- reactive({
    req(input$module, input$pres %in% pres_choices())
    q <- sprintf('{"module":"%s","presentation":"%s"}', input$module, input$pres)
    d <- jsonlite::flatten(pred$find(q, fields = FIELDS))
    validate(need(nrow(d) > 0, "No students for this selection."))
    d$flagged    <- as.logical(d$flagged)
    d$flagged[is.na(d$flagged)] <- FALSE
    d$actual     <- as.numeric(d$actual_at_risk) == 1
    d$eng        <- num_col(d, "theme_impacts.engagement")
    d$ass        <- num_col(d, "theme_impacts.assessments")
    d$bg         <- num_col(d, "theme_impacts.background")
    d$beh        <- d$eng + d$ass
    # background-driven only when background actually raises risk AND outweighs behaviour
    d$bg_driven  <- d$bg > 0 & d$bg > d$beh
    d
  })

  # apply the optional custom flag threshold on top of the stored model flags
  all_d <- reactive({
    d <- raw_d()
    if (isTRUE(input$custom_thr)) {
      d$flagged <- d$risk_score >= input$thr
      d$flagged[is.na(d$flagged)] <- FALSE
    }
    d
  })

  cur <- reactive({
    a  <- all_d(); cd <- as.numeric(input$cutoff)
    d  <- a[which(a$cutoff_day == cd), ]
    earlier <- sort(unique(a$cutoff_day[a$cutoff_day < cd]))
    tab <- table(a$id_student[which(a$cutoff_day %in% earlier & a$flagged)])
    cnt <- as.vector(tab); names(cnt) <- names(tab)
    n_prev <- cnt[match(as.character(d$id_student), names(cnt))]
    n_prev[is.na(n_prev)] <- 0
    d$status <- ifelse(!d$flagged, "Not flagged",
                  ifelse(n_prev == 0, "New",
                    ifelse(n_prev == length(earlier), "Persistent", "Returning")))
    if (input$test_only) d <- d[which(d$split == "test"), ]
    validate(need(nrow(d) > 0, "No students at this cutoff."))
    d[order(-d$risk_score), ]
  })
  shown <- reactive({
    d <- cur()
    switch(input$focus,
           flagged = d[which(d$flagged), ],
           new     = d[which(d$status == "New"), ],
           persist = d[which(d$status == "Persistent"), ],
           d)
  })

  # ---- summary cards -------------------------------------------------------
  output$cards <- renderUI({
    d <- cur(); f <- which(d$flagged); a <- d$actual
    prec <- if (length(f)) mean(a[f], na.rm = TRUE) else NA
    rec  <- sum(a & d$flagged, na.rm = TRUE) / max(sum(a, na.rm = TRUE), 1)
    layout_columns(fill = FALSE,
      value_box("Students", format(nrow(d), big.mark = ","), theme = "primary"),
      value_box("Flagged", format(length(f), big.mark = ","),
                p(class = "mb-0", pct(length(f) / nrow(d)), " of students"), theme = "danger"),
      value_box("Flag precision", pct(prec),
                p(class = "mb-0", "of flagged were really at risk"), theme = "secondary"),
      value_box("At-risk caught", pct(rec),
                p(class = "mb-0", "of all at-risk students flagged"), theme = "success"))
  })

  # ---- student table -------------------------------------------------------
  output$tbl <- renderDT({
    d <- shown(); validate(need(nrow(d) > 0, "No students match these filters."))
    out <- data.frame(
      Student = d$id_student,
      Risk    = round(d$risk_score, 2),
      Band    = ifelse(d$risk_score >= 0.7, "High", ifelse(d$flagged, "Medium", "Low")),
      Status  = d$status,
      Reasons = vapply(d$reasons, first_reason, ""),
      `Actual outcome` = ifelse(d$actual %in% TRUE, "At risk", "OK"),
      check.names = FALSE)
    datatable(out, selection = "single", rownames = FALSE,
              options = list(pageLength = 12, dom = "frtip")) |>
      formatStyle("Band", color = styleEqual(c("High", "Medium", "Low"), c(RED, AMBER, "#555")),
                  fontWeight = "bold")
  })

  # ---- selected student ----------------------------------------------------
  sel <- reactive({
    i <- input$tbl_rows_selected; req(i)
    d <- shown(); req(i <= nrow(d))
    d[i, ]
  })

  output$detail <- renderUI({
    if (!length(input$tbl_rows_selected))
      return(card(card_header("Why was this student flagged?"),
                  p(class = "text-muted", "Click a student in the table to see the reasons,
                    how their risk changed over time, and what drove the score.")))
    r <- sel(); rs <- r$reasons[[1]]
    co <- cur(); h <- all_d(); h <- h[which(h$id_student == r$id_student), ]
    h <- h[order(h$cutoff_day), ]
    rank   <- sum(co$risk_score > r$risk_score, na.rm = TRUE) + 1
    change <- if (nrow(h) > 1 && r$cutoff_day > h$cutoff_day[1])
      sprintf("%+.2f", r$risk_score - h$risk_score[1]) else "n/a"
    card(
      card_header(sprintf("Student %s: risk %.2f at day %s (%s)", r$id_student, r$risk_score, r$cutoff_day, r$status)),
      div(class = "d-flex justify-content-around border rounded py-2 mb-3",
          stat_box("Rank by risk", sprintf("%d of %d", rank, nrow(co))),
          stat_box("Checkpoints flagged", sprintf("%d of %d", sum(h$flagged), nrow(h))),
          stat_box(sprintf("Change since day %s", h$cutoff_day[1]), change)),
      if (is.data.frame(rs) && nrow(rs))
        tags$ul(lapply(seq_len(nrow(rs)), function(k)
          # no stray space before the colon, and capitalised theme name
          tags$li(strong(paste0(tools::toTitleCase(rs$theme[k]), ":")), " ", rs$text[k])))
      else p("No behavioural warning signs."),
      if (isTRUE(r$bg_driven))
        div(class = "alert alert-warning py-2",
            "This score is driven more by background factors than by behaviour.
             Treat the flag with caution and do not act on background alone."),
      plotOutput("trend", height = 190),
      plotOutput("impact", height = 170))
  })

  output$trend <- renderPlot({
    r <- sel(); h <- all_d(); h <- h[which(h$id_student == r$id_student), ]
    h <- h[order(h$cutoff_day), ]
    h$State <- ifelse(h$flagged, "Flagged", "Not flagged")
    ggplot(h, aes(cutoff_day, risk_score)) +
      geom_line(colour = GREY, linewidth = 1) +
      geom_point(aes(colour = State), size = 4) +
      scale_colour_manual(values = c(Flagged = RED, `Not flagged` = BLUE), name = NULL) +
      scale_x_continuous(breaks = c(28, 56, 84)) + ylim(0, 1) +
      labs(x = "Day", y = "Risk score", title = "Risk over time") + th
  })

  output$impact <- renderPlot({
    r <- sel()
    v <- data.frame(theme = c("Engagement", "Assessments", "Background"),
                    push  = c(r$eng, r$ass, r$bg))
    v$kind <- ifelse(v$theme == "Background", "Background (not a reason)",
                     ifelse(v$push > 0, "Raises risk", "Lowers risk"))
    v$theme <- factor(v$theme, levels = rev(v$theme))
    ggplot(v, aes(push, theme, fill = kind)) + geom_col(width = 0.55) +
      geom_vline(xintercept = 0, colour = "#444") +
      scale_fill_manual(values = c(`Raises risk` = RED, `Lowers risk` = BLUE,
                                   `Background (not a reason)` = GREY), name = NULL) +
      labs(x = "Push toward at-risk (right) or away from it (left)", y = NULL,
           title = "What drove the score") + th
  })

  # ---- overview charts -----------------------------------------------------
  output$dist <- renderPlot({
    d <- cur()
    d$Outcome <- ifelse(d$actual %in% TRUE, "Actually at risk", "Not at risk")
    p <- ggplot(d, aes(risk_score, fill = Outcome)) +
      geom_histogram(bins = 30, boundary = 0, colour = "white") +
      scale_fill_manual(values = c(`Actually at risk` = RED, `Not at risk` = BLUE), name = NULL) +
      labs(x = "Risk score", y = "Students") + th
    if (any(d$flagged)) {
      thr <- min(d$risk_score[which(d$flagged)])
      p <- p + geom_vline(xintercept = thr, linetype = "dashed") +
        annotate("text", x = thr, y = Inf, label = " flag line", hjust = 0, vjust = 1.5)
    }
    p
  })

  output$weeks <- renderPlot({
    d <- all_d()
    long <- do.call(rbind, lapply(split(d, d$cutoff_day), function(x)
      data.frame(cutoff = x$cutoff_day[1],
                 group = c("Flagged and at risk", "Flagged but OK", "At risk but missed"),
                 n = c(sum(x$flagged & x$actual, na.rm = TRUE),
                       sum(x$flagged & !x$actual, na.rm = TRUE),
                       sum(!x$flagged & x$actual, na.rm = TRUE)))))
    long$cutoff <- factor(paste("Day", long$cutoff),
                          levels = paste("Day", sort(unique(long$cutoff))))
    ggplot(long, aes(cutoff, n, fill = group)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c(`Flagged and at risk` = BLUE, `Flagged but OK` = AMBER,
                                   `At risk but missed` = RED), name = NULL) +
      labs(x = NULL, y = "Students") + th
  })

  # ---- fairness ------------------------------------------------------------
  fair <- reactive({
    d <- cur(); d[which(d$flagged & !is.na(d$bg_driven)), ]
  })

  output$fair_txt <- renderUI({
    f <- fair(); validate(need(nrow(f) > 0, "No flagged students at this cutoff."))
    b <- f[f$bg_driven, ]; o <- f[!f$bg_driven, ]
    div(class = "alert alert-info",
        sprintf("%d of %d flagged students (%s) are driven more by background than by behaviour.
                 Of those, %s were really at risk, versus %s of behaviour-driven flags.",
                nrow(b), nrow(f), pct(nrow(b) / nrow(f)),
                pct(mean(b$actual, na.rm = TRUE)), pct(mean(o$actual, na.rm = TRUE))))
  })

  output$fair_plot <- renderPlot({
    f <- fair(); req(nrow(f) > 0)
    f$Outcome <- ifelse(f$actual %in% TRUE, "Actually at risk", "Not at risk")
    ggplot(f, aes(beh, bg, colour = Outcome)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_point(alpha = 0.6, size = 2) +
      scale_colour_manual(values = c(`Actually at risk` = RED, `Not at risk` = BLUE), name = NULL) +
      labs(x = "Behaviour push (engagement + assessments)", y = "Background push") + th
  })

  # ---- model performance ---------------------------------------------------
  get_mets <- reactive({
    m <- mets$find(fields = '{"_id":0}')
    if ("model" %in% names(m)) m$model <- as.character(m$model)
    m
  })

  output$perf <- renderPlot({
    m <- get_mets()
    ggplot(m, aes(cutoff, auc, colour = model)) +
      geom_line(linewidth = 1) + geom_point(size = 3) +
      scale_x_continuous(breaks = c(28, 56, 84)) +
      scale_colour_manual(values = c(BLUE, RED, AMBER, GREY)[seq_along(unique(m$model))], name = NULL) +
      labs(x = "Cutoff day", y = "AUC on test students") + th
  })
  output$perf_tbl <- renderTable({
    m <- get_mets(); m[] <- lapply(m, function(x) if (is.numeric(x)) round(x, 3) else x); m
  })

  # ---- export --------------------------------------------------------------
  output$dl <- downloadHandler(
    filename = function() sprintf("flagged_%s_%s_day%s.csv", input$module, input$pres, input$cutoff),
    content = function(file) {
      d <- shown()
      write.csv(data.frame(
        student = d$id_student, risk = round(d$risk_score, 3), flagged = d$flagged,
        status = d$status,
        reasons = vapply(d$reasons, first_reason, ""),
        background_driven = d$bg_driven,
        actual_outcome = ifelse(d$actual %in% TRUE, "At risk", "OK")),
        file, row.names = FALSE)
    })
}

shinyApp(ui, server)