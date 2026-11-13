library(shiny); library(bslib); library(DT); library(ggplot2)

DATA_MODE <- tolower(Sys.getenv("EWS_DATA_MODE", "mongo"))
if (!DATA_MODE %in% c("mongo", "preview"))
  stop("EWS_DATA_MODE must be either 'mongo' (default) or 'preview'.", call. = FALSE)

PROJECT_DIR <- if (basename(normalizePath(getwd())) == "app") ".." else "."
PREVIEW_RECORDS <- file.path(PROJECT_DIR, "data", "processed", "records.jsonl")
PREVIEW_METRICS <- file.path(PROJECT_DIR, "reports", "results_by_cutoff.csv")
REQUIRED_PREDICTION_FIELDS <- c(
  "id_student", "module", "presentation", "cutoff_day", "split", "risk_score",
  "flagged", "actual_at_risk", "theme_impacts", "reasons"
)
REQUIRED_PREDICTION_COLUMNS <- c(
  "id_student", "module", "presentation", "cutoff_day", "split", "risk_score",
  "flagged", "actual_at_risk", "reasons", "theme_impacts.engagement",
  "theme_impacts.assessments", "theme_impacts.background"
)

require_fields <- function(d, fields, source) {
  missing <- setdiff(fields, names(d))
  validate(need(!length(missing), sprintf(
    "%s is incompatible: missing required field(s): %s.", source, paste(missing, collapse = ", ")
  )))
  d
}

read_preview_records <- function(path) {
  validate(need(file.exists(path), sprintf(
    "Preview data is unavailable: expected exported historical records at '%s'.", path
  )))
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  validate(need(length(lines), sprintf("Preview data file '%s' is empty.", path)))
  records <- tryCatch(
    lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE),
    error = function(error) {
      validate(need(FALSE, sprintf("Preview data file '%s' is not valid JSON Lines: %s", path, error$message)))
    }
  )
  missing_by_record <- lapply(records, function(record) setdiff(REQUIRED_PREDICTION_FIELDS, names(record)))
  invalid_record <- which(lengths(missing_by_record) > 0)
  validate(need(!length(invalid_record), sprintf(
    "Preview data file '%s' is incompatible: record %d is missing required field(s): %s.",
    path, invalid_record[1], paste(missing_by_record[[invalid_record[1]]], collapse = ", ")
  )))
  bad_records <- which(!vapply(records, function(record)
    is.list(record$theme_impacts) &&
      all(c("engagement", "assessments", "background") %in% names(record$theme_impacts)) &&
      is.list(record$reasons), logical(1)))
  validate(need(!length(bad_records), sprintf(
    "Preview data file '%s' is incompatible: theme_impacts must include engagement, assessments and background, and reasons must be an array (first invalid record: %d).",
    path, bad_records[1]
  )))
  bad_reason <- which(!vapply(records, function(record) all(vapply(record$reasons, function(reason)
    is.list(reason) && all(c("theme", "impact", "text") %in% names(reason)), logical(1))), logical(1)))
  validate(need(!length(bad_reason), sprintf(
    "Preview data file '%s' is incompatible: each reason must include theme, impact and text (first invalid record: %d).",
    path, bad_reason[1]
  )))
  d <- data.frame(
    id_student = vapply(records, `[[`, numeric(1), "id_student"),
    module = vapply(records, `[[`, character(1), "module"),
    presentation = vapply(records, `[[`, character(1), "presentation"),
    cutoff_day = vapply(records, `[[`, numeric(1), "cutoff_day"),
    split = vapply(records, `[[`, character(1), "split"),
    risk_score = vapply(records, `[[`, numeric(1), "risk_score"),
    flagged = vapply(records, `[[`, logical(1), "flagged"),
    actual_at_risk = vapply(records, `[[`, numeric(1), "actual_at_risk")
  )
  d$theme_impacts <- I(lapply(records, `[[`, "theme_impacts"))
  d$reasons <- I(lapply(records, function(record) {
    if (!length(record$reasons)) return(data.frame())
    do.call(rbind, lapply(record$reasons, as.data.frame, stringsAsFactors = FALSE))
  }))
  for (theme in c("engagement", "assessments", "background"))
    d[[paste0("theme_impacts.", theme)]] <- vapply(d$theme_impacts, function(x)
      if (is.null(x[[theme]])) NA_real_ else as.numeric(x[[theme]]), numeric(1))
  d
}

if (DATA_MODE == "mongo") {
  if (!requireNamespace("mongolite", quietly = TRUE))
    stop("MongoDB mode requires the R package 'mongolite'. Install it or launch with EWS_DATA_MODE=preview.", call. = FALSE)
  URL  <- "mongodb://localhost:27017"
  pred <- mongolite::mongo("predictions", db = "oulad_ews", url = URL)
  mets <- mongolite::mongo("model_metrics", db = "oulad_ews", url = URL)
} else if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Preview mode requires the R package 'jsonlite'.", call. = FALSE)
}
FIELDS <- '{"_id":0}'   # fetch every field except _id


# ---- palette (charts follow the active light/dark theme) --------------------
CHART_PAL <- list(
  light = list(ink = "#17212B", muted = "#64748B", grid = "#E3E9E6", surface = "#FFFFFF",
               at_risk = "#C2503F", ok = "#0F766E", gold = "#C49A45", neutral = "#94A3B8",
               deep = "#115E59", slate = "#475569"),
  dark  = list(ink = "#E7EEEC", muted = "#9AAAA7", grid = "#27322F", surface = "#151C1B",
               at_risk = "#EA8272", ok = "#4DB8AA", gold = "#D9B566", neutral = "#6B7B78",
               deep = "#2F8F84", slate = "#A8B6B3")
)

# ---- helpers ---------------------------------------------------------------
num_col <- function(d, nm) if (nm %in% names(d)) as.numeric(d[[nm]]) else rep(NA_real_, nrow(d))
pct     <- function(x) if (length(x) == 0 || is.na(x)) "n/a" else sprintf("%.0f%%", 100 * x)
week_label <- function(day) sprintf("Week %d", as.integer(round(as.numeric(day) / 7)))
# show every reason (not just the first) so rows no longer look identical
# (used by the CSV export, whose wording is deliberately left unchanged)
first_reason <- function(r) {
  if (is.data.frame(r) && nrow(r) && "text" %in% names(r))
    paste0(tools::toTitleCase(as.character(r$theme)), ": ", as.character(r$text), collapse = "  |  ")
  else "No behavioural warning signs"
}

# inline SVG icons (outline style, inherit currentColor) - no icon library needed
ICONS <- list(
  students = '<path d="M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2"/><circle cx="9" cy="7" r="4"/><path d="M22 21v-2a4 4 0 0 0-3-3.87"/><path d="M16 3.13a4 4 0 0 1 0 7.75"/>',
  overview = '<rect width="7" height="9" x="3" y="3" rx="1"/><rect width="7" height="5" x="14" y="3" rx="1"/><rect width="7" height="9" x="14" y="12" rx="1"/><rect width="7" height="5" x="3" y="16" rx="1"/>',
  fairness = '<path d="m16 16 3-8 3 8c-.87.65-1.92 1-3 1s-2.13-.35-3-1Z"/><path d="m2 16 3-8 3 8c-.87.65-1.92 1-3 1s-2.13-.35-3-1Z"/><path d="M7 21h10"/><path d="M12 3v18"/><path d="M3 7h2c2 0 5-1 7-2 2 1 5 2 7 2h2"/>',
  accuracy = '<circle cx="12" cy="12" r="10"/><circle cx="12" cy="12" r="6"/><circle cx="12" cy="12" r="2"/>',
  sun      = '<circle cx="12" cy="12" r="4"/><path d="M12 2v2"/><path d="M12 20v2"/><path d="m4.93 4.93 1.41 1.41"/><path d="m17.66 17.66 1.41 1.41"/><path d="M2 12h2"/><path d="M20 12h2"/><path d="m6.34 17.66-1.41 1.41"/><path d="m19.07 4.93-1.41 1.41"/>',
  moon     = '<path d="M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z"/>',
  download = '<path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><path d="m7 10 5 5 5-5"/><path d="M12 15V3"/>',
  info     = '<circle cx="12" cy="12" r="10"/><path d="M12 16v-4"/><path d="M12 8h.01"/>',
  lightbulb = '<path d="M15 14c.2-1 .7-1.7 1.5-2.5 1-.9 1.5-2.2 1.5-3.5A6 6 0 0 0 6 8c0 1 .2 2.2 1.5 3.5.7.7 1.3 1.5 1.5 2.5"/><path d="M9 18h6"/><path d="M10 22h4"/>',
  help     = '<circle cx="12" cy="12" r="10"/><path d="M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3"/><path d="M12 17h.01"/>',
  close    = '<path d="M18 6 6 18"/><path d="m6 6 12 12"/>',
  inbox    = '<polyline points="22 12 16 12 14 15 10 15 8 12 2 12"/><path d="M5.45 5.11 2 12v6a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-6l-3.45-6.89A2 2 0 0 0 16.76 4H7.24a2 2 0 0 0-1.79 1.11z"/>'
)
svg_icon <- function(name, size = 18, stroke = 1.75) HTML(sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="%s" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false">%s</svg>',
  as.integer(size), as.integer(size), stroke, ICONS[[name]]))

# Clarity mark: an open "C" ring with a single focal point in its opening
LOGO_SVG <- paste0(
  '<svg class="brand-logo" viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Clarity">',
  '<rect class="logo-bg" width="32" height="32" rx="9"/>',
  '<path class="logo-arc" d="M19.75 9.5A7.5 7.5 0 1 0 23.05 18.57" fill="none" stroke-width="2.6" stroke-linecap="round"/>',
  '<circle class="logo-dot" cx="23.05" cy="13.43" r="2.2"/></svg>')
FAVICON <- paste0("data:image/svg+xml,", utils::URLencode(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32"><rect width="32" height="32" rx="9" fill="#0F766E"/>',
  '<path d="M19.75 9.5A7.5 7.5 0 1 0 23.05 18.57" fill="none" stroke="#fff" stroke-width="2.6" stroke-linecap="round"/>',
  '<circle cx="23.05" cy="13.43" r="2.2" fill="#E3BE6C"/></svg>'), reserved = TRUE))

pill <- function(text, class = "pill-neutral") span(class = paste("pill", class), text)
# small "i" marker that shows a plain-English explanation on hover or keyboard focus
info_tip <- function(text) tags$span(class = "info-tip", tabindex = "0", role = "img",
  `aria-label` = text, `data-tip` = text, svg_icon("info", 14, 2))
nav_button <- function(id, icon, label) actionButton(id,
  tagList(span(class = "nav-icon", svg_icon(icon, 18)), span(class = "nav-label", label)),
  class = "rail-link", title = label, `aria-label` = label)
reason_chips <- function(r) {
  if (!is.data.frame(r) || !nrow(r) || !"theme" %in% names(r)) return(span(class = "muted-text", "None noted"))
  tags$div(class = "reason-chips", lapply(head(unique(as.character(r$theme)), 2), function(theme)
    pill(tools::toTitleCase(theme), "pill-chip")))
}
risk_bar <- function(value, band = "low") sprintf(
  '<div class="risk-cell risk-%s" role="img" aria-label="Estimated risk %.2f"><span>%.2f</span><div class="risk-track"><i style="width:%d%%"></i></div></div>',
  band, value, value, as.integer(round(100 * value))
)
STATUS_LABEL <- c(New = "New", Persistent = "Ongoing", Returning = "Returning", `Not flagged` = "No alert")
STATUS_CLASS <- c(New = "pill-new", Persistent = "pill-ongoing", Returning = "pill-returning", `Not flagged` = "pill-none")
status_pill <- function(x) pill(if (x %in% names(STATUS_LABEL)) STATUS_LABEL[[x]] else x,
                                if (x %in% names(STATUS_CLASS)) STATUS_CLASS[[x]] else "pill-none")
# `good_up`: TRUE when an increase is good news, NA when direction carries no judgement
delta_chip <- function(current, previous, is_percent = FALSE, good_up = NA, first = FALSE) {
  if (first)             return(span(class = "delta delta-flat", "First checkpoint"))
  if (is.na(previous) || is.na(current)) return(span(class = "delta delta-flat", "No comparison"))
  change <- current - previous
  dir  <- if (round(if (is_percent) 100 * change else change) == 0) "flat" else if (change > 0) "up" else "down"
  text <- if (dir == "flat") "No change" else if (is_percent) sprintf("%+.0f pts", 100 * change) else sprintf("%+d", as.integer(round(change)))
  tone <- if (dir == "flat" || is.na(good_up)) "neutral" else if ((dir == "up") == good_up) "good" else "watch"
  span(class = paste("delta", paste0("delta-", dir), paste0("delta-", tone)), text)
}
plot_theme <- function(p) theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        panel.grid.major = element_line(colour = p$grid, linewidth = 0.4),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.title = element_text(face = "bold", colour = p$ink, size = 14),
        legend.text = element_text(colour = p$ink), legend.title = element_blank(),
        axis.title = element_text(colour = p$muted), axis.text = element_text(colour = p$muted))
week_axis <- function(x) paste("Week", as.integer(round(x / 7)))

# Runs in <head> before first paint so the page never flashes the wrong theme.
THEME_BOOT <- r"---(
(function () {
  var d = document.documentElement, t = null;
  try { t = localStorage.getItem('clarity-theme'); } catch (e) {}
  if (t !== 'light' && t !== 'dark')
    t = (window.matchMedia && matchMedia('(prefers-color-scheme: dark)').matches) ? 'dark' : 'light';
  d.setAttribute('data-theme', t); d.setAttribute('data-bs-theme', t);
})();
)---"

APP_CSS <- r"---(
/* =========================================================
   Clarity - design system
   Tokens (light + dark) -> base -> sidebar -> header -> controls
   -> cards -> table -> drawer -> charts -> responsive
   ========================================================= */
:root {
  color-scheme: light;
  --teal: #0F766E;  --teal-deep: #115E59;  --teal-soft: #F0FDFA;
  --gold: #C49A45;  --gold-soft: #FBF6E9;  --gold-ink: #7A5A14;
  --bg: #F8FAF9;  --surface: #FFFFFF;  --surface-2: #F3F6F5;
  --line: #E3E9E6;  --line-2: #D2DBD7;
  --ink: #17212B;  --ink-2: #3C4957;  --muted: #64748B;
  --accent-text: #0F766E;  --accent-soft: #F0FDFA;  --accent-line: #B9E3DD;
  --btn: #0F766E;  --btn-hover: #115E59;
  --high: #B4382F;  --high-soft: #FCEDEB;  --high-bar: #C2503F;
  --med: #96600B;   --med-soft: #FFF2D9;   --med-bar: #D49A2A;
  --low-bar: #9AA8A4;  --good: #0F766E;  --good-soft: #E6F6F3;
  --live: #2F9E6B;
  --shadow-sm: 0 1px 2px rgba(23,33,43,.05);
  --shadow-pop: 0 10px 30px rgba(23,33,43,.12), 0 1px 3px rgba(23,33,43,.08);
  --r-lg: 14px;  --r-md: 10px;  --r-sm: 8px;
  --ease: cubic-bezier(.2,.7,.2,1);
  --font: Inter, system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
  --rail-w: 236px;
}
:root[data-theme="dark"] {
  color-scheme: dark;
  --teal-soft: rgba(94,220,203,.10);  --gold: #D9B566;  --gold-soft: rgba(217,181,102,.10);  --gold-ink: #E5C985;
  --bg: #0E1413;  --surface: #151C1B;  --surface-2: #1A2322;
  --line: #25302E;  --line-2: #33403D;
  --ink: #E7EEEC;  --ink-2: #C3CFCC;  --muted: #94A4A1;
  --accent-text: #5EDCCB;  --accent-soft: rgba(94,220,203,.10);  --accent-line: rgba(94,220,203,.28);
  --btn: #0F766E;  --btn-hover: #14877D;
  --high: #F0897D;  --high-soft: rgba(240,137,125,.12);  --high-bar: #EA8272;
  --med: #E6B655;   --med-soft: rgba(230,182,85,.12);   --med-bar: #D9A844;
  --low-bar: #5C6B68;  --good: #5EDCCB;  --good-soft: rgba(94,220,203,.12);
  --live: #4FBF8B;
  --shadow-sm: 0 1px 2px rgba(0,0,0,.3);
  --shadow-pop: 0 12px 32px rgba(0,0,0,.5), 0 1px 3px rgba(0,0,0,.4);
}
:root[data-theme] {
  --bs-body-bg: var(--bg);  --bs-body-color: var(--ink);  --bs-border-color: var(--line);
  --bs-body-font-family: var(--font);  --bs-secondary-color: var(--muted);  --bs-emphasis-color: var(--ink);
  --bs-tertiary-bg: var(--surface-2);  --bs-link-color: var(--accent-text);
}

/* ---------- base ---------- */
html, body { background: var(--bg); }
body {
  color: var(--ink); font-family: var(--font); font-size: 14px; line-height: 1.55;
  -webkit-font-smoothing: antialiased; padding: 0 !important; margin: 0; overflow-x: hidden;
}
h1, h2, h3, h4, h5, h6 { font-family: var(--font); color: var(--ink); letter-spacing: -.015em; }
.text-muted, .muted-text { color: var(--muted) !important; }
.muted-text { font-weight: 500; }
:where(button, a, input, select, .selectize-input, [tabindex]):focus-visible { outline: 2px solid var(--accent-text); outline-offset: 2px; }
.skip-link { position: absolute; left: 12px; top: -48px; z-index: 100; padding: 8px 14px; border-radius: var(--r-sm); background: var(--btn); color: #fff; font-weight: 600; text-decoration: none; transition: top .15s var(--ease); }
.skip-link:focus { top: 12px; color: #fff; }
html.theme-fade *, html.theme-fade *::before, html.theme-fade *::after {
  transition: background-color .28s ease, border-color .28s ease, color .28s ease, fill .28s ease, stroke .28s ease !important;
}
.recalculating { opacity: .55 !important; transition: opacity .15s ease !important; }
.tab-pane { padding: 0 !important; }
.tab-pane.active { animation: tab-in .22s ease; }

/* empty / validation states */
.shiny-output-error-validation {
  display: flex; flex-direction: column; align-items: center; gap: 6px; padding: 44px 24px;
  color: var(--muted); font-weight: 500; font-size: 14px; text-align: center;
}
.shiny-output-error-validation::before {
  content: ''; width: 40px; height: 40px; margin-bottom: 6px; border-radius: 12px; background: var(--surface-2);
  -webkit-mask: none; border: 1px dashed var(--line-2);
}
.card .shiny-output-error-validation, .table-card .shiny-output-error-validation { min-height: 160px; justify-content: center; }

/* ---------- surfaces ---------- */
.card, .surface {
  background: var(--surface) !important; color: var(--ink); border: 1px solid var(--line) !important;
  border-radius: var(--r-lg) !important; box-shadow: var(--shadow-sm) !important;
}
.card-header {
  background: transparent !important; border-bottom: 1px solid var(--line) !important; border-radius: var(--r-lg) var(--r-lg) 0 0 !important;
  padding: 16px 20px !important; font-size: 14px; font-weight: 600; color: var(--ink); letter-spacing: -.005em;
}
.card-body { padding: 16px 20px 20px; }
.card-caption { margin: 0; padding: 12px 20px 0; font-size: 13px; color: var(--muted); }
.page-intro { max-width: 68ch; margin: 0 0 20px; color: var(--muted); font-size: 14px; }

/* ---------- sidebar ---------- */
.app-shell { min-height: 100vh; display: flex; }
.app-rail {
  position: fixed; inset: 0 auto 0 0; z-index: 30; width: var(--rail-w); padding: 22px 14px 16px;
  display: flex; flex-direction: column; gap: 2px; background: var(--surface); border-right: 1px solid var(--line);
}
.brand { display: flex; align-items: center; gap: 11px; padding: 2px 8px 24px; }
.brand-logo { flex: none; width: 34px; height: 34px; display: block; }
.logo-bg { fill: #0F766E; }  .logo-arc { stroke: #fff; }  .logo-dot { fill: #E3BE6C; }
:root[data-theme="dark"] .logo-bg { fill: #14877D; }
.brand-name { font-size: 16px; font-weight: 650; letter-spacing: -.01em; line-height: 1.15; color: var(--ink); }
.brand-tag { margin-top: 2px; font-size: 11.5px; line-height: 1.3; color: var(--muted); text-wrap: balance; }
.rail-section { padding: 0 10px 8px; font-size: 11.5px; font-weight: 600; letter-spacing: .02em; color: var(--muted); }
.app-rail .rail-link {
  --bs-btn-bg: transparent; --bs-btn-border-color: transparent; --bs-btn-color: var(--ink-2);
  --bs-btn-hover-bg: var(--surface-2); --bs-btn-hover-border-color: transparent; --bs-btn-hover-color: var(--ink);
  --bs-btn-active-bg: var(--surface-2); --bs-btn-active-border-color: transparent; --bs-btn-active-color: var(--ink);
  position: relative; display: flex; align-items: center; gap: 12px; width: 100%; margin: 0; padding: 8px 10px; text-align: left;
  border: 0; border-radius: var(--r-md); background: transparent; color: var(--ink-2); box-shadow: none;
  font-size: 14px; font-weight: 500;
  transition: background-color .18s var(--ease), color .18s var(--ease);
}
.app-rail .rail-link:hover { background: var(--surface-2); color: var(--ink); }
.app-rail .rail-link.active { background: var(--accent-soft); color: var(--accent-text); font-weight: 600; }
.app-rail .rail-link.active::before {
  content: ''; position: absolute; left: 0; top: 9px; bottom: 9px; width: 3px; border-radius: 3px; background: var(--accent-text);
}
.nav-icon { display: grid; place-items: center; flex: none; width: 22px; height: 22px; color: var(--muted); transition: color .18s var(--ease); }
.rail-link:hover .nav-icon { color: var(--ink-2); }
.rail-link.active .nav-icon { color: var(--accent-text); }
.rail-foot { margin-top: auto; padding-top: 14px; border-top: 1px solid var(--line); display: flex; flex-direction: column; gap: 10px; }
.rail-status { display: flex; align-items: flex-start; gap: 10px; padding: 0 8px; font-size: 12px; line-height: 1.4; color: var(--muted); }
.rail-status strong { display: block; color: var(--ink-2); font-size: 12.5px; font-weight: 600; }
.status-dot { flex: none; width: 8px; height: 8px; margin-top: 5px; border-radius: 50%; background: var(--live); }
.status-dot-preview { background: var(--gold); }
.theme-row { display: flex; align-items: center; justify-content: space-between; gap: 10px; padding: 0 8px; font-size: 13px; font-weight: 500; color: var(--ink-2); }
.theme-toggle {
  position: relative; flex: none; width: 52px; height: 28px; padding: 0; cursor: pointer; border-radius: 999px;
  border: 1px solid var(--line-2); background: var(--surface-2); transition: background-color .25s ease, border-color .25s ease;
}
.theme-toggle:hover { border-color: var(--muted); }
.tt-knob {
  position: absolute; top: 2px; left: 2px; width: 22px; height: 22px; border-radius: 50%; display: grid; place-items: center;
  background: var(--surface); color: var(--gold-ink); box-shadow: 0 1px 3px rgba(0,0,0,.22), 0 0 0 1px var(--line-2);
  transition: transform .28s var(--ease), color .25s ease;
}
.tt-knob > span { position: absolute; inset: 0; display: grid; place-items: center; transition: opacity .2s ease, transform .3s var(--ease); }
.tt-knob svg { width: 13px; height: 13px; display: block; }
.tt-knob .tt-moon { opacity: 0; transform: rotate(-40deg) scale(.7); }
:root[data-theme="dark"] .tt-knob { transform: translateX(24px); color: var(--accent-text); }
:root[data-theme="dark"] .tt-knob .tt-sun { opacity: 0; transform: rotate(40deg) scale(.7); }
:root[data-theme="dark"] .tt-knob .tt-moon { opacity: 1; transform: none; }

/* ---------- main / header ---------- */
.app-main { position: relative; min-width: 0; margin-left: var(--rail-w); width: calc(100% - var(--rail-w)); padding: 36px 44px 72px; }
.app-main:focus { outline: none; }
.main-inner { max-width: 1240px; margin: 0 auto; }
.topbar { display: flex; align-items: flex-end; justify-content: space-between; gap: 16px 24px; flex-wrap: wrap; margin-bottom: 24px; }
.page-title { margin: 0; font-size: clamp(24px, 2.6vw, 30px); font-weight: 650; letter-spacing: -.025em; line-height: 1.15; }
.page-sub { margin: 6px 0 0; font-size: 14px; color: var(--muted); }
.page-sub strong { color: var(--ink-2); font-weight: 600; }
.note { margin: 0 0 16px; font-size: 13px; color: var(--muted); }

/* ---------- controls ---------- */
.controls { display: flex; flex-direction: column; gap: 16px; padding: 16px 20px; margin-bottom: 24px; }
.ctrl-group { display: flex; flex-wrap: wrap; align-items: flex-end; gap: 14px 20px; }
.ctrl-group + .ctrl-group { align-items: center; padding-top: 16px; border-top: 1px solid var(--line); }
.controls .shiny-input-container, .table-head .shiny-input-container { margin: 0; width: auto; }
.controls label.control-label, .table-head label.control-label, .ctrl-label {
  display: flex; align-items: center; gap: 6px; margin-bottom: 6px; font-size: 12.5px; font-weight: 500; color: var(--muted);
}
.controls .ctrl-select .shiny-input-container { width: 168px; }
.table-head .ctrl-select .shiny-input-container { width: 250px; }
.selectize-input {
  min-height: 38px; padding: 8px 12px !important; border: 1px solid var(--line-2) !important; border-radius: var(--r-md) !important;
  background: var(--surface) !important; box-shadow: none !important; color: var(--ink) !important; font-size: 14px; font-weight: 500;
  transition: border-color .18s var(--ease), box-shadow .18s var(--ease);
}
.selectize-input > input, .selectize-input .item { color: var(--ink) !important; }
.selectize-input:hover { border-color: var(--muted) !important; }
.selectize-input.focus, .selectize-input.dropdown-active { border-color: var(--accent-text) !important; box-shadow: 0 0 0 3px var(--accent-soft) !important; }
.selectize-control.single .selectize-input::after { border-color: var(--muted) transparent transparent !important; }
.selectize-control.single .selectize-input.dropdown-active::after { border-color: transparent transparent var(--muted) !important; }
.selectize-dropdown {
  margin-top: 6px; overflow: hidden; border: 1px solid var(--line-2) !important; border-radius: var(--r-md) !important;
  background: var(--surface) !important; color: var(--ink) !important; box-shadow: var(--shadow-pop) !important;
}
.selectize-dropdown .option, .selectize-dropdown .no-results { padding: 8px 12px; font-size: 14px; color: var(--ink-2); }
.selectize-dropdown .active { background: var(--accent-soft) !important; color: var(--accent-text) !important; }
.selectize-dropdown .selected { font-weight: 600; }

/* segmented checkpoint selector */
.week-control .shiny-options-group {
  display: inline-flex; gap: 2px; padding: 3px; border-radius: var(--r-md); background: var(--surface-2); border: 1px solid var(--line);
}
.week-control .shiny-options-group > *, .week-control .shiny-options-group label { margin: 0 !important; padding: 0 !important; }
.week-control .shiny-options-group label { position: relative; cursor: pointer; }
.week-control .shiny-options-group input { position: absolute; opacity: 0; pointer-events: none; }
.week-control .shiny-options-group span {
  display: block; padding: 6px 14px; border-radius: 7px; white-space: nowrap; font-size: 13.5px; font-weight: 500; color: var(--muted);
  transition: background-color .18s var(--ease), color .18s var(--ease), box-shadow .18s var(--ease);
}
.week-control .shiny-options-group label:hover span { color: var(--ink); }
.week-control .shiny-options-group input:checked + span { background: var(--surface); color: var(--accent-text); font-weight: 600; box-shadow: 0 0 0 1px var(--line-2), var(--shadow-sm); }
.week-control .shiny-options-group input:focus-visible + span { outline: 2px solid var(--accent-text); outline-offset: 1px; }

/* switches */
.switch-row .shiny-input-container { width: auto; }
.switch-row .checkbox, .switch-row .form-check { margin: 0; padding: 0; min-height: 0; }
.switch-row label { display: inline-flex; align-items: center; gap: 10px; margin: 0; cursor: pointer; font-size: 14px; font-weight: 500; color: var(--ink-2); }
.switch-row input[type=checkbox] {
  appearance: none; -webkit-appearance: none; flex: none; float: none !important; margin: 0 !important;
  position: relative; width: 34px; height: 20px; border-radius: 999px; cursor: pointer; border: 1px solid var(--line-2);
  background: var(--surface-2); transition: background-color .2s var(--ease), border-color .2s var(--ease);
}
.switch-row input[type=checkbox]::after {
  content: ''; position: absolute; top: 2px; left: 2px; width: 14px; height: 14px; border-radius: 50%; background: var(--surface);
  box-shadow: 0 0 0 1px var(--line-2), 0 1px 2px rgba(0,0,0,.18); transition: transform .2s var(--ease);
}
.switch-row input[type=checkbox]:checked { background: var(--btn); border-color: var(--btn); }
.switch-row input[type=checkbox]:checked::after { transform: translateX(14px); box-shadow: 0 1px 2px rgba(0,0,0,.25); }
.switch-row input[type=checkbox]:focus-visible { outline: 2px solid var(--accent-text); outline-offset: 2px; }
.switch-stack { display: flex; flex-wrap: wrap; align-items: center; gap: 12px 28px; }

/* slider */
.compact-slider { min-width: 260px; }
.compact-slider .irs { margin-top: -4px; }
.irs--shiny .irs-line { background: var(--line); border-color: var(--line-2); }
.irs--shiny .irs-bar { background: var(--btn); border-color: var(--btn); }
.irs--shiny .irs-handle { background: var(--surface); border: 2px solid var(--btn); box-shadow: var(--shadow-sm); }
.irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover { background: var(--accent-soft); }
.irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: var(--btn); color: #fff; }
.irs--shiny .irs-min, .irs--shiny .irs-max { background: var(--surface-2); color: var(--muted); }
.irs--shiny .irs-grid-text { color: var(--muted); }
.irs--shiny .irs-grid-pol { background: var(--line-2); }

/* export / primary button */
.export-btn.btn {
  --bs-btn-bg: var(--btn); --bs-btn-border-color: var(--btn); --bs-btn-color: #fff;
  --bs-btn-hover-bg: var(--btn-hover); --bs-btn-hover-border-color: var(--btn-hover); --bs-btn-hover-color: #fff;
  --bs-btn-active-bg: var(--btn-hover); --bs-btn-active-border-color: var(--btn-hover); --bs-btn-active-color: #fff;
  display: inline-flex; align-items: center; gap: 8px; height: 38px; padding: 0 16px; border-radius: var(--r-md);
  background: var(--btn); border: 1px solid var(--btn); color: #fff; font-size: 14px; font-weight: 600; box-shadow: var(--shadow-sm);
  transition: background-color .18s var(--ease), transform .12s var(--ease), box-shadow .18s var(--ease);
}
.export-btn.btn:hover { background: var(--btn-hover); border-color: var(--btn-hover); color: #fff; }
.export-btn.btn:active { transform: translateY(1px); box-shadow: none; }
.export-btn.btn:focus-visible { outline: 2px solid var(--accent-text); outline-offset: 2px; }

.filter-badges { display: flex; flex-wrap: wrap; align-items: center; gap: 6px; margin-left: auto; }

/* ---------- tooltips ---------- */
.info-tip { display: inline-grid; place-items: center; width: 16px; height: 16px; flex: none; color: var(--muted); cursor: help; border-radius: 50%; vertical-align: -2px; margin-left: 4px; }
.info-tip:hover, .info-tip:focus-visible { color: var(--accent-text); }
.info-tip svg { width: 14px; height: 14px; }
#clarity-tip {
  position: fixed; z-index: 200; max-width: 280px; padding: 9px 12px; border-radius: var(--r-md); pointer-events: none;
  background: var(--ink); color: var(--bg); font-size: 12.5px; font-weight: 450; line-height: 1.45; letter-spacing: 0;
  text-transform: none; text-align: left; box-shadow: var(--shadow-pop); opacity: 0; transform: translateY(3px);
  transition: opacity .14s ease, transform .14s var(--ease); visibility: hidden;
}
#clarity-tip.on { opacity: 1; transform: none; visibility: visible; }

/* ---------- pills ---------- */
.pill { display: inline-flex; align-items: center; gap: 6px; padding: 2px 9px; border-radius: 999px; font-size: 12px; font-weight: 600; line-height: 1.5; white-space: nowrap; }
.pill-dot::before { content: ''; width: 6px; height: 6px; border-radius: 50%; background: currentColor; }
.pill-high { background: var(--high-soft); color: var(--high); }
.pill-medium { background: var(--med-soft); color: var(--med); }
.pill-low { background: var(--surface-2); color: var(--muted); }
.pill-new { background: var(--gold-soft); color: var(--gold-ink); }
.pill-ongoing { background: var(--accent-soft); color: var(--accent-text); }
.pill-returning { background: transparent; color: var(--ink-2); box-shadow: inset 0 0 0 1px var(--line-2); }
.pill-none { background: transparent; color: var(--muted); font-weight: 500; padding-left: 0; }
.pill-chip { background: var(--surface-2); color: var(--ink-2); font-weight: 500; box-shadow: inset 0 0 0 1px var(--line); }
.pill-outcome-risk { background: var(--high-soft); color: var(--high); }
.pill-outcome-ok { background: transparent; color: var(--muted); font-weight: 500; padding-left: 0; }
.pill-neutral { background: var(--surface-2); color: var(--ink-2); box-shadow: inset 0 0 0 1px var(--line); font-weight: 500; }
.pill-accent { background: var(--accent-soft); color: var(--accent-text); }
.pill-goal-met { background: var(--good-soft); color: var(--good); }
.pill-goal-below { background: var(--gold-soft); color: var(--gold-ink); }

/* ---------- KPI cards ---------- */
.kpi-grid { display: grid; grid-template-columns: repeat(6, minmax(0, 1fr)); gap: 16px; margin-bottom: 24px; }
.kpi-grid > .kpi-card { grid-column: span 2; }
.kpi-grid > .kpi-card:nth-child(n+4) { grid-column: span 3; }
.kpi-card { display: flex; flex-direction: column; padding: 18px 20px; min-width: 0; }
.kpi-card:not(.no-intro) { animation: rise .4s var(--ease) backwards; animation-delay: calc(var(--i, 0) * 50ms); }
.kpi-label { display: flex; align-items: center; font-size: 13.5px; font-weight: 500; color: var(--ink-2); }
.kpi-value { margin: 10px 0 2px; font-size: 32px; font-weight: 650; line-height: 1.1; letter-spacing: -.03em; color: var(--ink); font-variant-numeric: tabular-nums; }
.kpi-hint { font-size: 12.5px; line-height: 1.45; color: var(--muted); }
.kpi-foot { display: flex; align-items: center; flex-wrap: wrap; gap: 4px 8px; margin-top: 0; padding-top: 12px; font-size: 12px; color: var(--muted); }
.delta { display: inline-flex; align-items: center; gap: 3px; font-size: 12px; font-weight: 600; font-variant-numeric: tabular-nums; color: var(--muted); }
.delta-up::before { content: '\2191'; }  .delta-down::before { content: '\2193'; }
.delta-good { color: var(--good); }  .delta-watch { color: var(--med); }  .delta-flat { color: var(--muted); font-weight: 500; }
.goal { margin-top: 14px; }
.goal-track { position: relative; height: 6px; border-radius: 99px; background: var(--surface-2); box-shadow: inset 0 0 0 1px var(--line); }
.goal-fill { display: block; height: 100%; border-radius: 99px; background: var(--btn); transform-origin: left; animation: grow .7s var(--ease) backwards; }
.goal-fill.below { background: var(--gold); }
.goal-tick { position: absolute; top: -4px; bottom: -4px; width: 2px; border-radius: 2px; background: var(--ink-2); }
.goal-meta { display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 6px 8px; margin-top: 8px; font-size: 12px; color: var(--muted); }
.goal-meta > span:first-child { white-space: nowrap; }

/* ---------- table card ---------- */
.table-card { overflow: hidden; }
.table-head { display: flex; align-items: flex-end; justify-content: space-between; flex-wrap: wrap; gap: 12px 20px; padding: 18px 20px 14px; }
.table-title { margin: 0; font-size: 16px; font-weight: 600; letter-spacing: -.01em; }
.table-sub { margin: 2px 0 0; font-size: 13px; color: var(--muted); }
.guide {
  display: flex; gap: 14px; margin: 0 20px 8px; padding: 14px 16px; border-radius: var(--r-md);
  background: var(--accent-soft); box-shadow: inset 0 0 0 1px var(--accent-line);
}
.guide-icon { flex: none; display: grid; place-items: center; width: 28px; height: 28px; border-radius: 8px; color: var(--accent-text); background: var(--surface); box-shadow: inset 0 0 0 1px var(--accent-line); }
.guide .guide-detail { margin-top: 8px; font-size: 13px; color: var(--ink-2); }
.guide p { margin: 0; font-size: 13.5px; line-height: 1.55; color: var(--ink-2); }
.guide strong { color: var(--ink); font-weight: 600; }
.guide .guide-lead { display: block; margin-bottom: 2px; font-size: 13.5px; font-weight: 600; color: var(--ink); }
.table-card .html-widget, .table-card .datatables { overflow-x: auto; }
.table-card .dt-container, .table-card .dataTables_wrapper { padding: 0 8px 8px; }
table.dataTable { width: 100% !important; min-width: 720px; border-collapse: separate !important; border-spacing: 0 !important; --bs-table-bg: transparent; --bs-table-color: var(--ink); --bs-table-striped-bg: transparent; --bs-table-hover-bg: transparent; }
table.dataTable thead th {
  padding: 11px 14px !important; font-size: 12px; font-weight: 600; letter-spacing: 0; text-transform: none; color: var(--muted);
  background: transparent; border-top: 1px solid var(--line) !important; border-bottom: 1px solid var(--line) !important;
  white-space: nowrap; vertical-align: middle; position: relative;
}
table.dataTable thead th:first-child { padding-left: 20px !important; }
table.dataTable th, table.dataTable td { text-align: left !important; }
table.dataTable thead > tr > th.sorting, table.dataTable thead > tr > th.sorting_asc, table.dataTable thead > tr > th.sorting_desc {
  background-image: none !important; padding-right: 30px !important; cursor: pointer;
}
table.dataTable thead > tr > th.sorting::before, table.dataTable thead > tr > th.sorting_asc::before, table.dataTable thead > tr > th.sorting_desc::before,
table.dataTable thead > tr > th.sorting::after, table.dataTable thead > tr > th.sorting_asc::after, table.dataTable thead > tr > th.sorting_desc::after {
  position: absolute; right: 12px; bottom: auto; display: block; font-size: 7px; line-height: 1; opacity: .38; color: var(--muted);
}
table.dataTable thead > tr > th.sorting::before, table.dataTable thead > tr > th.sorting_asc::before, table.dataTable thead > tr > th.sorting_desc::before { content: "\25B2" !important; top: calc(50% - 8px); }
table.dataTable thead > tr > th.sorting::after, table.dataTable thead > tr > th.sorting_asc::after, table.dataTable thead > tr > th.sorting_desc::after { content: "\25BC" !important; top: calc(50% + 1px); }
table.dataTable thead > tr > th.sorting_asc::before, table.dataTable thead > tr > th.sorting_desc::after { opacity: 1 !important; color: var(--accent-text) !important; }
table.dataTable thead > tr > th.sorting:hover::before, table.dataTable thead > tr > th.sorting:hover::after { opacity: .7; }
table.dataTable > tbody > tr > * { background-color: transparent; box-shadow: none !important; color: var(--ink); }
table.dataTable tbody td { padding: 13px 14px !important; vertical-align: middle; font-size: 14px; font-weight: 450; border-top: 1px solid var(--line) !important; border-bottom: 0 !important; }
table.dataTable tbody td:first-child { padding-left: 20px !important; font-variant-numeric: tabular-nums; font-weight: 600; letter-spacing: .01em; }
table.dataTable tbody tr:first-child td { border-top: 0 !important; }
table.dataTable tbody tr { cursor: pointer; transition: background-color .14s ease; }
table.dataTable tbody tr:hover > * { background-color: var(--surface-2) !important; }
table.dataTable tbody tr.selected > * { background-color: var(--accent-soft) !important; color: var(--ink) !important; }
table.dataTable tbody tr:focus-visible { outline: 2px solid var(--accent-text); outline-offset: -2px; }
table.dataTable tbody td.dataTables_empty, table.dataTable tbody td.dt-empty { padding: 40px 20px !important; text-align: center; color: var(--muted); font-weight: 500; }
/* sorting indicators (DataTables 1.x and 2.x markup) */
table.dataTable thead > tr > th.sorting::before, table.dataTable thead > tr > th.sorting::after,
table.dataTable thead > tr > th.sorting_asc::before, table.dataTable thead > tr > th.sorting_desc::before,
table.dataTable thead > tr > th.sorting_asc::after, table.dataTable thead > tr > th.sorting_desc::after { color: var(--muted); }
table.dataTable thead > tr > th.sorting_asc::before, table.dataTable thead > tr > th.sorting_desc::after { color: var(--accent-text) !important; opacity: 1 !important; }
table.dataTable thead .dt-column-order { color: var(--muted); }
table.dataTable thead th.dt-orderable-asc:hover, table.dataTable thead th.dt-orderable-desc:hover, table.dataTable thead th.sorting:hover { color: var(--ink); outline: none; }
table.dataTable thead th.dt-ordering-asc .dt-column-order::before, table.dataTable thead th.dt-ordering-desc .dt-column-order::after { color: var(--accent-text); opacity: 1; }
.dataTables_filter, .dt-search { padding: 4px 12px 12px; }
.dt-container .dt-search, .dt-container .dt-layout-row:first-child { margin-bottom: 0; }
.dataTables_filter input, .dt-search input {
  width: 280px; max-width: 100%; margin-left: 0 !important; padding: 8px 12px !important; border: 1px solid var(--line-2) !important; border-radius: var(--r-md) !important;
  background: var(--surface) !important; color: var(--ink) !important; font-size: 14px; outline: none; box-shadow: none;
  transition: border-color .18s var(--ease), box-shadow .18s var(--ease);
}
.dataTables_filter input::placeholder, .dt-search input::placeholder { color: var(--muted); }
.dataTables_filter input:focus, .dt-search input:focus { border-color: var(--accent-text) !important; box-shadow: 0 0 0 3px var(--accent-soft); }
.dataTables_filter label, .dt-search label { display: block; margin: 0; }
.dataTables_wrapper { display: flex; flex-wrap: wrap; align-items: center; }
.dataTables_wrapper > .dataTables_filter { order: 0; flex: 0 0 100%; text-align: left; }
.dataTables_wrapper > table.dataTable { order: 1; flex: 0 0 100%; }
.dataTables_wrapper > .dataTables_info { order: 2; flex: 1 1 auto; }
.dataTables_wrapper > .dataTables_paginate { order: 3; flex: 0 0 auto; }
.dataTables_paginate ul.pagination { margin: 0; gap: 2px; }
.dataTables_info, .dataTables_paginate, .dt-info, .dt-paging { padding: 12px 12px 6px !important; font-size: 13px; color: var(--muted) !important; }
.page-item .page-link, .dataTables_wrapper .dataTables_paginate .paginate_button, .dt-paging .dt-paging-button {
  border: 0 !important; border-radius: var(--r-sm) !important; margin: 0 1px; padding: 5px 10px !important; font-size: 13px; font-weight: 500;
  color: var(--ink-2) !important; background: transparent !important; box-shadow: none !important;
}
.page-item .page-link:hover, .dataTables_wrapper .dataTables_paginate .paginate_button:hover, .dt-paging .dt-paging-button:hover:not(.disabled) { background: var(--surface-2) !important; color: var(--ink) !important; }
.page-item.active .page-link, .dataTables_wrapper .dataTables_paginate .paginate_button.current, .dt-paging .dt-paging-button.current {
  background: var(--accent-soft) !important; color: var(--accent-text) !important; font-weight: 600; box-shadow: inset 0 0 0 1px var(--accent-line) !important;
}
.page-item.disabled .page-link, .dt-paging .dt-paging-button.disabled, .dataTables_paginate .paginate_button.disabled { color: var(--muted) !important; opacity: .5; }

.risk-cell { display: flex; align-items: center; gap: 12px; min-width: 140px; font-variant-numeric: tabular-nums; }
.risk-cell > span { width: 34px; font-size: 14px; font-weight: 600; }
.risk-track { flex: 1; max-width: 110px; min-width: 60px; height: 6px; overflow: hidden; border-radius: 99px; background: var(--surface-2); box-shadow: inset 0 0 0 1px var(--line); }
.risk-track i { display: block; height: 100%; border-radius: 99px; background: var(--low-bar); animation: grow .6s var(--ease) backwards; }
.risk-high i { background: var(--high-bar); }  .risk-medium i { background: var(--med-bar); }
.risk-high > span { color: var(--high); }
.reason-chips { display: flex; gap: 6px; flex-wrap: wrap; }

/* ---------- student drawer ---------- */
.drawer-scrim { position: fixed; inset: 0; z-index: 55; background: rgba(10,16,15,.35); animation: tab-in .2s ease; cursor: pointer; }
:root[data-theme="dark"] .drawer-scrim { background: rgba(0,0,0,.5); }
.student-drawer {
  position: fixed; z-index: 60; top: 0; right: 0; width: min(560px, 100vw); height: 100vh; height: 100dvh; overflow-y: auto; padding: 26px 28px 40px;
  background: var(--surface); color: var(--ink); border-left: 1px solid var(--line); box-shadow: var(--shadow-pop); animation: drawer-in .26s var(--ease);
}
.drawer-head { display: flex; align-items: flex-start; justify-content: space-between; gap: 12px; margin-bottom: 4px; }
.drawer-eyebrow { font-size: 12.5px; font-weight: 500; color: var(--muted); }
.drawer-title { margin: 2px 0 0; font-size: 26px; font-weight: 650; letter-spacing: -.02em; font-variant-numeric: tabular-nums; }
.drawer-close.btn {
  --bs-btn-bg: transparent; --bs-btn-border-color: var(--line-2); --bs-btn-color: var(--ink-2);
  --bs-btn-hover-bg: var(--surface-2); --bs-btn-hover-border-color: var(--line-2); --bs-btn-hover-color: var(--ink);
  --bs-btn-active-bg: var(--surface-2); --bs-btn-active-border-color: var(--line-2); --bs-btn-active-color: var(--ink);
  flex: none; display: grid; place-items: center; width: 36px; height: 36px; padding: 0; border: 1px solid var(--line-2); border-radius: var(--r-md); background: transparent; color: var(--ink-2);
}
.drawer-meta { display: flex; align-items: center; flex-wrap: wrap; gap: 8px; margin: 8px 0 18px; font-size: 14px; color: var(--ink-2); }
.detail-stats { display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; margin: 0 0 22px; }
.detail-stat { padding: 12px 14px; border-radius: var(--r-md); font-size: 12.5px; line-height: 1.4; color: var(--muted); background: var(--surface-2); box-shadow: inset 0 0 0 1px var(--line); }
.detail-stat strong { display: block; margin-bottom: 2px; font-size: 20px; font-weight: 650; letter-spacing: -.02em; color: var(--ink); font-variant-numeric: tabular-nums; }
.drawer-section { margin: 22px 0 8px; font-size: 14px; font-weight: 600; }
.reason-list { margin: 0; padding-left: 20px; line-height: 1.55; color: var(--ink-2); }
.reason-list li { margin-bottom: 8px; }
.reason-list strong { color: var(--ink); font-weight: 600; }
.plot-card { margin-top: 14px; padding: 12px 14px; }
.callout { display: flex; gap: 12px; margin: 14px 0; padding: 12px 14px; border-radius: var(--r-md); font-size: 13.5px; line-height: 1.55; color: var(--ink-2); }
.callout-caution { background: var(--gold-soft); box-shadow: inset 3px 0 0 var(--gold); }
.callout-info { background: var(--accent-soft); box-shadow: inset 3px 0 0 var(--accent-text); margin: 0 0 16px; }
.callout strong { color: var(--ink); font-weight: 600; }

/* ---------- tables in pages ---------- */
.perf-table table { width: 100%; margin: 0; border-collapse: collapse; font-size: 13.5px; font-variant-numeric: tabular-nums; }
.perf-table th { padding: 8px 10px; font-size: 12px; font-weight: 600; color: var(--muted); text-align: left; border-bottom: 1px solid var(--line); }
.perf-table td { padding: 9px 10px; color: var(--ink); border-bottom: 1px solid var(--line); }
.perf-table tr:last-child td { border-bottom: 0; }
.plot-note { margin: 12px 2px 0; font-size: 13px; color: var(--muted); max-width: 70ch; }
.status-footer { display: none; margin-top: 28px; font-size: 12px; color: var(--muted); }

/* ---------- animation ---------- */
@keyframes rise { from { opacity: 0; translate: 0 6px; } }
@keyframes grow { from { transform: scaleX(0); } }
@keyframes tab-in { from { opacity: 0; } }
@keyframes drawer-in { from { opacity: 0; translate: 24px 0; } }

/* ---------- responsive ---------- */
@media (max-width: 1100px) {
  :root { --rail-w: 72px; }
  .app-rail { padding: 18px 10px; align-items: center; }
  .brand { padding: 2px 0 20px; }
  .brand-text, .nav-label, .rail-section, .rail-status span:not(.status-dot), .theme-row > span:first-child { display: none; }
  .app-rail .rail-link { justify-content: center; padding: 8px; width: 44px; }
  .rail-foot { align-items: center; }
  .rail-status { padding: 0; }
  .theme-row { padding: 0; }
  .app-main { padding: 28px 24px 64px; }
  .kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
  .kpi-grid > .kpi-card, .kpi-grid > .kpi-card:nth-child(n+4) { grid-column: auto; }
  .kpi-grid > .kpi-card:last-child { grid-column: 1 / -1; }
}
@media (max-width: 700px) {
  :root { --rail-w: 0px; }
  .app-rail {
    inset: auto 0 0 0; width: auto; height: 64px; flex-direction: row; align-items: center; justify-content: space-around; gap: 4px;
    padding: 6px 10px calc(6px + env(safe-area-inset-bottom)); border-right: 0; border-top: 1px solid var(--line);
  }
  .brand, .rail-section, .rail-status { display: none; }
  .app-rail .rail-link { flex: 1 1 0; width: auto; min-width: 0; flex-direction: column; justify-content: center; gap: 1px; padding: 4px; }
  .nav-label { display: block; max-width: 100%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: 10.5px; }
  .app-rail .rail-link.active::before { display: none; }
  .rail-foot { margin: 0; padding: 0 0 0 6px; border: 0; flex: none; }
  .app-main { margin-left: 0; width: 100%; padding: 20px 16px 92px; }
  .topbar { margin-bottom: 18px; }
  .controls { padding: 14px 16px; }
  .controls .ctrl-select .shiny-input-container, .table-head .ctrl-select .shiny-input-container { width: 100%; }
  .ctrl-group { width: 100%; }
  .ctrl-select { flex: 1 1 140px; }
  .week-control .shiny-options-group span { padding: 6px 11px; }
  .filter-badges { margin-left: 0; }
  .kpi-value { font-size: 28px; }
  .kpi-card { padding: 16px; }
  .detail-stats { grid-template-columns: 1fr; }
  .student-drawer { padding: 20px 18px 40px; }
  .guide { margin: 0 14px 8px; }
  .table-head { padding: 16px 14px 12px; }
  .status-footer { display: block; }
  .compact-slider { min-width: 0; width: 100%; }
}
@media (max-width: 420px) { .kpi-grid { grid-template-columns: 1fr; } .kpi-grid > .kpi-card:last-child { grid-column: auto; } }
@media (prefers-reduced-motion: reduce) {
  *, *::before, *::after { animation-duration: .01ms !important; animation-delay: 0ms !important; animation-iteration-count: 1 !important; transition-duration: .01ms !important; scroll-behavior: auto !important; }
}
)---"

APP_JS <- r"---(
(function () {
  var root = document.documentElement;
  function current() { return root.getAttribute('data-theme') === 'dark' ? 'dark' : 'light'; }
  function sync() {
    var b = document.getElementById('theme_toggle');
    if (b) { b.setAttribute('aria-checked', current() === 'dark' ? 'true' : 'false'); }
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('app_theme', current());
  }
  function apply(t, save) {
    root.classList.add('theme-fade');
    root.setAttribute('data-theme', t); root.setAttribute('data-bs-theme', t);
    if (save) { try { localStorage.setItem('clarity-theme', t); } catch (e) {} }
    sync();
    setTimeout(function () { root.classList.remove('theme-fade'); }, 350);
  }
  document.addEventListener('click', function (e) {
    if (e.target.closest && e.target.closest('#theme_toggle')) apply(current() === 'dark' ? 'light' : 'dark', true);
  });
  // follow the operating system while the person has not chosen a theme themselves
  if (window.matchMedia) {
    var mq = matchMedia('(prefers-color-scheme: dark)');
    var follow = function (e) { var s = null; try { s = localStorage.getItem('clarity-theme'); } catch (x) {} if (!s) apply(e.matches ? 'dark' : 'light', false); };
    if (mq.addEventListener) mq.addEventListener('change', follow);
  }
  document.addEventListener('DOMContentLoaded', sync);
  $(document).on('shiny:connected', sync);

  Shiny.addCustomMessageHandler('activeNav', function (id) {
    $('.rail-link').removeClass('active').removeAttr('aria-current');
    $('#' + id).addClass('active').attr('aria-current', 'page');
  });

  // ---- tooltips (hover and keyboard focus) ----
  var tip = null;
  function ensureTip() {
    if (tip) return tip;
    tip = document.createElement('div'); tip.id = 'clarity-tip'; tip.setAttribute('role', 'tooltip');
    document.body.appendChild(tip); return tip;
  }
  function showTip(el) {
    var t = ensureTip(); t.textContent = el.getAttribute('data-tip'); el.setAttribute('aria-describedby', 'clarity-tip');
    t.classList.add('on');
    var r = el.getBoundingClientRect(), w = t.offsetWidth, h = t.offsetHeight;
    var left = Math.max(8, Math.min(window.innerWidth - w - 8, r.left + r.width / 2 - w / 2));
    var top = r.bottom + 8; if (top + h > window.innerHeight - 8) top = r.top - h - 8;
    t.style.left = left + 'px'; t.style.top = top + 'px';
  }
  function hideTip() { if (tip) tip.classList.remove('on'); }
  document.addEventListener('mouseover', function (e) { var el = e.target.closest && e.target.closest('[data-tip]'); if (el) showTip(el); });
  document.addEventListener('mouseout',  function (e) { if (e.target.closest && e.target.closest('[data-tip]')) hideTip(); });
  document.addEventListener('focusin',   function (e) { var el = e.target.closest && e.target.closest('[data-tip]'); if (el) showTip(el); });
  document.addEventListener('focusout',  function (e) { if (e.target.closest && e.target.closest('[data-tip]')) hideTip(); });
  window.addEventListener('scroll', hideTip, true);
  // a tooltip marker inside a sortable table heading must not trigger sorting
  ['click', 'keydown', 'keypress', 'keyup'].forEach(function (ev) {
    document.addEventListener(ev, function (e) { if (e.target.closest && e.target.closest('th .info-tip')) e.stopPropagation(); }, true);
  });

  // ---- keyboard: open a student with Enter/Space, close the drawer with Escape ----
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') {
      hideTip();
      var c = document.getElementById('close_detail'); if (c) c.click();
      return;
    }
    if ((e.key === 'Enter' || e.key === ' ') && e.target.matches && e.target.matches('table.dataTable tbody tr')) {
      e.preventDefault(); var cell = e.target.querySelector('td'); if (cell) ['mousedown', 'mouseup', 'click'].forEach(function (n) { cell.dispatchEvent(new MouseEvent(n, { bubbles: true, cancelable: true, view: window })); });
    }
  });
  document.addEventListener('click', function (e) {
    if (e.target.classList && e.target.classList.contains('drawer-scrim')) { var c = document.getElementById('close_detail'); if (c) c.click(); }
  });
  new MutationObserver(function (ms) {
    ms.forEach(function (m) {
      m.addedNodes.forEach(function (n) {
        if (n.nodeType !== 1) return;
        if (n.matches('.student-drawer') || n.querySelector('.student-drawer')) {
          var c = document.getElementById('close_detail'); if (c) setTimeout(function () { c.focus(); }, 60);
        }
      });
    });
  }).observe(document.documentElement, { childList: true, subtree: true });

  // ---- count KPI numbers up/down to their new value (skipped when reduced motion is on) ----
  var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  var last = {};
  function fmt(v, f) { return f === 'pct' ? Math.round(v) + '%' : Math.round(v).toLocaleString('en-US'); }
  function run(el) {
    if (el.dataset.counted) return;
    el.dataset.counted = '1';
    var to = parseFloat(el.dataset.count), key = el.dataset.key || '', f = el.dataset.format;
    if (isNaN(to)) return;
    var from = (key in last) ? last[key] : 0;
    last[key] = to;
    var finalText = el.textContent;
    if (reduce || from === to) return;
    var t0 = null, dur = 600;
    el.textContent = fmt(from, f);
    function step(ts) {
      if (t0 === null) t0 = ts;
      var p = Math.min((ts - t0) / dur, 1), e = 1 - Math.pow(1 - p, 3);
      el.textContent = p < 1 ? fmt(from + (to - from) * e, f) : finalText;
      if (p < 1) requestAnimationFrame(step);
    }
    requestAnimationFrame(step);
  }
  function scan(n) {
    if (n.nodeType !== 1) return;
    if (n.matches('.kpi-value[data-count]')) run(n);
    n.querySelectorAll('.kpi-value[data-count]').forEach(run);
  }
  new MutationObserver(function (ms) { ms.forEach(function (m) { m.addedNodes.forEach(scan); }); })
    .observe(document.documentElement, { childList: true, subtree: true });
})();
)---"

TOUR_CSS <- r"---(
/* ---------- first-visit guided tour ---------- */
.tour-block { position: fixed; inset: 0; z-index: 300; background: transparent; }
.tour-block.dim { background: rgba(10,16,15,.55); }
:root[data-theme="dark"] .tour-block.dim { background: rgba(0,0,0,.62); }
.tour-spot {
  position: fixed; z-index: 301; border-radius: 12px; pointer-events: none; box-shadow: 0 0 0 9999px rgba(10,16,15,.55), 0 0 0 2px var(--accent-text);
  transition: top .28s var(--ease), left .28s var(--ease), width .28s var(--ease), height .28s var(--ease), opacity .2s ease;
}
:root[data-theme="dark"] .tour-spot { box-shadow: 0 0 0 9999px rgba(0,0,0,.62), 0 0 0 2px var(--accent-text); }
.tour-spot.hidden { opacity: 0; box-shadow: none; }
.tour-card {
  position: fixed; z-index: 302; width: min(380px, calc(100vw - 24px)); padding: 20px 20px 16px; border-radius: var(--r-lg);
  background: var(--surface); color: var(--ink); border: 1px solid var(--line-2); box-shadow: var(--shadow-pop);
  transition: top .28s var(--ease), left .28s var(--ease), opacity .2s ease; animation: rise .3s var(--ease);
}
.tour-card.center { width: min(480px, calc(100vw - 24px)); padding: 28px 28px 20px; }
.tour-card { max-height: calc(100vh - 24px); overflow-y: auto; }
.tour-step { margin: 0 0 6px; font-size: 12px; font-weight: 600; color: var(--accent-text); }
.tour-title { margin: 0 0 8px; font-size: 17px; font-weight: 650; letter-spacing: -.015em; }
.tour-card.center .tour-title { font-size: 21px; }
.tour-text { margin: 0; font-size: 14px; line-height: 1.6; color: var(--ink-2); }
.tour-text + .tour-text { margin-top: 8px; }
.tour-foot { display: flex; align-items: center; justify-content: space-between; gap: 12px; margin-top: 18px; }
.tour-dots { display: flex; gap: 5px; }
.tour-dots i { width: 6px; height: 6px; border-radius: 50%; background: var(--line-2); transition: background-color .2s ease, transform .2s var(--ease); }
.tour-dots i.on { background: var(--accent-text); transform: scale(1.25); }
.tour-actions { display: flex; align-items: center; gap: 8px; }
.tour-btn-primary, .tour-btn-ghost, .tour-skip {
  height: 36px; padding: 0 14px; border-radius: var(--r-md); font-family: var(--font); font-size: 13.5px; font-weight: 600; cursor: pointer;
  transition: background-color .18s var(--ease), transform .12s var(--ease);
}
.tour-btn-primary { border: 1px solid var(--btn); background: var(--btn); color: #fff; }
.tour-btn-primary:hover { background: var(--btn-hover); border-color: var(--btn-hover); }
.tour-btn-ghost { border: 1px solid var(--line-2); background: transparent; color: var(--ink-2); }
.tour-btn-ghost:hover { background: var(--surface-2); }
.tour-btn-primary:active, .tour-btn-ghost:active { transform: translateY(1px); }
.tour-skip { border: 0; background: transparent; color: var(--muted); padding: 0 6px; font-weight: 500; }
.tour-skip:hover { color: var(--ink); text-decoration: underline; }
.tour-launch.btn, .tour-launch {
  display: flex; align-items: center; gap: 10px; width: 100%; padding: 6px 8px; border: 0; border-radius: var(--r-md); background: transparent;
  color: var(--ink-2); font-family: var(--font); font-size: 13px; font-weight: 500; cursor: pointer; text-align: left; transition: background-color .18s var(--ease), color .18s var(--ease);
}
.tour-launch:hover { background: var(--surface-2); color: var(--ink); }
.tour-launch svg { flex: none; color: var(--muted); }
@media (max-width: 1100px) { .tour-launch span { display: none; } .tour-launch { justify-content: center; width: 40px; } }
@media (max-width: 700px) { .tour-launch { width: 36px; padding: 6px; } }
)---"

TOUR_JS <- r"---(
(function () {
  var KEY = 'clarity-tour-done', memoryDone = false;
  function isDone() { try { return localStorage.getItem(KEY) === '1'; } catch (e) { return memoryDone; } }
  function markDone() { memoryDone = true; try { localStorage.setItem(KEY, '1'); } catch (e) {} }

  // ---- read the live numbers so the explanation talks about what is on screen ----
  function q(sel) { return document.querySelector(sel); }
  function cardValue(n) { var e = q('.kpi-grid .kpi-card:nth-child(' + n + ') .kpi-value'); return e ? e.textContent.trim() : 'n/a'; }
  function checkpointLabel() {
    var c = q('.week-control input:checked'); var s = c && c.parentNode && c.parentNode.querySelector('span');
    return s ? s.textContent.trim() : 'this checkpoint';
  }
  function cardsText() {
    var monitored = cardValue(1), flagged = cardValue(2), acc = cardValue(3), early = cardValue(4);
    var goal = q('.goal-meta .pill'), goalText = goal ? goal.textContent.trim().toLowerCase() : '';
    var out = ['At ' + checkpointLabel() + ', ' + monitored + ' students are in view and ' + flagged + ' are flagged as needing attention.'];
    if (acc !== 'n/a') out.push('Of the flagged students, ' + acc + ' were later recorded as failing or withdrawing (Accuracy of alerts). The alerts also reached ' + early + ' of all students who went on to fail or withdraw (Students identified early).');
    else out.push('Students identified early shows how many of the students who went on to fail or withdraw were flagged: ' + early + '.');
    out.push('The Detection goal bar shows whether alerts reach at least 75% of those students' + (goalText ? ' (' + goalText + ' right now)' : '') + '. No estimate is perfect: some flagged students will do fine, and some who struggle will not be flagged.');
    return out;
  }

  // ---- helpers that open and close a real student so the tour can show the details panel ----
  var openedByTour = false;
  function rowExists() { return !!q('table.dataTable tbody tr td:not(.dataTables_empty)'); }
  function openStudent(done) {
    if (q('.student-drawer')) return done();
    var cell = q('table.dataTable tbody tr td'); if (!cell || !rowExists()) return done();
    ['mousedown', 'mouseup', 'click'].forEach(function (n) { cell.dispatchEvent(new MouseEvent(n, { bubbles: true, cancelable: true, view: window })); });
    openedByTour = true;
    var n = 0, iv = setInterval(function () {
      n++; if (q('.student-drawer .drawer-title') || n > 40) { clearInterval(iv); setTimeout(done, 450); }
    }, 150);
  }
  function closeStudent(done) {
    var c = document.getElementById('close_detail');
    if (c) { c.click(); openedByTour = false; setTimeout(done, 450); } else done();
  }

  var STEPS = [
    { center: true, title: 'Welcome to Clarity',
      text: ['Clarity helps you spot students who may need extra support before it is too late.',
             'It looks at how each student has engaged with the course so far, such as logging in, reading and submitting work, and estimates who is more likely to fail or withdraw by the end.',
             'This tour takes about two minutes. You can replay it any time.'] },
    { center: true, title: 'What does “at risk” mean here?',
      text: ['A student counts as at risk if their final result was Fail or Withdrawn. Pass or Distinction counts as not at risk.',
             'Clarity estimates this early, using only activity up to Week 4, 8 or 12. It never uses the final result to make an estimate.',
             'This dashboard shows past courses, so the real outcome is known. That lets you see how well the alerts matched what actually happened.'] },
    { sel: '.controls', title: 'Choose what you are looking at',
      text: ['Start by choosing a module and a class session (one running of that module).',
             'Then pick a checkpoint. Week 4 gives the earliest warning but has the least evidence. By Week 12 the estimates are more reliable, because more work has been submitted.',
             'Leave “Held-out test students only” on for the fairest picture of how well the alerts work.'] },
    { sel: '.kpi-grid', title: 'Read the summary cards', text: cardsText },
    { sel: 'table.dataTable thead th:nth-child(2)', needsRow: true, title: 'Start at the top of the list',
      text: ['Students are listed by estimated risk, highest first. The score runs from 0 to 1: the higher it is, the more the student’s activity looks like students who went on to fail or withdraw.',
             'Risk level helps you scan: High is 0.70 or above, Medium means flagged but below 0.70, and Low means not flagged.'] },
    { sel: 'table.dataTable thead', needsRow: true, title: 'Why a student was flagged',
      text: ['Areas of concern shows where the student stands out: Engagement (how active they have been on the course site, including recently) or Assessments (work submitted, late or missed, and scores).',
             'Alert status shows whether the alert is New, Ongoing (flagged at every checkpoint so far) or Returning.',
             'Recorded outcome shows what actually happened by the end of the course, so you can judge how reliable the alerts have been.'] },
    { sel: '.student-drawer', lazy: true, needsRow: true, before: openStudent, title: 'A student’s details',
      text: ['Select any row (or press Enter) to open this panel. At the top you can see where the student ranks by risk, how many checkpoints they were flagged at, and how their estimated risk has changed.',
             'Below are the areas of concern in plain words, a chart of risk over time, and a chart of what is pushing the estimate up or down. Bars to the right raise the estimate, and the grey Background bar is context only.',
             'Press Esc to close the panel.'] },
    { center: true, before: closeStudent, title: 'Using the estimates well',
      text: ['Treat a flag as a prompt to look closer, not a verdict. Check the student’s engagement and assessment results, then decide what support, if any, is right.',
             'Some flagged students will do fine, and some who struggle will not be flagged. Alerts driven more by background than by activity are less reliable, so never act on background alone.',
             'Alerts can also be wrong more often for some groups of students. The Fairness review page helps you keep an eye on this.'] },
    { sel: '.app-rail nav, nav.app-rail', title: 'Explore the other pages',
      text: ['Overview shows how estimated risk is spread and how alerts compare with real outcomes at each checkpoint.',
             'Fairness review highlights alerts that are shaped more by background than by activity.',
             'Prediction accuracy shows how well the model has performed at each checkpoint.'] },
    { sel: '.export-btn', title: 'Take the list with you',
      text: ['Export saves the current list as a spreadsheet (CSV) that you can share or keep.'] },
    { sel: '.rail-foot', title: 'Make it comfortable',
      text: ['Switch between light and dark mode here at any time.',
             'You can replay this tour whenever you like with “Take the tour”.'] }
  ];

  var idx = 0, steps = [], els = null, timer = null, openerFocus = null, busy = false;

  function visible(el) { if (!el) return false; var r = el.getBoundingClientRect(); return r.width > 4 && r.height > 4; }
  function target(s) { return s.sel ? q(s.sel) : null; }
  function build() {
    steps = STEPS.filter(function (s) {
      if (s.needsRow && !rowExists()) return false;
      return s.center || s.lazy || visible(target(s));
    });
  }
  function make(tag, cls, parent) { var n = document.createElement(tag); if (cls) n.className = cls; if (parent) parent.appendChild(n); return n; }

  function place(card, rect, center) {
    var vw = window.innerWidth, vh = window.innerHeight, w = card.offsetWidth, h = card.offsetHeight, gap = 14, m = 12, top, left, docked = false;
    if (center || !rect) { top = (vh - h) / 2; left = (vw - w) / 2; }
    else {
      var space = { bottom: vh - rect.bottom, top: rect.top, right: vw - rect.right, left: rect.left };
      if (space.right >= w + gap + m && rect.width < vw * 0.5) { left = rect.right + gap; top = rect.top; }
      else if (space.bottom >= h + gap + m) { top = rect.bottom + gap; left = rect.left; }
      else if (space.top >= h + gap + m) { top = rect.top - h - gap; left = rect.left; }
      else if (space.left >= w + gap + m && rect.width < vw * 0.6) { left = rect.left - w - gap; top = rect.top + 8; }
      else { top = vh - h - m; left = (vw - w) / 2; docked = true; }   // large target: dock the card at the bottom
    }
    card.style.left = Math.max(m, Math.min(vw - w - m, left)) + 'px';
    card.style.top = Math.max(m, Math.min(vh - h - m, top)) + 'px';
    return docked;
  }

  function render() {
    var s = steps[idx], el = target(s), n = steps.length, card = els.card; s._adj = false;
    var paras = typeof s.text === 'function' ? s.text() : s.text;
    card.classList.toggle('center', !!s.center);
    card.innerHTML = '';
    make('p', 'tour-step', card).textContent = idx === 0 ? 'Quick tour' : 'Step ' + idx + ' of ' + (n - 1);
    var h = make('h2', 'tour-title', card); h.id = 'tour-title'; h.textContent = s.title;
    paras.forEach(function (t) { make('p', 'tour-text', card).textContent = t; });
    var foot = make('div', 'tour-foot', card), dots = make('div', 'tour-dots', foot);
    for (var i = 0; i < n; i++) make('i', i === idx ? 'on' : '', dots);
    var act = make('div', 'tour-actions', foot);
    if (idx === 0) {
      var sk = make('button', 'tour-skip', act); sk.type = 'button'; sk.textContent = 'Skip'; sk.onclick = end;
    } else {
      var bk = make('button', 'tour-btn-ghost', act); bk.type = 'button'; bk.textContent = 'Back'; bk.onclick = function () { go(idx - 1); };
    }
    var nx = make('button', 'tour-btn-primary', act); nx.type = 'button';
    nx.textContent = idx === 0 ? 'Start tour' : (idx === n - 1 ? 'Finish' : 'Next');
    nx.onclick = function () { if (idx === n - 1) end(); else go(idx + 1); };
    card.setAttribute('aria-labelledby', 'tour-title');
    els.block.classList.toggle('dim', !!s.center);
    els.spot.classList.toggle('hidden', !!s.center || !el);
    if (el && !s.center && !s.lazy) {
      var r0 = el.getBoundingClientRect();
      if (r0.top < 0 || r0.bottom > window.innerHeight) el.scrollIntoView({ block: 'center', behavior: 'auto' });
    }
    reposition();
    setTimeout(function () { if (els) nx.focus(); }, 220);   // after any panel that grabs focus as it opens
  }

  function reposition() {
    if (!els || busy) return;
    var s = steps[idx], el = target(s);
    if (!s.center && el && visible(el)) {
      var r = el.getBoundingClientRect(), pad = 6;
      var rect = { top: r.top - pad, left: r.left - pad, right: r.right + pad, bottom: r.bottom + pad, width: r.width + pad * 2, height: r.height + pad * 2 };
      var sp = els.spot.style; sp.top = rect.top + 'px'; sp.left = rect.left + 'px'; sp.width = rect.width + 'px'; sp.height = rect.height + 'px';
      var docked = place(els.card, rect, false);
      // a tall highlight leaves no room for the card: bring it to the top of the screen so the card can sit below it
      if (docked && !s.lazy && !s._adj) { s._adj = true; var dy = rect.top - 24; if (dy > 8) window.scrollBy(0, dy); }
    } else { place(els.card, null, true); }
  }

  function go(i) {
    if (busy) return;
    idx = Math.max(0, Math.min(steps.length - 1, i));
    var s = steps[idx];
    if (s.before) {
      busy = true; els.spot.classList.add('hidden');
      s.before(function () { busy = false; if (els) render(); });
    } else if (!s.sel && !s.center) { render(); }
    else {
      // moving away from the student panel: close it unless the next step needs it
      if (openedByTour && !s.lazy && q('.student-drawer')) { busy = true; closeStudent(function () { busy = false; if (els) render(); }); }
      else render();
    }
  }

  function onKey(e) {
    if (!els) return;
    if (e.key === 'Escape') { e.preventDefault(); e.stopPropagation(); end(); }
    else if (e.key === 'ArrowRight') { e.preventDefault(); if (idx < steps.length - 1) go(idx + 1); }
    else if (e.key === 'ArrowLeft') { e.preventDefault(); if (idx > 0) go(idx - 1); }
    else if (e.key === 'Tab') {   // keep keyboard focus inside the tour card
      var f = els.card.querySelectorAll('button'); if (!f.length) return;
      var first = f[0], last = f[f.length - 1];
      if (e.shiftKey && document.activeElement === first) { e.preventDefault(); last.focus(); }
      else if (!e.shiftKey && document.activeElement === last) { e.preventDefault(); first.focus(); }
      else if (!els.card.contains(document.activeElement)) { e.preventDefault(); first.focus(); }
    }
  }

  function start() {
    if (els) return;
    var drawerClose = document.getElementById('close_detail'); if (drawerClose) drawerClose.click();
    build(); if (!steps.length) return;
    openerFocus = document.activeElement; idx = 0; busy = false;
    els = { block: make('div', 'tour-block', document.body), spot: make('div', 'tour-spot hidden', document.body), card: make('div', 'tour-card', document.body) };
    els.card.setAttribute('role', 'dialog'); els.card.setAttribute('aria-modal', 'true');
    document.addEventListener('keydown', onKey, true);
    window.addEventListener('resize', reposition); window.addEventListener('scroll', reposition, true);
    timer = setInterval(reposition, 200);   // follow the target if the page re-renders
    render();
  }

  function end() {
    markDone();
    if (!els) return;
    clearInterval(timer); document.removeEventListener('keydown', onKey, true);
    window.removeEventListener('resize', reposition); window.removeEventListener('scroll', reposition, true);
    ['block', 'spot', 'card'].forEach(function (k) { if (els[k].parentNode) els[k].parentNode.removeChild(els[k]); });
    els = null; busy = false;
    if (openedByTour) { var c = document.getElementById('close_detail'); if (c) c.click(); openedByTour = false; }
    if (openerFocus && openerFocus.focus) try { openerFocus.focus(); } catch (e) {}
  }

  document.addEventListener('click', function (e) {
    if (e.target.closest && e.target.closest('#tour_btn')) { e.preventDefault(); start(); }
  });

  // first visit: begin once the dashboard has finished drawing its cards and table
  var tries = 0, wait = setInterval(function () {
    tries++;
    if (isDone() || tries > 60) { clearInterval(wait); return; }
    var ready = q('.kpi-card') && q('table.dataTable tbody tr') && !document.documentElement.classList.contains('shiny-busy');
    if (ready) { clearInterval(wait); setTimeout(function () { if (!isDone()) start(); }, 700); }
  }, 400);
})();
)---"

# ---- UI --------------------------------------------------------------------
TIP_EVAL <- "Held-out test students are ones the prediction model did not see while it was learning. Showing only them gives the fairest picture of how well alerts work. Untick to include everyone."
TIP_LEVEL <- "Students are flagged for attention when their estimated risk is at or above this level. Lowering it flags more students; raising it flags fewer."

ui <- page_fillable(
  title = "Clarity · Student support overview",
  theme = bs_theme(version = 5, primary = "#0F766E", secondary = "#115E59", success = "#2F9E6B",
                   warning = "#C49A45", danger = "#B4382F", bg = "#F8FAF9", fg = "#17212B",
                   base_font = font_google("Inter"), heading_font = font_google("Inter")),
  tags$head(
    tags$meta(name = "color-scheme", content = "light dark"),
    tags$link(rel = "icon", type = "image/svg+xml", href = FAVICON),
    tags$script(HTML(THEME_BOOT)),
    tags$style(HTML(APP_CSS)), tags$style(HTML(TOUR_CSS))),
  tags$script(HTML(APP_JS)), tags$script(HTML(TOUR_JS)),
  tags$a(class = "skip-link", href = "#main", "Skip to main content"),
  div(class = "app-shell",
    tags$nav(class = "app-rail", `aria-label` = "Dashboard sections",
      div(class = "brand", HTML(LOGO_SVG),
          div(class = "brand-text", div(class = "brand-name", "Clarity"),
              div(class = "brand-tag", "Student success, made clearer."))),
      div(class = "rail-section", "Dashboard"),
      nav_button("go_students", "students", "Students"), nav_button("go_overview", "overview", "Overview"),
      nav_button("go_fairness", "fairness", "Fairness review"), nav_button("go_performance", "accuracy", "Prediction accuracy"),
      div(class = "rail-foot",
        div(class = "rail-status", title = if (DATA_MODE == "preview") "Using exported historical records" else "Connected to the live student database",
            span(class = paste("status-dot", if (DATA_MODE == "preview") "status-dot-preview")),
            span(strong(if (DATA_MODE == "preview") "Preview data" else "Live data"),
                 if (DATA_MODE == "preview") "Exported historical records" else "Connected to the student database")),
        tags$button(id = "tour_btn", type = "button", class = "tour-launch", title = "Take a guided tour of the dashboard",
                    svg_icon("help", 18), span("Take the tour")),
        div(class = "theme-row", span("Dark mode"),
            tags$button(id = "theme_toggle", type = "button", class = "theme-toggle", role = "switch",
                        `aria-checked` = "false", `aria-label` = "Dark mode", title = "Switch between light and dark",
                        span(class = "tt-knob", span(class = "tt-sun", svg_icon("sun", 13, 2)), span(class = "tt-moon", svg_icon("moon", 13, 2))))))),
    tags$main(id = "main", class = "app-main", tabindex = "-1",
      div(class = "main-inner",
        div(class = "topbar",
          div(h1(class = "page-title", "Student support overview"), uiOutput("page_sub", container = function(...) tags$p(class = "page-sub", ...))),
          downloadButton("dl", tagList(svg_icon("download", 16, 2), "Export list"), class = "export-btn", icon = NULL)),
        div(class = "controls surface",
          div(class = "ctrl-group",
            div(class = "ctrl-select", selectInput("module", "Module", choices = NULL)),
            div(class = "ctrl-select", selectInput("pres", label = tagList("Class session", info_tip("A single running of the module, for example one intake or term.")), NULL)),
            div(class = "week-control",
                radioButtons("cutoff", label = tagList("Checkpoint", info_tip("Alerts use student activity up to this week of the course. Later checkpoints have more evidence to go on.")),
                             c("Week 4" = 28, "Week 8" = 56, "Week 12" = 84), inline = TRUE))),
          div(class = "ctrl-group",
            div(class = "switch-stack",
              div(class = "switch-row", checkboxInput("test_only", tagList("Held-out test students only", info_tip(TIP_EVAL)), TRUE)),
              div(class = "switch-row", checkboxInput("custom_thr", "Adjust alert level", FALSE))),
            conditionalPanel("input.custom_thr", div(class = "compact-slider",
              sliderInput("thr", label = tagList("Flag a student when estimated risk is at least", info_tip(TIP_LEVEL)), min = 0.05, max = 0.95, value = 0.38, step = 0.01))),
            uiOutput("filter_badges"))),
        uiOutput("data_status"),
        navset_hidden(id = "main_tab",
          nav_panel("Students",
            uiOutput("cards"),
            div(class = "card table-card",
              div(class = "table-head",
                div(h2(class = "table-title", "Students to review"), p(class = "table-sub", "Highest estimated risk first")),
                div(class = "ctrl-select", selectInput("focus", "Show",
                  c("Students needing attention" = "flagged", "New this checkpoint" = "new",
                    "Alerted at every checkpoint so far" = "persist", "All students" = "all")))),
              uiOutput("guidance"),
              DTOutput("tbl")),
            uiOutput("detail")),
          nav_panel("Overview",
            p(class = "page-intro", "A picture of how estimated risk is spread across students, and how alerts compare with what was later recorded at each checkpoint."),
            layout_columns(col_widths = c(6, 6),
              card(card_header("How estimated risk is spread"),
                   p(class = "card-caption", "Each bar counts students with a similar estimated risk. The dashed line marks the alert level."),
                   plotOutput("dist", height = 340)),
              card(card_header("Alerts and outcomes by checkpoint"),
                   p(class = "card-caption", "Counts include every student in this class session, not only held-out test students."),
                   plotOutput("weeks", height = 340)))),
          nav_panel("Fairness",
            p(class = "page-intro", "Some estimates are shaped more by a student's background than by their activity and assessments. Review those alerts with extra care."),
            uiOutput("fair_txt"),
            card(card_header("What is pushing each alert: activity or background?"), plotOutput("fair_plot", height = 400)),
            p(class = "plot-note", "Points above the dashed line are influenced more by background than by activity and assessments. Treat these alerts with extra caution.")),
          nav_panel("Model performance",
            p(class = "page-intro", "How well the alerts have matched what happened to past students. These figures come from students the model was tested on."),
            layout_columns(col_widths = c(7, 5),
              card(card_header(tagList("How well the model ranks students (AUC)", info_tip("AUC shows how well the model places students who ended up at risk above those who did not. 1.0 is perfect; 0.5 is no better than chance."))),
                   plotOutput("perf", height = 320)),
              card(card_header("Detailed results"), div(class = "perf-table", tableOutput("perf_tbl")))))
        ),
        div(class = "status-footer", span(class = paste("status-dot", if (DATA_MODE == "preview") "status-dot-preview"), style = "display:inline-block;margin-right:8px"),
            if (DATA_MODE == "preview") "Preview data: exported historical records" else "Live data: connected to the student database")
      )
    )
  )
)

# ---- server ----------------------------------------------------------------
server <- function(input, output, session) {
  detail_state <- reactiveValues(open = FALSE)
  cards_env <- new.env(); cards_env$seen <- FALSE   # intro animation plays on first render only
  app_theme <- reactive({ req(input$app_theme); if (identical(input$app_theme, "dark")) "dark" else "light" })
  pal <- reactive(CHART_PAL[[app_theme()]])
  observeEvent(input$go_students, updateTabsetPanel(session, "main_tab", selected = "Students"))
  observeEvent(input$go_overview, updateTabsetPanel(session, "main_tab", selected = "Overview"))
  observeEvent(input$go_fairness, updateTabsetPanel(session, "main_tab", selected = "Fairness"))
  observeEvent(input$go_performance, updateTabsetPanel(session, "main_tab", selected = "Model performance"))
  observeEvent(input$main_tab, {
    nav_ids <- c("Students" = "go_students", "Overview" = "go_overview", "Fairness" = "go_fairness", "Model performance" = "go_performance")
    tab <- if (is.null(input$main_tab)) "Students" else input$main_tab
    session$sendCustomMessage("activeNav", unname(nav_ids[[tab]]))
  }, ignoreNULL = FALSE)

  preview_d <- reactive({
    if (DATA_MODE == "preview") read_preview_records(PREVIEW_RECORDS) else NULL
  })
  output$data_status <- renderUI({
    if (DATA_MODE != "preview") return(NULL)
    d <- preview_d()
    p(class = "note", sprintf(
      "Preview data: showing exported historical records (%s).", format(nrow(d), big.mark = ",")
    ))
  })

  output$page_sub <- renderUI({
    req(input$cutoff)
    HTML(sprintf("Estimates of which students may benefit from support, based on activity up to <strong>%s</strong>.",
                 week_label(input$cutoff)))
  })

  modules <- reactive({
    if (DATA_MODE == "preview") sort(unique(preview_d()$module)) else sort(pred$distinct("module"))
  })
  observeEvent(modules(), updateSelectInput(session, "module", choices = modules()))

  pres_choices <- reactive({
    req(input$module)
    if (DATA_MODE == "preview") {
      sort(unique(preview_d()$presentation[preview_d()$module == input$module]))
    } else {
      sort(pred$distinct("presentation", sprintf('{"module":"%s"}', input$module)))
    }
  })
  observeEvent(pres_choices(), updateSelectInput(session, "pres", choices = pres_choices()))

  # all cutoffs for the chosen run: one Mongo query, reused by every chart
  raw_d <- reactive({
    req(input$module, input$pres %in% pres_choices())
    if (DATA_MODE == "preview") {
      d <- preview_d()
      d <- d[d$module == input$module & d$presentation == input$pres, ]
    } else {
      q <- sprintf('{"module":"%s","presentation":"%s"}', input$module, input$pres)
      d <- jsonlite::flatten(pred$find(q, fields = FIELDS))
    }
    d <- require_fields(d, REQUIRED_PREDICTION_COLUMNS, "Prediction data")
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
    validate(need(nrow(d) > 0, "No students at this checkpoint. Try another checkpoint or untick “Held-out test students only”."))
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

  previous_d <- reactive({
    d <- all_d()
    earlier <- d$cutoff_day[d$cutoff_day < as.numeric(input$cutoff)]
    if (!length(earlier)) return(NULL)
    d <- d[d$cutoff_day == max(earlier), ]
    if (input$test_only) d <- d[d$split == "test", ]
    d
  })
  summary_values <- function(d) {
    flagged <- which(d$flagged); actual <- d$actual
    list(students = nrow(d), flagged = length(flagged),
         precision = if (length(flagged)) mean(actual[flagged], na.rm = TRUE) else NA_real_,
         recall = sum(actual & d$flagged, na.rm = TRUE) / max(sum(actual, na.rm = TRUE), 1))
  }
  output$filter_badges <- renderUI({
    d <- cur()
    threshold <- if (input$custom_thr) input$thr else if (any(d$flagged)) min(d$risk_score[d$flagged]) else NA_real_
    div(class = "filter-badges",
        pill(if (input$test_only) "Held-out test students" else "All students", "pill-accent"),
        span(title = "Students are flagged for attention when their estimated risk is at or above this level.",
             pill(if (is.na(threshold)) "No alerts" else sprintf("Alert level: risk ≥ %.2f", threshold), "pill-neutral")))
  })

  # ---- summary cards -------------------------------------------------------
  output$cards <- renderUI({
    current <- summary_values(cur()); previous <- previous_d()
    prior <- if (is.null(previous)) NULL else summary_values(previous)
    intro <- !cards_env$seen; cards_env$seen <- TRUE
    prev  <- function(field) if (is.null(prior)) NA else prior[[field]]
    first <- is.null(prior)
    foot  <- function(chip) if (!first) div(class = "kpi-foot", chip, span("since previous checkpoint"))
    kpi_card <- function(label, tip, value, hint, chip, idx, key, raw = NA, fmt = "int", extra = NULL)
      div(class = paste("card kpi-card", if (!intro) "no-intro"), style = sprintf("--i:%d", idx),
          div(class = "kpi-label", label, info_tip(tip)),
          div(class = "kpi-value", `data-count` = if (is.na(raw)) NULL else raw, `data-format` = fmt, `data-key` = key, value),
          div(class = "kpi-hint", hint), extra, foot(chip))
    met <- !is.na(current$recall) && current$recall >= .75
    goal_card <- div(class = paste("card kpi-card", if (!intro) "no-intro"), style = "--i:4",
      div(class = "kpi-label", "Detection goal", info_tip("Our aim is for alerts to reach at least 75% of the students who are later recorded as at risk (a recall of at least 75%).")),
      div(class = "kpi-value", "75%"),
      div(class = "goal",
      div(class = "goal-track", role = "img",
          `aria-label` = sprintf("%s of at-risk students identified early, against a goal of 75%%", pct(current$recall)),
          span(class = paste("goal-fill", if (!met) "below"), style = sprintf("width:%d%%", as.integer(round(100 * min(max(current$recall, 0), 1))))),
          span(class = "goal-tick", style = "left:75%")),
      div(class = "goal-meta",
          pill(if (met) "Goal met" else "Below goal", if (met) "pill-goal-met pill-dot" else "pill-goal-below pill-dot"),
          span(sprintf("%s identified early", pct(current$recall))))))
    div(class = "kpi-grid",
      kpi_card("Students monitored", "The number of students included in the current view.",
               format(current$students, big.mark = ","),
               if (input$test_only) "Held-out test students in this class session" else "All students in this class session",
               delta_chip(current$students, prev("students"), first = first), 0, "students", raw = current$students),
      kpi_card("Students needing attention", "Students whose estimated risk is at or above the alert level. This is an estimate to help you decide who to look at first, not a confirmed outcome.",
               format(current$flagged, big.mark = ","),
               sprintf("%s of students monitored", pct(current$flagged / max(current$students, 1))),
               delta_chip(current$flagged, prev("flagged"), first = first), 1, "flagged", raw = current$flagged),
      kpi_card("Accuracy of alerts", "Of the students who were flagged at this checkpoint, the share who were later recorded as at risk (final result Fail or Withdrawn). Based on past outcomes. (Also called precision.)",
               pct(current$precision), "Flagged students who were recorded as at risk",
               delta_chip(current$precision, prev("precision"), TRUE, good_up = TRUE, first = first), 2, "precision",
               raw = if (is.na(current$precision)) NA else 100 * current$precision, fmt = "pct"),
      kpi_card("Students identified early", "Of all students later recorded as at risk (final result Fail or Withdrawn), the share who already had an alert at this checkpoint. (Also called recall.)",
               pct(current$recall), "Students recorded as at risk who were flagged",
               delta_chip(current$recall, prev("recall"), TRUE, good_up = TRUE, first = first), 3, "recall",
               raw = 100 * current$recall, fmt = "pct"),
      goal_card)
  })

  output$guidance <- renderUI({
    d <- tryCatch(cur(), error = function(e) NULL); if (is.null(d)) return(NULL)
    n <- sum(d$flagged)
    n_high <- sum(d$risk_score >= 0.7, na.rm = TRUE); n_bg <- sum(d$flagged & d$bg_driven, na.rm = TRUE)
    detail <- if (n == 0) NULL else sprintf("%s flagged, %d with high estimated risk. %d %s influenced more by background than by recent activity or results, so check %s carefully (see Fairness review).",
                format(n, big.mark = ","), n_high, n_bg, if (n_bg == 1) "alert is" else "alerts are", if (n_bg == 1) "it" else "them")
    lead <- if (n == 0) sprintf("No students currently need attention at %s.", week_label(input$cutoff))
            else sprintf("%s %s need%s attention at %s.", format(n, big.mark = ","), if (n == 1) "student" else "students", if (n == 1) "s" else "", week_label(input$cutoff))
    div(class = "guide", role = "note",
      span(class = "guide-icon", svg_icon("lightbulb", 16, 2)),
      div(p(class = "guide-lead", "Where to start"),
          p(lead, " Start with students showing higher risk. Look at their engagement and assessment results before deciding what support they may need. ",
            "These are estimates to help you prioritise, not confirmed outcomes. Select a student to see more."),
          if (!is.null(detail)) p(class = "guide-detail", detail)))
  })

  # ---- student table -------------------------------------------------------
  output$tbl <- renderDT({
    d <- shown()
    validate(need(nrow(d) > 0, "No students match these filters. Try “All students” or lower the alert level."))
    band <- ifelse(d$risk_score >= 0.7, "high", ifelse(d$flagged, "medium", "low"))
    band_pill <- c(high = "pill-high pill-dot", medium = "pill-medium pill-dot", low = "pill-low pill-dot")
    band_text <- c(high = "High", medium = "Medium", low = "Low")
    th_tip <- function(label, tip) paste0(label, as.character(info_tip(tip)))
    out <- data.frame(
      Student = d$id_student,
      Risk    = vapply(seq_len(nrow(d)), function(i) risk_bar(d$risk_score[i], band[i]), ""),
      Band    = vapply(band, function(b) as.character(pill(band_text[[b]], band_pill[[b]])), "", USE.NAMES = FALSE),
      Status  = vapply(d$status, function(x) as.character(status_pill(x)), "", USE.NAMES = FALSE),
      Reasons = vapply(d$reasons, function(x) as.character(reason_chips(x)), ""),
      `Actual outcome` = vapply(d$actual, function(x) as.character(pill(if (isTRUE(x)) "At risk" else "Not at risk", if (isTRUE(x)) "pill-outcome-risk" else "pill-outcome-ok")), ""),
      check.names = FALSE)
    datatable(out, selection = "single", rownames = FALSE, escape = FALSE,
              colnames = c(
                th_tip("Student ID", "Anonymised student identifier from the dataset."),
                th_tip("Estimated risk", "A score from 0 to 1 showing how likely the model thinks it is that a student will struggle. Higher means more likely. It is an estimate, not a certainty."),
                th_tip("Risk level", "High: estimated risk of 0.70 or more. Medium: flagged for attention, but below 0.70. Low: not flagged."),
                th_tip("Alert status", "New: first alert at this checkpoint. Ongoing: flagged at every checkpoint so far. Returning: flagged before, but not at every checkpoint. No alert: not flagged."),
                th_tip("Areas of concern", "The main areas where this student's activity or results stood out. Select a student for details."),
                th_tip("Recorded outcome", "What actually happened by the end of the course. At risk means the final result was Fail or Withdrawn; Not at risk means Pass or Distinction. Shown so you can see how well past alerts matched real outcomes.")),
              options = list(pageLength = 12, dom = "frtip", autoWidth = FALSE,
                             language = list(search = "", searchPlaceholder = "Search students",
                                             info = "Showing _START_ to _END_ of _TOTAL_ students",
                                             infoEmpty = "No students to show", zeroRecords = "No students match your search."),
                             columnDefs = list(list(width = "170px", targets = 1)),
                             initComplete = DT::JS("function(){ $(this.api().table().container()).find('input[type=search]').attr('aria-label','Search students'); }"),
                             drawCallback = DT::JS("function(){ $(this.api().table().body()).find('tr').attr('tabindex', 0); }")))
  })
  observeEvent(input$tbl_rows_selected, { if (length(input$tbl_rows_selected)) detail_state$open <- TRUE })
  observeEvent(input$close_detail, {
    detail_state$open <- FALSE
    DT::selectRows(dataTableProxy("tbl"), NULL)
  })

  # ---- selected student ----------------------------------------------------
  sel <- reactive({
    req(detail_state$open)
    i <- input$tbl_rows_selected; req(i)
    d <- shown(); req(i <= nrow(d))
    d[i, ]
  })

  output$detail <- renderUI({
    if (!detail_state$open || !length(input$tbl_rows_selected)) return(NULL)
    r <- sel(); rs <- r$reasons[[1]]
    co <- cur(); h <- all_d(); h <- h[which(h$id_student == r$id_student), ]
    h <- h[order(h$cutoff_day), ]
    rank   <- sum(co$risk_score > r$risk_score, na.rm = TRUE) + 1
    change <- if (nrow(h) > 1 && r$cutoff_day > h$cutoff_day[1])
      sprintf("%+.2f", r$risk_score - h$risk_score[1]) else "n/a"
    tagList(
      div(class = "drawer-scrim"),
      div(class = "student-drawer", role = "dialog", `aria-labelledby` = "drawer-title",
        div(class = "drawer-head",
            div(div(class = "drawer-eyebrow", "Student ID"), h2(id = "drawer-title", class = "drawer-title", r$id_student)),
            actionButton("close_detail", svg_icon("close", 18, 2), class = "drawer-close", `aria-label` = "Close student details", title = "Close (Esc)")),
        div(class = "drawer-meta", sprintf("Estimated risk %.2f at %s", r$risk_score, week_label(r$cutoff_day)), status_pill(r$status)),
        div(class = "detail-stats",
            div(class = "detail-stat", strong(sprintf("%d of %d", as.integer(rank), nrow(co))), "Position by estimated risk (1 is highest)"),
            div(class = "detail-stat", strong(sprintf("%d of %d", sum(h$flagged), nrow(h))), "Checkpoints with an alert"),
            div(class = "detail-stat", strong(change), sprintf("Change in estimated risk since %s", week_label(h$cutoff_day[1])))),
        div(class = "drawer-section", "Areas of concern"),
        if (is.data.frame(rs) && nrow(rs))
          tags$ul(class = "reason-list", lapply(seq_len(nrow(rs)), function(k)
            tags$li(strong(paste0(tools::toTitleCase(rs$theme[k]), ":")), " ", rs$text[k])))
        else p(class = "text-muted", "No areas of concern were noted for this student."),
        if (isTRUE(r$bg_driven))
          div(class = "callout callout-caution",
              div("This estimate is influenced more by background factors than by activity or assessments.
                   Treat it with extra care, and do not act on background alone.")),
        div(class = "callout callout-info",
            div(strong("What to consider next. "), "Look at this student's engagement and assessment results, then decide what support, if any, is right.
                This is an estimate based on past patterns, not a confirmed outcome.")),
        div(class = "card plot-card", plotOutput("trend", height = 250)),
        div(class = "card plot-card", plotOutput("impact", height = 220))))
  })

  output$trend <- renderPlot({
    p <- pal(); r <- sel(); h <- all_d(); h <- h[which(h$id_student == r$id_student), ]
    h <- h[order(h$cutoff_day), ]
    h$State <- ifelse(h$flagged, "Alert", "No alert")
    ggplot(h, aes(cutoff_day, risk_score)) +
      geom_line(colour = p$neutral, linewidth = 1) +
      geom_point(aes(colour = State), size = 4) +
      scale_colour_manual(values = c(Alert = p$at_risk, `No alert` = p$ok), name = NULL) +
      scale_x_continuous(breaks = c(28, 56, 84), labels = week_axis) + ylim(0, 1) +
      labs(x = NULL, y = "Estimated risk", title = "Estimated risk over time") + plot_theme(p)
  }, bg = "transparent")

  output$impact <- renderPlot({
    p <- pal(); r <- sel()
    v <- data.frame(theme = c("Engagement", "Assessments", "Background"),
                    push  = c(r$eng, r$ass, r$bg))
    v$kind <- ifelse(v$theme == "Background", "Background (context only)",
                     ifelse(v$push > 0, "Raises estimated risk", "Lowers estimated risk"))
    v$theme <- factor(v$theme, levels = rev(v$theme))
    ggplot(v, aes(push, theme, fill = kind)) + geom_col(width = 0.55) +
      geom_vline(xintercept = 0, colour = p$muted) +
      scale_fill_manual(values = c(`Raises estimated risk` = p$at_risk, `Lowers estimated risk` = p$ok,
                                   `Background (context only)` = p$neutral), name = NULL) +
      labs(x = "Pushes toward higher estimated risk (right) or lower (left)", y = NULL,
           title = "What is influencing this estimate") + plot_theme(p)
  }, bg = "transparent")

  # ---- overview charts -----------------------------------------------------
  output$dist <- renderPlot({
    p <- pal(); d <- cur()
    d$Outcome <- ifelse(d$actual %in% TRUE, "Recorded as at risk", "Not at risk")
    g <- ggplot(d, aes(risk_score, fill = Outcome)) +
      geom_histogram(bins = 30, boundary = 0, colour = p$surface) +
      scale_fill_manual(values = c(`Recorded as at risk` = p$at_risk, `Not at risk` = p$ok), name = NULL) +
      labs(x = "Estimated risk", y = "Students") + plot_theme(p)
    if (any(d$flagged)) {
      thr <- min(d$risk_score[which(d$flagged)])
      g <- g + geom_vline(xintercept = thr, linetype = "dashed", colour = p$ink) +
        annotate("text", x = thr, y = Inf, label = " alert level", hjust = 0, vjust = 1.5, colour = p$ink)
    }
    g
  }, bg = "transparent")

  output$weeks <- renderPlot({
    p <- pal(); d <- all_d()
    long <- do.call(rbind, lapply(split(d, d$cutoff_day), function(x)
      data.frame(cutoff = x$cutoff_day[1],
                 group = c("Alerted and recorded at risk", "Alerted, not at risk", "Recorded at risk, no alert"),
                 n = c(sum(x$flagged & x$actual, na.rm = TRUE),
                       sum(x$flagged & !x$actual, na.rm = TRUE),
                       sum(!x$flagged & x$actual, na.rm = TRUE)))))
    long$cutoff <- factor(week_axis(long$cutoff), levels = week_axis(sort(unique(long$cutoff))))
    ggplot(long, aes(cutoff, n, fill = group)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c(`Alerted and recorded at risk` = p$ok, `Alerted, not at risk` = p$gold,
                                   `Recorded at risk, no alert` = p$at_risk), name = NULL) +
      labs(x = NULL, y = "Students") + plot_theme(p)
  }, bg = "transparent")

  # ---- fairness ------------------------------------------------------------
  fair <- reactive({
    d <- cur(); d[which(d$flagged & !is.na(d$bg_driven)), ]
  })

  output$fair_txt <- renderUI({
    f <- fair(); validate(need(nrow(f) > 0, "No students are flagged at this checkpoint."))
    b <- f[f$bg_driven, ]; o <- f[!f$bg_driven, ]
    div(class = "callout callout-info",
        div(sprintf("%d of %d students needing attention (%s) are influenced more by background than by activity and assessments.
                 Of those, %s were recorded as at risk, compared with %s of the others.",
                nrow(b), nrow(f), pct(nrow(b) / nrow(f)),
                pct(mean(b$actual, na.rm = TRUE)), pct(mean(o$actual, na.rm = TRUE)))))
  })

  output$fair_plot <- renderPlot({
    p <- pal(); f <- fair(); req(nrow(f) > 0)
    f$Outcome <- ifelse(f$actual %in% TRUE, "Recorded as at risk", "Not at risk")
    ggplot(f, aes(beh, bg, colour = Outcome)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = p$muted) +
      geom_point(alpha = 0.7, size = 2) +
      scale_colour_manual(values = c(`Recorded as at risk` = p$at_risk, `Not at risk` = p$ok), name = NULL) +
      labs(x = "Push from activity and assessments", y = "Push from background") + plot_theme(p)
  }, bg = "transparent")

  # ---- model performance ---------------------------------------------------
  get_mets <- reactive({
    if (DATA_MODE == "preview") {
      validate(need(file.exists(PREVIEW_METRICS), sprintf(
        "Preview metrics are unavailable: expected '%s'.", PREVIEW_METRICS
      )))
      m <- tryCatch(
        read.csv(PREVIEW_METRICS, check.names = FALSE),
        error = function(error) validate(need(FALSE, sprintf(
          "Preview metrics file '%s' could not be read: %s", PREVIEW_METRICS, error$message
        )))
      )
    } else {
      m <- mets$find(fields = '{"_id":0}')
    }
    m <- require_fields(m, c("cutoff", "model", "auc"), "Model metrics")
    if ("model" %in% names(m)) m$model <- as.character(m$model)
    m
  })

  output$perf <- renderPlot({
    p <- pal(); m <- get_mets()
    ggplot(m, aes(cutoff, auc, colour = model)) +
      geom_line(linewidth = 1) + geom_point(size = 3) +
      scale_x_continuous(breaks = c(28, 56, 84), labels = week_axis) +
      scale_colour_manual(values = c(p$ok, p$gold, p$slate, p$deep)[seq_along(unique(m$model))], name = NULL) +
      labs(x = NULL, y = "AUC on held-out test students") + plot_theme(p)
  }, bg = "transparent")
  output$perf_tbl <- renderTable({
    m <- get_mets(); m[] <- lapply(m, function(x) if (is.numeric(x)) round(x, 3) else x)
    if ("cutoff" %in% names(m)) m$cutoff <- as.integer(m$cutoff)   # display only: whole days
    nice <- c(cutoff = "Cutoff (day)", model = "Model", auc = "AUC")
    names(m) <- ifelse(names(m) %in% names(nice), nice[names(m)], names(m))
    m
  })

  # ---- export (unchanged: file name and columns stay the same) --------------
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