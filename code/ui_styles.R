# App-wide CSS.
#
# Lifted out of ui.R so the navbar structure there stays readable. Passed to
# page_navbar() via its `header` argument; htmltools hoists the tags$head()
# contents into the document head, so the rules still apply globally.

app_styles <- function() {
  tags$head(
    tags$style(HTML("
      body {
        background: #1E242C;
        color: #E6EEF5;
      }
      h1, h2, h3, h4, h5, h6, p, label, .navbar-brand, .nav-link, .form-label {
        color: #E6EEF5 !important;
      }
      .navbar, .navbar-dark {
        background-color: #262D36 !important;
        border-bottom: 1px solid #3A434F;
      }
      .card {
        background-color: #252C35;
        border: 1px solid #3A434F;
        box-shadow: 0 10px 30px rgba(0,0,0,0.25);
      }
      .card-title, .card-text {
        color: #E6EEF5 !important;
      }
      .form-control, .selectize-input, .selectize-dropdown, .form-select {
        background-color: #222831 !important;
        color: #E6EEF5 !important;
        border: 1px solid #3A434F !important;
      }
      .selectize-dropdown-content {
        background-color: #222831;
      }
      .value-box {
        border: 1px solid #3A434F;
      }
      .lp-hero {
        background: linear-gradient(130deg, #252C35 0%, #2A313B 55%, #313946 100%);
        border: 1px solid #3A434F;
        border-radius: 16px;
        padding: 24px 28px;
        margin-bottom: 18px;
      }
      .lp-hero h2 {
        margin-bottom: 6px;
        font-weight: 700;
      }
      .lp-hero p {
        color: #A9B7C6 !important;
        margin-bottom: 0;
      }
      .lp-grid {
        margin-top: 8px;
      }
      .lp-stat-card {
        border: 1px solid #434D5A !important;
        border-radius: 14px !important;
        box-shadow: 0 10px 22px rgba(0, 0, 0, 0.22);
      }
      .lp-stat-card .value-box-title {
        font-size: 0.9rem;
        letter-spacing: 0.04em;
        text-transform: uppercase;
      }
      .lp-stat-card .value-box-value {
        font-size: 2rem;
        font-weight: 700;
      }
      .lp-stat-card--readiness .value-box-value {
        font-size: 1.45rem;
      }
      .analysis-plot-card {
        min-height: 700px;
      }
      .riegel-shell {
        padding: 10px 8px;
      }
      .riegel-hero {
        background: linear-gradient(130deg, #252C35 0%, #29313B 45%, #313A45 100%);
        border: 1px solid #3A434F;
        border-radius: 16px;
        padding: 22px 26px;
        margin-bottom: 16px;
      }
      .riegel-hero h3 {
        margin-bottom: 6px;
        font-weight: 700;
      }
      .riegel-hero p {
        margin-bottom: 0;
        color: #A9B7C6 !important;
      }
      .riegel-metric-card {
        background-color: #222831 !important;
        border: 1px solid #3A434F !important;
        border-radius: 14px !important;
        min-height: 150px;
      }
      .riegel-metric-label {
        color: #9DB0C4 !important;
        font-size: 0.8rem;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        margin-bottom: 8px;
      }
      .riegel-metric-value {
        font-size: 2rem;
        font-weight: 700;
        color: #E6EEF5;
      }
      .riegel-metric-sub {
        color: #9AA4B2 !important;
        margin-top: 8px;
      }
      .riegel-details {
        background-color: #222831;
        border: 1px solid #3A434F;
        border-radius: 14px;
        padding: 18px 20px;
        margin-top: 14px;
      }
      .baseline-step {
        background-color: #252C35;
        border: 1px solid #3A434F;
        border-radius: 16px;
        padding: 18px 20px;
        margin-bottom: 16px;
      }
      .baseline-step h4 {
        margin-bottom: 6px;
      }
      .baseline-step p {
        color: #A9B7C6 !important;
        margin-bottom: 14px;
      }
      .fhm-callout {
        background: linear-gradient(130deg, #222a34 0%, #273240 100%);
        border: 1px solid #3A434F;
        border-radius: 12px;
        padding: 14px 16px;
        margin-bottom: 12px;
      }
      .fhm-callout p {
        color: #C8D3DF !important;
        margin-bottom: 8px;
      }
      .fhm-callout p:last-child {
        margin-bottom: 0;
      }
      .fhm-metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
        gap: 10px;
        margin: 8px 0 14px 0;
      }
      .fhm-metric-card {
        background-color: #222831;
        border: 1px solid #3A434F;
        border-radius: 12px;
        padding: 10px 12px;
      }
      .fhm-metric-label {
        color: #9DB0C4 !important;
        font-size: 0.78rem;
        letter-spacing: 0.04em;
        text-transform: uppercase;
      }
      .fhm-metric-value {
        color: #E6EEF5;
        font-size: 1.35rem;
        font-weight: 700;
        margin-top: 4px;
      }
      .riegel-details h5 {
        margin-bottom: 12px;
      }
      .riegel-details p {
        color: #C8D3DF !important;
        margin-bottom: 6px;
      }
      .nav-tabs .nav-link {
        color: #A9B7C6 !important;
      }
      .nav-tabs .nav-link.active {
        background-color: #252C35;
        border-color: #3A434F #3A434F #252C35;
        color: #E6EEF5 !important;
      }
      .text-muted {
        color: #9AA4B2 !important;
      }
      .table {
        color: #E6EEF5;
      }
      .table thead th {
        background-color: #222831;
        border-bottom: 1px solid #3A434F;
      }
      .table tbody tr {
        border-color: #3A434F;
      }
      .small-box {
        font-size: 24px;
        background-color: #00a65a !important;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.1);
      }
      .small-box h3 {
        font-size: 34px;
        font-weight: bold;
        margin: 0;
      }
      .small-box p {
        font-size: 18px;
        margin-bottom: 10px;
      }
      .small-box .icon {
        font-size: 50px;
        margin-left: 10px;
      }
      .coros-badge {
        display: inline-block;
        margin-top: 8px;
        padding: 3px 10px;
        border-radius: 999px;
        font-size: 0.72rem;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        font-weight: 600;
      }
      .coros-badge--live {
        background-color: rgba(35, 209, 139, 0.14);
        border: 1px solid #23D18B;
        color: #7BE8B8 !important;
      }
      .coros-badge--pending {
        background-color: rgba(249, 200, 70, 0.12);
        border: 1px solid #F9C846;
        color: #F9D877 !important;
      }
      .coros-roadmap {
        color: #C8D3DF;
        margin-bottom: 14px;
        padding-left: 20px;
      }
      .coros-roadmap li {
        margin-bottom: 7px;
      }
      .coros-roadmap code {
        background-color: #1B2028;
        border: 1px solid #3A434F;
        border-radius: 4px;
        padding: 1px 5px;
        color: #5FA8D3;
      }
      .dropdown-menu {
        background-color: #262D36;
        border: 1px solid #3A434F;
      }
      .dropdown-item {
        color: #C8D3DF !important;
      }
      .dropdown-item:hover, .dropdown-item:focus {
        background-color: #313946;
        color: #E6EEF5 !important;
      }
      .dropdown-item.active {
        background-color: #313946;
        color: #E6EEF5 !important;
      }
      .coros-metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(165px, 1fr));
        gap: 12px;
        margin-bottom: 18px;
      }
      .coros-metric {
        background-color: #222831;
        border: 1px solid #3A434F;
        border-radius: 14px;
        padding: 14px 16px;
      }
      .coros-metric-label {
        color: #9DB0C4 !important;
        font-size: 0.75rem;
        letter-spacing: 0.06em;
        text-transform: uppercase;
      }
      .coros-metric-value {
        font-size: 1.9rem;
        font-weight: 700;
        line-height: 1.15;
        margin-top: 4px;
      }
      .coros-metric-sub {
        color: #8B97A6 !important;
        font-size: 0.8rem;
        margin-top: 4px;
      }
      .coros-signal {
        background-color: #222831;
        border: 1px solid #3A434F;
        border-left-width: 4px;
        border-radius: 10px;
        padding: 12px 16px;
        margin-bottom: 10px;
      }
      .coros-signal--good { border-left-color: #23D18B; }
      .coros-signal--watch { border-left-color: #F9C846; }
      .coros-signal--flag { border-left-color: #E84855; }
      .coros-signal-head {
        display: flex;
        align-items: baseline;
        gap: 12px;
        flex-wrap: wrap;
      }
      .coros-signal-metric {
        color: #E6EEF5;
        font-weight: 600;
        min-width: 130px;
      }
      .coros-signal-value {
        color: #E6EEF5;
        font-size: 1.25rem;
        font-weight: 700;
      }
      .coros-signal-note {
        color: #A9B7C6 !important;
        font-size: 0.88rem;
        margin-top: 5px;
      }
      .coros-badge--good {
        background-color: rgba(35, 209, 139, 0.14);
        border: 1px solid #23D18B;
        color: #7BE8B8 !important;
      }
      .coros-badge--watch {
        background-color: rgba(249, 200, 70, 0.12);
        border: 1px solid #F9C846;
        color: #F9D877 !important;
      }
      .coros-badge--flag {
        background-color: rgba(232, 72, 85, 0.14);
        border: 1px solid #E84855;
        color: #F79098 !important;
      }
      .coros-badge {
        margin-top: 0;
      }
      .coros-range-note {
        margin-top: 6px;
        border-top: 1px solid #3A434F;
        padding-top: 10px;
      }
      .coros-cov-title {
        color: #9DB0C4 !important;
        font-size: 0.72rem;
        letter-spacing: 0.05em;
        text-transform: uppercase;
        margin-bottom: 6px;
      }
      .coros-cov-row {
        display: flex;
        justify-content: space-between;
        gap: 8px;
        font-size: 0.8rem;
        padding: 2px 0;
      }
      .coros-cov-name { color: #A9B7C6; }
      .coros-cov-n { color: #E6EEF5; font-variant-numeric: tabular-nums; }
      .coros-cov-none { color: #6B7785; }
      .coros-cov-foot {
        color: #7C8896 !important;
        font-size: 0.72rem;
        margin-top: 9px;
        line-height: 1.35;
      }
    "))
  )
}
