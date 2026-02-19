# simple_graph works

    Code
      simple_graph(fig, df_in, "Site_Name")

---

    Code
      simple_graph(fig, df_in, "Site_Name", add_lines = TRUE)

# graph_style works

    Code
      suppressMessages(graph_style(fig_in, "title", "y-axis label", c(0, 1)))
    Condition
      Warning:
      No trace type specified and no positional attributes specified
    Message
      No trace type specified:
        Based on info supplied, a 'scatter' trace seems appropriate.
        Read more about this trace type -> https://plotly.com/r/reference/#scatter
      No scatter mode specifed:
        Setting the mode to markers
        Read more about this attribute -> https://plotly.com/r/reference/#scatter-mode

# plot_thresholds works

    Code
      plot_thresholds(df_in, thresh, date_range, y_range)

---

    Code
      plot_thresholds(df_in, thresh, date_range, y_range)

---

    Code
      plot_thresholds(df_in, thresh, date_range, y_range)

---

    Code
      suppressMessages(plot_thresholds(df_in, NULL, date_range, y_range))
    Condition
      Warning:
      No trace type specified and no positional attributes specified
    Message
      No trace type specified:
        Based on info supplied, a 'scatter' trace seems appropriate.
        Read more about this trace type -> https://plotly.com/r/reference/#scatter
      No scatter mode specifed:
        Setting the mode to markers
        Read more about this attribute -> https://plotly.com/r/reference/#scatter-mode

# add_gam works

    Code
      add_gam(fig_in, df_in)

