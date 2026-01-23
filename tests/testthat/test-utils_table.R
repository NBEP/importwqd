test_that("column_styles works", {
  list_out <- list(
    Site_Name = reactable::colDef(
      rowHeader = TRUE,
      sticky = "left",
      style = list(borderRight = "1px solid #eee")
    ),
    score_str = reactable::colDef(
      name = "Score",
      show = TRUE,
      style = htmlwidgets::JS(
        "function(rowInfo) {
          if (rowInfo.values['score_str'] == 'Excellent') {
            return { backgroundColor: '#afccec' }
          } else if (rowInfo.values['score_str'] == 'Good' |
              rowInfo.values['score_str'] == 'Meets Criteria') {
            return { backgroundColor: '#cbe4e7' }
          } else if (rowInfo.values['score_str'] == 'Fair') {
            return { backgroundColor: '#ffffe0' }
          } else if (rowInfo.values['score_str'] == 'Poor' |
              rowInfo.values['score_str'] == 'Does Not Meet Criteria') {
            return { backgroundColor: '#f9cfb4' }
          } else if (rowInfo.values['score_str'] == 'No Data Available' |
              rowInfo.values['score_str'] == 'No Threshold Established') {
            return { fontStyle: 'italic' }
          }
        }"
      )
    ),
    Group = reactable::colDef(
      style = list(borderRight = "1px solid #eee")
    ),
    score_num = reactable::colDef(
      name = "Value",
      na = "-",
      header = htmlwidgets::JS(
        "function(column, state) {
          const { col_title } = state.meta
          return col_title
        }"
      )
    )
  )

  expect_equal(
    column_styles(tst$data_score),
    list_out
  )
})
