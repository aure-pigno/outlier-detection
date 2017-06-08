context("introduce_value_increased_outliers")

test_that("the resulting dataset is different from the original dataset", {
  affected_rows = nrow(dataset1)/3
  expect_false(all(dataset1 ==  introduce_value_increased_outliers(dataset1, affected_rows/nrow(dataset1), .1, .2,.7)$data))
})

test_that("the proportion of changed observations is correct", {
  proportion=.1
  expect_equal(sum(introduce_value_increased_outliers(dataset1, proportion, .1, .2, .7)$is_modified), round(nrow(dataset1)*proportion))
})