context("change_class")

test_that("the resulting classes are different from the original classes", {
  expect_false(all(dataset1$class == change_class(dataset1, proportion=.1)$data$class))
})

test_that("the proportion of changed class is correct", {
  proportion=.1
  expect_equal(sum(change_class(dataset1, proportion=proportion)$is_modified), round(nrow(dataset1)*proportion))
})