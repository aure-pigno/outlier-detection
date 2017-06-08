context("generate_normal_variable")

test_that("the generate data approximate the mean value", {
  samples= 100000
  mean_value = 0
  sd_value = 1 
  data = generate_normal_variable(samples, mean_value , sd_value)
  expect_true(all.equal(mean_value, mean(data),tolerance=.1))
})

test_that("the generate data approximate the sd value", {
  samples= 100000
  mean_value = 0
  sd_value = 1 
  data = generate_normal_variable(samples, mean_value , sd_value)
  expect_true(all.equal(sd_value, sd(data),tolerance=.1))
})

test_that("the values are following a normal distribution", {
  samples= 1000000
  mean_value = 0
  sd_value = 1 
  data = generate_normal_variable(samples, mean_value , sd_value)
  expect_true(ks.test(unique(data),"pnorm", mean=mean_value, sd=sd_value)$p.value>0.05) 
})
