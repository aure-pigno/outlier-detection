context("generate_uniform_variable")

test_that("no value is under the lower bound", {
  samples= 100000
  lower_bound = -1 
  higher_bound = 1 
  data = generate_uniform_variable(samples, lower_bound , higher_bound)
  expect_true(min(data)>= lower_bound) 
})

test_that("no value is over the higher bound", {
  samples= 100000
  lower_bound = -1 
  higher_bound = 1 
  data = generate_uniform_variable(samples, lower_bound , higher_bound)
  expect_true(min(data)<= higher_bound) 
})

test_that("the values are uniformly distributed", {
  samples= 1000000
  lower_bound = -1 
  higher_bound = 1 
  data = generate_uniform_variable(samples, lower_bound , higher_bound)
  expect_true(ks.test(unique(data),"punif",lower_bound,higher_bound)$p.value>0.05) 
})
