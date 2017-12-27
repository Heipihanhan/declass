library(testthat)
library(declass)


context("declass welcome page")
test_that("declass_welcome returns content in html or string format", {
  expect_is(declass_welcome("html"), "xml_document")
  expect_is(declass_welcome("string"), "character")
})



context("declass fields")
test_that("declass_fields returns content in string or list format", {
  expect_is(declass_fields("string"), "character")
  expect_is(declass_fields("list"), "list")
})



context("declass collections")
test_that("declass_collection returns content in character or list format", {
  expect_is(declass_collection("string"), "character")
  expect_is(declass_collection("list"), "list")
})



context("declass collection entity")
test_that("declass_collection_entity returns entities for a given collection", {
  expect_equal(declass_collection_entity("cpdoc"), c("countries", "persons", "topics"))
  expect_equal(declass_collection_entity("clinton"), c("countries", "persons", "classifications"))
  expect_equal(declass_collection_entity("kissinger"), c("countries", "persons", "topics"))
  expect_equal(declass_collection_entity("statedeptcables"), c("countries", "persons", "topics", "classifications"))
  expect_equal(declass_collection_entity("frus"), c("countries", "persons", "topics", "classifications"))
  expect_equal(declass_collection_entity("ddrs"), c("countries", "persons", "topics", "classifications"))
  expect_equal(declass_collection_entity("cabinet"), c("countries", "persons", "topics"))
  expect_equal(declass_collection_entity("pdb"), c("countries", "persons", "topics", "classifications"))
  expect_error(declass_collection_entity())
})



context("declass entity data")
test_that("declass_entity_data returns fields of data regarding count, id, and name", {
  expect_equal(names(declass_entity_data("kissinger", "topics")), c("count", "id", "name"))
})



context("declass query random ids")
test_that("declass_query_random_ids returns data regarding the id field with the specified number of ids", {
  expect_equal(names(declass_query_random_ids(2)), c("id"))
  expect_equal(nrow(declass_query_random_ids(5)), 5)
})



context("declass query id data")
test_that("declass_query_id_data returns document in a list, containing entities of count and results", {
  expect_is(declass_query_id_data("Clinton-74931"), "list")
  expect_equal(length(declass_query_id_data("Clinton-74931")), 2)
  expect_equal(declass_query_id_data("Clinton-74931")[[1]], 1)
  expect_equal(colnames(declass_query_id_data("Clinton-74931")[[2]][1:4]), c("body_summary", "cable_references", "classification", "cable_type"))
  expect_equal(colnames(declass_query_id_data("1973JERUSA01275")[[2]][1:4]), c("body_summary", "cable_references", "classification", "cable_type"))
})



context("declass query random ids data")
test_that("declass_query_random_ids_data returns document in a list, containing entities of count and results ", {
  expect_is(declass_query_random_ids_data(5), "list")
  expect_equal(length(declass_query_random_ids_data(5)), 2)
  expect_equal(length(declass_query_random_ids_data(10)), 2)
  expect_equal(colnames(declass_query_random_ids_data(5)[[2]][1:4]), c("body_summary", "cable_references", "classification", "cable_type"))
  expect_equal(colnames(declass_query_random_ids_data(10)[[2]][1:4]), c("body_summary", "cable_references", "classification", "cable_type"))
})




