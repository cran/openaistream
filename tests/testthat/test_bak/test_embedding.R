#test_embedding
test_that("embedding",{
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  embed<-handle_openai$embeddings(model = "text-embedding-ada-002", input = "who are you?",verbosity = 3)
  expect_type(embed$data$embedding[[1]],"double")

  embed<-handle_openai$embeddings(model = "text-embedding-ada-002", input = "who are you?",verbosity = 4)
  expect_true(!embed$success)
})
