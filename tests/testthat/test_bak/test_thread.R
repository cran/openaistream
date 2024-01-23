test_that("thread",{
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  thc<-handle_openai$threads_create()
  expect_contains(names(thc),"id")
  thr<-handle_openai$threads_retrieve(thc$id)
  expect_contains(names(thr),"id")
  thm<-handle_openai$threads_modify(thr$id,metadata=list(test="test"))
  expect_equal(thm$metadata$test,"test")
  thd<-handle_openai$threads_delete(thr$id)
  expect_true(thd$deleted)
  #error test
  thc<-handle_openai$threads_create(verbosity = 4)
  expect_true(!thc$success)
  thr<-handle_openai$threads_retrieve(thc$id,verbosity = 4)
  expect_true(!thr$success)
  thm<-handle_openai$threads_modify(thr$id,metadata=list(test="test"),verbosity = 4)
  expect_true(!thm$success)
  thd<-handle_openai$threads_delete(thr$id,verbosity = 4)
  expect_true(!thd$success)
})
