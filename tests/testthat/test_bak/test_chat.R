#test_chat
test_that("chat",{
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  # chat test not stream
  streamlg <- handle_openai$get_completions_query(
    prompt = "Please explain the World Wars?",
    model = "davinci-002",
    stream = FALSE,
    max_tokens = 20,
    verbosity = 0,n=3
  )
  expect_equal(nrow(streamlg$vres),3)
  #test error
  streamlg <- handle_openai$get_completions_query(
    prompt = "Please explain the World Wars?",
    model = "davinci-002",
    stream = FALSE,
    max_tokens = 20,
    verbosity = 4,n=3
  )
  expect_true(!streamlg$success)
  streamlg <- handle_openai$get_chat_completions_query(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = F,
    max_tokens = 10,n=3
  )
  expect_equal(nrow(streamlg$vres),3)
  streamlg <- handle_openai$get_chat_completions_query(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = F,
    max_tokens = 10,n=3,verbosity = 4
  )
  expect_true(!streamlg$success)
})
