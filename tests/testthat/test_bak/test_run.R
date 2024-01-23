test_that("run",{
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  thc<-handle_openai$threads_create(verbosity = 3)
  expect_contains(names(thc),"id")
  train_file_path<-system.file("exdata","assfile.csv", package = "openaistream")
  file_id <- handle_openai$files_upload(path = train_file_path,purpose = "assistants",verbosity = 0)
  ass<-handle_openai$assistants_create(name="cor_flag",
                                       model="gpt-4-1106-preview",
                                       instructions="I am HealthNutritionAssistant, designed to provide professional and accurate health and nutrition advice.
                                   My primary function is to answer health or nutrition related questions using the uploaded file “assfile.csv,” which contains common health and nutrition questions and their answers.
                                   When a user asks a health or nutrition question, I'll first consult this file.
                                   If it contains a relevant answer, I will use it to respond.
                                   If the file lacks a direct answer, I'll offer general advice based on my broad knowledge and suggest consulting a professional doctor or nutritionist for specific issues.
                                   My tone is friendly and professional.
                                   I'll always clarify the source of my information, like “According to our health FAQs...” I encourage users to seek professional medical advice for specific health concerns.
                                   I do not collect or store personal health information.
                                   My responses are based on the file's content and current best medical practices. I am regularly updated to reflect the latest medical research and health guidelines.",
                                   tools=list(list(type="retrieval"),list(type="code_interpreter")),
                                   file_ids=list(file_id$id),verbosity = 3
  )
  ass2<-handle_openai$assistants_create(name="cor_flag2",
                                       model="gpt-4-1106-preview",
                                       instructions="You are a weather bot. Use the provided functions to answer questions.",
                                       tools=list(list(type="function",
                                                       "function" = list(name="getCurrentWeather",
                                                                         description="Get the weather in location",
                                                                         parameters=list(
                                                                           type="object",
                                                                           properties=list(
                                                                             location=list(type="string",description="The city and state e.g. San Francisco, CA"),
                                                                             unit=list(type="string",enum=list("c", "f"))
                                                                             ),
                                                                           required=list("location")
                                                                         ))),
                                                  list(type="function",
                                                       "function" = list(name="getNickname",
                                                                         description="Get the nickname of a city",
                                                                         parameters=list(
                                                                           type="object",
                                                                           properties=list(
                                                                             location=list(type="string",description="The city and state e.g. San Francisco, CA")
                                                                           ),
                                                                           required=list("location")
                                                                         ))
                                                       )),verbosity = 3
  )
  expect_equal(ass$model,"gpt-4-1106-preview")
  runct<-handle_openai$runs_create_tread(ass$id,thread=list(messages=list(list(role="user",content="What foods are good for heart health?"))),verbosity = 3)
  expect_equal(runct$object,"thread.run")
  runl<-handle_openai$runs_list(runct$thread_id)
  expect_contains(runl$data$thread_id,runct$thread_id)
  Sys.sleep(10)
  runls<-handle_openai$runs_steps_list(runct$thread_id,runct$id,limit=1)
  expect_equal(nrow(runls$data),1)
  runr<-handle_openai$runs_retrieve(runct$thread_id,runct$id)
  for(i in 1:4){
    if(runr$status!="completed"){
      Sys.sleep(5)
      runr<-handle_openai$runs_retrieve(runct$thread_id,runct$id)
    }else{
      break
    }
  }
  expect_equal(runr$object,"thread.run")
  runrs<-handle_openai$runs_steps_retrieve(runct$thread_id,runct$id,runls$data$id[1])
  expect_equal(runrs$object,"thread.run.step")
  Sys.sleep(15)
  runm<-handle_openai$runs_modify(runct$thread_id,runct$id,metadata=list(test="test"),verbosity = 3)
  expect_equal(runm$metadata$test,"test")

  mesc<-handle_openai$messages_create(runct$thread_id,role = "user",content="beijing weather?")
  runmls<-handle_openai$messages_list(runct$thread_id)
  runct<-handle_openai$runs_create(runct$thread_id,ass2$id,verbosity = 3)
  runrs<-handle_openai$runs_retrieve(runct$thread_id,runct$id)
  for(i in 1:10){
    if(runrs$status!="completed"){
      Sys.sleep(5)
      runrs<-handle_openai$runs_retrieve(runct$thread_id,runct$id)
    }else{
      break
    }
  }

  runto<-handle_openai$runs_submit_tool_outputs(runct$thread_id,runct$id,tool_outputs = list(list(tool_call_id=runrs$required_action$submit_tool_outputs$tool_calls$id,output="BJ")),verbosity = 3)
  expect_equal(runto$object,"thread.run")
  runmls<-handle_openai$messages_list(runct$thread_id)

  del_res<-handle_openai$files_delete(file_id$id, verbosity = 0)
  expect_true(del_res$deleted)
  thd<-handle_openai$threads_delete(thc$id)
  expect_true(thd$deleted)
  assd<-handle_openai$assistants_delete(ass$id)
  expect_true(assd$deleted)
  assd<-handle_openai$assistants_delete(ass2$id)
  expect_true(assd$deleted)
})
