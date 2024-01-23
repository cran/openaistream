test_that("assistants",{
  skip_on_cran()
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  ass<-handle_openai$assistants$create(name="cor_flag",
                                   model="gpt-4",
                                   instructions="I am HealthNutritionAssistant, designed to provide professional and accurate health and nutrition advice.
                                   My primary function is to answer health or nutrition related questions using the uploaded file “assfile.csv,” which contains common health and nutrition questions and their answers.
                                   When a user asks a health or nutrition question, I'll first consult this file.
                                   If it contains a relevant answer, I will use it to respond.
                                   If the file lacks a direct answer, I'll offer general advice based on my broad knowledge and suggest consulting a professional doctor or nutritionist for specific issues.
                                   My tone is friendly and professional.
                                   I'll always clarify the source of my information, like “According to our health FAQs...” I encourage users to seek professional medical advice for specific health concerns.
                                   I do not collect or store personal health information.
                                   My responses are based on the file's content and current best medical practices. I am regularly updated to reflect the latest medical research and health guidelines."
  )
  expect_equal(ass$name,"cor_flag")
  assr<-handle_openai$assistants$retrieve(ass$id)
  expect_equal(assr$name,"cor_flag")
  assm<-handle_openai$assistants$modify(assr$id,model="gpt-4-1106-preview",tools=list(list(type="retrieval"),list(type="code_interpreter")))
  expect_equal(assm$model,"gpt-4-1106-preview")
  assl<-handle_openai$assistants$list()
  expect_contains(assl$data$name,"cor_flag")
  #ass file
  train_file_path<-system.file("exdata","assfile.csv", package = "openaistream")
  file_id <- handle_openai$files$upload(path = train_file_path,purpose = "assistants",verbosity = 0)
  #this is a openai bug,the first is success ,but return is error,so we neet run two
  assfc<-handle_openai$assistants$file_create(assm$id,file_id$id,verbosity = 0)
  assfc<-handle_openai$assistants$file_create(assm$id,file_id$id,verbosity = 0)
  expect_equal(assfc$object,"assistant.file")
  assfr<-handle_openai$assistants$file_retrieve(assm$id,file_id$id)
  expect_equal(assfr$object,"assistant.file")
  assfl<-handle_openai$assistants$file_list(assm$id)
  expect_contains(assfl$data$id,file_id$id)
  assfd<-handle_openai$assistants$file_delete(assm$id,file_id$id)
  expect_true(assfd$deleted)
  del_res<-handle_openai$files$delete(file_id$id, verbosity = 0)
  expect_true(del_res$deleted)
  assd<-handle_openai$assistants$delete(ass$id)
  expect_true(assd$deleted)

  #error test
  ass<-handle_openai$assistants$create(name="cor_flag",
                                       model="gpt-4",
                                       instructions="I am HealthNutritionAssistant",verbosity = 4
  )
  expect_true(!ass$success)
  #error test
  assr<-handle_openai$assistants$retrieve(NULL,verbosity = 4)
  expect_true(!assr$success)
  assm<-handle_openai$assistants$modify(assr$id,model="gpt-4-1106-preview",tools=list(list(type="retrieval"),list(type="code_interpreter")),verbosity = 4)
  expect_true(!assm$success)
  assd<-handle_openai$assistants$delete(ass$id,verbosity = 4)
  expect_true(!assd$success)
  assl<-handle_openai$assistants$list(verbosity = 4)
  expect_true(!assl$success)
  assfc<-handle_openai$assistants$file_create(assm$id,file_id$id,verbosity = 4)
  expect_true(!assfc$success)
  assfr<-handle_openai$assistants$file_retrieve(assm$id,file_id$id,verbosity = 4)
  expect_true(!assfr$success)
  assfl<-handle_openai$assistants$file_list(assm$id,verbosity = 4)
  expect_true(!assfl$success)
  assfd<-handle_openai$assistants$file_delete(assm$id,file_id$id,verbosity = 4)
  expect_true(!assfd$success)
})
