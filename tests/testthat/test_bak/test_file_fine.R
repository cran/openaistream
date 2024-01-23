test_that("test get_model_retrieve",{
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1", 10890)
  }
  res<-handle_openai$get_model_retrieve(model = "gpt-3.5-turbo",verbosity = 0)
  expect_equal(res$id,"gpt-3.5-turbo")
  res<-handle_openai$get_model_retrieve(model = "gpt-3.5-turbo",verbosity = 200)
  expect_true(!res$success)
})

test_that("test fileload and fine train",{
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  train_file_path<-system.file("exdata","train.jsonl", package = "openaistream")

  file_id <- handle_openai$files_upload(path = train_file_path,purpose = "fine-tune")
  expect_equal(file_id$filename,"train.jsonl")
  #error test
  res<-handle_openai$files_upload(path = train_file_path,purpose = "fine-tune",verbosity = 5)
  expect_true(!res$success)

  file_list<-handle_openai$files_get_list(verbosity = 0)
  expect_null(file_list$success)
  #error test
  res<-handle_openai$files_get_list(verbosity = 5)
  expect_true(!res$success)

  file_mes<-handle_openai$files_retrieve(file_id=file_id$id, verbosity = 0)
  expect_equal(file_mes$id,file_id$id)
  #error test
  res<-handle_openai$files_retrieve(file_id=file_id$id, verbosity = 5)
  expect_true(!res$success)

  #job create for cancel job
  job<-handle_openai$fine_tuning_jobs_create(model = "gpt-3.5-turbo",training_file = file_id$id)
  expect_equal(job$training_file,file_id$id)

  #job create for error test
  res<-handle_openai$fine_tuning_jobs_create(model = "gpt-3.5-turbo",training_file = file_id$id,verbosity = 4)
  expect_true(!res$success)

  job_events<-handle_openai$fine_tuning_jobs_events(job$id, verbosity = 0)
  expect_contains(names(job_events$data),"data")

  #error test
  res<-handle_openai$fine_tuning_jobs_events(job$id, verbosity = 4)
  expect_true(!res$success)

  job_retrieve<-handle_openai$fine_tuning_jobs_retrieve(job$id, verbosity = 0)
  expect_equal(job_retrieve$id,job$id)

  #error test
  res<-handle_openai$fine_tuning_jobs_retrieve(job$id, verbosity = 4)
  expect_true(!res$success)

  job_list<-handle_openai$fine_tuning_jobs_list()
  expect_contains(names(job_list),"data")

  job_list<-handle_openai$fine_tuning_jobs_list(limit = 2, verbosity = 3)
  expect_contains(names(job_list),"data")

  #error test
  res<-handle_openai$fine_tuning_jobs_list(verbosity = 4)
  expect_true(!res$success)

  job_status<-handle_openai$fine_tuning_jobs_cancel(job$id)
  expect_equal(job_status$id,job$id)

  #error test
  res<-handle_openai$fine_tuning_jobs_cancel("ftjob-VxJMKqY0gTmGPT")
  expect_true(!res$success)

  #error test
  del_res<-handle_openai$files_delete(file_id$id, verbosity = 0)
  expect_true(del_res$deleted)

  #error test
  res<-handle_openai$files_delete(file_id$id, verbosity = 5)
  expect_true(!res$success)

  #error test
  res<-handle_openai$files_upload(path = NULL,purpose = "fine-tune",verbosity = 3)
  expect_true(!res$success)
  #error test
  res<-handle_openai$files_upload(path = "/tmp/sadasdsdad",purpose = "fine-tune",verbosity = 3)
  expect_true(!res$success)
})



