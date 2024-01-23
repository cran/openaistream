test_that("long time test",{
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  train_file_path<-system.file("exdata","train.jsonl", package = "openaistream")
  file_id <- handle_openai$files_upload(path = train_file_path,purpose = "fine-tune")
  # the pretest
  #job create for delete job
  job2<-handle_openai$fine_tuning_jobs_create(model = "gpt-3.5-turbo",training_file = file_id$id)
  #test jobs_delete
  Sys.sleep(200)
  #error test
  for(i in 1:20){
    Sys.sleep(20)
    job_retrieve<-handle_openai$fine_tuning_jobs_retrieve(job2$id, verbosity = 0)
    print(paste0("\n",i," retry ",job_retrieve$status))
    if(job_retrieve$status=="succeeded"){
      res<-handle_openai$fine_tuning_jobs_delete(job_retrieve$fine_tuned_model)
      expect_true(res$deleted)
      break
    }
  }
  #error test
  res<-handle_openai$fine_tuning_jobs_delete("gpt-3.5-turbo-sadsada")
  expect_true(!res$success)
  del_res<-handle_openai$files_delete(file_id$id, verbosity = 0)
})
