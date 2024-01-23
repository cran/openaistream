test_that("init the openai api", {
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  expect_type(handle_openai, "environment")
  fc<-c("runs_steps_list","runs_steps_retrieve","runs_create_tread","runs_cancel",
        "runs_submit_tool_outputs","runs_list","runs_modify","runs_retrieve","runs_create",
        "messages_file_list","messages_file_retrieve","messages_list","messages_modify",
        "messages_retrieve","messages_create","threads_delete","threads_modify",
        "threads_retrieve","threads_create","assistants_file_list",
        "assistants_file_delete","assistants_file_retrieve",
        "assistants_file_create","assistants_list","assistants_delete",
        "assistants_modify","assistants_retrieve","assistants_create","images_variations",
        "images_edits","images_generations","audio_translations","audio_transcriptions",
        "audio_speech","embeddings","fine_tuning_jobs_events","fine_tuning_jobs_cancel",
        "fine_tuning_jobs_retrieve","fine_tuning_jobs_list","fine_tuning_jobs_create",
        "files_retrieve","files_delete","files_get_list","files_upload","get_chat_completions_query",
        "get_completions_query","fine_tuning_jobs_delete","get_model_retrieve","get_model_list",
        "set_proxy")
  expect_in(fc,names(handle_openai))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1", 10890)
  }
  res<-handle_openai$get_model_list(verbosity = 100)
  expect_true(!res$success)
  res<-handle_openai$get_model_list()
  expect_null(res$success)
})
