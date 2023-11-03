#' An R6 Class Interface to OpenAI API
#'
#' @description Provides methods to interact with OpenAI API including
#'   fetching model details, generating completions, managing files, and more.
#'   Always ensure that the API key is kept private.
#' @examples
#' key <- "YOUR_API_KEY"
#' #NOTE To retrieve the correct data, it is necessary to provide an 'openaikey';
#' # otherwise, an exception will be returned.
#' aaa <- openai$new(key)
#' #if need proxy
#' #aaa$set_proxy("127.0.0.1", 8890)
#' # List model
#' aaa$get_model_list()
#' \donttest{
#' #The following request will cause a timeout when the correct key is not present,
#' # so it is not necessary to test it during the check.
#' # Upload a file
#' file_id <- aaa$files_upload(path = "../../model_newbuy/gpt_F_tuning/st03/test_st04_v01.jsonl")
#'
#' # Delete a file
#' aaa$files_delete(file_id$id, verbosity = 0)
#'
#' # Retrieve a file's details
#' aaa$files_retrieve(NULL, verbosity = 0)

#' # Get a list of files
#' aaa$files_get_list()

#' # List fine-tuning jobs
#' aaa$fine_tuning_jobs_list(limit = 2, verbosity = 3)
#'
#' # Retrieve details of a specific fine-tuning job
#' aaa$fine_tuning_jobs_retrieve("ftjob-XVOnvAP2owx4fh4B65QQP0Bk", verbosity = 0)
#'
#' # Get events related to a specific fine-tuning job
#' aaa$fine_tuning_jobs_events("ftjob-XVOnvAP2owx4fh4B65QQP0Bk", verbosity = 3)

#' # Compute embeddings for input data
#' aaa$embeddings(model = "text-embedding-ada-002", input = "who are you?")

#' # chat test not stream
#' streamlg <- aaa$get_completions_query(
#'   prompt = "Please explain the World Wars?",
#'   model = "davinci-002",
#'   stream = FALSE,
#'   max_tokens = 20,
#'   num = 4,
#'   verbosity = 0
#' )
#'
#' # chat test stream
#' streamlg <- aaa$get_chat_completions_query(
#'   messages = data.frame(role = c("system", "user"),
#'                         content = c("You are a assistant.", "How's the weather today?")),
#'   model = "gpt-3.5-turbo",
#'   stream = TRUE,
#'   max_tokens = 10
#' )
#'
#' streamlg$get_state()
#' streamlg$next_value
#' streamlg$close()
#' streamlg$next_value
#' streamlg$get_state()
#' }
#' @export
openai <- R6Class(
  "openai",
  private = list(
    api_key = NULL,
    proxy=list(),
    api_endpoints = list(
      chat_completions = "https://api.openai.com/v1/chat/completions",
      completions = "https://api.openai.com/v1/completions",
      files = "https://api.openai.com/v1/files",
      models = "https://api.openai.com/v1/models",
      fine_tuning_jobs ="https://api.openai.com/v1/fine_tuning/jobs",
      embeddings = "https://api.openai.com/v1/embeddings"
    ),
    api_call = function(endpoint, path="", body=NULL, method="GET", headers=list(), query=list(), verbosity=0,return_handle=FALSE) {
      url <- private$api_endpoints[[endpoint]]
      req <- request(url) %>% req_headers(!!!c(list(Authorization = paste0("Bearer ", private$api_key)), headers))
      if (path != "") {
        paths <- unlist(strsplit(path, "/"))
        for (p in paths) {
          req <- req %>%req_url_path_append(p)
        }
      }
      if (length(query) > 0) {
        req <- req %>%req_url_query(!!!query)
      }
      if (!is.null(body)) {
        req <- req %>%req_body_json(body)
      }
      if(length(private$proxy) == 2) {
        req <- req %>%
          req_proxy(private$proxy$ip, private$proxy$port)
      }
      if (return_handle) {
        return(curl::curl(req$url, handle = httr2:::req_handle(req)))
      }
      res <- tryCatch({
        req %>% req_perform(verbosity = verbosity)
      }, error=function(e) {
        return(openai_error$new(as.character(e)))
      })
      if (inherits(res, "openai_error")) {
        return(res)
      }else{
        parsed <- fromJSON(rawToChar(res$body))
        return(list(success=TRUE, data=parsed))
      }
    }
  ),
  public = list(
    #' @description Initialize the OpenAI API interface with the provided API key.
    #' @param api_key The OpenAI API key.
    initialize = function(api_key) {
      private$api_key<-api_key
    },
    #' @description Configure the proxy settings.
    #' @param proxy_ip The IP address of the proxy.
    #' @param proxy_port The port number of the proxy.
    set_proxy = function(proxy_ip,proxy_port){
      if(!grepl("^([0-9]{1,3}\\.){3}[0-9]{1,3}$", proxy_ip)) {
        stop("Invalid proxy IP address.")
      }
      ip_components <- unlist(strsplit(proxy_ip, split = "\\."))
      if(any(as.numeric(ip_components) > 255) || any(as.numeric(ip_components) < 0)) {
        stop("Invalid proxy IP address.")
      }
      if(!is.numeric(proxy_port) || proxy_port < 1 || proxy_port > 65535) {
        stop("Invalid proxy port number.")
      }
      private$proxy$ip = proxy_ip
      private$proxy$port = proxy_port
    },
    #' @description Retrieve a list of available models from OpenAI.
    #' @param verbosity Verbosity level for the API call.
    #' @return A list of available models.
    get_model_list=function(verbosity=0){
      result <- private$api_call("models", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data$data)
      }
    },
    #' @description Retrieve details of a specific model from OpenAI.
    #' @param model The model ID.
    #' @param verbosity Verbosity level for the API call.
    #' @return Details of the specified model.
    get_model_retrieve=function(model,verbosity=0){
      result <- private$api_call("models", paste0("/", model), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data$data)
      }
    },
    #' @description Get completions for a given prompt using a specific model.
    #' @param prompt The input text to get completions for.
    #' @param model The model to use for generating completions.
    #' @param stream A boolean indicating whether to stream the results.
    #' @param num The number of completions to retrieve.
    #' @param verbosity Verbosity level for the API call.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @return Completions based on the input prompt.
    get_completions_query=function(prompt,model,stream=F,...,num=2,verbosity = 0){
      option = list(...)
      option$prompt = prompt
      option$model = model
      option$stream = stream
      if (option$stream) {
        handle <- private$api_call("completions", body=option, headers=list(Accept="text/event-stream", `Content-Type` = "application/json"), return_handle=TRUE, verbosity=verbosity)
        return(DataStream$new(requery = handle, num = num))
      } else {
        result <- private$api_call("completions", body=option, headers=list(`Content-Type` = "application/json"),method = "POST", verbosity=verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(list(all_resp=result$data, vres = result$data$choices))
        }
      }
    },
    #' @description Generate conversational completions using chat models.
    #' @param messages A list of message objects, where each message has a role and content.
    #' @param model The model to use for generating chat completions.
    #' @param stream A boolean indicating whether to stream the results.
    #' @param num The number of chat completions to retrieve.
    #' @param verbosity Verbosity level for the API call.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @return Completions based on the conversational context.
    get_chat_completions_query=function(messages,model,stream=F,...,num=2,verbosity = 0){
      option = list(...)
      option$messages = messages
      option$model = model
      option$stream = stream
      if (option$stream) {
        handle <- private$api_call("chat_completions", body=option, headers=list(Accept="text/event-stream", `Content-Type` = "application/json"), return_handle=TRUE, verbosity=verbosity)
        return(DataStream$new(requery = handle , num = num))
      } else {
        result <- private$api_call("chat_completions", body=option, headers=list(`Content-Type` = "application/json"),method = "POST", verbosity=verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(list(all_resp=result$data, vres = result$data$choices))
        }
      }
    },
    #' @description Upload a file to OpenAI.
    #' @param path Path to the file that needs to be uploaded.
    #' @param purpose Purpose of the file (e.g., "fine-tune").
    #' @param verbosity Verbosity level for the API call.
    #' @return Response indicating the success or failure of the upload operation.
    files_upload=function(path,purpose="fine-tune",verbosity=0){
      result<-tryCatch({
        request(private$api_endpoints[["files"]]) %>% req_headers(!!!list(Authorization = paste0("Bearer ", private$api_key)))%>%
          req_proxy(private$proxy$ip, private$proxy$port)%>%
          req_body_multipart(file = curl::form_file(path), purpose = purpose)%>%
          req_perform(verbosity=verbosity)
      }, error=function(e) {
        return(openai_error$new(as.character(e)))
      })
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(fromJSON(rawToChar(result$body)))
      }
    },
    #' @description Retrieve a list of files from OpenAI.
    #' @param verbosity Verbosity level for the API call.
    #' @return A list of files.
    files_get_list=function(verbosity=0){
      result <- private$api_call("files", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete a file from OpenAI.
    #' @param file_id The ID of the file to delete.
    #' @param verbosity Verbosity level for the API call.
    #' @return Response indicating the success or failure of the delete operation.
    files_delete=function(file_id,verbosity=0){
      result <- private$api_call("files", paste0("/", file_id), method = "DELETE", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieve details of a specific file from OpenAI.
    #' @param file_id The ID of the file to retrieve details for.
    #' @param verbosity Verbosity level for the API call.
    #' @return Details of the specified file.
    files_retrieve=function(file_id,verbosity=0){
      result <- private$api_call("files", paste0("/", file_id), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create a fine-tuning job in OpenAI.
    #' @param model The model ID.
    #' @param training_file The file used for training.
    #' @param n_epochs Number of epochs for the fine-tuning.
    #' @param verbosity Verbosity level for the API call.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @return Response indicating the success or failure of the fine-tuning job creation.
    fine_tuning_jobs_create=function(model,training_file,n_epochs=1,verbosity=0,...){
      option <- list(...)
      option$model <- model
      option$training_file <- training_file
      option$hyperparameters <- list(n_epochs=n_epochs)

      result <- private$api_call("fine_tuning_jobs", body = option, method = "POST", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description List fine-tuning jobs from OpenAI.
    #' @param verbosity Verbosity level for the API call.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @return A list of fine-tuning jobs.
    fine_tuning_jobs_list=function(...,verbosity=0){
      option = list(...)
      result <- private$api_call("fine_tuning_jobs", query = option, verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieve details of a specific fine-tuning job from OpenAI.
    #' @param job_id The ID of the fine-tuning job.
    #' @param verbosity Verbosity level for the API call.
    #' @return Details of the specified fine-tuning job.
    fine_tuning_jobs_retrieve=function(job_id,verbosity=0){
      result <- private$api_call("fine_tuning_jobs", paste0("/", job_id), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Cancel a specific fine-tuning job in OpenAI.
    #' @param job_id The ID of the fine-tuning job to cancel.
    #' @param verbosity Verbosity level for the API call.
    #' @return Response indicating the success or failure of the cancel operation.
    fine_tuning_jobs_cancel=function(job_id,verbosity=0){
      result <- private$api_call("fine_tuning_jobs", paste0("/", job_id, "/", "cancel"), method = "POST", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Get events related to a specific fine-tuning job from OpenAI.
    #' @param job_id The ID of the fine-tuning job to retrieve events for.
    #' @param verbosity Verbosity level for the API call.
    #' @return A list of events related to the specified fine-tuning job.
    fine_tuning_jobs_events=function(job_id,verbosity=0){
      result <- private$api_call("fine_tuning_jobs", paste0("/", job_id, "/", "events"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Compute embeddings for input data using a specified model in OpenAI.
    #' @param model The model to use for generating embeddings.
    #' @param input Input data to generate embeddings for.
    #' @param verbosity Verbosity level for the API call.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @return Embeddings for the input data.
    embeddings=function(model,input,verbosity=0,...){
      option <- list(...)
      option$model <- model
      option$input <- input

      result <- private$api_call("embeddings", body = option, headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)

