% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assistants.R
\name{assistants}
\alias{assistants}
\title{assistants Class}
\description{
assistants Class

assistants Class
}
\section{Super class}{
\code{\link[openaistream:openai]{openaistream::openai}} -> \code{assistants}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-assistants-create}{\code{assistants$create()}}
\item \href{#method-assistants-retrieve}{\code{assistants$retrieve()}}
\item \href{#method-assistants-modify}{\code{assistants$modify()}}
\item \href{#method-assistants-delete}{\code{assistants$delete()}}
\item \href{#method-assistants-list}{\code{assistants$list()}}
\item \href{#method-assistants-file_create}{\code{assistants$file_create()}}
\item \href{#method-assistants-file_retrieve}{\code{assistants$file_retrieve()}}
\item \href{#method-assistants-file_delete}{\code{assistants$file_delete()}}
\item \href{#method-assistants-file_list}{\code{assistants$file_list()}}
\item \href{#method-assistants-clone}{\code{assistants$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="openaistream" data-topic="openai" data-id="initialize"><a href='../../openaistream/html/openai.html#method-openai-initialize'><code>openaistream::openai$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-create"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-create}{}}}
\subsection{Method \code{create()}}{
Create an assistant with a model and instructions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$create(model, ..., verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{model}}{character Required. ID of the model to use. You can use the List models API to see all of your available models,
or see our Model overview for descriptions of them.}

\item{\code{...}}{Additional parameters as required by the OpenAI API.For example:name;description;instructions;tools;file_ids;metadata}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An assistant object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-retrieve"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-retrieve}{}}}
\subsection{Method \code{retrieve()}}{
Retrieves an assistant.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$retrieve(assistant_id, verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{assistant_id}}{character Required. The ID of the assistant to retrieve.}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The assistant object matching the specified ID.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-modify"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-modify}{}}}
\subsection{Method \code{modify()}}{
Modifies an assistant.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$modify(assistant_id, ..., verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{assistant_id}}{character Required. The ID of the assistant to modify.}

\item{\code{...}}{Additional parameters as required by the OpenAI API.For example:model;name;description;instructions;tools;file_ids,metadata}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The assistant object matching the specified ID.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-delete"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-delete}{}}}
\subsection{Method \code{delete()}}{
Delete an assistant.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$delete(assistant_id, verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{assistant_id}}{character Required. The ID of the assistant to delete.}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Deletion status
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-list"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-list}{}}}
\subsection{Method \code{list()}}{
Returns a list of assistants
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$list(..., verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Additional parameters as required by the OpenAI API.For example:limit;order;after;before;}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list of assistant objects.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-file_create"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-file_create}{}}}
\subsection{Method \code{file_create()}}{
Create an assistant file by attaching a File to an assistant.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$file_create(assistant_id, file_id, verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{assistant_id}}{character Required. The ID of the assistant for which to create a File.}

\item{\code{file_id}}{character Required. A File ID (with purpose="assistants") that the assistant should use.
Useful for tools like retrieval and code_interpreter that can access files.}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An assistant file object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-file_retrieve"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-file_retrieve}{}}}
\subsection{Method \code{file_retrieve()}}{
Retrieves an AssistantFile.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$file_retrieve(assistant_id, file_id, verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{assistant_id}}{character Required. The ID of the assistant who the file belongs to.}

\item{\code{file_id}}{character Required. The ID of the file we're getting.}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The assistant file object matching the specified ID.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-file_delete"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-file_delete}{}}}
\subsection{Method \code{file_delete()}}{
Delete an assistant file.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$file_delete(assistant_id, file_id, verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{assistant_id}}{character Required. The ID of the assistant who the file belongs to.}

\item{\code{file_id}}{character Required. The ID of the file we're getting.}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Deletion status
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-file_list"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-file_list}{}}}
\subsection{Method \code{file_list()}}{
Retrieve a list of files from OpenAI.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$file_list(assistant_id, ..., verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{assistant_id}}{character Required. The ID of the assistant who the file belongs to.}

\item{\code{...}}{Additional parameters as required by the OpenAI API.For example:limit,order,after,before}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list of files.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assistants-clone"></a>}}
\if{latex}{\out{\hypertarget{method-assistants-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{assistants$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
