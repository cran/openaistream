% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moderations.R
\name{moderations}
\alias{moderations}
\title{moderations Class}
\description{
moderations Class

moderations Class
}
\section{Super class}{
\code{\link[openaistream:openai]{openaistream::openai}} -> \code{moderations}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-moderations-create}{\code{moderations$create()}}
\item \href{#method-moderations-clone}{\code{moderations$clone()}}
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
\if{html}{\out{<a id="method-moderations-create"></a>}}
\if{latex}{\out{\hypertarget{method-moderations-create}{}}}
\subsection{Method \code{create()}}{
Classifies if text violates OpenAI's Content Policy.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moderations$create(input, ..., verbosity = 0)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{input}}{character Required. The input text to classify.}

\item{\code{...}}{Additional parameters as required by the OpenAI API.For example:model...}

\item{\code{verbosity}}{numeric. Verbosity level for the API call(0:no output;1:show headers;
2:show headers and bodies;3: show headers, bodies, and curl status messages.).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Returns a list of image objects.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-moderations-clone"></a>}}
\if{latex}{\out{\hypertarget{method-moderations-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moderations$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
