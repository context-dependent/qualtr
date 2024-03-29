% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/08_share-surveys.R
\name{share_surveys}
\alias{share_surveys}
\title{Share multiple surveys with multiple groups}
\usage{
share_surveys(d, custom_permissions = list())
}
\arguments{
\item{d}{is df\link{survey_id, group_id}, where each
row i represents a request to share the
survey with survey_id\link{i} with the
group with recipient_id\link{i}
recipient_id is either a UserId or GroupId}

\item{custom_permissions}{is a list that conforms to the following
scheme:
list(
surveyDefinitionManipulation = list(
copySurveyQuestions   = TRUE,
editSurveyFlow        = TRUE,
useBlocks             = TRUE,
useSkipLogic          = TRUE,
useConjoint           = TRUE,
useTriggers           = TRUE,
useQuotas             = TRUE,
setSurveyOptions      = TRUE,
editQuestions         = TRUE,
deleteSurveyQuestions = TRUE,
useTableOfContents    = TRUE,
useAdvancedQuotas     = TRUE
),
surveyManagement = list(
editSurveys           = TRUE,
activateSurveys       = TRUE,
deactivateSurveys     = TRUE,
copySurveys           = TRUE,
distributeSurveys     = TRUE,
deleteSurveys         = TRUE,
translateSurveys      = TRUE
),
response = list(
editSurveyResponses   = TRUE,
createResponseSets    = TRUE,
viewResponseId        = TRUE,
useCrossTabs          = TRUE,
useScreenouts         = TRUE
),
result = list(
downloadSurveyResults = TRUE,
viewSurveyResults     = TRUE,
filterSurveyResults   = TRUE,
viewPersonalData      = TRUE
)
)
but that only specifies values to change from
the defaults}
}
\description{
Share multiple surveys with multiple groups
}
