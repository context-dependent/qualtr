#' Share a Survey with a User or Group
#' 
#' @param survey_id is character: the Qx SurveyId to share
#' @param recipient_id is character: the Qx UserId or GroupId to share it with
#' #' @param custom_permissions 
#' is a list that conforms to the following
#' scheme: 
#'     list(
#'        surveyDefinitionManipulation = list(
#'            copySurveyQuestions   = TRUE,
#'            editSurveyFlow        = TRUE,
#'            useBlocks             = TRUE,
#'            useSkipLogic          = TRUE,
#'            useConjoint           = TRUE,
#'            useTriggers           = TRUE,
#'            useQuotas             = TRUE,
#'            setSurveyOptions      = TRUE,
#'            editQuestions         = TRUE,
#'            deleteSurveyQuestions = TRUE,
#'            useTableOfContents    = TRUE,
#'            useAdvancedQuotas     = TRUE
#'        ),
#'        surveyManagement = list(
#'            editSurveys           = TRUE,
#'            activateSurveys       = TRUE,
#'            deactivateSurveys     = TRUE,
#'            copySurveys           = TRUE,
#'            distributeSurveys     = TRUE,
#'            deleteSurveys         = TRUE,
#'            translateSurveys      = TRUE
#'        ),
#'        response = list(
#'            editSurveyResponses   = TRUE,
#'            createResponseSets    = TRUE,
#'            viewResponseId        = TRUE,
#'            useCrossTabs          = TRUE,
#'            useScreenouts         = TRUE
#'        ),
#'        result = list(
#'            downloadSurveyResults = TRUE,
#'            viewSurveyResults     = TRUE,
#'            filterSurveyResults   = TRUE,
#'            viewPersonalData      = TRUE
#'        )
#'    )
#' 
#' @export 
share_survey <- function(survey_id, recipient_id, custom_permissions = list()) {
    default_permissions <- list(
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

    updated_permissions <- modifyList(default_permissions, custom_permissions)

    request_body <- list(
        recipientId = recipient_id, 
        permissions = updated_permissions
    )

    sv_url <- glue::glue("{Sys.getenv('QUALTRICS_ROOT_URL')}/API/v3/surveys/{survey_id}/permissions/collaborations")

    req <- httr::POST(
        url = sv_url, 
        body = request_body, 
        encode = "json", 
        httr::add_headers(headers())
    )



    return(req)
}

#' List User Groups visible by current user (must be brand admin)
#' 
#' @export 
list_user_groups <- function() {
    req <- httr::GET(
        glue::glue("{Sys.getenv('QUALTRICS_ROOT_URL')}/API/v3/groups"),
        httr::add_headers(headers())
    )

    raw <- httr::content(req)

    res <- tibble::as_tibble(
        purrr::transpose(raw$result$elements) |> 
        purrr::map(unlist)
    )

    return(res)

}

#' Share multiple surveys with multiple groups 
#' 
#' @param d 
#' is df[survey_id, group_id], where each 
#' row i represents a request to share the 
#' survey with survey_id[i] with the 
#' group with recipient_id[i]
#' recipient_id is either a UserId or GroupId 
#' @param custom_permissions 
#' is a list that conforms to the following
#' scheme: 
#'     list(
#'        surveyDefinitionManipulation = list(
#'            copySurveyQuestions   = TRUE,
#'            editSurveyFlow        = TRUE,
#'            useBlocks             = TRUE,
#'            useSkipLogic          = TRUE,
#'            useConjoint           = TRUE,
#'            useTriggers           = TRUE,
#'            useQuotas             = TRUE,
#'            setSurveyOptions      = TRUE,
#'            editQuestions         = TRUE,
#'            deleteSurveyQuestions = TRUE,
#'            useTableOfContents    = TRUE,
#'            useAdvancedQuotas     = TRUE
#'        ),
#'        surveyManagement = list(
#'            editSurveys           = TRUE,
#'            activateSurveys       = TRUE,
#'            deactivateSurveys     = TRUE,
#'            copySurveys           = TRUE,
#'            distributeSurveys     = TRUE,
#'            deleteSurveys         = TRUE,
#'            translateSurveys      = TRUE
#'        ),
#'        response = list(
#'            editSurveyResponses   = TRUE,
#'            createResponseSets    = TRUE,
#'            viewResponseId        = TRUE,
#'            useCrossTabs          = TRUE,
#'            useScreenouts         = TRUE
#'        ),
#'        result = list(
#'            downloadSurveyResults = TRUE,
#'            viewSurveyResults     = TRUE,
#'            filterSurveyResults   = TRUE,
#'            viewPersonalData      = TRUE
#'        )
#'    )
#' but that only specifies values to change from 
#' the defaults
#' @export
share_surveys <- function(d, custom_permissions = list()) {
    res <- d |>
        mutate(
            response = purrr::map2(survey_id, recipient_id, share_survey),
            status  = purrr::map_chr(response, qualtrics_response_codes)
        )

    return(res)
}