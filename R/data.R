#' Issue Framing and Support for Welfare Reform
#'
#' A dataset of 213 Danish students containing variables on gender, education, political interest,
#'   ideology, political knowledge, extremity of political values, treatment assignment (job/poor frame),
#'   beliefs about why some people receive welfare benefits, perceived importance of different
#'   considerations related to welfare policy, and support for a proposed welfare reform (Slothuus 2008; Imai and
#'   Yamamoto 2013).
#'
#' @format A data frame with 213 rows and 15 columns: \describe{
#'   \item{gender1}{Gender. 0: Female; 1: Male.}
#'   \item{educ1}{Level of education.
#'     1: the municipal primary and lower secondary school before ninth form;
#'     2: the municipal primary and lower secondary school after ninth or tenth form;
#'     3: Basic schooling; 4: Vocational education;
#'     5: Higher preparatory examination course student
#'     6: Upper secondary school student; 7: Higher commercial examination student
#'     8: Higher technical examination student; 9: Short-term further education;
#'     10: Medium-term further education; 11: Long-term further education;
#'     12: Foreign education; 13: Else.}
#'   \item{polint1}{Political interest, measured on a 0-4 scale.}
#'   \item{ideo1}{Ideological self-placement on a 1-8 scale. A larger value denotes a more
#'     right-wing position.}
#'   \item{know1}{Political knowledge. 1: low; 2: medium; 3: high.}
#'   \item{value1}{Extremity of political values. 0: moderate. 1: extreme.}
#'   \item{ttt}{Treatment assignment. Whether the respondent read a newspaper article that highlighted
#'     the positive effect of welfare reform on job creation (1) versus one emphasizing its negative
#'     effect on the poor (0).}
#'   \item{W1}{The degree to which the respondent attributes welfare recipiency to internal factors,
#'     measured on a 0-1 scale.}
#'   \item{W2}{The degree to which the respondent attributes welfare recipiency to external factors,
#'     measured on a 0-1 scale.}
#'   \item{M1}{How important the respondent thinks that there should always be an incentive for
#'     people to take a job instead of receiving welfare benefits, measured on a 0-1 scale.}
#'   \item{M2}{How important the respondent thinks that nobody should live in poverty, measured on
#'     a 0-1 scale.}
#'   \item{M3}{How important the respondent thinks that government expenditures on welfare
#'     benefits should not be too expensive, measured on a 0-1 scale.}
#'   \item{M4}{How important the respondent thinks that no defrauder should receive welfare benefits,
#'     measured on a 0-1 scale.}
#'   \item{M5}{How important the respondent thinks that the unemployed should have benefit rates
#'     making it possible to maintain a decent standard of living conditions, measured on a 0-1 scale.}
#'   \item{Y}{Support for the proposed welfare reform, measured on a seven-point scale.}
#'   }
#' @references Slothuus, Rune. 2008. "More than Weighting Cognitive Importance: A Dual-process Model of Issue
#'   Framing Effects." Political Psychology 29(1):1-28.
#' @references Imai, Kosuke and Teppei Yamamoto. 2013. "Identification and Sensitivity Analysis for Multiple
#'   Causal Mechanisms: Revisiting Evidence from Framing Experiments." Political Analysis 21(2):141-171.
"welfare"


#' The Legacy of Political Violence among Crimean Tatars
#'
#' A dataset of 427 Crimean Tatars including variables on ancestor victimization,
#'  political identities of first-, second- and third-generation respondents,
#'  and political attitudes toward Russia's annexation of Crimea (Lupu and Peisakhin 2017).
#'
#' @format A data frame with 427 rows and 19 columns: \describe{
#'   \item{kulak}{Whether the first-generation respondent had close relatives subject to
#'    dekulakization. 0: no; 1: yes.}
#'   \item{prosoviet_pre}{Whether the first-generation respondent's close relatives privately
#'   supported or opposed Soviet authorities. 1: opposed; 2: indifferent; 3: supported.}
#'   \item{religiosity_pre}{How important it was for the first-generation respondent's family to
#'   follow Islamic customs and traditions while in deportation. 1: not at all important;
#'   2: somewhat important; 3: very important.}
#'   \item{land_pre}{Whether the first-generation respondent's close relatives owned agricultural
#'   land. 0: no; 1: some; 2: a lot.}
#'   \item{orchard_pre}{Whether the first-generation respondent's close relatives owned orchards.
#'   0: no; 1: yes.}
#'   \item{animals_pre}{Whether the first-generation respondent's close relatives owned pasture animals.
#'   0: no; 1: some; 2: a lot.}
#'   \item{carriage_pre}{Whether the first-generation respondent's close relatives owned horse-drawn
#'   carriages. 0: no; 1: yes.}
#'   \item{otherprop_pre}{Whether the first-generation respondent's close relatives owned other substantial
#'   property. 0: no; 1: yes.}
#'   \item{violence}{Whether the first-generation respondent had a family member who died due to poor
#'   conditions during the 1944-45 deportation to Crimea. 0: no; 1: yes.}
#'   \item{trust_g1}{Whether the first-generation respondent trusts Crimean Tatars more than Russians.
#'   1: trusts Crimean Tatars more; 0: indifferent; -1: trusts Russians more.}
#'   \item{victim_g1}{Whether the first-generation respondent consider them or their close relatives victims
#'   of the Soviet political system. 0: no; 1: yes.}
#'   \item{fear_g1}{Whether the first-generation respondent started to fear concerning their future
#'   after the March referendum. 0: no; 1: yes.}
#'   \item{trust_g2}{The degree to which second-generation respondents trust Crimean Tatars more than Russians,
#'   ranging from -1 to 1 (averaged over multiple respondents).}
#'   \item{victim_g2}{The degree to which second-generation respondents consider them or their close relatives
#'   victims of the Soviet political system, ranging from 0 to 1 (averaged over multiple respondents).}
#'   \item{fear_g2}{The degree to which second-generation respondents started to fear concerning their future
#'   after the March referendum, ranging from 0 to 1 (averaged over multiple respondents).}
#'   \item{trust_g3}{Whether the third-generation respondent trusts Crimean Tatars more than Russians.
#'   1: trusts Crimean Tatars more; 0: indifferent; -1: trusts Russians more.}
#'   \item{victim_g3}{Whether the third-generation respondent considers them or their close relatives victims
#'   of the Soviet political system. 0: no; 1: yes.}
#'   \item{fear_g3}{Whether the third-generation respondent started to fear concerning their future
#'   after the March referendum. 0: no; 1: yes.}
#'   \item{annex}{Whether the third-generation respondent supports Russia’s annexation of Crimea. 0: no; 1: yes.}
#'   }
#' @references Lupu, Noam and Leonid Peisakhin. 2017. "The Legacy of Political Violence across Generations.”
#'   American Journal of Political Science 61(4):836-851.
"tatar"
