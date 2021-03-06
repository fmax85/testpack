#' FARS sample data frame
#'
#' Data frame generated from the 2013 dataset
#'
#' @source U.S. Department of Transportation National Highway Traffic Safety
#'    Administration, random sample of 1,000 cases of fatal crash accidents
#'    recorded in 2013
#'
#' @docType data
#'
#' @usage data(fars.test.df)
#'
#' @format A data frame with 50 columns:
#' \describe{
#'   \item{STATE}{State Number}
#'   \item{ST_CASE}{Consecutive Number}
#'   \item{VE_TOTAL}{Number of Vehicle Forms Submitted- ALL}
#'   \item{VE_FORMS}{Number of Motor Vehicles in Transport (MVIT)}
#'   \item{PVH_INVL}{Number of Parked/Working Vehicles}
#'   \item{PEDS}{Number of Forms Submitted for Persons Not in Motor Vehicles}
#'   \item{PERNOTMVIT}{Number of Persons Not in Motor Vehicles in Transport (MVIT)}
#'   \item{PERMVIT}{Number of Persons in Motor Vehicles in Transport (MVIT)}
#'   \item{PERSONS}{Number of Forms Submitted for Persons in Motor Vehicles}
#'   \item{COUNTY}{County}
#'   \item{CITY}{City}
#'   \item{DAY}{Day of Crash}
#'   \item{MONTH}{Month of Crash}
#'   \item{YEAR}{Year of Crash}
#'   \item{DAY_WEEK}{Day of Week}
#'   \item{HOUR}{Hour of Crash}
#'   \item{MINUTE}{Minute of Crash}
#'   \item{NHS}{National Highway System}
#'   \item{ROAD_FNC}{Roadway Function Class (discontinued)}
#'   \item{ROUTE}{Route Signing}
#'   \item{TWAY_ID}{Trafficway Identifier}
#'   \item{TWAY_ID2}{Trafficway Identifier}
#'   \item{MILEPT}{Milepoint}
#'   \item{LATITUDE}{Latitude}
#'   \item{LONGITUD}{Longitude}
#'   \item{SP_JUR}{Special Jurisdiction }
#'   \item{HARM_EV}{First Harmful Event}
#'   \item{MAN_COLL}{Manner of Collision}
#'   \item{RELJCT1}{Relation to Junction- Within Interchange Area}
#'   \item{RELJCT2}{Relation to Junction- Specific Location}
#'   \item{TYP_INT}{Type of Intersection}
#'   \item{WRK_ZONE}{Work Zone}
#'   \item{REL_ROAD}{Relation to Trafficway}
#'   \item{LGT_COND}{Light Condition}
#'   \item{WEATHER1}{Atmospheric Conditions}
#'   \item{WEATHER2}{Atmospheric Conditions}
#'   \item{WEATHER}{Atmospheric Conditions}
#'   \item{SCH_BUS}{School Bus Related}
#'   \item{RAIL}{Rail Grade Crossing Identifier}
#'   \item{NOT_HOUR}{Hour of Notification}
#'   \item{NOT_MIN}{Minute of Notification}
#'   \item{ARR_HOUR}{Hour of Arrival at Scene}
#'   \item{ARR_MIN}{Minute of Arrival at Scene}
#'   \item{HOSP_HR}{Hour of EMS Arrival at Hospital}
#'   \item{HOSP_MN}{Minute of EMS Arrival at Hospital}
#'   \item{CF1}{Related Factors- Crash Level}
#'   \item{CF2}{Related Factors- Crash Level}
#'   \item{CF3}{Related Factors- Crash Level}
#'   \item{FATALS}{Fatalities}
#'   \item{DRUNK_DR}{Number of Drinking Drivers}
#' }
#'
#' @examples
#' data(fars.test.df)
"fars.test.df"
