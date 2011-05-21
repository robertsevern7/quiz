module States (
  StateFlags,
  stateFlags
  ) where

import Logic
import StaticFiles
import Yesod.Helpers.Static

import Control.Arrow ((&&&)) 
  
data State = State {
    name :: String
  , abbreviation :: String
  , captial :: String
  , mostPopulusCity :: String
  , areaSquareMiles :: Integer
  , flag :: StaticRoute
}

data StateFlags = StateFlags [(String,StaticRoute)]

stateFlags :: StateFlags
stateFlags = StateFlags (map (name &&& flag) states)

instance QuestionMaker StateFlags where
  generateQuestion seed IdentifyType (StateFlags stateFlags) = do
    (state,picture) <- chooseFromList seed stateFlags
    return (Just (Identify "Which state has the following flag?" picture state))
  generateQuestion seed _ _ = return Nothing
 
-- Data source nabbed from:
--   http://www.tellingmachine.com/post/all-50-states-as-xml-json-csv-xls-files.aspx
-- Images taken from 
--   http://hilltopit.com/nation_flags.html
states :: [State]
states = [
  State "ALABAMA" "AL" "Montgomery" "Birmingham" 52423 images_flags_states_flag_of_Alabama_png,
  State "ALASKA" "AK" "Juneau" "Anchorage" 698473 images_flags_states_flag_of_Alaska_png,
  State "ARIZONA" "AZ" "Phoenix" "Phoenix" 6595778 images_flags_states_flag_of_Arizona_png,
  State "ARKANSAS" "AR" "Little Rock" "Little Rock" 2889450 images_flags_states_flag_of_Arkansas_png,
  State "CALIFORNIA" "CA" "Sacramento" "Los Angeles" 36961664 images_flags_states_flag_of_California_png,
  State "COLORADO" "CO" "Denver" "Denver" 5024748 images_flags_states_flag_of_Colorado_png,
  State "CONNECTICUT" "CT" "Hartford" "Bridgeport" 3518288 images_flags_states_flag_of_Connecticut_png,
  State "DELAWARE" "DE" "Dover" "Wilmington" 885122 images_flags_states_flag_of_Delaware_png,
  State "FLORIDA" "FL" "Tallahassee" "Jacksonville" 18537969 images_flags_states_flag_of_Florida_png,
  State "GEORGIA" "GA" "Atlanta" "Atlanta" 9829211 images_flags_states_flag_of_Georgia_png,
  State "HAWAII" "HI" "Honolulu" "Honolulu" 1295178 images_flags_states_flag_of_Hawaii_png,
  State "IDAHO" "ID" "Boise" "Boise" 1545801 images_flags_states_flag_of_Idaho_png,
  State "ILLINOIS" "IL" "Springfield" "Chicago" 12910409 images_flags_states_flag_of_Illinois_png,
  State "INDIANA" "IN" "Indianapolis" "Indianapolis" 6423113 images_flags_states_flag_of_Indiana_png,
  State "IOWA" "IA" "Des Moines" "Des Moines" 3007856 images_flags_states_flag_of_Iowa_png,
  State "KANSAS" "KS" "Topeka" "Wichita" 2818747 images_flags_states_flag_of_Kansas_png,
  State "KENTUCKY" "KY" "Frankfort" "Louisville" 4314113 images_flags_states_flag_of_Kentucky_png,
  State "LOUISIANA" "LA" "Baton Rouge" "New Orleans" 4492076 images_flags_states_flag_of_Louisiana_png,
  State "MAINE" "ME" "Augusta" "Portland" 1318301 images_flags_states_flag_of_Maine_png,
  State "MARYLAND" "MD" "Annapolis" "Baltimore" 5699478 images_flags_states_flag_of_Maryland_png,
  State "MASSACHUSETTS" "MA" "Boston" "Boston" 6593587 images_flags_states_flag_of_Massachusetts_png,
  State "MICHIGAN" "MI" "Lansing" "Detroit" 9969727 images_flags_states_flag_of_Michigan_png,
  State "MINNESOTA" "MN" "Saint Paul" "Minneapolis" 5266214 images_flags_states_flag_of_Minnesota_png,
  State "MISSISSIPPI" "MS" "Jackson" "Jackson" 2951996 images_flags_states_flag_of_Mississippi_png,
  State "MISSOURI" "MO" "Jefferson City" "Kansas City" 5987580 images_flags_states_flag_of_Missouri_png,
  State "MONTANA" "MT" "Helena" "Billings" 974989 images_flags_states_flag_of_Montana_png,
  State "NEBRASKA" "NE" "Lincoln" "Omaha" 1796619 images_flags_states_flag_of_Nebraska_png,
  State "NEVADA" "NV" "Carson City" "Las Vegas" 2643085 images_flags_states_flag_of_Nevada_png,
  State "NEW HAMPSHIRE" "NH" "Concord" "Machester" 1324575 images_flags_states_flag_of_New_Hampshire_png,
  State "NEW JERSEY" "NJ" "Trenton" "Newark" 8707739 images_flags_states_flag_of_New_Jersey_png,
  State "NEW MEXICO" "NM" "Santa Fe" "Albuquerque" 2009671 images_flags_states_flag_of_New_Mexico_png,
  State "NEW YORK" "NY" "Albany" "New York" 19541453 images_flags_states_flag_of_New_York_png,
  State "NORTH CAROLINA" "NC" "Raleigh" "Charlotte" 9380884 images_flags_states_flag_of_North_Carolina_png,
  State "NORTH DAKOTA" "ND" "Bismarck" "Fargo" 646844 images_flags_states_flag_of_North_Dakota_png,
  State "OHIO" "OH" "Columbus" "Columbus" 11542645 images_flags_states_flag_of_Ohio_png,
  State "OKLAHOMA" "OK" "Oklahoma City" "Oklahoma City" 3687050 images_flags_states_flag_of_Oklahoma_png,
  State "OREGON" "OR" "Salem" "Portland" 3825657 images_flags_states_flag_of_Oregon_png,
  State "PENNSYLVANIA" "PA" "Harrisburg" "Philadelphia" 12604767 images_flags_states_flag_of_Pennsylvania_png,
  State "RHODE ISLAND" "RI" "Providence" "Providence" 1053209 images_flags_states_flag_of_Rhode_Island_png,
  State "SOUTH CAROLINA" "SC" "Columbia" "Columbia" 4561242 images_flags_states_flag_of_South_Carolina_png,
  State "SOUTH DAKOTA" "SD" "Pierre" "Sioux Falls" 812383 images_flags_states_flag_of_South_Dakota_png,
  State "TENNESSEE" "TN" "Nashville" "Memphis" 6296254 images_flags_states_flag_of_Tennessee_png,
  State "TEXAS" "TX" "Austin" "Houston" 24782302 images_flags_states_flag_of_Texas_png,
  State "UTAH" "UT" "Salt Lake City" "Salt Lake City" 2784572 images_flags_states_flag_of_Utah_png,
  State "VERMONT" "VT" "Montpelier" "Burlington" 621760 images_flags_states_flag_of_Vermont_png,
  State "VIRGINIA" "VA" "Richmond" "Virginia Beach" 7882590 images_flags_states_flag_of_Virginia_png,
  State "WASHINGTON" "WA" "Olympia" "Seattle" 6664195 images_flags_states_flag_of_Washington_png,
  State "WEST VIRGINIA" "WV" "Charleston" "Charleston" 1819777 images_flags_states_flag_of_West_Virginia_png,
  State "WISCONSIN" "WI" "Madison" "Milwaukee" 5654774 images_flags_states_flag_of_Wisconsin_png,
  State "WYOMING" "WY" "Cheyenne" "Cheyenne" 544270 images_flags_states_flag_of_Wyoming_png
  ]