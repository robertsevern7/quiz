module States where

data State = State {
    name :: String
  , abbreviation :: String
  , captial :: String
  , mostPopulusCity :: String
  , areaSquareMiles :: Integer
}

-- Data source nabbed from:
-- http://www.tellingmachine.com/post/all-50-states-as-xml-json-csv-xls-files.aspx
states :: [State]
states = [
  State "ALABAMA" "AL" "Montgomery" "Birmingham" 52423,
  State "ALASKA" "AK" "Juneau" "Anchorage" 698473,
  State "ARIZONA" "AZ" "Phoenix" "Phoenix" 6595778,
  State "ARKANSAS" "AR" "Little Rock" "Little Rock" 2889450,
  State "CALIFORNIA" "CA" "Sacramento" "Los Angeles" 36961664,
  State "COLORADO" "CO" "Denver" "Denver" 5024748,
  State "CONNECTICUT" "CT" "Hartford" "Bridgeport" 3518288,
  State "DELAWARE" "DE" "Dover" "Wilmington" 885122,
  State "FLORIDA" "FL" "Tallahassee" "Jacksonville" 18537969,
  State "GEORGIA" "GA" "Atlanta" "Atlanta" 9829211,
  State "HAWAII" "HI" "Honolulu" "Honolulu" 1295178,
  State "IDAHO" "ID" "Boise" "Boise" 1545801,
  State "ILLINOIS" "IL" "Springfield" "Chicago" 12910409,
  State "INDIANA" "IN" "Indianapolis" "Indianapolis" 6423113,
  State "IOWA" "IA" "Des Moines" "Des Moines" 3007856,
  State "KANSAS" "KS" "Topeka" "Wichita" 2818747,
  State "KENTUCKY" "KY" "Frankfort" "Louisville" 4314113,
  State "LOUISIANA" "LA" "Baton Rouge" "New Orleans" 4492076,
  State "MAINE" "ME" "Augusta" "Portland" 1318301,
  State "MARYLAND" "MD" "Annapolis" "Baltimore" 5699478,
  State "MASSACHUSETTS" "MA" "Boston" "Boston" 6593587,
  State "MICHIGAN" "MI" "Lansing" "Detroit" 9969727,
  State "MINNESOTA" "MN" "Saint Paul" "Minneapolis" 5266214,
  State "MISSISSIPPI" "MS" "Jackson" "Jackson" 2951996,
  State "MISSOURI" "MO" "Jefferson City" "Kansas City" 5987580,
  State "MONTANA" "MT" "Helena" "Billings" 974989,
  State "NEBRASKA" "NE" "Lincoln" "Omaha" 1796619,
  State "NEVADA" "NV" "Carson City" "Las Vegas" 2643085,
  State "NEW HAMPSHIRE" "NH" "Concord" "Machester" 1324575,
  State "NEW JERSEY" "NJ" "Trenton" "Newark" 8707739,
  State "NEW MEXICO" "NM" "Santa Fe" "Albuquerque" 2009671,
  State "NEW YORK" "NY" "Albany" "New York" 19541453,
  State "NORTH CAROLINA" "NC" "Raleigh" "Charlotte" 9380884,
  State "NORTH DAKOTA" "ND" "Bismarck" "Fargo" 646844,
  State "OHIO" "OH" "Columbus" "Columbus" 11542645,
  State "OKLAHOMA" "OK" "Oklahoma City" "Oklahoma City" 3687050,
  State "OREGON" "OR" "Salem" "Portland" 3825657,
  State "PENNSYLVANIA" "PA" "Harrisburg" "Philadelphia" 12604767,
  State "RHODE ISLAND" "RI" "Providence" "Providence" 1053209,
  State "SOUTH CAROLINA" "SC" "Columbia" "Columbia" 4561242,
  State "SOUTH DAKOTA" "SD" "Pierre" "Sioux Falls" 812383,
  State "TENNESSEE" "TN" "Nashville" "Memphis" 6296254,
  State "TEXAS" "TX" "Austin" "Houston" 24782302,
  State "UTAH" "UT" "Salt Lake City" "Salt Lake City" 2784572,
  State "VERMONT" "VT" "Montpelier" "Burlington" 621760,
  State "VIRGINIA" "VA" "Richmond" "Virginia Beach" 7882590,
  State "WASHINGTON" "WA" "Olympia" "Seattle" 6664195,
  State "WEST VIRGINIA" "WV" "Charleston" "Charleston" 1819777,
  State "WISCONSIN" "WI" "Madison" "Milwaukee" 5654774,
  State "WYOMING" "WY" "Cheyenne" "Cheyenne" 544270
  ]