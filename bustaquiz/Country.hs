module Country (
  CapitalQuiz,
  capitalQuiz,
  CountryFlagsQuiz,
  countryFlagsQuiz
  ) where

import Logic
import Control.Arrow
import StaticFiles
import Yesod.Helpers.Static

-- For countries, the corpus of data is so tiny that
-- we'll cheat http://download.geonames.org/export/dump/countryInfo.txt
-- Massaged the data with a few emacs macros and dumped it in this file.
data CountryInfo = CountryInfo {
  name :: String,
  capital :: String,
  sizeInSqKilometre :: Int,
  tld :: String,
  currency :: String,
  flag :: StaticRoute
}

data CapitalQuiz = CapitalQuiz [CountryInfo]

capitalQuiz :: CapitalQuiz
capitalQuiz = CapitalQuiz countries

-- TODO there is some duplication here with the questions regarding states
data CountryFlagsQuiz = CountryFlagsQuiz [(String,StaticRoute)]

countryFlagsQuiz :: CountryFlagsQuiz
countryFlagsQuiz = CountryFlagsQuiz (map (name &&& flag) countries)

instance QuestionMaker CountryFlagsQuiz where
  generateQuestion seed IdentifyType (CountryFlagsQuiz countryFlags) = do
    (country,picture) <- chooseFromList seed countryFlags
    return (Just (Identify "Which country has the following flag?" picture country))
  generateQuestion _ _ _ = return Nothing

-- TODO probably insane to keep all the country info for just this - use just name and capital
instance QuestionMaker CapitalQuiz where
  generateQuestion seed AssociateType (CapitalQuiz countries) = do
    match <- rndSelect seed (filter (not . null . capital) countries) 10
    let capitals = map (name &&& capital) match
    return $ Just (Associate "Match the countries with the capitals" capitals)  
  generateQuestion seed IdentifyMultipleType (CapitalQuiz countries) = do
    match <- rndSelect seed (filter (not . null . capital) countries) 10
    let capitals = map (name &&& capital) match
    return $ Just (Associate "Name the capitals of these countries" capitals) 
  generateQuestion _ _ _ = return Nothing
                    
countries :: [CountryInfo]
countries = [CountryInfo "Andorra" "Andorra la Vella" 468 ".ad" "Euro" images_flags_countries_flag_of_Andorra_png,
             CountryInfo "United Arab Emirates" "Abu Dhabi" 82880 ".ae" "Dirham" images_flags_countries_flag_of_the_United_Arab_Emirates_png,
             CountryInfo "Afghanistan" "Kabul" 647500 ".af" "Afghani" images_flags_countries_flag_of_Afghanistan_png,
             CountryInfo "Antigua and Barbuda" "St. John's" 443 ".ag" "Dollar" images_flags_countries_flag_of_Antigua_and_Barbuda_png,
             CountryInfo "Anguilla" "The Valley" 102 ".ai" "Dollar" images_flags_countries_flag_of_Anguilla_png,
             CountryInfo "Albania" "Tirana" 28748 ".al" "Lek" images_flags_countries_flag_of_Albania_png,
             CountryInfo "Armenia" "Yerevan" 29800 ".am" "Dram" images_flags_countries_flag_of_Armenia_png,
             CountryInfo "Netherlands Antilles" "Willemstad" 960 ".an" "Guilder" images_flags_countries_flag_of_the_Netherlands_Antilles_png,
             CountryInfo "Angola" "Luanda" 1246700 ".ao" "Kwanza" images_flags_countries_flag_of_Angola_png,
             CountryInfo "Argentina" "Buenos Aires" 2766890 ".ar" "Peso" images_flags_countries_flag_of_Argentina_png,
             CountryInfo "American Samoa" "Pago Pago" 199 ".as" "Dollar" images_flags_countries_flag_of_American_Samoa_png,
             CountryInfo "Austria" "Vienna" 83858 ".at" "Euro" images_flags_countries_flag_of_Austria_png,
             CountryInfo "Australia" "Canberra" 7686850 ".au" "Dollar" images_flags_countries_flag_of_Australia_png,
             CountryInfo "Aruba" "Oranjestad" 193 ".aw" "Guilder" images_flags_countries_flag_of_Aruba_png,
             CountryInfo "Azerbaijan" "Baku" 86600 ".az" "Manat" images_flags_countries_flag_of_Azerbaijan_png,
             CountryInfo "Bosnia and Herzegovina" "Sarajevo" 51129 ".ba" "Marka" images_flags_countries_flag_of_Bosnia_and_Herzegovina_png,
             CountryInfo "Barbados" "Bridgetown" 431 ".bb" "Dollar" images_flags_countries_flag_of_Barbados_png,
             CountryInfo "Bangladesh" "Dhaka" 144000 ".bd" "Taka" images_flags_countries_flag_of_Bangladesh_png,
             CountryInfo "Belgium" "Brussels" 30510 ".be" "Euro" images_flags_countries_flag_of_Belgium_png,
             CountryInfo "Burkina Faso" "Ouagadougou" 274200 ".bf" "Franc" images_flags_countries_flag_of_Burkina_Faso_png,
             CountryInfo "Bulgaria" "Sofia" 110910 ".bg" "Lev" images_flags_countries_flag_of_Bulgaria_png,
             CountryInfo "Bahrain" "Manama" 665 ".bh" "Dinar" images_flags_countries_flag_of_Bahrain_png,
             CountryInfo "Burundi" "Bujumbura" 27830 ".bi" "Franc" images_flags_countries_flag_of_Burundi_png,
             CountryInfo "Benin" "Porto-Novo" 112620 ".bj" "Franc" images_flags_countries_flag_of_Benin_png,
             CountryInfo "Saint Barthélemy" "Gustavia" 21 ".gp" "Euro" images_flags_countries_flag_of_Saint_Barthelemy_png,
             CountryInfo "Bermuda" "Hamilton" 53 ".bm" "Dollar" images_flags_countries_flag_of_Bermuda_png,
             CountryInfo "Brunei" "Bandar Seri Begawan" 5770 ".bn" "Dollar" images_flags_countries_flag_of_Brunei_png, 
             CountryInfo "Bolivia" "La Paz" 1098580 ".bo" "Boliviano" images_flags_countries_flag_of_Bolivia_png,
             CountryInfo "Brazil" "Brasília" 8511965 ".br" "Real" images_flags_countries_flag_of_Brazil_png,
             CountryInfo "Bahamas" "Nassau" 13940 ".bs" "Dollar" images_flags_countries_flag_of_the_Bahamas_png,
             CountryInfo "Bhutan" "Thimphu" 47000 ".bt" "Ngultrum" images_flags_countries_flag_of_Bhutan_png,
             CountryInfo "Bouvet Island" "" (- 1)  ".bv" "Krone" images_flags_countries_flag_of_Norway_png,
             CountryInfo "Botswana" "Gaborone" 600370 ".bw" "Pula" images_flags_countries_flag_of_Botswana_png,
             CountryInfo "Belarus" "Minsk" 207600 ".by" "Ruble" images_flags_countries_flag_of_Belarus_png,
             CountryInfo "Belize" "Belmopan" 22966 ".bz" "Dollar" images_flags_countries_flag_of_Belize_png,
             CountryInfo "Canada" "Ottawa" 9984670 ".ca" "Dollar" images_flags_countries_flag_of_Canada_png,
             CountryInfo "Cocos Islands" "West Island" 14 ".cc" "Dollar" images_flags_countries_flag_of_the_Cocos_Keeling_Islands_png, -- TODO
             CountryInfo "Democratic Republic of the Congo" "Kinshasa" 2345410 ".cd" "Franc" images_flags_countries_flag_of_the_Democratic_Republic_of_the_Congo_png,
             CountryInfo "Central African Republic" "Bangui" 622984 ".cf" "Franc" images_flags_countries_flag_of_the_Central_African_Republic_png,
             CountryInfo "Republic of the Congo" "Brazzaville" 342000 ".cg" "Franc" images_flags_countries_flag_of_the_Republic_of_the_Congo_png,
             CountryInfo "Switzerland" "Berne" 41290 ".ch" "Franc" images_flags_countries_flag_of_Switzerland_png,
             CountryInfo "Ivory Coast" "Yamoussoukro" 322460 ".ci" "Franc" images_flags_countries_flag_of_the_Ivory_Coast_png,
             CountryInfo "Cook Islands" "Avarua" 240 ".ck" "Dollar" images_flags_countries_flag_of_the_Cook_Islands_png,
             CountryInfo "Chile" "Santiago" 756950 ".cl" "Peso" images_flags_countries_flag_of_Chile_png,
             CountryInfo "Cameroon" "Yaoundé" 475440 ".cm" "Franc" images_flags_countries_flag_of_Cameroon_png,
             CountryInfo "China" "Beijing" 9596960 ".cn" "Yuan Renminbi" images_flags_countries_flag_of_China_png,
             CountryInfo "Colombia" "Bogotá" 1138910 ".co" "Peso" images_flags_countries_flag_of_Colombia_png,
             CountryInfo "Costa Rica" "San José" 51100 ".cr" "Colon" images_flags_countries_flag_of_Costa_Rica_png,
             CountryInfo "Cuba" "Havana" 110860 ".cu" "Peso" images_flags_countries_flag_of_Cuba_png,
             CountryInfo "Cape Verde" "Praia" 4033 ".cv" "Escudo" images_flags_countries_flag_of_Cape_Verde_png,
             CountryInfo "Christmas Island" "Flying Fish Cove" 135 ".cx" "Dollar" images_flags_countries_flag_of_Christmas_Island_png,
             CountryInfo "Cyprus" "Nicosia" 9250 ".cy" "Euro" images_flags_countries_flag_of_Cyprus_png,
             CountryInfo "Czech Republic" "Prague" 78866 ".cz" "Koruna" images_flags_countries_flag_of_the_Czech_Republic_png,
             CountryInfo "Germany" "Berlin" 357021 ".de" "Euro" images_flags_countries_flag_of_Germany_png,
             CountryInfo "Djibouti" "Djibouti" 23000 ".dj" "Franc" images_flags_countries_flag_of_Djibouti_png,
             CountryInfo "Denmark" "Copenhagen" 43094 ".dk" "Krone" images_flags_countries_flag_of_Denmark_png,
             CountryInfo "Dominica" "Roseau" 754 ".dm" "Dollar" images_flags_countries_flag_of_Dominica_png,
             CountryInfo "Dominican Republic" "Santo Domingo" 48730 ".do" "Peso" images_flags_countries_flag_of_the_Dominican_Republic_png,
             CountryInfo "Algeria" "Algiers" 2381740 ".dz" "Dinar" images_flags_countries_flag_of_Algeria_png,
             CountryInfo "Ecuador" "Quito" 283560 ".ec" "Dollar" images_flags_countries_flag_of_Ecuador_png,
             CountryInfo "Estonia" "Tallinn" 45226 ".ee" "Kroon" images_flags_countries_flag_of_Estonia_png,
             CountryInfo "Egypt" "Cairo" 1001450 ".eg" "Pound" images_flags_countries_flag_of_Egypt_png,
             CountryInfo "Western Sahara" "El-Aaiun" 266000 ".eh" "Dirham" images_flags_countries_flag_of_Western_Sahara_png,
             CountryInfo "Eritrea" "Asmara" 121320 ".er" "Nakfa" images_flags_countries_flag_of_Eritrea_png,
             CountryInfo "Spain" "Madrid" 504782 ".es" "Euro" images_flags_countries_flag_of_Spain_png,
             CountryInfo "Ethiopia" "Addis Ababa" 1127127 ".et" "Birr" images_flags_countries_flag_of_Ethiopia_png,
             CountryInfo "Finland" "Helsinki" 337030 ".fi" "Euro" images_flags_countries_flag_of_Finland_png,
             CountryInfo "Fiji" "Suva" 18270 ".fj" "Dollar" images_flags_countries_flag_of_Fiji_png,
             CountryInfo "Falkland Islands" "Stanley" 12173 ".fk" "Pound" images_flags_countries_flag_of_the_Falkland_Islands_png,
             CountryInfo "Micronesia" "Palikir" 702 ".fm" "Dollar" images_flags_countries_flag_of_Micronesia_png,
             CountryInfo "Faroe Islands" "Tórshavn" 1399 ".fo" "Krone" images_flags_countries_flag_of_the_Faroe_Islands_png,
             CountryInfo "France" "Paris" 547030 ".fr" "Euro" images_flags_countries_flag_of_France_png,
             CountryInfo "Gabon" "Libreville" 267667 ".ga" "Franc" images_flags_countries_flag_of_Gabon_png,
             CountryInfo "United Kingdom" "London" 244820 ".uk" "Pound" images_flags_countries_flag_of_the_United_Kingdom_png,
             CountryInfo "Grenada" "St. George's" 344 ".gd" "Dollar" images_flags_countries_flag_of_Grenada_png,
             CountryInfo "Georgia" "Tbilisi" 69700 ".ge" "Lari" images_flags_countries_flag_of_Georgia_png,
             CountryInfo "French Guiana" "Cayenne" 91000 ".gf" "Euro" images_flags_countries_flag_of_France_png,
             CountryInfo "Guernsey" "St Peter Port" 78 ".gg" "Pound" images_flags_countries_flag_of_Guernsey_png,
             CountryInfo "Ghana" "Accra" 239460 ".gh" "Cedi" images_flags_countries_flag_of_Ghana_png,
             CountryInfo "Gibraltar" "Gibraltar" 6 ".gi" "Pound" images_flags_countries_flag_of_Gibraltar_png,
             CountryInfo "Greenland" "Nuuk" 2166086 ".gl" "Krone" images_flags_countries_flag_of_Greenland_png,
             CountryInfo "Gambia" "Banjul" 11300 ".gm" "Dalasi" images_flags_countries_flag_of_The_Gambia_png,
             CountryInfo "Guinea" "Conakry" 245857 ".gn" "Franc" images_flags_countries_flag_of_Guinea_png,
             CountryInfo "Guadeloupe" "Basse-Terre" 1780 ".gp" "Euro" images_flags_countries_flag_of_Guadeloupe_png,
             CountryInfo "Equatorial Guinea" "Malabo" 28051 ".gq" "Franc" images_flags_countries_flag_of_Equatorial_Guinea_png,
             CountryInfo "Greece" "Athens" 131940 ".gr" "Euro" images_flags_countries_flag_of_Greece_png,
             CountryInfo "South Georgia and the South Sandwich Islands" "Grytviken" 3903 ".gs" "Pound" images_flags_countries_flag_of_South_Georgia_and_the_South_Sandwich_Islands_png,
             CountryInfo "Guatemala" "Guatemala City" 108890 ".gt" "Quetzal" images_flags_countries_flag_of_Guatemala_png,
             CountryInfo "Guam" "Hagåtña" 549 ".gu" "Dollar" images_flags_countries_flag_of_Guam_png,
             CountryInfo "Guinea-Bissau" "Bissau" 36120 ".gw" "Franc" images_flags_countries_flag_of_Guinea_Bissau_png,
             CountryInfo "Guyana" "Georgetown" 214970 ".gy" "Dollar" images_flags_countries_flag_of_Guyana_png,
             CountryInfo "Hong Kong" "Hong Kong" 1092 ".hk" "Dollar" images_flags_countries_flag_of_Hong_Kong_png,
--              CountryInfo "Heard Island and McDonald Islands"  "" 412 ".hm" "Dollar" NO FLAG,
             CountryInfo "Honduras" "Tegucigalpa" 112090 ".hn" "Lempira" images_flags_countries_flag_of_Honduras_png,
             CountryInfo "Croatia" "Zagreb" 56542 ".hr" "Kuna" images_flags_countries_flag_of_Croatia_png,
             CountryInfo "Haiti" "Port-au-Prince" 27750 ".ht" "Gourde" images_flags_countries_flag_of_Haiti_png,
             CountryInfo "Hungary" "Budapest" 93030 ".hu" "Forint" images_flags_countries_flag_of_Hungary_png,
             CountryInfo "Indonesia" "Jakarta" 1919440 ".id" "Rupiah" images_flags_countries_flag_of_Indonesia_png,
             CountryInfo "Ireland" "Dublin" 70280 ".ie" "Euro" images_flags_countries_flag_of_Ireland_png,
             CountryInfo "Israel" "Jerusalem" 20770 ".il" "Shekel" images_flags_countries_flag_of_Israel_png,
             CountryInfo "Isle of Man" "Douglas  Isle of Man" 572 ".im" "Pound" images_flags_countries_flag_of_the_Isle_of_Man_png,
             CountryInfo "India" "New Delhi" 3287590 ".in" "Rupee" images_flags_countries_flag_of_India_png,
             CountryInfo "British Indian Ocean Territory" "Diego Garcia" 60 ".io" "Dollar" images_flags_countries_flag_of_the_British_Indian_Ocean_Territory_png,
             CountryInfo "Iraq" "Baghdad" 437072 ".iq" "Dinar" images_flags_countries_flag_of_Iraq_png,
             CountryInfo "Iran" "Tehran" 1648000 ".ir" "Rial" images_flags_countries_flag_of_Iran_png,
             CountryInfo "Iceland" "Reykjavík" 103000 ".is" "Krona" images_flags_countries_flag_of_Iceland_png,
             CountryInfo "Italy" "Rome" 301230 ".it" "Euro" images_flags_countries_flag_of_Italy_png,
             CountryInfo "Jersey" "Saint Helier" 116 ".je" "Pound" images_flags_countries_flag_of_Jersey_png,
             CountryInfo "Jamaica" "Kingston" 10991 ".jm" "Dollar" images_flags_countries_flag_of_Jamaica_png,
             CountryInfo "Jordan" "Amman" 92300 ".jo" "Dinar" images_flags_countries_flag_of_Jordan_png,
             CountryInfo "Japan" "Tokyo" 377835 ".jp" "Yen" images_flags_countries_flag_of_Japan_png,
             CountryInfo "Kenya" "Nairobi" 582650 ".ke" "Shilling" images_flags_countries_flag_of_Kenya_png,
             CountryInfo "Kyrgyzstan" "Bishkek" 198500 ".kg" "Som" images_flags_countries_flag_of_Kyrgyzstan_png,
             CountryInfo "Cambodia" "Phnom Penh" 181040 ".kh" "Riels" images_flags_countries_flag_of_Cambodia_png,
             CountryInfo "Kiribati" "South Tarawa" 811 ".ki" "Dollar" images_flags_countries_flag_of_Kiribati_png,
             CountryInfo "Comoros" "Moroni" 2170 ".km" "Franc" images_flags_countries_flag_of_the_Comoros_png,
             CountryInfo "Saint Kitts and Nevis" "Basseterre" 261 ".kn" "Dollar" images_flags_countries_flag_of_Saint_Kitts_and_Nevis_png,
             CountryInfo "North Korea" "Pyongyang" 120540 ".kp" "Won" images_flags_countries_flag_of_North_Korea_png,
             CountryInfo "South Korea" "Seoul" 98480 ".kr" "Won" images_flags_countries_flag_of_South_Korea_png,
             CountryInfo "Kosovo" "Priština" (- 1) "" "Euro" images_flags_countries_flag_of_Kosovo_png,
             CountryInfo "Kuwait" "Kuwait City" 17820 ".kw" "Dinar" images_flags_countries_flag_of_Kuwait_png,
             CountryInfo "Cayman Islands" "George Town" 262 ".ky" "Dollar" images_flags_countries_flag_of_the_Cayman_Islands_png,
             CountryInfo "Kazakhstan" "Astana" 2717300 ".kz" "Tenge" images_flags_countries_flag_of_Kazakhstan_png,
             CountryInfo "Laos" "Vientiane" 236800 ".la" "Kip" images_flags_countries_flag_of_Laos_png,
             CountryInfo "Lebanon" "Beirut" 10400 ".lb" "Pound" images_flags_countries_flag_of_Lebanon_png,
             CountryInfo "Saint Lucia" "Castries" 616 ".lc" "Dollar" images_flags_countries_flag_of_Saint_Lucia_png,
             CountryInfo "Liechtenstein" "Vaduz" 160 ".li" "Franc" images_flags_countries_flag_of_Liechtenstein_png,
             CountryInfo "Sri Lanka" "Colombo" 65610 ".lk" "Rupee" images_flags_countries_flag_of_Sri_Lanka_png,
             CountryInfo "Liberia" "Monrovia" 111370 ".lr" "Dollar" images_flags_countries_flag_of_Liberia_png,
             CountryInfo "Lesotho" "Maseru" 30355 ".ls" "Loti" images_flags_countries_flag_of_Lesotho_png,
             CountryInfo "Lithuania" "Vilnius" 65200 ".lt" "Litas" images_flags_countries_flag_of_Lithuania_png,
             CountryInfo "Luxembourg" "Luxembourg" 2586 ".lu" "Euro" images_flags_countries_flag_of_Luxembourg_png,
             CountryInfo "Latvia" "Riga" 64589 ".lv" "Lat" images_flags_countries_flag_of_Latvia_png,
             CountryInfo "Libya" "Tripolis" 1759540 ".ly" "Dinar" images_flags_countries_flag_of_Libya_png,
             CountryInfo "Morocco" "Rabat" 446550 ".ma" "Dirham" images_flags_countries_flag_of_Morocco_png,
             CountryInfo "Monaco" "Monaco" 2 ".mc" "Euro" images_flags_countries_flag_of_Monaco_png,
             CountryInfo "Moldova" "Chişinău" 33843 ".md" "Leu" images_flags_countries_flag_of_Moldova_png,
             CountryInfo "Montenegro" "Podgorica" 14026 ".cs" "Euro" images_flags_countries_flag_of_Montenegro_png,
             CountryInfo "Saint Martin" "Marigot" 53 ".gp" "Euro" images_flags_countries_flag_of_Saint_Martin_png,
             CountryInfo "Madagascar" "Antananarivo" 587040 ".mg" "Ariary" images_flags_countries_flag_of_Madagascar_png,
             CountryInfo "Marshall Islands" "Majuro" 181 ".mh" "Dollar" images_flags_countries_flag_of_the_Marshall_Islands_png,
             CountryInfo "Macedonia" "Skopje" 25333 ".mk" "Denar" images_flags_countries_flag_of_Macedonia_png,
             CountryInfo "Mali" "Bamako" 1240000 ".ml" "Franc" images_flags_countries_flag_of_Mali_png,
             CountryInfo "Myanmar" "Yangon" 678500 ".mm" "Kyat" images_flags_countries_flag_of_Myanmar_png,
             CountryInfo "Mongolia" "Ulan Bator" 1565000 ".mn" "Tugrik" images_flags_countries_flag_of_Mongolia_png,
             CountryInfo "Macao" "Macao" 254 ".mo" "Pataca" images_flags_countries_flag_of_Macau_png,
             CountryInfo "Northern Mariana Islands" "Saipan" 477 ".mp" "Dollar" images_flags_countries_flag_of_the_Northern_Mariana_Islands_png,
             CountryInfo "Martinique" "Fort-de-France" 1100 ".mq" "Euro" images_flags_countries_flag_of_Martinique_png,
             CountryInfo "Mauritania" "Nouakchott" 1030700 ".mr" "Ouguiya" images_flags_countries_flag_of_Mauritania_png,
             CountryInfo "Montserrat" "Plymouth" 102 ".ms" "Dollar" images_flags_countries_flag_of_Montserrat_png,
             CountryInfo "Malta" "Valletta" 316 ".mt" "Euro" images_flags_countries_flag_of_Malta_png, 
             CountryInfo "Mauritius" "Port Louis" 2040 ".mu" "Rupee" images_flags_countries_flag_of_Mauritius_png,
             CountryInfo "Maldives" "Malé" 300 ".mv" "Rufiyaa" images_flags_countries_flag_of_Maldives_png,
             CountryInfo "Malawi" "Lilongwe" 118480 ".mw" "Kwacha" images_flags_countries_flag_of_Malawi_png,
             CountryInfo "Mexico" "Mexico City" 1972550 ".mx" "Peso" images_flags_countries_flag_of_Mexico_png,
             CountryInfo "Malaysia" "Kuala Lumpur" 329750 ".my" "Ringgit" images_flags_countries_flag_of_Malaysia_png,
             CountryInfo "Mozambique" "Maputo" 801590 ".mz" "Meticail" images_flags_countries_flag_of_Mozambique_png,
             CountryInfo "Namibia" "Windhoek" 825418 ".na" "Dollar" images_flags_countries_flag_of_Namibia_png,
             CountryInfo "New Caledonia" "Nouméa" 19060 ".nc" "Franc" images_flags_countries_flag_of_New_Caledonia_png,
             CountryInfo "Niger" "Niamey" 1267000 ".ne" "Franc" images_flags_countries_flag_of_Niger_png,
             CountryInfo "Norfolk Island" "Kingston" 35 ".nf" "Dollar" images_flags_countries_flag_of_Norfolk_Island_png,
             CountryInfo "Nigeria" "Abuja" 923768 ".ng" "Naira" images_flags_countries_flag_of_Nigeria_png,
             CountryInfo "Nicaragua" "Managua" 129494 ".ni" "Cordoba" images_flags_countries_flag_of_Nicaragua_png,
             CountryInfo "Netherlands" "Amsterdam" 41526 ".nl" "Euro" images_flags_countries_flag_of_the_Netherlands_png,
             CountryInfo "Norway" "Oslo" 324220 ".no" "Krone" images_flags_countries_flag_of_Norway_png,
             CountryInfo "Nepal" "Kathmandu" 140800 ".np" "Rupee" images_flags_countries_flag_of_Nepal_png,
             CountryInfo "Nauru" "Yaren" 21 ".nr" "Dollar" images_flags_countries_flag_of_Nauru_png,
             CountryInfo "Niue" "Alofi" 260 ".nu" "Dollar" images_flags_countries_flag_of_Niue_png,
             CountryInfo "New Zealand" "Wellington" 268680 ".nz" "Dollar" images_flags_countries_flag_of_New_Zealand_png,
             CountryInfo "Oman" "Muscat" 212460 ".om" "Rial" images_flags_countries_flag_of_Oman_png,
             CountryInfo "Panama" "Panama City" 78200 ".pa" "Balboa" images_flags_countries_flag_of_Panama_png,
             CountryInfo "Peru" "Lima" 1285220 ".pe" "Sol" images_flags_countries_flag_of_Peru_png,
             CountryInfo "French Polynesia" "Papeete" 4167 ".pf" "Franc" images_flags_countries_flag_of_French_Polynesia_png,
             CountryInfo "Papua New Guinea" "Port Moresby" 462840 ".pg" "Kina" images_flags_countries_flag_of_Papua_New_Guinea_png,
             CountryInfo "Philippines" "Manila" 300000 ".ph" "Peso" images_flags_countries_flag_of_the_Philippines_png,
             CountryInfo "Pakistan" "Islamabad" 803940 ".pk" "Rupee" images_flags_countries_flag_of_Pakistan_png,
             CountryInfo "Poland" "Warsaw" 312685 ".pl" "Zloty" images_flags_countries_flag_of_Poland_png,
             CountryInfo "Saint Pierre and Miquelon" "Saint-Pierre" 242 ".pm" "Euro" images_flags_countries_flag_of_Saint_Pierre_and_Miquelon_png,
             CountryInfo "Pitcairn" "Adamstown" 47 ".pn" "Dollar" images_flags_countries_flag_of_the_Pitcairn_Islands_png,
             CountryInfo "Puerto Rico" "San Juan" 9104 ".pr" "Dollar" images_flags_countries_flag_of_Puerto_Rico_png,
             CountryInfo "Palestinian Territory" "East Jerusalem" 5970 ".ps" "Shekel" images_flags_countries_flag_of_Palestine_png,
             CountryInfo "Portugal" "Lisbon" 92391 ".pt" "Euro" images_flags_countries_flag_of_Portugal_png,
             CountryInfo "Palau" "Koror" 458 ".pw" "Dollar" images_flags_countries_flag_of_Palau_png,
             CountryInfo "Paraguay" "Asunción" 406750 ".py" "Guarani" images_flags_countries_flag_of_Paraguay_png,
             CountryInfo "Qatar" "Doha" 11437 ".qa" "Rial" images_flags_countries_flag_of_Qatar_png,
             CountryInfo "Reunion" "Saint-Denis" 2517 ".re" "Euro" images_flags_countries_flag_of_Reunion_png,
             CountryInfo "Romania" "Bucharest" 237500 ".ro" "Leu" images_flags_countries_flag_of_Romania_png,
             CountryInfo "Serbia" "Belgrade" 88361 ".rs" "Dinar" images_flags_countries_flag_of_Serbia_png,
             CountryInfo "Russia" "Moscow" 1710000000 ".ru" "Ruble" images_flags_countries_flag_of_Russia_png,
             CountryInfo "Rwanda" "Kigali" 26338 ".rw" "Franc" images_flags_countries_flag_of_Rwanda_png,
             CountryInfo "Saudi Arabia" "Riyadh" 1960582 ".sa" "Rial" images_flags_countries_flag_of_Saudi_Arabia_png,
             CountryInfo "Solomon Islands" "Honiara" 28450 ".sb" "Dollar" images_flags_countries_flag_of_the_Solomon_Islands_png,
             CountryInfo "Seychelles" "Victoria" 455 ".sc" "Rupee" images_flags_countries_flag_of_the_Seychelles_png,
             CountryInfo "Sudan" "Khartoum" 2505810 ".sd" "Dinar" images_flags_countries_flag_of_Sudan_png,
             CountryInfo "Sweden" "Stockholm" 449964 ".se" "Krona" images_flags_countries_flag_of_Sweden_png,
             CountryInfo "Singapore" "Singapur" 693 ".sg" "Dollar" images_flags_countries_flag_of_Singapore_png,
             CountryInfo "Saint Helena" "Jamestown" 410 ".sh" "Pound" images_flags_countries_flag_of_Saint_Helena_png,
             CountryInfo "Slovenia" "Ljubljana" 20273 ".si" "Euro" images_flags_countries_flag_of_Slovenia_png,
-- NO FLAG             CountryInfo "Svalbard and Jan Mayen" "Longyearbyen" 62049 ".sj" "Krone" images_flags_countries_flag_of_, 
             CountryInfo "Slovakia" "Bratislava" 48845 ".sk" "Euro" images_flags_countries_flag_of_Slovakia_png,
             CountryInfo "Sierra Leone" "Freetown" 71740 ".sl" "Leone" images_flags_countries_flag_of_Sierra_Leone_png,
             CountryInfo "San Marino" "San Marino" 61 ".sm" "Euro" images_flags_countries_flag_of_San_Marino_png,
             CountryInfo "Senegal" "Dakar" 196190 ".sn" "Franc" images_flags_countries_flag_of_Senegal_png,
             CountryInfo "Somalia" "Mogadishu" 637657 ".so" "Shilling" images_flags_countries_flag_of_Somalia_png,
             CountryInfo "Suriname" "Paramaribo" 163270 ".sr" "Dollar" images_flags_countries_flag_of_Suriname_png,
             CountryInfo "Sao Tome and Principe" "São Tomé" 1001 ".st" "Dobra" images_flags_countries_flag_of_Sao_Tome_and_Principe_png,
             CountryInfo "El Salvador" "San Salvador" 21040 ".sv" "Dollar" images_flags_countries_flag_of_El_Salvador_png,
             CountryInfo "Syria" "Damascus" 185180 ".sy" "Pound" images_flags_countries_flag_of_Syria_png,
             CountryInfo "Swaziland" "Mbabane" 17363 ".sz" "Lilangeni" images_flags_countries_flag_of_Swaziland_png, 
             CountryInfo "Turks and Caicos Islands" "Cockburn Town" 430 ".tc" "Dollar" images_flags_countries_flag_of_the_Turks_and_Caicos_Islands_png,
             CountryInfo "Chad" "N'Djamena" 1284000 ".td" "Franc" images_flags_countries_flag_of_Chad_png,
             CountryInfo "French Southern Territories" "Martin-de-Viviès" 7829 ".tf" "Euro" images_flags_countries_flag_of_the_French_Southern_and_Antarctic_Lands_png,
             CountryInfo "Togo" "Lomé" 56785 ".tg" "Franc" images_flags_countries_flag_of_Togo_png,
             CountryInfo "Thailand" "Bangkok" 514000 ".th" "Baht" images_flags_countries_flag_of_Thailand_png,
             CountryInfo "Tajikistan" "Dushanbe" 143100 ".tj" "Somoni" images_flags_countries_flag_of_Tajikistan_png,
             CountryInfo "Tokelau" "" 10 ".tk" "Dollar" images_flags_countries_flag_of_Tokelau_png,
             CountryInfo "East Timor" "Dili" 15007 ".tp" "Dollar" images_flags_countries_flag_of_East_Timor_png,
             CountryInfo "Turkmenistan" "Ashgabat" 488100 ".tm" "Manat" images_flags_countries_flag_of_Turkmenistan_png,
             CountryInfo "Tunisia" "Tunis" 163610 ".tn" "Dinar" images_flags_countries_flag_of_Tunisia_png,
             CountryInfo "Tonga" "Nuku'alofa" 748 ".to" "Pa'anga" images_flags_countries_flag_of_Tonga_png,
             CountryInfo "Turkey" "Ankara" 780580 ".tr" "Lira" images_flags_countries_flag_of_Turkey_png,
             CountryInfo "Trinidad and Tobago" "Port of Spain" 5128 ".tt" "Dollar" images_flags_countries_flag_of_Trinidad_and_Tobago_png,
             CountryInfo "Tuvalu" "Vaiaku" 26 ".tv" "Dollar" images_flags_countries_flag_of_Tuvalu_png,
             CountryInfo "Taiwan" "Taipei" 35980 ".tw" "Dollar" images_flags_countries_flag_of_Taiwan_png,
             CountryInfo "Tanzania" "Dodoma" 945087 ".tz" "Shilling" images_flags_countries_flag_of_Tanzania_png,
             CountryInfo "Ukraine" "Kiev" 603700 ".ua" "Hryvnia" images_flags_countries_flag_of_Ukraine_png,
             CountryInfo "Uganda" "Kampala" 236040 ".ug" "Shilling" images_flags_countries_flag_of_Uganda_png,
             CountryInfo "United States" "Washington" 9629091 ".us" "Dollar" images_flags_countries_flag_of_the_United_States_png,
             CountryInfo "Uruguay" "Montevideo" 176220 ".uy" "Peso" images_flags_countries_flag_of_Uruguay_png,
             CountryInfo "Uzbekistan" "Tashkent" 447400 ".uz" "Som" images_flags_countries_flag_of_Uzbekistan_png, 
             CountryInfo "Vatican" "Vatican City" 0 ".va" "Euro" images_flags_countries_flag_of_the_Vatican_City_png,
             CountryInfo "Saint Vincent and the Grenadines" "Kingstown" 389 ".vc" "Dollar" images_flags_countries_flag_of_Saint_Vincent_and_the_Grenadines_png,
             CountryInfo "Venezuela" "Caracas" 912050 ".ve" "Bolivar" images_flags_countries_flag_of_Venezuela_png,
             CountryInfo "British Virgin Islands" "Road Town" 153 ".vg" "Dollar" images_flags_countries_flag_of_the_British_Virgin_Islands_png,
             CountryInfo "U.S. Virgin Islands" "Charlotte Amalie" 352 ".vi" "Dollar" images_flags_countries_flag_of_the_United_States_Virgin_Islands_png,
             CountryInfo "Vietnam" "Hanoi" 329560 ".vn" "Dong" images_flags_countries_flag_of_Vietnam_png,
             CountryInfo "Vanuatu" "Port Vila" 12200 ".vu" "Vatu" images_flags_countries_flag_of_Vanuatu_png,
             CountryInfo "Wallis and Futuna" "Matâ'Utu" 274 ".wf" "Franc" images_flags_countries_flag_of_Wallis_and_Futuna_png,
             CountryInfo "Samoa" "Apia" 2944 ".ws" "Tala" images_flags_countries_flag_of_Samoa_png,
             CountryInfo "Yemen" "San‘a’" 527970 ".ye" "Rial" images_flags_countries_flag_of_Yemen_png,
             CountryInfo "Mayotte" "Mamoudzou" 374 ".yt" "Euro" images_flags_countries_flag_of_Mayotte_png,
             CountryInfo "South Africa" "Pretoria" 1219912 ".za" "Rand" images_flags_countries_flag_of_South_Africa_png,
             CountryInfo "Zambia" "Lusaka" 752614 ".zm" "Kwacha" images_flags_countries_flag_of_Zambia_png,
             CountryInfo "Zimbabwe" "Harare" 390580 ".zw" "Dollar" images_flags_countries_flag_of_Zimbabwe_png,
             CountryInfo "Serbia and Montenegro" "Belgrade" 102350 ".cs" "Dinar" images_flags_countries_flag_of_Montenegro_png]