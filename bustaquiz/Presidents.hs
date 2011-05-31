module Presidents (
  OrderOfService,
  orderOfService
  ) where

import Logic (QuestionMaker,generateQuestion, rndSelect,Question(Order),QuestionType(OrderType))
import Data.List (sortBy)
import Data.Ord (comparing)

data Affiliation = Federalist 
                 | DemocraticRepublican
                 | Democrat
                 | Republican
                 | Whig
                 | NationalUnion

data President = President {
    name :: String
  , yearsInOffice :: Double
  , yearFirstInaugurated :: Double -- Needed to give ordering!
  , ageAtInauguration :: Int
  , stateElectedFrom :: String
  , affiliation :: Affiliation
  , occupation :: String
  , college :: String
}


orderOfService :: OrderOfService
orderOfService = OrderOfService 

data OrderOfService = OrderOfService

instance QuestionMaker OrderOfService where
  generateQuestion seed OrderType OrderOfService = do
    pres <- rndSelect seed presidents 10
    let sorted = sortBy (comparing yearFirstInaugurated) pres
        desc = "Put the following Presidents in the order in which they were inaugurated, starting with the earliest"
    return (Just (Order desc $ map (\p -> (name p,show $ yearFirstInaugurated p)) sorted))
  generateQuestion _ _ _ = return Nothing

-- Data nabbed from 
-- http://qrc.depaul.edu/Excel_Files/Presidents.xls
presidents :: [President]
presidents = [
  President "John Adams" 4 1797 61 "Massachusetts" Federalist "Lawyer" "Harvard",
  President "Thomas Jefferson" 8 1801 57 "Virginia" DemocraticRepublican "Planter  Lawyer" "William and Mary",
  President "James Madison" 8 1809 57 "Virginia" DemocraticRepublican "Lawyer" "Princeton",
  President "James Monroe" 8 1817 58 "Virginia" DemocraticRepublican "Lawyer" "William and Mary",
  President "John Quincy Adams" 4 1825 57 "Massachusetts" DemocraticRepublican "Lawyer" "Harvard",
  President "Andrew Jackson" 8 1829 61 "Tennessee" Democrat "Lawyer" "None",
  President "Martin Van Buren " 4 1837 54 "New York" Democrat "Lawyer" "None",
  President "William Henry Harrison" 0.8 1841.0 68 "Ohio" Whig "Soldier" "Hampden-Sydney",
  President "John Tyler" 4 1841.1 51 "Virginia" Whig "Lawyer" "William and Mary",
  President "James K. Polk" 4 1845 49 "Tennessee" Democrat "Lawyer" "U. of North Carolina",
  President "Zachary Taylor" 1 1849 64 "Louisiana" Whig "Soldier" "None",
  President "Millard Fillmore" 3 1850 50 "New York" Whig "Lawyer" "None",
  President "Franklin Pierce" 4 1853 48 "New Hampshire" Democrat "Lawyer" "Bowdoin",
  President "James Buchanan" 4 1857 65 "Pennsylvania" Democrat "Lawyer" "Dickinson",
  President "Abraham Lincoln" 4 1861 52 "Illinois" Republican "Lawyer" "None",
  President "Andrew Johnson" 4 1865 56 "Tennessee" NationalUnion "Tailor" "None",
  President "Ulysses S. Grant" 8 1869 46 "Illinois" Republican "Soldier" "US Military Academy",
  President "Rutherford B. Hayes" 4 1877 54 "Ohio" Republican "Lawyer" "Kenyon",
  President "James A. Garfield" 0.5 1881 49 "Ohio" Republican "Lawyer" "Williams",
  President "Chester A. Arthur" 3 1881 50 "New York" Republican "Lawyer" "Union",
  President "Grover Cleveland" 4 1885 47 "New York" Democrat "Lawyer" "None",
  President "Benjamin Harrison" 4 1889 55 "Indiana" Republican "Lawyer" "Miami",
  President "Grover Cleveland" 4 1893 55 "New York" Democrat "Lawyer" "None",
  President "William McKinley" 4 1897 54 "Ohio" Republican "Lawyer" "Allegheny College",
  President "Theodore Roosevelt" 8 1901 42 "New York" Republican "Author" "Harvard",
  President "William Howard Taft" 4 1909 51 "Ohio" Republican "Lawyer" "Yale",
  President "Woodrow Wilson" 8 1913 56 "New Jersey" Democrat "Educator" "Princeton",
  President "Warren G. Harding" 2 1921 55 "Ohio" Republican "Editor" "None",
  President "Calvin Coolidge" 6 1923 51 "Massachusetts" Republican "Lawyer" "Amherst",
  President "Herbert Hoover" 4 1929 54 "California" Republican "Engineer" "Stanford",
  President "Franklin Roosevelt" 12 1933 51 "New York" Democrat "Lawyer" "Harvard",
  President "Harry S. Truman" 8 1945 60 "Missouri" Democrat "Businessman" "None",
  President "Dwight D. Eisenhower" 8 1953 62 "New York" Republican "Soldier" "US Military Academy",
  President "John F. Kennedy" 3 1961 43 "Massachusetts" Democrat "Author" "Harvard",
  President "Lyndon B. Johnson" 5 1963 55 "Texas" Democrat "Teacher" "Southwest Texas State",
  President "Richard M. Nixon" 5 1969 56 "New York" Republican "Lawyer" "Whittier",
  President "Gerald Ford" 3 1974 61 "Michigan" Republican "Lawyer" "Michigan",
  President "Jimmy Carter" 4 1977 52 "Georgia" Democrat "Businessman" "US Naval Academy",
  President "Ronald Reagan" 8 1981 69 "California" Republican "Actor" "Eureka College",
  President "George Bush" 4 1989 64 "Texas" Republican "Businessman" "Yale",
  President "Bill Clinton" 8 1993 46 "Arkansas" Democrat "Lawyer" "Georgetown",
  President "George W. Bush" 8 2001 54 "Texas" Republican "Businessman" "Yale",
  President "Barack Obama" 0 2009 47 "Illinois" Democrat "Lawyer" "Columbia University"
  ]