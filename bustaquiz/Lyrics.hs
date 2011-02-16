module Lyrics (
  BeatlesLyrics,
  beatlesLyrics
  ) where

-- TODO Export a question maker from here.

import Network.HTTP
import Control.Monad (forM)
import Logic (rndSelect,QuestionMaker,generateQuestion,QuestionType(AssociateType),Question(Associate))
import Data.Maybe (fromJust)

newtype Artist = Artist String
newtype Song = Song String

restRoot :: String
restRoot = "http://lyrics.wikia.com/api.php"

mkLyricJsonUrl :: Artist -> Song -> Request String
mkLyricJsonUrl (Artist artist) (Song song) = getRequest $ restRoot 
                                             ++ "?artist=" 
                                             ++ urlEncode artist
                                             ++ "&song=" 
                                             ++ urlEncode song
                                             ++ "&fmt=text"

getLyrics' :: Artist -> Song -> IO String
getLyrics' artist song = simpleHTTP (mkLyricJsonUrl artist song) >>= getResponseBody

-- TODO Error handling could do with being improved!
-- TODO Encoding ("\n" and other escapings.)
-- TODO Lookup everything via the API, rewrite URLs for amazon links in response
getLyrics :: Artist -> Song -> IO (Maybe String)
getLyrics artist song = do
  x <- getLyrics' artist song
  if x == "Not Found" then return Nothing else return (Just x)
         
data BeatlesLyrics = BeatlesLyrics [Song]  

beatlesLyrics :: BeatlesLyrics
beatlesLyrics = BeatlesLyrics beatlesSongs
  
instance QuestionMaker BeatlesLyrics where
  generateQuestion seed AssociateType (BeatlesLyrics songs) = do
    match <- rndSelect seed songs 3
    associations <- forM match (\s@(Song song) -> do
                                   lyrics <- getLyrics beatles s
                                   return (song,fromJust lyrics))
    return (Just (Associate "Match the songs with the lyrics" associations))
  generateQuestion _ _ _ = return Nothing
    
-- The beatles did a lot of songs         
beatles :: Artist 
beatles = Artist "Beatles"

beatlesSongs :: [Song]
beatlesSongs = map Song [
  "A Day in the Life",
  "A Hard Day's Night",
  "A Shot of Rhythm and Blues",
  "A Taste of Honey",
  "A World Without Love",
  "Across the Universe",
  "Act Naturally",
  "Ain't She Sweet",
  "All I've Got to Do",
  "All My Loving",
  "All Things Must Pass",
  "All Together Now",
  "All You Need Is Love",
  "And I Love Her",
  "And Your Bird Can Sing",
  "Anna (Go to Him)",
  "Another Girl",
  "Any Time at All",
  "Ask Me Why",
  "Baby It's You",
  "Baby's in Black",
  "Baby, You're a Rich Man",
  "Back in the U.S.S.R.",
  "Bad Boy",
  "Bad to Me",
  "Because",
  "Being for the Benefit of Mr. Kite!",
  "Birthday",
  "Blackbird",
  "Blue Jay Way",
  "Boys",
  "Bésame Mucho",
  "Can't Buy Me Love",
  "Carnival of Light",
  "Carol",
  "Carry That Weight",
  "Catswalk",
  "Cayenne",
  "Chains",
  "Christmas Time (Is Here Again)",
  "Clarabella",
  "Come and Get It",
  "Come Together",
  "Cry Baby Cry",
  "Cry for a Shadow",
  "Crying, Waiting, Hoping",
  "Day Tripper",
  "Dear Prudence",
  "Devil in Her Heart",
  "Dig a Pony",
  "Dig It",
  "Dizzy Miss Lizzy",
  "Do You Want to Know a Secret",
  "Doctor Robert",
  "Don't Bother Me",
  "Don't Ever Change",
  "Don't Let Me Down",
  "Don't Pass Me By",
  "Drive My Car",
  "Eight Days a Week",
  "Eleanor Rigby",
  "Etcetera",
  "Every Little Thing",
  "Everybody's Got Something to Hide Except Me and My Monkey",
  "Everybody's Trying to Be My Baby",
  "Fixing a Hole",
  "Flying",
  "For No One",
  "For You Blue",
  "Free as a Bird",
  "From a Window",
  "From Me to You",
  "Get Back",
  "Getting Better",
  "Girl",
  "Glad All Over",
  "Glass Onion",
  "Golden Slumbers",
  "Good Day Sunshine",
  "Good Morning Good Morning",
  "Good Night",
  "Got To Get You Into My Life",
  "Hallelujah, I Love Her So",
  "Happiness Is a Warm Gun",
  "Hello Little Girl",
  "Hello, Goodbye",
  "Help!",
  "Helter Skelter",
  "Her Majesty",
  "Here Comes the Sun",
  "Here, There and Everywhere",
  "Hey Bulldog",
  "Hey Jude",
  "Hold Me Tight",
  "Honey Don't",
  "Honey Pie",
  "How Do You Do It?",
  "I Am the Walrus",
  "I Call Your Name",
  "I Don't Want to See You Again",
  "I Don't Want to Spoil the Party",
  "I Feel Fine",
  "I Forgot to Remember to Forget",
  "I Got a Woman",
  "I Got to Find My Baby",
  "I Just Don't Understand",
  "I Lost My Little Girl",
  "I Me Mine",
  "I Need You",
  "I Saw Her Standing There",
  "I Should Have Known Better",
  "I Wanna Be Your Man",
  "I Want to Hold Your Hand",
  "I Want to Tell You",
  "I Want You (She's So Heavy)",
  "I Will",
  "I'll Be Back",
  "I'll Be on My Way",
  "I'll Cry Instead",
  "I'll Follow the Sun",
  "I'll Get You",
  "I'll Keep You Satisfied",
  "I'm a Loser",
  "I'm Down",
  "I'm Gonna Sit Right Down and Cry (Over You)",
  "I'm Happy Just to Dance with You",
  "I'm In Love",
  "I'm Looking Through You",
  "I'm Only Sleeping",
  "I'm So Tired",
  "I'm Talking About You",
  "I've Got a Feeling",
  "I've Just Seen a Face",
  "If I Fell",
  "If I Needed Someone",
  "If You've Got Trouble",
  "In My Life",
  "In Spite of All the Danger",
  "It Won't Be Long",
  "It's All Too Much",
  "It's for You",
  "It's Only Love",
  "Jazz Piano Song",
  "Johnny B. Goode",
  "Julia",
  "Junk",
  "Kansas City/Hey, Hey, Hey, Hey",
  "Keep Your Hands Off My Baby",
  "Komm Gib Mir Deine Hand",
  "Lady Madonna",
  "Leave My Kitten Alone",
  "Lend Me Your Comb",
  "Let It Be",
  "Like Dreamers Do",
  "Little Child",
  "Lonesome Tears in My Eyes",
  "Long Tall Sally",
  "Long, Long, Long",
  "Looking Glass",
  "Love Me Do",
  "Love Me Tender",
  "Love of the Loved",
  "Love You To",
  "Lovely Rita",
  "Loving You",
  "Lucille",
  "Lucy in the Sky with Diamonds",
  "Madman",
  "Maggie Mae",
  "Magical Mystery Tour",
  "Mailman, Bring Me No More Blues",
  "Martha My Dear",
  "Matchbox",
  "Maxwell's Silver Hammer",
  "Mean Mr. Mustard",
  "Memphis, Tennessee",
  "Michelle",
  "Misery",
  "Money (That's What I Want)",
  "Moonlight Bay",
  "Mother Nature's Son",
  "Mr. Moonlight",
  "No Reply",
  "Nobody I Know",
  "Norwegian Wood (This Bird Has Flown)",
  "Not a Second Time",
  "Not Guilty",
  "Nothin' Shakin'",
  "Nowhere Man",
  "Ob-La-Di, Ob-La-Da",
  "Octopus's Garden",
  "Oh! Darling",
  "Old Brown Shoe",
  "One After 909",
  "One and One Is Two",
  "Only a Northern Song",
  "Ooh! My Soul",
  "P.S. I Love You",
  "Paperback Writer",
  "Penny Lane",
  "Piggies",
  "Please Mr. Postman",
  "Please Please Me",
  "Polythene Pam",
  "Rain",
  "Real Love",
  "Revolution 1",
  "Revolution 9",
  "Revolution",
  "Rip It Up/",
  "Rock and Roll Music",
  "Rocky Raccoon",
  "Roll Over Beethoven",
  "Run for Your Life",
  "Savoy Truffle",
  "Searchin'",
  "September in the Rain",
  "Sexy Sadie",
  "Sgt. Pepper's Lonely Hearts Club Band (Reprise)",
  "Sgt. Pepper's Lonely Hearts Club Band",
  "Shakin' in the Sixties",
  "She Came in Through the Bathroom Window",
  "She Loves You",
  "She Said She Said",
  "She's a Woman",
  "She's Leaving Home",
  "Shout",
  "Sie Liebt Dich",
  "Slow Down",
  "So How Come (No One Loves Me)",
  "Soldier of Love (Lay Down Your Arms)",
  "Some Other Guy",
  "Something",
  "Step Inside Love/Las Paranoias",
  "Strawberry Fields Forever",
  "Sun King",
  "Sure to Fall (In Love with You)",
  "Sweet Little Sixteen",
  "Take Good Care of My Baby",
  "Taxman",
  "Teddy Boy",
  "Tell Me What You See",
  "Tell Me Why",
  "Thank You Girl",
  "That Means a Lot",
  "That'll Be the Day",
  "That's All Right (Mama)",
  "The Ballad of John and Yoko",
  "The Continuing Story of Bungalow Bill",
  "The End",
  "The Fool on the Hill",
  "The Hippy Hippy Shake",
  "The Honeymoon Song",
  "The Inner Light",
  "The Long and Winding Road",
  "The Night Before",
  "The Sheik of Araby",
  "The Word",
  "There's a Place",
  "Things We Said Today",
  "Think for Yourself",
  "This Boy",
  "Three Cool Cats",
  "Ticket to Ride",
  "Till There Was You",
  "Tip of My Tongue",
  "To Know Her is to Love Her",
  "Tomorrow Never Knows",
  "Too Much Monkey Business",
  "Twist and Shout",
  "Two of Us",
  "Wait",
  "Watching Rainbows",
  "We Can Work It Out",
  "What Goes On",
  "What You're Doing",
  "What's The New Mary Jane",
  "When I Get Home",
  "When I'm Sixty-Four",
  "While My Guitar Gently Weeps",
  "Why Don't We Do It in the Road?",
  "Wild Honey Pie",
  "Winston's Walk",
  "With a Little Help from My Friends",
  "Within You Without You",
  "Woman",
  "Words of Love",
  "Yellow Submarine",
  "Yer Blues",
  "Yes It Is",
  "Yesterday",
  "You Can't Do That",
  "You Know My Name (Look Up the Number)",
  "You Know What to Do",
  "You Like Me Too Much",
  "You Never Give Me Your Money",
  "You Really Got a Hold on Me",
  "You Won't See Me",
  "You'll Be Mine",
  "You're Going to Lose That Girl",
  "You've Got to Hide Your Love Away",
  "Young Blood",
  "Your Mother Should Know"]