module MovieQuotes (
  QuoteSelection,
  quoteSelection
  ) where

import Logic

data Quote = Quote {
    quote :: String
  , movie :: String
}

quoteSelection :: QuoteSelection
quoteSelection = QuoteSelection questions

data QuoteSelection = QuoteSelection [(String, String)]

instance QuestionMaker QuoteSelection where
  generateQuestion seed AssociateType (QuoteSelection questions) = do
    quoteList <- rndSelect seed questions 10
    return $ Just (Associate "Match these quotes to the film" quoteList)
  generateQuestion seed IdentifyMultipleType (QuoteSelection questions) = do
    quoteList <- rndSelect seed questions 10
    return $ Just (Associate "Name the films from these quotes" quoteList)
  generateQuestion seed IdentifyTextType (QuoteSelection questions) = do
    question <- chooseFromList seed questions
    return $ Just (uncurry (IdentifyText "Name the film from the quote") question Nothing)
  generateQuestion _ _ _ = return Nothing

questions :: [(String, String)]
questions = [
  ("Frankly, my dear, I don't give a damn.", "Gone With the Wind"),
  ("I'm going to make him an offer he can't refuse.", "The Godfather"),
  ("You don't understand! I coulda had class. I coulda been a contender. I could've been somebody, instead of a bum, which is what I am.", "On the Waterfront"),
  ("Toto, I've got a feeling we're not in Kansas anymore.", "The Wizard of Oz"),
  ("Here's looking at you, kid.", "Casablanca"),
  ("Go ahead, make my day.", "Sudden Impact"),
  ("All right, Mr. DeMille, I'm ready for my close-up.", "Sunset Blvd."),
  ("May the Force be with you.", "Star Wars"),
  ("Fasten your seatbelts. It's going to be a bumpy night.", "All About Eve"),
  ("You talking to me?", "Taxi Driver"),
  ("What we've got here is failure to communicate.", "Cool Hand Luke"),
  ("I love the smell of napalm in the morning.", "Apocalypse Now"),
  ("Love means never having to say you're sorry.", "Love Story"),
  ("The stuff that dreams are made of.", "The Maltese Falcon"),
  ("E.T. phone home.", "E.T. The Extra-Terrestrial"),
  ("They call me Mister Tibbs!", "In the Heat of the Night"),
  ("Rosebud.", "Citizen Kane"),
  ("Made it, Ma! Top of the world!", "White Heat"),
  ("I'm as mad as hell, and I'm not going to take this anymore!", "Network"),
  ("Louis, I think this is the beginning of a beautiful friendship.", "Casablanca"),
  ("A census taker once tried to test me. I ate his liver with some fava beans and a nice Chianti.", "The Silence of the Lambs"),
  ("Bond. James Bond.", "Dr. No"),
  ("There's no place like home.", "The Wizard of Oz"),
  ("I am big! It's the pictures that got small.", "Sunset Blvd."),
  ("Show me the money!", "Jerry Maguire"),
  ("Why don't you come up sometime and see me?", "She Done Him Wrong"),
  ("I'm walking here! I'm walking here!", "Midnight Cowboy"),
  ("Play it, Sam. Play 'As Time Goes By.'", "Casablanca"),
  ("You can't handle the truth!", "A Few Good Men"),
  ("I want to be alone.", "Grand Hotel"),
  ("After all, tomorrow is another day!", "Gone With the Wind"),
  ("Round up the usual suspects.", "Casablanca"),
  ("I'll have what she's having.", "When Harry Met Sally"),
  ("You know how to whistle, don't you, Steve? You just put your lips together and blow.", "To Have and Have Not"),
  ("You're gonna need a bigger boat.", "Jaws"),
  ("Badges? We ain't got no badges! We don't need no badges! I don't have to show you any stinking badges!", "The Treasure of the Sierra Madre"),
  ("I'll be back.", "The Terminator"),
  ("Today, I consider myself the luckiest man on the face of the earth.", "The Pride of the Yankees"),
  ("If you build it, he will come.", "Field of Dreams"),
  ("Mama always said life was like a box of chocolates. You never know what you're gonna get.", "Forrest Gump"),
  ("We rob banks.", "Bonnie and Clyde"),
  ("Plastics.", "The Graduate"),
  ("We'll always have Paris.", "Casablanca"),
  ("I see dead people.", "The Sixth Sense"),
  ("Stella! Hey, Stella!", "A Streetcar Named Desire"),
  ("Oh, Jerry, don't let's ask for the moon. We have the stars.", "Now, Voyager"),
  ("Shane. Shane. Come back!", "Shane"),
  ("Well, nobody's perfect.", "Some Like It Hot"),
  ("It's alive! It's alive!", "Frankenstein"),
  ("Houston, we have a problem.", "Apollo 13"),
  ("You've got to ask yourself one question: 'Do I feel lucky?' Well, do ya, punk?", "Dirty Harry"),
  ("You had me at hello.", "Jerry Maguire"),
  ("One morning I shot an elephant in my pajamas. How he got in my pajamas, I don't know.", "Animal Crackers"),
  ("There's no crying in baseball!", "A League of Their Own"),
  ("La-dee-da, la-dee-da.", "Annie Hall"),
  ("A boy's best friend is his mother.", "Psycho"),
  ("Greed, for lack of a better word, is good.", "Wall Street"),
  ("Keep your friends close, but your enemies closer.", "The Godfather II"),
  ("As God is my witness, I'll never be hungry again.", "Gone With the Wind"),
  ("Well, here's another nice mess you've gotten me into!", "Sons of the Desert"),
  ("Say hello to my little friend!", "Scarface"),
  ("What a dump.", "Beyond the Forest"),
  ("Mrs. Robinson, you're trying to seduce me. Aren't you?", "The Graduate"),
  ("Gentlemen, you can't fight in here! This is the War Room!", "Dr. Strangelove"),
  ("Elementary, my dear Watson.", "The Adventures of Sherlock Holmes"),
  ("Get your stinking paws off me, you damned dirty ape.", "Planet of the Apes"),
  ("Of all the gin joints in all the towns in all the world, she walks into mine.", "Casablanca"),
  ("Here's Johnny!", "The Shining"),
  ("They're here!", "Poltergeist"),
  ("Is it safe?", "Marathon Man"),
  ("Wait a minute, wait a minute. You ain't heard nothin' yet!", "The Jazz Singer"),
  ("No wire hangers, ever!", "Mommie Dearest"),
  ("Mother of mercy, is this the end of Rico?", "Little Caesar"),
  ("Forget it, Jake, it's Chinatown.", "Chinatown"),
  ("I have always depended on the kindness of strangers.", "A Streetcar Named Desire"),
  ("Hasta la vista, baby.", "Terminator 2: Judgment Day"),
  ("Soylent Green is people!", "Soylent Green"),
  ("Open the pod bay doors, HAL.", "2001: A Space Odyssey"),
  ("Striker: Surely you can't be serious. Rumack: I am serious... and don't call me Shirley.", "Airplane!"),
  ("Yo, Adrian!", "Rocky"),
  ("Hello, gorgeous.", "Funny Girl"),
  ("Toga! Toga!", "National Lampoon's Animal House"),
  ("Listen to them. Children of the night. What music they make.", "Dracula"),
  ("Oh, no, it wasn't the airplanes. It was Beauty killed the Beast.", "King Kong"),
  ("My precious.", "The Lord of the Rings: Two Towers"),
  ("Attica! Attica!", "Dog Day Afternoon"),
  ("Sawyer, you're going out a youngster, but you've got to come back a star!", "42nd Street"),
  ("Listen to me, mister. You're my knight in shining armor. Don't you forget it. You're going to get back on that horse, and I'm going to be right behind you, holding on tight, and away we're gonna go, go, go!", "On Golden Pond"),
  ("Tell 'em to go out there with all they got and win just one for the Gipper.", "Knute Rockne All American"),
  ("A martini. Shaken, not stirred.", "Goldfinger"),
  ("Who's on first.", "The Naughty Nineties"),
  ("Cinderella story. Outta nowhere. A former greenskeeper, now, about to become the Masters champion. It looks like a mirac...It's in the hole! It's in the hole! It's in the hole!", "Caddyshack"),
  ("Life is a banquet, and most poor suckers are starving to death!", "Auntie Mame"),
  ("I feel the need - the need for speed!", "Top Gun"),
  ("Carpe diem. Seize the day, boys. Make your lives extraordinary.", "Dead Poets Society"),
  ("Snap out of it!", "Moonstruck"),
  ("My mother thanks you. My father thanks you. My sister thanks you. And I thank you.", "Yankee Doodle Dandy"),
  ("Nobody puts Baby in a corner.", "Dirty Dancing"),
  ("I'll get you, my pretty, and your little dog, too!", "The Wizard of Oz"),
  ("I'm king of the world!", "Titanic"),
  ("Hey, don't knock masturbation. It's sex with someone I love.", "Annie Hall"),
  ("She's my daughter!...She's my sister! She's my daughter! My sister, my daughter...She's my sister and my daughter.","Chinatown"),
  ("Because when you're a call girl, you control it, that's why. Because someone wants you...and for an hour...I'm the best actress in the world.", "Klute"),
  ("So we finish 18 and he's gonna stiff me. And I say, 'Hey, Lama, hey, how about a little something, you know, for the effort, you know.' And he says, 'Oh, uh, there won't be any money, but when you die, on your deathbed, you will receive total consciousness.' So I got that goin' for me, which is nice.","Caddyshack"),
  ( "Hitler was better-looking than Churchill, he was a better dresser than Churchill, he had more hair, he told funnier jokes, and he could dance the pants off of Churchill!","The Producers"),
  ("No, I'm all man. I even fought in WWII. Of course, I was wearing women's undergarments under my uniform.","Ed Wood"),
  ("...Your mother's in here with us, Karras. Would you like to leave a message? I'll see that she gets it.","The Exorcist"),
  ("You hear me talkin', hillbilly boy? I ain't through with you by a damn sight. I'm gonna get medieval on your ass.","Pulp Fiction"),
  ("I fart in your general direction. Your mother was a hamster and your father smelt of elderberries.","Monty Python and the Holy Grail"), 
  ("He won't come after me. He won't. I can't explain it. He would consider that...rude.","The Silence of the Lambs"),
  ("Excuse me while I whip this out.","Blazing Saddles"),
  ("No, Mr. Bond. I expect you to die!","Goldfinger"),
  ("Kid, the next time I say, 'Let's go someplace like Bolivia,' let's go someplace like Bolivia.","Butch Cassidy and the Sundance Kid"),
  ("Wendy?...Darling. Light of my life. I'm not gonna hurt ya. You didn't let me finish my sentence. I said: 'I'm not gonna hurt ya.' I'm just gonna bash your brains in. I'm gonna bash 'em right the f--- in! Ha, ha.","The Shining"),
  ("I just hate you and I hate your ass face.","Waiting for Guffman"),
  ("You shoot off a guy's head with his pants down, believe me, Texas is not the place you wanna get caught.","Thelma & Louise"),
  ("And I guess that was your accomplice in the wood chipper.","Fargo"),
  ("The greatest trick the devil ever pulled was convincing the world he didn't exist.","The Usual Suspects"),
  ("I do not, for one, think that the problem was that the band was down. The problem may have been that there was a Stonehenge monument on the stage that was in danger of being crushed by a dwarf.","This Is Spinal Tap"),
  ("That's what I love about these high school girls, man. I get older, they stay the same age.","Dazed and Confused"),
  ("...I have nipples, Greg. Could you milk me?","Meet the Parents"),
  ("Fat, drunk, and stupid is no way to go through life, son.","National Lampoon's Animal House"),
  ("Who told you to step on my sneakers, who told you to walk on my side of the block, who told you to be in my neighborhood?","Do The Right Thing"),
  ("There's a lotta things about me you don't know anything about, Dottie. Things you wouldn't understand. Things you couldn't understand.","Pee-wee's Big Adventure"),
  ("Joey, do you like movies about gladiators?","Airplane!"),
  ("My name is Maximus Decimus Meridius, Commander of the armies of the North, General of the Felix legions, loyal servant to the true Emperor, Marcus Aurelius. Father to a murdered son, husband to a murdered wife. And I will have my vengeance, in this life or the next.","Gladiator"),
  ("Bring the dog, I love animals. I'm a great cook.","Fatal Attraction"),
  ("And one day, not long from now, my looks will go. They will discover I can't act, and I will become some sad middle-aged woman who looks a bit like someone who was famous for a while.","Notting Hill"),
  ("Aristotle was not Belgian. The central message of Buddhism is not 'every man for himself.' And the London Underground is not a political movement. Those are all mistakes, Otto. I looked 'em up.","A Fish Called Wanda"),
  ("Empire had the better ending. I mean, Luke gets his hand cut off, finds out Vader's his father, Han gets frozen and taken away by Boba Fett. It ends on such a down note. I mean, that's what life is, a series of down endings. All Jedi had was a bunch of Muppets.","Clerks"),
  ("My daughter is in pain...Give my daughter the shot!","Terms of Endearment"),
  ("Relax, all right? Don't try to strike everybody out. Strikeouts are boring; besides that, they're fascist. Throw some ground balls. It's more democratic.","Bull Durham"),
  ("You know, I have one simple request. And that is to have sharks with frickin' laser beams attached to their heads! Now evidently, my cycloptic colleague informs me that that can't be done. Can you remind me what I pay you people for? Honestly, throw me a bone here. What do we have?","Austin Powers: International Man of Mystery"),
  ("I'm not bad. I'm just drawn that way.","Who Framed Roger Rabbit"), 
  ("You want me to strap her to the hood?...She'll be fine. It's not as if it's going to rain or something.","National Lampoon's Vacation"),
  ("When I first saw you, I thought you were handsome. Then, of course, you spoke.","As Good As It Gets"),
  ("When it comes down to making out, whenever possible, put on side 1 of Led Zeppelin IV.","Fast Times at Ridgemont High"), 
  ("I wanted to see exotic Vietnam, the jewel of Southeast Asia. I, uh, I wanted to meet interesting and stimulating people of an ancient culture, and kill them.","Full Metal Jacket"),
  ("Look at that! Look how she moves! That's just like Jell-O on springs.","Some Like It Hot"),
  ("When the legend becomes fact, print the legend.","The Man Who Shot Liberty Valance"),
  ("Someone has to die in order that the rest of us should value life more.","The Hours"),
  ("Nobody's looking for a puppeteer in today's wintry economic climate.","Being John Malkovich"),
  ("Jerry, d'you know the human head weighs eight pounds?","Jerry Maguire"),
  ("I can't believe I gave my panties to a geek.","Sixteen Candles"),
  ("I love my dead gay son.","Heathers"),
  ("Where was ya, Wang? We was worried.","Murder by Death"),
  ("How am I not myself?","I Heart Huckabees"),
  ("Welcome to Debbie Country","Singles"),
  ("I feel like I'm taking crazy pills!","Zoolander"),
  ("Well, this piece is called 'Lick My Love Pump'.","Spinal Tap"),
  ("This is the guy behind the guy behind the guy'.","Swingers"),
  ("I hate you, and I hate your ass face!", "Waiting for Guffman"),
  ("Back and to the left.","JFK"),
  ("No, I said 'allo,' but that's close enough.","Labyrinth"),
  ("That's bee-YOU-tee-ful, what is that, velvet?","Coming to America"),
  ("It's a moral imperative.","Real Genius"),
  ("Go do that voodoo that you do so well!","Blazing Saddles"),
  ("No dice, soldier.","Brick"),
  ("To crush your enemies, see them driven before you, and 'o hear the lamentation of their women.","Conan the Barbarian"),
  ("Take this quarter, go downtown, and have a rat gnaw that'thing off your face! Good day to you, madam.","Uncle Buck"),
  ("'Do you concur?' 'Damnit! Why didn't I concur?!'","Catch Me If You Can"),
  ("The place where a U.S. soldier goes to defecate, relieve himself, open his bowel, shit, fart, dump, crap, and unload, is called the latrine. The la-trine, from the French.","Biloxi Blues"),
  ("Big bottoms, big bottoms, talk about mudflaps, my girls got 'em.","Spinal Tap"),
  ("My life is as good as an Abba song. It's as good as Dancing Queen.","Muriel's Wedding"),
  ("Guns are for show. Knives are for pros.","Lock, Stock, and Two Smoking Barrels"),
  ("I shall call him Squishy. And he shall be mine. And he shall be MY Squishy.","Finding Nemo"),
  ("I'll sleep with you for a meatball.","Victor/Victoria"),
  ("Follow me, or perish, sweater monkeys.","Bring it On"),
  ("What's a nubian?","Chasing Amy"),
  ("Hokey religions and ancient weapons are no match for a good blaster by your side, kid.","Star Wars"),
  ("You've got red on you.","Shaun of the Dead"),
  ("I touched the earth, and he loved me back.","Secretary"),
  ("Not you, fat Jesus.","The Hangover"),
  ("This pile of shit has a thousand eyes.","Stand By Me"),
  ("Not the beeeees!.","Wicker Man"),
  ("She's been fucked more times than she's had a hot meal.","Kiss Kiss, Bang Bang"),
  ("I can't believe I just gave my panties to a geek.","Sixteen Candles"),
  ("It's a veg-e-ta-ble.","y Blue Heaven"),
  ("Goddammit, I'd piss on a spark plug if I thought it'd do any good!","War Games"),
  ("I killed the president of Paraguay with a fork. How have you been?","Grosse Pointe Blank"),
  ("Now, you've got a corpse in a car, minus a head, in a garage. Take me to it.","Pulp Fiction"),
  ("Ever since I can remember I always wanted to be a gangster.","Goodfellas"),
  ("Wolfman has nards!","Monster Squad"),
  ("He's an angel. He's an angel straight from heaven!","Raising Arizona"),
  ("Those who are tardy do not get fruit cup.","High Anxiety"),
  ("Somebody's got to go back and get a shitload of dimes.","Blazing Saddles"),
  ("You idiots! These are not them! You've captured their stunt doubles!","Spaceballs"),
  ("Bratwurst? Aren't we the optimist?","10 Things I Hate About You"),
  ("Sabrina, don't just stare at it, eat it.","American Psycho"),
  ("I take your fucking bullets!","Scarface"),
  ("I'm kind of a big deal","Anchorman"),
  ("Sometimes you win, sometimes you lose and sometimes it rains.","Bull Durham"),
  ("We deal in lead, friend.","The Magnificent Seven"),
  ("I don't know, I mostly just hurt people.","Alien Resurrection"),
  ("Go that way, really fast. If something gets in your way, turn.","Better Off Dead"),
  ("All every woman really wants, be it mother, senator, nun, is some serious deep-dickin'.","Chasing Amy"),
  ("Let's shag ass.","The Royal Tenenbaums"),
  ("I don't understand. All my life I've been waiting for someone and when I find her, she's ... she's a fish.","Splash"),
  ("Demented and sad, but social.","The Breakfast Club"),
  ("This is so bad it's gone past good and back to bad again.","Ghost World"),
  ("Beautiful, naked, big-titted women just don't fall out of the sky, you know.","Dogma"),
  ("They've done studies, you know. Sixty percent of the time, it works every time.","Anchorman"),
  ("I'm hungry. Let's get a taco.","Reservoir Dogs"),
  ("They're coming to get you, Barbara!","Night of the Living Dead"),
  ("Maybe you're the plucky comic relief.","Galaxy Quest"),
  ("We were frightened of being left alone for the rest of our lives. Only people of a certain disposition are frightened of being alone for the rest of their lives at the age of 26...we were of that disposition.","High Fidelity"),
  ("I used to fuck guys like you in prison","Roadhouse"),
  ("Are you crazy? The fall will probably kill you.","Butch Cassidy and the Sundance Kid"),
  ("Strikeouts are boring. Besides that, they're fascist.","Bull Durham"),
  ("Gentlemen, you can't fight in here! This is the War Room!","Dr. Strangelove"),
  ("Shut the fuck up, Donny.","The Big Lebowski"),
  ("If God did not want them shorn, he would not have made them sheep.","The Magnificent Seven"),
  ("He was always a rather stupidly optimistic man. I mean, I'm afraid it came as a great shock to him when he died.","Clue"),
  ("Nobody fucks with the Jesus.","The Big Lebowski"),
  ("Meet me in Montauk.","Eternal Sunshine of the Spotless Mind"),
  ("Did you have a brain tumor for breakfast?","Heathers"),
  ("That's just the way it crumbles... cookie wise.","The Apartment"),
  ("Winners go home and fuck the prom queen.","The Rock"),
  ("Why didn't somebody tell me my ass was so big?","Spaceballs"),
  ("I aim to misbehave.","Serenity"),
  ("People are so stupid I can't bear to be around them anymore.","Imaginary Heroes"),
  ("I mean, say what you like about the tenets of National Socialism, Dude, at least it's an ethos.","The Big Lebowski"),
  ("The swan ate my baby!","Drop Dead Gorgeous"),
  ("I'm gonna punch you in the ovary, that's what I'm gonna do. A straight shot, right to the babymaker.","Anchorman"),
  ("My grammy never gave gifts. She was too busy getting raped by Cossacks.","Annie Hall"),
  ("The only true currency in this bankrupt world is what you share with someone else when you're uncool.","Almost Famous"),
  ("SQUIRREL!","Up"),
  ("Excuse me stewardess, I speak jive.","Airplane"),
  ("Inconceivable!","The Princess Bride"),
  ("I've been listening to my gut since I was 14 years old, and frankly speaking, I've come to the conclusion that my guts have shit for brains.","High Fidelity"),
  ("My God. I haven't been fucked like that since grade school.","Fight Club"),
  ("You're killin' me Smalls!","The Sandlot"),
  ("I was born a poor black child.","The Jerk"),
  ("Ray, next time someone asks you if you're a god, you say YES!","Ghostbusters"),
  ("Hope is a good thing, maybe the best of things, and no good thing ever dies.","The Shawshank Redemption"),
  ("I want my two dollars!","Better Off Dead"),
  ("Son, you got a panty on your head.","Raising Arizona"),
  ("It ain't white boy day is it?","True Romance")
  ]