module Logic where

import Text.JSON

-- TODO some of these question types are less than self-explanatory
-- |All the different types of questions
data QuestionFormat = MultipleChoice [String] String -- ^ Choose one from a set
                    | FreeText String -- |^ Free text to compare against supplied text
                    | MultipleFreeText [String] -- |^ Multiple choices of free text 
                    | IdentifyFrom [String] String -- |^ Given a set of strings identify some known answer
					deriving Show

mkString :: String -> JSValue
mkString = JSString . toJSString

-- TODO hideous duplication here
-- TODO nicer way of expressing the answer would be good
instance JSON QuestionFormat where
    readJSON jsValue = Error "No need to support this just yet"
    showJSON (MultipleChoice choices answer) = makeObj [("questionType", mkString "multipleChoice")
                                                       ,("choices", JSArray (map mkString choices))
                                                       ,("answer", mkString answer)]
    showJSON (FreeText answer) = makeObj [("questionType", mkString "freeText")
                                         ,("answer", mkString answer)]
    showJSON (MultipleFreeText answers) = makeObj [("questionType", mkString "multipleFreeText")
                                                  ,("answers", JSArray (map mkString answers))]
    showJSON (IdentifyFrom hints answer) = makeObj [("questionType", mkString "identifyFrom")
                                                   ,("answer", mkString answer)
                                                   ,("hints", JSArray (map mkString hints))]

-- |May want to change this to something "formattable"
type Description = String

-- |A question is a question format, together with a description
data Question = Question Description QuestionFormat deriving Show

-- |When a Question is 
instance JSON Question where
    readJSON jsValue = Error "No need to support this just yet"
    showJSON (Question description questionFormat) = makeObj [("question", mkString description)
                                                             ,("format", showJSON questionFormat)]

-- |A question maker uses some logic to generate questions
class QuestionMaker a where 
    generateQuestion :: a -> IO Question

    