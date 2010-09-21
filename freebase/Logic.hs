module Logic where

import Text.JSON

-- |All the different types of questions
data QuestionFormat = MultipleChoice [String] String
                    | FreeText String
                    | MultipleFreeText [String]

mkString :: String -> JSValue
mkString = JSString . toJSString

-- TODO hideous duplication here
-- TODO nicer way of expressing the answer would be good
instance JSON QuestionFormat where
    readJSON jsValue = undefined
    showJSON (MultipleChoice choices answer) = makeObj [("questionType", mkString "multipleChoice")
                                                       ,("choices", JSArray (map mkString choices))
                                                       ,("answer", mkString answer)]
    showJSON (FreeText answer) = makeObj [("questionType", mkString "freeText")
                                         ,("answer", mkString answer)]
    showJSON (MultipleFreeText answers) = makeObj [("questionType", mkString "multipleFreeText")
                                                  ,("answers", JSArray (map mkString answers))]

-- |May want to change this to something "formattable"
type Description = String

-- |A question is a question format, together with a description
data Question = Question Description QuestionFormat

-- |When a Question is 
instance JSON Question where
    readJSON jsValue = undefined
    showJSON (Question description questionFormat) = makeObj [("question", mkString description)
                                                             ,("format", showJSON questionFormat)]

-- |A question maker uses some logic to generate questions
class QuestionMaker a where
    generateQuestion :: a -> Question

    