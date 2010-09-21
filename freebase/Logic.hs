module Logic where

import Text.JSON

-- |All the different types of questions
data QuestionFormat = MultipleChoice [String] String
                    | FreeText String
                    | MultipleFreeText [String]

instance JSON QuestionFormat where
    readJSON jsValue = undefined
    showJSON (MultipleChoice choices answer) = undefined
    showJSON (FreeText answer) = undefined
    showJSON (MultipleFreeText answers) = undefined

-- |May want to change this to something "formattable"
type Description = String

-- |A question is a question format, together with a description
data Question = Question Description QuestionFormat

-- |When a Question is 
instance JSON Question where
    readJSON jsValue = undefined
    showJSON (Question description questionFormat) = makeObj [("question", JSString $ toJSString description)
                                                             ,("format", showJSON questionFormat)]

-- |A question maker uses some logic to generate questions
class QuestionMaker a where
    generateQuestion :: a -> Question

    