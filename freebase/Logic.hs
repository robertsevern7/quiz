module Logic where

-- |All the different types of questions
data QuestionFormat = MultipleChoice [String] String
                    | FreeText String
                    | MultipleFreeText [String]

-- |A question is a question format, together with a description
data Question = Question Description QuestionFormat

-- |A question maker uses some logic to generate questions
class QuestionMaker a where
    generateQuestion :: a -> Question

    