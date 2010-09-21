module Logic where

-- |All the different types of questions
data QuestionFormat = MultipleChoice [String] String
                    | FreeText String
                    | MultipleFreeText [String]

    