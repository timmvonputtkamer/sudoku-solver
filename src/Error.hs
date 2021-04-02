module Error (Error(Error), extractErrorMessage) where

newtype Error = Error String

extractErrorMessage :: Error -> String
extractErrorMessage (Error message) = message
