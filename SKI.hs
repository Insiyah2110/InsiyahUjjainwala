-- I am Insiyah Ujjainwala
-- McMaster email: ujjainwi@mcmaster.ca

module A3.SKI where

-- * Question 1: Revenge of the Goblins and Gnomes   

data SKI
    = S
    | K
    | I
    | App SKI SKI
    deriving (Eq, Show)

ski :: SKI -> Maybe SKI

ski (App (App (App S x) y) z)= Just (App (App x z) (App y z))
ski (App (App K x) _) = Just x
ski (App I x) = Just x


ski (App e1 e2) = 
    case ski e1 of 
        Just e1' -> Just (App e1' e2)
        Nothing ->
            case ski e2 of 
                Just e2' -> Just (App e1 e2')
                Nothing -> Nothing
ski _ = Nothing