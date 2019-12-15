
import Control.Monad (mapM_)
import Data.List (group)

type Password = (Char, Char, Char, Char, Char, Char)

passwords :: [String]
passwords = do
    a <- ['2'..'7']
    let b_lower_bound = if a == '2'
        then '7'
        else a
    b <- [b_lower_bound..'9']
    let c_lower_bound = case (a,b) of
                ('2', '7') -> '8'
                _ -> b
    c <- [c_lower_bound..'9']
    d <- [c..'9']
    e <- [d..'9']
    f <- [e..'9']
    return (a:b:c:d:e:f:"")

-- part 1
validPasswords = filter hasAdjacentEqual $ passwords
    where hasAdjacentEqual ls = any id (zipWith (==) ls (tail ls))

-- part 2
validPasswords' = filter isValid passwords
    where isValid pass = any ((== 2) . length) $ group pass

main = mapM_ putStrLn validPasswords'
