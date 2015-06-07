import Distribution.Simple
import System.IO
import System.Directory
import System.Environment
import Data.List
main = do
    --your 
    (x:_) <- getArgs
    if x == "configure" then do
            dir <- getAppUserDataDirectory "GiveYouAHead"
            isE <- doesDirectoryExist dir
            if isE == True then putStrLn "" else createDirectory dir
            isE <- doesDirectoryExist (dir++"/data")
            if isE == True then putStrLn "" else createDirectory (dir++"/data")
            isE <- doesDirectoryExist (dir++"/data/language")
            if isE == True then putStrLn "" else createDirectory (dir++"/data/language")
            hD <- openFile (dir++"/data/delList.dat") ReadMode
            stSrc <- hGetLine hD
            hClose hD
            putStrLn stSrc
            writeFile (dir ++ "/data/delList.dat") (show$dropRepeated$sort((read stSrc ::[String])++dL))
            writeFile (dir ++ "/data/language/C.cmap") (show langM)
            defaultMain
    --your end
        else
            defaultMain
    where
    langM = [
        ("*Template","\nint main() \n{\n        return 0;\n}\n"),
        ("*NoteMark","//"),
        ("*SrcAhead","C_"),
        ("*SrcBack",".c"),
        ("*COB","CompilerExtraOptionsBegin"),
        ("*COE","CompilerExtraOptionsEnd"),
        ("*ImportAhead","#include<"),
        ("*ImportBack",">\n"), -- need break a new line
        ("*FE","c"),
        ("*Compiler","gcc"),
        ("*Debug","-g"),
        ("*Object","-o")
        ]
    dL = [
        "*.c~",
        "*.o"
        ]


dropRepeated :: (Eq a)=> [a] -> [a]
dropRepeated [] = []
dropRepeated (x:[]) = [x]
dropRepeated (x:y:xs)
    | x == y = dropRepeated (x:xs)
    | otherwise = x:dropRepeated (y:xs)
