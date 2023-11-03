module RealWorldHaskell.C01WC (wc,wcWord, wcChar) where


wc = interact calLineCount
    where calLineCount input =show (length ( lines input)) ++ "\n"

wcWord = interact wordCount
    where wordCount input = show (length( words input)) ++ "\n"

wcChar = interact charCount
    where charCount input = show (length input) ++ "\n"